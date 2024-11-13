library("dataDownloader")
library(broom)
library(fs)
library(zoo)
library(slider)
library(fluxible)
library(ggforce)
library(progress)
source("R/Load packages.R")



#download and unzip files from OSF
get_file(node = "pk4bg",
         file = "Three-D_cflux_2021.zip",
         path = "data/c-flux/summer_2021",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_field-record_2021.csv",
         path = "data/c-flux/summer_2021",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_cutting_2021.csv",
         path = "data/c-flux/summer_2021",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_soilR-chambers-size.csv",
         path = "data/c-flux/summer_2021",
         remote_path = "RawData/C-Flux")


soilR_chamber <- read_csv("data/c-flux/summer_2021/Three-D_soilR-chambers-size.csv") |>
  mutate(
    soil_chamber_vol = pi * (soil_collar_cm/2)^2 * depth_above_cm * 0.001, # vol in liter
    type = "soilR"
  )

# Unzip files
zipFile <- "data/c-flux/summer_2021/Three-D_cflux_2021.zip"
if(file.exists(zipFile)){
  outDir <- "data/c-flux/summer_2021"
  unzip(zipFile, exdir = outDir)
}

#importing fluxes data
location <- "data/c-flux/summer_2021" #location of datafiles

conc_raw <-
  dir_ls(location, regexp = "*CO2*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% 
  rename( #rename the column to get something more practical without space
    CO2 = "CO2 (ppm)",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    PAR = "PAR (umolsm2)",
    datetime = "Date/Time"
    ) %>%  
  mutate(
    datetime = dmy_hms(datetime)
  ) %>%
  select(datetime, CO2, PAR, temp_air, temp_soil)


#import the record file from the field

record <- read_csv("data/c-flux/summer_2021/Three-D_field-record_2021.csv", na = c(""), col_types = "cctDfc") %>% 
  drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
  mutate(
    start = ymd_hms(paste(date, starting_time)) #converting the date as posixct, pasting date and starting time together
    # end = start + measurement, #creating column End
    # start_window = start + startcrop, #cropping the start
    # end_window = end - endcrop #cropping the end of the measurement
  )  |>
  distinct(start, .keep_all = TRUE) # some replicates were also marked as LRC and that is not correct

#matching the CO2 concentration data with the turfs using the field record
conc <- flux_match(
  conc_raw,
  record,
  conc_col = "CO2"
  )

str(conc)

# fitting fluxes

slopes_exp_2021 <- flux_fitting(
  conc_df = conc,
  fit_type = "exp",
  end_cut = 30
)

slopes_exp_2021_flag <- flux_quality(
  slopes_exp_2021,
  error = 400, # the gas analyser was off but the slope is ok
  force_ok_id = c(
    198 # looks ok despite b above threshold
  ),
  weird_fluxes_id = c(
    402, # slope is off
    403, # slope is off
    409, # slope is off
    562, # slope in opposite direction
    985 # slope is off
  )
)

# flux_plot is time consuming so we keep it as comments to avoid running accidentally
# flux_plot(
#   slopes_exp_2021_flag,
#   print_plot = "FALSE",
#   output = "pdf",
#   f_plotname = "plot_2021",
#   f_ylim_lower = 300
#   )

#need to clean PAR, temp_air, temp_soil

#put NA for when the soil temp sensor was not pluged in

slopes_exp_2021_flag <- slopes_exp_2021_flag %>% 
  mutate(
    temp_soil = case_when(
      comments == "soilT logger not plugged in" ~ NA_real_,
      comments == "Soil T NA" ~ NA_real_,
      TRUE ~ temp_soil
    )
  )


str(slopes_exp_2021_flag)


#PAR: same + NA for soilR and ER

slopes_exp_2021_flag <- slopes_exp_2021_flag |>
  mutate(
    PAR =
      case_when(
        type == "ER" & PAR < 0 ~ 0,
        type == "LRC5" & PAR > 10 ~ NA_real_, # PAR sensor had faulty contact
        type == "LRC5" & PAR < 0 ~ 0, # when covering chamber after strong light, sensor can go negative but it is 0
        type == "ER" & PAR > 10 ~ NA_real_,
        type == "NEE" & PAR < 70 ~ NA_real_, # PAR sensor had faulty contact
        type == "SoilR" ~ NA_real_, # no PAR data collected when doing soil respiration, but sensor connected
        TRUE ~ PAR
      )
  )



plot_PAR <- function(slope_df, filter, filename, scale){
plot <- filter(slope_df, type == ((filter))) %>%
  ggplot(aes(x = f_datetime)) +
    geom_point(size = 0.2, aes(group = f_fluxID, y = PAR, color = f_cut)) +
    scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
    do.call(facet_wrap_paginate,
      args = c(facets = ~f_fluxID, ncol = 5, nrow = 3, scales = ((scale)))
    ) +
    scale_color_manual(values = c(
      "cut" = "#D55E00",
      "keep" = "#009E73"
    ))

    pdf(((filename)), paper = "a4r", width = 11.7, height = 8.3)


 pb <- progress_bar$new(
      format =
        "Printing plots in pdf document [:bar] :current/:total (:percent)",
      total = n_pages(plot)
    )
    pb$tick(0)
    Sys.sleep(3)
    for (i in 1:n_pages(plot)) {
      pb$tick()
      Sys.sleep(0.1)
      print(plot +
        do.call(facet_wrap_paginate,
          args = c(
            facets = ~f_fluxID,
            page = i,
            ncol = 5, nrow = 3, scales = ((scale))
          )
        ))
    }
    quietly(dev.off())

}

plot_PAR(slopes_exp_2021_flag, "NEE", "plot_NEE_PAR.pdf", "free")
plot_PAR(slopes_exp_2021_flag, "ER", "plot_ER_PAR.pdf", "free")
plot_PAR(slopes_exp_2021_flag, "LRC1", "plot_LRC1_PAR.pdf", "free")
plot_PAR(slopes_exp_2021_flag, "LRC2", "plot_LRC2_PAR.pdf", "free")
plot_PAR(slopes_exp_2021_flag, "LRC3", "plot_LRC3_PAR.pdf", "free")
plot_PAR(slopes_exp_2021_flag, "LRC4", "plot_LRC4_PAR.pdf", "free")
plot_PAR(slopes_exp_2021_flag, "LRC5", "plot_LRC5_PAR.pdf", "free")

# we need to include soil respiration chamber volume
slopes_2021 <- left_join(slopes_exp_2021_flag, soilR_chamber) |>
  mutate(
    chamber_vol = case_when(
      type == "soilR" ~ soil_chamber_vol,
      type != "soilR" ~ 24.5
    )
  )

str(slopes_2021)

fluxes2021 <- flux_calc(
  slopes_2021,
  slope_col = "f_slope_corr",
  conc_unit = "ppm",
  flux_unit = "mmol",
  cut_col = "f_cut",
  keep_arg = "keep",
  chamber_volume = "chamber_vol",
  cols_keep = c(
    "turfID",
    "type",
    "campaign",
    "comments",
    "f_quality_flag"
  ),
  cols_ave = c(
    "PAR",
    "temp_soil"
  )
)

fluxes2021 <- fluxes2021 |>
  mutate(
    PAR = case_when(
      is.nan(PAR) == TRUE ~ NA_real_, #mean(PAR) returned NaN when PAR was all NAs but it is missing values
      TRUE ~ as.numeric(PAR)
    )
  )

str(fluxes2021)


  
#replacing PAR Na by the average PAR of the 3h period in which the measurement is
roll_period <- 3

PAR_ER <- filter(fluxes2021, type == "ER") %>%
  slide_period_dfr(
    # .,
    .$datetime,
    "hour",
    .every = roll_period,
    ~data.frame(
      datetime = max(.x$datetime),
      PAR_roll_ER = mean(.x$PAR, na.rm = TRUE)
    )
  )

PAR_NEE <- filter(fluxes2021, type == "NEE") %>%
  slide_period_dfr(
    # .,
    .$datetime,
    "hour",
    .every = roll_period,
    ~data.frame(
      datetime = max(.x$datetime),
      PAR_roll_NEE = mean(.x$PAR, na.rm = TRUE)
    )
  )



fluxes2021 <- left_join(fluxes2021, PAR_ER) %>% 
  left_join(PAR_NEE) %>% 
  fill(PAR_roll_NEE, .direction = "up") %>% 
  fill(PAR_roll_ER, .direction = "up") %>% 
  mutate(
    comments = case_when(
      is.na(PAR) == TRUE
      # & type == ("ER" | "NEE")
      ~ paste0(comments,  ";", " PAR 3h period average"),
      TRUE ~ comments
    ),
    comments = str_replace_all(comments, "NA; ", ""),
    PAR = case_when(
      is.na(PAR) == TRUE
      & type == "ER"
      ~ PAR_roll_ER,
      is.na(PAR) == TRUE
      & type == "NEE"
      ~ PAR_roll_NEE,
      TRUE ~ PAR
    )

    
  ) %>% 
  select(!c(PAR_roll_NEE, PAR_roll_ER))
  

#replace soil temp Na with average of measurements in the same 3h period
# roll_period <- 3

soiltemp_ER <- filter(fluxes2021, type == "ER") %>%
  slide_period_dfr(
    # .,
    .$datetime,
    "hour",
    .every = roll_period,
    ~data.frame(
      datetime = max(.x$datetime),
      soiltemp_roll_ER = mean(.x$temp_soil, na.rm = TRUE)
    )
  )

soiltemp_NEE <- filter(fluxes2021, type == "NEE") %>%
  slide_period_dfr(
    # .,
    .$datetime,
    "hour",
    .every = roll_period,
    ~data.frame(
      datetime = max(.x$datetime),
      soiltemp_roll_NEE = mean(.x$temp_soil, na.rm = TRUE)
    )
  )



fluxes2021 <- left_join(fluxes2021, soiltemp_ER) %>% 
  left_join(soiltemp_NEE) %>% 
  fill(soiltemp_roll_NEE, .direction = "up") %>% 
  fill(soiltemp_roll_ER, .direction = "up") %>% 
  mutate(
    comments = case_when(
      is.na(temp_soil) == TRUE
      # & type != "SoilR"
      # & type == ("ER" | "NEE")
      ~ paste0(comments,  ";", " soil temp 3h period average"),
      TRUE ~ comments
    ),
    comments = str_replace_all(comments, "NA; ", ""),
    temp_soil = case_when(
      is.na(temp_soil) == TRUE
      & type == "ER"
      ~ soiltemp_roll_ER,
      is.na(temp_soil) == TRUE
      & type == "NEE"
      ~ soiltemp_roll_NEE,
      TRUE ~ temp_soil
    )

    
  ) %>% 
  select(!c(soiltemp_roll_NEE, soiltemp_roll_ER))




# write_csv(fluxes2021, "data_cleaned/c-flux/Three-D_c-flux_2021_cleaned.csv")


# fluxes quality ----------------------------------------------------------

# fluxes_quality <- fluxes2021 %>% mutate(
#   significance = case_when(
#     p.value <= 0.05 ~ "significant",
#     p.value > 0.05 ~ "non-significant"
#   ),
#   significance = as.factor(significance),
#   quality = case_when(
#     adj.r.squared >= 0.6 ~ "good",
#     adj.r.squared < 0.6 ~ "bad"
#   ),
#   quality = as.factor(quality)
# ) %>% 
#   select(turfID, quality, significance) %>% 
#   right_join(co2_cut) %>% 
#   group_by(fluxID) %>% 
#   mutate(
#     time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
#   ) %>% 
#   ungroup()
# 
# filter(fluxes_quality, campaign == 1) %>% 
#   ggplot(aes(x = datetime, y = CO2, color = quality)) +
#   geom_line(size = 0.2, aes(group = fluxID)) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 30, scales = "free") +
#   scale_color_manual(values = c(
#     "good" = "green",
#     "bad" = "red"
#   )) +
#   ggsave("threed_2021_quality_1B.png", height = 40, width = 80, units = "cm")
# 
# filter(fluxes_quality, quality == "bad") %>% 
#   ggplot(aes(x = time, y = CO2, color = significance)) +
#   geom_point(size = 0.2, aes(group = fluxID)) +
#   geom_smooth(method = "lm", se = FALSE, size = 0.5, fullrange = TRUE) +
#   # scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 10, scales = "free") +
#   scale_color_manual(values = c(
#     "significant" = "dark green",
#     "non-significant" = "red"
#   )) +
#   ggsave("threed_2021_significanceB.png", height = 40, width = 80, units = "cm")




