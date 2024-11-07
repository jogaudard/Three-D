library("dataDownloader")
library(broom)
library(fs)
library(zoo)
library(slider)
library(fluxible)
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
  ) 

#matching the CO2 concentration data with the turfs using the field record
conc <- flux_match(
  conc_raw,
  record,
  conc_col = "CO2"
  )

str(conc)

#need to clean PAR, temp_air, temp_soil

#put NA for when the soil temp sensor was not pluged in

conc <- conc %>% 
  mutate(
    temp_soil = case_when(
      comments == "soilT logger not plugged in" ~ NA_real_,
      comments == "Soil T NA" ~ NA_real_,
      TRUE ~ temp_soil
    )
  )






#PAR: same + NA for soilR and ER

co2_cut_clean <- co2_cut_clean %>% 
  mutate(
    PAR = case_when(
      # type == "ER" ~ NA_real_, #no PAR for ecosystem respiration (but maybe I should keep it??)
      # type == "SoilR" ~ NA_real_, #no PAR with soil respiration, the sensor was somewhere else anyway
      fluxID == 32 & datetime <= ymd_hms("2021-06-04T14:43:21") ~ NA_real_, # for when the sensor messed up because of the heat (should see a drop close to 0 or negative values)
      # datetime %in% c(ymd_hms(""):ymd_hms("")), # for when the sensor messed up because of the heat (should see a drop close to 0 or negative values)
      fluxID %in% c(
        2,
        6,
        14,
        16,
        18,
        30,
        38,
        40,
        54,
        56,
        66,
        68,
        70,
        72,
        85,
        139,
        141,
        143,
        361,
        367,
        382
        
      )
      
      ~ NA_real_,
      # fluxID == c(2, 6, 14, 16, 18, 31, 33, 39, 41, 57, 59, 69, 71, 74, 76, 89, 145, 147, 149) ~ NA_real_,
      # datetime %in% c(ymd_hms("2021-06-05T10:37:55"):ymd_hms("2021")) ~ NA_real_,
      fluxID == 64 & datetime >= ymd_hms("2021-06-05T10:37:55") ~ NA_real_,
      fluxID == 200 & datetime <= ymd_hms("2021-06-21T12:12:57") ~ NA_real_,
      fluxID == 200 & datetime >= ymd_hms("2021-06-21T12:13:07") ~ NA_real_,
      datetime %in% c(ymd_hms("2021-06-23T14:36:20"):ymd_hms("2021-06-23T14:36:40")) ~ NA_real_,
      fluxID == 277 & datetime <= ymd_hms("2021-06-23T14:45:35") ~ NA_real_,
      fluxID == 277 & datetime >= ymd_hms("2021-06-23T14:46:00") ~ NA_real_,
      datetime %in% c(ymd_hms("2021-06-23T13:46:50"):ymd_hms("2021-06-23T13:47:10")) ~ NA_real_,
      fluxID == 698 & datetime <= ymd_hms("2021-08-16T16:22:50") ~ NA_real_,
      fluxID == 895 & datetime >= ymd_hms("2021-08-19T16:00:30") ~ NA_real_,
      fluxID == 901 & datetime <= ymd_hms("2021-08-19T16:09:50") ~ NA_real_,
      fluxID == 1054 & datetime <= ymd_hms("2021-09-06T15:02:50") ~ NA_real_,
      datetime %in% c(ymd_hms("2021-09-09T10:52:10"):ymd_hms("2021-09-09T10:53:20")) ~ NA_real_,
      type == "ER" & PAR <= 0 ~ 0, #close to 0 the logger can have some negative values but it is 0 in reality
      fluxID == 1301 & datetime <= ymd_hms("2021-09-09T15:05:15") ~ NA_real_,
      fluxID == 82 & PAR <= 0 ~ 0, # negative values close to 0
      datetime %in% c(ymd_hms("2021-06-04T10:29:40"):ymd_hms("2021-06-04T10:29:52")) ~ NA_real_,
      datetime %in% c(ymd_hms("2021-06-21T12:33:40"):ymd_hms("2021-06-21T12:33:50")) ~ NA_real_,
      TRUE ~ as.numeric(PAR)
      )
  )

#replacing PAR Na by 2h before/after average
# roll_period <- 1
# 
# PAR_ER <- filter(co2_cut, type == "ER") %>% 
#   slide_period_dfr(
#     # .,
#     .$datetime,
#     "hour",
#     .every = roll_period,
#     ~data.frame(
#       datetime = max(.x$datetime),
#       PAR_roll = mean(.x$PAR, na.rm = TRUE)
#     )
#   )
# 
# PAR_NEE <- filter(co2_cut, type == "NEE") %>% 
#   slide_period_dfr(
#     # .,
#     .$datetime,
#     "hour",
#     .every = roll_period,
#     ~data.frame(
#       datetime = max(.x$datetime),
#       PAR_roll = mean(.x$PAR, na.rm = TRUE)
#     )
#   )
# 
# 
# co2_cut <- left_join(co2_cut, PAR_ER)


# filter(co2_cut, campaign == 1) %>% 
# filter(co2_cut, type == "NEE") %>% #cleaned
#   ggplot(aes(x = datetime, y = PAR)) +
#     geom_line(size = 0.2, aes(group = fluxID)) +
#     scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#     # scale_x_date(date_labels = "%H:%M:%S") +
#     facet_wrap(vars(fluxID), ncol = 40, scales = "free") +
#     ggsave("threed_2021_detail_PAR_NEE.png", height = 40, width = 80, units = "cm")

# filter(co2_cut, type == "ER") %>% #cleaned
#   ggplot(aes(x = datetime, y = PAR)) +
#   geom_line(size = 0.2, aes(group = fluxID)) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 40, scales = "free") +
#   ggsave("threed_2021_detail_PAR_ER.png", height = 40, width = 80, units = "cm")

filter(co2_cut_clean, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

co2_cut_clean %>% filter(type == "NEE") %>% 
  ggplot(aes(datetime, PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID))

co2_cut_clean %>% filter(fluxID %in% c(174, 42)) %>% 
  ggplot(aes(datetime, PAR)) +
  geom_point() +
  facet_wrap(vars(fluxID), scales = "free") +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M")

filter(co2_cut_clean, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

co2_cut_clean %>% filter(type == "ER") %>% 
  ggplot(aes(datetime, PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID))

co2_cut_clean %>% filter(fluxID %in% c(21, 90, 476)) %>% 
  ggplot(aes(datetime, PAR)) +
  geom_point() +
  facet_wrap(vars(fluxID), scales = "free") +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M")


# filter(co2_cut, 
#            type == "LRC1"
#          | type == "LRC2"
#          | type == "LRC3" 
#          | type == "LRC4" 
#          | type == "LRC5"
#        ) %>% #cleaned
#   ggplot(aes(x = datetime, y = PAR)) +
#   geom_line(size = 0.2, aes(group = fluxID)) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 10, scales = "free") +
#   ggsave("threed_2021_detail_PAR_LRC.png", height = 40, width = 80, units = "cm")


##Next part is for calculating the fluxes, once the data have been cleaned

#first, a function to calculate fluxes
# (26.01.2022) made a new function which I believe is better, old one is in comment further

# flux.calc2 <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
#                        chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
#                        tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
#                        atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
#                        plot_area = 0.0625 # area of the plot in m^2, default for Three-D
# )
# {
#   R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
#   vol = chamber_volume + tube_volume
#   # co2conc <- co2_cut
#   slopes <- co2conc %>% 
#     group_by(fluxID) %>% 
#     mutate(
#       time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
#     ) %>% 
#     select(fluxID, time, CO2) %>%
#     do({model = lm(CO2 ~ time, data=.)    # create your model
#     data.frame(tidy(model),              # get coefficient info
#                glance(model))}) %>%          # get model info
#     filter(term == "time") %>% 
#     rename(slope = estimate) %>% 
#     select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs) %>% 
#     ungroup()
#   
#   means <- co2conc %>% 
#     group_by(fluxID) %>% 
#     summarise(
#       PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
#       temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
#       + 273.15, #transforming in kelvin for calculation
#       temp_soilavg = mean(temp_soil, na.rm = TRUE) #mean value of temp_soil for each flux
#     ) %>% 
#     ungroup()
#   
#   fluxes_final <- left_join(slopes, means, by = "fluxID") %>% 
#     left_join(
#       co2conc,
#       by = "fluxID"
#     ) %>% 
#     select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs, PARavg, temp_airavg, temp_soilavg, turfID, type, campaign, comments, start_window) %>% 
#     distinct() %>% 
#     rename(
#       datetime = start_window
#     ) %>% 
#     mutate(
#       flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
#       *3600 #secs to hours
#       /1000 #micromol to mmol
#     ) %>% #flux is now in mmol/m^2/h, which is more common
#     arrange(datetime) %>% 
#     select(!slope)
#   
#   return(fluxes_final)
#   
# }

# flux.calc <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
#                       chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
#                       tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
#                       atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
#                       plot_area = 0.0625 # area of the plot in m^2, default for Three-D
# )
# {
#   R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
#   vol = chamber_volume + tube_volume
#   fluxes_final <- co2conc %>% 
#     # group_by(ID) %>% 
#     nest(-fluxID) %>% 
#     mutate(
#       data = map(data, ~.x %>% 
#                    mutate(time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"), #add a column with the time difference between each measurements and the beginning of the measurement. Usefull to calculate the slope.
#                           PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
#                           temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of Temp_air for each flux
#                           + 273.15, #transforming in kelvin for calculation
#                           temp_soilavg = mean(temp_soil, na.rm = TRUE) #mean value of temp_soil for each flux
#                    )), 
#       fit = map(data, ~lm(CO2 ~ time, data = .)), #fit is a new column in the tibble with the slope of the CO2 concentration vs time (in secs^(-1))
#       # slope = map_dbl(fit, "time")
#       results = map(fit, glance), #to see the coefficients of the model
#       slope = map(fit, tidy) #creates a tidy df with the coefficients of fit
#     ) %>% 
#     
#     unnest(results, slope) %>% 
#     unnest(data) %>% 
#     filter(term == 'time'  #filter the estimate of time only. That is the slope of the CO2 concentration. We need that to calculate the flux.
#            # & r.squared >= 0.7 #keeping only trendline with an r.squared above or equal to 0.7. Below that it means that the data are not good quality enough
#            # & p.value < 0.05 #keeping only the significant fluxes
#     ) %>% 
#     # select(ID, Plot_ID, Type, Replicate, Remarks, Date, PARavg, Temp_airavg, r.squared, p.value, estimate, Campaign) %>% #select the column we need, dump the rest
#     distinct(fluxID, turfID, type, comments, date, PARavg, temp_airavg, temp_soilavg, r.squared, p.value, estimate, campaign, .keep_all = TRUE) %>%  #remove duplicate. Because of the nesting, we get one row per Datetime entry. We only need one row per flux. Select() gets rid of Datetime and then distinct() is cleaning those extra rows.
#     #calculate fluxes using the trendline and the air temperature
#     mutate(flux = (estimate * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
#            *3600 #secs to hours
#            /1000 #micromol to mmol
#     ) %>%  #flux is now in mmol/m^2/h, which is more common
#     select(datetime, fluxID, turfID, type, comments, date, PARavg, temp_airavg, temp_soilavg, r.squared, p.value, nobs, flux, campaign)
#   
#   return(fluxes_final)
#   
# }



fluxes2021 <- flux.calc3(co2_cut_clean) %>% 
  mutate(
    PARavg = case_when(
      is.nan(PARavg) == TRUE ~ NA_real_, #mean(PAR) returned NaN when PAR was all NAs but it is missing values
      TRUE ~ as.numeric(PARavg)
    )
  )

p = 0.01
R2 = 0.7

flux_to_check <- fluxes2021 %>% filter(
  (type == "ER" & flux <= 0)
  | (p.value > p  & adj.r.squared < R2 & type == "NEE")
  | (p.value > p  & adj.r.squared < R2 & type == "ER")
  | (p.value <= p & adj.r.squared < R2)
) %>% 
  select(fluxID) %>% 
  deframe()

co2_cut %>% filter(
  fluxID %in% flux_to_check
) %>% 
  ggplot(aes(x = datetime, y = CO2, color = cut)) +
    geom_line(size = 0.2, aes(group = fluxID)) +
    scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
    # scale_x_date(date_labels = "%H:%M:%S") +
    facet_wrap(vars(fluxID), ncol = 20, scales = "free")

fluxes2021 <- fluxes2021 %>% 
  mutate(
    flux = case_when( # discard data of low quality
      type == "ER" & flux < 0 ~ NA_real_,
      p.value > p  & adj.r.squared < R2 & type == "NEE" ~ 0,
      p.value > p  & adj.r.squared < R2 & type == "ER" ~ NA_real_,
      p.value <= p & adj.r.squared < R2 ~ NA_real_,
      p.value > p & adj.r.squared >= R2 ~ flux,
      p.value <= p & adj.r.squared >= R2 ~ flux
      # TRUE ~ corrected_flux
    )
  )

fluxes2021 %>% filter(type == "ER") %>% 
  summarise(
    rangeER = range(flux, na.rm = TRUE)
  )

  
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
      PAR_roll_ER = mean(.x$PARavg, na.rm = TRUE)
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
      PAR_roll_NEE = mean(.x$PARavg, na.rm = TRUE)
    )
  )



fluxes2021 <- left_join(fluxes2021, PAR_ER) %>% 
  left_join(PAR_NEE) %>% 
  fill(PAR_roll_NEE, .direction = "up") %>% 
  fill(PAR_roll_ER, .direction = "up") %>% 
  mutate(
    comments = case_when(
      is.na(PARavg) == TRUE
      # & type == ("ER" | "NEE")
      ~ paste0(comments,  ";", " PAR 3h period average"),
      TRUE ~ comments
    ),
    comments = str_replace_all(comments, "NA; ", ""),
    PARavg = case_when(
      is.na(PARavg) == TRUE
      & type == "ER"
      ~ PAR_roll_ER,
      is.na(PARavg) == TRUE
      & type == "NEE"
      ~ PAR_roll_NEE,
      TRUE ~ PARavg
    )
      # replace_na(PARavg,
      #                   case_when(
      #                     type == "ER" ~ PAR_roll_ER,
      #                     type == "NEE" ~ PAR_roll_NEE
      #                   )
      #                   )
    
  ) %>% 
  select(!c(PAR_roll_NEE, PAR_roll_ER))
  # rename(
  #   date_time = datetime,
  #   turfID = turf_ID
  # )

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
      soiltemp_roll_ER = mean(.x$temp_soilavg, na.rm = TRUE)
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
      soiltemp_roll_NEE = mean(.x$temp_soilavg, na.rm = TRUE)
    )
  )



fluxes2021 <- left_join(fluxes2021, soiltemp_ER) %>% 
  left_join(soiltemp_NEE) %>% 
  fill(soiltemp_roll_NEE, .direction = "up") %>% 
  fill(soiltemp_roll_ER, .direction = "up") %>% 
  mutate(
    comments = case_when(
      is.na(temp_soilavg) == TRUE
      # & type != "SoilR"
      # & type == ("ER" | "NEE")
      ~ paste0(comments,  ";", " soil temp 3h period average"),
      TRUE ~ comments
    ),
    comments = str_replace_all(comments, "NA; ", ""),
    temp_soilavg = case_when(
      is.na(temp_soilavg) == TRUE
      & type == "ER"
      ~ soiltemp_roll_ER,
      is.na(temp_soilavg) == TRUE
      & type == "NEE"
      ~ soiltemp_roll_NEE,
      TRUE ~ temp_soilavg
    )
    # replace_na(PARavg,
    #                   case_when(
    #                     type == "ER" ~ PAR_roll_ER,
    #                     type == "NEE" ~ PAR_roll_NEE
    #                   )
    #                   )
    
  ) %>% 
  select(!c(soiltemp_roll_NEE, soiltemp_roll_ER))
# rename(
#   date_time = datetime,
#   turfID = turf_ID
# )



write_csv(fluxes2021, "data_cleaned/c-flux/Three-D_c-flux_2021_cleaned.csv")


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




