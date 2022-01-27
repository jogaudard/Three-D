#script to standardize data according to PAR and soil temperature

library("dataDownloader")
library(broom)
source("R/Load packages.R")
source("R/Rgathering/create meta data.R")



#download data from OSF and read it
# get_file(node = "pk4bg",
#          file = "Three-D_c-flux_2021.csv",
#          path = "data/C-Flux/summer_2021",
#          remote_path = "C-Flux")
 
flux <- read_csv("data_cleaned/c-flux/Three-D_c-flux_2021_cleaned.csv")




#adding meta data
flux <- left_join(flux, metaTurfID, by = "turfID")

#LRC
lrc_flux <- flux %>% 
  filter(
    type == "LRC1"
    | type == "LRC2"
    | type == "LRC3" 
    | type == "LRC4" 
    | type == "LRC5"
    )

#graph each light response curves
ggplot(lrc_flux, aes(x = PARavg, y = flux, color = turfID)) +
  geom_point(size = 0.1) +
  facet_wrap(vars(campaign)) +
  # geom_smooth(method = "lm", se = FALSE)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

#grouping per treatment instead of turfs
ggplot(lrc_flux, aes(x = PARavg, y = flux, color = warming)) +
  geom_point(size = 0.1) +
  facet_wrap(vars(campaign)) +
  # geom_smooth(method = "lm", se = FALSE)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

ggplot(lrc_flux, aes(x = PARavg, y = flux, color = warming)) +
  geom_point(size = 0.1) +
  # facet_wrap(vars(campaign)) +
  # geom_smooth(method = "lm", se = FALSE)
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE)

#extract the equation and correct all the NEE fluxes for PAR = 1000 micromol/s/m2

coefficients_lrc <- lrc_flux %>%
  group_by(warming, campaign) %>% 
  nest %>% 
  mutate(lm = map(data, ~ lm(flux ~ PARavg + I(PARavg^2), data = .x)),
         table = map(lm, tidy),
         table = map(table, select, term, estimate),
         table = map(table, pivot_wider, names_from = term, values_from = estimate)
         
  ) %>% 
  unnest(table) %>% 
  select(warming, `(Intercept)`, PARavg, `I(PARavg^2)`, campaign) %>% 
  rename(
    origin = "(Intercept)",
    a = "I(PARavg^2)",
    b = "PARavg"
  )


#what I want to do: predict flux at PAR = 1000, given the origin
#origini is calculated with coefficients from the model and flux and PAR value of specific flux
# corrected_flux = flux + a (1000^2 - PAR^2) + b (1000 - PAR)

PARfix <- 300 #PAR value at which we want the corrected flux to be for NEE
PARnull <- 0 #PAR value for ER

flux_corrected_PAR <- flux %>% 
  left_join(coefficients_lrc, by = c("warming", "campaign")) %>% 
  mutate(
    PAR_corrected_flux = 
      case_when( #we correct only the NEE
        type == "NEE" ~ flux + a * (PARfix^2 - PARavg^2) + b * (PARfix - PARavg),
        type == "ER" ~ flux + a * (PARnull^2 - PARavg^2) + b * (PARnull - PARavg)
      )
    # delta_flux = flux - corrected_flux
  )# %>% 
  # filter( #removing LRC now that we used them
  #   type == "NEE"
  #   | type == "ER"
  # )

#we can do the same for soil temperature
#let's have a look
filter(flux_corrected_PAR,
       type == "ER" |
         type == "NEE") %>% 
ggplot(aes(x = temp_soilavg, y = PAR_corrected_flux
                           , color = type
                           )) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  facet_wrap(vars(campaign))

filter(flux_corrected_PAR,
       type == "ER" |
         type == "NEE") %>%
  ggplot(aes(x = temp_soilavg, y = PAR_corrected_flux
             # , color = type
             )) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, fullrange = TRUE) +
  facet_grid(vars(warming), vars(campaign))

coefficients_soiltemp <- filter(flux_corrected_PAR, 
                                type == "ER" |
                                  type == "NEE"
                                ) %>%
  group_by(warming, campaign) %>% 
  nest %>% 
  mutate(lm = map(data, ~ lm(PAR_corrected_flux ~ temp_soilavg + I(temp_soilavg^2), data = .x)),
         table = map(lm, tidy),
         table = map(table, select, term, estimate),
         table = map(table, pivot_wider, names_from = term, values_from = estimate)
         
  ) %>% 
  unnest(table) %>% 
  select(warming, `(Intercept)`, temp_soilavg, `I(temp_soilavg^2)`, campaign) %>% 
  rename(
    origin2 = "(Intercept)",
    c = "I(temp_soilavg^2)",
    d = "temp_soilavg"
  )

soiltempfix <- 15
flux_corrected <- flux_corrected_PAR %>% 
  left_join(coefficients_soiltemp, by = c("warming", "campaign")) %>% 
  mutate(
    corrected_flux =
      PAR_corrected_flux + c * (soiltempfix^2 - temp_soilavg^2) + d * (soiltempfix - temp_soilavg),
      
    delta_flux = flux - corrected_flux
  ) %>% 
  select(!c(origin, a, b, origin2, c, d))

#visualize the difference between corrected and not corrected
# flux_corrected %>% 
#   filter( #removing LRC now that we used them
#     type == "NEE"
#     | type == "ER"
#   ) %>% 
# ggplot(aes(x = PARavg, y = delta_flux, color = warming)) +
#   geom_point() +
#   # geom_line() +
#   facet_grid(vars(campaign), vars(type), scales = "free")

# flux_corrected %>% 
#   # filter( #removing LRC now that we used them
#   #   type == "NEE"
#   #   | type == "ER"
#   # ) %>% 
#   ggplot() +
#   geom_point(aes(x = PARavg, y = flux, color = warming)) +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(x = PARavg, y = corrected_flux, color = warming))
  # geom_line() +
  # facet_grid(vars(campaign), vars(type), scales = "free")
flux_corrected %>% 
  filter( #removing LRC now that we used them
        type == "NEE"
        | type == "ER"
      ) %>%
  ggplot(aes(x = flux, y = corrected_flux, color = warming)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  facet_grid(vars(type), vars(campaign))

write_csv(flux_corrected, "data_cleaned/c-flux/Three-D_c-flux_2021.csv")

