library("dataDownloader")
source("R/Load packages.R")
source("https://raw.githubusercontent.com/jogaudard/common/master/soil_moist_tomst.R")

#getting the raw data
get_file(node = "pk4bg",
         file = "Three-D_soil-moisture_2021.csv",
         path = "data/climate",
         remote_path = "RawData/Climate")

get_file(node = "pk4bg",
         file = "THREE-D_clean_microclimate_2019-2021.csv.zip",
         path = "data/climate",
         remote_path = "Climate")

soilmoisture <- read_csv("data/climate/Three-D_soil-moisture_2021.csv") %>% 
  mutate(
    soil_moisture = str_replace(soil_moisture,
                                "ABOVE",
                                "100"),
    soil_moisture = as.double(soil_moisture)
  )

unzip("data/climate/THREE-D_clean_microclimate_2019-2021.csv.zip", exdir = "data/climate")
threed_microclimate_full <- read_csv("data/climate/THREE-D_clean_microclimate_2019-2021.csv", col_type = "Tffffffffffddddf??TTcc" )
  
threed_microclimate <- threed_microclimate_full %>% 
  select(date_time, turfID, soilmoisture, soil_temperature) %>% 
  mutate(
    date = date(date_time)
  ) %>% 
  group_by(date, turfID) %>% 
  summarise(
    daily_soilmoisture_raw = mean(soilmoisture, na.rm = TRUE)
  ) %>% 
  mutate(
    # daily_soilmoisture = soil.moist(daily_soilmoisture_raw, soil_temperature)
    daily_soilmoisture = 100 * daily_soilmoisture_raw #to compare percentage
  ) %>% 
  filter(
    date >= ymd("2021-06-01") &
      date <= ymd("2021-09-30")
  )

soilmoisture_comparison <- full_join(soilmoisture, threed_microclimate) %>% 
  rename(
    manual = "soil_moisture",
    logger = "daily_soilmoisture"
  ) %>% 
  pivot_longer(c(manual, logger), names_to = "measurement", values_to = "soilmoisture") %>% 
  mutate(
    campaign = as.factor(campaign)
  )

soilmoisture_avg <- soilmoisture %>% 
  group_by(campaign, turfID, date) %>% 
  summarise(
    mean = mean(soil_moisture, na.rm = TRUE),
    SD = sd(soil_moisture, na.rm = TRUE)
  )

ggplot(soilmoisture_avg, aes(date, SD)) +
  geom_point()

ggplot(soilmoisture_comparison, aes(x = date, y = soilmoisture, color = measurement)) +
  geom_point()

soilmoisture_comparison %>% filter(
  !is.na(campaign)
) %>% 
ggplot(aes(x = campaign, y = soilmoisture, color = measurement)) +
  geom_boxplot() +
  ggsave("soilmoisture_comparison.png", height = 10, width = 15, units = "cm")

write_csv(soilmoisture, "data_cleaned/climate/Three-D_soil-moisture_2021.csv")
# ggplot(soilmoisture_comparison, aes(y = soilmoisture, color = measurement)) +
#   geom_line(aes(x = ))