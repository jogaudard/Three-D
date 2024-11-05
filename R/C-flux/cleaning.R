# This script is to clean raw data from various loggers into one datafile with calculated fluxes
library(fs)
library("dataDownloader")
library(broom)
library("fluxible")
source("R/Load packages.R")


#download and unzip files from OSF
get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_field-record_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_cutting_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

# Unzip files
zipFile <- "data/C-Flux/summer_2020/Three-D_cflux_2020.zip"
if(file.exists(zipFile)){
  outDir <- "data/C-Flux/summer_2020"
  unzip(zipFile, exdir = outDir)
}

location <- "data/C-Flux/summer_2020/rawData" #location of datafiles
#import all squirrel files and select date/time and CO2_calc columns, merge them, name it fluxes
fluxes <-
  dir_ls(location, regexp = "*CO2*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% 
  rename(CO2 = "CO2 (ppm)") %>%  #rename the column to get something more practical without space
  mutate(
    date = dmy(Date), #convert date in POSIXct
    datetime = as_datetime(paste(date, Time))  #paste date and time in one column
    ) %>%
  select(datetime,CO2)

#import date/time and PAR columns from PAR file
PAR <-
  list.files(path = location, pattern = "*PAR*", full.names = T) %>% 
  map_df(~read_table2(., "", na = c("NA"), col_names = paste0("V",seq_len(12)))) %>% #need that because logger is adding columns with useless information
  rename(date = V2, time = V3, PAR = V4) %>% 
  mutate(
    PAR = as.numeric(as.character(.$PAR)), #removing any text from the PAR column (again, the logger...)
    datetime = paste(date, time),
    datetime = ymd_hms(datetime)
    ) %>% 
  select(datetime, PAR)

#import date/time and value column from iButton file

temp_air <-dir_ls(location, regexp = "*temp*") %>% 
  map_dfr(read_csv,  na = c("#N/A"), skip = 20, col_names = c("datetime", "unit", "temp_value", "temp_dec"), col_types = "ccnn") %>%
  mutate(temp_dec = replace_na(temp_dec,0),
    temp_air = temp_value + temp_dec/1000, #because stupid iButtons use comma as delimiter AND as decimal point
    datetime = dmy_hms(datetime)
    ) %>% 
  select(datetime, temp_air)


#join the df


conc2020 <- fluxes %>% 
  left_join(PAR, by = "datetime") %>% 
  left_join(temp_air, by = "datetime")

#import the record file

record2020 <- read_csv("data/C-Flux/summer_2020/Three-D_field-record_2020.csv", na = c(""), col_types = "ccntDfc") %>% 
  drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
  mutate(
    start = ymd_hms(paste(date, starting_time)) #converting the date as posixct, pasting date and starting time together
  ) %>% 
  rename(plot_ID = turf_ID)



# match 2020 concentration df


