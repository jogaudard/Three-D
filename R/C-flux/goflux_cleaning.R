# in this script I will try to use the GoFluxTourself package to see how it works and if I like it

# importing packages ----

my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate",
                 "broom",
                 "zoo",
                 "hms",
                 "GoFluxYourself",
                 "filesstrings"
                 )

lapply(my_packages, library, character.only = TRUE) 

# importing data ------

get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_field-record_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")


# Unzip files
zipFile <- "data/C-Flux/summer_2020/Three-D_cflux_2020.zip"
if(file.exists(zipFile)){
  outDir <- "data/C-Flux/summer_2020"
  unzip(zipFile, exdir = outDir)
}

file.remove(zipFile) # let's free some space

location <- "data/C-Flux/summer_2020/rawData"
CO2_files <- list.files(location, pattern = "*CO2*", full.names = TRUE)

move_files(CO2_files, "data/C-Flux/summer_2020/rawData/CO2")


# from exemple
file.path <- system.file("extdata", "LGR/example_LGR.txt", package = "GoFluxYourself")
example_LGR_imp <- LGR_import(inputfile = file.path)

aux.path <- system.file("extdata", "LGR/example_LGR_aux.txt", 
                        package = "GoFluxYourself")
auxfile <- read.delim(aux.path) %>% 
  mutate(start.time = as.POSIXct(start.time, tz = "UTC"))

  example_LGR_ow <- obs.win(inputfile = example_LGR_imp, auxfile = auxfile,
                          obs.length = 180, shoulder = 30)
# splitting data  and attributing IDs -----

data <- import2RData()
