# in this script I will try to use the GoFluxTourself package to see how it works and if I like it

# importing packages ----

my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate",
                 "broom",
                 "zoo",
                 "hms",
                 "GoFluxYourself"
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

# splitting data  and attributing IDs
