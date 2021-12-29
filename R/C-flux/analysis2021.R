library("dataDownloader")
library(broom)
source("R/Load packages.R")
source("R/Rgathering/create meta data.R")


get_file(node = "pk4bg",
         file = "Three-D_c-flux_2021.csv",
         path = "data/C-Flux/summer_2021",
         remote_path = "C-Flux")

flux <- read_csv("data/C-Flux/summer_2021/Three-D_c-flux_2021.csv") %>% 
  mutate(
    corrected_flux = case_when( # discard data of low quality
      p.value <= 0.05 & r.squared >= 0.7 ~ corrected_flux,
      p.value >= 0.05 & r.squared <= 0.7 ~ corrected_flux, #or should it be 0 in that case?
      )
  ) %>% 
  filter(
    type == "NEE" |
      type == "ER" #we do not use the LRC data
  ) %>% 
  select(turfID, type, comments, date, temp_soilavg, campaign, corrected_flux)
  

# GEP
flux_gep <- flux %>%
    # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>% 
  # select(!c(comments)) %>%
  # select(campaign, turfID, date, type, corrected_flux) %>%
  pivot_wider(names_from = type, values_from = c(corrected_flux, temp_soilavg, comments)) %>% 
  rename(
    ER = corrected_flux_ER,
    NEE = corrected_flux_NEE
  ) %>%
  mutate(
    GEP = NEE - ER
  ) %>% 
  pivot_longer(c(ER, NEE, GEP), names_to = "type", values_to = "corrected_flux") %>% 
  mutate(
    temp_soil = case_when(
      type == "ER" ~ temp_soilavg_ER,
      type == "NEE" ~ temp_soilavg_NEE,
      type == "GEP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
    ),
    comments = case_when(
      type == "ER" ~ comments_ER,
      type == "NEE" ~ comments_NEE,
      type == "GEP" ~ comments_NEE
    )
  ) %>% 
  select(!c(temp_soilavg_ER, temp_soilavg_NEE, comments_ER, comments_NEE))

flux2021 <- left_join(flux_gep, metaTurfID, by = "turfID") %>% 
  mutate(
    warming = str_replace_all(warming, c(
      "W" = "Transplant",
      "A" = "Ambient"
    )),
    site = str_replace_all(origSiteID, c(
      "Vik" = "Vikesland",
      "Joa" = "Joasete",
      "Lia" = "Liahovden"
    )),
    block = factor(origBlockID),
    grazing = str_replace_all(grazing, c(
      "I" = "Intensive",
      "C" = "Control",
      "M" = "Medium",
      "N" = "Natural"
    )),
    Nlevel = as.factor(Nlevel)
  ) %>% 
  select(turfID, type, comments, date, temp_soil, campaign, corrected_flux, site, block, grazing, warming, Nlevel) %>% 
  group_by(type, site, block, campaign, grazing) %>% #creating ID to connect transplant and ambient
  mutate(
    pairID = cur_group_id()
  ) %>% 
  ungroup()

#graphing
ggplot(flux2021, aes(x = temp_soil, y = corrected_flux, color = grazing)) +
  geom_point() +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(vars(type), vars(campaign))

ggplot(flux2021, aes(x = temp_soil, y = corrected_flux, color = Nlevel)) +
  geom_point() +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(vars(type), vars(campaign))
  
  
