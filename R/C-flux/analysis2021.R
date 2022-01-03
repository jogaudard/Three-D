library("dataDownloader")
library(broom)
source("R/Load packages.R")
source("R/Rgathering/create meta data.R")


get_file(node = "pk4bg",
         file = "Three-D_c-flux_2021_corrected.csv",
         path = "data/C-Flux/summer_2021",
         remote_path = "C-Flux")

flux <- read_csv("data/C-Flux/summer_2021/Three-D_c-flux_2021_corrected.csv") %>% 
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
    nitrogen = case_when(
      Nlevel %in% c(1:3) ~ 0,
      Nlevel == 4 ~ 0.5,
      Nlevel == 5 ~ 1,
      Nlevel == 6 ~ 5,
      Nlevel == 7 ~ 10,
      Nlevel == 8 ~ 50,
      Nlevel == 9 ~ 100,
      Nlevel == 10 ~ 150
    )
  ) %>% 
  select(turfID, type, comments, date, temp_soil, campaign, corrected_flux, site, block, grazing, warming, nitrogen) %>% 
  group_by(type, site, block, campaign, grazing) %>% #creating ID to connect transplant and ambient
  mutate(
    pairID = cur_group_id()
  ) %>% 
  ungroup()

#graphing
ggplot(flux2021, aes(x = temp_soil, y = corrected_flux, color = grazing)) +
  geom_point(size = 0.4) +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free") +
  ggsave("temp_soil_vs_flux_grazing.png", height = 20, width = 20, units = "cm")

ggplot(flux2021, aes(x = temp_soil, y = corrected_flux, color = nitrogen)) +
  geom_point(size = 0.4) +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free") +
  ggsave("temp_soil_vs_flux_Nlevel.png", height = 20, width = 20, units = "cm")
  
ggplot(flux2021, aes(x = nitrogen, y = corrected_flux, color = warming)) +
  geom_point(size = 0.4) +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free")
  
