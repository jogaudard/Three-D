library("dataDownloader")
library(broom)
source("R/Load packages.R")
source("R/Rgathering/create meta data.R")


get_file(node = "pk4bg",
         file = "Three-D_c-flux_2021.csv",
         path = "data_cleaned/c-flux",
         remote_path = "C-Flux")

flux <- read_csv("data_cleaned/c-flux/Three-D_c-flux_2021.csv") %>% 
  mutate(
    corrected_flux = case_when( # discard data of low quality
      p.value <= 0.05 & r.squared >= 0.7 ~ corrected_flux,
      p.value >= 0.05 & r.squared <= 0.7 ~ corrected_flux, #or should it be 0 in that case?
      ),
    PAR_corrected_flux = case_when( # discard data of low quality
      p.value <= 0.05 & r.squared >= 0.7 ~ PAR_corrected_flux,
      p.value >= 0.05 & r.squared <= 0.7 ~ PAR_corrected_flux, #or should it be 0 in that case?
    ),
    date = date(datetime)
  ) %>% 
  filter(
    type == "NEE" |
      # type == "SoilR" |
      type == "ER" #we do not use the LRC data
  ) %>% 
  select(turfID, type, comments, date, temp_soilavg, campaign, PAR_corrected_flux, corrected_flux)
  

# GEP
flux_gep <- flux %>%
    # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>% 
  select(!c(PAR_corrected_flux)) %>%
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

flux_gep_PAR <- flux %>% #to inlcude PAR-only corrected fluxes if we want to look at soil temperature
  # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>% 
  select(date, turfID, type, PAR_corrected_flux) %>%
  # select(campaign, turfID, date, type, corrected_flux) %>%
  pivot_wider(names_from = type, values_from = PAR_corrected_flux) %>% 
  # rename(
  #   ER = PAR_corrected_flux_ER,
  #   NEE = PAR_corrected_flux_NEE
  # ) %>%
  mutate(
    GEP = NEE - ER
  ) %>% 
  pivot_longer(c(ER, NEE, GEP), names_to = "type", values_to = "PAR_corrected_flux") #%>% 
  # mutate(
  #   temp_soil = case_when(
  #     type == "ER" ~ temp_soilavg_ER,
  #     type == "NEE" ~ temp_soilavg_NEE,
  #     type == "GEP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
  #   ),
  #   comments = case_when(
  #     type == "ER" ~ comments_ER,
  #     type == "NEE" ~ comments_NEE,
  #     type == "GEP" ~ comments_NEE
  #   )
  # ) %>% 
  # select(!c(temp_soilavg_ER, temp_soilavg_NEE, comments_ER, comments_NEE))

flux2021 <- left_join(flux_gep, metaTurfID, by = "turfID") %>% 
  left_join(flux_gep_PAR, by = c("turfID", "date", "type")) %>% 
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
    site = as.factor(site),
    block = factor(origBlockID),
    grazing = str_replace_all(grazing, c(
      "I" = "Intensive",
      "C" = "Control",
      "M" = "Medium",
      "N" = "Natural"
    )),
    grazing = factor(grazing, levels = c("Control", "Medium", "Intensive", "Natural")),
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
    # , nitrogen = as.factor(nitrogen)
  ) %>% 
  select(turfID, type, comments, date, temp_soil, campaign, PAR_corrected_flux, corrected_flux, site, block, grazing, warming, nitrogen) %>% 
  group_by(type, site, block, campaign, grazing) %>% #creating ID to connect transplant and ambient
  mutate(
    pairID = cur_group_id()
  ) %>% 
  ungroup()

#graphing
ggplot(flux2021, aes(x = temp_soil, y = PAR_corrected_flux, color = grazing)) +
  geom_point(size = 0.4) +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free") +
  ggsave("temp_soil_vs_flux_grazing.png", height = 20, width = 20, units = "cm")

ggplot(flux2021, aes(x = temp_soil, y = PAR_corrected_flux, color = nitrogen)) +
  geom_point(size = 0.4) +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free") +
  ggsave("temp_soil_vs_flux_nitrogen.png", height = 20, width = 20, units = "cm")
  
ggplot(flux2021, aes(x = nitrogen, y = PAR_corrected_flux, color = warming)) +
  geom_point(size = 0.4) +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm",
              # formula = y ~ poly(x, 2),
              se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free") +
  ggsave("nitrogen_vs_flux_warming.png", height = 20, width = 20, units = "cm")


ggplot(flux2021, aes(x = nitrogen, y = PAR_corrected_flux, color = warming)) +
  geom_point(size = 0.4) +
  # geom_line(aes(group = pairID)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free")

ggplot(flux2021, aes(x = nitrogen, y = corrected_flux, color = grazing)) +
  geom_point(size = 0.4) +
  geom_smooth(method = "lm",
              # formula = y ~ poly(x, 2),
              se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free") +
  labs(
    # title = "",
    caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
    color = "Grazing treatment",
    x = "Nitrogen addition [kg/ha/y]",
    y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
  ) +
  ggsave("nitrogen_vs_flux_grazing_fixedtemp2.png", height = 20, width = 20, units = "cm")

ggplot(flux2021, aes(x = nitrogen, y = corrected_flux, color = warming)) +
  geom_point(size = 0.4) +
  geom_smooth(method = "lm",
              # formula = y ~ poly(x, 2),
              se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign), scales = "free") +
  labs(
    # title = "",
    caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
    color = "Grazing treatment",
    x = "Nitrogen addition [kg/ha/y]",
    y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
  ) +
  ggsave("nitrogen_vs_flux_warming_fixedtemp2.png", height = 20, width = 20, units = "cm")

flux2021 %>% 
  filter(type == "ER") %>% 
  ggplot(aes(x = nitrogen, y = corrected_flux, color = warming, shape = site)) +
  geom_point(size = 1.5) +
  # geom_smooth(method = "lm",
              # formula = y ~ poly(x, 2),
              # se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(grazing), vars(campaign), scales = "fixed") +
  labs(
    title = "Ecosystem respiration (Three-D, 2021)",
    caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
    color = "Warming",
    x = "Nitrogen addition [kg/ha/y]",
    y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
  ) +
  scale_color_manual(values = c(
    "Ambient" = "#1e90ff",
    "Transplant" = "#ff0800"
  )) +
  scale_x_continuous(trans = 'log10') +
  ggsave("nitrogen_vs_ER_warming_fixedtemp.png", height = 20, width = 38, units = "cm")
       
flux2021 %>% 
  filter(type == "NEE") %>% 
  ggplot(aes(x = nitrogen, y = corrected_flux, color = warming, shape = site)) +
  geom_point(size = 0.4) +
  # geom_smooth(method = "lm",
  #             # formula = y ~ poly(x, 2),
  #             se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(grazing), vars(campaign), scales = "fixed") +
  labs(
    title = "Net ecosystem exchange",
    caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
    color = "Warming",
    x = "Nitrogen addition [kg/ha/y]",
    y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
  ) +
  scale_color_manual(values = c(
    "Ambient" = "#1e90ff",
    "Transplant" = "#ff0800"
  )) +
  ggsave("nitrogen_vs_NEE_warming_fixedtemp.png", height = 20, width = 20, units = "cm")

flux2021 %>% 
  filter(type == "GEP") %>% 
  ggplot(aes(x = nitrogen, y = corrected_flux, color = warming, shape = site)) +
  geom_point(size = 1.5) +
  # geom_smooth(method = "lm",
  #             # formula = y ~ poly(x, 2),
  #             se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(grazing), vars(campaign), scales = "fixed") +
  labs(
    title = "Gross ecosystem production (Three-D, 2021)",
    caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
    color = "Warming",
    x = "Nitrogen addition [kg/ha/y]",
    y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
  ) +
  scale_color_manual(values = c(
    "Ambient" = "#1e90ff",
    "Transplant" = "#ff0800"
  )) +
  scale_x_continuous(trans = 'log10') +
  ggsave("nitrogen_vs_GEP_warming_fixedtemp.png", height = 20, width = 38, units = "cm")

flux2021 %>% 
  filter(
    nitrogen == 0 &
      grazing == "Control"
  ) %>% 
  mutate(
    campaign = as.factor(campaign)
  ) %>% 
  ggplot(aes(x = campaign, y = corrected_flux)) +
  geom_boxplot(aes(fill = warming), position = "dodge") +
  # geom_col(aes(fill = warming), position = "dodge")
  facet_wrap(vars(type), ncol = 3) +
  labs(
    title = "Control plots only",
    caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
    fill = "Warming",
    x = "Campaigns",
    y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
  ) +
  scale_fill_manual(values = c(
    "Ambient" = "#1e90ff",
    "Transplant" = "#ff0800"
  )) +
  ggsave("controlplotsflux.png", height = 20, width = 38, units = "cm")

#let's try to compare paired (transplant vs ambient) plots
flux2021_delta <- flux2021 %>% 
  select(type, campaign, site, block, grazing, nitrogen, pairID, warming, corrected_flux) %>% 
  pivot_wider(names_from = "warming", values_from = "corrected_flux") %>% 
  mutate(
    delta_flux = Transplant - Ambient
  )

ggplot(
  flux2021_delta,
  aes(x = nitrogen, y = delta_flux, color = grazing)
) +
  geom_point() +
  geom_smooth(method = "lm",
              # formula = y ~ poly(x, 2),
              se = FALSE, size = 0.5, fullrange = TRUE) +
  facet_grid(vars(type), vars(campaign))

flux2021_slopes <- flux2021_delta %>% 
  drop_na(nitrogen, delta_flux) %>% 
  nest(-c(type, campaign, grazing)) %>% 
  # group_by(type, campaign, grazing) %>% 
  mutate(
    fit = map(data, ~lm(delta_flux ~ nitrogen, data = .)),
    results = map(fit, glance),
    slope = map(fit, tidy)
  ) %>% 
  unnest(c(results, slope), names_repair = "universal") %>% 
  unnest(data) %>% 
  filter(
    term == "nitrogen"
  ) %>% 
  rename(
    slope_flux_nitrogen = "estimate"
  ) %>% 
  select(type, campaign, grazing, site, block, nitrogen, pairID, Transplant, Ambient, delta_flux, slope_flux_nitrogen)
  # summarise(
  #     fit = map(data, ~lm(delta_flux ~ nitrogen, data = .)),
  #     results = map(fit, glance),
  #     slope = map(fit, tidy)
  # )

ggplot(flux2021_slopes) +
  geom_boxplot(aes(x = type, y = slope_flux_nitrogen, color = grazing)) 
  facet_wrap(vars(campaign))
  
flux2021 %>% 
    # filter(type == "ER") %>% 
    ggplot(aes(x = nitrogen, y = corrected_flux, color = warming)) +
    geom_point(size = 1.5) +
    # geom_smooth(method = "lm",
    #             # formula = y ~ poly(x, 2),
    #             se = FALSE, size = 0.5, fullrange = TRUE) +
    facet_grid(vars(grazing), vars(type), scales = "fixed") +
    labs(
      title = "Ecosystem fluxes (Three-D, 2021)",
      caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
      color = "Warming",
      x = "Nitrogen addition [kg/ha/y]",
      y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
    ) +
    scale_color_manual(values = c(
      "Ambient" = "#1e90ff",
      "Transplant" = "#ff0800"
    )) +
    scale_x_continuous(trans = 'log10')
 
flux2021 %>% 
  mutate(
    campaign = as.factor(campaign)
  ) %>% 
  ggplot(aes(x = type, y = corrected_flux)) +
  geom_boxplot(aes(fill = warming), position = "dodge") +
  # geom_col(aes(fill = warming), position = "dodge")
  # facet_wrap(vars(type), ncol = 3) +
  labs(
    title = "Warming treatment",
    caption = bquote(~CO[2]~'Flux standardized at PAR = 300 mol/'*m^2*'/s for NEE and PAR = 0 mol/'*m^2*'/s for ER, and soil temperature = 15 °C'),
    fill = "Warming",
    # x = "Campaigns",
    y = bquote(~CO[2]~'flux [mmol/'*m^2*'/h]')
  ) +
  scale_fill_manual(values = c(
    "Ambient" = "#1e90ff",
    "Transplant" = "#ff0800"
  ))

flux2021 %>% 
  mutate(
    type = as.factor(type),
    campaign = as.factor(campaign),
    warming = as.factor(warming)
  ) %>% 
  filter(
    type != "NEE"
  ) %>% 
  ggplot(aes(x = nitrogen, y = corrected_flux, shape = warming, color = site)) +
  geom_point(size = 1.5) +
  scale_shape_manual(values = c(Ambient = 16, Transplant = 1)) +
  # scale_linetype_manual(values = c(Ambient = "solid", Transplant = "dotted")) +
  # scale_y_log10() +
  scale_x_continuous(trans = 'log10') +
  # geom_smooth(method = "lm",
  #             # formula = y ~ poly(x, 2),
  #             se = FALSE, size = 0.5, fullrange = FALSE) +
  # facet_grid(rows = vars(site), cols = vars(grazing))
  facet_grid(cols = vars(grazing), rows = vars(type))
