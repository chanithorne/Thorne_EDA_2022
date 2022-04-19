library(tidyverse)
library(googlesheets4)
gs4_deauth()

NP_Species_18 <- read_sheet("https://docs.google.com/spreadsheets/d/1bBaDxI7PnZIbDDT4N_kmLap8wHPJO6cyj4Qaxv3xwvI/edit#gid=0")
NP_Species_18

Houston_Species_18 <- read_sheet("https://docs.google.com/spreadsheets/d/1dE-9shHj4D0H8DHCF25JeJpycBEBOA5x38j4uQILnJs/edit#gid=0")
Houston_Species_18

data <-
  bind_rows(
    NP_Species_18, 
    Houston_Species_18
  ) %>% 
  mutate(
    plot = factor(plot)
  )

count(data, taxa) %>% 
  count()

count(data, site, taxa) %>% 
  count(site)

data_presence_site <- 
  data %>% 
  distinct(site, taxa) %>% 
  mutate(present = TRUE) %>% 
  pivot_wider(
    names_from = "site", 
    values_from = "present", 
    values_fill = FALSE
  ) %>% 
  print()

filter(
  data_presence_site,
  !Houston,
  !NP
) %>% 
  arrange(taxa)

np_species_div <- data(taxa, plot)
np_species_div

data %>% 
  count(site, plot, treatment, name = "n_spp") %>% 
  ggplot() +
  geom_col (size = 3) + (mapping = aes(x = plot, y = n_spp, fill = treatment)) +
  facet_wrap(~ site)

ggsave("SAC.png",
       height = 8,
       width = 12,
       units = "in",
       dpi = 400)
