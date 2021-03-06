library(tidyverse)
library(googlesheets4)
library(dplyr)
library(readr)
gs4_deauth()

NP_Species_21 <- read_sheet("https://docs.google.com/spreadsheets/d/1hfK3ptAkMU4AiM5PknHe4IebFl29wEWNdMKpc5nEC4M/edit#gid=0")
NP_Species_21

Houston_Species_21 <- read_sheet("1-VpDe1FBQKK1nWYI_HOV8qVGD1fSHKUoe65E11iqOEU")


data <-
  bind_rows(
    NP_Species_20, 
    Houston_Species_20
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


#FOR POSTER
data %>% 
  count(site, plot, treatment, name = "n_spp") %>% 
  ggplot() +
  geom_col(mapping = aes(x = plot, y = n_spp, fill = treatment)) +
  facet_wrap(~ site)

#Comparing controls from NP and Houston
data %>% 
  count(site, plot, treatment, name = "n_spp") %>% 
  filter(treatment == "C") %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = site, y = n_spp, color = treatment),
              size=2)

ggplot(data = NP_Species_20) +
  geom_histogram(mapping = aes(x = cover, fill = taxa), binwidth = 1)

ggplot(data = NP_Species_20) +
  geom_histogram(mapping = aes(x = cover), binwidth = 1) +
  facet_wrap(~ taxa)
