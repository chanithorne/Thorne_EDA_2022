library(tidyverse)
library(googlesheets4)
library(dplyr)
library(readr)
gs4_deauth()

NP_Species_21 <- read_sheet("https://docs.google.com/spreadsheets/d/1hfK3ptAkMU4AiM5PknHe4IebFl29wEWNdMKpc5nEC4M/edit#gid=0")
NP_Species_21

Houston_Species_21 <- read_sheet("1-VpDe1FBQKK1nWYI_HOV8qVGD1fSHKUoe65E11iqOEU")

#Read In Native/ Non-native Data

np_url_21 <- "https://docs.google.com/spreadsheets/d/1hfK3ptAkMU4AiM5PknHe4IebFl29wEWNdMKpc5nEC4M/edit#gid=0"
np_data_21 <-
  read_sheet(np_url_21) %>% 
  left_join(read_sheet(np_url_21, sheet = 2)) %>% 
  print()

houston_url_21 <- "https://docs.google.com/spreadsheets/d/1-VpDe1FBQKK1nWYI_HOV8qVGD1fSHKUoe65E11iqOEU/edit#gid=1776011343"

houston_data_21 <-
  read_sheet(houston_url_21) %>% 
  left_join(read_sheet(houston_url_21, sheet = 2)) %>% 
  print()


data <-
  bind_rows(
    np_data_21, 
    houston_data_21
  ) %>% 
  mutate(
    plot = factor(plot)
  )
data

data_presence_site <- 
  data %>% 
  distinct(site, status) %>% 
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
  arrange(status)

#Comparing controls from NP and Houston
data %>% 
  count(site, plot, treatment, status, name = "n_spp") %>% 
  ggplot() +
  geom_jitter(mapping = aes(x = site, y = n_spp, color = treatment),
              size=2)

ggplot(data = data) +
  geom_histogram(mapping = aes(x = cover, fill = status), binwidth = 5)


#Save Histogram Image
ggsave("Rplot.png",
       height = 8,
       width = 12,
       units = "in",
       dpi = 400)

#Data Analysis

data_grouped <- group_by(data, status)
data_grouped

data_summary <-
  summarize(
    data_grouped, 
    mean_count = mean(cover, na.rm = TRUE),
    sem = sd(cover, na.rm = TRUE) / sqrt(n()),
    ci_upper_limit = mean_count + 1.96 * sem,
    ci_lower_limit = mean_count - 1.96 * sem
  )
data_summary

ggplot(data = data) +
  geom_jitter(mapping = aes(x = status, y = cover), alpha = .1,
              shape= 16, size= 2) +
  geom_linerange(
    data = data_summary, 
    mapping = aes(x = status, y = mean_count, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "red", size= 2
  )+ 
  geom_point(
    data = data_summary, 
    mapping = aes(x = status, y = mean_count),
    color = "red", size= 4
  )+ 
  scale_y_log10()+
  labs(
    x= NULL,
    y= "Cover (%)"
  )

ggsave("RAnalysis.png",
       height = 8,
       width = 12,
       units = "in",
       dpi = 400)
