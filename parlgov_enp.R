library(tidyverse) 
library(lubridate)

elec_url <- "https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_election.csv"

elec_raw <- read_csv(elec_url)

elec_de <- elec_raw %>% 
  filter(country_name_short == "DEU")

plot_enp <- elec_de %>% 
  filter(election_type == "parliament") %>% 
  mutate(vote_share_0 = vote_share / 100) %>% 
  group_by(election_date) %>% 
  summarise(enp = 1 / sum(vote_share_0^2)) %>% 
  ggplot(aes(x = election_date, y = enp)) + 
  geom_point() + 
  geom_line() +
  theme_light() +
  scale_x_date(breaks = seq(as.Date("1920-01-01"), as.Date("2020-01-01"), "10 years"), date_labels = "%Y") +
  theme(text = element_text(family = "Corbel"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Election Date",
       y = "ENP",
       caption = "Plot created by Lukas Warode\n Data used from ParlGov database (Döring/Manow)") 

plot_enp

ggsave("plot_enp.png",
       plot = plot_enp,
       width = 8,
       height = 4.5)
