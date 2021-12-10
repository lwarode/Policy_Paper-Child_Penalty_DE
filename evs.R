library(tidyverse)
library(haven)
library(labelled)
# library(sjlabelled)
library(sjmisc)

evs_raw <- read_dta("ZA7505_v2-0-0.dta")

evs_de <- evs_raw %>% 
  filter(cntry_AN == "DE")

evs_de %>% 
  select(reg_nuts1) %>% 
  distinct()

evs_de %>% 
  select(reg_nuts2) %>% 
  distinct()

evs_de %>% 
  select(reg_iso) %>% 
  distinct() -> state_codes

# state_codes %>% 
#   mutate(german_state = unlabelled(reg_iso),
#          german_state = str_extract(german_state, "(?<=DE-[A-Z][A-Z][:space:]).+"))

state_codes %>% 
  mutate(
    german_state = reg_iso %>% 
      unlabelled() %>% 
      str_extract("(?<=DE-[A-Z][A-Z][:space:]).+")
  )

# evs_de %>% 
#   sjmisc::find_var(
#     "sex",
#     search = "name_label"
#   )


evs_raw <- read_dta("ZA7505_v2-0-0.dta")

evs_de <- evs_raw %>% 
  filter(cntry_AN == "DE")

evs_de %>% 
  .$C001 %>% 
  table

evs_de %>%
  mutate(
    german_state = reg_iso %>% 
      unlabelled() %>% 
      str_extract("(?<=DE-[A-Z][A-Z][:space:]).+")
  ) %>% 
  filter(C001 >= 1) %>% 
  group_by(german_state, C001) %>% 
  count() %>% 
  group_by(german_state) %>% 
  mutate(freq = (n / sum(n)) %>% round(3)) %>%   
  filter(C001 == 1) %>% 
  arrange(desc(freq)) %>% 
  na.omit() %>% 
  ggplot(aes(x = german_state %>% fct_reorder(-freq), y = freq)) + 
  geom_col() +
  labs(x = "",
       y = "",
       title = "Jobs scarce: Men should have more right to a job than women",
       subtitle = "% Agree per State") +
  scale_y_continuous(labels = scales::percent_format(1)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave(
  filename = "plot_coo1_de_state.pdf",
  plot = last_plot()
)

ggsave(
  filename = "plot_coo1_de_state.png",
  plot = last_plot()
)

# state_codes$label <- get_labels(state_codes$reg_iso)
# 
# label_to_colnames(state_codes)
# 
# var_label(state_codes)
# 
# class(state_codes$reg_iso)
# val_label(state_codes$reg_iso)
# 
# val_labels(state_codes$reg_iso)
# 
# attributes(state_codes$reg_iso)
# attributes(state_codes$reg_iso)$label
# 
# evs_de <- evs_de %>% 
#   mutate(
#     german_state
#   )
# 
# state_codes %>% 
#   slice(2) -> test


