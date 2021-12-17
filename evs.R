library(tidyverse)
library(haven)
library(labelled)
# library(sjlabelled)
library(sjmisc)
library(sf)
library(tmap)
library(rvest)

#read in the data
evs_raw <- read_dta("EVS/ZA7505_v2-0-0.dta/ZA7505_v2-0-0.dta")

#get just Germany
evs_de <- evs_raw %>% 
  filter(cntry_AN == "DE")

#find the different regional codes
evs_de %>% 
  select(reg_nuts1) %>% 
  distinct()

#find the next level of regional codes
evs_de %>% 
  select(reg_nuts2) %>% 
  distinct()


#get the state codes
evs_de %>% 
  select(reg_iso) %>% 
  distinct() -> state_codes


#### Load Germany map #### 

# source: https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/nuts-gebiete-1-1-000-000-stand-31-12-nuts1000-31-12.html
german_map_full <- st_read("german_shapefiles/nuts1000_1231/1000_NUTS1.shp")

evs_summarize <- evs_de %>%
  filter(D061 >= 1) %>% 
  group_by(reg_nuts1, D061) %>% 
  count() %>% 
  group_by(reg_nuts1) %>% 
  mutate(freq = 100*(n / sum(n)) %>% round(3)) %>%   
  filter(D061 == 1) %>% 
  arrange(desc(freq)) %>% 
  na.omit()

map <- merge(german_map_full, evs_summarize, by.x = "NUTS_CODE", by.y = "reg_nuts1")

tmap_options(check.and.fix = TRUE)
women_working_opinion_map <- tm_shape(map) +
  tm_polygons(title = "Fully agree with\nstatement (%)",
              col = 'freq',
              style = "cont",
              n = 5) + 
  tm_layout(legend.position = c("left", "top"))

ggplot(map) +
  geom_sf(aes(fill = freq)) + 
  scale_fill_viridis_c(alpha = 0.8) + 
  theme_void() +
  coord_sf() + 
  labs(fill= "Fully agree with\nstatement (%)")



women_working_opinion_map
tmap_save( women_working_opinion_map, "evs_map.png")



#### Elterngeld of fathers ####

# https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Soziales/Elterngeld/Tabellen/zeitreihe-vaeteranteil.html
html_file <- read_html("Statistisches Bundesamt - Statistisches Bundesamt.html")

#extract all tables from the document
table_elterngeld <- html_table(html_file, header = TRUE)

elterngeld <- data.frame(table_elterngeld)
elterngeld <- elterngeld %>% filter(Land != "Land") %>%
  mutate(X2017.1 = parse_number(str_replace(X2017.1, ',', '.'))) %>%
  mutate(X2018.1 = parse_number(str_replace(X2018.1, ',', '.'))) %>%
  mutate(X2019.1 = parse_number(str_replace(X2019.1, ',', '.'))) %>%
  mutate(X2020.1 = parse_number(str_replace(X2020.1, ',', '.'))) 


elterngeld_map <- merge(german_map_full, elterngeld, by.x = "NUTS_NAME", by.y = "Land")

tm_shape(elterngeld_map)+
  tm_polygons(col = "X2018.1")

elterngeld_plot <- ggplot(elterngeld_map) + 
  geom_sf(aes(fill = X2020.1)) +
  scale_fill_viridis_c(trans = "sqrt", alpha = 0.8) + 
  theme_void() +
  coord_sf() + 
  labs(fill= "Share of 'Elterngeld'\ntaken by fathers (%)")

ggsave("elterngeld_laenderlevel.png", elterngeld_plot)

#### "Jobs scarce: Men should have more riht to a job than women (3-point scale)" (C001) ####

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




#### Archive ####
# state_codes %>% 
#   mutate(german_state = unlabelled(reg_iso),
#          german_state = str_extract(german_state, "(?<=DE-[A-Z][A-Z][:space:]).+"))

# evs_de %>% 
#   sjmisc::find_var(
#     "sex",
#     search = "name_label"
#   )
#Extract the state names
state_codes %>% 
  mutate(
    german_state = reg_iso %>% 
      unlabelled() %>% 
      str_extract("(?<=DE-[A-Z][A-Z][:space:]).+")
  )





# from the top 

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




#evs_de <- evs_de %>% 
#  mutate(RS = ifelse(reg_nuts1 == 'DE1', '08',
#              ifelse(reg_nuts1 == 'DE2', '09',
#              ifelse(reg_nuts1 == 'DE3', '11',
#              ifelse(reg_nuts1 == 'DE4', '12',
#              ifelse(reg_nuts1 == 'DE5', '04',
#              ifelse(reg_nuts1 == 'DE6', '02',
#              ifelse(reg_nuts1 == 'DE7', '06',
#              ifelse(reg_nuts1 == 'DE8', '13',
#              ifelse(reg_nuts1 == 'DE9', '03',
#              ifelse(reg_nuts1 == 'DEA', '05',
#              ifelse(reg_nuts1 == 'DEB', '07',
#              ifelse(reg_nuts1 == 'DEC', '10',
#              ifelse(reg_nuts1 == 'DED', '14',
#              ifelse(reg_nuts1 == 'DEE', '15',
#              ifelse(reg_nuts1 == 'DEF', '01',
#              ifelse(reg_nuts1 == 'DEG', '15', NA)))))))))))))))))

#### Pre-school child suffers with working mother (D061)
evs_de %>%
  mutate(
    german_state = reg_iso %>% 
      unlabelled() %>% 
      str_extract("(?<=DE-[A-Z][A-Z][:space:]).+")
  ) %>% 
  filter(D061 >= 1) %>% 
  group_by(german_state, D061) %>% 
  count() %>% 
  group_by(german_state) %>% 
  mutate(freq = (n / sum(n)) %>% round(3)) %>%   
  filter(D061 == 1) %>% 
  arrange(desc(freq)) %>% 
  na.omit() %>% 
  ggplot(aes(x = german_state %>% fct_reorder(-freq), y = freq)) + 
  geom_col() +
  labs(x = "",
       y = "",
       title = "Child suffers when mother goes to work",
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

evs_de %>%
  mutate(
    german_state = reg_iso %>% 
      unlabelled() %>% 
      str_extract("(?<=DE-[A-Z][A-Z][:space:]).+")
  ) %>% 
  filter(D061 >= 1) %>% 
  group_by(german_state, D061) %>% 
  count() %>% 
  group_by(german_state) %>% 
  mutate(freq = (n / sum(n)) %>% round(3)) %>%   
  #filter(D061 <= 2) %>% 
  #arrange(desc(freq)) %>% 
  na.omit()
  
  
  

ggsave(
  filename = "plot_coo1_de_state.png",
  plot = last_plot()
)
