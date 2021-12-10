library(tidyverse)

lijphart_data_raw <- readr::read_csv("Lijphart-PoD_Dataset3_Appendix.csv")

lijphart_data <- lijphart_data_raw %>% 
  mutate(`exec-part_dim_1945-2010` = `exec-part_dim_1945-2010` %>% 
           str_replace(",", ".") %>% 
           as.numeric,
         `fed-unit_dim_1945-2010` = `fed-unit_dim_1945-2010` %>% 
           str_replace(",", ".") %>% 
           as.numeric) 

plot_lijphart <- lijphart_data %>% 
  ggplot(aes(x = `exec-part_dim_1945-2010`,
             y = `fed-unit_dim_1945-2010`)) + 
  geom_point(color = ifelse(lijphart_data$country_name_short == "DEU", "red", "black")) + 
  ggrepel::geom_text_repel(aes(label = country_name_short), 
                           family = "Corbel",
                           color = ifelse(lijphart_data$country_name_short == "DEU", "red", "black")) +
  geom_vline(xintercept = 0, alpha = 0.35) +
  geom_hline(yintercept = 0, alpha = 0.35) + 
  scale_x_continuous(limits = c(-2.5, 2.5)) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  theme_light() +
  theme(text = element_text(family = "Corbel"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "2 Dimensions of Government Constraints",
       x = "Unity of Executive",
       y = "Federalism vs. Centralization",
       caption = "Plot created by Lukas Warode\n Data used from Patterns of Democracy (Lijphart)") 

plot_lijphart

ggsave("plot_lijphart.png",
       plot = plot_lijphart,
       width = 8,
       height = 4.5)
