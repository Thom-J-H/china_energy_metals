
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
#library(RColorBrewer)
#library(patchwork)
library(glue)
library(scales)


# Data Mess ---------------------------------------------------------------

load("~/R_STUDIO/China_Global/data/tidy_data/metals_energy_all.RData")

# Graphs ------------------------------------------------------------------


breaks_years <- seq(2005, 2023, by = 2)

metal_millions <- seq(0, 15000, by = 2500)


Metals_by_Year <- rename(Metals_by_Year, total = by_year)

Metals_by_Year_Sub <-  Metals_by_Year_Sub %>% 
  left_join(Metals_by_Year, by = c("year"))


metals_sub_year_plot_facet <- Metals_by_Year_Sub %>%
  ggplot( aes(x = year, y = by_year, color = subsector)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", 
       y = "Millions USD ($)",
       color = "Subsector",
       title = "Chinese Foreign Investment: Metals Sector ($ Millions)",
       subtitle = "Data Source: aei.org/china-global-investment-tracker",
       caption = 'Data Humanist, CC0 (Public Domain') +
  scale_x_continuous(breaks = breaks_years) +
  scale_y_continuous(breaks = metal_millions) +
  facet_wrap(~subsector) +
  guides(color = "none") +
  theme_minimal() 

  


metals_sub_year_plot_facet 



energy_sub_year_plot_facet <- Energy_by_Year_Sub %>%
  ggplot( aes(x = year, y = by_year, color = subsector)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", 
       y = "Millions USD ($)",
       color = "Subsector",
       title = "Chinese Foreign Investment: Energy Sector ($ Millions)",
       subtitle = "Data Source: aei.org/china-global-investment-tracker",
       caption = 'Data Humanist, CC0 (Public Domain') +
  scale_x_continuous(breaks = breaks_years) +
  facet_wrap(~subsector) +
  guides(color = "none") +
  theme_minimal()

energy_sub_year_plot_facet 


metals_sub_year_plot_stack <- Metals_by_Year_Sub  %>%
  ggplot( aes(x = year, y = by_year, fill = subsector), color = "black") +
  geom_col() +
  labs(x = "Year", 
       y = "Millions USD ($)",
       fill = "Subsector",
       title = "Chinese Foreign Investment: Metals Sector ($ Millions)",
       subtitle = "Data Source: aei.org/china-global-investment-tracker",
       caption = 'Data Humanist, CC0 (Public Domain') +
  scale_x_continuous(breaks = breaks_years) +
  theme_minimal()  

metals_sub_year_plot_stack 







energy_sub_year_stack <- Energy_by_Year_Sub %>%
  ggplot( aes(x = year, y = by_year, fill = subsector), color = "black") +
  geom_col() +
  labs(x = "Year", 
       y = "Millions USD ($)",
       fill = "Subsector",
       title = "Chinese Foreign Investment: Energy Sector ($ Millions)",
       subtitle = "Data Source: aei.org/china-global-investment-tracker",
       caption = 'Data Humanist, CC0 (Public Domain') +
  scale_x_continuous(breaks = breaks_years) +
  theme_minimal() 


energy_sub_year_stack 



# By Nation ---------------------------------------------------------------


region_dat <- China_Metals_Dat %>%
  select(country, region) %>% distinct()


Metals_by_Nation <- Metals_by_Nation %>%
  left_join(region_dat, by = "country" )

Metals_by_Nation %>%
  filter(country != "Australia") %>% 
  ggplot( aes(x = region, y = by_nation, fill = by_nation)) +
  geom_col() +
  coord_flip()


Energy_by_Year_Sub %>%
  group_by(subsector) %>%
  summarize(sum(by_year, na.rm = TRUE))


Metals_by_Nation_Sub %>%
  group_by(subsector) %>%
  summarize(sum(by_nation, na.rm = TRUE))