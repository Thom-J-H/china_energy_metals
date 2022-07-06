# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
#library(RColorBrewer)
#library(patchwork)
library(glue)
library(scales)



# Data --------------------------------------------------------------------

load("~/R_STUDIO/China_Global/data/tidy_data/china_metals_global.rda")
load("~/R_STUDIO/China_Global/data/tidy_data/china_energy_global.rda")
load("~/R_STUDIO/China_Global/data/tidy_data/world_map2_project.rda")




region_dat <- China_Metals_Dat %>%
  select(country, region) %>% distinct()


Metals_by_Nation <- Metals_by_Nation %>%
  left_join(region_dat, by = "country" )


Metals_by_Nation_Sub <- Metals_by_Nation_Sub %>%
  left_join(region_dat, by = "country" )



region_dat2 <- China_Energy_Chemicals %>%
  select(country, region) %>% distinct()


Energy_by_Nation <- Energy_by_Nation %>%
  left_join(region_dat2, by = "country" )


Energy_by_Nation_Sub <- Energy_by_Nation_Sub %>%
  left_join(region_dat2, by = "country" )



setdiff(world_map2$country, Energy_by_Nation$country)

setdiff(Energy_by_Nation$country, world_map2$country)

# "USA" "United States"  
# "Bosnia"   "Bosnia and Herzegovina" 
# "Kyrgyzstan"  "Kyrgyz Republic" 
#  "Democratic Republic of the Congo"  "Congo, Dem. Rep."   
# "Laos"   "Lao"  
# "Britain"  "United Kingdom"  
# "UAE"   "United Arab Emirates" 
# "Sao Tome"  "Sao Tome and Principe"  
#  "Russian Federation"   "Russia"  
# "Trinidad-Tobago"    "Trinidad and Tobago" 



Energy_by_Nation <- Energy_by_Nation %>%
  mutate(country = case_when(country == "USA"  ~   "United States"  ,
                             country == "UAE"  ~   "United Arab Emirates"  ,
                             country == "Trinidad-Tobago"  ~   "Trinidad and Tobago"   ,
                             country == "Russian Federation"   ~   "Russia" ,
                             country == "Sao Tome"   ~  "Sao Tome and Principe"  ,
                             country == "Britain"   ~  "United Kingdom"   ,
                             country == "Laos"   ~  "Lao"   ,
                             country == "Democratic Republic of the Congo"   ~  "Congo, Dem. Rep."  ,
                             country ==  "Kyrgyzstan"    ~  "Kyrgyz Republic" ,
                             country ==  "Bosnia"     ~  "Bosnia and Herzegovina"  ,
                             country ==  "Congo"     ~  "Congo, Rep." ,
                             TRUE ~ country)  )



setdiff(Energy_by_Nation$country, world_map2$country)




test_map  <- world_map2 %>% 
  left_join(Energy_by_Nation , by = "country") 




## remove Antarctica "ATA"

test_map2  <- test_map   %>%
  filter(code_3 != "ATA") %>%
  ggplot(aes(x = long, 
             y = lat, 
             group = group, 
             label = country)) +
  geom_polygon(aes(fill = by_nation), 
               color = "white", size = 0.02 ) +
  scale_fill_viridis_c(option = "C" ) +
  theme_void() +
  labs(fill = "Energy Investment: \nUSD ($) Millions ",
       title = "Chinese Foreign Investment (2005-2021): Energy Sector",
       subtitle = "Data Source: aei.org/china-global-investment-tracker",
       caption = 'Data Humanist, CC0 (Public Domain)')

test_map2 

plotly::ggplotly(test_map2  )




setdiff(Metals_by_Nation$country, world_map2$country)



Metals_by_Nation <- Metals_by_Nation %>%
  mutate(country = case_when(country == "USA"  ~   "United States"  ,
                             country == "UAE"  ~   "United Arab Emirates"  ,
                             country == "Trinidad-Tobago"  ~   "Trinidad and Tobago"   ,
                             country == "Russian Federation"   ~   "Russia" ,
                             country == "Sao Tome"   ~  "Sao Tome and Principe"  ,
                             country == "Britain"   ~  "United Kingdom"   ,
                             country == "Laos"   ~  "Lao"   ,
                             country == "Democratic Republic of the Congo"   ~  "Congo, Dem. Rep."  ,
                             country ==  "Kyrgyzstan"    ~  "Kyrgyz Republic" ,
                             country ==  "Bosnia"     ~  "Bosnia and Herzegovina"  ,
                             country ==  "Congo"     ~  "Congo, Rep." ,
                             TRUE ~ country)  )

setdiff(Metals_by_Nation$country, world_map2$country)






metals_map_dat  <- world_map2 %>% 
  left_join(Metals_by_Nation , by = "country") 




## remove Antarctica "ATA"

metals_map   <- metals_map_dat     %>%
  filter(code_3 != "ATA") %>%
  ggplot(aes(x = long, 
             y = lat, 
             group = group, 
             label = country)) +
  geom_polygon(aes(fill = by_nation),
               color = "white", size = 0.02  ) +
  scale_fill_viridis_c(option = "C", 
                       trans = scales::log10_trans() ) +
  theme_void() +
  labs(fill = "Metals Investment: \nUSD ($) Millions ",
       title = "Chinese Foreign Investment (2005-2021): Metals Sector",
       subtitle = "Data Source: aei.org/china-global-investment-tracker",
       caption = 'Data Humanist, CC0 (Public Domain)') 

metals_map 


#library(plotly)
#p_metal <- plotly::ggplotly(metals_map)


