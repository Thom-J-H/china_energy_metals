# Libraries ---------------------------------------------------------------


library(tidyverse)
library(here)
library(RColorBrewer)
library(patchwork)
library(glue)
library(scales)


# Data --------------------------------------------------------------------


# Energy Global -----------------------------------------------------------


library(readxl)
China_Energy_Chemicals <- read_excel("data/raw_data/China_Energy_Chemicals.xlsx")
View(China_Energy_Chemicals)


China_Energy_Chemicals <- China_Energy_Chemicals %>%
  replace_na( list(Subsector = "Unspecified") )


China_Energy_Chemicals <- China_Energy_Chemicals %>%
  janitor::clean_names()


names(China_Energy_Chemicals )

China_Energy_Chemicals %>% visdat::vis_dat()


# Map One
Energy_by_Nation <- China_Energy_Chemicals %>%
  filter(sector == "Energy") %>%
  group_by(country, sector) %>%
  summarize(by_nation = sum(quantity_in_millions, na.rm = TRUE))


# Map Sub
Energy_by_Nation_Sub <-  China_Energy_Chemicals %>%
  filter(sector == "Energy") %>%
  group_by(country, sector, subsector) %>%
  summarize(by_nation = sum(quantity_in_millions, na.rm = TRUE))


Energy_by_Nation_Sub


# By Year
Energy_by_Year <-  China_Energy_Chemicals %>%
  filter(sector == "Energy") %>%
  group_by(year, sector) %>%
  summarize(by_year= sum(quantity_in_millions, na.rm = TRUE))


# Year Sub
Energy_by_Year_Sub <-  China_Energy_Chemicals %>%
  filter(sector == "Energy") %>%
  group_by(year, sector, subsector) %>%
  summarize(by_year= sum(quantity_in_millions, na.rm = TRUE))


Energy_by_Nation$by_nation %>% sum() 
Energy_by_Nation_Sub$by_nation %>% sum() 
Energy_by_Year$by_year %>% sum() 
Energy_by_Year_Sub$by_year %>% sum() 


ls()


save_first <- ls()

save(list = save_first, 
     file = here::here("data", "tidy_data", "china_energy_global.rda") )



# Metals ------------------------------------------------------------------


library(readxl)
China_Metals_Dat <- read_excel("data/raw_data/China_Metals_Dat.xlsx")
View(China_Metals_Dat)





China_Metals_Dat  <- China_Metals_Dat  %>%
  replace_na( list(Subsector = "Unspecified") ) %>%
  janitor::clean_names()


names(China_Metals_Dat)



China_Metals_Dat  %>% visdat::vis_dat()




# Map One
Metals_by_Nation <- China_Metals_Dat %>%
  filter(sector == "Metals") %>%
  group_by(country, sector) %>%
  summarize(by_nation = sum(quantity_in_millions, na.rm = TRUE))


# Map Sub
Metals_by_Nation_Sub <-  China_Metals_Dat %>%
  filter(sector == "Metals") %>%
  group_by(country, sector, subsector) %>%
  summarize(by_nation = sum(quantity_in_millions, na.rm = TRUE))


Metals_by_Nation_Sub


# By Year
Metals_by_Year <-  China_Metals_Dat %>%
  filter(sector == "Metals") %>%
  group_by(year, sector) %>%
  summarize(by_year= sum(quantity_in_millions, na.rm = TRUE))


# Year Sub
Metals_by_Year_Sub <-  China_Metals_Dat %>%
  filter(sector == "Metals") %>%
  group_by(year, sector, subsector) %>%
  summarize(by_year= sum(quantity_in_millions, na.rm = TRUE))


Metals_by_Nation$by_nation %>% sum() 
Metals_by_Nation_Sub$by_nation %>% sum() 
Metals_by_Year$by_year %>% sum() 
Metals_by_Year_Sub$by_year %>% sum() 




save_second <- c("Metals_by_Year_Sub" , "Metals_by_Year",
              "Metals_by_Nation_Sub", "Metals_by_Nation",
              "China_Metals_Dat")



save(list = save_second, 
     file = here::here("data", "tidy_data", "china_metals_global.rda") )

