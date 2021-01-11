library(tidyverse)
library(sf)

rawdata <- read_csv("London data/phe_cases_london_boroughs.csv") %>%
  filter(date == "2020/12/2")

mapdata <- read_sf("London data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>% 
  st_transform(("+init=epsg:4326"))



traffic_flow_borough <- readxl::read_xls("London data/traffic-flow-borough-.xls", sheet = "Traffic Flows - All vehicles") %>% 
  select("LA Code", "2020estimate") %>%
  drop_na() 

finadata <- mapdata %>% 
  select(NAME, GSS_CODE, geometry) %>% 
  left_join(rawdata %>% 
              select(new_cases, area_code), by = c("GSS_CODE" = "area_code")) %>% 
  left_join(traffic_flow_borough, by = c("GSS_CODE" = "LA Code")) 

lon <- lapply(finadata$geometry, FUN = function(x){st_centroid(x)[1]}) %>% 
  unlist()
lat <- lapply(finadata$geometry, FUN = function(x){st_centroid(x)[2]}) %>% 
  unlist()

finadata2 <- finadata %>% 
  mutate(lon = lon) %>% 
  mutate(lat = lat) %>% 
  rename(estimate2020 = `2020estimate`)

ggplot() + 
  geom_sf(data = mapdata, aes(geometry = geometry)) + 
  geom_point(data = finadata2, aes(x = lon, y = lat)) + 
  theme_bw() + 
  labs(title = "the centroids of all Wards in London") + 
  theme(panel.border = element_blank())



library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)



