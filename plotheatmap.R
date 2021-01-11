library(tidyverse)
library(sf)

rawdata <- read_csv("London data/phe_cases_london_boroughs.csv")

mapdata <- read_sf("London data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>% 
  st_transform(("+init=epsg:4326"))



# part1 -------------------------------------------------------------------


uniqe_date <- rawdata$date %>% 
  unique()
temp_unique_date <- uniqe_date[100]


# rawdata %>% filter(date == temp_unique_date) %>% 
#   left_join(mapdata %>% select(GSS_CODE, geometry), by = c("area_code" = "GSS_CODE")) %>% 
#   ggplot(aes(geometry = geometry)) +
#   geom_sf(aes(fill = new_cases)) + 
#   theme_bw() + 
#   labs(title = glue::glue("date: {temp_unique_date}")) +
#   theme(panel.border = element_blank())

dir.create("heatmapplot")


for (i in seq_along(uniqe_date)) {
  temp_unique_date <- uniqe_date[i]
  cat(i, '\r')
  
  p <- mapdata %>% 
    select(GSS_CODE, geometry) %>% 
    left_join(rawdata %>% 
                filter(date == temp_unique_date), by = c("GSS_CODE" = "area_code")) %>% 
    ggplot(aes(geometry = geometry)) +
    geom_sf(aes(fill = new_cases)) + 
    theme_bw() + 
    labs(title = glue::glue("date: {temp_unique_date}")) +
    theme(panel.border = element_blank())
  
  
  ggsave(filename = paste0("heatmapplot/", str_replace_all(temp_unique_date, pattern = "/", replacement = "_"), ".png"),
         plot = p, width = 7, height = 5)
}

# part3 -------------------------------------------------------------------
data3 <- rawdata %>% 
  mutate(date = lubridate::as_date(date)) %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(area_code, month) %>% 
  summarise(add_month = max(total_cases) - min(total_cases)) %>% 
  left_join(mapdata %>% 
              select(GSS_CODE, geometry), by = c("area_code" = "GSS_CODE"))

p2 <-  data3 %>% 
  ggplot(aes(geometry = geometry)) + 
  geom_sf(aes(fill = add_month)) + facet_wrap(~month, ncol=3) + 
  labs(fill = "month add") + 
  scale_fill_viridis_c(begin = 0, end = 1) + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        panel.border = element_blank())

p2
ggsave(filename = "monthadd.png", width = 12, height = 10,plot = p2)

# part 2 ------------------------------------------------------------------
library(ggthemes)


traffic_flow_borough <- readxl::read_xls("London data/traffic-flow-borough-.xls", sheet = "Traffic Flows - All vehicles")
traffic_flow_borough %>% 
  select("LA Code", "2020estimate") %>% 
  drop_na() %>% 
  left_join(mapdata %>% 
              select(GSS_CODE, geometry), by = c("LA Code" = "GSS_CODE")) %>%
  ggplot(aes(geometry = geometry, fill = `2020estimate`)) + 
  geom_sf() + 
  scale_fill_continuous_tableau("Classic Red") +
  labs(title = "traffic-flow-borough:2020estimate") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position = 'bottom')

ggsave(filename = "traffic-flow-borough.png", width = 7, height = 5)  
