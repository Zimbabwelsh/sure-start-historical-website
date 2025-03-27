library(tidyverse)
library(sf)
library(viridis)

df <- read.csv("data/national archive to nimdm wards (checked).csv")

prev <- read.csv("data/sure start coverage (hansard 2006) (checked).csv")

df$bestMatch <- stringr::str_to_upper(df$bestMatch)

prev$bestMatch <- stringr::str_to_upper(prev$bestMatch)

## Match to shapefiles from 1993 electoral wards (from https://www.opendatani.gov.uk/@land-property/osni-open-data-largescale-boundaries-wards-1993)

geom <- read_sf("data/geometry/OSNI_Open_Data_-_Largescale_Boundaries_-_Wards_(1993).shp")

fulldf <- left_join(geom, df,
                    join_by(NAME==bestMatch)) %>% 
  left_join(., prev, join_by(NAME==bestMatch))

fulldf <- fulldf %>% mutate(
  expansion = case_when(!is.na(hansard_ward) & !is.na(sure.start.centre) ~ 2006,
                        is.na(hansard_ward) & !is.na(sure.start.centre) ~ 2009,
                        is.na(hansard_ward)~ NA,) %>% as.factor()
)

write_sf( fulldf, "data/geometry/geom_data_2009_expansion.shp")




uptake2009 <- ggplot(fulldf)+
  geom_sf(aes(fill = expansion), color = "lightgrey" ,linewidth =0.2)+ 
  theme_bw()+
  scale_fill_viridis(discrete=TRUE, begin=0.2, na.value = "#ECECEC", direction=-1)
 
uptake2009

ggsave("uptake2009.png", uptake2009, dpi = 600, width = 6, height = 6)
