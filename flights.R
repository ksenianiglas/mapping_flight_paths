library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(tidyverse)
library(plyr)
library(gganimate)

# A dataset of your flights - enetered manually to Excel, 
# downloaded from my.flightradar24, etc
flights <- read_delim("flightdiary_2020_04_13_11_40.csv", delim = ",")

# Coordinates of cities obtained from https://simplemaps.com/data/world-cities
coords <- read_csv("worldcities.csv") %>%
  select(city_ascii, lat, lng)


flights <- flights %>%
  select(Date, From, To) %>%
  mutate(From = str_extract(From, "[A-Za-z ]*") %>% str_sub(end = -2), # Removing unnecessary data and symbols from city names
         To = str_extract(To, "[A-Za-z ]*") %>% str_sub(end = -2)) %>%
  left_join(coords, by = c("From" = "city_ascii")) %>% # adding coordinates
  left_join(coords, by = c("To" = "city_ascii")) %>%
  distinct(Date, To, From, .keep_all = TRUE)

# Base - world map
worldmap <- map_data("world")
wrld <- c(geom_polygon(aes(long,lat,group=group), 
                       size = 0.1, 
                       colour= "#1a1a1a", 
                       fill="#404040", alpha=0.8, data=worldmap))         
fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}         

# Great circle routes
routes <- gcIntermediate(flights[,c('lng.x', 'lat.x')], 
                         flights[,c('lng.y', 'lat.y')], 50, 
                         breakAtDateLine = FALSE, 
                         addStartEnd = TRUE, sp=TRUE)
# Fortify routes to dataframe
fortifiedroutes <- fortify.SpatialLinesDataFrame(routes)


# Add order for animation
fortifiedroutes <- fortifiedroutes %>%
  arrange(group,order) %>%
  mutate(ord = rownames(.) %>% as.numeric()) 

# Create plot + animation
anim <- ggplot() +
  wrld +
  geom_line(aes(long,lat, group = id), size=.4, data= fortifiedroutes,
            color = "#f2ee66",
            alpha = .3) +
  theme(panel.background = element_rect(fill='#1f1f1f',colour='#2b2b2b'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  transition_reveal(fortifiedroutes$ord)

animate(anim, fps = 20, width = 1980, height = 1080, 
        duration = 30, end_pause = 40)
anim_save("flights.gif")
