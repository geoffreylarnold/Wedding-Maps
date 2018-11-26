require(ggmap)
require(rgdal)
require(dplyr)
require(rgeos)

hoods <- readOGR("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson")
hood_centroids <- as.data.frame(gCentroid(hoods, byid = T))
hoods@data <- cbind(hoods@data, hood_centroids)

levels(hoods$hood)

hood_list <- sort(c("Central Business District", "Troy Hill", "Central Oakland", "North Oakland", "Carrick", "Friendship", "Middle Hill", "Upper Hill", "Garfield", "Squirrel Hill North", "Shadyside", "South Side Slopes", "Greenfield", "East Liberty", "Manchester", "Allegheny Center", "Mount Washington", "Bluff", "North Shore", "Central Northside", "Fairywood", "West End", "Squirrel Hill South", "Strip District"))

length(hood_list)

hoods <- subset(hoods, hood %in% hood_list)

color <- c("#002366", "#ffff31", "#a1a4a3")

hoods$color <- color
no <- 0
for (i in hood_list) {
  no <- no + 1
  poly <- subset(hoods, hood == i)
  fort <- fortify(poly, region = "hood")
  location <- gCentroid(poly)@coords
  
  gmap <- get_stamenmap(bbox = poly@bbox, source = "stamen", maptype = "toner-2011", crop = F, zoom = 15) 
  ggmap(gmap, extent = "device") + 
    geom_map(map = fort, data = fort, aes(map_id=id, x=long, y=lat, group=group), color = poly$color, alpha=0, size = 1.5) +
    ggtitle(label = paste0(no, ". ", poly$hood)) + 
    theme(plot.title = element_text(colour = color, size = 22, face = "bold", hjust = 0.5))
  
  ggsave(paste0("./cards/", gsub(" ", "_", i),".pdf"), 
         plot = last_plot(),
         units = "in",
         height = 4, width = 6,
         dpi = 300)
}
