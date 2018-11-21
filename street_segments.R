library(httr)
library(jsonlite)
library(plyr)
library(lubridate)
library(sp)
library(maptools)
library(dplyr)
library(geojsonio)
library(rgeos)
library(rgdal)

keys <- fromJSON("key.json")

makeLine <- function(x) {
  Lines(Line(cbind(x["Lng"], x["Lat"])), ID = paste(c(sample(1:9,1), sample( 0:9, 19, replace=TRUE)), collapse=""))
}

# Cg General Shape API Call
cgLine <- function(class, fields, filter) {
  if (is.na(filter)){
    url <- paste0("https://cgweb06.cartegraphoms.com/", keys$cg_org, "/api/v1/classes/", class, "?fields=", fields, ",InactiveField,cgShape&limit=100000000&offset=0")
  } else {
    url <- paste0("https://cgweb06.cartegraphoms.com/", keys$cg_org, "/api/v1/classes/", class, "?filter=", filter, "&fields=", fields, ",InactiveField,cgShape&limit=100000000&offset=0")
  }
  request <- GET(gsub(" ", "%20", url), authenticate(keys$cg_un, keys$cg_pw, type = "basic"))
  content <- content(request, as = "text")
  load <- jsonlite::fromJSON(content)[[class]]
  load <- load[load$InactiveField == FALSE,]
  load <- load[!is.na(load$CgShape$ShapeType),]
  ap <- lapply(load$CgShape$Points, makeLine)
  lines <- SpatialLines(ap, proj4string=CRS("+proj=utm +north +zone=16T + datum=WGS84"))
  row.names(lines) <- row.names(load)
  final <- SpatialLinesDataFrame(lines, load)
  final$CgShape <- NULL
  return(final)
}

streets <- cgLine("cgPavementClass", "Oid,StreetField", '(([HOODLEFT] is equal to "NORTH OAKLAND"))')

leaflet(streets) %>%
  addPolylines(popup = ~paste(Oid)) %>%
  addTiles()

segments <- c("2131727916", "1043961741", "76190254", "1296642428", "607565176", "1375437518", "1883671662", "1008177727", "788949536", "1126813671", "789316009", "1960296195", "1745022739", "2012266745", "626021130", "2120182547", "881883842", "2006828970", "1525554447", "1535153097", "1859620527", "1637462100")

parking <- streets[streets$Oid %in% segments,]

leaflet(parking) %>%
  addPolylines(popup = ~paste(Oid)) %>%
  addTiles()

writeOGR(parking, "street_parking.geojson", layer="meuse", driver="GeoJSON", overwrite_layer = TRUE)
