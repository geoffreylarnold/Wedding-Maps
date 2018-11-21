require(leaflet)
require(readr)
require(httr)
require(jsonlite)
require(rgdal)
require(sp)
require(plyr)
require(dplyr)

google_api <- fromJSON("key.json")$google

googleGeo <- function(lookUp, google_api) {
  lookUpEncode <- URLencode(lookUp)
  url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", lookUpEncode, "&key=", google_api)
  c <- httr::content(httr::RETRY("GET", url))
  if (c$status != "ZERO_RESULTS") {
    address <- rlang::flatten(c$results)
    df <- data.frame(address = lookUp, x = address$geometry$location$lng[1], y = address$geometry$location$lat[1])
  } else {
    df <- data.frame(address = lookUp, x = NA, y = NA)
  }
  return(df)
}

googleGeoCol <- function(addresses, google_api) {
  first <- T
  for (i in addresses) {
    if (first){
      final <- googleGeo(i, google_api)
      first <- F
    } else {
      join <- googleGeo(i, google_api)
      final <- rbind(final, join)
    }
  }
  return(final)
}

iconSet <- awesomeIconList(
  church = makeAwesomeIcon(icon = "heart", library = "fa", markerColor = "white"),
  venue = makeAwesomeIcon(icon = "music", library = "fa", markerColor = "blue"),
  rehearsal = makeAwesomeIcon(icon = "cutlery", library = "fa", markerColor = "yellow"),
  hotel = makeAwesomeIcon(text = "H", library = "fa", markerColor = "green"),
  paid_parking = makeAwesomeIcon(icon = "usd", library = "fa", markerColor = "orange"),
  free_parking = makeAwesomeIcon(text = "P", library = "fa", markerColor = "orange")
)

locs <- read_csv("locations.csv")

geo <- googleGeoCol(locs$address, google_api) %>%
  inner_join(locs) %>%
  mutate(address = ifelse(address == "Heinz Chapel", "4200 Fifth Avenue, Pittsburgh, Pennsylvania 15260", address))

write.csv(geo, "geocoded.csv", row.names = F)

geo <- read_csv("geocoded.csv")

street_parking <- readOGR("street_parking.geojson")

leaflet(geo) %>%
  addTiles(urlTemplate = "http://{s}.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}", attribution = "Google", options = tileOptions(maxZoom = 20, subdomains = c('mt0','mt1','mt2','mt3'))) %>%
  addAwesomeMarkers(~x, ~y, icon = ~iconSet[icon], popup = ~paste0("<b>", name, "</b><br>", address)) %>%
  addPolylines(data = street_parking, color = 'orange', popup = ~paste("<b>", tools::toTitleCase(tolower(StreetField)), "</b><br>Street Parking"))
