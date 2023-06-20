library(sf)
library(tmap)
library(dplyr)
library(leaflet)

load("RData/alcohol.RData")
load("RData/IMD_Auckland.RData")
load("RData/density_for_off_license_with_geom.RData")

## Density map - On license

# st_write(density_for_on_license_with_geom,'Density/DenOn.shp')
crs = st_crs(IMD_Auckland)
ShaperDenOff = st_read('Shapefiles/shaper/ShaperDenOff/ShaperDenOff.shp')
st_geometry(density_for_off_license_with_geom) = ShaperDenOff$geometry
st_crs(density_for_off_license_with_geom) = crs
Density = density_for_off_license_with_geom

Off_license = filter(alcohol, License_Ty=="Off-licence")

## Pop
Density = Density[order(Density$Density_per_10000_people),]
breaks = Density$Density_per_10000_people
breaks[1]=floor(breaks[1])
breaks[4]=ceiling(breaks[4])
breaks[2:3]=round(breaks[2:3])
Label_Area = st_cast(Density)[-3,]
map = tm_shape(Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              style="fixed", 
              breaks=breaks,
              title="Density per 10000 people") +
  tm_shape(Label_Area) + tm_text("Areas") +
  tm_shape(Off_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

save(object = map, file = "map/off_license_pop_density.RData")

load("map/off_license_pop_density.RData")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

## Area
breaks = sort(Density$Density_per_km²)*10
breaks[1]=floor(breaks[1])
breaks[4]=ceiling(breaks[4])
breaks[2:3]=round(breaks[2:3])
breaks = breaks/10
map = tm_shape(Density)+
  tm_polygons(col = "Density_per_km²",
              palette = "Blues",
              style = "fixed",
              breaks = breaks,
              title = "Density per km²") +
  tm_shape(Label_Area) + tm_text("Areas") +
  tm_shape(Off_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

save(map, file = "map/off_license_area_density.RData")

load("map/on_license_area_density.rds")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")
