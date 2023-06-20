library(sf)
library(tmap)
library(dplyr)
library(leaflet)

load("RData/alcohol.RData")
load("RData/IMD_Auckland.RData")

# off license
IMD_Auckland$Decile = ceiling(IMD_Auckland$Dec_IMD18/2)
Off_license = filter(alcohol, License_Ty == 'Off-licence')
off_license_imd = st_join(Off_license, IMD_Auckland)
off_license_imd = na.omit(off_license_imd)

# 4 zones
zone_list = lapply(unique(IMD_Auckland$Areas), function(x)filter(IMD_Auckland,Areas==x))
names(zone_list) = unique(IMD_Auckland$Areas)
den = function(y){
  Population = sapply(1:5,function(x)sum(y$Census_Pop[y$Decile==x]))/10000
  Shape_area = sapply(1:5,function(x)sum(y$Shape_Area[y$Decile==x]))/1e6
  data.frame(Population,Shape_area)
}
pop_area = lapply(zone_list, den)
pop_area = do.call(rbind,pop_area)
alcohol_zone = lapply(unique(IMD_Auckland$Areas), function(x)filter(off_license_imd,Areas==x))
Number_of_outlets = lapply(alcohol_zone, function(x)sapply(1:5,function(y)sum(x$Decile==y)))
Number_of_outlets = unlist(Number_of_outlets)
Area = rep(unique(IMD_Auckland$Areas),each=5)
Decile = rep(1:5,4)
off_zones_density = cbind(Area,Decile,pop_area,Number_of_outlets)
off_zones_density$Density_per_10000_people = off_zones_density$Number_of_outlets/off_zones_density$Population
off_zones_density$Density_per_km² = off_zones_density$Number_of_outlets/off_zones_density$Shape_area

# save(off_zones_density, file = "RData/dep_zones_off_license.RData")

# Other
Zones_Deprivation_Density = merge(zone_list$Other,off_zones_density[off_zones_density$Area=="Other",])
# st_write(Zones_Deprivation_Density, "Shapefiles/org/rest_off_density.shp")
shaper_imd_overall = st_read('Shapefiles/shaper/shaper_rest_off_density/shaper_rest_off_density.shp')
crs = st_crs(IMD_Auckland)
st_geometry(Zones_Deprivation_Density) = shaper_imd_overall$geometry
st_crs(Zones_Deprivation_Density) = crs

Deprivation_Density = Zones_Deprivation_Density

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              title="Density per 10000 people") +
  tm_shape(Off_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/rest_Deprivation_Density_pop_off.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              title="Density per km²") +
  tm_shape(Off_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/rest_Deprivation_Density_area_off.RData")

# Waitakere
Zones_Deprivation_Density = merge(zone_list$`Trust - Waitakere`,off_zones_density[off_zones_density$Area=="Trust - Waitakere",])
# st_write(Zones_Deprivation_Density, "DepDensity/org/rest_off_density.shp")
# shaper_imd_overall = st_read('DepDensity/shaper/shaper_rest_off_density/shaper_rest_off_density.shp')
# crs = st_crs(IMD_Auckland)
# st_geometry(Zones_Deprivation_Density) = shaper_imd_overall$geometry
# st_crs(Zones_Deprivation_Density) = crs

Deprivation_Density = Zones_Deprivation_Density

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              title="Density per 10000 people") +
  tm_shape(Off_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/waitakere_Deprivation_Density_pop_off.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              style = "jenks",
              title="Density per km²") +
  tm_shape(Off_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/waitakere_Deprivation_Density_area_off.RData")

# CBD
Zones_Deprivation_Density = merge(zone_list$CBD,off_zones_density[off_zones_density$Area=="CBD",])

Deprivation_Density = Zones_Deprivation_Density

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              style = "fixed",
              breaks = c(10,12,14,32,34,45),
              title="Density per 10000 people") +
  tm_shape(Off_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/cbd_Deprivation_Density_pop_off.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              style = "fixed",
              breaks = c(12,13,17,19,20,35),
              title="Density per km²") +
  tm_shape(Off_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/cbd_Deprivation_Density_area_off.RData")

# portage
Zones_Deprivation_Density = merge(zone_list$`Trust - Portage`,off_zones_density[off_zones_density$Area=="Trust - Portage",])

Deprivation_Density = Zones_Deprivation_Density

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              style="jenks",
              title="Density per 10000 people") +
  tm_shape(Off_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/portage_Deprivation_Density_pop_off.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              style="jenks",
              title="Density per km²") +
  tm_shape(Off_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

# save(object = map, file = "map/portage_Deprivation_Density_area_off.RData")

