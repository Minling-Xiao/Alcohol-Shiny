library(sf)
library(tmap)
library(dplyr)
library(leaflet)

load("RData/alcohol.RData")
load("RData/IMD_Auckland.RData")

# on license
IMD_Auckland$Decile = ceiling(IMD_Auckland$Dec_IMD18/2)
On_license = filter(alcohol, License_Ty == 'On-licence')
on_license_imd = st_join(On_license, IMD_Auckland)
on_license_imd = na.omit(on_license_imd)

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
alcohol_zone = lapply(unique(IMD_Auckland$Areas), function(x)filter(on_license_imd,Areas==x))
Number_of_outlets = lapply(alcohol_zone, function(x)sapply(1:5,function(y)sum(x$Decile==y)))
Number_of_outlets = unlist(Number_of_outlets)
Area = rep(unique(IMD_Auckland$Areas),each=5)
Decile = rep(1:5,4)
on_zones_density = cbind(Area,Decile,pop_area,Number_of_outlets)
on_zones_density$Density_per_10000_people = on_zones_density$Number_of_outlets/on_zones_density$Population
on_zones_density$Density_per_km² = on_zones_density$Number_of_outlets/on_zones_density$Shape_area

# save(on_zones_density, file = "RData/dep_zones_on_license.RData")

# Other
Zones_Deprivation_Density = merge(zone_list$Other,on_zones_density[on_zones_density$Area=="Other",])
# st_write(Zones_Deprivation_Density, "Shapefiles/org/rest_on_density.shp")
shaper_imd_overall = st_read('Shapefiles/shaper/shaper_rest_on_density/shaper_rest_on_density.shp')
crs = st_crs(IMD_Auckland)
st_geometry(Zones_Deprivation_Density) = shaper_imd_overall$geometry
st_crs(Zones_Deprivation_Density) = crs

Deprivation_Density = Zones_Deprivation_Density
map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              title="Density per 10000 people") +
  tm_shape(On_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/rest_Deprivation_Density_pop_on.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              title="Density per km²") +
  tm_shape(On_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/rest_Deprivation_Density_area_on.RData")

# CBD
Zones_Deprivation_Density = merge(zone_list$CBD,on_zones_density[on_zones_density$Area=="CBD",])

Deprivation_Density = Zones_Deprivation_Density
map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              title="Density per 10000 people") +
  tm_shape(On_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/cbd_Deprivation_Density_pop_on.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              style = "quantile",
              title="Density per km²") +
  tm_shape(On_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/cbd_Deprivation_Density_area_on.RData")

# waitakere
Zones_Deprivation_Density = merge(zone_list$`Trust - Waitakere`,on_zones_density[on_zones_density$Area=="Trust - Waitakere",])

Deprivation_Density = Zones_Deprivation_Density
map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              title="Density per 10000 people") +
  tm_shape(On_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/waitakere_Deprivation_Density_pop_on.RData")

Deprivation_Density = Deprivation_Density[order(Deprivation_Density$Density_per_km²),]
breaks = unique(Deprivation_Density$Density_per_km²)*10
breaks[1]=floor(breaks[1])
breaks[5]=ceiling(breaks[5])
breaks[2:4]=round(breaks[2:4])
breaks = unique(breaks)/10
map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              style="fixed",
              breaks = breaks,
              title="Density per km²") +
  tm_shape(On_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/waitakere_Deprivation_Density_area_on.RData")

# portage
Zones_Deprivation_Density = merge(zone_list$`Trust - Portage`,on_zones_density[on_zones_density$Area=="Trust - Portage",])

Deprivation_Density = Zones_Deprivation_Density[order(Zones_Deprivation_Density$Density_per_10000_people),]
breaks = unique(Deprivation_Density$Density_per_10000_people)
breaks[1]=floor(breaks[1])
breaks[5]=ceiling(breaks[5])
breaks[2:4]=round(breaks[2:4])
breaks = unique(breaks)
map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              style="fixed",
              breaks = breaks,
              title="Density per 10000 people") +
  tm_shape(On_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/portage_Deprivation_Density_pop_on.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              style="jenks",
              title="Density per km²") +
  tm_shape(On_license) + tm_dots(col = "ivory")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(object = map, file = "map/portage_Deprivation_Density_area_on.RData")


