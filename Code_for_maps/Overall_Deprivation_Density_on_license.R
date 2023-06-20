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

# overall
Number_of_outlets = sapply(1:5,function(x)sum(on_license_imd$Decile==x))
Population = sapply(1:5,function(x)sum(IMD_Auckland$Census_Pop[IMD_Auckland$Decile==x]))/10000
Shape_area = sapply(1:5,function(x)sum(IMD_Auckland$Shape_Area[IMD_Auckland$Decile==x]))/1e6
Density_per_10000_people = Number_of_outlets/Population
Density_per_km² = Number_of_outlets/Shape_area
overall = data.frame(Decile=1:5,Number_of_outlets,Population,Density_per_10000_people,Shape_area,Density_per_km²)
# save(overall, file = "RData/dep_overall_on_license.RData")

overall_on_density = merge(IMD_Auckland,overall)
# st_write(overall_on_density, "DepDensity/org/overall_on_density.shp")
shaper_imd_overall = st_read('Shapefiles/shaper/shaper_overall_on_density/shaper_overall_on_density.shp')
crs = st_crs(IMD_Auckland)
st_geometry(overall_on_density) = shaper_imd_overall$geometry
st_crs(overall_on_density) = crs

Deprivation_Density = overall_on_density

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              style="fixed",
              breaks=c(7,12,16,20,22),
              title="Density per 10000 people") +
  tm_shape(On_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(map, file = "map/Overall_Deprivation_Density_pop_on.RData")

map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              # style="fixed", 
              # breaks=breaks,
              title="Density per 10000 people") +
  tm_shape(On_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")

# save(map, file = "map/Overall_Deprivation_Density_area_on.RData")









