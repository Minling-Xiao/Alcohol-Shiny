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

# overall
Number_of_outlets = sapply(1:5,function(x)sum(off_license_imd$Decile==x))
Population = sapply(1:5,function(x)sum(IMD_Auckland$Census_Pop[IMD_Auckland$Decile==x]))/10000
Shape_area = sapply(1:5,function(x)sum(IMD_Auckland$Shape_Area[IMD_Auckland$Decile==x]))/1e6
Density_per_10000_people = Number_of_outlets/Population
Density_per_km² = Number_of_outlets/Shape_area
overall = data.frame(Decile=1:5,Number_of_outlets,Population,Density_per_10000_people,Shape_area,Density_per_km²)
# save(overall, file = "RData/dep_overall_off_license.RData")

overall_off_density = merge(IMD_Auckland,overall)
# st_write(overall_off_density, "Shapefiles/org/overall_off_density.shp")
shaper_imd_overall = st_read('Shapefiles/shaper/shaper_overall_off_density/shaper_overall_off_density.shp')
crs = st_crs(IMD_Auckland)
st_geometry(overall_off_density) = shaper_imd_overall$geometry
st_crs(overall_off_density) = crs

Deprivation_Density = overall_off_density[order(overall_off_density$Density_per_10000_people),]
breaks = unique(Deprivation_Density$Density_per_10000_people)
breaks[1]=floor(breaks[1])
breaks[5]=ceiling(breaks[5])
breaks[2:4]=round(breaks[2:4])
breaks = unique(breaks)
map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_10000_people",
              palette="Blues",
              style="fixed", 
              breaks=breaks,
              title="Density per 10000 people") +
  tm_shape(Off_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

# save(object = map, file = "map/Overall_Deprivation_Density_pop_off.RData")

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")


Deprivation_Density = overall_off_density[order(overall_off_density$Density_per_km²),]
breaks = unique(Deprivation_Density$Density_per_km²)*10
breaks[1]=floor(breaks[1])
breaks[5]=ceiling(breaks[5])
breaks[2:4]=round(breaks[2:4])
breaks = unique(breaks)/10
map = tm_shape(Deprivation_Density)+
  tm_polygons(col="Density_per_km²",
              palette="Blues",
              style="fixed", 
              breaks=breaks,
              title="Density per km²") +
  tm_shape(Off_license) + tm_dots(col = "ivory") +
  tm_view(set.view=c(174.76,-36.8,10))

tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")

save(object = map, file = "map/Overall_Deprivation_Density_area_off.RData")
