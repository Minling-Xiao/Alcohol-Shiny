## Map on homepage
library(sf)
library(tmap)
library(dplyr)
library(leaflet)

load("RData/alcohol.RData")
load("RData/IMD_Auckland.RData")
load("RData/density_for_on_license_with_geom.RData")

IMD = st_read('Shapefiles/shaper/shaperIMD/shaperIMD.shp')
crs = st_crs(IMD_Auckland)
st_geometry(IMD_Auckland) = IMD$geometry
st_crs(IMD_Auckland) = crs

IMD=IMD_Auckland
On_license = filter(alcohol, License_Ty=="On-licence")
Off_license = filter(alcohol, License_Ty=="Off-licence")
Label_Area = st_cast(density_for_on_license_with_geom)[-2,]

map = tm_shape(IMD) + tm_polygons("Areas", palette = "Set2", alpha = 0.8,popup.vars=colnames(IMD)[1:(ncol(IMD)-1)]) +
  tm_shape(On_license) + tm_dots(col = "ivory") +
  tm_shape(Off_license) + tm_dots(col ="purple") + tm_view(set.view=c(174.76,-36.8,10)) +
  tm_add_legend(type="fill",labels = c("On_license","Off_license"),col = c("ivory","purple"),title = "Alcohol Outlets")

# save(map,file = "map/HomepageMap.RData")

load("map/HomepageMap.RData")

tmap_leaflet(map, in.shiny = TRUE)%>%hideGroup(c("On_license","Off_license"))