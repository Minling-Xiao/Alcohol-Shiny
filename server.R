source("outlet_scenario_fun.R")

server = function(input, output){
  
  alcohol = reactive({
    load("RData/alcohol.RData")
    alcohol = filter(alcohol,License_Ty=="On-licence"|License_Ty=="Off-licence")
    return(alcohol)
  })
  
  imd = reactive({
    load("RData/simplified_IMD_Auckland.RData")
    return(simplified_imd)
  })
  
  on_license_density = reactive({
    load("RData/density_for_on_license_with_geom.RData")
    return(density_for_on_license_with_geom)
  })
  
  off_license_density = reactive({
    load("RData/density_for_off_license_with_geom.RData")
    return(density_for_off_license_with_geom)
  })
  
  overall_on_density_table = reactive({
    load("RData/dep_overall_on_license.RData")
    overall$Density_per_10000_people = round(overall$Density_per_10000_people,4)
    overall$`Density_per_km²` = round(overall$`Density_per_km²`,6)
    overall$Shape_area = round(overall$Shape_area,6)
    names(overall)[which(names(overall)=='Population')] = 'Population(10,000)'
    names(overall)[which(names(overall)=='Shape_area')] = 'Shape_area(km²)'
    return(overall)
  })
  
  zones_on_density_table = reactive({
    load("RData/dep_zones_on_license.RData")
    on_zones_density$Density_per_10000_people = round(on_zones_density$Density_per_10000_people,4)
    on_zones_density$`Density_per_km²` = round(on_zones_density$`Density_per_km²`,6)
    on_zones_density$Shape_area = round(on_zones_density$Shape_area,6)
    names(on_zones_density)[which(names(on_zones_density)=='Population')] = 'Population(10,000)'
    names(on_zones_density)[which(names(on_zones_density)=='Shape_area')] = 'Shape_area(km²)'
    on_zones_density[,c(1,2,5,3,6,4,7)]
  })
  
  overall_off_density_table = reactive({
    load("RData/dep_overall_off_license.RData")
    overall$Density_per_10000_people = round(overall$Density_per_10000_people,4)
    overall$`Density_per_km²` = round(overall$`Density_per_km²`,6)
    overall$Shape_area = round(overall$Shape_area,6)
    names(overall)[which(names(overall)=='Population')] = 'Population(10,000)'
    names(overall)[which(names(overall)=='Shape_area')] = 'Shape_area(km²)'
    return(overall)
  })
  
  zones_off_density_table = reactive({
    load("RData/dep_zones_off_license.RData")
    off_zones_density$Density_per_10000_people = round(off_zones_density$Density_per_10000_people,4)
    off_zones_density$`Density_per_km²` = round(off_zones_density$`Density_per_km²`,6)
    off_zones_density$Shape_area = round(off_zones_density$Shape_area,6)
    names(off_zones_density)[which(names(off_zones_density)=='Population')] = 'Population(10,000)'
    names(off_zones_density)[which(names(off_zones_density)=='Shape_area')] = 'Shape_area(km²)'
    off_zones_density[,c(1,2,5,3,6,4,7)]
  })
  
  school = reactive({
    load("RData/Secondary_school.RData")
    return(Secondary_school)
  })
  
  Marae = reactive({
    load("RData/Marae_Auckland.RData")
    return(Marae_Auckland)
  })
  
  ## Homepage
  output$alcohol_map<-renderLeaflet({
    IMD = imd()
    alcohol = alcohol()
    on_license_density = on_license_density()
    
    On_license = filter(alcohol, License_Ty=="On-licence")
    Off_license = filter(alcohol, License_Ty=="Off-licence")
    Label_Area = st_cast(on_license_density)[-2,]
    
    map = tm_shape(IMD) + tm_polygons("Areas", palette = "Set2", alpha = 0.8,
                                      legend.show = FALSE) +
      tm_shape(On_license) + tm_dots(col = "ivory") +
      tm_shape(Off_license) + tm_dots(col ="purple") + tm_view(set.view=c(174.76,-36.8,10)) +
      tm_add_legend(type="fill",
                    labels = c("CBD","Other","Trust - Portage","Trust - Waitakere","On_license","Off_license"),
                    col = c(brewer.pal(4, "Set2"),"ivory","purple"))
    
    tmap_leaflet(map,in.shiny=T)%>%hideGroup(c("On_license","Off_license"))
  })
  
  ## Density - On license
  output$on_density_pop_map = renderLeaflet({
    alcohol = alcohol()
    Density = on_license_density()
    breaks = sort(Density$Density_per_10000_people)
    breaks = c(floor(breaks[1]), ceiling(breaks))
    breaks = unique(breaks)
    Label_Area = st_cast(Density)[-2,]
    On_license = filter(alcohol, License_Ty=="On-licence")
    
    map = tm_shape(Density)+
      tm_polygons(col="Density_per_10000_people",
                  palette="Blues",
                  style="fixed", 
                  breaks=breaks,
                  title="Density per 10000 people") +
      tm_shape(Label_Area) + tm_text("Areas") +
      tm_shape(On_license) + tm_dots(col = "ivory") +
      tm_view(set.view=c(174.76,-36.8,10))
    
    tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")
  })

  output$on_density_area_map = renderLeaflet({
    alcohol = alcohol()
    Density = on_license_density()
    breaks = sort(Density$`Density_per_km²`)*10
    breaks = c(floor(breaks[1]), ceiling(breaks))
    breaks = unique(breaks)/10
    Label_Area = st_cast(Density)[-2,]
    On_license = filter(alcohol, License_Ty=="On-licence")
    
    map = tm_shape(Density)+
      tm_polygons(col = "Density_per_km²",
                  palette = "Purples",
                  style = "fixed",
                  breaks = breaks,
                  title = "Density per km²") +
      tm_shape(Label_Area) + tm_text("Areas") +
      tm_shape(On_license) + tm_dots(col = "ivory") +
      tm_view(set.view=c(174.76,-36.8,10))
    
    tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("On_license")
  })
  
  output$on_density_pop_table = renderDataTable({
    data = on_license_density()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    as.data.frame(data)[,1:4]
  })
  
  output$on_density_area_table = renderDataTable({
    data = on_license_density()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    as.data.frame(data)[,c(1:2,5:6)]
  })
  
  ## Density - Off license
  output$off_density_pop_map = renderLeaflet({
    alcohol = alcohol()
    Density = off_license_density()
    breaks = sort(Density$Density_per_10000_people)
    breaks = c(floor(breaks[1]), ceiling(breaks))
    breaks = unique(breaks)
    Label_Area = st_cast(Density)[-2,]
    Off_license = filter(alcohol, License_Ty=="Off-licence")
    
    map = tm_shape(Density)+
      tm_polygons(col="Density_per_10000_people",
                  palette="Blues",
                  style="fixed", 
                  breaks=breaks,
                  title="Density per 10000 people") +
      tm_shape(Label_Area) + tm_text("Areas") +
      tm_shape(Off_license) + tm_dots(col = "ivory") +
      tm_view(set.view=c(174.76,-36.8,10))
    
    tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")
  })
  
  output$off_density_pop_table = renderDataTable({
    data = off_license_density()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    as.data.frame(data)[,1:4]
  })
  
  output$off_density_area_map = renderLeaflet({
    alcohol = alcohol()
    Density = off_license_density()
    breaks = sort(Density$`Density_per_km²`)*10
    breaks = c(floor(breaks[1]), ceiling(breaks))
    breaks = unique(breaks)/10
    Label_Area = st_cast(Density)[-2,]
    Off_license = filter(alcohol, License_Ty=="Off-licence")
    
    map = tm_shape(Density)+
      tm_polygons(col = "Density_per_km²",
                  palette = "Purples",
                  style = "fixed",
                  breaks = breaks,
                  title = "Density per km²") +
      tm_shape(Label_Area) + tm_text("Areas") +
      tm_shape(Off_license) + tm_dots(col = "ivory") +
      tm_view(set.view=c(174.76,-36.8,10))
    
    tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")
  })
  
  output$off_density_area_table = renderDataTable({
    data = off_license_density()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    as.data.frame(data)[,c(1:2,5:6)]
  })
  
  output$overall_on_density_table = renderDataTable({
    data = overall_on_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data
  })
  
  output$cbd_on_density_table = renderDataTable({
    data = zones_on_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="CBD",-1]
  })
  
  output$portage_on_density_table = renderDataTable({
    data = zones_on_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="Trust - Portage",-1]
  })
  
  output$waitakere_on_density_table = renderDataTable({
    data = zones_on_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="Trust - Waitakere",-1]
  })
  
  output$rest_on_density_table = renderDataTable({
    data = zones_on_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="Other",-1]
  })
  
  output$overall_off_density_table = renderDataTable({
    data = overall_off_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data
  })
  
  output$cbd_off_density_table = renderDataTable({
    data = zones_off_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="CBD",-1]
  })
  
  output$portage_off_density_table = renderDataTable({
    data = zones_off_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="Trust - Portage",-1]
  })
  
  output$waitakere_off_density_table = renderDataTable({
    data = zones_off_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="Trust - Waitakere",-1]
  })
  
  output$rest_off_density_table = renderDataTable({
    data = zones_off_density_table()
    names(data)[which(names(data)=='Population')] = 'Population(10,000)'
    names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
    data[data$Area=="Other",-1]
  })
  
  ### Scenario
  
  output$scenario_area_off = renderUI({
    if (!input$scenario_area3){
      checkboxGroupInput(inputId = "scenario_area4",
                         label = NULL,
                         choices = c("CBD"="CBD", 
                                     "Trust - Waitakere"="Trust - Waitakere", 
                                     "Trust - Portage"="Trust - Portage", 
                                     "The rest of Auckland"="Other"))
    }
  })
  
  ## Off-license scenario
  
  off_license_scenario = eventReactive(input$go_off_license,{
    school = school()
    marae = Marae()
    imd = imd()
    alcohol = alcohol()
    alcohol = filter(alcohol, License_Ty == "Off-licence")
    school_dist = input$off_license_distance_from_school
    marae_dist = input$off_license_distance_from_Marae
    outlet_dist = input$off_license_distance_between_outlets
    
    if (is.na(school_dist) | school_dist<0) school_dist=0
    if (is.na(marae_dist) | marae_dist<0) marae_dist=0
    if (is.na(outlet_dist) | outlet_dist<0) outlet_dist=0
    
    join = st_join(alcohol,imd)
    join = join[!is.na(join$Areas),]
    join[!st_is_empty(join),,drop=F]
    
    school_scenario = NULL
    marae_scenario = NULL
    outlet_scenario = NULL
    school_buffer = NULL
    marae_buffer = NULL
    area = NULL
    
    ## school scenario
    
    if (school_dist > 0){
      school_buffer = st_buffer(school, school_dist)
      school_buffer = st_union(school_buffer)
      if (!input$scenario_area3){
        
        # new outlets in selected areas
        outlet = join[join$Areas %in% input$scenario_area4,]
        outlet1 = outlet[-unlist(st_intersects(school_buffer, outlet)),]
        
        # outlets in other areas
        outlet2 = join[!join$Areas %in% input$scenario_area4,]
        
        school_scenario = rbind(outlet1,outlet2)
      } else {
        school_scenario = join[-unlist(st_intersects(school_buffer, join)),]
      }
    }
    
    ## Marae Scenario
    
    if (marae_dist > 0){
      marae_buffer = st_buffer(marae, marae_dist)
      marae_buffer = st_union(marae_buffer)
      if (!input$scenario_area3){
        
        # new outlets in selected areas
        outlet = join[join$Areas %in% input$scenario_area4,]
        outlet1 = outlet[-unlist(st_intersects(marae_buffer, outlet)),]
        
        # outlets in other areas
        outlet2 = join[!join$Areas %in% input$scenario_area4,]
        
        marae_scenario = rbind(outlet1,outlet2)
      } else {
        marae_scenario = join[-unlist(st_intersects(marae_buffer, join)),]
      }
    }
    
    # overall scenario
    
    FID = Reduce(intersect,compact(list(school_scenario$FID_,marae_scenario$FID_)))
    overall_scenario = join[join$FID_%in% FID,]
    
    ## Outlet Scenario
    
    if (outlet_dist > 0){
      
      if (!input$scenario_area3) area = input$scenario_area4

      outlet_scenario = outlet_scenario_fun(join = join, outlet_dist = outlet_dist,
                                            auckland = input$scenario_area3,
                                            area = area)
      
      if (nrow(overall_scenario)==0) overall_scenario = outlet_scenario
      else overall_scenario = outlet_scenario_fun(join = overall_scenario, outlet_dist = outlet_dist,
                                                  auckland = input$scenario_area3, area = area)
    }
    
    alcohol = overall_scenario
    alcohol$Quintile = ceiling(alcohol$Dec_IMD18/2)
    IMD_Auckland = imd
    IMD_Auckland$Quintile = ceiling(IMD_Auckland$Dec_IMD18/2)
    if(nrow(alcohol)==0){
      overall_den = overall_off_density_table()
      off_zones_density = zones_off_density_table()
    } else{
      # overall den
      Number_of_outlets1 = sapply(1:5,function(x)sum(alcohol$Quintile==x))
      Population = sapply(1:5,function(x)sum(IMD_Auckland$Census_Pop[IMD_Auckland$Quintile==x]))/10000
      Shape_area = sapply(1:5,function(x)sum(IMD_Auckland$Shape_Area[IMD_Auckland$Quintile==x]))/1e6
      Density_per_10000_people = Number_of_outlets1/Population
      `Density_per_km²` = Number_of_outlets1/Shape_area
      overall_den = data.frame(Quintile=1:5,Population,Shape_area,
                               Number_of_outlets=Number_of_outlets1,
                               Density_per_10000_people,`Density_per_km²`)
      
      # zones dep
      zone_list = lapply(unique(IMD_Auckland$Areas), function(x)filter(IMD_Auckland,Areas==x))
      names(zone_list) = unique(IMD_Auckland$Areas)
      den = function(y){
        Population = sapply(1:5,function(x)sum(y$Census_Pop[y$Quintile==x]))/10000
        Shape_area = sapply(1:5,function(x)sum(y$Shape_Area[y$Quintile==x]))/1e6
        data.frame(Population,Shape_area)
      }
      pop_area = lapply(zone_list, den)
      pop_area = do.call(rbind,pop_area)
      alcohol_zone = lapply(unique(IMD_Auckland$Areas), function(x)filter(alcohol,Areas==x))
      Number_of_outlets = lapply(alcohol_zone, function(x)sapply(1:5,function(y)sum(x$Quintile==y)))
      Number_of_outlets = unlist(Number_of_outlets)
      Area = rep(unique(IMD_Auckland$Areas),each=5)
      Quintile = rep(1:5,4)
      off_zones_density = cbind(Area,Quintile,pop_area,Number_of_outlets)
      off_zones_density$Density_per_10000_people = off_zones_density$Number_of_outlets/off_zones_density$Population
      off_zones_density$`Density_per_km²` = off_zones_density$Number_of_outlets/off_zones_density$Shape_area
    }
    return(list("school_dist"=school_dist,
                "mario_dist"=marae_dist,
                "outlet_dist"=outlet_dist,
                "school_scenario"=school_scenario,
                "marae_scenario"=marae_scenario,
                "outlet_scenario"=outlet_scenario,
                "overall_scenario"=overall_scenario,
                "school_buffer"=school_buffer,
                "marae_buffer"=marae_buffer,
                "overall_dep_den"=overall_den,
                "zones_dep_den"=off_zones_density))
  })
  
  output$off_license_scenario_map <- renderLeaflet({
    imd = imd()
    alcohol = alcohol()
    School = school()
    Marae = Marae()
    label = off_license_density()
    school_dist = off_license_scenario()[[1]]
    marae_dist = off_license_scenario()[[2]]
    outlet_dist = off_license_scenario()[[3]]
    School_scenario = off_license_scenario()[[4]]
    Marae_scenario = off_license_scenario()[[5]]
    Outlet_scenario = off_license_scenario()[[6]]
    Overall_scenario = off_license_scenario()[[7]]
    School_buffer = off_license_scenario()[[8]]
    Marae_buffer = off_license_scenario()[[9]]
    
    CBD = st_union(filter(imd,Areas=="CBD"))
    Trust_Waitakere = st_union(filter(imd, Areas=="Trust - Waitakere"))
    Trust_Portage = st_union(filter(imd, Areas=="Trust - Portage"))
    Existing_Outlets = filter(alcohol, License_Ty == "Off-licence")
    Existing_Outlets = st_join(Existing_Outlets,imd)
    Existing_Outlets = Existing_Outlets[!st_is_empty(Existing_Outlets),drop=FALSE]
    Area_label = st_cast(label[-2,])
    
    if (school_dist>0){
      school_buffer_map = tm_shape(School_buffer) + tm_polygons(col="beige")
      school_map = tm_shape(School_scenario) + tm_dots(col="deepskyblue")
    } else{
      school_buffer_map = NULL
      school_map = NULL
    }
    
    if (marae_dist>0){
      marae_buffer_map = tm_shape(Marae_buffer) + tm_polygons(col="beige")
      marae_map = tm_shape(Marae_scenario) + tm_dots(col="olivedrab1")
    } else{
      marae_buffer_map = NULL
      marae_map = NULL
    }
      
    if (outlet_dist > 0) outlet_map = tm_shape(Outlet_scenario) + tm_dots(col="purple")
    else outlet_map = NULL
    
    if (nrow(Overall_scenario) >0) overall_outlet_map = tm_shape(Overall_scenario) + tm_dots(col="red1")
    else overall_outlet_map = NULL
    
    map = marae_buffer_map + school_buffer_map + tm_shape(Existing_Outlets) + tm_dots(col="snow3") + 
      school_map + marae_map +
      outlet_map + overall_outlet_map +
      tm_shape(School) + tm_dots(col="blue") +
      tm_shape(Marae) + tm_dots(col="green") +
      tm_shape(CBD) + tm_borders(col="slateblue1",lwd=2) +
      tm_shape(Trust_Waitakere) + tm_borders(col="orange",lwd=2) +
      tm_shape(Trust_Portage) + tm_borders(col="violetred1",lwd=2) +
      tm_shape(Area_label) + tm_text("Areas") +
      tm_view(set.view=c(174.74,-36.86,11)) +
      tm_add_legend(type="fill", 
                    labels=c("Marae","School","Existing_Outlets","Marae_scenario",
                             "School_scenario","Outlet_scenario","Overall_scenario"),
                    col=c("green","blue","snow3","olivedrab1",
                          "deepskyblue","purple","red1"))
    
    tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup(c("Marae","School","Marae_buffer","School_buffer",
                                                    "Marae_scenario","School_scenario",
                                                    "Outlet_scenario","Overall_scenario"))
  })
  
  off_license_scenario_density = reactive({
    alcohol = off_license_scenario()[[7]]
    org_density = off_license_density()
    
    if (nrow(alcohol)==0) {
      names(org_density)[which(names(org_density)=='Population')] = 'Population(10,000)'
      names(org_density)[which(names(org_density)=='Shape_area')] = 'Shape_area(km²)'
      org_density
    }else{
      Number_of_outlets_after = lengths(st_intersects(org_density,alcohol))
      org_density$Number_of_outlets_after = Number_of_outlets_after
      org_density$Density_per_10000_people_after = Number_of_outlets_after/org_density$Population 
      org_density$`Density_per_km²_after` = Number_of_outlets_after/org_density$Shape_area
      colnames(org_density)[c(2,4,6)] = c("Number_of_outlets_before","Density_per_10000_people_before","Density_per_km²_before")
      names(org_density)[which(names(org_density)=='Population')] = 'Population(10,000)'
      names(org_density)[which(names(org_density)=='Shape_area')] = 'Shape_area(km²)'
      org_density[,c("Areas","Number_of_outlets_before","Number_of_outlets_after","Population(10,000)",
                     "Density_per_10000_people_before","Density_per_10000_people_after",
                     "Shape_area(km²)","Density_per_km²_before","Density_per_km²_after","geometry")]
    }
    
  })
  
  output$off_license_scenario_density_table = renderDataTable({
    if (nrow(off_license_scenario()[[7]])==0){
      data = off_license_density()
      names(data)[which(names(data)=='Population')] = 'Population(10,000)'
      names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
      as.data.frame(data)[,1:(ncol(data)-1)]
    }else{
      data = as.data.frame(off_license_scenario_density())
      colnames(data) = c("Areas","Number_of_outlets_before","Number_of_outlets_after",
                         "Population","Density_before(pop)","Density_after(pop)",
                         "Shape_area","Density_before(area)","Density_after(area)","geometry")
      names(data)[which(names(data)=='Population')] = 'Population(10,000)'
      names(data)[which(names(data)=='Shape_area')] = 'Shape_area(km²)'
      data[,1:(ncol(data)-1)]
    }
  })
  
  output$off_license_scenario_density_pop_plot = renderLeaflet({
    Existing_Outlets = filter(alcohol(), License_Ty == "Off-licence")
    Resulting_Outlets = off_license_scenario()[[7]]
    if (nrow(Resulting_Outlets)==0){
      alcohol = alcohol()
      Density = off_license_density()
      breaks = sort(Density$Density_per_10000_people)
      breaks = c(floor(breaks[1]), ceiling(breaks))
      breaks = unique(breaks)
      Label_Area = st_cast(Density)[-2,]
      Off_license = filter(alcohol, License_Ty=="Off-licence")
      
      map = tm_shape(Density)+
        tm_polygons(col="Density_per_10000_people",
                    palette="Blues",
                    style="fixed", 
                    breaks=breaks,
                    title="Density per 10000 people") +
        tm_shape(Label_Area) + tm_text("Areas") +
        tm_shape(Off_license) + tm_dots(col = "ivory") +
        tm_view(set.view=c(174.76,-36.8,10))
      
      return(tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license"))
    }else{
      Density = off_license_scenario_density()
      breaks = sort(Density$Density_per_10000_people_after)
      breaks = ceiling(breaks)
      breaks = unique(c(ifelse((breaks[1]-1)>0,min(0,breaks[1]-1),0),breaks))
      Label_Area = st_cast(Density)[-2,]
      map = tm_shape(Density)+
        tm_polygons(col="Density_per_10000_people_after",
                    palette="Blues",
                    style="fixed", 
                    breaks=breaks,
                    title="Density per 10000 people")+
        tm_shape(Label_Area)+tm_text("Areas")+
        tm_shape(Existing_Outlets) + tm_dots("ivory") +
        tm_shape(Resulting_Outlets) + tm_dots("red3") +
        tm_view(set.view=c(174.76,-36.8,10))
      tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup(c("Existing_Outlets","Resulting_Outlets"))
    }
  })
  
  output$off_license_scenario_density_area_plot = renderLeaflet({
    Existing_Outlets = filter(alcohol(), License_Ty == "Off-licence")
    Resulting_Outlets = off_license_scenario()[[7]]
    if (nrow(Resulting_Outlets)==0) {
      alcohol = alcohol()
      Density = off_license_density()
      breaks = sort(Density$`Density_per_km²`)*10
      breaks = c(floor(breaks[1]), ceiling(breaks))
      breaks = unique(breaks)/10
      Label_Area = st_cast(Density)[-2,]
      Off_license = filter(alcohol, License_Ty=="Off-licence")
      
      map = tm_shape(Density)+
        tm_polygons(col = "Density_per_km²",
                    palette = "Purples",
                    style = "fixed",
                    breaks = breaks,
                    title = "Density per km²") +
        tm_shape(Label_Area) + tm_text("Areas") +
        tm_shape(Off_license) + tm_dots(col = "ivory") +
        tm_view(set.view=c(174.76,-36.8,10))
      
      map = tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup("Off_license")
    } else{
      Density = off_license_scenario_density()
      breaks = sort(Density$`Density_per_km²_after`)*10
      breaks = c(floor(breaks[1]), ceiling(breaks))
      breaks = unique(breaks/10)
      Label_Area = st_cast(Density)[-2,]
      map = tm_shape(Density)+
        tm_polygons(col = "Density_per_km²_after",
                    palette = "Purples",
                    style = "fixed",
                    breaks = breaks,
                    title = "Density per km²")+
        tm_shape(Label_Area) + tm_text("Areas")+
        tm_shape(Existing_Outlets) + tm_dots("ivory") +
        tm_shape(Resulting_Outlets) + tm_dots("red3") +
        tm_view(set.view=c(174.76,-36.8,10))
      map = tmap_leaflet(map,in.shiny = TRUE)%>%hideGroup(c("Existing_Outlets","Resulting_Outlets"))
    }
    map
  })
  
  output$off_license_scenario_density_overall_dep_table = renderDataTable({
    before = overall_off_density_table()
    density = off_license_scenario()[[10]]
    
    if(nrow(off_license_scenario()[[7]])==0) return(before)
    else{
      density$Density_per_10000_people = round(density$Density_per_10000_people,4)
      density$`Density_per_km²` = round(density$`Density_per_km²`,6)
      names(density)[4:6]=paste0(names(density)[4:6],"_after")
      density$Density_per_10000_people_before = round(before$Density_per_10000_people,4)
      density$`Density_per_km²_before` = round(before$`Density_per_km²`,6)
      density$Number_of_outlets_before = before$Number_of_outlets
      names(density)[which(names(density)=='Population')] = 'Population(10,000)'
      names(density)[which(names(density)=='Shape_area')] = 'Shape_area(km²)'
      density[,c(1:3,9,4,7,5,8,6)]
    } 
  })
  
  output$off_license_scenario_density_zones_dep_table = renderDataTable({
    before = zones_off_density_table()
    density = off_license_scenario()[[11]]
    
    if(nrow(off_license_scenario()[[7]])==0) return(before)
    else{
      density$Density_per_10000_people = round(density$Density_per_10000_people,4)
      density$`Density_per_km²` = round(density$`Density_per_km²`,6)
      names(density)[5:7]=paste0(names(density)[5:7],"_after")
      density$Density_per_10000_people_before = round(before$Density_per_10000_people,4)
      density$`Density_per_km²_before` = round(before$`Density_per_km²`,6)
      density$Number_of_outlets_before = before$Number_of_outlets
      names(density)[which(names(density)=='Population')] = 'Population(10,000)'
      names(density)[which(names(density)=='Shape_area')] = 'Shape_area(km²)'
      density[,c(1:4,10,5,8,6,9,7)]
      
    } 
  })
  
  # Deprivation Histogram
  output$on_overall_hist_pop = renderPlotly({
    density = overall_on_density_table()
    den = sort(density$Density_per_10000_people)
    EQ = den[5]/den[1]
    RR = density$Density_per_10000_people[density$Quintile==5]/density$Density_per_10000_people[density$Quintile==1]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    plot_ly(data=density,x=paste0("Q",density$Quintile),y=~Density_per_10000_people,type="bar",color=I("skyblue")) %>%
      layout(yaxis=list(title="Density"), xaxis=list(title=paste0("Quintile",'\n',"EQ = ",round(EQ,1),"   RR = ",round(RR,1))))
  })
  
  output$on_overall_hist_area = renderPlotly({
    density = overall_on_density_table()
    den = sort(density$`Density_per_km²`)
    EQ = den[5]/den[1]
    RR = density$`Density_per_km²`[density$Quintile==5]/density$`Density_per_km²`[density$Quintile==1]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    plot_ly(data=density,x=paste0("Q",density$Quintile),y=~`Density_per_km²`,type="bar",color=I("skyblue")) %>%
      layout(yaxis=list(title="Density"), xaxis=list(title=paste0("Quintile",'\n',"EQ = ",round(EQ,1),"   RR = ",round(RR,1))))
  })
  
  output$on_zones_hist_pop = renderPlotly({
    density = zones_on_density_table()
    
    df1 = density[density$Area!="CBD",]
    df2 = density[density$Area=="CBD",]
    
    EQ = sapply(unique(density$Area), 
                function(x){den = sort(density$Density_per_10000_people[density$Area==x]);den[5]/den[1]})
    RR = sapply(unique(density$Area), 
                function(x){
                  df = density[density$Area==x,]
                  df$Density_per_10000_people[df$Quintile==5]/df$Density_per_10000_people[df$Quintile==1]})
    EQ = EQ[c(1,3,2,4)]
    RR = RR[c(1,3,2,4)]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    p1 = plot_ly(data=df1,x=~Area,y=~Density_per_10000_people, type="bar",color=~factor(Quintile),name=~factor(Quintile)) %>% 
      style(showlegend = FALSE)
    p2 = plot_ly(data=df2,x=~Area,y=~Density_per_10000_people, type="bar",color=~factor(Quintile)) %>% 
      layout(yaxis=list(side="right"))
    
    subplot(p1, p2, widths=c(0.75,0.25)) %>%
      layout(legend = list(title=list(text="Quintile"),orientation = "h",xanchor = "center", x = 0.5, y = 0.95), 
             yaxis=list(title="Density")) %>%
      add_annotations(x = c(.12, .36, .61, 0.89),
                      y = -0.05,
                      text = paste0("EQ = ",round(EQ,1),"    RR = ",round(RR,1)),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      yanchor = "top",
                      showarrow = F)
  })
  
  output$on_zones_hist_area = renderPlotly({
    density = zones_on_density_table()
    
    df1 = density[density$Area!="CBD",]
    df2 = density[density$Area=="CBD",]
    
    EQ = sapply(unique(density$Area), 
                function(x){den = sort(density$`Density_per_km²`[density$Area==x]);den[5]/den[1]})
    RR = sapply(unique(density$Area), 
                function(x){
                  df = density[density$Area==x,]
                  df$`Density_per_km²`[df$Quintile==5]/df$`Density_per_km²`[df$Quintile==1]})
    EQ = EQ[c(1,3,2,4)]
    RR = RR[c(1,3,2,4)]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    p1 = plot_ly(data=df1,x=~Area,y=~`Density_per_km²`, type="bar",color=~factor(Quintile),name=~factor(Quintile)) %>% 
      style(showlegend = FALSE)
    p2 = plot_ly(data=df2,x=~Area,y=~`Density_per_km²`, type="bar",color=~factor(Quintile)) %>% 
      layout(yaxis=list(side="right"))
    
    subplot(p1, p2, widths=c(0.75,0.25)) %>%
      layout(legend = list(title=list(text="Quintile"),orientation = "h",xanchor = "center", x = 0.5, y = 0.95), 
             yaxis=list(title="Density")) %>%
      add_annotations(x = c(.12, .36, .61, 0.89),
                      y = -0.05,
                      text = paste0("EQ = ",round(EQ,1),"    RR = ",round(RR,1)),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      yanchor = "top",
                      showarrow = F)
  })
  
  output$off_overall_hist_pop = renderPlotly({
    density = overall_off_density_table()
    den = sort(density$Density_per_10000_people)
    EQ = den[5]/den[1]
    RR = density$Density_per_10000_people[density$Quintile==5]/density$Density_per_10000_people[density$Quintile==1]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    plot_ly(data=density,x=paste0("Q",density$Quintile),y=~Density_per_10000_people,type="bar",color=I("skyblue")) %>%
      layout(yaxis=list(title="Density"), xaxis=list(title=paste0("Quintile",'\n',"EQ = ",round(EQ,1),"   RR = ",round(RR,1))))
  })
  
  output$off_overall_hist_area = renderPlotly({
    density = overall_off_density_table()
    den = sort(density$`Density_per_km²`)
    EQ = den[5]/den[1]
    RR = density$`Density_per_km²`[density$Quintile==5]/density$`Density_per_km²`[density$Quintile==1]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    plot_ly(data=density,x=paste0("Q",density$Quintile),y=~`Density_per_km²`,type="bar",color=I("skyblue")) %>%
      layout(yaxis=list(title="Density"), xaxis=list(title=paste0("Quintile",'\n',"EQ = ",round(EQ,1),"   RR = ",round(RR,1))))
  })
  
  output$off_zones_hist_pop = renderPlotly({
    density = zones_off_density_table()
    
    df1 = density[density$Area!="CBD",]
    df2 = density[density$Area=="CBD",]
    
    EQ = sapply(unique(density$Area), 
                function(x){den = sort(density$Density_per_10000_people[density$Area==x]);den[5]/den[1]})
    RR = sapply(unique(density$Area), 
                function(x){
                  df = density[density$Area==x,]
                  df$Density_per_10000_people[df$Quintile==5]/df$Density_per_10000_people[df$Quintile==1]})
    EQ = EQ[c(1,3,2,4)]
    RR = RR[c(1,3,2,4)]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    p1 = plot_ly(data=df1,x=~Area,y=~Density_per_10000_people, type="bar",color=~factor(Quintile),name=~factor(Quintile)) %>% 
      style(showlegend = FALSE)
    p2 = plot_ly(data=df2,x=~Area,y=~Density_per_10000_people, type="bar",color=~factor(Quintile)) %>% 
      layout(yaxis=list(side="right"))
    
    subplot(p1, p2, widths=c(0.75,0.25)) %>%
      layout(legend = list(title=list(text="Quintile"),orientation = "h",xanchor = "center", x = 0.5, y = 0.95), 
             yaxis=list(title="Density")) %>%
      add_annotations(x = c(.12, .36, .61, 0.89),
                      y = -0.05,
                      text = paste0("EQ = ",round(EQ,1),"    RR = ",round(RR,1)),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      yanchor = "top",
                      showarrow = F)
  })
  
  output$off_zones_hist_area = renderPlotly({
    density = zones_off_density_table()
    
    df1 = density[density$Area!="CBD",]
    df2 = density[density$Area=="CBD",]
    
    EQ = sapply(unique(density$Area), 
                function(x){den = sort(density$`Density_per_km²`[density$Area==x]);den[5]/den[1]})
    RR = sapply(unique(density$Area), 
                function(x){
                  df = density[density$Area==x,]
                  df$`Density_per_km²`[df$Quintile==5]/df$`Density_per_km²`[df$Quintile==1]})
    EQ = EQ[c(1,3,2,4)]
    RR = RR[c(1,3,2,4)]
    
    EQ = ifelse(is.na(EQ)|is.infinite(EQ),NA,EQ)
    RR = ifelse(is.na(RR)|is.infinite(RR),NA,RR)
    
    p1 = plot_ly(data=df1,x=~Area,y=~`Density_per_km²`, type="bar",color=~factor(Quintile),name=~factor(Quintile)) %>% 
      style(showlegend = FALSE)
    p2 = plot_ly(data=df2,x=~Area,y=~`Density_per_km²`, type="bar",color=~factor(Quintile)) %>% 
      layout(yaxis=list(side="right"))
    
    subplot(p1, p2, widths=c(0.75,0.25)) %>%
      layout(legend = list(title=list(text="Quintile"),orientation = "h",xanchor = "center", x = 0.5, y = 0.95), 
             yaxis=list(title="Density")) %>%
      add_annotations(x = c(.12, .36, .61, 0.89),
                      y = -0.05,
                      text = paste0("EQ = ",round(EQ,1),"    RR = ",round(RR,1)),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      yanchor = "top",
                      showarrow = F)
  })
  
  output$scenario_zones_hist_pop = renderPlotly({
    before = zones_off_density_table()
    after = off_license_scenario()[[11]]
    
    EQ_before = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                function(x){den = sort(before$Density_per_10000_people[before$Area==x]);den[5]/den[1]})
    RR_before = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                function(x){
                  df = before[before$Area==x,]
                  df$Density_per_10000_people[df$Quintile==5]/df$Density_per_10000_people[df$Quintile==1]})
    EQ_after = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                       function(x){den = sort(after$Density_per_10000_people[after$Area==x]);den[5]/den[1]})
    RR_after = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                       function(x){
                         df = after[after$Area==x,]
                         df$Density_per_10000_people[df$Quintile==5]/df$Density_per_10000_people[df$Quintile==1]})
    EQ_before = round(EQ_before,1)
    RR_before = round(RR_before,1)
    EQ_after = round(EQ_after,1)
    RR_after = round(RR_after,1)
    
    EQ_before = ifelse(is.na(EQ_before)|is.infinite(EQ_before),NA,EQ_before)
    RR_before = ifelse(is.na(RR_before)|is.infinite(RR_before),NA,RR_before)
    EQ_after = ifelse(is.na(EQ_after)|is.infinite(EQ_after),NA,EQ_after)
    RR_after = ifelse(is.na(RR_after)|is.infinite(RR_after),NA,RR_after)
    
    df = merge(before,after,by=c("Area","Quintile"))
    df$Quintile=paste0("Q",df$Quintile)
    p1 = plot_ly(df[df$Area=="CBD",],x=~Quintile, y=~Density_per_10000_people.x, type='bar', color=I("royalblue"))%>%
      add_trace(y=~Density_per_10000_people.y, color=I("orange"))%>%style(showlegend = FALSE)%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "bottom",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[1],'    RR = ',RR_before[1],'\n',
                                    'A: EQ = ',EQ_after[1],'    RR = ',RR_after[1]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    p2 = plot_ly(df[df$Area=="Other",],x=~Quintile, y=~Density_per_10000_people.x, type='bar', color=I("royalblue"))%>%
      add_trace(y=~Density_per_10000_people.y, color=I("orange"))%>%style(showlegend = FALSE)%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "bottom",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[2],'    RR = ',RR_before[2],'\n',
                                    'A: EQ = ',EQ_after[2],'    RR = ',RR_after[2]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    p3 = plot_ly(df[df$Area=="Trust - Waitakere",],x=~Quintile, y=~Density_per_10000_people.x, type='bar', color=I("royalblue"))%>%
      add_trace(y=~Density_per_10000_people.y, color=I("orange"))%>%style(showlegend = FALSE)%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "center",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[3],'    RR = ',RR_before[3],'\n',
                                    'A: EQ = ',EQ_after[3],'    RR = ',RR_after[3]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    p4 = plot_ly(df[df$Area=="Trust - Portage",],x=~Quintile, y=~Density_per_10000_people.x, type='bar', color=I("royalblue"),name="Before")%>%
      add_trace(y=~Density_per_10000_people.y, color=I("orange"),name="After")%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "center",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[4],'    RR = ',RR_before[4],'\n',
                                    'A: EQ = ',EQ_after[4],'    RR = ',RR_after[4]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    subplot(p1,p2,p3,p4,margin = 0.05,nrows=2)
  })
  
  output$scenario_zones_hist_area = renderPlotly({
    before = zones_off_density_table()
    after = off_license_scenario()[[11]]
    
    EQ_before = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                       function(x){den = sort(before$`Density_per_km²`[before$Area==x]);den[5]/den[1]})
    RR_before = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                       function(x){
                         df = before[before$Area==x,]
                         df$`Density_per_km²`[df$Quintile==5]/df$`Density_per_km²`[df$Quintile==1]})
    EQ_after = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                      function(x){den = sort(after$`Density_per_km²`[after$Area==x]);den[5]/den[1]})
    RR_after = sapply(c("CBD","Other","Trust - Waitakere","Trust - Portage"), 
                      function(x){
                        df = after[after$Area==x,]
                        df$`Density_per_km²`[df$Quintile==5]/df$`Density_per_km²`[df$Quintile==1]})
    EQ_before = round(EQ_before,1)
    RR_before = round(RR_before,1)
    EQ_after = round(EQ_after,1)
    RR_after = round(RR_after,1)
    
    EQ_before = ifelse(is.na(EQ_before)|is.infinite(EQ_before),NA,EQ_before)
    RR_before = ifelse(is.na(RR_before)|is.infinite(RR_before),NA,RR_before)
    EQ_after = ifelse(is.na(EQ_after)|is.infinite(EQ_after),NA,EQ_after)
    RR_after = ifelse(is.na(RR_after)|is.infinite(RR_after),NA,RR_after)
    
    df = merge(before,after,by=c("Area","Quintile"))
    df$Quintile=paste0("Q",df$Quintile)
    p1 = plot_ly(df[df$Area=="CBD",],x=~Quintile, y=~`Density_per_km².x`, type='bar', color=I("royalblue"))%>%
      add_trace(y=~`Density_per_km².y`, color=I("orange"))%>%style(showlegend = FALSE)%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "bottom",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[1],'    RR = ',RR_before[1],'\n',
                                    'A: EQ = ',EQ_after[1],'    RR = ',RR_after[1]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    p2 = plot_ly(df[df$Area=="Other",],x=~Quintile, y=~`Density_per_km².x`, type='bar', color=I("royalblue"))%>%
      add_trace(y=~`Density_per_km².y`, color=I("orange"))%>%style(showlegend = FALSE)%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "bottom",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[2],'    RR = ',RR_before[2],'\n',
                                    'A: EQ = ',EQ_after[2],'    RR = ',RR_after[2]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    p3 = plot_ly(df[df$Area=="Trust - Waitakere",],x=~Quintile, y=~`Density_per_km².x`, type='bar', color=I("royalblue"))%>%
      add_trace(y=~`Density_per_km².y`, color=I("orange"))%>%style(showlegend = FALSE)%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "center",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[3],'    RR = ',RR_before[3],'\n',
                                    'A: EQ = ',EQ_after[3],'    RR = ',RR_after[3]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    p4 = plot_ly(df[df$Area=="Trust - Portage",],x=~Quintile, y=~`Density_per_km².x`, type='bar', color=I("royalblue"),name="Before")%>%
      add_trace(y=~`Density_per_km².y`, color=I("orange"),name="After")%>%
      add_annotations(text = ~Area,
                      x = 0.5,y = 1,yref = "paper",xref = "paper",
                      xanchor = "center",yanchor = "center",showarrow = FALSE) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before[4],'    RR = ',RR_before[4],'\n',
                                    'A: EQ = ',EQ_after[4],'    RR = ',RR_after[4]),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "top",showarrow = FALSE)
    subplot(p1,p2,p3,p4,margin = 0.05,nrows=2)
  })
  
  output$scenario_overall_hist_pop = renderPlotly({
    before = overall_off_density_table()
    after = off_license_scenario()[[10]]
    
    den_before = sort(before$Density_per_10000_people)
    EQ_before = round(den_before[5]/den_before[1],1)
    RR_before = round(before$Density_per_10000_people[before$Quintile==5]/before$Density_per_10000_people[before$Quintile==1],1)
    
    den_after = sort(after$Density_per_10000_people)
    EQ_after = round(den_after[5]/den_after[1],1)
    RR_after = round(after$Density_per_10000_people[after$Quintile==5]/after$Density_per_10000_people[after$Quintile==1],1)
    
    EQ_before = ifelse(is.na(EQ_before)|is.infinite(EQ_before),NA,EQ_before)
    RR_before = ifelse(is.na(RR_before)|is.infinite(RR_before),NA,RR_before)
    EQ_after = ifelse(is.na(EQ_after)|is.infinite(EQ_after),NA,EQ_after)
    RR_after = ifelse(is.na(RR_after)|is.infinite(RR_after),NA,RR_after)
    
    plot_ly(before, x=paste0("Q",before$Quintile),y=~Density_per_10000_people,type="bar",color=I("seagreen"),name="Before")%>%
      add_trace(y=after$Density_per_10000_people, color=I("plum1"),name="After")%>%
      layout(yaxis=list(title="Density")) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before,'    RR = ',RR_before,'\n',
                                    'A: EQ = ',EQ_after,'    RR = ',RR_after),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "bottom",showarrow = FALSE)
  })
  
  output$scenario_overall_hist_area = renderPlotly({
    before = overall_off_density_table()
    after = off_license_scenario()[[10]]
    
    den_before = sort(before$`Density_per_km²`)
    EQ_before = round(den_before[5]/den_before[1],1)
    RR_before = round(before$`Density_per_km²`[before$Quintile==5]/before$`Density_per_km²`[before$Quintile==1],1)
    
    den_after = sort(after$`Density_per_km²`)
    EQ_after = round(den_after[5]/den_after[1],1)
    RR_after = round(after$`Density_per_km²`[after$Quintile==5]/after$`Density_per_km²`[after$Quintile==1],1)
    
    EQ_before = ifelse(is.na(EQ_before)|is.infinite(EQ_before),NA,EQ_before)
    RR_before = ifelse(is.na(RR_before)|is.infinite(RR_before),NA,RR_before)
    EQ_after = ifelse(is.na(EQ_after)|is.infinite(EQ_after),NA,EQ_after)
    RR_after = ifelse(is.na(RR_after)|is.infinite(RR_after),NA,RR_after)
    
    plot_ly(before, x=paste0("Q",before$Quintile),y=~`Density_per_km²`,type="bar",color=I("seagreen"),name="Before")%>%
      add_trace(y=after$`Density_per_km²`, color=I("plum1"),name="After")%>%
      layout(yaxis=list(title="Density")) %>%
      add_annotations(text = paste0('B: EQ = ',EQ_before,'    RR = ',RR_before,'\n',
                                    'A: EQ = ',EQ_after,'    RR = ',RR_after),
                      x = 0.01,y = 0.95,yref = "paper",xref = "paper",
                      xanchor = "left",yanchor = "bottom",showarrow = FALSE)
  })
}

shinyApp(ui=ui, server=server)
