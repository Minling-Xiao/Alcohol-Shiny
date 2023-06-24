library(sf)
library(tmap)
library(dplyr)
library(plotly)
library(leaflet)
library(stringr)
library(tidyverse)
library(sp)
library(rgeos)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(RColorBrewer)


ui = navbarPage(
  theme = shinytheme("yeti"),
  title = strong("Access to Alcohol"),
  
  tabPanel("Home", icon=icon("home"),
           fluidRow(
             column(5,h2(strong("Introduction")),
                    includeMarkdown(file.path("text", "Intro.md"))),
             column(7, h2(strong("Licensed Alcohol Outlets Locations")),
                    leafletOutput("alcohol_map", height = 700)%>%withSpinner(color="grey"))),
           hr(),
           fluidRow(column(12,includeMarkdown(file.path("text", "DataSource.md"))))),
  
  navbarMenu("Density - 4 Zones",icon=icon("layer-group"),
             tabPanel("On License", icon = icon("utensils"),
                      h2(strong("On License")),
                      hr(),
                      fluidRow(
                        column(6,"By Population",
                               leafletOutput("on_density_pop_map", height = 700)%>%withSpinner(color="grey")),
                        column(6,"By Shape Area",
                               leafletOutput("on_density_area_map", height = 700)%>%withSpinner(color="grey"))),
                      fluidRow(
                        column(6,dataTableOutput("on_density_pop_table")%>%withSpinner(color="grey")),
                        column(6,dataTableOutput("on_density_area_table")%>%withSpinner(color="grey")))),
             
             tabPanel("Off License", icon = icon("wine-bottle"),
                      h2(strong("Off License")),
                      hr(),
                      fluidRow(
                        column(6,"By Population"),
                        column(6,"By Shape Area")),
                      fluidRow(
                        column(6,leafletOutput("off_density_pop_map", height = 700)%>%withSpinner(color="grey")),
                        column(6,leafletOutput("off_density_area_map", height = 700)%>%withSpinner(color="grey"))),
                      fluidRow(
                        column(6,dataTableOutput("off_density_pop_table")%>%withSpinner(color="grey")),
                        column(6,dataTableOutput("off_density_area_table")%>%withSpinner(color="grey"))))),
  
  navbarMenu("Density - Deprivation",icon=icon("bars-staggered"),
             tabPanel("On License",icon = icon("utensils"),
                      h2(strong("On License")),
                      tabsetPanel(
                        tabPanel("Histogram",
                                 fluidRow(column(3,h4(strong("By Population")))),
                                 br(),
                                 fluidRow(column(3,plotlyOutput("on_overall_hist_pop")%>%withSpinner(color="grey")),
                                          column(8,offset=1,plotlyOutput("on_zones_hist_pop")%>%withSpinner(color="grey"))),
                                 hr(),
                                 fluidRow(column(3,h4(strong("By Shape Area")))),
                                 br(),
                                 fluidRow(column(3,plotlyOutput("on_overall_hist_area")%>%withSpinner(color="grey")),
                                          column(8,offset=1,plotlyOutput("on_zones_hist_area")%>%withSpinner(color="grey")))),
                        tabPanel("Summary",
                                 br(),
                                 h4(strong("Overall")),
                                 dataTableOutput("overall_on_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("CBD")),
                                 dataTableOutput("cbd_on_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("Trust - Portage")),
                                 dataTableOutput("portage_on_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("Trust - Waitakere")),
                                 dataTableOutput("waitakere_on_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("The rest of Auckland")),
                                 dataTableOutput("rest_on_density_table")%>%withSpinner(color="grey")
                                 ))),
             tabPanel("Off License",icon = icon("wine-bottle"),
                      h2(strong("Off License")),
                      tabsetPanel(
                        tabPanel("Histogram",
                                 fluidRow(column(3,h4(strong("By Population")))),
                                 br(),
                                 fluidRow(column(3,plotlyOutput("off_overall_hist_pop")%>%withSpinner(color="grey")),
                                          column(8,offset=1,plotlyOutput("off_zones_hist_pop")%>%withSpinner(color="grey"))),
                                 hr(),
                                 fluidRow(column(3,h4(strong("By Shape Area")))),
                                 br(),
                                 fluidRow(column(3,plotlyOutput("off_overall_hist_area")%>%withSpinner(color="grey")),
                                          column(8,offset=1,plotlyOutput("off_zones_hist_area")%>%withSpinner(color="grey")))),
                        tabPanel("Summary",
                                 br(),
                                 h4(strong("Overall")),
                                 dataTableOutput("overall_off_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("CBD")),
                                 dataTableOutput("cbd_off_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("Trust - Portage")),
                                 dataTableOutput("portage_off_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("Trust - Waitakere")),
                                 dataTableOutput("waitakere_off_density_table")%>%withSpinner(color="grey"),
                                 hr(),
                                 h4(strong("The rest of Auckland")),
                                 dataTableOutput("rest_off_density_table")%>%withSpinner(color="grey")
                        )))),

  navbarMenu("School & Marae",icon=icon("map-location-dot"),
             tabPanel("Off License", icon = icon("wine-bottle"),
                      titlePanel(h2(strong("Off License"))),
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     strong("Scenario - No outlets within:"),
                                     br(),
                                     numericInput(inputId = "off_license_distance_from_school",
                                                  label="Distance from a school:",value = 0, min=0),
                                     numericInput(inputId = "off_license_distance_from_Marae",
                                                  label="Distance from a Marae:",value = 0, min=0),
                                     numericInput(inputId = "off_license_distance_between_outlets",
                                                  label="Distance between outlets:",value = 0, min="0"),
                                     hr(),
                                     strong("Apply to:"),
                                     checkboxInput(inputId = "scenario_area3", label = "All 4 areas", value =TRUE ),
                                     uiOutput("scenario_area_off"),
                                     br(),
                                     actionButton(inputId = "go_off_license", label = "Go")),
                        mainPanel(width = 10,
                                  tabsetPanel(
                                    tabPanel("Map",leafletOutput("off_license_scenario_map",height = 800)
                                             %>%withSpinner(color="grey")),
                                    tabPanel("Density - Zones",
                                             fluidRow(
                                               column(6,h4(strong("By Population")),
                                                      leafletOutput("off_license_scenario_density_pop_plot", height = 600)%>%
                                                        withSpinner(color="grey")),
                                               column(6,h4(strong("By Shape Area")),
                                                      leafletOutput("off_license_scenario_density_area_plot", height = 600)%>%
                                                        withSpinner(color="grey")))),
                                    tabPanel("Density - Deprivation",
                                             fluidRow(column(4,h4(strong("By Population")))),
                                             fluidRow(column(4,plotlyOutput("scenario_overall_hist_pop")
                                                             %>%withSpinner(color="grey")),
                                                      column(8,plotlyOutput("scenario_zones_hist_pop")
                                                             %>%withSpinner(color="grey"))),
                                             hr(),
                                             fluidRow(column(4,h4(strong("By Shape Area")))),
                                             fluidRow(column(4,plotlyOutput("scenario_overall_hist_area")
                                                             %>%withSpinner(color="grey")),
                                                      column(8,plotlyOutput("scenario_zones_hist_area")
                                                             %>%withSpinner(color="grey")))),
                                    tabPanel("Summary",
                                             h4(strong("Density - 4 Zones")),
                                             br(),
                                             dataTableOutput("off_license_scenario_density_table")%>%withSpinner(color="grey"),
                                             hr(),
                                             h4(strong("Density - Deprivation")),
                                             br(),
                                             "Overall",
                                             br(),
                                             dataTableOutput("off_license_scenario_density_overall_dep_table")
                                             %>%withSpinner(color="grey"),
                                             br(),
                                             "Zones",
                                             br(),
                                             dataTableOutput("off_license_scenario_density_zones_dep_table")
                                             %>%withSpinner(color="grey")))))))
  
)

