#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(DT)
library(colorspace)
library(leaflet)
library(rgdal)

 

# Process Carbon Stock Data
stocks = read_csv("data/stocks.csv")
stocks$StockGroup = factor(stocks$StockGroup, levels=c("TEC","Soil","Live","DOM"))

# Process Net Flux List
netFlux = read_csv("data/net_flux.csv")
netFlux$Flux = factor(netFlux$Flux, levels=c("NECB","NEP","Rh","NPP"))
unique(netFlux$Flux)

# Process Transition Flows Data
transFlows = read_csv("data/transitionFlows.csv") %>% mutate(Mean=Mean/1000, Lower=Lower/1000, Upper=Upper/1000)
transFlows$LUC = factor(transFlows$LUC, levels=c("BAU", "High", "Medium", "Low"))
transFlows$TransitionGroup = factor(transFlows$TransitionGroup, levels=c("Drought","Fire","Forest Clearcut", "Forest Selection","Orchard Removal","Ag Expansion","Urbanization"))

# Process Transition Data
disturbanceData = read_csv("data/disturbances.csv")
disturbanceData$Severity = factor(disturbanceData$Severity, levels=c("High", "Medium", "Low"))
disturbanceData$LUC = factor(disturbanceData$LUC, levels=c("BAU", "High", "Medium", "Low"))


# Process LULC Data
lulc = read_csv("data/lulc.csv")
lulc$LUC = factor(lulc$LUC, levels=c("BAU", "High", "Medium", "Low"))


# Define unique lists
ecoList = unique(stocks$EcoregionName)
lucList = unique(stocks$LUC)
stockList = unique(stocks$StockGroup)

# Define color palettes
stockPal = c("DOM"="#D55E00", "Live"="#009E73", "Soil"="#E69F00", "TEC"="#f3f3f3")
gcmPal = c("CanESM2"="#35f06d", "CNRM-CM5"="#0072B2", "HadGEM2-ES"="#D55E00", "MIROC5"="#CC79A7")
fluxPal = c("NPP"="#2CA02C", "Rh"="#8C564B", "NEP"="#1F77B4", "NECB"="#E1750E")
sevPal = c("High"="#f03b20", "Medium"="#feb24c", "Low"="#ffeda0")
statePal = c("Forest"="#38814E", "Grassland"="#FDE9AA", "Shrubland"="#DCCA8F")
flowPal = c("Emission"="#a6cee3", "Harvest"="#33a02c", "Mortality"="#b2df8a", "Deadfall"="#1f78b4")
transPal = c("Urbanization"="#e5c494", "Ag Expansion"="#fc8d62", "Orchard Removal"="#8da0cb", "Forest Selection"="#a6d854", "Forest Clearcut"="#66c2a5", "Fire"="#ffd92f", "Drought"="#e78ac3")
lulcPal = c("Agriculture"="#CA9146","Barren"="#D2CDC0","Developed"="#B50000","Forest"="#38814E","Grassland"="#FDE9AA","Shrubland"="#DCCA8F","Water"="#5475A8","Wetland"="#C8E6F8", "SnowIce"="#ffffff")
ecoPal = c("Coast Range"="#31a354", "Cascades"="#78c679", "East Cascades"="#ffffcc", "Klamath Mtns."="#c2e699", "Sierra Nevada"="#006837",
           "Central B&R"="#feedde", "Northern B&R"="#fdbe85", "Mojave B&R"="#fd8d3c", "Sonoran B&R"="#d94701",
           "Central Valley"="#41b6c4", "Oak Woodlands"="#225ea8", "SoCal Mtns."="#a1dab4")

#Define ecoregion shapefile
ecoregions <- readOGR("www/ca_eco_l3/ca_diss_simp.shp",layer = "ca_diss_simp", GDAL1_integer64_policy = TRUE) 

# Define UI for application that draws a histogram
ui = fluidPage(theme = shinytheme("flatly"),
       useShinyjs(),
       includeHTML("www/header.html"),
       tags$head(includeCSS("www/common.css")),
       navbarPage("California Carbon Scenarios",id="navTabset",
        #Home layout
        tabPanel("Home",value = "homePanel",    
           #Banner
           tags$div(
             tags$div(  
               tags$div(
                 column(8, 
                        tags$img(src = "ca_boxes.png", height = "200px", class="toolBoxImg"),
                        tags$h1("California Carbon Scenarios", id="bannerHeader"),
                        tags$p("Land change and carbon balance scenario projections for the State of California with the LUCAS model", id="bannerText"),
                        offset = 3),
                 class="row"),
               class = "container container-home"), 
             class="row", id="banner"),
           #Tools Row
           tags$div(
             tags$div( 
               tags$hr(),
               tags$div(
                  tags$p("Explore 32 unique land change and carbon balance scenarios, consisting of 4 land-use scenarios and 2 radiative forcing scenarios as simulated by 4 global climate models for the State of California", id="scenariosText"),
               class="row", id="toolsTitleRows"),  
               tags$hr(),
                
               tags$div(
                 tags$div(
                   tags$h3("Carbon Stocks"),
                   tags$div(
                     tags$img(src = "stocks_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Explore projections of carbon stored in Living Biomass, Dead Organic Matter, and Soils"),
                     actionButton('jumpToP1', 'Explore stocks'), 
                     class ="row"),
                   class="toolBox"),     
                 tags$div(
                   tags$h3("Carbon Fluxes"),
                   tags$div(
                     tags$img(src = "net_flux_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Explore projections of net carbon fluxes including NPP, NEP, and NECB"),
                     actionButton('jumpToP2', 'Explore fluxes'),
                     class ="row"),
                   class="toolBox"),   
                 tags$div(  
                   tags$h3("Carbon Flux from Land Change"), 
                   tags$div(
                     tags$img(src = "transition_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Explore projections of carbon fluxes resulting from land use change and ecosystem disturbance"),
                     actionButton('jumpToP5', 'Explore fluxes'),
                     class ="row"),
                   class="toolBox"),  
                 tags$div(
                   tags$h3("Land Use & Land Cover"),
                   tags$div(
                     tags$img(src = "landuse_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Explore model scenario output for land use and land cover change over time"),
                     actionButton('jumpToP3', 'Explore state classes'), 
                     class ="row"),
                   class="toolBox"), 
                 tags$div(
                   tags$h3("Wildfire & Drought"),
                   tags$div(
                     tags$img(src = "disturbance_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Explore climate based scenario trends in both wildfire and drought occurrence"),
                     actionButton('jumpToP4', 'Explore disturbance'),
                     class ="row"),
                   class="toolBox"),     
                 id="toolsRow"),    
               class = "container container-home"),      
             class="row", id="tools")    

            ),
            tabPanel("Dashboard",value ="dashboardPanel", 
                fluidPage(
                  tags$div(
                    tags$button(
                      id = "jumpToP10",
                      class = "btn btn-default action-button shiny-bound-inputs dashboard-type selected",
                      name =  "stocks_80.png",
                      img(src = "stocks_80_white.png",
                          height = "60px"),
                      tags$span("Carbon Stocks")
                    ),
                    tags$button(
                      id = "jumpToP20",
                      class = "btn btn-default action-button shiny-bound-inputs dashboard-type",
                      name =  "net_flux_80.png",
                      img(src = "net_flux_80.png",
                          height = "60px"),
                      tags$span("Carbon Fluxes")
                    ),
                    tags$button(
                      id = "jumpToP50",
                      class = "btn btn-default action-button shiny-bound-inputs dashboard-type",
                      name =  "transition_80.png",
                      img(src = "transition_80.png",
                          height = "60px"),
                        tags$span("Carbon Flux from Land Change")
                    ),
                    tags$button(
                      id = "jumpToP30",
                      class = "btn btn-default action-button shiny-bound-inputs dashboard-type",
                      name =  "landuse_80.png", 
                      img(src = "landuse_80.png", 
                          height = "60px"),
                      tags$span("Land Use & Land Cover")
                    ),
                    tags$button(
                      id = "jumpToP40",
                      class = "btn btn-default action-button shiny-bound-inputs dashboard-type",   
                      name =  "disturbance_80.png",
                      img(src = "disturbance_80.png",
                          height = "60px"),  
                      tags$span("Wildfire & Drought")
                    ),
                   id="buttomRow"),
                   
                    hr(),
                
                    sidebarLayout(
                       sidebarPanel(width=3, 
                           tags$h3("Model Variables", 
                             actionButton("aboutModelVariables", "",icon("glyphicon glyphicon-info-sign", lib = "glyphicon")),      
                            id="sidebarTitle"),  
                           tags$hr(id="sidebarBreak"),
                           awesomeRadio(
                             inputId="ecoregion",
                             label="Region of Interest",
                             choices=sort(unique(stocks$EcoregionName)),
                             selected="State",
                             width="100%",
                             checkbox=TRUE),
                           prettySwitch(
                             inputId = "view_map",
                             label = "Choose ecoregion on map", 
                             value=FALSE,
                             status="success",
                             fill = TRUE),      
                           leafletOutput("mymap"), 
                           awesomeRadio(
                             inputId="luc",
                             label="Land Use Scenario",
                             choices=sort(unique(stocks$LUC)),
                             selected="BAU",
                             width="100%",
                             checkbox=TRUE),
                           checkboxGroupButtons(inputId="rcp", 
                                                label="Climate Scenarios", 
                                                choiceValues=unique(stocks$RCP),
                                                choiceNames=c("Low Emissions (RCP 4.5)", "High Emissions (RCP 8.5)"),
                                                selected="rcp45",
                                                direction="vertical",
                                                size="sm",
                                                width="100%",
                                                checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                           checkboxGroupButtons(inputId="gcm", 
                                                label="Climate Models", 
                                                choiceValues=unique(stocks$GCM),
                                                choiceNames=c("Average (CanESM2)","Warm-Wet (CNRM-CM5)", "Hot-Dry (HadGEM2-ES)", "Complement (MIROC5)"),
                                                selected="CanESM2",
                                                direction="vertical",
                                                size="sm",
                                                width="100%",
                                                checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                           sliderTextInput(inputId="years",
                                           label="Select years to plot",
                                           choices=seq(2001,2101,1),
                                           selected=c(2001,2100),
                                           width="100%",
                                           grid=FALSE,
                                           force_edges=T),
                           prettySwitch(
                             inputId="ci1",
                             label="Toggle 95% Confidence Intervals",
                             value=TRUE,
                             status="primary",
                             slim=TRUE)),
                       
                       mainPanel(width=9,   
                           tabsetPanel(id="dashboardTabset",
                               tabPanel("Carbon Stocks",value = "Carbon Stocks", width=12,        
                                  tabsetPanel(
                                    tabPanel("Ecosystem Carbon Storage Over Time",    
                                             
                                             
                                                       fluidRow(
                                                                column(width=12, align="left", h2("Ecosystem Carbon Storage Over Time")),
                                                                column(width=12, align="left", "Use the selection tools at left to plot projected carbon storage in millions of metric tons of carbon (y-axis) by year (x-axis) over your preferred region, scenario, and range of years. The gray shaded area shows the full
                                                                       range of values projected under the 32 alternative scenarios. The colored lines and ribbons show the range for each unique scenario.
                                                                       Toggle between the four carbon stock groups using the buttons on the upper right to show carbon storage by stock groups. Soil includes soil organic carbon, DOM includes all 
                                                                       dead organic matter sotred in litter and standing and dowed dead vegetation, and Live includes all above- and below-ground living vegetation.
                                                                       TEC is total ecosystem carbon and is the sum of the Soil, DOM, and Live pools."),
                                                                column(width=12, align="right",
                                                                       radioGroupButtons(width=250,
                                                                                         inputId = "stockGroup", label = actionButton("aboutCarbonStocks", " Carbon Stocks",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton"),  
                                                                                         choices = c("TEC","Soil","Live","DOM"),
                                                                                         selected="TEC",
                                                                                         size="sm",
                                                                                         justified = TRUE, 
                                                                                         checkIcon = list(yes = icon("signal", lib = "glyphicon")))
                                                                ),
                                                                column(width=12, align="right",
                                                                        plotOutput("stocksPlot1", height="700", hover = hoverOpts("stocksPlot1_hover", delay = 20, delayType = "debounce")),
                                                                        uiOutput("stocksPlot1_hover_info"),
                                                                        prettySwitch(
                                                                          inputId = "showStockTable",
                                                                          label = "View Chart Data",  
                                                                          value=FALSE,
                                                                          status="success",
                                                                          fill = TRUE),
                                                                        DTOutput("stocktable")))),
                                    tabPanel("Net Change in Ecosystem Carbon Storage",
                                            
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Net Change in Ecosystem Carbon Storage")),
                                                        column(width=12, align="left", " Use the selection tools at left to plot net change in carbon storage in  millions of metric tons of carbon (y-axis) for each of the stock groups between two dates, over your preferred region, scenario, and range of years.
                                                               Negative values indicate a net loss of carbon from ecosystems. RCP scenarios are shown on the x-axis and can be disaggregated 
                                                               for each climate model (GCM). Soil includes soil organic carbon, DOM includes all dead organic matter sotred in litter and 
                                                               standing and dowed dead vegetation, and Live includes all above- and below-ground living vegetation. TEC is total ecosystem 
                                                               carbon and is the sum of the Soil, DOM, and Live pools. Error bars represent the 95% interval based on 100 MC iterations."),
                                                        column(width=12, align="right",actionButton("aboutNetChangeCarbon", " Carbon Stocks",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton")),
                                                        column(width=12,
                                                               column(4, htmlOutput("range")),
                                                                plotOutput("stocksPlot2", height="700", hover = hoverOpts("stocksPlot2_hover", delay = 20, delayType = "debounce")),
                                                                uiOutput("stocksPlot2_hover_info")))))),

                               
                               tabPanel("Net Carbon Fluxes", value="Net Carbon Fluxes", width=12,
                                        tabsetPanel(
                                          
                                          tabPanel("Carbon Fluxes Over Time",
                                               
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Carbon Fluxes Over Time")),
                                                        column(width=12, align="left", "Use the selection tools at left to plot the rolling 10-year average of four different carbon fluxes in millions of metric tons of carbon (y-axis) plotted over time (x-axis) over your preferred region, scenario, and range of years. In each plot the colored line
                                                               represents the 10 year rolling average carbon flux. The dotted line represents the mean value over all scenarios and the grey area represents the min and max values over all scenarios. 
                                                               NPP is net primary productivity, Rh is heterotrophic respiration (respiration from dead organic matter and soils), NEP is net ecosystem productivity (NPP minus Rh),
                                                               and NECB is net ecosystem carbon balance (NEP minus carbon losses from land use and disturbances). Select different Land Use and Climate Scenarios from the sidebar to explore their effects on carbon fluxes through time."),
                                                        column(width=12, align="right",
                                                               radioGroupButtons(width=250,inputId = "netflux", label = actionButton("aboutCarbonFluxes", " Carbon Fluxes",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton"),choices = c("NPP","Rh","NEP","NECB"),selected="NECB",size="sm",checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=12, align="right",
                                                               plotOutput("fluxplot1", height="700"),
                                                               prettySwitch(inputId = "showFluxTable",label = "View Chart Data", value=FALSE,status="success",fill = TRUE),
                                                               prettySwitch(inputId="annual",label="Add Annual Projections",value=FALSE,status="primary",slim=TRUE),
                                                               prettySwitch(inputId="smooth",label="Add Trend Line",value=FALSE,status="primary",slim=TRUE),
                                                               DTOutput("fluxtable")))),
                                        tabPanel("Cumulative Carbon Fluxes",  
                                          
                                                    fluidRow(
                                                      column(width=12, align="left", h2("Cumulative Carbon Fluxes")),
                                                      column(width=12, align="left", "Use the selection tools at left to plot the mean cumulative net carbon flux in million metric tons carbon (x-axis) according to flux type (y-axis) over your preferred region, scenario, and range of years.
                                                             Negative values indicate a loss of carbon from ecosystems while positive values indicate ecosystems were a net sink of carbon. Rh is heterotrophic respiration (respiration from dead organic matter and soils), NEP is net ecosystem productivity (NPP minus Rh),
                                                               and NECB is net ecosystem carbon balance (NEP minus carbon losses from land use and disturbances). Select different Land Use and Climate Scenarios from the sidebar to explore their effects on cumulative carbon fluxes."),
                                                      column(width=6, align="left"),
                                                      column(width=6, align="right",
                                                             checkboxGroupButtons(inputId="netflux2",label=actionButton("aboutCumulativeCarbonFluxes", " Carbon Fluxes",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton"), choices=c("NPP","Rh","NEP","NECB"),selected=c("NPP","Rh","NEP","NECB"),direction="horizontal",size="sm",width="100%",checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                      column(width=12, align="right",
                                                             plotOutput("fluxplot2", height="700")))),
                                        
                                        tabPanel("Cumulative Carbon Balance",
                                                 
                                                 fluidRow(
                                                   column(width=12, align="left", h2("Cumulative Carbon Balance")),
                                                   column(width=12, align="left", "Use the selection tools at left to plot cumulative net ecosystem carbon balance, or the net amount of carbon that was either gained (sequestered) 
                                                          or lost (emitted) from ecosystems over your preferred region, scenario, and range of years. Values are in millions of metric tons of carbon (y-axis) over time (x-axis). The colored areas show how each ecoregion's cumulative ecosystem carbon balance changes through time. Positive 
                                                          values indicate the region was accumulating more carbon than it emitted back to the atmosphere (net carbon sink) and negative values indicate the 
                                                          region lost more carbon through removal, leaching, and emission to the atmosphere, than it gained (net carbon source). The gray area outline on the graph
                                                          represents the cumulative net ecosystem carbon balance for the entire state of California. Select different Land Use 
                                                          and Climate Scenarios from the sidebar to explore their effects on ecosystem carbon balance. "),
                                                   
                                                   
                                                   column(width=12, align="Right", actionButton("aboutCarbonBalance", "Carbon Balance",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton")),
                                                   column(width=12,
                                                          plotOutput("netfluxPlot3", height="700px"))))
                                        
                                        
                                        )),

                               
                               tabPanel("Land Use Fluxes", value="Land Use Fluxes", width=12,
                                        tabsetPanel(
                                          tabPanel("Carbon Fluxes by Disturbance Type",
                                                   
                                                             fluidRow(
                                                               column(width=12, h2("Carbon Fluxes by Disturbance Type")),
                                                               column(width=12, "Use the selection tools at left to plot carbon fluxes in millions of metric tons of carbon (y-axis) by year(x-axis) resulting from land use change and disturbance over your preferred region, scenario, and range of years. Toggle between land change categories
                                                                        to view the mean annual estimate of mortality, harvest, and emissions. Select different Land Use Scenarios from the siderbar to explore the effects of Land Use and climate model on carbon fluxes."),
                                                               #column(width=12, align="Right", actionButton("aboutCarbonFluxesByDisturbance", "Disturbance and Flux",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton")),
                                                               column(width=12, align="right",
                                                                      radioGroupButtons(width="100%",
                                                                             inputId = "transitionTypes", label = actionButton("aboutCarbonFluxesByDisturbance", "Disturbance and Flux",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton"), 
                                                                             choices = c("Drought","Fire","Forest Clearcut", "Forest Selection","Orchard Removal","Ag Expansion","Urbanization"),
                                                                             selected = "Urbanization",
                                                                             size = "sm",
                                                                             justified = TRUE,
                                                                             direction = "horizontal",
                                                                             checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                              plotOutput("transitionFlows1", height="700")))),
                                          tabPanel("Disturbance Type Contribution by Carbon Flux",
                                                   
                                                             fluidRow(
                                                                      column(width=12, align="left", h2("Disturbance Type Contribution by Flux")),
                                                                      column(width=12, align="left", "Use the selection tools at left to plot average annual total carbon flux in millions of metric tons of carbon (y-axis) by year (x-axis) for each land use/land cover or disturbance related flux type over your preferred region, scenario, and range of years. Each colored area represents the
                                                                             contribution from one LULC or disturbance type. Toggle between carbon fluxes to view cabon flux plots. Select different Land Use and Climate Scenarios from the sidebar to explore their effects on disturbance fluxes"),
                                                                      #column(width=12, align="Right", actionButton("aboutDisturbancebyType", "Disturbance and Flux",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton")),
                                                                      column(width=12, align="right", 
                                                                             radioGroupButtons(width="100%",
                                                                                inputId = "transfluxTypes", 
                                                                                label = actionButton("aboutDisturbancebyType", "Disturbance and Flux",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton"), 
                                                                                choices = c("Emission","Harvest","Mortality"),
                                                                                selected = "Emission",  
                                                                                size = "sm",
                                                                                justified = TRUE,  
                                                                                direction = "horizontal",
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                           plotOutput("transitionFlows2", height="700")))),
                                          tabPanel("Cumulative Fluxes from Land Use Change and Disturbance",
                                                   
                                                             fluidRow(
                                                                      column(width=12, align="left", h2("Cumulative Fluxes from Land Use Change and Disturbance")),
                                                                      column(width=12, align="left", "Use the selection tools at left to plot cumulative carbon fluxes in  millions of metric tons of carbon (x-axis) caused by land use change and disturbance (y-axis) over your preferred region, scenario, and range of years. Compare land use change and disturbance types and the impact of different flux types. Select different land Use and climate scenarios from the sidebar to explore how they influence fluxes from land use change and disturbance."),
                                                                      column(width=12, align="right",
                                                                             checkboxGroupButtons(width="100%",
                                                                                inputId="transitionTypes2", 
                                                                                label=actionButton("aboutCumulativeFluxFromDisturbance", "Disturbance and Flux",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton"),
                                                                                choices = c("Drought","Fire","Forest Clearcut", "Forest Selection","Orchard Removal","Ag Expansion","Urbanization"),
                                                                                selected = c("Drought","Fire","Forest Clearcut", "Forest Selection","Orchard Removal","Ag Expansion","Urbanization"),
                                                                                direction = "horizontal",
                                                                                size="sm",  
                                                                                justified=TRUE,
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                                             checkboxGroupButtons(width="100%",
                                                                                                  inputId="transfluxTypes2", 
                                                                                                  label="", 
                                                                                                  choices = c("Emission","Harvest","Mortality"),
                                                                                                  selected = c("Emission","Harvest","Mortality"),
                                                                                                  direction = "horizontal",
                                                                                                  size="sm",
                                                                                                  justified=TRUE,
                                                                                                  checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                                             plotOutput("transitionFlows3", height="700")))))),
                               
                               
                               
                               
                               tabPanel("Land Cover Disturbance", value="Land Cover Disturbance", width=12,
                                        tabsetPanel(
                                          tabPanel("Annual Disturbance Area by Scenario",
                                            
                                                  fluidRow(
                                                           column(width=12, align="left", h2("Annual Disturbance Area by Scenario")),
                                                           column(width=12, align="left", "Use the selection tools at left to create a time series of disturbed area in square kilometers (y-axis) by year (x-axis) for each selected climate model, climate scenario, and land use scenario. Values shown represent the mean (square kilometers) amount of disturbed area along with the 95% confidence intervals. The gray shaded area in the background shows the maximum range calculated over all scenarios. Use the buttons on the upper right to toggle between wildfire and drought induced tree mortality and add a time series trend line."),
                                                           column(width=12, align="right",
                                                                  radioGroupButtons(width="200px",
                                                                                    inputId = "transitionsDist", 
                                                                                    label = "Disturbance Type", 
                                                                                    choices = unique(disturbanceData$TransitionGroup),
                                                                                    selected="Fire",
                                                                                    size="sm",
                                                                                    justified = TRUE,
                                                                                    checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                           column(width=12, align="right",
                                                                  prettySwitch(
                                                                    inputId="smoothDist",
                                                                    label="Add Trend Line",
                                                                    value=FALSE,
                                                                    status="primary",
                                                                    slim=TRUE)),
                                                           column(width=12, align="right",
                                                                  plotOutput("transitionsDistPlot", height="700")))),
                                          tabPanel("Fire Disturbance by Severity Class",
                                            
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Fire Disturbance by Severity Class")),
                                                        column(width=12, align="left", "Use the selection tools at left to create boxplot diagrams showing wildfire area burned for each selected climate model, climate scenario, land use scenario and timeframe. The boxplots show the distribution of mean annual are burned (km2) (y-axis) by scenario (x-axis) and have been disaggregated by fire severity class (i.e. low, medium, and high). The boxes represent the 25th and 75th percentiles while the whiskers represent the 10th and 90th percentiles; outliers are shown as points. Because the plot only shows the distribution of annual mean projections, the range is likely much larger due to uncertainty associated with fire projections (see the disturbance over time tab)."),
                                                        column(width=9, align="right",
                                                               radioGroupButtons(width="200px",
                                                                                 inputId = "transitionsDist1", 
                                                                                 label = "Disturbance Type", 
                                                                                 choices = unique(disturbanceData$TransitionGroup),
                                                                                 selected="Fire",
                                                                                 size="sm",
                                                                                 justified = TRUE,
                                                                                 checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=3, align="right",
                                                           checkboxGroupButtons(inputId="severityTypes", 
                                                                                label="Fire Severity Class", 
                                                                                choices=c("High", "Medium", "Low"),
                                                                                selected=c("High", "Medium", "Low"),
                                                                                direction="horizontal",
                                                                                size="sm",
                                                                                width="100%",
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=12, 
                                                           column(4, htmlOutput("range2")),
                                                           plotOutput("transitionsFirePlot", height="700")))),
                                          tabPanel("Land Cover Change from Disturbance",
                                            
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Land Cover Change from Disturbance")),
                                                        column(width=12, align="left", "Use the selection tools at left to create box plot diagrams of total land use and land cover change from wildfire and drought occurrence over your preferred region, scenario, and range of years. The boxplot shows the distribution of mean annual disturbance-based change (km2) (y-axis) in the selected land cover class by scenario (x-axis) and has been disaggregated by land cover class. The boxes represent the 25th and 75th percentiles while the whiskers represent the 10th and 90th percentiles; outliers are shown as points. Because the plot only shows the distribution of annual mean projections, the range is likely much larger due to uncertainty associated with fire projections (see the disturbance over time tab)."),
                                                        column(width=9, align="right",
                                                               radioGroupButtons(width="200px",
                                                                                 inputId = "transitionsDist2", 
                                                                                 label = "Disturbance Type", 
                                                                                 choices = unique(disturbanceData$TransitionGroup),
                                                                                 selected="Fire",
                                                                                 size="sm",
                                                                                 justified = TRUE,
                                                                                 checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=3, align="right",
                                                           checkboxGroupButtons(inputId="stateTypes", 
                                                                                label="Land Cover Class", 
                                                                                choices=c("Forest", "Grassland", "Shrubland"),
                                                                                selected=c("Forest", "Grassland", "Shrubland"),
                                                                                direction="horizontal",
                                                                                size="sm",
                                                                                width="100%",
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=12, 
                                                           column(4, htmlOutput("range3")),
                                                           plotOutput("transitionsFirePlot2", height="700")))))),
                               
                               
                               
                               tabPanel("Land Cover Totals", value="Land Cover Totals",
                                  tabsetPanel(
                                    tabPanel("Land Use/Land Cover Composition Over Time",
                                             
                                             fluidRow(
                                               column(width=12, align="left", h2("Land Use/Land Cover Composition Over Time")),
                                               column(width=12, align="left", "Use the selection tools at left to create line graph time series of land use and land cover change over your preferred region, scenario, 
                                                      and range of years. Use the toggle buttons below to add and remove land use/land cover class graphs from the visualization. Values in the line graphs represent the 
                                                      number of square kilometers (y-axis) of the selected land use/land cover in the region over time (x-axis), 
                                                      grey background of each line graph shows the full range of outcomes across all scenarios."),
                                               column(width=12, align="right",
                                                      
                                                      checkboxGroupButtons(width="100%",
                                                                           inputId = "lulcType", label = actionButton("aboutLanduseCompositionTime", "Landuse Composition",icon("glyphicon glyphicon-info-sign", lib = "glyphicon"),class="aboutButton"), 
                                                                           choices = c("Agriculture","Developed","Forest","Grassland","Shrubland","Wetland"),
                                                                           selected = c("Agriculture","Developed","Forest","Grassland","Shrubland","Wetland"),
                                                                           size="sm",
                                                                           justified = TRUE, 
                                                                           checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                      plotOutput("lulcPlot1", height="700")),
                                               
                                               column(width=12, align="left",
                                                      
                                                      tags$p("Note: Land Use/Land Cover composition was modeled independent of climate scenario and climate model. Changing climate scenario and climate model will have no effect.")))),
                                    tabPanel("Land Use/Land Cover Composition",
                                        
                                                  fluidRow(
                                                    column(width=12, align="left", h2("Land Use/Land Cover Composition")),
                                                    column(width=12, align="left", "Use the selection tools at left to create bar graphs depicting land use/land cover composition by your preferred region, land use scenario, and range of years. 
                                                    Values in the bar graphs represent percent of the region's total land area (y-axis) within each land use/land cover class. Bar graph at left is the starting year, bar graph at right is the ending year in user-selected time range."),
                                                    column(width=12, align="left",
                                                          plotOutput("lulcPlot2", height="700"),
                                                          tags$p("Note: Land Use/Land Cover composition was modeled independent of climate scenario and climate model. Changing climate scenario and climate model will have no effect.")
                                                          )))
                                    )))))
                   
                   
                   
                   
                )
            ),
        tabPanel("About",value ="aboutPanel",
                 fluidPage(
                   #About Title
                   tags$div(  
                       tags$section(
                         tags$h1("About the LUCAS California Model"),
                         tags$hr(class="abouthr"),
                         tags$p("The Land Use and Carbon Scenario Simulator - or LUCAS Model - was developed to explore alternative scenarios of land-use and land-cover (LULC) change and its impact on ecosystem carbon balance.
                                The LUCAS model developed for California combines a state-and-transition simulation model (STSM) with a carbon stock-flow model, to estimate changes in carbon storage and flux across terrestrial ecosystems.
                                The LUCAS STSM-SF approach has been described in numerous scientific publications. The LUCAS model uses the SyncroSim modeling framework - a general purpose software application for ecosystem-absed modeling applications.
                                The modeling framework is thoroughly described in publications by", a("Daniel et al, 2016", href="", target="_blank"), "and", a("Daniel et al, 2018", href="", target="_blank")),
                         tags$p("For a complete desctiption of the methods and models used to develop the projections used in these visualizations, see the publication by Sleeter et al., 2019 in", 
                                a("Global Change Biology", href="https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.14677", target="_blank")),
                         tags$p("Land use scenarios used in this study were developed to support California's Fourth Climate Change Assessment and are described in detail in Sleeter et al., 2018 in the journal",
                                a("Earth's Future", href="https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2017EF000560", target="_blank"))
                       ),
                       tags$section(
                         tags$h1("Download Data"),
                         tags$hr(class="abouthr"),
                         tags$p("The LUCAS model was used to project changes in ecosystem carbon balance resulting from land use and land use change, 
                                climate change, and ecosystem disturbances such as wildfire and drought in California. We simulated 32 unique scenarios, consisting of 4 land-use scenarios and 2 radiative forcing scenarios 
                                as simulated by 4 global climate models. For each scenario, we ran 100 Monte Carlo realizations of the model, at a 1-km spatial resolution
                                anually between 2001-2100."),
                         tags$h4("Data available for download"),  
                         tags$ul(
                         tags$li("All", a("tabular simulation results", href="https://www.sciencebase.gov/catalog/item/5d03f536e4b0e3d3115806dc", target="_blank"), "for the 32 scenarios used in this application are availble from the SciencBase repository in zipped csv format."),
                         tags$li("An un-run version of the ", a("California LUCAS Model", href="https://www.sciencebase.gov/catalog/item/5d03f5a3e4b0e3d3115806df", target="_blank"), "developed within the ", 
                                 a("SyncroSim software framework" , href="http://doc.syncrosim.com/index.php?title=Reference_Guide", target="_blank"), "is also available for download.")
                         
                         ),
                         tags$p("Steps for setting up and running the model as well as code used for post-processing tabular scenario results are
                              available on ", a("Github." , href="https://github.com/bsleeter/california-carbon-scenarios", target="_blank"), "More information about the California LUCAS model and other LUCAS modeling efforts
                              may be found in our ",a("team website", href="https://www.usgs.gov/centers/wgsc/science/land-use-and-climate-change-team"))
                       ),
                       tags$section(
                         tags$h1("Partners"),
                         tags$hr(class="abouthr"),  
                         fluidRow(
                           column(width=3, align="left", img(src="logos/apexrms.png", class="center"), class="logoCol"),
                           column(width=3, align="left", img(src="logos/ca_energy_comission.png", class="center"), class="logoCol"),
                           column(width=3, align="left", img(src="logos/salo.svg", class="center"), class="logoCol"),
                           column(width=3, align="left", img(src="logos/tnc.svg",  class="center"), class="logoCol"),   
                            
                         class="partnersRow")  
                         ), 
                       tags$section(
                         tags$h1("Contact"),
                         tags$hr(class="abouthr"), 
                         tags$p("For questions about the California LUCAS Model results please email ",a("Benjamin M. Sleeter",href="mailto:bsleeter@usgs.gov")) 
                         
                       ), 
                   class="container")    
                 )
          )
       
      
       ), 
       #custon javascript function to add select class and update image src on button click
       div(tags$script("$('.dashboard-type').click(function(){
            
            if (!$(this).hasClass('selected')) {
                $('.dashboard-type').each(function( index ) {
                  console.log($(this).children('img').attr('src'));
                  $(this).children('img').attr('src', $(this)[0].name);
                  $(this).removeClass('selected');
                });
               
                srcSplit = $(this).children('img').attr('src').split('.png')[0]
                $(this).children('img').attr('src', srcSplit+'_white.png');
                $(this).addClass('selected')
               
            } else  {
                $('.dashboard-type').each(function( index ) {
                  $(this)[0].children('img').attr('src',$(this)[0].name)
                  $(this)[0].removeClass('selected');
                });
                originalSrc = $(this).name;
                $(this).children('img').attr('src', originalSrc);
                $(this).removeClass('selected')
              
            }
        });")),  
       includeHTML("www/footer.html") 
       
)                 
                           


server = (function(input, output, session) {
  
  #Modal for model variables
  observeEvent(input$aboutModelVariables, {
    showModal(modalDialog(
      title = "Model Variables",
      tags$p("The model vairables panel on the left side of the application
              contains variable choices used to filter results within the main
              graph section of the application. Use the toggle boxes and buttons to
             select combinations among 4 LULC scenarios, 2 radiative forcing
             scenarios and 4 climate models. You may also choose an ecoregion to summarize
             by or summarize by the entire state of California."),
      tags$p("Combinations of variables chosen in the 
      model variables panel will filter all data visualizations within the main
      section of the application. See the descriptions below
      for more information about each variable choice."
      ),
      tags$div(img(src='images/ToolDescription.png', width="700px") 
      ),
     
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })
  #modal for carbon stocks 
  observeEvent(input$aboutCarbonStocks, {
    showModal(modalDialog(
      title = "Carbon Stock Definitions",
      tags$ul(
        tags$li(
          "TEC - Total Ecosystem Carbon, represents the sum of all live and dead ecosystem carbon stocks."
        ),
        tags$li(
          "Soil - Soil organic carbon to 1-m depth below the surface." 
        ),
        tags$li(
          "Live - Total live plant biomass carbon (above- and below-ground)."  
        ),
        tags$li(
          "DOM - Dead Organic Matter on the surface, includes litter, standing deadwood and down deadwood but not soil."  
        )
      ),
    
      easyClose = TRUE,
      footer = modalButton("Dismiss")
      ))
  })
  #modal for carbon stocks net change
  observeEvent(input$aboutNetChangeCarbon, {
    showModal(modalDialog(
      title = "Carbon Stock Definitions",
      tags$ul(
        tags$li(
          "TEC - Total Ecosystem Carbon, represents the sum of all live and dead ecosystem carbon stocks."
        ),
        tags$li(
          "Soil - Soil organic carbon to 1-m depth below the surface." 
        ),
        tags$li(
          "Live - Total live plant biomass carbon (above- and below-ground)."  
        ),
        tags$li(
          "DOM - Dead Organic Matter on the surface, includes litter, standing deadwood and down deadwood but not soil."  
        )
      ),
      
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })
 
  #modal for carbon fluxes
  observeEvent(input$aboutCarbonFluxes, {
    showModal(modalDialog(
      #title = "Carbon Fluxes Over Time",
      tags$h4("Carbon Flux Definitions"),
      tags$ul(
        tags$li(
          "NPP - Net primary prodution"
        ),
        tags$li(
          "Rh - Heterotrophic respiration (respiration from dead organic matter and soils)." 
        ),
        tags$li(
          "NEP - Net ecosystem productivity (NPP minus Rh)."  
        ),
        tags$li(
          "NECB - Net ecosystem carbon balance (NEP minus carbon losses from land use and disturbances)."  
        )
      ),
      tags$h4("Carbon Flux Plot Example"),
      tags$div(img(src='images/cabonFluxOverTime.png', width="700px")),  
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })
  #modal aboutCumulativeCarbonFluxes
  observeEvent(input$aboutCumulativeCarbonFluxes, {
    showModal(modalDialog(
      title = "Carbon Flux Definitions",
      tags$ul(
        tags$li(
          "NPP - Net primary prodution" 
        ),
        tags$li(
          "Rh - Heterotrophic respiration (respiration from dead organic matter and soils)." 
        ),
        tags$li(
          "NEP - Net ecosystem productivity (NPP minus Rh)."  
        ),
        tags$li(
          "NECB - Net ecosystem carbon balance (NEP minus carbon losses from land use and disturbances)."  
        )
      ),
    
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })
  
  #modal aboutCumulativeCarbonBalance
  observeEvent(input$aboutCarbonBalance, {
    showModal(modalDialog(
      title = "Cumulative Carbon Balance Graph",
      tags$p("This plot shows cumulative net ecosystem carbon balance, or the net amount of carbon that was either gained (sequestered) or lost (emitted) from ecosystems over time.
             Positive values indicate the region was accumulating more carbon than it emitted back to the atmosphere (net carbon sink) and negative values indicate the region lost
             more carbon through removal, leaching, and emission to the atmosphere, than it gained (net carbon source). Values are reported in Millions of Metric Tons of Carbon (MMTC)."
      ),
      tags$div(img(src='images/cumulativeBalanaceDescription.png', width="700px")   
      ),
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })
  #modal aboutCarbonFluxesByDisturbance
  observeEvent(input$aboutCarbonFluxesByDisturbance, {
    showModal(modalDialog(
      title = "Disturbance Types and Carbon Fluxes",
      tags$p(" The LUCAS model tracks transitions between state types defined to represent the processes associated with urbanization, agricultural expansion, orchard removal, forest harvest, wildfire, and drought mortality.
              Select a disturbance type to plot fluxes according to disturbance type. Fluxes are reported by emission, harvest, and mortality."),
      tags$h4("Carbon Fluxes"),
      tags$ul(
        tags$li(
          "Emission - Total amount of carbon emitted to the atmosphere due to disturbance." 
        ),
        tags$li(
          "Harvest - Total amount of tree biomass carbon transferrred to the wood products due to disturbance."
        ),
        tags$li(
          "Mortality - Total amount of tree live biomass carbon transferred to standing dead due to disturbance. Carbon flux due to biomass death."
        )
      ),
      
      easyClose = TRUE,
      footer = modalButton("Dismiss")
    ))
  })
  
  #modal aboutDisturbancebyType
  observeEvent(input$aboutDisturbancebyType, {
    showModal(modalDialog(
      title = "Disturbance Types and Carbon Fluxes",
      tags$p("The LUCAS model tracks transitions between state types defined to represent the processes associated with urbanization, agricultural expansion, orchard removal, forest harvest, wildfire, and drought mortality.
             Select a carbon flux to plot the contributions of each disturbance type to the total carbon flux."),
      tags$h4("Carbon Fluxes"),
      tags$ul(
        tags$li(
          "Emission - Total amount of carbon emitted to the atmosphere due to disturbance." 
        ),
        tags$li(
          "Harvest - Total amount of tree biomass carbon transferrred to the wood products due to disturbance."
        ),
        tags$li(
          "Mortality - Total amount of tree live biomass carbon transferred to standing dead due to disturbance. Carbon flux due to biomass death."
        )
      ),
      
      easyClose = TRUE,
      footer = modalButton("Dismiss")
      ))
  })
  
 #modal for aboutCumulativeFluxFromDisturbance
  observeEvent(input$aboutCumulativeFluxFromDisturbance, {
    showModal(modalDialog(
      title = "Disturbance Types and Carbon Fluxes",
      tags$p("The LUCAS model tracks transitions between state types defined to represent the processes associated with urbanization, agricultural expansion, orchard removal, forest harvest, wildfire, and drought mortality.
             Select carbon fluxes and disturbance types to compare carbon flux according to disturbance type."),
      tags$h4("Carbon Fluxes"),
      tags$ul(
        tags$li(
          "Emission - Total amount of carbon emitted to the atmosphere due to disturbance." 
        ),
        tags$li(
          "Harvest - Total amount of tree biomass carbon transferrred to the wood products due to disturbance."
        ),
        tags$li(
          "Mortality - Total amount of tree live biomass carbon transferred to standing dead due to disturbance. Carbon flux due to biomass death."
        )
      ),
      
      easyClose = TRUE,
      footer = modalButton("Dismiss")
      ))
  })
  #Modal aboutLanduseCompositionTime
  observeEvent(input$aboutLanduseCompositionTime, {
    showModal(modalDialog(
      title = "Land Use/Land Cover Composition Over Time",
      tags$p("This plot shows land use/land cover composition over time in square kilometers. Toggle land use/land cover classes to add or remove them from the chart."),
      tags$p("Note: Land Use/Land Cover composition was modeled independent of climate scenario and climate model. Therefore changing climate scenario and climate model will have no effect."),
      tags$div(img(src='images/landuseCompositionDescription.png', width="700px")   
      ),
      
      easyClose = TRUE,
      footer = modalButton("Dismiss")
      ))
  })
 #Onclick event for california ecoregion map
 observeEvent(input$mymap_shape_click, {
  
    eco = input$mymap_shape_click$id
    print(eco)
    updateAwesomeRadio(
      session = session, inputId = "ecoregion",
      selected=eco
    )
  })
  
  factpal <- colorFactor(c("#31a354", "#78c679","#ffffcc","#c2e699","#006837","#feedde","#fdbe85","#fd8d3c","#d94701","#41b6c4","#225ea8","#a1dab4"), ecoList)
 
  
  output$mymap <- renderLeaflet({
    leaflet(ecoregions) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(layerId=~eco_name,color = ~factpal(eco_name), weight = 1, smoothFactor = 0.5,label = ~eco_name,
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })
  
  observeEvent(input$view_map, {
    toggle("mymap")
  })
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset", selected = "Carbon Stocks")
    click("jumpToP10")
   
    
  })
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Net Carbon Fluxes")
    click("jumpToP20")
   
  })
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Cover Totals") 
    click("jumpToP30")
    
  })
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Cover Disturbance")
    click("jumpToP40")
    
  })
  observeEvent(input$jumpToP5, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Use Fluxes")
    click("jumpToP50")
  
  })
  
  
  
  observeEvent(input$jumpToP10, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Carbon Stocks")
  })
  observeEvent(input$jumpToP20, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Net Carbon Fluxes")
  })
  observeEvent(input$jumpToP30, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Cover Totals") 
  })
  observeEvent(input$jumpToP40, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Cover Disturbance")
  })
  observeEvent(input$jumpToP50, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Use Fluxes")
  })
  

  
  
##### Carbon Stocks Page #####
##### Stock Plot 1 #####
   
    selectData1 = reactive({
        stocks %>% filter(Ecosystem=="Yes", LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, StockGroup==input$stockGroup) %>%
                          filter(Timestep>=input$years[1], Timestep<=input$years[2])
    })
    
    stocksRangeData = reactive({
        stocks %>% filter(Ecosystem=="Yes", EcoregionName==input$ecoregion, StockGroup==input$stockGroup) %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% group_by(Timestep, EcoregionName, StockGroup) %>% summarise(Mean=mean(Mean), Min=min(Lower), Max=max(Upper))
    })
    
    output$stocktable = renderDT(server = FALSE, {
        DT::datatable(selectData1(),  
                                 extensions='Buttons', 
                                 options=list(pageLength=5, searching=TRUE, autoWidth=TRUE, ordering=TRUE, dom ='Blfrtip',buttons=c('copy', 'csv', 'excel', "pdf")))
    })
    
    observeEvent(input$showStockTable, {
        toggle("stocktable")
    })

    
    output$stocksPlot1 <-  renderPlot({
        p1 = ggplot() +
            geom_ribbon(data=stocksRangeData(), aes(x=Timestep, y=Mean, ymin=Min, ymax=Max), fill="gray95") +
            geom_line(data=selectData1(), aes(x=Timestep, y=Mean, fill=GCM, color=GCM)) +
            scale_fill_manual(values=gcmPal) +
            scale_color_manual(values=gcmPal) +
            facet_wrap(~RCP, scales="free") +
            theme_light(18) +
            labs(fill="Climate Model",
                 color="Climate Model",
                 y="Million Metric Tons of Carbon (MMT C)", 
                 x="") +
            theme(legend.position = "top",
                  legend.justification = "left",
                  legend.title = element_text(size=14),
                  legend.text = element_text(size=14),
                  legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                  plot.background = element_rect(fill="#ffffff", color=NA),
                  plot.title = element_blank(),
                  plot.subtitle = element_blank(),
                  plot.margin=margin(5,5,5,5),
                  strip.background = element_blank(),
                  strip.text = element_text(color="gray20", size=14),
                  panel.grid.major = element_line(linetype = "dashed", color="gray90", size=0.1),
                  panel.grid.minor = element_line(linetype = "dashed", color="gray90", size=0.1),
                  panel.border = element_blank(),
                  panel.spacing.x = unit(3, "lines"),
                  axis.title = element_text(size = 14),
                  axis.line = element_line(color="gray60", size=0.5))
        
        if(input$ci1)
            p1 = p1 + geom_ribbon(data=selectData1(), aes(x=Timestep, y=Mean, ymin=Lower, ymax=Upper, fill=GCM), alpha=0.3, color=NA)
        p1 
        
    })
    
    output$stocksPlot1_hover_info <- renderUI({
        hover <- input$stocksPlot1_hover
        
        point <- nearPoints(selectData1(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        wellPanel(
            style = style,
            p(HTML(paste0(
                "<b> Timestep: </b>", point$Timestep, "<br/>",
                "<b> Landuse Scenario: </b>", point$LUC, "<br/>",
                "<b> RCP: </b>", point$RCP, "<br/>",
                "<b> Climate Model: </b>", point$GCM, "<br/>",
                "<b> Stock Group: </b>", point$StockGroup, "<br/>",
                "<b> Mean Carbon (MMT): </b>", round(point$Mean,1), "<br/>",
                "<b> Lower Bound (MMT): </b>", round(point$Lower,1), "<br/>",
                "<b> Upper Bound (MMT): </b>", round(point$Upper,1), "<br/>"))))
    })
    
    

##### Stock Plot 2 #####    
    output$range  = renderUI({HTML(paste("Start year: ","<b>",as.character(input$years[1]),"</b>","<br/>", "End year: ", "<b>",as.character(input$years[2]),"</b>"))})
    selectData2 = reactive({
        stocks %>% filter(Ecosystem=="Yes", LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion) %>%
            filter(Timestep==input$years[1] | Timestep==input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,StockGroup) %>% mutate(MeanChange=Mean-lag(Mean), LowerChange=Lower-lag(Lower), UpperChange=Upper-lag(Upper))
    })
    
    output$stocksPlot2 <- renderPlot({
      p2 = ggplot(data=selectData2(), aes(x=RCP, y=MeanChange, fill=StockGroup, color=StockGroup)) +
            geom_bar(stat="identity", color="black", position="dodge") + 
            geom_hline(yintercept = 0, color="gray20", size=0.2) +
            scale_fill_manual(values=stockPal) +
            scale_color_manual(values=stockPal) +
            facet_wrap(~GCM, ncol=4) +
            theme_light(18) +
            labs(fill="Carbon Stock Type",
                 x="Climate Scenario (RCP)", 
                 y="Million Metric Tons Carbon (MMT C)") +
            theme(legend.position = "top",
                legend.justification = "left",
                legend.title = element_text(size=14),
                legend.text = element_text(size=14),
                legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.background = element_rect(fill="#ffffff", color=NA),
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.margin=margin(5,5,5,5),
                strip.background = element_blank(),
                strip.text = element_text(color="gray20", size=14),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.border = element_blank(),
                panel.spacing.x = unit(2, "lines"),
                axis.title = element_text(size = 14),
                axis.line = element_line(color="gray60", size=0.5))
        
        if(input$ci1)
            p2 = p2 + geom_errorbar(aes(ymin=LowerChange, ymax=UpperChange), color="black", width=0.9, position = "dodge") 
        p2
        
    })
    
    output$stocksPlot2_hover_info <- renderUI({
        hover <- input$stocksPlot2_hover
        
        point <- nearPoints(selectData2(), hover, threshold = 20, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.95); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        wellPanel(
            style = style,
            p(HTML(paste0(
                "<b> Timestep: </b>", point$Timestep, "<br/>",
                "<b> Landuse Scenario: </b>", point$LUC, "<br/>",
                "<b> RCP: </b>", point$RCP, "<br/>",
                "<b> Climate Model: </b>", point$GCM, "<br/>",
                "<b> Stock Group: </b>", point$StockGroup, "<br/>",
                "<b> Mean Carbon (MMT): </b>", round(point$Mean,1), "<br/>",
                "<b> Lower Bound (MMT): </b>", round(point$Lower,1), "<br/>",
                "<b> Upper Bound (MMT): </b>", round(point$Upper,1), "<br/>"))))
    })
    
    
    
    

    
    

    

    
    ##### Carbon Flux Page  ##### 
##### Flux Plot 1 #####
    selectData3 = reactive({
        netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux==input$netflux) %>%
        group_by(LUC,GCM,RCP,EcoregionName,Flux) %>%
        mutate(Mean10=rollmean(Mean, 10, fill=NA, align=c("center")), 
               Lower10=rollmean(Lower, 10, fill=NA, align=c("center")), 
               Upper10=rollmean(Upper, 10, fill=NA, align=c("center"))) %>%
        filter(Timestep>=input$years[1], Timestep<=input$years[2])
    })
    
    fluxRange = reactive({
      netFlux %>% filter(EcoregionName==input$ecoregion, Flux==input$netflux) %>% 
        filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% 
        group_by(Timestep, EcoregionName, Flux) %>% summarise(Mean=mean(Mean), Min=min(Lower), Max=max(Upper))
    })
    
    observeEvent(input$showFluxTable, {
      toggle("fluxtable")
    })
    
    selectData3Round = reactive({
      netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux==input$netflux) %>%
        mutate(Mean=round(Mean,2), Lower=round(Lower,2), Upper=round(Upper,2))
    })
    
    output$fluxtable = renderDT(server = FALSE, {
      DT::datatable(selectData3Round(),  
                    extensions='Buttons', 
                    options=list(pageLength=5, searching=TRUE, autoWidth=TRUE, ordering=TRUE, dom ='Blfrtip',buttons=c('copy', 'csv', 'excel', "pdf")))
    })
    
    output$fluxplot1 <- renderPlot({
        p3 = ggplot() +
          geom_ribbon(data=fluxRange(), aes(x=Timestep, mean=Mean, ymin=Min, ymax=Max), fill="gray90") +
          geom_line(data=fluxRange(), aes(x=Timestep, y=Mean), color="gray35", linetype="dotted", size=0.7) +
          geom_line(data=selectData3(), aes(x=Timestep, y=Mean10, color=GCM), size=1) + 
          geom_hline(yintercept=0, color="black", size=0.5) +
          scale_fill_manual(values=gcmPal) +
          scale_color_manual(values=gcmPal) +
          facet_grid(RCP~GCM) +
          theme_minimal(20) +
          labs(fill="Climate Model",
               color="Climate Model",
               x="", 
               y="Million Metric Tons Carbon (MMT C)") +
          theme(legend.position = "top",
                legend.justification = "left",
                legend.title = element_text(size=14),
                legend.text = element_text(size=14),
                legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.background = element_rect(fill="#ffffff", color=NA),
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.margin=margin(5,5,5,5),
                strip.background = element_blank(),
                strip.text = element_text(color="gray20", size=14),
                panel.grid.major = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.border = element_blank(),
                panel.spacing.x = unit(2, "lines"),
                axis.title = element_blank(),
                axis.text = element_text(size = 14),
                axis.line = element_line(color="gray60", size=0.5))
        

        if(input$ci1)
            p3 = p3 + geom_ribbon(data=selectData3(), aes(x=Timestep, y=Mean, ymin=Lower10, ymax=Upper10, fill=GCM), alpha=0.3, color=NA)
        if(input$annual)
            p3 = p3 + geom_line(data=selectData3(), aes(x=Timestep, y=Mean, color=GCM), alpha=0.2, show.legend = FALSE)
        if(input$smooth)
          p3 = p3 + geom_smooth(data=selectData3(), aes(x=Timestep, y=Mean, fill=GCM, color=GCM), show.legend = FALSE, se=FALSE)
        
        p3
        
    })
    
    
##### Flux Plot 2 #####    
    selectData4 = reactive({
        netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux %in% input$netflux2) %>%
        filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% 
        group_by(LUC,GCM,RCP,EcoregionName,Flux) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
    })
    
    output$fluxplot2 <- renderPlot({
        p4 = ggplot(data=selectData4(), aes(x=Flux, y=Mean, fill=Flux)) +
            geom_bar(stat="identity", position="dodge", color="black") + 
            geom_hline(yintercept=0) +
            coord_flip() +
            scale_fill_manual(values=fluxPal) +
            scale_color_manual(values=fluxPal) +
            facet_grid(GCM~RCP) +
            theme_minimal(20) +
          labs(fill="Carbon Flux Type",
               x="",
               y="Million Metric Tons Carbon (MMT C)") +
          theme(legend.position = "top",
                legend.justification = "left",
                legend.title = element_text(size=14),
                legend.text = element_text(size=14),
                legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.background = element_rect(fill="#ffffff", color=NA),
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.margin=margin(5,5,5,5),
                strip.background = element_blank(),
                strip.text = element_text(color="gray20", size=14),
                panel.grid.major.x = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.border = element_blank(),
                panel.spacing.x = unit(2, "lines"),
                axis.title = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                axis.text.x = element_text(size = 14),
                axis.line = element_line(color="gray60", size=0.5))
        
        if(input$ci1)
            p4 = p4 + geom_errorbar(aes(ymin=Lower, ymax=Upper), alpha=0.5, color="black", width=0.5)
        p4
        
    })
    
    
##### Flux Plot 3 ##### 
    selectData20 = reactive({
      netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, Flux %in% input$netflux, EcoregionName==input$ecoregion, Flux=="NECB") %>%
        filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,Flux) %>% mutate(CumSum=cumsum(Mean)) %>%
        mutate(positive = ifelse(CumSum>=0, CumSum,0),
               negative = ifelse(CumSum<0,CumSum, -1e-36))

    })
    
    selectData20b = reactive({
      netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, Flux %in% input$netflux, EcoregionName=="State", Flux=="NECB") %>%
        filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,Flux) %>% mutate(CumSum=cumsum(Mean)) %>%
        mutate(positive = ifelse(CumSum>=0, CumSum,0),
               negative = ifelse(CumSum<0,CumSum, -1e-36))
      
    })
    
    output$netfluxPlot3 = renderPlot({
      p20 = ggplot() +
        geom_area(data=selectData20b(), aes(x=Timestep, y=positive), size=0.1, fill="gray85", color="gray85") +
        geom_area(data=selectData20b(), aes(x=Timestep, y=negative), size=0.1, fill="gray85", color="gray85") +
        geom_area(data=selectData20(), aes(x=Timestep, y=positive), size=0.1, alpha=0.5, fill="red", color="red") +
        geom_area(data=selectData20(), aes(x=Timestep, y=negative), size=0.1, alpha=0.5, fill="red", color="red") +
        geom_hline(yintercept=0) +
        facet_grid(GCM~RCP) +
        theme_light(18) +
        labs(x="",
             y="Million Metric Tons of Carbon (MMT C)") +
        theme(legend.position = "bottom",
              legend.justification = "center",
              legend.title = element_text(size=14),
              legend.text = element_text(size=14),
              legend.background = element_rect(fill="#ffffff", color="#ffffff"),
              plot.background = element_rect(fill="#ffffff", color=NA),
              plot.title = element_blank(),
              plot.subtitle = element_blank(),
              plot.margin=margin(5,5,5,5),
              strip.background = element_blank(),
              strip.text = element_text(color="gray20", size=14),
              panel.grid.major = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.grid.minor = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.border = element_blank(),
              panel.spacing.x = unit(2, "lines"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.line = element_line(color="gray60", size=0.5))
      p20
    })
    

    

    
    ##### Disturbances Page #####    
##### Disturbance Area #####    
    
    selectData5 = reactive({
      disturbanceData %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup==input$transitionsDist) %>%
        filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% 
        group_by(LUC,GCM,RCP,Timestep,EcoregionName,TransitionGroup) %>% 
        summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
    })
    
    distRange = reactive({
      disturbanceData %>% filter(EcoregionName==input$ecoregion, TransitionGroup==input$transitionsDist) %>% 
        group_by(LUC,GCM,RCP,Timestep,EcoregionName,TransitionGroup) %>% 
        summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
        group_by(Timestep,EcoregionName,TransitionGroup) %>% summarise(Mean=mean(Mean), Min=min(Lower), Max=max(Upper))
    })
    
    output$transitionsDistPlot = renderPlot({
     p5 = ggplot() +
       geom_ribbon(data=distRange(), aes(x=Timestep, y=Mean, ymin=Min, ymax=Max), fill="gray95") +
       geom_line(data=selectData5(), aes(x=Timestep, y=Mean, color=GCM, fill=GCM)) +
       facet_grid(GCM~RCP) +
       scale_fill_manual(values=gcmPal) +
       scale_color_manual(values=gcmPal) +
       theme_minimal(20) +
       labs(fill="Climate Model",
            color="Climate Model",
            x="",
            y=expression(square~kilometers~(km^2))) +
       theme(legend.position = "top",
             legend.justification = "left",
             legend.title = element_text(size=14),
             legend.text = element_text(size=14),
             legend.background = element_rect(fill="#ffffff", color="#ffffff"),
             plot.background = element_rect(fill="#ffffff", color="#ffffff"),
             plot.title = element_blank(),
             plot.subtitle = element_blank(),
             plot.margin=margin(5,5,5,5),
             strip.background = element_blank(),
             strip.text = element_text(color="gray20", size=14),
             panel.grid.major = element_line(linetype = "dashed", color="gray90", size=0.1),
             panel.grid.minor = element_blank(),
             #panel.border = element_blank(),
             panel.spacing.x = unit(2, "lines"),
             axis.title = element_text(size = 14),
             axis.text = element_text(size = 14),
             axis.line = element_line(color="gray60", size=0.5))
       
       if(input$ci1)
         p5 = p5 + geom_ribbon(data=selectData5(), aes(x=Timestep, ymin=Lower, ymax=Upper, fill=GCM), alpha=0.5, width=0.5, color=NA)
       if(input$smoothDist)
         p5 = p5 + geom_smooth(data=selectData5(), aes(x=Timestep, y=Mean, ymin=Lower, ymax=Upper, color=GCM), method="loess", se=FALSE, size=1)
       p5

   }) 
    
    
    output$range2  = renderUI({HTML(paste("Start year: ","<b>",as.character(input$years[1]),"</b>","<br/>", "End year: ", "<b>",as.character(input$years[2]),"</b>"))})
    
    selectData6 = reactive({
      disturbanceData %>% 
        filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup %in% input$transitionsDist1, Severity %in% input$severityTypes) %>%  
        filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>%
        group_by(LUC,GCM,RCP,Timestep,EcoregionName,TransitionGroup, Severity) %>% 
        summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
    }) 
    
    output$transitionsFirePlot = renderPlot({
      p6 = ggplot(data=selectData6(), aes(x=RCP, y=Mean, fill=Severity)) +
        geom_boxplot(outlier.alpha=0.9, outlier.size = 2, outlier.shape = 16) +
        facet_wrap(~GCM, ncol=4) +
        scale_fill_manual(values=sevPal) +
        scale_color_manual(values=sevPal) +
        theme_minimal(20) +
        labs(fill="Fire Severity",
             color="Fire Severity",
             x="",
             y=expression(square~kilometers~(km^2~yr))) +
        theme(legend.position = "top",
              legend.justification = "left",
              legend.title = element_text(size=14),
              legend.text = element_text(size=14),
              legend.background = element_rect(fill="#ffffff", color="#ffffff"),
              plot.background = element_rect(fill="#ffffff", color="#ffffff"),
              plot.title = element_blank(),
              plot.subtitle = element_blank(),
              plot.margin=margin(5,5,5,5),
              strip.background = element_blank(),
              strip.text = element_text(color="gray20", size=14),
              panel.grid.major.y = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              #panel.border = element_blank(),
              panel.spacing.x = unit(2, "lines"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 14),
              axis.line = element_line(color="gray60", size=0.5))
      p6
    
    })
    
    output$range3  = renderUI({HTML(paste("Start year: ","<b>",as.character(input$years[1]),"</b>","<br/>", "End year: ", "<b>",as.character(input$years[2]),"</b>"))})
    
    selectData7 = reactive({
      disturbanceData %>% 
        filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup %in% input$transitionsDist2, StateClass %in% input$stateTypes) %>%  
        filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>%
        group_by(LUC,GCM,RCP,Timestep,EcoregionName,TransitionGroup, StateClass) %>% 
        summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
    }) 
    
    output$transitionsFirePlot2 = renderPlot({
      p6 = ggplot(data=selectData7(), aes(x=RCP, y=Mean, fill=StateClass)) +
        geom_boxplot(outlier.alpha=0.9, outlier.size = 2, outlier.shape = 16) +
        facet_wrap(~GCM, ncol=4) +
        scale_fill_manual(values=statePal) +
        scale_color_manual(values=statePal) + 
        theme_minimal(20) +
        labs(fill="State Class",
             color="State Class",
             x="",
             y=expression(square~kilometers~(km^2~yr)))+
        theme(legend.position = "top",
              legend.justification = "left",
              legend.title = element_text(size=14),
              legend.text = element_text(size=14),
              legend.background = element_rect(fill="#ffffff", color="#ffffff"),
              plot.background = element_rect(fill="#ffffff", color="#ffffff"),
              plot.title = element_blank(),
              plot.subtitle = element_blank(),
              plot.margin=margin(5,5,5,5),
              strip.background = element_blank(),
              strip.text = element_text(color="gray20", size=14),
              panel.grid.major.y = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              #panel.border = element_blank(),
              panel.spacing.x = unit(2, "lines"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 14),
              axis.line = element_line(color="gray60", size=0.5))
      p6
      
    })
    
    
    



##### Transition Flows Page #####

  selectData8 = reactive({
    transFlows %>% filter(LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup==input$transitionTypes, Flow!="Deadfall") %>%
       filter(Timestep>=input$years[1], Timestep<=input$years[2]) 
})

      output$transitionFlows1 <- renderPlot({
          p8 = ggplot(data=selectData8(), aes(x=Timestep, y=Mean, fill=Flow, color=Flow)) +
          geom_area(color="gray40", size=0.2) +
          scale_fill_manual(values=flowPal) +
          scale_color_manual(values=flowPal) +
          facet_grid(GCM~RCP) +
          theme_light(18) +
          labs(fill="Carbon Flux Type",
               color="Carbon Flux Type",
               x="", 
               y="Million Metric Tons Carbon (MMT C) per Year") +
          theme(legend.position = "top",
              legend.justification = "center",
              legend.title = element_text(size=14),
              legend.text = element_text(size=14),
              legend.background = element_rect(fill="#ffffff", color="#ffffff"),
              plot.background = element_rect(fill="#ffffff", color="#ffffff"),
              plot.title = element_blank(),
              plot.subtitle = element_blank(),
              plot.margin=margin(5,5,5,5),
              strip.background = element_blank(),
              strip.text = element_text(color="gray20", size=14),
              panel.grid.major.x = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.grid.minor.x = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.grid.major.y = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.grid.minor.y = element_line(linetype = "dashed", color="gray90", size=0.1),
              panel.border = element_blank(),
              panel.spacing.x = unit(2, "lines"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=14),
              axis.line = element_line(color="gray60", size=0.5))
  
        p8
})

      
      selectData9 = reactive({
        transFlows %>% filter(LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion, Flow==input$transfluxTypes) %>%
          filter(Timestep>=input$years[1], Timestep<=input$years[2]) 
      })
      
      output$transitionFlows2 <- renderPlot({
        p9 = ggplot(data=selectData9(), aes(x=Timestep, y=Mean, fill=TransitionGroup, color=TransitionGroup)) +
          geom_area(color="gray40", size=0.2) +
          scale_fill_manual(values=transPal) +
          scale_color_manual(values=transPal) +
          facet_grid(GCM~RCP) +
          theme_light(18) +
          labs(fill="Transition Type",
               color="Transition Type",
               x="", 
               y="Million Metric Tons Carbon (MMT C) per Year") +
          theme(legend.position = "top",
                legend.justification = "left",
                legend.title = element_text(size=14),
                legend.text = element_text(size=14),
                legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.margin=margin(5,5,5,5),
                strip.background = element_blank(),
                strip.text = element_text(color="gray20", size=14),
                panel.grid.major.x = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor.x = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.major.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.border = element_blank(),
                panel.spacing.x = unit(2, "lines"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=14),
                axis.line = element_line(color="gray60", size=0.5))
        
        p9
      })
      
      
      selectData10 = reactive({
        transFlows %>% filter(LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup %in% input$transitionTypes2, Flow %in% input$transfluxTypes2) %>%
          filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% 
          group_by(LUC,GCM,RCP,EcoregionName,TransitionGroup,Flow) %>% summarize(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
      })
      
      output$transitionFlows3 <- renderPlot({
        p10 = ggplot(data=selectData10(), aes(x=TransitionGroup, y=Mean, fill=Flow, color=Flow)) +
          geom_bar(stat="identity") +
          scale_fill_manual(values=flowPal) +
          scale_color_manual(values=flowPal) +
          facet_grid(GCM~RCP) +
          coord_flip() +
          theme_light(18) +
          labs(fill="Carbon Flux Type",
               color="Carbon Flux Type",
               x="", 
               y="Million Metric Tons Carbon (MMT C) per Year") +
          theme(legend.position = "top",
                legend.justification = "left",
                legend.title = element_text(size=14),
                legend.text = element_text(size=14),
                legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.margin=margin(5,5,5,5),
                strip.background = element_blank(),
                strip.text = element_text(color="gray20", size=14),
                panel.grid.major.x = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor.x = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.major.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.border = element_blank(),
                panel.spacing.x = unit(2, "lines"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=14),
                axis.line = element_line(color="gray60", size=0.5))

        p10
      })
      
      
      
      
##### LULC Plots #####
      

      selectData11 = reactive({
        lulc %>% filter(LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion, LULC %in% input$lulcType) %>%
          filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% group_by(LUC,Timestep,LULC) %>% summarise(Mean=mean(Mean), Lower=min(Lower), Upper=max(Upper))
      })
      
      selectData11Range = reactive({
        lulc %>% filter(EcoregionName==input$ecoregion, LULC %in% input$lulcType) %>%
          filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% group_by(EcoregionName,Timestep,LULC) %>% summarise(Mean=mean(Mean), Lower=min(Lower), Upper=max(Upper))
      })
      
      
      output$lulcPlot1 <- renderPlot({
        p11 = ggplot() +
          geom_ribbon(data=selectData11Range(),aes(x=Timestep, ymin=Lower, ymax=Upper), fill="gray95") +
          geom_line(data=selectData11(), aes(x=Timestep, y=Mean, color=LULC)) +
          scale_fill_manual(values=lulcPal) +
          scale_color_manual(values=lulcPal) +
          facet_wrap(~LULC, scales = "free") +
          theme_light(18) +
          guides(fill = guide_legend(nrow = 1)) +
          labs(fill="Land Use/Cover Class",
               color="Land Use/Cover Class",
               y=expression(Square~kilometers~(km^2))) +
          theme(legend.position = "top",
                legend.justification = "center",
                legend.title = element_text(size=14),
                legend.text = element_text(size=14),
                legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.margin=margin(5,5,5,5),
                strip.background = element_blank(),
                strip.text = element_text(color="gray20", size=14),
                panel.grid.major = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.spacing.x = unit(2, "lines"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=14),
                axis.line = element_line(color="gray60", size=0.5))
        if(input$ci1)
          p11 = p11 + geom_ribbon(data=selectData11(), aes(x=Timestep, ymin=Lower, ymax=Upper, fill=LULC), alpha=0.5, width=0.5, color=NA)
        
        p11
      })
      
      
      
      selectData12 = reactive({
        lulc %>% filter(LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion) %>%
          filter(Timestep==input$years[1] | Timestep==input$years[2]) %>% group_by(LUC,Timestep,LULC) %>% summarise(Mean=mean(Mean), Lower=min(Lower), Upper=max(Upper)) %>%
          group_by(LUC,Timestep) %>% mutate(MeanPercent = Mean/sum(Mean)) %>% group_by(LUC,Timestep) %>% arrange(LULC) %>% mutate(lab.ypos = cumsum(MeanPercent) - 0.5*MeanPercent)
      })
      
      output$lulcPlot2 <- renderPlot({
        p12 = ggplot(data=selectData12(), aes(x=factor(Timestep), y=MeanPercent, fill=LULC)) +
          geom_bar(stat="identity", color="gray40", position="dodge") +
          geom_text(aes(y=MeanPercent+0.015, label=paste0(round(MeanPercent*100), "%")), position = position_dodge(width=0.9), size=5) +
          scale_fill_manual(values=lulcPal) +
          theme_light(18) +
          labs(fill="Land Use/Cover Class",
               color="Land Use/Cover Class",
               y="Percent (%)") +
          theme(legend.position = "right",
                legend.justification = "center",
                legend.title = element_text(size=14),
                legend.text = element_text(size=14),
                legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.background = element_rect(fill="#ffffff", color="#ffffff"),
                plot.title = element_blank(),
                plot.subtitle = element_blank(),
                plot.margin=margin(5,5,5,5),
                strip.background = element_blank(),
                strip.text = element_text(color="gray20", size=14),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.grid.minor.y = element_line(linetype = "dashed", color="gray90", size=0.1),
                panel.border = element_blank(),
                panel.spacing.x = unit(2, "lines"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=14),
                axis.line = element_line(color="gray60", size=0.5))
        
        p12
        
      })    
      
      
      
      
})


# Run the application 
shinyApp(ui = ui, server = server)




