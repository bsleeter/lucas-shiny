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
gcmPal = c("CanESM2"="#F0E442", "CNRM-CM5"="#0072B2", "HadGEM2-ES"="#D55E00", "MIROC5"="#CC79A7")
fluxPal = c("NPP"="#2CA02C", "Rh"="#8C564B", "NEP"="#1F77B4", "NECB"="#E1750E")
sevPal = c("High"="#f03b20", "Medium"="#feb24c", "Low"="#ffeda0")
statePal = c("Forest"="#38814E", "Grassland"="#FDE9AA", "Shrubland"="#DCCA8F")
flowPal = c("Emission"="#a6cee3", "Harvest"="#33a02c", "Mortality"="#b2df8a", "Deadfall"="#1f78b4")
transPal = c("Urbanization"="#e5c494", "Ag Expansion"="#fc8d62", "Orchard Removal"="#8da0cb", "Forest Selection"="#a6d854", "Forest Clearcut"="#66c2a5", "Fire"="#ffd92f", "Drought"="#e78ac3")
lulcPal = c("Agriculture"="#CA9146","Barren"="#D2CDC0","Developed"="#B50000","Forest"="#38814E","Grassland"="#FDE9AA","Shrubland"="#DCCA8F","Water"="#5475A8","Wetland"="#C8E6F8", "SnowIce"="#ffffff")
ecoPal = c("Coast Range"="#31a354", "Cascades"="#78c679", "East Cascades"="#ffffcc", "Klamath Mtns."="#c2e699", "Sierra Nevada"="#006837",
           "Central B&R"="#feedde", "Northern B&R"="#fdbe85", "Mojave B&R"="#fd8d3c", "Sonoran B&R"="#d94701",
           "Central Valley"="#41b6c4", "Oak Woodlands"="#225ea8", "SoCal Mtns."="#a1dab4")


# Define UI for application that draws a histogram
ui = fluidPage(
       useShinyjs(),
       includeHTML("www/header.html"),
       tags$head(includeCSS("www/common.css")),
       navbarPage("California Carbon Scenarios",id="navTabset",

        tabPanel("Home",value = "homePanel",  
           #Banner
           tags$div(
             tags$div(  
               tags$div(
                 column(8, 
                        tags$img(src = "ca_boxes.png", height = "200px", class="toolBoxImg"),
                        tags$h1("California Carbon Scenarios", id="bannerHeader"),
                        tags$p("Land change and carbon balance scenario projections for the State of California with the LUCAS model", id="bannerText"),
                        offset = 2),
                 class="row"),
               class = "container"),
             class="row", id="banner"),
           #Tools Row
           tags$div(
             tags$div( 
               #Tools Title
               tags$div(
                 tags$h1("Explore Projections", id="toolsTitle"),
                 class="row", id="toolsTitleRows"), 
               tags$div(
                 tags$div(
                   tags$h3("Carbon Stocks"),
                   tags$div(
                     tags$img(src = "stocks_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing eli. Aliquam eget sapien sapien. Curabitur in metus urna. In hac habitasse platea dictumst."),
                     actionButton('jumpToP1', 'Explore stocks'), 
                     class ="row"),
                   class="toolBox"),     
                 tags$div(
                   tags$h3("Carbon Net Flux"),
                   tags$div(
                     tags$img(src = "net_flux_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing eli. Aliquam eget sapien sapien. Curabitur in metus urna. In hac habitasse platea dictumst."),
                     actionButton('jumpToP2', 'Explore fluxes'),
                     class ="row"),
                   class="toolBox"),   
                 tags$div(  
                   tags$h3("Land Cover Fluxes"), 
                   tags$div(
                     tags$img(src = "transition_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing eli. Aliquam eget sapien sapien. Curabitur in metus urna. In hac habitasse platea dictumst."),
                     actionButton('jumpToP5', 'Explore fluxes'),
                     class ="row"),
                   class="toolBox"),  
                # tags$div(
                #   tags$h3("Carbon Flows"),
                #   tags$div(
                #     tags$img(src = "fluxes_80.png", height = "80px", class="toolBoxImg"),
                #     tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing eli. Aliquam eget sapien sapien. Curabitur in metus urna. In hac habitasse platea dictumst."),
                #     actionButton('jumpToP', 'Explore flows'),
                #     class ="row"),
                #   class="toolBox"),      
                 tags$div(
                   tags$h3("Land Cover State Class"),
                   tags$div(
                     tags$img(src = "landuse_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing eli. Aliquam eget sapien sapien. Curabitur in metus urna. In hac habitasse platea dictumst."),
                     actionButton('jumpToP3', 'Explore state classes'), 
                     class ="row"),
                   class="toolBox"), 
                 tags$div(
                   tags$h3("Land Cover Disturbance"),
                   tags$div(
                     tags$img(src = "disturbance_80.png", height = "80px", class="toolBoxImg"),
                     tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing eli. Aliquam eget sapien sapien. Curabitur in metus urna. In hac habitasse platea dictumst."),
                     actionButton('jumpToP4', 'Explore disturbance'),
                     class ="row"),
                   class="toolBox"),     
                 id="toolsRow"),    
               class = "container"),      
             class="row", id="tools")    

            ),
            tabPanel("Dashboard",value ="dashboardPanel",
                fluidPage(
                    actionButton(inputId = "jumpToP10", label = "Carbon Stocks"),
                    actionButton(inputId = "jumpToP20", label = "Net Fluxes"),
                    actionButton(inputId = "jumpToP50", label = "Land Use Fluxes"),
                    actionButton(inputId = "jumpToP30", label = "Land Use & Land Cover"),
                    actionButton(inputId = "jumpToP40", label = "Wildfire & Drought"),
                    hr(),
                
                    sidebarLayout(
                       sidebarPanel(width=3, style = "background-color: #00264c; color: #ffffff",
                           awesomeRadio(
                             inputId="ecoregion",
                             label="Region of Interest",
                             choices=sort(unique(stocks$EcoregionName)),
                             selected="State",
                             width="100%",
                             checkbox=TRUE),
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
                                    tabPanel("Carbon stocks over time", 
                                             
                                                       fluidRow(align="right",
                                                         radioGroupButtons(width=250,
                                                                           inputId = "stockGroup", label = "", 
                                                                           choices = c("TEC","Soil","Live","DOM"),
                                                                           selected="TEC",
                                                                           size="sm",
                                                                           justified = TRUE, 
                                                                           checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                       fluidRow(
                                                                column(width=12, align="left", h2("Projected Carbon Storage in California by Scenario")),
                                                                column(width=12, align="left", "This plot shows projected carbon storage over time by scenario. The gray shaded area shows the full
                                                                       range of values projected under the 32 alternative futures. The colored lines and ribbons show the range for each unique scenario.
                                                                       Select one of the four carbon stock groups from the buttons on the upper right."),
                                                                column(width=12, align="right",
                                                                        plotOutput("stocksPlot1", height="600", hover = hoverOpts("stocksPlot1_hover", delay = 20, delayType = "debounce")),
                                                                        uiOutput("stocksPlot1_hover_info"),
                                                                        prettySwitch(
                                                                          inputId = "showStockTable",
                                                                          label = "View Chart Data", 
                                                                          value=FALSE,
                                                                          status="success",
                                                                          fill = TRUE),
                                                                        DTOutput("stocktable")))),
                                    tabPanel("Net change in carbon stocks",
                                            
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Net Change in Carbon Stocks by Scenario")),
                                                        column(width=12, align="left", "This plot shows the net change in carbon storage for each of the stock groups between two dates.
                                                               Negative values indicate a net loss of carbon from ecosystems. RCP scenarios are shown on the x-axis and can be disaggregated 
                                                               for each climate model (GCM). Soil includes soil organic carbon, DOM includes all dead organic matter sotred in litter and 
                                                               standing and dowed dead vegetation, and Live includes all above- and below-ground living vegetation. TEC is total ecosystem 
                                                               carbon and is the sum of the Soil, DOM, and Live pools."),
                                                        column(width=12,
                                                                plotOutput("stocksPlot2", height="700", hover = hoverOpts("stocksPlot2_hover", delay = 20, delayType = "debounce")),
                                                                uiOutput("stocksPlot2_hover_info")))))),

                               
                               tabPanel("Net Carbon Fluxes", value="Net Carbon Fluxes", width=12,
                                        tabsetPanel(
                                          tabPanel("NECB by Ecoregion",
                                            
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Cumulative Net Ecosystem Carbon Balance (NECB) by Ecoregion and Scenario")),
                                                        column(width=12, align="left", "This plot shows the cumulative net ecosystem carbon balance, or the total net amount of carbon which was either stored or sequestered in ecosystems,
                                                               Each colored area shows the cumulative NECB over time. Positive values indicate the region was a net sink of carbon while negative values indicate that the region was a 
                                                               net source of carbon to the atmosphere."),
                                                        column(width=12,
                                                              plotOutput("netfluxPlot3", height="700px")))),
                                          tabPanel("Net flux over time",
                                               
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Net Carbon Flux over Time")),
                                                        column(width=12, align="left", "This plot shows a rolling 10-year average of four different carbon fluxes plotted over time for each scenario.
                                                               NPP is net primary productivity, Rh is heterotrophic respiration (respiration from dead organic matter and soils), NEP is net ecosystem productivity (NPP minus Rh),
                                                               and NECB is net ecosystem carbon balance (NEP minus carbon losses from land use and disturbances)."),
                                                        column(width=12, align="right",
                                                               radioGroupButtons(width=250,inputId = "netflux", label = "",choices = c("NPP","Rh","NEP","NECB"),selected="NECB",size="sm",checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=12, align="right",
                                                               plotOutput("fluxplot1", height="600"),
                                                               prettySwitch(inputId = "showFluxTable",label = "View Chart Data", value=FALSE,status="success",fill = TRUE),
                                                               prettySwitch(inputId="annual",label="Add Annual Projections",value=FALSE,status="primary",slim=TRUE),
                                                               prettySwitch(inputId="smooth",label="Add Trend Line",value=FALSE,status="primary",slim=TRUE),
                                                               DTOutput("fluxtable")))),
                                        tabPanel("Cumulative Net Flux",  
                                          
                                                    fluidRow(
                                                      column(width=12, align="left", h2("Cumulative Net Carbon Flux by Scenario")),
                                                      column(width=12, align="left", "This plot shows the mean cumulative net carbon flux and corresponding 95% confidence intervals for each climate model and scenario.
                                                             Negative values indicate a loss of carbon from ecosystems while positive values indicate ecosystems were a net sink of carbon."),
                                                      column(width=6, align="left"),
                                                      column(width=6, align="right",
                                                             checkboxGroupButtons(inputId="netflux2",label="", choices=c("NPP","Rh","NEP","NECB"),selected=c("NPP","Rh","NEP","NECB"),direction="horizontal",size="sm",width="100%",checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                      column(width=12, align="right",
                                                             plotOutput("fluxplot2", height="600")))))),

                               
                               tabPanel("Land Use Fluxes", value="Land Use Fluxes", width=12,
                                        tabsetPanel(
                                          tabPanel("Fluxes by Land Use Change",
                                                   
                                                             fluidRow(
                                                               column(width=12, h2("Carbon fluxes from land use change and disturbance")),
                                                               column(width=12, "The LUCAS model projected a range of carbon fluxes resulting from land use change and disturbance. Use the sceanrio controls
                                                                        to view the mean annual estimate of mortality, harvest, deadfall (transfer from standing to down deadwood), and emissions
                                                                        resulting from key land change categories. Select different Land Use Scenarios from the siderbar to explore the effects of land change."),
                                                               column(width=12, align="center",
                                                                      radioGroupButtons(width="100%",
                                                                             inputId = "transitionTypes", label = "", 
                                                                             choices = c("Drought","Fire","Forest Clearcut", "Forest Selection","Orchard Removal","Ag Expansion","Urbanization"),
                                                                             selected = "Urbanization",
                                                                             size = "sm",
                                                                             justified = TRUE,
                                                                             direction = "horizontal",
                                                                             checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                              plotOutput("transitionFlows1", height="600")))),
                                          tabPanel("...by Flux Type",
                                                   
                                                             fluidRow(
                                                                      column(width=12, align="left", h2("This is the plot title")),
                                                                      column(width=12, align="left", "This is the plot subtitle which is used to convey additional information about the plot and the controls."),
                                                                      column(width=12, align="center",
                                                                             radioGroupButtons(width="100%",
                                                                                inputId = "transfluxTypes", 
                                                                                label = "Select Carbon Flux Type", 
                                                                                choices = c("Deadfall","Emission","Harvest","Mortality"),
                                                                                selected = "Emission",
                                                                                size = "sm",
                                                                                justified = TRUE,
                                                                                direction = "horizontal",
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                           plotOutput("transitionFlows2", height="600")))),
                                          tabPanel("Cumulative Land Use Fluxes",
                                                   
                                                             fluidRow(
                                                                      column(width=12, align="left", h2("This is the plot title")),
                                                                      column(width=12, align="left", "This is the plot subtitle which is used to convey additional information about the plot and the controls."),
                                                                      column(width=12, align="center",
                                                                             checkboxGroupButtons(width="100%",
                                                                                inputId="transitionTypes2", 
                                                                                label="Select Transition Type", 
                                                                                choices = c("Drought","Fire","Forest Clearcut", "Forest Selection","Orchard Removal","Ag Expansion","Urbanization"),
                                                                                selected = c("Drought","Fire","Forest Clearcut", "Forest Selection","Orchard Removal","Ag Expansion","Urbanization"),
                                                                                direction = "horizontal",
                                                                                size="sm",
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                                             plotOutput("transitionFlows3", height="600")))))),
                               
                               
                               
                               
                               tabPanel("Land Cover Disturbance", value="Land Cover Disturbance", width=12,
                                        tabsetPanel(
                                          tabPanel("Disturbance over time",
                                            
                                                  fluidRow(
                                                           column(width=12, align="left", h2("Annual Disturbance Area by Scenario")),
                                                           column(width=12, align="left", "Plot shows the mean and 95% confidence intervals of disturbed area over time for climate model and scenario. 
                                                           The gray  shaded area shows the maximum range calculated over all scenarios. Use the buttons on the right to toggle between wildfire and drought induced tree mortality."),
                                                           column(width=12, align="right",
                                                                  radioGroupButtons(width="200px",
                                                                                    inputId = "transitionsDist", 
                                                                                    label = "", 
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
                                                                  plotOutput("transitionsDistPlot", height="600"),
                                                                  prettySwitch(
                                                                    inputId = "showTransitionTable",
                                                                    label = "View Chart Data", 
                                                                    value=FALSE,
                                                                    status="success",
                                                                    fill = TRUE),
                                                                  DTOutput("transitiontable")))),
                                          tabPanel("Disturbance by severity class",
                                            
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Disturbance by Severity Class")),
                                                        column(width=12, align="left", "The boxplot shows the distribution of mean annual disturbance area by scenario and has been disaggregated by severity class. 
                                                        The boxes represent the 25th and 75th percentiles while the whiskers represent the 10th and 90th percentiles; outliers are shown as points. Because the plot only shows
                                                               the distribution of annual mean projections, the range is likely much larger due to uncertainty associated with fire projections (see the disturbance over time tab)."),
                                                        column(width=12, align="right",
                                                           checkboxGroupButtons(inputId="severityTypes", 
                                                                                label="Severity Class", 
                                                                                choices=c("High", "Medium", "Low"),
                                                                                selected=c("High", "Medium", "Low"),
                                                                                direction="horizontal",
                                                                                size="sm",
                                                                                width="100%",
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=12, align="center",
                                                           plotOutput("transitionsFirePlot", height="600")))),
                                          tabPanel("Disturbance by Land Cover Class",
                                            
                                                      fluidRow(
                                                        column(width=12, align="left", h2("Disturbance by Land Cover Class")),
                                                        column(width=12, align="left", "The boxplot shows the distribution of mean annual disturbance area by scenario and has been disaggregated by land cover class. 
                                                        The boxes represent the 25th and 75th percentiles while the whiskers represent the 10th and 90th percentiles; outliers are shown as points. Because the plot only shows
                                                               the distribution of annual mean projections, the range is likely much larger due to uncertainty associated with fire projections (see the disturbance over time tab)."),
                                                        column(width=12, align="right",
                                                           checkboxGroupButtons(inputId="stateTypes", 
                                                                                label="Land Cover Class", 
                                                                                choices=c("Forest", "Grassland", "Shrubland"),
                                                                                selected=c("Forest", "Grassland", "Shrubland"),
                                                                                direction="horizontal",
                                                                                size="sm",
                                                                                width="100%",
                                                                                checkIcon = list(yes = icon("signal", lib = "glyphicon")))),
                                                        column(width=12, align="center",
                                                           plotOutput("transitionsFirePlot2", height="600")))))),
                               
                               
                               
                               tabPanel("Land Cover Totals", value="Land Cover Totals",
                                  tabsetPanel(
                                    tabPanel("Land Use/Land Cover Area",
                                        
                                                  fluidRow(
                                                    column(width=12, align="left", h2("Land Use/Land Cover Composition")),
                                                    column(width=12, align="left", "This plot shows the composition of land use & land cover for the selected region and scenario at two points in time.
                                                           Values are expressed as the percent of the regions total area."),
                                                    column(width=12, align="left",
                                                          plotOutput("lulcPlot2", height="700")))),
                                    tabPanel("Land Use/Land Cover over Time",
                                        
                                                  fluidRow(
                                                    column(width=12, align="left", h2("Land Use/Land Cover Composition Over Time")),
                                                    column(width=12, align="left", "This plot shows the composition of land use and land cover over time. The gray shaded area represents the range of values
                                                           projected over all scenarios. "),
                                                    column(width=12, align="right",
                                                           
                                                           checkboxGroupButtons(width="100%",
                                                                             inputId = "lulcType", label = "", 
                                                                             choices = c("Agriculture","Developed","Forest","Grassland","Shrubland","Wetland"),
                                                                             selected = c("Agriculture","Developed","Forest","Grassland","Shrubland","Wetland"),
                                                                             size="sm",
                                                                             justified = TRUE, 
                                                                             checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                                           plotOutput("lulcPlot1", height="700")))))))))
                   
                   
                   
                   
                )
            ),
        tabPanel("About",value ="aboutPanel",
                 fluidPage(
                   "About Panel"
                 )
        ),
        tabPanel("Data",value ="dataPanel",
                 fluidPage(
                   "Data Panel"
                 )
        ),
        tabPanel("Contact",value ="contactPanel",
                 fluidPage(
                   "Contact Panel"
                 )
        )
       ),
       
       includeHTML("www/footer.html") 
       
)                 
                           


server = (function(input, output, session) {
  ## starts app in dashboard app with first tab selected
  #updateTabsetPanel(session, "navTabset",
                    #selected = "dashboardPanel")
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Carbon Stocks")
  })
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Net Carbon Fluxes")
  })
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Cover Totals") 
  })
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Cover Disturbance")
  })
  observeEvent(input$jumpToP5, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Land Use Fluxes")
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
                 y="Million Metric Tons of Carbon", 
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
                 y="Million Metric Tons Carbon") +
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
               y="Million Metric Tons Carbon") +
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
               y="Million Metric Tons Carbon") +
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
                axis.text.x = element_blank(),
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
    
    output$netfluxPlot3 = renderPlot({
      p20 = ggplot() +
        geom_area(data=selectData20(), aes(x=Timestep, y=positive, fill=EcoregionName, color=EcoregionName), size=0.1) +
        geom_area(data=selectData20(), aes(x=Timestep, y=negative, fill=EcoregionName, color=EcoregionName), size=0.1) +
        geom_hline(yintercept=0) +
        facet_grid(GCM~RCP) +
        scale_fill_discrete_qualitative(palette = "Dark 3") +
        scale_color_discrete_qualitative(palette = "Dark 3") +
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
    
    
    
    selectData6 = reactive({
      disturbanceData %>% 
        filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup %in% input$transitionsDist, Severity %in% input$severityTypes) %>%  
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
    
    
    selectData7 = reactive({
      disturbanceData %>% 
        filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup %in% input$transitionsDist, StateClass %in% input$stateTypes) %>%  
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
    transFlows %>% filter(LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup==input$transitionTypes) %>%
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
               y="Million Metric Tons Carbon per Year") +
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
               y="Million Metric Tons Carbon per Year") +
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
        transFlows %>% filter(LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion, TransitionGroup %in% input$transitionTypes2) %>%
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
               y="Million Metric Tons Carbon per Year") +
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




