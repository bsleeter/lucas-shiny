#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(extrafont)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(zoo)
library(ggiraph)

loadfonts()
# Process Carbon Stock Data
stocksEco = read_csv("data/ecoregion_stocks_by_scenario_timestep_95ci.csv") %>% filter(Ecosystem=="Yes")
stocksEcoTEC = stocksEco %>% group_by(LUC,GCM,RCP,Timestep, EcoregionID, EcoregionName, Ecosystem) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")
stocksState = read_csv("data/state_stocks_by_scenario_timestep_95ci.csv") %>% filter(Ecosystem=="Yes") %>%
    mutate(EcoregionID=0, EcoregionName="State")
stocksStateTEC = stocksState %>% group_by(LUC,GCM,RCP,Timestep, EcoregionID, EcoregionName, Ecosystem) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")
stocks = bind_rows(stocksEco, stocksState, stocksEcoTEC, stocksStateTEC)
stocks["Mean"]=stocks["Mean"]/1000
stocks["Lower"]=stocks["Lower"]/1000
stocks["Upper"]=stocks["Upper"]/1000

# Process Net Flux List
fluxEco = read_csv("data/ecoregion_netflux_by_scenario_timestep_95ci.csv")
netFluxEco = fluxEco %>% filter(Flux %in% c("NPP","Rh","NEP","NECB"))

fluxState = read_csv("data/state_netflux_by_scenario_timestep_95ci.csv")
netFluxState = fluxState %>% filter(Flux %in% c("NPP","Rh","NEP","NECB")) %>% mutate(EcoregionID=0, EcoregionName="State")
netFlux = bind_rows(netFluxEco, netFluxState)
unique(netFluxState$Flux)


ecoList = unique(stocks$EcoregionName)
lucList = unique(stocks$LUC)
stockList = unique(stocks$StockGroup)

# Define UI for application that draws a histogram
ui = fluidPage(
       includeHTML("www/header.html"),
        
      # navbarPage(title="California Carbon Scenarios", theme = shinytheme("simplex"),
        
      #      tabPanel("Carbon Stocks",value = "Carbon Stocks Projected Carbon Storage in California", icon=icon("bar-chart-o"),

       tags$head(includeCSS("www/common.css")),   
       titlePanel("California Carbon Scenarios"),
        
       navbarPage(title="", theme = shinytheme("simplex"),
           
                 
           tabPanel("Carbon Stocks",value = "Carbon StocksProjected Carbon Storage in California",
                      
                      titlePanel("Carbon Stocks"),
                      

                      sidebarLayout(
                          sidebarPanel(h3("Carbon Stocks"), width=3,
                            selectInput("ecoregion", label=h4("Region"), choices=unique(stocks$EcoregionName), selected="State"),
                            selectInput("stockGroup",label=h4("Carbon Stock"), choices=unique(stocks$StockGroup),selected="TEC"),
                            selectInput("luc", label=h4("Land Use Scenario"), choices=unique(stocks$LUC), selected="BAU"),
                            checkboxGroupInput("rcp", label=h4("Climate Scenario"),choiceValues=unique(stocks$RCP),choiceNames=c("Low Emissions (RCP 4.5)", "High Emissions (RCP 8.5)"), selected="rcp45"),
                            checkboxGroupInput("gcm",label=h4("Climate Model"),choiceValues=unique(stocks$GCM),choiceNames=c("Average (CanESM2)","Warm-Wet (CNRM-CM5)", "Hot-Dry (HadGEM2-ES)", "Complement (MIROC5)"), selected="CanESM2"),
                            sliderInput("years", label=h4("Year Range"), min=2001, max=2100, value=c(2001,2100), sep="", width="100%"),
                            checkboxInput("ci1", "Toggle 95% Confidence Intervals", value=TRUE)
                            ),
                          
                        # Plot 1
                        mainPanel(
                            div(
                                style = "position:relative",

                                plotOutput("stocksPlot1", height="400", hover = hoverOpts("plot_hover", delay = 20, delayType = "debounce")),
                               
                                plotOutput("stocksPlot2", height="400", hover = hoverOpts("plot_hover", delay = 20, delayType = "debounce")),
                                uiOutput("hover_info")
                                
                        ),
                        width=9
                      )
                 )
                 
           ),
            
            tabPanel("Net Carbon Fluxes", value="Net Carbon Fluxes", icon=icon("calendar"),
                    sidebarLayout(
                        sidebarPanel(h3("Carbon Fluxes"), width=3,
                            selectInput("ecoregion2", label=h4("Region"), choices=unique(netFlux$EcoregionName), selected="State"),
                            selectInput("netFlux",label=h4("Net Flux"), choices=unique(netFlux$Flux),selected="NECB"),
                            selectInput("luc2", label=h4("Land Use Scenario"), choices=unique(netFlux$LUC), selected="BAU"),
                            checkboxGroupInput("rcp2", label=h4("Climate Scenario"),choiceValues=unique(netFlux$RCP),choiceNames=c("Low Emissions (RCP 4.5)", "High Emissions (RCP 8.5)"), selected="rcp45"),
                            checkboxGroupInput("gcm2",label=h4("Climate Model"),choiceValues=unique(netFlux$GCM),choiceNames=c("Average (CanESM2)","Warm-Wet (CNRM-CM5)", "Hot-Dry (HadGEM2-ES)", "Complement (MIROC5)"), selected="CanESM2"),
                            sliderInput("years2", label=h4("Year Range"), min=2001, max=2100, value=c(2001,2100), sep="", width="100%"),
                            checkboxInput("smooth", "Add trend line", value=FALSE),
                            checkboxInput("ci", "Add 95% Confidence Intervals", value=FALSE),
                            checkboxInput("annual", "Add Annual Projections", value=FALSE)
                            
                        ),
                        
                        # Plot Area
                        
                            mainPanel(width=9,
                              
                                    plotOutput("fluxplot1", 
                                               height="400"),
                                   
                                    plotOutput("fluxplot2", height="400")
                           
                            )
            
                    )
                          
            ),
           tabPanel("Landcover Totals", value="Landcover Totals",
                    
                    titlePanel("Landcover Totals")
           ),
           tabPanel("Landcover Transition", value="Landcover Transition",
                    
                    titlePanel("Landcover Transition")
           )
             
           
        ),

        
       

       includeHTML("www/footer.html")

    )
                                            
                                     


server = (function(input, output) {
    
# Carbon Stocks Page
    selectData1 = reactive({
        stocks %>% filter(Ecosystem=="Yes", LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, StockGroup==input$stockGroup) %>%
                          filter(Timestep>=input$years[1], Timestep<=input$years[2])
        
        
        
    })
    
    
    output$stocksPlot1 <-  renderPlot({
        
        p1 = ggplot(data=selectData1(), aes(x=Timestep, y=Mean, fill=GCM, color=GCM)) +
            geom_line() +
            geom_point() +
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~LUC) +
            theme_ipsum_rc(14, grid="Y") +
            labs(x="Year", y="Million Metric Tons of Carbon", title="Total Ecosystem Carbon Storage by Scenario", subtitle="Total carbon stored in Live, Dead, and Soil pools") +
            theme(legend.position = "top", plot.margin=margin(5,5,5,5)) 
        
        if(input$ci1)
            p1 = p1 + geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.5, color=NA)
        
        p1 
        
    })
    
   
    output$hover_info <- renderUI({
        hover <- input$plot_hover
        
        point <- nearPoints(selectData2(), hover, threshold = 50, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        # calculate point position INSIDE the image as percent of total dimensions
        # from left (horizontal) and from top (vertical)
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        print(top_px)
        print(left_px)
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0(
                          "<b> Timestep: </b>", point$Timestep, "<br/>",
                          "<b> Mean Carbon (MMT): </b>", point$MeanChange, "<br/>",
                          "<b> Lower Bound (MMT): </b>", point$LowerChange, "<br/>",
                          "<b> Upper Bound (MMT): </b>", point$UpperChange, "<br/>",
                          "<b> RCP: </b>", hover$panelvar2, "<br/>",
                          "<b> Landuse Scenario: </b>", hover$panelvar1, "<br/>"
                         
                          )))
        )
    })
    
    
    
    
    selectData2 = reactive({
        stocks %>% filter(Ecosystem=="Yes", LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion) %>%
            filter(Timestep==input$years[1] | Timestep==input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,StockGroup) %>% mutate(MeanChange=Mean-lag(Mean), LowerChange=Lower-lag(Lower), UpperChange=Upper-lag(Upper))
    })
    
    output$stocksPlot2 <- renderPlot({
        p2 = ggplot(data=selectData2(), aes(x=StockGroup, y=MeanChange, fill=StockGroup, color=StockGroup)) +
            geom_bar(stat="identity") + 
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_grid(GCM~RCP) +
            theme_ipsum_rc(14, grid="Y") +
            labs(x="Year", y="Million Metric Tons of Carbon", title="Average Projected Ecosystem Carbon Storage by Carbon Pool", subtitle="Living biomass (Live), dead organic matter (DOM), and soil organic carbon (SOC) storage") +
            theme(legend.position = "top", plot.margin=margin(30,5,5,5))
        if(input$ci1)
            p2 = p2 + geom_errorbar(aes(ymin=LowerChange, ymax=UpperChange), color="black", width=0.5) 
        p2
        
    })
    
    
    
    
    # Carbon Flux Page    
    selectData3 = reactive({
        netFlux %>% filter(LUC %in% input$luc2, GCM %in% input$gcm2, RCP %in% input$rcp2, EcoregionName==input$ecoregion2, Flux==input$netFlux) %>%
            filter(Timestep>=input$years2[1], Timestep<=input$years2[2]) %>% 
            mutate(Mean10=rollmean(Mean, 10, fill=NA, align=c("center")), Lower10=rollmean(Lower, 10, fill=NA, align=c("center")), Upper10=rollmean(Upper, 10, fill=NA, align=c("center")))
    })
    
    output$fluxplot1 <- renderPlot({
        p = ggplot(data=selectData3(), aes(x=Timestep, y=Mean10, fill=GCM, color=GCM)) +
            geom_line(size=1) + 
            geom_hline(yintercept=0, color="black", size=0.5) +
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~LUC) +
            theme_ipsum_rc(20, grid="Y") +
            labs(x="Year", y="Million Metric Tons of Carbon", title="Net Ecosystem Carbon Flux by Scenario", subtitle="Rolling annual 10-year average net carbon fluxes by scenario") +
            theme(legend.position = "top", plot.margin=margin(5,5,5,5))
        
        if(input$smooth)
            p = p + geom_smooth(method="lm", se=FALSE)
        if(input$ci)
            p = p + geom_ribbon(aes(ymin=Lower10, ymax=Upper10), alpha=0.5, color=NA)
        if(input$annual)
            p = p + geom_line(data=selectData3(), aes(x=Timestep, y=Mean), alpha=0.2)
        
        p
        
    })
    
    
    
    selectData4 = reactive({
        netFlux %>% filter(LUC %in% input$luc2, GCM %in% input$gcm2, RCP %in% input$rcp2, EcoregionName==input$ecoregion2, Flux=="NECB") %>%
            filter(Timestep>=input$years2[1], Timestep<=input$years2[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,Flux) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
    })
    
    output$fluxplot2 <- renderPlot({
        p4 = ggplot(data=selectData4(), aes(x=GCM, y=Mean, fill=GCM, color=GCM)) +
            geom_bar(stat="identity") + 
            geom_hline(yintercept=0) +
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~LUC) +
            theme_ipsum_rc(20, grid="Y") +
            labs(y="Million Metric Tons of Carbon", title="Net Ecosystem Carbon Balance (NECB) by Scenario", subtitle="Rolling annual 10-year average net carbon fluxes by scenario") +
            theme(legend.position = "top", plot.margin=margin(5,5,5,5))
        
        if(input$ci)
            p4 = p4 + geom_errorbar(aes(ymin=Lower, ymax=Upper), alpha=0.5, color="black", width=0.5)
        p4
        
    })
    
    
    
})


# Run the application 
shinyApp(ui = ui, server = server)




