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
library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(DT)


# Process Carbon Stock Data
stocksEco = read_csv("data/ecoregion_stocks_by_scenario_timestep_95ci.csv") %>% 
    filter(Ecosystem=="Yes") %>% 
    mutate(Mean=Mean/1000, Lower=Lower/1000, Upper=Upper/1000) %>% 
    select(-EcoregionID)

stocksEcoTEC = stocksEco %>% 
    group_by(LUC,GCM,RCP,Timestep, EcoregionName, Ecosystem) %>% 
    summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")

stocksState = read_csv("data/state_stocks_by_scenario_timestep_95ci.csv") %>% 
    filter(Ecosystem=="Yes") %>% 
    mutate(Mean=Mean/1000, Lower=Lower/1000, Upper=Upper/1000) %>%
    mutate(EcoregionName="State")

stocksStateTEC = stocksState %>% 
    group_by(LUC,GCM,RCP,Timestep, EcoregionName, Ecosystem) %>% 
    summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")

stocks = bind_rows(stocksEco, stocksState, stocksEcoTEC, stocksStateTEC) %>%
    mutate(Mean=round(Mean,3), Lower=round(Lower,3), Upper=round(Upper,3))

# Process Net Flux List
fluxEco = read_csv("data/ecoregion_netflux_by_scenario_timestep_95ci.csv") %>% 
    select(-EcoregionID)

netFluxEco = fluxEco %>% 
    filter(Flux %in% c("NPP","Rh","NEP","NECB"))

fluxState = read_csv("data/state_netflux_by_scenario_timestep_95ci.csv")

netFluxState = fluxState %>% 
    filter(Flux %in% c("NPP","Rh","NEP","NECB")) %>% 
    mutate(EcoregionName="State")

netFlux = bind_rows(netFluxEco, netFluxState)

ecoList = unique(stocks$EcoregionName)
lucList = unique(stocks$LUC)
stockList = unique(stocks$StockGroup)

# Define UI for application that draws a histogram
ui = fluidPage(
       setBackgroundColor(color="#f1f1f1", gradient=c("linear"), direction=c("bottom"), shinydashboard = FALSE),
       useShinyjs(),
       includeHTML("www/header.html"),
       tags$head(includeCSS("www/common.css")),
       navbarPage("California Carbon Scenarios",id="navTabset",
            tabPanel("Home",value = "homePanel",
                  
                     actionButton('jumpToP1', 'Carbon Stocks'),
                     actionButton('jumpToP2', 'Carbon Net Flux'),
                     actionButton('jumpToP3', 'Land Cover State'),
                     actionButton('jumpToP4', 'Land Cover Transition')
            ),
            tabPanel("Dashboard",value ="dashboardPanel",
                fluidPage(
                   sidebarLayout(
                       
                       sidebarPanel(style = "background: #00264c; color: #ffffff",
                           h3("Carbon Stocks"), 
                           width=3,
                           selectInput("ecoregion", label=h4("Region"), choices=unique(stocks$EcoregionName), selected="State"),
                           selectInput("luc", label=h4("Land Use Scenario"), choices=unique(stocks$LUC), selected="BAU"),
                           checkboxGroupInput("rcp", label=h4("Climate Scenario"),choiceValues=unique(stocks$RCP),choiceNames=c("Low Emissions (RCP 4.5)", "High Emissions (RCP 8.5)"), selected="rcp45"),
                           checkboxGroupInput("gcm",label=h4("Climate Model"),choiceValues=unique(stocks$GCM),choiceNames=c("Average (CanESM2)","Warm-Wet (CNRM-CM5)", "Hot-Dry (HadGEM2-ES)", "Complement (MIROC5)"), selected="CanESM2"),
                           sliderInput("years", label=h4("Year Range"), min=2001, max=2100, value=c(2001,2100), sep="", width="100%"),
                           checkboxInput("ci1", "Toggle 95% Confidence Intervals", value=TRUE)),
                       
                       mainPanel(width=9,   
                           
                           tabsetPanel(id="dashboardTabset",
                               tabPanel("Carbon Stocks",value = "Carbon Stocks Projected Carbon Storage in California", width=12, 
                                        
                                   wellPanel(style = "background: #ffffff",
                                       fluidRow(     
                                        column(width=12))),
                                   
                                   wellPanel(style = "background: #ffffff", 
                                       fluidRow(
                                        column(width=12,
                                               radioGroupButtons(width=250,
                                                   inputId = "stockGroup", label = h4("Select Carbon Stock"), 
                                                   choices = unique(stocks$StockGroup),
                                                   selected="TEC",
                                                   size="sm",
                                                   justified = TRUE, 
                                                   checkIcon = list(yes = icon("signal", lib = "glyphicon"))),
                                        
                                               plotOutput("stocksPlot1", height="800", hover = hoverOpts("stocksPlot1_hover", delay = 20, delayType = "debounce")),
                                               uiOutput("stocksPlot1_hover_info"),
                                               
                                               #checkboxInput("showStockTable","View/Download Chart Data", FALSE),
                                               prettySwitch(
                                                   inputId = "showStockTable",
                                                   label = "View Chart Data", 
                                                   value=FALSE,
                                                   status="success",
                                                   fill = TRUE),
                                               
                                               DTOutput("stocktable")))),
                                   
                                   wellPanel(style = "background: #ffffff",
                                       fluidRow(       
                                        column(width=12,
                                               plotOutput("stocksPlot2", height="600", hover = hoverOpts("stocksPlot2_hover", delay = 20, delayType = "debounce")),
                                               uiOutput("stocksPlot2_hover_info"))))),
                               
                               tabPanel("Net Carbon Fluxes", value="Net Carbon Fluxes", icon=icon("calendar"), width=12,
                                        
                                        column(width=12, 
                                               selectInput("netFlux",label=h4("Net Flux"), choices=unique(netFlux$Flux),selected="NECB"),
                                               checkboxInput("ci", "Add 95% Confidence Intervals", value=FALSE),
                                               checkboxInput("annual", "Add Annual Projections", value=FALSE)),
                                        column(width=12,
                                               plotOutput("fluxplot1", height="400"),
                                               plotOutput("fluxplot2", height="400"))),
                               
                               tabPanel("Landcover Totals", value="Landcover Totals"),
                               
                               tabPanel("Landcover Transition", value="Landcover Transition"))))
                   
                   
                   
                   
                  ),
                  includeHTML("www/footer.html")
            )
       )
           
)                 
                           


server = (function(input, output, session) {
  ## starts app in dashboard app with first tab selected
  updateTabsetPanel(session, "navTabset",
                    selected = "dashboardPanel")
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Carbon Stocks Projected Carbon Storage in California")
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
                      selected = "Landcover Totals") 
  })
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "navTabset",
                      selected = "dashboardPanel")
    updateTabsetPanel(session, "dashboardTabset",
                      selected = "Landcover Transition")
  })
# Carbon Stocks Page
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
            geom_ribbon(data=stocksRangeData(), aes(x=Timestep, y=Mean, ymin=Min, ymax=Max), fill="gray95", alpha=0.5) +
            geom_line(data=selectData1(), aes(x=Timestep, y=Mean, fill=GCM, color=GCM)) +
            geom_point(aes(x=Timestep, y=Mean, fill=GCM, color=GCM),
                       size=2, shape=16, data=filter(selectData1(), Timestep %in% seq(2001,2101,1))) +
            scale_fill_brewer(palette = "Dark2") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(~RCP, scales="free") +
            theme_light(18) +
            scale_x_continuous(limits=c(2000,2102), expand = c(0,0)) +
            labs(y="Million Metric Tons of Carbon", x="", 
                 title="Projected Carbon Storage by Scenario",
                 subtitle="Carbon stored in live vegetation and soils\ndeclines while carbon dead vegetation increases sharply") +
            theme(legend.position = "top",
                  legend.justification = "left",
                  legend.title = element_blank(),
                  legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                  
                  plot.background = element_rect(fill="#ffffff", color="#ffffff"),
                  plot.title = element_text(size=32),
                  plot.subtitle = element_text(size=16),
                  plot.margin=margin(5,5,5,5),
                  
                  strip.background = element_blank(),
                  strip.text = element_text(color="gray20"),
                  
                  panel.grid.major = element_line(linetype = "dashed", color="gray90", size=0.1),
                  panel.grid.minor = element_line(linetype = "dashed", color="gray90", size=0.1),
                  panel.border = element_blank(),
                  panel.spacing.x = unit(3, "lines"),
                  
                  axis.title = element_text(size = 14),
                  axis.line = element_line(color="gray60", size=0.5))
                  
        
        if(input$ci1)
            p1 = p1 + geom_ribbon(data=selectData1(), aes(x=Timestep, y=Mean, ymin=Lower, ymax=Upper, fill=GCM), alpha=0.5, color=NA)
       
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
    
    

##### Stock Change Plot #####    
    
    selectData2 = reactive({
        stocks %>% filter(Ecosystem=="Yes", LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion) %>%
            filter(Timestep==input$years[1] | Timestep==input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,StockGroup) %>% mutate(MeanChange=Mean-lag(Mean), LowerChange=Lower-lag(Lower), UpperChange=Upper-lag(Upper))
    })
    
    output$stocksPlot2 <- renderPlot({
        p2 = ggplot(data=selectData2(), aes(x=RCP, y=MeanChange, fill=StockGroup, color=StockGroup)) +
            geom_bar(stat="identity", color="black", position="dodge") + 
            scale_fill_brewer(palette = "Accent") +
            scale_color_brewer(palette = "Accent") +
            facet_wrap(~GCM, ncol=4) +
            theme_light(18) +
            labs(x="RCP Scenario", y="MMT C", title="Net Change in Carbon Stocks by Scenario") +
            theme(legend.position = "top",
                  legend.justification = "left",
                  legend.title = element_blank(),
                  legend.background = element_rect(fill="#ffffff", color="#ffffff"),
                  
                  plot.background = element_rect(fill="#ffffff", color="#ffffff"),
                  plot.title = element_text(size=32),
                  plot.subtitle = element_text(size=16),
                  plot.margin=margin(5,5,5,5),
                  
                  strip.background = element_blank(),
                  strip.text = element_text(color="gray20"),
                  
                  panel.grid.major = element_line(linetype = "dashed", color="gray90", size=0.1),
                  panel.grid.minor = element_line(linetype = "dashed", color="gray90", size=0.1),
                  panel.border = element_blank(),
                  panel.spacing.x = unit(3, "lines"),
                  
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
                "<b> Upper Bound (MMT): </b>", round(point$Upper,1), "<br/>"
                
                
            )))
        )
    })
    
    
    
    
    # Carbon Flux Page    
    selectData3 = reactive({
        netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux==input$netFlux) %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% 
            mutate(Mean10=rollmean(Mean, 10, fill=NA, align=c("center")), Lower10=rollmean(Lower, 10, fill=NA, align=c("center")), Upper10=rollmean(Upper, 10, fill=NA, align=c("center")))
    })
    
    output$fluxplot1 <- renderPlot({
        p = ggplot(data=selectData3(), aes(x=Timestep, y=Mean10, fill=GCM, color=GCM)) +
            geom_line(size=1) + 
            geom_hline(yintercept=0, color="black", size=0.5) +
            scale_fill_brewer() +
            scale_color_brewer() +
            facet_wrap(RCP~LUC) +
            theme_minimal(20) +
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
        netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux=="NECB") %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,Flux) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
    })
    
    output$fluxplot2 <- renderPlot({
        p4 = ggplot(data=selectData4(), aes(x=GCM, y=Mean, fill=GCM, color=GCM)) +
            geom_bar(stat="identity") + 
            geom_hline(yintercept=0) +
            scale_fill_brewer() +
            scale_color_brewer() +
            facet_wrap(RCP~LUC) +
            theme_minimal(20) +
            labs(y="Million Metric Tons of Carbon", title="Net Ecosystem Carbon Balance (NECB) by Scenario", subtitle="Rolling annual 10-year average net carbon fluxes by scenario") +
            theme(legend.position = "top", plot.margin=margin(5,5,5,5))
        
        if(input$ci)
            p4 = p4 + geom_errorbar(aes(ymin=Lower, ymax=Upper), alpha=0.5, color="black", width=0.5)
        p4
        
    })
    
    
    
})


# Run the application 
shinyApp(ui = ui, server = server)




