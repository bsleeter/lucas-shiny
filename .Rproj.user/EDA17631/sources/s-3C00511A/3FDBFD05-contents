
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(zoo)
library(plotly)

stocksEco = read_csv("data/ecoregion_stocks_by_scenario_timestep_95ci.csv") %>% filter(Ecosystem=="Yes") %>% mutate(Mean=Mean/1000, Lower=Lower/1000, Upper=Upper/1000)
stocksEcoTEC = stocksEco %>% group_by(LUC,GCM,RCP,Timestep, EcoregionID, EcoregionName, Ecosystem) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")

stocksState = read_csv("data/state_stocks_by_scenario_timestep_95ci.csv") %>% filter(Ecosystem=="Yes") %>% mutate(Mean=Mean/1000, Lower=Lower/1000, Upper=Upper/1000) %>%
    mutate(EcoregionID=0, EcoregionName="State")
stocksStateTEC = stocksState %>% group_by(LUC,GCM,RCP,Timestep, EcoregionID, EcoregionName, Ecosystem) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")

stocks = bind_rows(stocksEco, stocksState, stocksEcoTEC, stocksStateTEC)


# Process Net Flux List
fluxEco = read_csv("data/ecoregion_netflux_by_scenario_timestep_95ci.csv")
netFluxEco = fluxEco %>% filter(Flux %in% c("NPP","Rh","NEP","NECB"))

fluxState = read_csv("data/state_netflux_by_scenario_timestep_95ci.csv")
netFluxState = fluxState %>% filter(Flux %in% c("NPP","Rh","NEP","NECB")) %>% mutate(EcoregionID=0, EcoregionName="State")
netFlux = bind_rows(netFluxEco, netFluxState)

selectData3 = 
    netFlux %>% filter(LUC =="BAU", GCM=="CanESM2", RCP=="rcp45", EcoregionName=="State", Flux=="NPP") %>%
        filter(Timestep>=2001, Timestep<=2100) %>% group_by(LUC,GCM,RCP,EcoregionName,Flux) %>% summarise(Mean=mean(Mean), Lower=mean(Lower), Upper=mean(Upper))


ui <- dashboardPage(
    
    dashboardHeader(title="California Carbon Scenarios"),
    
    dashboardSidebar(
                     sidebarMenu(
                        menuItem("Carbon Stocks", tabName = "stocks", icon = icon("th-large")),
                        menuItem("Carbon Fluxes", tabName = "fluxes", icon = icon("random")),
                        menuItem("Land Use Emissions", icon = icon("fire"), tabName = "emissions", badgeLabel = "new", badgeColor = "green"),
                        menuItem("Land Use Change", icon = icon("arrows-alt-h"), tabName = "luchange", badgeLabel = "new", badgeColor = "green"),
                        menuItem("Land Use", icon = icon("tree"), tabName = "landuse", badgeLabel = "new", badgeColor = "green"),
                        menuItem("CFE", icon = icon("feather-alt"), tabName = "cfe", badgeLabel = "new", badgeColor = "green"),
                        selectInput("ecoregion", label="Region of Interest", choices=sort(unique(stocks$EcoregionName)), selected="State"),
                        selectInput("stockGroup",label="Carbon Stock", choices=unique(stocks$StockGroup),selected="TEC"),
                        selectInput("luc", label="Land Use Scenario", choices=unique(stocks$LUC), selected="BAU"),
                        checkboxGroupInput("rcp", label="Climate Scenario",choiceValues=unique(stocks$RCP),choiceNames=c("Low Emissions (RCP 4.5)", "High Emissions (RCP 8.5)"), selected="rcp45"),
                        checkboxGroupInput("gcm",label="Climate Model",choiceValues=unique(stocks$GCM),choiceNames=c("Average (CanESM2)","Warm-Wet (CNRM-CM5)", "Hot-Dry (HadGEM2-ES)", "Complement (MIROC5)"), selected="CanESM2"),
                        sliderInput("years", label="Year Range", min=2001, max=2100, value=c(2001,2100), sep="", width="100%"),
                        checkboxInput("smooth", "Add trend line", value=FALSE),
                        checkboxInput("ci", "Add 95% Confidence Intervals", value=FALSE),
                        checkboxInput("annual", "Add Annual Projections", value=FALSE))),
    
    
    
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "stocks", h2("Carbon Stocks"),
                    
                    fluidRow(
                        column(width=12,
                            selectInput("stockGroup",label="Carbon Stock", choices=unique(stocks$StockGroup),selected="TEC"))),
                    fluidRow(
                        column(width=12, 
                            box(plotlyOutput("stocksPlot1"), 
                                width=NULL, title = "Total Ecosystem Carbon Storage by Scenario", background=NULL, solidHeader=TRUE, status="primary", collapsible=TRUE)),
                        column(width=12, 
                            box(plotOutput("stocksPlot2", height="800px"), 
                                width=NULL, title = "Projected Net Change in Carbon Stocks by Scenario", background=NULL, solidHeader=TRUE, status="primary", collapsible=TRUE)))),
        
            
            
            
            tabItem(tabName = "fluxes", h2("Carbon Fluxes"),
                    
                    fluidRow(
                        column(width=2,
                            selectInput("netflux",label="Carbon Flux", choices=unique(netFlux$Flux),selected="NECB"))),
                        
                    
                    fluidRow(
                        column(width=12, 
                            box(plotOutput("fluxPlot1"), 
                                width=NULL, title = "Net Carbon Flux", background=NULL, solidHeader=TRUE, status="primary", collapsible=TRUE))),
                    
                  
                    
                    fluidRow(
                        box(width=12,title = "Annual NPP", background=NULL, solidHeader=TRUE, status="primary", collapsible=TRUE,
                            column(plotOutput("anfluxPlot"), width=12))),
                        
                    fluidRow(
                        column(width=12, 
                            box(plotOutput("fluxPlot2"), 
                                width=NULL, title = "Projected NECB", background=NULL, solidHeader=TRUE, status="primary", collapsible=TRUE)))),
            
            
            
            tabItem(tabName = "emissions"),
            tabItem(tabName = "luchange"),
            tabItem(tabName = "landuse"),
            tabItem(tabName = "cfe")),
            
        
        

        
        
        
        fluidRow(width=12,
                 box(width = 12, title = "Stocks Summary", background = NULL, solidHeader = TRUE, status = "primary", collapsible=TRUE,
                     footer = fluidRow(
                         column(width = 3,
                                descriptionBlock(
                                    number = "-9%", number_color = "red", number_icon = "fa fa-caret-down",header = "-483 MMT C", 
                                    text = "Net Carbon lost from ecosystems by 2100",  right_border = F, margin_bottom = FALSE)),
                         column(width = 3,
                                descriptionBlock(
                                    number = "-5%", number_color = "red",number_icon = "fa fa-caret-down", header = "-183 MMT C", 
                                    text = "Net Carbon loss between 2001-2015", right_border = F, margin_bottom = FALSE)),
                         column(width = 3,
                                descriptionBlock(
                                    number = "-5%", number_color = "red", number_icon = "fa fa-caret-down",header = "-350 MMT C", 
                                    text = "Projected decline in live vegetation",  right_border = F, margin_bottom = FALSE)),
                         column(width = 3,
                                descriptionBlock(
                                    number = "18%", number_color = "green",number_icon = "fa fa-caret-up", header = "+120 MMT C", 
                                    text = "Projected increase in Dead vegetation", right_border = F, margin_bottom = FALSE)))
                 ))
        

        )
    
)

server <- function(input, output) { 
    
    selectData1 = reactive({
        stocks %>% filter(Ecosystem=="Yes", LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, StockGroup==input$stockGroup) %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2])
    })
    
    output$stocksPlot1 <-  renderPlotly({
        p1 = ggplot(data=selectData1(), aes(x=Timestep, y=Mean, fill=GCM, color=GCM)) +
            geom_line() +
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~LUC) +
            theme_minimal() +
            labs(x="", y="MMT C", caption="Total carbon stored in Live, Dead, and Soil pools") +
            theme(legend.position = "top") 
        
        if(input$ci)
            p1 = p1 + geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.5, color=NA)
        
        ggplotly(p1, tooltip=c("Timestep", "Mean","Lower","Upper")) %>% layout(legend=list(orientation="v"))
        
        
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
            theme_minimal(18) +
            labs(x="Year", y="MMT C", caption="Change in Living biomass (Live), dead organic matter (DOM), and soil organic carbon (SOC)") +
            theme(legend.position = "top", plot.margin=margin(30,5,5,5))
        
        if(input$ci)
            p2 = p2 + geom_errorbar(aes(ymin=LowerChange, ymax=UpperChange), color="black", width=0.5) 
        
        #ggplotly(p2, tooltip=c("Mean","Lower","Upper"))
        
        p2
        
    })
    
    
    
    # Carbon Flux Page    
    selectData3 = reactive({
        netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux==input$netflux) %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% 
            mutate(Mean10=rollmean(Mean, 10, fill=NA, align=c("center")), Lower10=rollmean(Lower, 10, fill=NA, align=c("center")), Upper10=rollmean(Upper, 10, fill=NA, align=c("center")))
    })
    
    
    output$fluxPlot1 <- renderPlot({
        p3 = ggplot(data=selectData3(), aes(x=Timestep, y=Mean10, fill=GCM, color=GCM)) +
            geom_line(size=1) + 
            geom_hline(yintercept=0, color="black", size=0.5) +
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~LUC) +
            theme_minimal(18) +
            labs(x="Year", y="MMT C", caption="Rolling annual 10-year average net carbon fluxes by scenario") +
            theme(legend.position = "top", plot.margin=margin(5,5,5,5),
                  legend.title = element_blank())
        
        if(input$smooth)
            p3 = p3 + geom_smooth(method="lm", se=FALSE)
        if(input$ci)
            p3 = p3 + geom_ribbon(aes(ymin=Lower10, ymax=Upper10), alpha=0.5, color=NA)
        if(input$annual)
            p3 = p3 + geom_line(data=selectData3(), aes(x=Timestep, y=Mean), alpha=0.2)
        
        p3
        
    })
    
    
    
    anfluxData = reactive({
        netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux==input$netflux) %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2]) 
    })
    
    
    output$anfluxPlot <- renderPlot({
        anflux = ggplot(data=anfluxData(), aes(x=GCM, y=Mean, fill=RCP)) +
            geom_boxplot(outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.7) +
            geom_hline(yintercept=0) +
            scale_fill_ipsum() +
            scale_color_ipsum() +
            theme_minimal(18) +
            coord_flip() +
            ylim(-100,260) +
            labs(y="MMT C", caption="Annual carbon flux rates for selected years") +
            theme(legend.position = "top", plot.margin=margin(5,5,5,5),
                  legend.title = element_blank())
        
        anflux
        
    })
    
    
    

    
    selectData4 = reactive({
        netFlux %>% filter(LUC %in% input$luc, GCM %in% input$gcm, RCP %in% input$rcp, EcoregionName==input$ecoregion, Flux==input$netflux) %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,Flux) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper))
    })
    
    output$fluxPlot2 <- renderPlot({
        p4 = ggplot(data=selectData4(), aes(x=GCM, y=Mean, fill=GCM, color=GCM)) +
            geom_bar(stat="identity") + 
            geom_hline(yintercept=0) +
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~LUC) +
            theme_minimal(18) +
            labs(y="MMT C", caption="Rolling annual 10-year average net carbon fluxes by scenario") +
            theme(legend.position = "top", plot.margin=margin(5,5,5,5), legend.title = element_blank())
        
        if(input$ci)
            p4 = p4 + geom_errorbar(aes(ymin=Lower, ymax=Upper), alpha=0.5, color="black", width=0.5)
        p4
        
    })
    
    
    
    
    
    }

shinyApp(ui, server)