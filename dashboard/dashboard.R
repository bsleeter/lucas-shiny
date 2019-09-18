
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



ui <- dashboardPagePlus(
    
    dashboardHeader(title="California Carbon Scenarios"),
    
    dashboardSidebar(title="Scenario Filters",
                     sidebarMenu(
                         selectInput("ecoregion", label=h4("Region"), choices=unique(stocks$EcoregionName), selected="State"),
                         selectInput("stockGroup",label=h4("Carbon Stock"), choices=unique(stocks$StockGroup),selected="TEC"),
                         selectInput("luc", label=h4("Land Use Scenario"), choices=unique(stocks$LUC), selected="BAU"),
                         checkboxGroupInput("rcp", label=h4("Climate Scenario"),choiceValues=unique(stocks$RCP),choiceNames=c("Low Emissions (RCP 4.5)", "High Emissions (RCP 8.5)"), selected="rcp45"),
                         checkboxGroupInput("gcm",label=h4("Climate Model"),choiceValues=unique(stocks$GCM),choiceNames=c("Average (CanESM2)","Warm-Wet (CNRM-CM5)", "Hot-Dry (HadGEM2-ES)", "Complement (MIROC5)"), selected="CanESM2"),
                         sliderInput("years", label=h4("Year Range"), min=2001, max=2100, value=c(2001,2100), sep="", width="100%"),
                         checkboxInput("ci1", "Toggle 95% Confidence Intervals", value=TRUE)
                     )),
    dashboardBody(
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
                 )),
        
        fluidRow(
            column(width=12, 
                box(plotlyOutput("stocksPlot1"), 
                    width=NULL, title = "Total Ecosystem Carbon Storage by Scenario", background=NULL, solidHeader=TRUE, status="primary", collapsible=TRUE)),
            
            column(width=12, 
                   box(plotlyOutput("stocksPlot2", height="800px"), 
                       width=NULL, title = "Projected Net Change in Carbon Stocks by Scenario", background=NULL, solidHeader=TRUE, status="primary", collapsible=TRUE)))
        

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
        
        if(input$ci1)
            p1 = p1 + geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.5, color=NA)
        
        ggplotly(p1, tooltip=c("Timestep", "Mean","Lower","Upper"), hoveron = "points", hoverinfo = "Timestep+Mean") %>% layout(legend=list(orientation="v"))
        
    })
    
    
    selectData2 = reactive({
        stocks %>% filter(Ecosystem=="Yes", LUC %in% input$luc, GCM %in% input$gcm,  RCP %in% input$rcp, EcoregionName==input$ecoregion) %>%
            filter(Timestep==input$years[1] | Timestep==input$years[2]) %>% group_by(LUC,GCM,RCP,EcoregionName,StockGroup) %>% mutate(MeanChange=Mean-lag(Mean), LowerChange=Lower-lag(Lower), UpperChange=Upper-lag(Upper))
    })
    
    output$stocksPlot2 <- renderPlotly({
        p2 = ggplot(data=selectData2(), aes(x=StockGroup, y=MeanChange, fill=StockGroup, color=StockGroup)) +
            geom_bar(stat="identity") + 
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_grid(GCM~RCP) +
            theme_ipsum(14, grid="Y") +
            labs(x="Year", y="MMT C", caption="Change in Living biomass (Live), dead organic matter (DOM), and soil organic carbon (SOC)") +
            theme(legend.position = "top", plot.margin=margin(30,5,5,5))
        if(input$ci1)
            p2 = p2 + geom_errorbar(aes(ymin=LowerChange, ymax=UpperChange), color="black", width=0.5) 
        
        ggplotly(p2)
        
    })
    
    }

shinyApp(ui, server)