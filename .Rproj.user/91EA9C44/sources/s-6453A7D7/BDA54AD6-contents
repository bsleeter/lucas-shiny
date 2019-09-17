#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(extrafont)

stocksEco = read_csv("data/ecoregion_stocks_by_scenario_timestep_95ci.csv") %>% filter(Ecosystem=="Yes")
stocksEcoTEC = stocksEco %>% group_by(LUC,GCM,RCP,Timestep, EcoregionID, EcoregionName, Ecosystem) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")

stocksState = read_csv("data/state_stocks_by_scenario_timestep_95ci.csv") %>% filter(Ecosystem=="Yes") %>%
    mutate(EcoregionID=0, EcoregionName="State")
stocksStateTEC = stocksState %>% group_by(LUC,GCM,RCP,Timestep, EcoregionID, EcoregionName, Ecosystem) %>% summarise(Mean=sum(Mean), Lower=sum(Lower), Upper=sum(Upper)) %>%
    mutate(StockGroup="TEC")

stocks = bind_rows(stocksEco, stocksState, stocksEcoTEC, stocksStateTEC)
ecoList = unique(stocks$EcoregionName)
lucList = unique(stocks$LUC)
stockList = unique(stocks$StockGroup)

# Define UI for application that draws a histogram
ui = fluidPage(theme="common.css",
       includeHTML("www/header.html"),
       navbarPage(title="California Carbon Scenarios", theme = shinytheme("simplex"),
           #navbarMenu(title="Change dataset",
                 tabPanel("Carbon Stocks",value = "Carbon Stocks",
                      # Application title
                      #titlePanel("Carbon Stocks"),
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                          sidebarPanel(
    
                            selectInput("ecoregion", 
                                        label = h4("Region"),
                                        choices = unique(stocks$EcoregionName),
                                        selected = "State"),
                            
                            selectInput("stockGroup",
                                        label = h4("Carbon Stock"),
                                        choices = unique(stocks$StockGroup),
                                        selected="TEC"),
                            
                            selectInput("luc",
                                        label = h4("Land Use Scenario"),
                                        choices = unique(stocks$LUC),
                                        selected="BAU"),
                            
                            checkboxGroupInput("rcp",
                                        label = h4("Climate Scenario"),
                                        choiceValues = unique(stocks$RCP),
                                        choiceNames = c("Low Emissions (RCP 4.5)", "High Emissions (RCP 8.5)"),
                                        selected="rcp45"),
                            
                            checkboxGroupInput("gcm",
                                        label = h4("Climate Model"),
                                        choiceValues = unique(stocks$GCM),
                                        choiceNames = c("Average (CanESM2)","Warm-Wet (CNRM-CM5)", "Hot-Dry (HadGEM2-ES)", "Complement (MIROC5)"),
                                        selected="CanESM2")
                            
                        ),
                        
                        # Plot 1
                        mainPanel(
                            
                            plotOutput("stocksPlot1"),
                            
                            sliderInput("years",
                                        label = h4("Year Range"),
                                        min = 2001, max = 2100, value = c(2001,2100), sep = "", width ="100%"),
                            
                            plotOutput("stocksPlot2")
                        ),
                      )
                 ),
                 tabPanel("Landover Totals", value = "Landcover Totals"
                          #ui for second tab goes here
                         )
             #)
           
        ),

        
       

       includeHTML("www/footer.html")

    )
                                            
                                            


server = (function(input, output) {
    

    selectData1 = reactive({
        stocks %>% filter(Ecosystem=="Yes", 
                          LUC %in% input$luc, 
                          GCM %in% input$gcm, 
                          RCP %in% input$rcp, 
                          EcoregionName==input$ecoregion, 
                          StockGroup==input$stockGroup) %>%
                          filter(Timestep>=input$years[1], Timestep<=input$years[2])
    })
    
    output$stocksPlot1 <- renderPlot({
        ggplot(data=selectData1(), aes(x=Timestep, y=Mean/1000, fill=GCM, color=GCM)) +
            geom_ribbon(aes(ymin=Lower/1000, ymax=Upper/1000), alpha=0.5, color=NA) +
            geom_line() + 
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~LUC) +
            theme_ipsum_rc(14, grid="XY") +
            labs(x="Year", y="Million Metric Tons of Carbon") +
            theme(legend.position = "bottom") 
        
    })
    
    selectData2 = reactive({
        stocks %>% filter(Ecosystem=="Yes", 
                          LUC %in% input$luc, 
                          GCM %in% input$gcm, 
                          RCP %in% input$rcp, 
                          EcoregionName==input$ecoregion, 
                          StockGroup!="TEC") %>%
            filter(Timestep>=input$years[1], Timestep<=input$years[2])
    })
    
    output$stocksPlot2 <- renderPlot({
        ggplot(data=selectData2(), aes(x=Timestep, y=Mean/1000, fill=StockGroup, color=StockGroup)) +
            geom_bar(stat="identity") + 
            scale_fill_ipsum() +
            scale_color_ipsum() +
            facet_wrap(RCP~GCM) +
            theme_ipsum_rc(14, grid="XY") +
            labs(x="Year", y="Million Metric Tons of Carbon") +
            theme(legend.position = "bottom") 
        
    })
})


# Run the application 
shinyApp(ui = ui, server = server)




