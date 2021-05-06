#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Description of Data:
#I found this data on Kaggle which showed Data on different manufactures for 10
# developed countries. The data began being collected in December 2020 and is being
#updated to today. I wanted to see the pattern/trend of vaccination in recent time.
#This data is represented by daily recording from December.

#load libraries
library(shiny)
library(tidyverse)

#load data
country_vaccinations <- read_csv("data/country_vaccinations_by_manufacturer.csv") 

#list of locations
location_options <- country_vaccinations%>%
    distinct(location) %>%
    arrange(location)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Vaccination Progress in 10 countries of the World\n(End of 2020- May 2021)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Navigate through different plots by using the tabs")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Timeline",
                                 selectInput("Location_Highlighter",
                                             "Choose Country to Highlight:",
                                             choices = location_options),
                                 plotOutput("timeline_plot")),
                        tabPanel("Bar Plot",
                                 sliderInput("Countries",
                                             "Number of Countries:",
                                             min = 1,
                                             max = 11,
                                             value = 6),
                                 selectInput("Vaccine Manufacturer",
                                             "Choose a Vaccine:",
                                             c("Pfizer" = 1,
                                               "Moderna" = 2)),
                                 plotOutput("barPlot")),
                        tabPanel("Scatter Plot",
                                 plotOutput("scatter_plot")))
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barPlot <- renderPlot({
        country_vaccinations %>%
            group_by(location)%>%
            summarize(total_vaccinations_total = sum(total_vaccinations))%>%
            top_n(input$Countries)%>%
            ggplot(aes(y = reorder(location, total_vaccinations_total),
                       x= total_vaccinations_total))+
                geom_col()+
            theme_linedraw()+
            labs(x = "Total Number of Vaccinations Since December 2020",
                 y='Countries')
        
    })
    output$timeline_plot <- renderPlot({
        
        all_locations <- country_vaccinations%>%
            group_by(location, date)%>%
            summarize(total_vaccinations_total = sum(total_vaccinations))
        
        highlighted_location <- all_locations %>%
            filter(location == input$Location_Highlighter)
            
            all_locations%>%
                ggplot(aes(x = date,
                       y = total_vaccinations_total,
                       group = location))+
                geom_line(color = "grey")+
                geom_line(data = highlighted_location,
                          color = "red")+
                ylab("Total Number of Vaccinations")+
                xlab("Time Frame (Dec 2020 - May 2021)")
    })
    
    output$scatter_plot <- renderPlot({
        country_vaccinations%>%
            group_by(location, date)%>%
            summarize(total_vaccinations_total = sum(total_vaccinations))%>%
            ggplot(aes(x = date,
                       y = total_vaccinations_total,
                       color = location))+
            geom_point()+
            theme_linedraw()+
            labs(y = "Total Number of Vaccinations",
                 x = "Time Frame (Dec 2020 - May 2021)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
