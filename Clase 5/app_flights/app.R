#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(tidyverse)
library(moderndive)
library(colourpicker)
library(patchwork)
library(nycflights13)
library(shinythemes)

flights_data <- flights %>% 
    left_join(airlines)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("NY Flights 2013"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        
                        
                        radioButtons("origen",
                                           h4(strong("Origin")),
                                           inline = "True",
                                           choices = unique(flights_data$origin),
                                           selected = "EWR"),
                        radioButtons("topn",
                                    h4(strong("Airlines (Mean)")),
                                    inline = "True",
                                    choices = list("Top 3" = 3, "Top 5" = 5, 
                                                   "Top 7" = 7),
                                    selected = 3),
                        
                        selectInput("x1",
                                   h4(strong("Variable")),
                                   choices = list("Air Time" = "air_time", "Distance" = "distance"),
                                   selected = "air_time",
                                   multiple = FALSE),
                        
                        checkboxInput("media",
                                      strong("Mean"),
                                      value = TRUE),
                        
                        sliderInput("trans",
                                    h5(em("Transparency")),
                                    min = 0.1,
                                    max = 1,
                                    value = 0.5)),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        plotOutput("distPlot")
                    )
                )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        compare_mean <- flights_data %>% 
            filter(origin == input$origen) %>% 
            group_by(name) %>% 
            summarise(Mean = mean(get(input$x1), na.rm = TRUE)) %>% 
            top_n(as.integer(input$topn))
        
        flights_data2 <- flights_data %>% 
            filter( name %in% compare_mean$name)
        
        g <- ggplot(flights_data2, aes(x =  get(input$x1))) +
            geom_density(aes(fill = factor(name)), alpha = input$trans) +
            labs( title = "Density Plot", subtitle = paste("Origin:",input$origen), 
                  caption = "Source: nycflights13",
                  x = input$x1, fill = "Name of the Airline") + theme_bw()
        
        g
        
        if (input$media == TRUE){
            
            g + geom_vline(data = compare_mean, aes(xintercept = Mean, color = factor(name)),
                           linetype = "dashed", size = 0.8, show.legend = FALSE)
        } else {
            g 
            }
            
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)