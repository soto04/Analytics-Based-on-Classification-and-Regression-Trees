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
library(RColorBrewer)
library(shinythemes)

cars_data <- mpg

(cars_data2 <- cars_data %>% 
    filter(year == as.integer(1999),
           displ >= as.numeric(1.6) &
               displ <= as.numeric(7),
           manufacturer %in% c(unique(cars_data$manufacturer)))) 


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Application title
                titlePanel("CARS"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("bins",
                                    h4(strong("Number of bins:")),
                                    min = 1,
                                    max = 50,
                                    value = 25),
                        
                        sliderInput("displacement",
                                    h4("Engine displacement, in litres"),
                                    min = min(cars_data$displ),
                                    max = max(cars_data$displ),
                                    value = c(min(cars_data$displ), max(cars_data$displ)),
                                    sep = ""),
                        
                        #box and histogram
                        radioButtons("years",
                                     h4(strong("Year of manufacture")),
                                     inline = "True",
                                     choices = c("1999" = 1999, "2008" = 2008, 
                                                 "All" = "all"),
                                     selected = "all"),
                        
                        checkboxGroupInput("classv",
                                     h4(strong("Class of Vehicle")),
                                     inline = "True",
                                     choices = unique(cars_data$class),
                                     selected = unique(cars_data$class))),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        plotOutput("distPlot")
                    )
                )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        valores <-   RColorBrewer::brewer.pal(length(unique(cars_data$class)), "Accent")
        names(valores) <- levels(cars_data$class)
        
        valores2 <-   RColorBrewer::brewer.pal(length(unique(cars_data$cyl)), "Set1")
        names(valores2) <- levels(factor(cars_data$cyl))
        
        if (input$years == "all"){
            
            cars_data2 <- cars_data %>% 
                filter(displ >= as.numeric(input$displacement[1]) &
                           displ <= as.numeric(input$displacement[2]),
                       class %in% c(input$classv)) 
        } else { 
            
            cars_data2 <- cars_data %>% 
            filter(year == as.integer(input$years),
                   displ >= as.numeric(input$displacement[1]) &
                             displ <= as.numeric(input$displacement[2]),
                   class %in% c(input$classv)) 
        }
        
        g1 <- ggplot(cars_data2, aes(x = displ)) +
            geom_histogram(aes(fill = class), bins = input$bins, col = "black") +
            scale_fill_manual(values = valores)+
            labs(title="Histogram Cars", 
                 subtitle="Engine Displacement across Vehicle Classes") +theme_light()
        
        g2 <- ggplot(cars_data2, aes(class, cty)) +
            geom_boxplot(aes(fill = factor(cyl))) +
            scale_fill_manual(values = valores2) +
            theme (axis.text.x = element_text(angle = 50, vjust = 0.6)) +
            labs(title="Box plot",
                 subtitle="City Mileage grouped by Class of vehicle",
                 caption="Source: mpg",
                 x="Class of Vehicle",
                 y="City Mileage") + theme_bw()
        
        g1 + g2
    
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)