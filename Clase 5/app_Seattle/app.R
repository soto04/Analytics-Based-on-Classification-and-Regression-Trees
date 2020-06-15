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
library(shinythemes)
library(RColorBrewer)
library(shinythemes)

house_prices <- house_prices %>% mutate(
    log10_price = log10(price), log10_size = log10(sqft_living) )



themes <- list("grey" = theme_grey(), "bw" = theme_bw(), "Light" = theme_light(),
               "Minimal" = theme_minimal())

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),
    
    # Application title
    titlePanel("Seattle House Prices"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        h4("Year"),
                        min = min(house_prices$yr_built),
                        max = max(house_prices$yr_built),
                        value = c(2000,2015),
                        sep = ""),
            checkboxGroupInput("conditions",
                               h4("Conditions"),
                               inline = "True",
                               choices = list("1" = 1, "2" = 2, "3" = 3,
                                              "4" = 4, "5" = 5),
                               selected = c(1:5)),
            
            sliderInput("trans",
                        h5(em("Transparency")),
                        min = 0.1,
                        max = 1,
                        value = 0.5), 
            
            selectInput("theme", label = h4("Select theme for plot"), 
                        choices = names(themes), selected = "Light")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # input$year[1]= minyear, input$year[2] = maxyear
        #Fijar los colores de las condiciones
        valores <-   RColorBrewer::brewer.pal(5, "Set1")
        names(valores) <- levels(house_prices$condition)
        
        ggplot(house_prices %>% filter(yr_built  %in%  c(input$year[1]:input$year[2]),
                                           condition %in% c(input$conditions)), 
               aes(x = log10_size, y = log10_price, color = condition)) + 
            geom_point(alpha = input$trans, size = 2.5) +
            scale_color_manual(values = valores) +
            guides(color = guide_legend(override.aes = list(size = 10))) +
            labs(x = "log10 living space (square feet)", y= "log10 price",
                 title = "House size") + themes[[input$theme]]

    })
}

# Run the application 
shinyApp(ui = ui, server = server)