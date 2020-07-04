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
library(tidymodels)
library(imager)
library(shinythemes)

kimg_plot <- function(im){
    
    bdf <- as.data.frame(im, wide ="c") 
    #set.seed(123)
    k1 <- kmeans(bdf[,c("c.1", "c.2", "c.3")],centers = 5, nstart = 10, iter.max = 30)
    
    
    par(bg=NA)
    plot(im, axes= FALSE)
    legend("right", legend =c("", "", "", "", "") , fill = rgb(k1$centers), 
           border =rgb(k1$centers), cex = 3,bty = "n", xpd=TRUE)
    }


ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel(strong("Kmeans Images")),
    

    
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "tipo",
                         label = NULL,
                         choices = list("Upload" = 1, "Link" = 2),
                         inline = TRUE),
            
            conditionalPanel(condition = "input.tipo == 1",
                             fileInput(inputId = "imagen_up",
                                       label = "Upload Picture",
                                       accept = c("image/jpg","image/png", "image/jpeg" ))),
            
            conditionalPanel(condition = "input.tipo == 2",
                             textInput(inputId = "imagen_url",
                                       label = "Link Picture"))
       ),

        
        mainPanel(
            titlePanel("5 Dominant Colors"),
        
        plotOutput("k_image"),
        
        
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    options(shiny.maxRequestSize=5*1024^2) #modificar tamaño máximo del archivo
    
    imagen_re <- reactive({
        
        gsub("\\\\","/", input$imagen_up$datapath)
    })

    output$k_image <- renderPlot({
        
        if (input$tipo == 1){
            if (is.null(input$imagen_up))
                return(NULL)
            
            imagen <- load.image(imagen_re())
            
            kimg_plot(imagen)
       
        } else{
            
            if (input$imagen_url == "")
                return(NULL)
            
            imagen <- load.image(as.character(input$imagen_url)) 
            
            kimg_plot(imagen)
            
            }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
