library(shiny)

# Define UI for application that draws a histogram
#La interfaz gráfica
#height y widht de la imagen esta en pixeles
ui <- fluidPage(
    titlePanel("Título de la app"),
    sidebarLayout( 
        sidebarPanel("sidebar panel"),
        mainPanel(
            img(src = "tidyverse.jpg",
                height = 200, width = 150)
        )
    )
    
)

# Define server logic required to draw a histogram
#Llamadas a R para actualizar los gráficos y su aplicación
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)
