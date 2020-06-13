library(shiny)

# Define UI for application that draws a histogram
#La interfaz gráfica
ui <- fluidPage(
    titlePanel("Título de la app"),
    sidebarLayout( position = "right",
        sidebarPanel("sidebar panel"),
        mainPanel("main panel")
    )
    
)

# Define server logic required to draw a histogram
#Llamadas a R para actualizar los gráficos y su aplicación
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)