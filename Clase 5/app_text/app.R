library(shiny)

# Define UI for application that draws a histogram
#La interfaz gráfica
ui <- fluidPage(
    titlePanel("Título de la app"),
    sidebarLayout( 
                   sidebarPanel("sidebar panel"),
                   mainPanel(
                       h1("Primer nivel", align = "center"),
                       h2("Segundo nivel"),
                       h3("Tercer nivel"),
                       p("Este funciona para hacer nuevos párrafos...",
                         strong("strong() funcionarpara hacer negrita la letra"),
                         em("em() crea texto en itálica"),
                         code("@lazarus para escribir texto que parezca código")),
                       p("texto en color", style = "color:blue")
                   )
    )
    
)

# Define server logic required to draw a histogram
#Llamadas a R para actualizar los gráficos y su aplicación
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)