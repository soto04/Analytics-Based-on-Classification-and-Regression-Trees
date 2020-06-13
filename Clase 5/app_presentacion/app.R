library(shiny)

# Define UI for application that draws a histogram
#La interfaz gráfica
ui <- fluidPage(
    titlePanel("Claudia Soto"),
    sidebarLayout( 
        sidebarPanel(position = "left",
                     h1(strong("Soto04")),
                     p("You can look for my code at"),
                     code("https://github.com/soto04"),
                     br(),
                     br(),
                     img(src = "imagen.jpg", 
                         height = "40%", width = "40%"),
                     br(),
                     p("Eat", strong("Sleep"),span(strong(em("Be Happy")), style= "color:purple"))),
        mainPanel(
            h1(strong("Current Position"), 
               align = "left"),
            p("Master Data Science at", strong(em("ITESO"))),
            p("Learning:", em("Analytics Based onc Classification and Regression Trees")),
            br(),
            br(),
            h2(strong("Interest")),
            fluidRow(
                column(3, 
                       p("- Music ")),
                column(3, 
                       p("- Bulldogs"))),
            br(),
            br(),
            img(src = "Imagen2.jpg", 
                height = "25%", width = "25%"),
            br(),
            p(strong("Keep Calm", em("and use"),"The Force"), style = "color:darkred")
            
            
        )
    )
    
)

# Define server logic required to draw a histogram
#Llamadas a R para actualizar los gráficos y su aplicación
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)
