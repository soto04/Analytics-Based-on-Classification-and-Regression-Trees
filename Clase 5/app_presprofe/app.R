library(shiny)



# Define UI ----
ui <- fluidPage(
    titlePanel("Lazaro Alonso"),
    sidebarLayout(
        sidebarPanel(
            h2("Dictator"),
            p("You can look for my code at "),
            code('https://github.com/lazarusA'),
            br(),
            br(),
            br(),
            br(),
            img(src = "lazarusa.jpeg", height = 200, width = 200),
            br(),
            "Stay safe and  ", 
            span("Healthy", style = "color:blue")
        ),
        mainPanel(
            h1("Current position"),
            p("Guest Scientist at Max Planck Institute for the Physics of complex systems", 
              em("Applying machine learning techniques to "), 
              "complex networks, time series, image recognition, speech, etc."),
            br(),
            p("You can know more about me in my ",
              a("web homepage.", 
                href = "https://lazarusa.github.io/Webpage/index.html")),
            br(),
            h2("Interests"),
            fluidRow(
                column(3, 
                       p("test 1 "),
                       p("testtt 1")
                ),
                column(3, 
                       "test 2", "more tests")
            )
        )
    )
)



# Define server logic ----
server <- function(input, output) {
    
}



# Run the app ----
shinyApp(ui = ui, server = server)