
library(shiny)
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(patchwork)
library(rpart)
library(rpart.plot)
library(shinythemes)
library(DT)
library(randomForest)
library(caret)



texto1 <- p("Using a dataset comprised of songs of two music genres", strong(em("(Hip-Hop and Rock)")), "we will train a classifier to distinguish between the two genres based only on track information derived from Echonest (now part of Spotify).",
br(), br(),
"Streaming services have looked into means of categorizing music to allow for personalized recommendations. One method involves direct analysis of the raw audio information in a given song, scoring the raw data on a variety of metrics.",
br(), br(),
"Let's load the metadata about our tracks alongside the track metrics compiled by The Echo Nest. A song is about more than its title, artist, and number of listens. We have another dataset that has musical features of each track such as danceability and acousticness on a scale from -1 to 1.",
br(), br(),
"For more information: ", a(href =" https://www.kaggle.com/ajaymanwani/song-genre-classification-from-audio-data-using-ml",
                            "Song Genre Classification from Audio Data using ML"),".")

texto2 <- p("Only those related to the audio features were used:",
br(), br(),
strong("- Acousticness:", style = "color:mediumseagreen"), "This value describes how acoustic a song is. A score of 1.0 means the song is most likely to be an acoustic one.",
br(),br(),
strong("- Danceability:", style = "color:mediumseagreen"), "“Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable”.",
br(),br(),
strong("- Energy:", style = "color:mediumseagreen")," represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy”.",
br(),br(),
strong("- Instrumentalness:", style = "color:mediumseagreen"), "This value represents the amount of vocals in the song. The closer it is to 1.0, the more instrumental the song is.",
br(),br())

texto3 <- p(strong("- Liveness:", style = "color:mediumseagreen"), "This value describes the probability that the song was recorded with a live audience. According to the official documentation “a value above 0.8 provides strong likelihood that the track is live”.",
br(), br(),
strong("- Speechiness:", style = "color:mediumseagreen"), "“Speechiness detects the presence of spoken words in a track”. If the speechiness of a song is above 0.66, it is probably made of spoken words, a score between 0.33 and 0.66 is a song that may contain both music and words, and a score below 0.33 means the song does not have any speech.",
br(),br(),
strong("- Tempo:", style = "color:mediumseagreen"), "The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.",
br(),br(),
strong("- Valence:", style = "color:mediumseagreen"), "A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).",
br(),br())


set.seed(1)

data <- read.csv("fma-rock-vs-hiphop.csv")
data_echo <- jsonlite::fromJSON("echonest-metrics.json")
echo_track <- unnest(as_tibble(data_echo,validate = FALSE), col = ls(data_echo)) 
echo_track <- echo_track %>% 
    inner_join(data %>% select("track_id", "genre_top"), by = "track_id")

echo_feat <- ls(echo_track %>% select(-track_id, -genre_top))

color_feat <-   RColorBrewer::brewer.pal(length(echo_feat), "Set2")
names(color_feat) <- levels(factor(echo_feat))

colores <-   c("mediumvioletred","midnightblue")
names(colores) <- levels(factor(echo_track$genre_top))

paleta <- c(RColorBrewer::brewer.pal(n= 9, "Blues")[3:8], 
            RColorBrewer::brewer.pal(n= 9, "Purples")[3:8])

corr_all <- tibble::rownames_to_column(as.data.frame(round(cor(echo_track %>% keep(is.numeric) %>% select(-track_id)), digits=4)), "Features")
corr_hip <- tibble::rownames_to_column(as.data.frame(round(cor(echo_track %>% filter(genre_top == "Hip-Hop") %>% keep(is.numeric) %>% select(-track_id)), digits=4)), "Features")
corr_rock <- tibble::rownames_to_column(as.data.frame(round(cor(echo_track %>% filter(genre_top == "Rock") %>% keep(is.numeric) %>% select(-track_id)), digits=4)), "Features")

data_split <- initial_split(echo_track %>% select(-track_id), prop = 0.75, strata = "genre_top")
echo_train <- training(data_split)
echo_test <- testing(data_split)



hist_echo <- function(filtro1, filtro2){
    echo_track %>% 
        filter(genre_top == filtro1) %>% 
        select(-track_id) %>%
        select(all_of(filtro2)) %>% 
        gather()
} 


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("darkly"),
    titlePanel("Song Genre Classification"),
    
    mainPanel(img(src="music.jpg", height = 100, width = 200),
              br(),
              br(),
            tabsetPanel(
                    tabPanel("Goal",
                        mainPanel(
                         br(),
                         br(),
                         p(texto1)
                         )),#tab goal

                    tabPanel("Data Exploration",
                             tabsetPanel(
                                 tabPanel("Data",
                                      h4(strong("Table")),
                                      tableOutput("table_data"),
                                      br(),
                                      plotOutput("plot_data", height = 300, width = 400),
                                      h4(strong("Description")),
                                      p(texto2),
                                      fluidRow(column(4),img(src = "rock.jpg", height = 200, widht = 150)),
                                      p(texto3)
                                      ),
                                 
                                 tabPanel("Histogram",
                                          sidebarLayout(
                                              sidebarPanel(
                                                  
                                                  radioButtons("compare",
                                                               h4(strong("Compare")),
                                                               inline = "True",
                                                               choices = c("Yes", "No"),
                                                               selected = "No"),
                                                  
                                                  checkboxGroupInput("feat",
                                                                     h4("Audio Features"),
                                                                     inline = "True",
                                                                     choices = echo_feat,
                                                                     selected = echo_feat),
                                                  sliderInput("bins",
                                                              h5("Number of bins:"),
                                                              min = 1,
                                                              max = 100,
                                                              value = 50)),
                                          mainPanel(plotOutput("echo_histogram1"),
                                                    br(),
                                                    br(),
                                                    conditionalPanel(condition = "input.compare == 'Yes'",
                                                    plotOutput("echo_histogram2"))
                                                  ))),#tabpanel histogram data exploration
                                 
                                 tabPanel("Box",
                                          sidebarLayout(
                                              sidebarPanel(
                                                  
                                                  checkboxGroupInput("feat_box",
                                                                     h4("Audio Features"),
                                                                     inline = "True",
                                                                     choices = echo_feat,
                                                                     selected = echo_feat)),
                                              mainPanel(plotOutput("echo_box"),
                                              ))),#tabpanel box data exploration

                                 tabPanel("Correlation Matrix",
                                          sidebarLayout(
                                              sidebarPanel(
                                                  radioButtons("genre_corr",
                                                               h4("Genre"),
                                                               inline = "True",
                                                               choices = c("All", "Rock","Hip-Hop"),
                                                               selected = "All"),
                                                  
                                                  radioButtons("feat_corrx",
                                                               h4("X"),
                                                               inline = "True",
                                                               choices = echo_feat,
                                                               selected = "valence"),
                                                  
                                                  radioButtons("feat_corry",
                                                               h4("y"),
                                                               inline = "True",
                                                               choices = echo_feat,
                                                               selected = "danceability"),
                                                  
                                                  sliderInput("trans",
                                                              h5(em("Transparency")),
                                                              min = 0.1,
                                                              max = 1,
                                                              value = 0.15)),
                                              
                                              mainPanel(
                                                        tableOutput("table_corr"),
                                                        br(),
                                                        br(),
                                                        p(),
                                                  plotOutput("plot_corr")
                                              ))))),#tabpanel data exploration
                    
                    tabPanel("Trees",
                             tabsetPanel(
                                 tabPanel("Decision Tree",
                                          sidebarLayout(
                                              sidebarPanel(
                                                  numericInput("tree_cp",
                                                               h5("CP Value"),
                                                               value = 0.01,
                                                               min = 0,
                                                               max = 1),
                                                  
                                                  radioButtons("split_tree",
                                                               h4("split"),
                                                               inline = "True",
                                                               choices = c("gini","information"))
                                                  
                                              ),#sidebarpanel dec
                                          
                                          
                                          mainPanel(
                                              h2("Decision Tree Diagram"),
                                              br(),
                                              br(),
                                              plotOutput("plot_decision"),
                                              br(),
                                              br(),
                                              tableOutput("table_cp"),
                                              br(),
                                              plotOutput("plot_cp")))),
                                 
                                 tabPanel("Prediction",
                                          sidebarLayout(
                                              sidebarPanel(
                                                  sliderInput("tree_n",
                                                               h5("Number of trees to grow"),
                                                               min = 10,
                                                               max = 500,
                                                              value = 50),
                                                  
                                                  sliderInput("tree_mtry",
                                                               h4("Number of variables randomly sampled as candidates at each split."),
                                                              min = 1,
                                                              max = 50,
                                                              value = 5)),
                                              mainPanel(
                                                plotOutput("import_feat"),
                                                br(),
                                                br(),
                                                htmlOutput("table_scores")
                                               # tableOutput("prueba")
                                                
                                                  
                                                  
                                              )#mainPanel
                                                  
                                              )#sidebarpanel dec
                                          
                                          
                                          ) #Tab panel prediccion
                                 
                                 
                             )#tabsetpanel trees
                             
                             )# tabpanel trees

)))#fluidpage

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    tree.music <- reactive(rpart(genre_top ~ ., data = echo_track %>% select(-track_id), 
                                 cp = input$tree_cp, parms = list(split = input$split_tree)))
    rf_echo <- reactive(randomForest(factor(genre_top) ~ ., 
                                                data = echo_train, importance=TRUE, ntree = input$tree_n, mtry = input$tree_mtry))

### Table   
     output$table_data <- renderTable({
         
         head(echo_track, 7)
         
     }, bordered = TRUE)

     
     output$table_corr <- renderTable({
         
         if(input$genre_corr == "All"){
             corr_all
             
         } else if(input$genre_corr == "Rock"){
             corr_rock
             
         } else{
             corr_hip
         }
     }, bordered = TRUE)
     
### Graphs
     output$plot_data <- renderPlot({
         
         ggplot(data = echo_track, aes(x = genre_top)) +
             geom_bar(aes(fill = genre_top)) +
             labs(title = "Genre of Music", x = "", fill = "") + 
             scale_fill_manual(values =c( "#984EA3","#386CB0" )) + 
             theme_bw() +theme(legend.position = "none")
         
     })

    output$echo_histogram1 <- renderPlot({
        
        if(input$compare == "No"){
            filtro1 <- unique(echo_track$genre_top)
            titulo <- "Hip-Hop and Rock"
        } else{
            filtro1 <- "Hip-Hop"
            titulo <- "Hip-Hop"
        }
        
        hist_echo(filtro1, input$feat) %>% 
            ggplot(aes(value, fill = key)) +
            facet_wrap(~ key, scales = "free") +
            geom_histogram(bins = input$bins) + 
            scale_fill_manual(values = color_feat) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(title = titulo, x = "", fill = "")
        
    })
    
    output$echo_histogram2 <- renderPlot({

        
        if (input$compare == "No")
            return(NULL)
        
        hist_echo("Rock", input$feat) %>% 
            ggplot(aes(value, fill = key)) +
            facet_wrap(~ key, scales = "free") +
            geom_histogram(bins = input$bins) + 
            scale_fill_manual(values = color_feat) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(title = "Rock", x = "", fill = "")
        
    })
    
    output$echo_box <- renderPlot({
         echo_track %>%
            select(input$feat_box, genre_top) %>%
            gather(key, value, -genre_top) %>%
            ggplot(aes(x = genre_top, y = value)) +
            facet_wrap(~ key, scales = "free") +
            geom_boxplot(aes(fill = genre_top), alpha = 0.6) +
            scale_fill_manual(values = c("mediumvioletred","midnightblue")) +
            theme_minimal() +
            theme(legend.position = "none") +
            labs(title = "Box Plot", x = "", y = "") 
        
        
    })
    
    output$plot_corr <- renderPlot({
        
        if (input$genre_corr == "All") {
            filtro <- unique(echo_track$genre_top)
        } else{
            filtro <- c(input$genre_corr)
        }
            
        ggplot(data = echo_track %>% filter(genre_top == filtro), 
               aes(x = get(input$feat_corrx), y = get(input$feat_corry), color = genre_top)) +
            geom_point(alpha = input$trans, size = 3) +
            scale_color_manual(values =colores) +
            labs(x = input$feat_corrx, y = input$feat_corry, color = "Genre") +
            theme_light()
        
    })
    
### TREE
  
    output$plot_decision <- renderPlot({

        rpart.plot(tree.music(), type = 2,  under = TRUE,  roundint=FALSE, 
                   box.palette = paleta, 
                   round = 0, shadow.col = "gray")
    })
    
    output$table_cp <- renderTable({
        
        printcp(tree.music() )
        
    },bordered = TRUE)
    
    output$plot_cp <- renderPlot({
        
        rpart::plotcp(tree.music())
    })

    output$table_scores <- renderText({
      
      echo_pred <- predict(rf_echo(), echo_test, type = "class")
      genre_pred <- as.character(echo_pred)
      paste("F1_score:   ",MLmetrics:: F1_Score(y_true = echo_test$genre_top, y_pred = genre_pred, positive = NULL),
            "<br>",
            "Accuracy:   ",MLmetrics::Accuracy(y_pred =genre_pred, y_true = echo_test$genre_top),
      "<br>",
      "Recall:   ",MLmetrics::Recall(y_true = echo_test$genre_top, y_pred = genre_pred, positive = NULL),
      "<br>",
      "Precision:   ",MLmetrics::Precision(y_true = echo_test$genre_top, y_pred = genre_pred, positive = NULL))
      
      
    })
    
    output$import_feat <- renderPlot({
      
      varImpPlot(rf_echo())
      
    })
    # 
    # output$prueba <- renderTable({
    #   
    #   echo_pred <- predict(rf_echo(), echo_test, type = "class")
    # 
    #   head(echo_pred,7)
    # })
    # 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
