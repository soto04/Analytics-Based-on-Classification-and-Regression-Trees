
# https://www.tidymodels.org/
#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
library(shiny)
library(tidymodels)
library(tidyr)
library(shinythemes)
library(AmesHousing)
library(themis)
library(discrim)
library(tidyr)
library(modeldata)
library(sweep)
library(forecast)  # for `auto.arima`
library(timetk)    # for `tk_ts`
library(zoo)       # for `as.yearmon`
library(pls)
library(mlbench)
library(kernlab)
library(furrr)
library(scales)
library(rlang)

### How to build a parsnip mode

set_new_model("discrim_mixture")
set_model_mode(model = "discrim_mixture", mode = "classification")
set_model_engine(
    "discrim_mixture", 
    mode = "classification", 
    eng = "mda"
)
set_dependency("discrim_mixture", eng = "mda", pkg = "mda")

set_model_arg(
    model = "discrim_mixture",
    eng = "mda",
    parsnip = "sub_classes",
    original = "subclasses",
    func = list(pkg = "foo", fun = "bar"),
    has_submodel = FALSE
)

discrim_mixture <-
    function(mode = "classification",  sub_classes = NULL) {
        # Check for correct mode
        if (mode  != "classification") {
            stop("`mode` should be 'classification'", call. = FALSE)
        }
        
        # Capture the arguments in quosures
        args <- list(sub_classes = rlang::enquo(sub_classes))
        
        # Save some empty slots for future parts of the specification
        out <- list(args = args, eng_args = NULL,
                    mode = mode, method = NULL, engine = NULL)
        
        # set classes in the correct order
        class(out) <- make_classes("discrim_mixture")
        out
    }
set_fit(
    model = "discrim_mixture",
    eng = "mda",
    mode = "classification",
    value = list(
        interface = "formula",
        protect = c("formula", "data"),
        func = c(pkg = "mda", fun = "mda"),
        defaults = list()
    )
)

class_info <- 
    list(
        pre = NULL,
        post = NULL,
        func = c(fun = "predict"),
        args =
            # These lists should be of the form:
            # {predict.mda argument name} = {values provided from parsnip objects}
            list(
                # We don't want the first two arguments evaluated right now
                # since they don't exist yet. `type` is a simple object that
                # doesn't need to have its evaluation deferred. 
                object = quote(object$fit),
                newdata = quote(new_data),
                type = "class"
            )
    )

set_pred(
    model = "discrim_mixture",
    eng = "mda",
    mode = "classification",
    type = "class",
    value = class_info
)

prob_info <-
    pred_value_template(
        post = function(x, object) {
            tibble::as_tibble(x)
        },
        func = c(fun = "predict"),
        # Now everything else is put into the `args` slot
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "posterior"
    )

set_pred(
    model = "discrim_mixture",
    eng = "mda",
    mode = "classification",
    type = "prob",
    value = prob_info
)

###


d_broom <- "broom summarizes key information about models in tidy tibble()s. 
broom provides three verbs to make it convenient to 
interact with model objects:
- tidy() summarizes information about model components
- glance() reports information about the entire model
- augment() adds informations about observations to a dataset
For a detailed introduction, please see vignette(\"broom\").

broom tidies 100+ models from popular modelling packages and almost all of the model
objects in the stats package that comes with base R. vignette(\"available-methods\")
lists method availability.

If you aren’t familiar with tidy data structures and want to know how they
can make your life easier, we highly recommend reading Hadley Wickham’s Tidy Data.
"

d_rsample <- "rsample contains a set of functions to create different types of 
resamples and corresponding classes for their analysis. The goal is to have 
a modular set of methods that can be used across different R packages for:

- traditional resampling techniques for estimating the sampling distribution 
of a statistic and
- estimating model performance using a holdout set

The scope of rsample is to provide the basic building blocks for creating and 
analyzing resamples of a data set but does not include code for modeling or 
calculating statistics. The “Working with Resample Sets” vignette gives 
demonstrations of how rsample tools can be used.

Note that resampled data sets created by rsample are directly accessible in a 
resampling object but do not contain much overhead in memory. Since the original 
data is not modified, R does not make an automatic copy."

d_parsnip <- "The goal of parsnip is to provide a tidy, unified interface to 
models that can be used to try a range of models without getting bogged down in 
the syntactical minutiae of the underlying packages.
One challenge with different modeling functions available in R that do the same 
thing is that they can have different interfaces and arguments."

d_recipes <- "The recipes package is an alternative method for creating and 
preprocessing design matrices that can be used for modeling or visualization.
While R already has long-standing methods for creating these matrices 
(e.g. formulas and model.matrix), there are some limitations to what the 
existing infrastructure can do.

The idea of the recipes package is to define a recipe or blueprint that can be 
used to sequentially define the encodings and preprocessing of the data 
(i.e. “feature engineering”)."

d_workflows <- "A workflow is an object that can bundle together your pre-processing,
modeling, and post-processing requests. For example, if you have a recipe and parsnip 
model, these can be combined into a workflow. The advantages are:

You don’t have to keep track of separate objects in your workspace.

The recipe prepping and model fitting can be executed using a single call to fit().

If you have custom tuning parameter settings, these can be defined using a simpler 
interface when combined with tune.

In the future, workflows will be able to add post-processing operations, such as 
modifying the probability cutoff for two-class models."

d_tune <- "The goal of tune is to facilitate the tuning of hyper-parameters the 
tidymodels packages. It relies heavily on recipes, parsnip, and dials."

d_yardstick <- "yardstick is a package to estimate how well models are working 
using tidy data principles."

d_dials <-  "This package contains tools to create and manage values of tuning 
parameters and is designed to integrate well with the parsnip package.

The name reflects the idea that tuning predictive models can be like turning a 
set of dials on a complex machine under duress."


overviews_keys <- c("rsample","parsnip","recipes","workflows", "tune","yardstick",
            "broom", "dials" )

overviews_descriptions <- c(d_rsample,d_parsnip,d_recipes,d_workflows,
                            d_tune, d_yardstick, d_broom, d_dials)
names(overviews_descriptions) <- overviews_keys
data(Orange)
Orange <- as_tibble(Orange)


# Define UI for application
ui <- fluidPage(
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        #p("Tidymodels", style="color:firebrick"),
        a("Tidymodels", style="color:firebrick", href="https://www.tidymodels.org/"),
        tabPanel("Installation",
                 fluidRow(
                     column(5, img(src = "tidymodels.png", height=300, width = 300)
                            ),
                     column(3,
                            h3("TIDYMODELS"), br(),
                            p("The tidymodels framework is a collection of packages for modeling 
                            and machine learning using", strong("tidyverse"),"principles."), 
                            p("Install tidymodels with:"),
                            br(),br(), code("install.packages(\"tidymodels\")"), br(),br(),
                            p("Run", em("library(tidymodels)"), "to load the core packages and make 
                              them available in your current R session"))),
                 fluidRow(code("by soto04"))
                ),
        tabPanel("Packages", 
                 h3("CORE TIDYMODELS"),
                 uiOutput(outputId = "logo"),
                 p("The core tidymodels packages work together to enable a wide variety of modeling approaches."),
                 selectInput("state", "Choose a tidymodel library:",
                             list(`package` =overviews_keys) # hacer una lista limpia con los paquetes principales
                             ),
                 verbatimTextOutput("result"),
                 #htmlOutput("result")
                 
                 ),
        tabPanel("Learn",
                     tabsetPanel(
                         tabPanel("Perform",
                                  titlePanel("Performs Statistical Analysis"),
                                  tabsetPanel(
                                      tabPanel("Correlation",
                                              h4("Correlation and regression 
                                                 fundamentals with tidy data principles"),
                                              br(),
                                              p(em("Analyze the results of correlation tests and simple regression models for many data sets at once.")),
                                              br(),
                                              p("This contains 35 observations of three variables: Tree, age, and circumference. 
                                              Tree is a factor with five levels describing five trees. 
                                                As might be expected, age and circumference are correlated:"),
                                              br(),
                                              plotOutput("plot_correlation"),
                                              fluidRow(column(6,p("Suppose you want to test for correlations individually within each tree. You can do this with dplyr’s")),
                                                       column(6, tableOutput("table_correlation"))),
                                              fluidRow(a(href="https://www.tidymodels.org/learn/statistics/tidy-analysis/",
                                                         tags$img(src="analysis.png",
                                                                  title="learn more",
                                                                  width = "50",
                                                                  height= "50")))),
                                      tabPanel("K-means", 
                                               h4("K-means clustering with tidy data principles"),
                                               br(),
                                               p(em("Summarize clustering characteristics and estimate the best number of clusters for a data set.")),
                                               br(),
                                               p("K-means clustering serves as a useful example of applying tidy data principles to statistical analysis"),
                                               br(),
                                               plotOutput("plot_kmeans"),
                                               p("This is an ideal case for k-means clustering."),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/statistics/k-means/",
                                                          tags$img(src="analysis.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("Bootstrap", 
                                               h4(" Bootstrap resampling and tidy regression models"),
                                               br(),
                                               p(em("Apply bootstrap resampling to estimate uncertainty in model parameters.")),
                                               br(),
                                               p("Bootstrapping consists of randomly sampling a data set with replacement, then performing the analysis individually on each bootstrapped replicate. 
                                                 The variation in the resulting estimate is then a reasonable approximation of the variance in our estimate."),
                                               br(),
                                               p("Let’s say we want to fit a nonlinear model to the weight/mileage relationship in the mtcars data set."),
                                               br(),
                                               plotOutput("plot_bootstrap1"),
                                               br(),
                                               p("Bootstrapping is a popular method for providing confidence intervals and predictions that are more robust to the nature of the data."),
                                               br(),
                                               plotOutput("plot_bootstrap2"),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/statistics/bootstrap/",
                                                          tags$img(src="analysis.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      tabPanel("Hypothesis testing",
                                               h4("Hypothesis testing using resampling and tidy data"),
                                               br(),
                                               p(em("Perform common hypothesis tests for statistical inference using flexible functions.")),
                                               br(),
                                               p("Regardless of which hypothesis test we’re using, we’re still asking the same kind of question:"),
                                               p(strong(em("Is the effect or difference in our observed data real, or due to chance?"))),
                                               br(),
                                               p("To answer this question, we start by assuming that the observed data came from some world where “nothing is going on” 
                                                 (i.e. the observed effect was simply due to random chance), and call this assumption our null hypothesis. 
                                                 (In reality, we might not believe in the null hypothesis at all; the null hypothesis is in opposition to the alternate hypothesis, 
                                                 which supposes that the effect present in the observed data is actually due to the fact that something is going on.) 
                                                 We then calculate a test statistic from our data that describes the observed effect. 
                                                 We can use this test statistic to calculate a p-value, giving the probability that our observed data could come about 
                                                 if the null hypothesis was true. If this probability is below some pre-defined significance level", 
                                                 strong("\u03B1"), "then we can reject our null hypothesis."),
                                               br(),
                                               br(),
                                               plotOutput("plot_hypothesis"),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/statistics/infer/",
                                                          tags$img(src="analysis.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("Statistical analysis",
                                               h4("Statistical analysis of contingency tables"),
                                               br(),
                                               p(em("Use tests of independence and goodness of fit to analyze tables of counts.")),
                                               br(),
                                               p("This data set is related to cognitive impairment in 333 patients from Craig-Schapiro et al (2011). 
                                                 One of the main research questions in these data were how a person’s genetics related to the Apolipoprotein E gene affect their cognitive skills."),
                                               plotOutput("plot_analysis"),
                                               br(),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/statistics/xtabs/",
                                                          tags$img(src="analysis.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50"))))
                                      )),
                         
                         tabPanel("Create",
                                  titlePanel("Create Robust Models"),
                                  tabsetPanel(
                                      tabPanel("Regression",
                                               h4("Regression models two ways"),
                                               br(),
                                               p(em("Create and train different kinds of regression models with different computational engines.")),
                                               br(),
                                               p("We’ll use the Ames housing data set."),
                                               plotOutput("plot_regression"),
                                               p(em("This plot compares the performance of the random forest and regularized regression models.")),
                                               br(),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/models/parsnip-ranger-glmnet/",
                                                          tags$img(src="create.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      tabPanel("Classification",
                                               h4("Classification models using a neural network"),
                                               br(),
                                               p(em("Train a classification model and evaluate its performance.")),
                                               br(),
                                               p("Let’s fit a model to a small, two predictor classification data set."),
                                               plotOutput("plot_classification1"),
                                               p("Let’s use a single hidden layer neural network to predict the outcome. "),
                                               p("We can use the keras package to fit a model with 5 hidden units and a 10% dropout rate, to regularize the model"),
                                               p("Let’s also create a grid to get a visual sense of the class boundary for the validation set"),
                                               br(),
                                               plotOutput("plot_classification2"),
                                               br(),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/models/parsnip-nnet/",
                                                          tags$img(src="create.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      tabPanel("Subsampling",
                                               h4("Subsampling for class imbalances"),
                                               br(),
                                               p(em("Improve model performance in imbalanced data sets through undersampling or oversampling.")),
                                               br(),
                                               p("Subsampling a training set, either undersampling or oversampling the appropriate class or classes, 
                                                 can be a helpful approach to dealing with classification data where one or more classes occur very 
                                                 infrequently. In such a situation (without compensating for it), most models will overfit to the majority 
                                                 class and produce very good statistics for the class containing the frequently occurring classes while the 
                                                 minority classes have poor performance."),
                                               br(),
                                               p("Consider a two-class problem where the first class has a very low rate of occurrence. "),
                                               br(),
                                               p("Let’s plot the metrics for each resample to see how the individual results changed."),
                                               plotOutput("plot_subsampling"),
                                               br(),
                                               p("This visually demonstrates that the subsampling mostly affects metrics that use the hard class predictions"),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/models/sub-sampling/",
                                                          tags$img(src="create.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      tabPanel("Modeling Time Series",
                                               h4("Modeling time series with tidy resampling"),
                                               br(),
                                               p(em("Calculate performance estimates for time series forecasts using resampling.")),
                                               br(),
                                               p("The data for this article are sales of alcoholic beverages originally from the Federal Reserve Bank of St. Louis website."),
                                               br(),
                                               plotOutput("plot_modeling"),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/models/time-series/",
                                                          tags$img(src="create.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      tabPanel("Multivariate Analysis",
                                               h4("Multivariate analysis using partial least squares"),
                                               br(),
                                               p(em("Build and fit a predictive model with more than one outcome.")),
                                               br(),
                                               p("“Multivariate analysis” usually refers to multiple outcomes being modeled, 
                                                 analyzed, and/or predicted. There are multivariate versions of many common statistical tools."),
                                               br(),
                                               p("The data that we’ll use has three outcomes. modeldata::meats:"),
                                               p("The goal is to predict the proportion of the three substances using the chemistry test. 
                                                 There can often be a high degree of between-variable correlations in predictors, and that is certainly the case here."),
                                               br(),
                                               plotOutput("plot_multivariate"),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/models/pls/",
                                                          tags$img(src="create.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50"))))
                                      )),
                         
                         
                         tabPanel("Tune, Compare and Work", 
                                  titlePanel("Tune, Compare and Work with your Models"),
                                  tabsetPanel(
                                      tabPanel("Model tuning",
                                               h4("Model tuning via grid search"),
                                               br(),
                                               p(em("Choose hyperparameters for a model by training on a grid of many possible parameter values.")),
                                               br(),
                                               p("This article demonstrates how to tune a model using grid search. Many models have hyperparameters that 
                                                 can’t be learned directly from a single data set when training the model. Instead, we can train many models 
                                                 in a grid of possible hyperparameter values and see which ones turn out best."),
                                               br(),
                                               p("To demonstrate model tuning, we’ll use the Ionosphere data in the mlbench package:"),
                                               br(),
                                               p("To demonstrate, we’ll fit a radial basis function support vector machine to these data and 
                                               tune the SVM cost parameter and the ", strong("\u03C3"),"parameter in the kernel function." ),
                                               tableOutput("table_tuning"),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/work/tune-svm/",
                                                          tags$img(src="compare.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("Nested resampling",
                                               h4("Nested resampling"),
                                               br(),
                                               p(em("Estimate the best hyperparameters for a model using nested resampling")),
                                               br(),
                                               p("A typical scheme for splitting the data when developing a predictive model 
                                                 is to create an initial split of the data into a training and test set. If resampling is used, 
                                                 it is executed on the training set. A series of binary splits is created. In rsample, 
                                                 we use the term analysis set for the data that are used to fit the model and the term assessment 
                                                 set for the set used to compute performance:"),
                                               br(),
                                               p("We will simulate some regression data to illustrate the methods. The mlbench package has a 
                                                 function mlbench::mlbench.friedman1() that can simulate a complex regression data structure from the original 
                                                 MARS publication. A training set size of 100 data points are generated as well as a large set that will be used 
                                                 to characterize how well the resampling procedure performed."),
                                               br(),
                                               plotOutput("plot_nested"),
                                               br(),
                                               p("Each gray line is a separate bootstrap resampling curve created from a different 90% of the data. 
                                                 The blue line is a LOESS smooth of all the results pooled together."),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/work/nested-resampling/",
                                                          tags$img(src="compare.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("Iterative Bayesian optimization",
                                               h4("Iterative Bayesian optimization of a classification model"),
                                               br(),
                                               p(em("Identify the best hyperparameters for a model using Bayesian optimization of iterative search.")),
                                               br(),
                                               p("Many of the examples for model tuning focus on grid search. For that method, all the candidate tuning 
                                                 parameter combinations are defined prior to evaluation. Alternatively, iterative search can be used to 
                                                 analyze the existing tuning parameter results and then predict which tuning parameters to try next."),
                                               br(),
                                               p("To demonstrate this approach to tuning models, let’s return to the cell segmentation data"),
                                               br(),
                                               plotOutput("plot_bayesian1"),
                                               br(),
                                               p("There are many parameter combinations have roughly equivalent results."),
                                               br(),
                                               p(strong("How did the parameters change over iterations? ")),
                                               plotOutput("plot_bayesian2"),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/work/bayes-opt/",
                                                          tags$img(src="compare.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50"))))
                                  )),
                         
                         tabPanel("Develop", 
                                  titlePanel("Develop Custom Modeling Tools"),
                                  tabsetPanel(
                                      tabPanel("Create",
                                               h4("Create your own recipes step function"),
                                               br(),
                                               p(em("Write a new recipe step for data preprocessing.")),
                                               br(),
                                               p("Let’s create a step that replaces the value of a variable with its percentile from the training set. 
                                                 The example data we’ll use is from the modeldata package:"),
                                               br(),
                                               p("To illustrate the transformation with the carbon variable, note the training set distribution 
                                                 of this variable with a vertical line below for the first value of the test set."),
                                               plotOutput("plot_create"),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/develop/recipes/",
                                                          tags$img(src="develop.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("How to build a parsnip model",
                                               h4("How to build a parsnip model"),
                                               br(),
                                               p(em("Create a parsnip model function from an existing model implementation.")),
                                               br(),
                                               p("The parsnip package constructs models and predictions by representing those actions in expressions. 
                                                 There are a few reasons for this:"),
                                               p("- It eliminates a lot of duplicate code."),
                                               p("- Since the expressions are not evaluated until fitting, it eliminates many package dependencies."),
                                               br(),
                                               p("As an example, we’ll create a function for mixture discriminant analysis. "),
                                               tableOutput("table_build"),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/develop/models/",
                                                          tags$img(src="develop.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("Custom performance",
                                               h4("Custom performance metric"),
                                               br(),
                                               p(em("Create a new performance metric and integrate it with yardstick functions.")),
                                               br(),
                                               p("Mean squared error (sometimes MSE or from here on, mse()) is a numeric metric that 
                                                 measures the average of the squared errors. Numeric metrics are generally the simplest 
                                                 to create with yardstick, as they do not have multiclass implementations. "),
                                               br(),
                                               tableOutput("table_custom"),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/develop/metrics/",
                                                          tags$img(src="develop.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("How to create a tuning parameter funcion",
                                               h4("How to create a tuning parameter funcion"),
                                               br(),
                                               p(em("Build functions to use in tuning both quantitative and qualitative parameters.")),
                                               br(),
                                               p("Some models and recipe steps contain parameters that dials does not know about. You can construct 
                                                 new quantitative and qualitative parameters using new_quant_param() or new_qual_param(), respectively. "),
                                               br(),
                                               p("Here’s an example of a basic quantitative parameter object:"),
                                               br(),
                                               p(em("Sample from the parameter:")),
                                               tableOutput("table_how"),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/develop/parameters/",
                                                          tags$img(src="develop.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50")))),
                                      
                                      tabPanel("Create your own broom tidier methods",
                                               h4("Create your own broom tidier methods"),
                                               br(),
                                               p(em("Write tidy(), glance(), and augment() methods for new model objects.")),
                                               br(),
                                               p("The broom package provides tools to summarize key information about models in tidy tibble()s. 
                                                 The package provides three verbs, or “tidiers,” to help make model objects easier to work with:"),
                                               p("- tidy() summarizes information about model components"),
                                               p("- glance() reports information about the entire model"),
                                               p("-augment() adds information about observations to a dataset"),
                                               br(),
                                               fluidRow(a(href="https://www.tidymodels.org/learn/develop/broom/",
                                                          tags$img(src="develop.png",
                                                                   title="learn more",
                                                                   width = "50",
                                                                   height= "50"))))
                                  ))
                     )
                 
                 )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$result <- renderText({
        paste(overviews_descriptions[input$state])
        #HTML(overviews_descriptions[input$state])
    })
    output$logo <- renderUI({
        tags$img(src = paste0(input$state,".png"), height = 150, widht = 100 )
    })
    output$plot_correlation <- renderPlot({
        ggplot(Orange, aes(age, circumference, color = Tree)) +
            geom_line() + theme_minimal() + 
            labs(title = paste("Correlation Orange: age and circumference", 
                               cor(Orange$age, Orange$circumference)))
    })
    output$table_correlation <- renderTable({
        Orange %>% 
            group_by(Tree) %>%
            summarize(correlation = cor(age, circumference))
    })
    
    output$plot_kmeans <- renderPlot({
        set.seed(27)
        
        centers <- tibble(
            cluster = factor(1:3), 
            num_points = c(100, 150, 50),  # number points in each cluster
            x1 = c(5, 0, -3),              # x1 coordinate of cluster center
            x2 = c(-1, 1, -2))              # x2 coordinate of cluster center
        
        labelled_points <- 
            centers %>%
            mutate(
                x1 = map2(num_points, x1, rnorm),
                x2 = map2(num_points, x2, rnorm)) %>% 
            select(-num_points) %>% 
            unnest(cols = c(x1, x2))
        
        ggplot(labelled_points, aes(x1, x2, color = cluster)) +
            geom_point(alpha = 0.4, size = 2.5) +theme_minimal()
    })
    
    output$plot_bootstrap1 <- renderPlot({
        ggplot(mtcars, aes(mpg, wt)) + 
            geom_point( color="slateblue", size = 2.5) + theme_minimal()
    })
    
    output$plot_bootstrap2 <- renderPlot({
        set.seed(27)
        boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)
        
        fit_spline_on_bootstrap <- function(split) {
            data <- analysis(split)
            smooth.spline(data$wt, data$mpg, df = 4)}
        
        boot_splines <- 
            boots %>% 
            sample_n(200) %>% 
            mutate(spline = map(splits, fit_spline_on_bootstrap),
                   aug_train = map(spline, augment))
        
        splines_aug <- 
            boot_splines %>% 
            unnest(aug_train)
        
        ggplot(splines_aug, aes(x, y)) +
            geom_line(aes(y = .fitted, group = id), alpha = 0.1, col = "deeppink3") +
            geom_point( color="slateblue", size = 2.5) + theme_minimal()
        
    })
    
    output$plot_hypothesis <- renderPlot({
        data(gss)
        null_f_distn_theoretical <- gss %>%
            specify(age ~ partyid) %>%
            hypothesize(null = "independence") %>%
            calculate(stat = "F") 
        
        F_hat <- gss %>% 
            specify(age ~ partyid) %>%
            calculate(stat = "F")
        
        visualize(null_f_distn_theoretical, method = "theoretical") +
            shade_p_value(obs_stat = F_hat, direction = "greater") + theme_minimal()
    })
    
    output$plot_analysis <-  renderPlot({
        data(ad_data, package = "modeldata")
        ad_data %>%
            select(Genotype, Class)
        
        # calculate the observed statistic
        observed_indep_statistic <- ad_data %>%
            specify(Genotype ~ Class) %>%
            calculate(stat = "Chisq")
        
        # generate the null distribution by theoretical approximation
        null_distribution_theoretical <- ad_data %>%
            specify(Genotype ~ Class) %>%
            hypothesize(null = "independence") %>%
            # note that we skip the generation step here!
            calculate(stat = "Chisq")
        
        # visualize the theoretical null distribution and test statistic!
        ad_data %>%
            specify(Genotype ~ Class) %>%
            hypothesize(null = "independence") %>%
            visualize(method = "theoretical") + 
            shade_p_value(observed_indep_statistic,
                          direction = "greater") + theme_minimal()
    })
    
    
    output$plot_regression <- renderPlot({
        ames <- make_ames()
        
        set.seed(4595)
        data_split <- initial_split(ames, strata = "Sale_Price", p = 0.75)
        
        ames_train <- training(data_split)
        ames_test  <- testing(data_split)
        
        norm_recipe <- 
            recipe(
                Sale_Price ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold, 
                data = ames_train
            ) %>%
            step_other(Neighborhood) %>% 
            step_dummy(all_nominal()) %>%
            step_center(all_predictors()) %>%
            step_scale(all_predictors()) %>%
            step_log(Sale_Price, base = 10) %>% 
            # estimate the means and standard deviations
            prep(training = ames_train, retain = TRUE)
        
        rf_defaults <- rand_forest(mode = "regression")
        preds <- c("Longitude", "Latitude", "Lot_Area", "Neighborhood", "Year_Sold")
        
        rf_xy_fit <- 
            rf_defaults %>%
            set_engine("ranger") %>%
            fit_xy(
                x = ames_train[, preds],
                y = log10(ames_train$Sale_Price))
        
        test_normalized <- bake(norm_recipe, new_data = ames_test, all_predictors())
        
        test_results <- 
            ames_test %>%
            select(Sale_Price) %>%
            mutate(Sale_Price = log10(Sale_Price)) %>%
            bind_cols(
                predict(rf_xy_fit, new_data = ames_test[, preds]))
        
        glmn_fit <- 
            linear_reg(penalty = 0.001, mixture = 0.5) %>% 
            set_engine("glmnet") %>%
            fit(Sale_Price ~ ., data = juice(norm_recipe))
        
        test_results <- 
            test_results %>%
            rename(`random forest` = .pred) %>%
            bind_cols(
                predict(glmn_fit, new_data = test_normalized) %>%
                    rename(glmnet = .pred))
        
        test_results %>% 
            gather(model, prediction, -Sale_Price) %>% 
            ggplot(aes(x = prediction, y = Sale_Price)) + 
            geom_abline(col = "green", lty = 2, size = 1) + 
            geom_point(alpha = .2, color = "cornflowerblue", size = 1.5) + 
            facet_wrap(~model) + 
            coord_fixed() + theme_bw()
    })
    
    output$plot_classification1 <- renderPlot({
        data(bivariate)
        nrow(bivariate_train)
        nrow(bivariate_val)
        
        ggplot(bivariate_train, aes(x = A, y = B, col = Class)) + 
            geom_point(alpha = .25) + theme_minimal()
    })
    
    output$plot_classification2 <- renderPlot({
        biv_rec <- 
            recipe(Class ~ ., data = bivariate_train) %>%
            step_BoxCox(all_predictors())%>%
            step_normalize(all_predictors()) %>%
            prep(training = bivariate_train, retain = TRUE)
        
        # We will juice() to get the processed training set back
        
        # For validation:
        val_normalized <- bake(biv_rec, new_data = bivariate_val, all_predictors())
        # For testing when we arrive at a final model: 
        test_normalized <- bake(biv_rec, new_data = bivariate_test, all_predictors())
        
        set.seed(57974)
        nnet_fit <-
            mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
            set_mode("classification") %>% 
            # Also set engine-specific `verbose` argument to prevent logging the results: 
            set_engine("keras", verbose = 0) %>%
            fit(Class ~ ., data = juice(biv_rec))
        
        a_rng <- range(bivariate_train$A)
        b_rng <- range(bivariate_train$B)
        x_grid <-
            expand.grid(A = seq(a_rng[1], a_rng[2], length.out = 100),
                        B = seq(b_rng[1], b_rng[2], length.out = 100))
        x_grid_trans <- bake(biv_rec, x_grid)
        
        # Make predictions using the transformed predictors but 
        # attach them to the predictors in the original units: 
        x_grid <- 
            x_grid %>% 
            bind_cols(predict(nnet_fit, x_grid_trans, type = "prob"))
        
        ggplot(x_grid, aes(x = A, y = B)) + 
            geom_contour(aes(z = .pred_One), breaks = .5, col = "maroon4", size =1) + 
            geom_point(data = bivariate_val, aes(col = Class), alpha = 0.3, size =2)+
            theme_minimal()
    })
    
    output$plot_subsampling <- renderPlot({
        
        imbal_data <- 
            readr::read_csv("https://bit.ly/imbal_data") %>% 
            mutate(Class = factor(Class))
        dim(imbal_data)
        table(imbal_data$Class)
        
        imbal_rec <- 
            recipe(Class ~ ., data = imbal_data) %>% 
            step_rose(Class)
        
        qda_mod <- 
            discrim_regularized(frac_common_cov = 0, frac_identity = 0) %>% 
            set_engine("klaR")
        
        qda_rose_wflw <- 
            workflow() %>% 
            add_model(qda_mod) %>% 
            add_recipe(imbal_rec)
        
        set.seed(5732)
        cv_folds <- vfold_cv(imbal_data, strata = "Class", repeats = 5)
        
        cls_metrics <- metric_set(roc_auc, j_index)
        
        set.seed(2180)
        qda_rose_res <- fit_resamples(
            qda_rose_wflw, 
            resamples = cv_folds, 
            metrics = cls_metrics)
        
        #collect_metrics(qda_rose_res)
        
        qda_wflw <- 
            workflow() %>% 
            add_model(qda_mod) %>% 
            add_formula(Class ~ .)
        
        set.seed(2180)
        qda_only_res <- fit_resamples(qda_wflw, resamples = cv_folds, metrics = cls_metrics)
        #collect_metrics(qda_only_res)
        
        
        
        no_sampling <- 
            qda_only_res %>% 
            collect_metrics(summarize = FALSE) %>% 
            dplyr::select(-.estimator) %>% 
            mutate(sampling = "no_sampling")
        
        with_sampling <- 
            qda_rose_res %>% 
            collect_metrics(summarize = FALSE) %>% 
            dplyr::select(-.estimator) %>% 
            mutate(sampling = "rose")
        
        bind_rows(no_sampling, with_sampling) %>% 
            mutate(label = paste(id2, id)) %>%  
            ggplot(aes(x = sampling, y = .estimate, group = label)) + 
            geom_line(alpha = .8, color = "cornflowerblue") + 
            facet_wrap(~ .metric, scales = "free_y") +
            theme_bw()
    })
    
    output$plot_modeling <- renderPlot({
        
        data("drinks")
        glimpse(drinks)
        
        roll_rs <- rolling_origin(
            drinks, 
            initial = 12 * 20, 
            assess = 12,
            cumulative = FALSE)
        get_date <- function(x) {
            min(assessment(x)$date)}
        
        start_date <- map(roll_rs$splits, get_date)
        roll_rs$start_date <- do.call("c", start_date)
        
        fit_model <- function(x, ...) {
            # suggested by Matt Dancho:
            x %>%
                analysis() %>%
                # Since the first day changes over resamples, adjust it
                # based on the first date value in the data frame 
                tk_ts(start = .$date[[1]] %>% as.yearmon(), 
                      frequency = 12, 
                      silent = TRUE) %>%
                auto.arima(...)}
        
        roll_rs$arima <- map(roll_rs$splits, fit_model)
        
        roll_rs$interpolation <- map_dbl(
            roll_rs$arima,
            function(x) 
                sw_glance(x)[["MAPE"]])
        get_extrap <- function(split, mod) {
            n <- nrow(assessment(split))
            # Get assessment data
            pred_dat <- assessment(split) %>%
                mutate(
                    pred = as.vector(forecast(mod, h = n)$mean),
                    pct_error = ( S4248SM144NCEN - pred ) / S4248SM144NCEN * 100
                )
            mean(abs(pred_dat$pct_error))
        }
        
        roll_rs$extrapolation <- 
            map2_dbl(roll_rs$splits, roll_rs$arima, get_extrap)
        
        roll_rs %>%
            select(interpolation, extrapolation, start_date) %>%
            pivot_longer(cols = matches("ation"), names_to = "error", values_to = "MAPE") %>%
            ggplot(aes(x = start_date, y = MAPE, col = error)) + 
            geom_point() + 
            geom_line() + theme_minimal()
    })
    
    output$plot_multivariate <- renderPlot({
        
        data(meats)
        
        norm_rec <- 
            recipe(water + fat + protein ~ ., data = meats) %>%
            step_normalize(everything()) 
        
        set.seed(57343)
        folds <- vfold_cv(meats, repeats = 10)
        
        folds <- 
            folds %>%
            mutate(recipes = map(splits, prepper, recipe = norm_rec))
        
        
        get_var_explained <- function(recipe, ...) {
            
            # Extract the predictors and outcomes into their own matrices
            y_mat <- juice(recipe, composition = "matrix", all_outcomes())
            x_mat <- juice(recipe, composition = "matrix", all_predictors())
            
            # The pls package prefers the data in a data frame where the outcome
            # and predictors are in _matrices_. To make sure this is formatted
            # properly, use the `I()` function to inhibit `data.frame()` from making
            # all the individual columns. `pls_format` should have two columns.
            pls_format <- data.frame(
                endpoints = I(y_mat),
                measurements = I(x_mat)
            )
            # Fit the model
            mod <- plsr(endpoints ~ measurements, data = pls_format)
            
            # Get the proportion of the predictor variance that is explained
            # by the model for different number of components. 
            xve <- explvar(mod)/100 
            
            # To do the same for the outcome, it is more complex. This code 
            # was extracted from pls:::summary.mvr. 
            explained <- 
                drop(pls::R2(mod, estimate = "train", intercept = FALSE)$val) %>% 
                # transpose so that components are in rows
                t() %>% 
                as_tibble() %>%
                # Add the predictor proportions
                mutate(predictors = cumsum(xve) %>% as.vector(),
                       components = seq_along(xve)) %>%
                # Put into a tidy format that is tall
                pivot_longer(
                    cols = c(-components),
                    names_to = "source",
                    values_to = "proportion"
                )
        }
        
        folds <- 
            folds %>%
            mutate(var = map(recipes, get_var_explained),
                   var = unname(var))
        
        variance_data <- 
            bind_rows(folds[["var"]]) %>%
            filter(components <= 15) %>%
            group_by(components, source) %>%
            summarize(proportion = mean(proportion))

        ggplot(variance_data, aes(x = components, y = proportion, col = source)) + 
            geom_line(size = 1) + 
            geom_point(size = 1.5) + theme_minimal()
    })
    
    output$table_tuning <- renderTable({
        data(Ionosphere)
        Ionosphere <- Ionosphere %>% select(-V2)
        svm_mod <-
            svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
            set_mode("classification") %>%
            set_engine("kernlab")
        
        iono_rec <-
            recipe(Class ~ ., data = Ionosphere)  %>%
            # In case V1 is has a single value sampled
            step_zv(all_predictors()) %>% 
            # convert it to a dummy variable
            step_dummy(V1) %>%
            # Scale it the same as the others
            step_range(matches("V1_"))
        
        set.seed(4943)
        iono_rs <- bootstraps(Ionosphere, times = 30)
        
        roc_vals <- metric_set(roc_auc)
        ctrl <- control_grid(verbose = FALSE)
        
        set.seed(325)
        recipe_res <-
            svm_mod %>% 
            tune_grid(
                iono_rec,
                resamples = iono_rs,
                metrics = roc_vals,
                control = ctrl)
        
        p <- show_best(recipe_res, metric = "roc_auc")
        p
    })
    
    output$plot_nested <- renderPlot({
        
        sim_data <- function(n) {
            tmp <- mlbench.friedman1(n, sd = 1)
            tmp <- cbind(tmp$x, tmp$y)
            tmp <- as.data.frame(tmp)
            names(tmp)[ncol(tmp)] <- "y"
            tmp
        }
        
        set.seed(9815)
        train_dat <- sim_data(100)
        large_dat <- sim_data(10^5)
        
        results <- nested_cv(train_dat, 
                             outside = vfold_cv(repeats = 5), 
                             inside = bootstraps(times = 25))
        # `object` will be an `rsplit` object from our `results` tibble
        # `cost` is the tuning parameter
        svm_rmse <- function(object, cost = 1) {
            y_col <- ncol(object$data)
            mod <- 
                svm_rbf(mode = "regression", cost = cost) %>% 
                set_engine("kernlab") %>% 
                fit(y ~ ., data = analysis(object))
            
            holdout_pred <- 
                predict(mod, assessment(object) %>% dplyr::select(-y)) %>% 
                bind_cols(assessment(object) %>% dplyr::select(y))
            rmse(holdout_pred, truth = y, estimate = .pred)$.estimate}
        
        # In some case, we want to parameterize the function over the tuning parameter:
        rmse_wrapper <- function(cost, object) svm_rmse(object, cost)
        
        tune_over_cost <- function(object) {
            tibble(cost = 2 ^ seq(-2, 8, by = 1)) %>% 
                mutate(RMSE = map_dbl(cost, rmse_wrapper, object = object))}
        
        # `object` is an `rsplit` object in `results$inner_resamples` 
        summarize_tune_results <- function(object) {
            # Return row-bound tibble that has the 25 bootstrap results
            map_df(object$splits, tune_over_cost) %>%
                # For each value of the tuning parameter, compute the 
                # average RMSE which is the inner bootstrap estimate. 
                group_by(cost) %>%
                summarize(mean_RMSE = mean(RMSE, na.rm = TRUE),
                          n = length(RMSE))}
        
        tuning_results <- map(results$inner_resamples, summarize_tune_results) 
        
        plan(multisession)
        
        tuning_results <- future_map(results$inner_resamples, summarize_tune_results) 
        
        pooled_inner <- tuning_results %>% bind_rows
        
        best_cost <- function(dat) dat[which.min(dat$mean_RMSE),]
        
        p <- 
            ggplot(pooled_inner, aes(x = cost, y = mean_RMSE)) + 
            scale_x_continuous(trans = 'log2') +
            xlab("SVM Cost") + ylab("Inner RMSE")
        
        for (i in 1:length(tuning_results))
            p <- p  +
            geom_line(data = tuning_results[[i]], alpha = .2, color = "darkslateblue") +
            geom_point(data = best_cost(tuning_results[[i]]), pch = 16, alpha = 3/4)
        
        g <- p + geom_smooth(data = pooled_inner, se = FALSE) + theme_minimal()
        g
    })
    
    output$plot_bayesian1 <- renderPlot({
        
        data(cells)
        
        set.seed(2369)
        tr_te_split <- initial_split(cells %>% select(-case), prop = 3/4)
        cell_train <- training(tr_te_split)
        cell_test  <- testing(tr_te_split)
        
        set.seed(1697)
        folds <- vfold_cv(cell_train, v = 10)
        
        cell_pre_proc <-
            recipe(class ~ ., data = cell_train) %>%
            step_YeoJohnson(all_predictors()) %>%
            step_normalize(all_predictors()) %>%
            step_pca(all_predictors(), num_comp = tune()) %>%
            step_downsample(class)
        
        svm_mod <-
            svm_rbf(mode = "classification", cost = tune(), rbf_sigma = tune()) %>%
            set_engine("kernlab")
        
        svm_wflow <-
            workflow() %>%
            add_model(svm_mod) %>%
            add_recipe(cell_pre_proc)
        
        svm_set <- parameters(svm_wflow)
        
        svm_set <- 
            svm_set %>% 
            update(num_comp = num_comp(c(0L, 20L)))
        
        set.seed(12)
        search_res <-
            svm_wflow %>% 
            tune_bayes(
                resamples = folds,
                # To use non-default parameter ranges
                param_info = svm_set,
                # Generate five at semi-random to start
                initial = 5,
                iter = 50,
                # How to measure performance?
                metrics = metric_set(roc_auc),
                control = control_bayes(no_improve = 30, verbose = TRUE))
        
        estimates <- 
            collect_metrics(search_res) %>% 
            arrange(.iter)
        
        autoplot(search_res, type = "performance") + theme_bw()
    })
    
    output$plot_bayesian2 <- renderPlot({
        
        collect_metrics(search_res) %>%
            select(-.metric,-.estimator,-mean,-n,-std_err) %>%
            mutate(cost = log10(cost), 
                   rbf_sigma = log10(rbf_sigma)) %>%
            pivot_longer(cols = c(-.iter),
                         names_to = "parameter",
                         values_to = "value") %>%
            ggplot(aes(x = .iter, y = value)) +
            geom_point(color = "violetred4") +
            facet_wrap( ~ parameter, scales = "free_y") +
            theme_bw()
    })
    
    output$plot_create <-  renderPlot({
        data(biomass)
        
        biomass_tr <- biomass[biomass$dataset == "Training",]
        biomass_te <- biomass[biomass$dataset == "Testing",]
        
        ggplot(biomass_tr, aes(x = carbon)) + 
            geom_histogram(binwidth = 5, col = "violetred4", fill = "mediumpurple", alpha = .5) + 
            geom_vline(xintercept = biomass_te$carbon[1], lty = 2, color = "firebrick") + theme_bw()
        
    })
    
    output$table_build <-  renderTable({
        
        str(mda::mda)
        
        discrim_mixture(sub_classes = 2) %>%
            translate(engine = "mda")
        
        data("two_class_dat", package = "modeldata")
        set.seed(4622)
        example_split <- initial_split(two_class_dat, prop = 0.99)
        example_train <- training(example_split)
        example_test  <-  testing(example_split)
        
        mda_spec <- discrim_mixture(sub_classes = 2) %>% 
            set_engine("mda")
        
        mda_fit <- mda_spec %>%
            fit(Class ~ ., data = example_train, engine = "mda")
        
        predict(mda_fit, new_data = example_test, type = "prob") %>%
            bind_cols(example_test %>% select(Class))
        
    })
    
    output$table_custom <-  renderTable({
        
        mse <- function(data, ...) {
            UseMethod("mse")
        }
        
        mse.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
            
            metric_summarizer(
                metric_nm = "mse",
                metric_fn = mse_vec,
                data = data,
                truth = !! enquo(truth),
                estimate = !! enquo(estimate), 
                na_rm = na_rm,
                ...
            )
            
        }
        
        set.seed(1234)
        size <- 100
        times <- 10
        
        # create 10 resamples
        solubility_resampled <- bind_rows(
            replicate(
                n = times,
                expr = sample_n(solubility_test, size, replace = TRUE),
                simplify = FALSE
            ),
            .id = "resample"
        )
        
        solubility_resampled %>%
            group_by(resample) %>%
            mse(solubility, prediction)
        
    })
    
    output$table_how <- renderTable({
        num_initial_terms <- function(range = c(1L, 10L), trans = NULL) {
            new_quant_param(
                type = "integer",
                range = range,
                inclusive = c(TRUE, TRUE),
                trans = trans,
                label = c(num_initial_terms = "# Initial MARS Terms"),
                finalize = NULL
            )
        }
        
        num_initial_terms()

        
        # Sample from the parameter:
        set.seed(4832856)
        num_initial_terms() %>% value_sample(5)

        
    })
    
    output$table_methods <- renderTable({
        
        data(trees)
        
        # fit the timber volume as a function of girth and height
        trees_model <- lm(Volume ~ Girth + Height, data = trees)
        
        trees_model_tidy <- summary(trees_model)$coefficients %>% 
            as_tibble(rownames = "term")
        
        
        colnames(trees_model_tidy) <- c("term", "estimate", "std.error", "statistic", "p.value")
        confint(trees_model)

        trees_model$model
    })

    
    
    
}



# Run the application 
shinyApp(ui, server)
