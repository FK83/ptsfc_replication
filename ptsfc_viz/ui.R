#
# This is the definition of the user interface for the time series forecasting challenge app.
# 

# load necessary libraries:
library(shiny)
library(plotly)
library(htmlwidgets)
library(DT)

# identify available forecast dates (to be used in date selection):
available_plot_data <- sort(list.files("plot_data"))
available_plot_data <- available_plot_data[grepl("plot_data_", available_plot_data)]
available_dates <- gsub(".csv", "", gsub("plot_data_", "", available_plot_data))

# get vector of model names (to b eused in dropdowns):
dat_models <- read.csv("plot_data/list_models.csv")
models <- dat_models$model
names(models) <- models


# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("Probabilistic Time Series Forecasting Challenge"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            strong("Select forecast date"),
            br(),
            div(style="display: inline-block;vertical-align:top;", actionButton("skip_backward", "<")),
            div(style="display: inline-block;vertical-align:top;width:200px",
                selectizeInput("select_date",
                               label = NULL,
                               choices = rev(available_dates))),
            div(style="display: inline-block;vertical-align:top;", actionButton("skip_forward", ">")),
            selectizeInput("select_target",
                           label = "Target",
                           choices = c("Temperature" = "temperature",
                                       "Wind" = "wind"), width = "200px"),
            div(style="display: inline-block;vertical-align:top;width:200px",
                selectizeInput("select_models1", "Model 1:", choices = models, multiple = FALSE,
                           selected = "Model1")),
            div(style="display: inline-block;vertical-align:top;width:200px",
                selectizeInput("select_models2", "Model 2:", choices = c("(none selected)" = "empty2", models), multiple = FALSE,
                               selected = "Model2")),
            div(style="display: inline-block;vertical-align:top;width:200px",
                selectizeInput("select_models3", "Model 3:", choices = c("(none selected)" = "empty3", models), multiple = FALSE,
                               selected = "empty3")),
            div(style="display: inline-block;vertical-align:top;width:200px",
                selectizeInput("select_models4", "Model 4:", choices = c("(none selected)" = "empty4", models), multiple = FALSE,
                               selected = "empty4")),
            radioButtons("select_interval", label = "Show prediction interval:",
                         choices = c("95%", "50%", "none"), selected = "95%", inline = TRUE)
        ),

        # Main panel:
        mainPanel(
          # Forecast visualization:
            h4("Forecast visualization"),
            plotlyOutput("tsplot", height = "600px"),
            # Ranking table:
            h4("Rankings"),
            dataTableOutput("tab_rankings")
        )
    )
))
