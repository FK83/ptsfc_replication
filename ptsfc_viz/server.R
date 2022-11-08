#
# This is the server logic for the time series forecasting challenge app.
# 

# load necessary libraries:
library(shiny)
library(plotly)
library(htmlwidgets)
library(DT)

# be sure locale is English
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# number of models which can be selected at the same time.
# This solution via dropdown menues was chosen because including
# all models into the plotly graph rendered the app slow.
n_models_to_select <- 4
# to adjust the number of dropdown menus change this number,
# add the respective dropdowns in the UI and edit the line
# get selected models from input:
#        plot_data$select_models <- c(input$select_models1,
#                                     input$select_models2,
#                                     input$select_models3,
#                                     input$select_models4)
# below to include additional selection options.

# define date ranges for display of time series:
min_date <- as.Date("2021-09-01")
max_date <- as.Date("2022-04-01")

# read in rankings:
rankings <- read.csv("plot_data/rankings.csv")
rankings$mean_rk_overall <- round(rankings$mean_rk_overall, 2)

# get vector of model names:
dat_models <- read.csv("plot_data/list_models.csv")
models <- dat_models$model

# assign colors:
cols <- c('rgb(31, 119, 180)',
          'rgb(255, 127, 14)',
          'rgb(44, 160, 44)',
          'rgb(214, 39, 40)',
          'rgb(148, 103, 189)',
          'rgb(140, 86, 75)',
          'rgb(227, 119, 194)',
          'rgb(127, 127, 127)',
          'rgb(188, 189, 34)',
          'rgb(23, 190, 207)')
cols <- rep_len(cols, length.out = n_models_to_select)
# generate transparent versions of colors:
cols_transp <- gsub("rgb", "rgba", cols, fixed = TRUE)
cols_transp <- gsub(")", ", 0.5)", cols_transp, fixed = TRUE)

# create a list containing truth data:
dat_truth <- list()

# add temperature:
dat_truth$temperature <- read.csv("plot_data/temperature.csv")
dat_truth$temperature$time <- as.Date(dat_truth$temperature$time)
dat_truth$temperature <- subset(dat_truth$temperature, time >= min_date & time <= max_date)

# add wind:
dat_truth$wind <- read.csv("plot_data/wind.csv")
dat_truth$wind$time <- as.Date(dat_truth$wind$time)
dat_truth$wind <- subset(dat_truth$wind, time >= min_date & time <= max_date)

# vector of forecast dates available to be plotted:
available_plot_data <- sort(list.files("plot_data"))
available_plot_data <- available_plot_data[grepl("plot_data_", available_plot_data)]
available_dates <- gsub(".csv", "", gsub("plot_data_", "", available_plot_data))

# create list with forecast data (one element per forecast date)
forecast_data <- list()
for(i in seq_along(available_plot_data)){
    forecast_data[[available_dates[i]]] <- read.csv(paste0("plot_data/", available_plot_data[i]))
}

# Define server logic:
shinyServer(function(input, output, session) {

    # listen to "skip backward" button
    observe({
        # if pressed decrease input$select_date by 7 days
        input$skip_backward
        isolate({
            if(!is.null(input$select_date) & input$skip_backward > 0){
                new_date <- as.Date(input$select_date) - 7
                # only if the respective forecast date is available
                if(as.character(new_date) %in% available_dates){
                    updateSelectInput(session = session, inputId = "select_date",
                                      selected = as.character(new_date))
                }
            }
        })
    })

    # listen to "skip forward" button
    observe({
        # if pressed increase input$select_date by 7 days
        input$skip_forward
        isolate({
            if(!is.null(input$select_date) & input$skip_forward > 0){
                new_date <- as.Date(input$select_date) + 7
                # only if the respective forecast date is available
                if(as.character(new_date) %in% available_dates){
                    updateSelectInput(session = session, inputId = "select_date",
                                      selected = as.character(new_date))
                }
            }
        })
    })

    ####### reactive handling of data sets:

    # prepare data for plotting:
    plot_data <- reactiveValues()
    observe({
        # a mapping to determine which trace (~layer) in the plotly plot corresponds to what (needed to update the plot, see below)
        temp <- list("selected_date" = 1, # is grey area gets plotted below everything else
                     "truth" = 2*n_models_to_select + 2) # truth gets plotted on top of everything else
        # forecasts get plotted between grey area and truth.
        # indices of different forecasts: always one for shaded area (uncertainty), one for line (point forecast):
        for(i in seq_along(plot_data$select_models)){
          temp[[plot_data$select_models[i]]] <- 2*i + 0:1
        }
        plot_data$mapping <- temp

        # truth data as of selected date:
        plot_data$truth <- NULL

        # use the truth date corresponding to the input:
        if(!is.null(input$select_target)){
          temp <- dat_truth[[input$select_target]]
        }
        
        # somehow re-ordering fixes some issues with hover texts which otherwise are not shown properly
        temp$time <- rev(temp$time)
        temp$value <- rev(temp$value)
        plot_data$truth <- temp
        rm(temp)

        # get selected models from input:
        plot_data$select_models <- c(input$select_models1,
                                     input$select_models2,
                                     input$select_models3,
                                     input$select_models4)

        # choose y axis limit depending on target (simply hard-coded):
        yl <- NULL
        if(input$select_target == "wind"){
            yl <- c(0, 150)
        }
        if(input$select_target == "temperature"){
            yl <- c(-20, 50)
        }
        plot_data$ylim <- yl

        # determine y-axis limit depending on target (hard-coded):
        ylab <- NULL
        if(input$select_target == "temperature"){
            ylab <- "temperature (degree celsius)"
        }
        if(input$select_target == "wind"){
            ylab <- "wind speed (km/h)"
        }
        plot_data$ylab <- ylab

        # prepare plotting data for forecasts
        if(!is.null(input$select_date)){

            # run through models:
            for(mod in models){

                # subset to required info:
                subs <- subset(forecast_data[[input$select_date]],
                               target == input$select_target &
                                   model == mod)

                if(nrow(subs) > 0){
                    # prepare list of simple data frames for plotting:
                    points <- subs[, c("target_end_date", "q0.5")]
                    if(input$select_interval == "skip"){
                        lower <- subs[, c("target_end_date", "q0.5")]
                        upper <- subs[, c("target_end_date", "q0.5")]
                    }
                    if(input$select_interval == "50%"){
                        lower <- subs[, c("target_end_date", "q0.25")]
                        upper <- subs[, c("target_end_date", "q0.75")]
                    }
                    if(input$select_interval == "95%"){
                        lower <- subs[, c("target_end_date", "q0.025")]
                        upper <- subs[, c("target_end_date", "q0.975")]
                    }

                    colnames(points) <- colnames(lower) <- colnames(upper) <- c("x", "y")
                    intervals <- rbind(lower, upper[nrow(upper):1, ])
                    
                    # reverting necessary to avoid bug with mouseovers (don't know why)
                    points$x <- rev(points$x)
                    points$y <- rev(points$y)
                    intervals$x <- rev(intervals$x)
                    intervals$y <- rev(intervals$y)

                    # store:
                    plot_data[[mod]] <- list(points = points, intervals = intervals)
                }else{
                    plot_data[[mod]] <- NULL
                }
            }
        }

    })

    # initial plot:
    output$tsplot <- renderPlotly({

        # only run at start of app, rest is done in updates below
        isolate({

            # initialize plot:
            p <- plot_ly(mode = "lines", hovertemplate = '%{y}', source = "tsplot") %>% # hovertemplate = '%{y}' ensures labels are completely visible
                layout(yaxis = list(title = plot_data$ylab), # axis + legend settings
                       xaxis = list(title = "time"),
                       hovermode = "x unified") %>%
                add_polygons(x = c(min(as.Date(plot_data$truth$time)), as.Date(input$select_date) + 1, # grey shade to separate past and future
                                   as.Date(input$select_date) + 1, min(as.Date(plot_data$truth$time))),
                             y = rep(plot_data$ylim, each = 2),
                             hoverinfo = "skip",
                             fillcolor = "rgba(0.9, 0.9, 0.9, 0.5)",
                             line = list(width = 0),
                             showlegend = FALSE) %>%
                add_lines(x = rep(as.POSIXlt(input$select_date), 2) + 24*60*60, # vertical line for selected date
                          y = plot_data$ylim, hoverinfo = "skip",
                          line = list(color = 'rgb(0.5, 0.5, 0.5)', dash = "dot"),
                          showlegend = FALSE) # %>%

            # add forecasts: run through selected models
            for(m in seq_along(plot_data$select_models)){
                mod <- plot_data$select_models[m]
                if(!is.null(plot_data[[mod]])){
                    # if forecast available: prepare plot data
                    show_forecast <- TRUE
                    x <- plot_data[[mod]]$points$x
                    y <- plot_data[[mod]]$points$y
                    s <- 5
                    x_intervals <- plot_data[[mod]]$intervals$x
                    y_intervals <- plot_data[[mod]]$intervals$y
                }else{
                    # if no forecast available: "hide" the respective trace
                    show_forecast <- FALSE
                    x <- min(plot_data$truth$time)
                    y <- 0
                    s <- 0.001
                    x_intervals <- min(plot_data$truth$time)
                    y_intervals <- 0
                }
                # add shaded areas for uncertainty:
                p <- p%>% add_polygons(x = x_intervals, y = y_intervals,
                                       line = list(width = 0),
                                       fillcolor = cols_transp[m],
                                       hoverinfo = "skip",
                                       legendgroup = mod, showlegend = FALSE)
                # add point forecasts:
                p <- p %>% add_trace(x = x, y = y,
                                     name = mod,
                                     type = "scatter",
                                     mode = "lines",
                                     line = list(dash = "dot",
                                                 width = 2,
                                                 color = cols[m]),
                                     legendgroup = mod, showlegend = show_forecast)
            }

            # trace for most recent truth data on top
            p <- p %>% add_lines(x = plot_data$truth$time,
                                 y = plot_data$truth$value,
                                 name = paste("truth"),
                                 line = list(color = 'rgb(0, 0, 0)'),
                                 showlegend = TRUE)
            # suppress legend double click:
            p <- p %>% onRender("function(el,x){el.on('plotly_legenddoubleclick', function(){ return false; })}")
            # return
            p
        })
    })

    # register proxy (necessary to modify plotly objsect):
    myPlotProxy <- plotlyProxy("tsplot", session)

    ### Code bits for various updates of plotly plot if anything changes:
    
    # update shaded area to mark selected date:
    observe({
        plotlyProxyInvoke(myPlotProxy, "restyle", list(x = list(rep(as.Date(input$select_date) + 1, 2)),
                                                       y = list(plot_data$ylim), 
                                                       hoverinfo = "skip"),
                          list(1))
        plotlyProxyInvoke(myPlotProxy, "restyle", list(x = list(c(min(as.Date(plot_data$truth$time)), as.Date(input$select_date) + 1, # grey shade to separate past and future
                                                                  as.Date(input$select_date) + 1, min(as.Date(plot_data$truth$time)))),
                                                       y = list(rep(plot_data$ylim, each = 2))),
                          list(0))
    })

    # update truth as of selected date:
    observe({
        plotlyProxyInvoke(myPlotProxy, "restyle", list(x = list(plot_data$truth$time),
                                                       y = list(plot_data$truth$value)),
                          list(plot_data$mapping$truth))
    })

    # update forecasts:
    observe({
        for(mod in c(paste0("empty", 1:n_models_to_select), models)){
            if(!is.null(plot_data[[mod]])){
                # if forecast available: prepare plot data
                show_forecast <- TRUE
                x <- plot_data[[mod]]$points$x
                y <- plot_data[[mod]]$points$y
                s <- 5
                x_intervals <- plot_data[[mod]]$intervals$x
                y_intervals <- plot_data[[mod]]$intervals$y
            }else{
                # if no forecast available: "hide" the respective trace
                show_forecast <- FALSE
                x <- min(plot_data$truth$time)
                y <- 1
                s <- 0.001
                x_intervals <- min(plot_data$truth$time)
                y_intervals <- 0
            }
            # shaded area for uncertainty:
            plotlyProxyInvoke(myPlotProxy, "restyle",
                              list(x = list(x_intervals),
                                   y = list(y_intervals),
                                   hoverinfo = "skip"),
                              list(plot_data$mapping[[mod]][1]))
            # point forecasts:
            plotlyProxyInvoke(myPlotProxy, "restyle",
                              list(x = list(x),
                                   y = list(y),
                                   name = list(mod),
                                   showlegend = show_forecast),
                              list(plot_data$mapping[[mod]][2]))
        }

    })

    # update ylim
    observe({
        # y axis limit
        yl <- NULL
        if(input$select_target == "wind"){
            yl <- c(0, 150)
        }
        if(input$select_target == "temperature"){
            yl <- c(-20, 50)
        }
        plot_data$ylim <- yl
    })

    # update y-label
    observe({
        plotlyProxyInvoke(myPlotProxy, "relayout",
                          list(yaxis = list(title = plot_data$ylab)))
    })


    # generate table with ranks:
    output$tab_rankings <- DT::renderDataTable({
        datatable(rankings, filter = "top",
                  colnames = c("", "model", "av. rank wind", "av. rank temperature", "av. rank overall"))
    })

})
