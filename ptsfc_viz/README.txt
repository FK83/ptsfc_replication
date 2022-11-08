This folder contains codes and example data for a simple interactive web app displaying forecasts and a ranking of participants.

It can be run using the command runApp in R or, if using RStudio, the "Play" button shown upon opening ui.R or server.R.

The app is implemented in R Shiny and requires the following packages:
DT (tested with version 0.25)
htmlwidgets (1.5.4)
plotly (4.10.0)
shiny (1.7.2)

Contents:
ui.R - Defines the user interface of the R Shiny app
server.R - Defines the server logic of the R Shiny app
plot_data - Contains all data necessary for plotting
   
   list_models.csv - A csv file containing the names of all models which shall be shown in the app. This needs to be adapted manually if models are added.
   plot_data<date>.csv - Forecasts submitted for different models at date <date>.
   rankings - A csv with average ranks for different targets to be shown in the rankings table below the plot.
   temperature - the obsrved temperature time series.
   wind - the observed wind speed time series.
