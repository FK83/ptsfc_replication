### Contents

This folder contains codes and example data for a simple interactive web app displaying forecasts and a ranking of participants. It can be run using the command runApp in R or, if using RStudio, the "Play" button shown upon opening ui.R or server.R. The app is implemented in R Shiny and requires the following packages: DT (tested with version 0.25); htmlwidgets (1.5.4); plotly (4.10.0);  shiny (1.7.2).

The specific contents of the folder are as follows:

- ui.R - Defines the user interface of the R Shiny app
- server.R - Defines the server logic of the R Shiny app
- plot_data - Contains all data necessary for plotting
   - list_models.csv - A csv file containing the names of all models which shall be shown in the app. This needs to be adapted manually if models are added.
   - plot_data_YYYY-MM_DD.csv - Forecasts submitted for different models at date YYYY-MM_DD.
   - rankings.csv - Average ranks for different targets to be shown in the rankings table below the plot.
   - temperature.csv - The observed temperature time series (at noon at Berlin-Tempelhof weather station).
   - wind.csv - The observed wind speed time series (at noon at Berlin-Tempelhof weather station).

### Data source

The observation data for temperature and wind speed at Berlin-Tempelhof included in the files '/ptsfc_viz/plot_data/temperature.csv' and '/ptsfc_viz/plot_data/wind.csv' were obtained from the Climate Data Center of the German Weather Service (Deutscher Wetterdienst, DWD) at https://www.dwd.de/EN/climate_environment/cdc/cdc_node_en.html. DWD allows the use of these datasets without restrictions in accordance with the "Verordnung zur Festlegung der Nutzungsbestimmungen für die Bereitstellung von Geodaten des Bundes (GeoNutzV)" (Regulation for the Determination of the Conditions of Use for the Provision of Geodata of the Federal Government (GeoNutzV)) with the addition of a source note.
