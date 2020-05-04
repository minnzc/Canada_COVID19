## Canada COVID-19 interactive tracking tool

This github page contains the code and input data for the [Canada COVID-19 tracking tool](https://mhcui.shinyapps.io/Canada_COVID/).

Input COVID-19 data are obtained from the [Epidemiological Data from the COVID-19 Outbreak in Canada github page](https://github.com/ishaberry/Covid19Canada).

This dashboard aims to complement existing COVID-19 dashboard (such the [Johns Hopkins University COVID-19 dashboard](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)) with several additional interactive features, including analysis of healthcare efficiency, economic impact, and provincial government responses. 

## Analysis code

Key elements of the analysis code are as follows:
- *clean.do* – a Stata script that extracts and reformats time-series from various sources. This script is run once daily to update the files contained in the *input_data* folder.
- *get_geocodes.R* - an R script used to gather geocodes for plotting. This script requires access to Google APIs.
- *app.R* - an R script used to render the Shiny app. This consists of several plotting functions as well as the ui (user interface) and server code required to render the Shiny app. This script will become more complex as additional interactive features are added.
- *input_data* - a folder containing dynamic input data relating to the evolving COVID-19 pandemic and static input data relating to past epidemics and country mapping coordinates.

## Other resources

Several resources proved invaluable when building this app, including:
- The [COVID-19 tracker](https://vac-lshtm.shinyapps.io/ncov_tracker/) and [associated code](https://github.com/eparker12/nCoV_tracker);
- The [RStudio Leaflet tutorials](https://rstudio.github.io/leaflet/).

## Authors
Minnie Cui

## Contact
minniehcui@gmail.com
