## Canada COVID-19 interactive tracking tool

This github page contains the code and input data for the [Canada COVID-19 dashboard](https://mhcui.shinyapps.io/Canada_COVID/).

Input COVID-19 data are obtained from the [Berry et al. 2020](https://github.com/ishaberry/Covid19Canada).

This dashboard aims to complement existing COVID-19 dashboards with several additional interactive features, including analysis of healthcare efficiency, economic impact, and provincial government responses. 

Please see the *About* tab in the [Canada COVID-19 dashboard](https://mhcui.shinyapps.io/Canada_COVID/) for a full list of data sources and references.

## Major Updates (DD/MM/YYYY)
- **24/01/2021**: 'Vaccines completed' series added. 'Vaccines completed' refers to people who have received both doses of a vaccine. 'Vaccines administered' refers to people who have received one or more doses.
- **16/12/2020**: Vaccinations and testing data now included in the main dashboard. Data courtesey of [Berry et al. 2020](https://github.com/ishaberry/Covid19Canada), as with all other COVID-19 data. 
- **11/06/2020**: Front page and "General graphs" tab updated with 'per 1000 people metrics' based on Canada's 2018 health region population estimates. "Dynamics", and "Moving average dynamics" tabs now feature 'per 1000 active cases' rates instead of 'per 100 active cases'.
- **07/05/2020**: "General graphs", "Dynamics", and "Moving average dynamics" tabs now feature disaggregation at the health region level. Google Mobility metrics now available on the "Economic impact" tab, under 'Mobility'.
- **06/05/2020**: Front page map is now graphed based on a time series data set rather than a one-day snapshot. As a result, animated features on the left-panel will also animate the geographic progression of the virus using the map.
- **01/05/2020**: Default government response stringency index no longer includes financial measures. Coding on cancellations of public events/limitations on public gatherings changed.

## Analysis code

Key elements of the analysis code are as follows:
- **clean.do**: a Stata script that extracts and reformats time-series from various sources. This script is run once daily to update the files contained in the *input_data* folder.
- **get_geocodes.R**: an R script used to gather geocodes for plotting. This script requires access to Google APIs.
- **app.R**: an R script used to render the Shiny app. This consists of several plotting functions as well as the ui (user interface) and server code required to render the Shiny app. This script will become more complex as additional interactive features are added.
- **input_data**: a folder containing dynamic input data relating to the evolving COVID-19 pandemic and static input data relating to past epidemics and country mapping coordinates.

## Other resources

Several resources proved invaluable when building this app, including:
- The [COVID-19 tracker](https://vac-lshtm.shinyapps.io/ncov_tracker/) and [associated code](https://github.com/eparker12/nCoV_tracker);
- The [RStudio Leaflet tutorials](https://rstudio.github.io/leaflet/).

## Authors
Minnie Cui
minniehcui@gmail.com
