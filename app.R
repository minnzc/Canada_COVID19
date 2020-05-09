## Canada COVID-2019 interactive dashboard
## Author:       Minnie Cui
## Affiliation:  Bank of Canada
## Code created: 14 April 2020
## Last updated: 8 May 2020

## includes code adapted from the following sources:
# https://github.com/eparker12/nCoV_tracker

###################################################################################################
# CODE SET UP

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# set mapping colour for each outbreak
covid_col = "#cc4c02"
new_col = "#835e8d"
death_col = "#c20000"
recovered_col = "#358f3b"
active_col = "#5678b8"

# import data
cv_cases = read.csv("input_data/covid_geocodes.csv", encoding="UTF-8")
cv_cases_region = read.csv("input_data/covid_region.csv", encoding="UTF-8")
cv_cases_province = read.csv("input_data/covid_province.csv")
cv_cases_canada = read.csv("input_data/covid_canada.csv")

###################################################################################################
# REQUIRED FUNCTIONS

# function for gettin max and excluding null entries
get_max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
get_min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

# function to plot new cases by region
province_plot_new = function(cv_cases, plot_date, ylabel) {
    plot_df = subset(cv_cases, date<=plot_date)
    max_scale = get_max(cv_cases$new_outcome)
    g1 = ggplot(plot_df, aes(x = date, y = new_outcome, fill = region, group = 1,
                             text = paste0("Date: ", format(date, "%d %B %Y"), "\n", "Region: ", region, "Y-axis value: ",new_outcome))) +
        ylim(0, max_scale) + xlab("Date") + geom_bar(position="stack", stat="identity") + 
        ylab(ylabel) + theme_bw() + scale_fill_manual(values=province_cols) + xlim(cv_min_date, current_date) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text"), width = 900) %>% layout(legend = list(font = list(size=11)))
}

# ------------------------------
# function to plot cumulative cases by region
province_plot_cumulative = function(cv_cases, plot_date, ylabel) {
    plot_df = subset(cv_cases, date<=plot_date)
    max_scale = get_max(cv_cases$outcome)
    g1 = ggplot(plot_df, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0("Date: ", format(date, "%d %B %Y"), "\n", "Region: ", region, "\n", ylabel, ": ",outcome))) +
        ylim(0, max_scale) + xlab("Date") + geom_line(alpha=0.8) + geom_point(size = 2, alpha = 0.8) +
        ylab(paste(ylabel, "(persons, thousands)")) + theme_bw() + 
        scale_colour_manual(values=province_cols) + xlim(cv_min_date, current_date) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text"), width = 900) %>% layout(legend = list(font = list(size=11)))
}

province_plot_cumulative_log = function(cv_cases, plot_date, ylabel) {
    plot_df = subset(cv_cases, date<=plot_date)
    max_scale = get_max(cv_cases$outcome)
    g1 = ggplot(plot_df, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0("Date: ", format(date, "%d %B %Y"), "\n", "Region: ", region, "\n", ylabel, ": ",outcome))) +
        xlab("Date") + geom_line(alpha=0.8) + geom_point(size = 2, alpha = 0.8) +
        ylab(paste("Log of", ylabel, "(persons, thousands)")) + theme_bw() + xlim(cv_min_date, current_date) +
        scale_colour_manual(values=province_cols) + scale_y_continuous(trans="log10", labels = scales::number_format(accuracy = 0.1)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text"), width = 900) %>% layout(legend = list(font = list(size=11)))
}

# function to plot scatter plots
scatter_plot = function(cv_cases, plot_date, lag=c("18-day", "14-day")) {
    plot_df = subset(cv_cases, date<=plot_date)
    
    if (lag=="18-day") {
        max_scale = max(get_max(cv_cases$outcome), get_max(cv_cases$active18))
        g = ggplot(plot_df, aes(x = active18, y = outcome, colour = region, group = 1,
                                text = paste0("Date: ", format(date, "%d %B %Y"), 
                                              "\n", "Region: ", region, 
                                              "\n", "Y-axis variable (day t): ", outcome, 
                                              "\n", "Active cases (day t-18): ", active18))) +
            ylim(0, max_scale) + xlim(0, max_scale)+ xlab("Active cases (day t-18)")
    }
    
    if (lag=="14-day") {
        max_scale = max(get_max(cv_cases$outcome), get_max(cv_cases$active14))
        g = ggplot(plot_df, aes(x = active14, y = outcome, colour = region, group = 1,
                                text = paste0("Date: ", format(date, "%d %B %Y"), 
                                              "\n", "Region: ", region, 
                                              "\n", "Y-axis variable (day t): ", outcome, 
                                              "\n", "Active cases (day t-14): ", active14))) +
            ylim(0, max_scale) + xlim(0, max_scale) + xlab("Active cases (day t-14)")
    }
    
    g1 = g + geom_abline(intercept = 0, slope = 1) + geom_point(size = 2, alpha = 0.8) +
        ylab("Y-axis variable (day t)") + theme_bw() + 
        scale_colour_manual(values=province_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), axis.title=element_text(size=10,face="bold"))
    ggplotly(g1, tooltip = c("text"), width = 900) %>% layout(legend = list(font = list(size=11)))
}

# ------------------------------
# function to plot ratio plots
ratio_plot = function(cv_cases, plot_date, lag=c("18-day", "14-day")) {
    plot_df = subset(cv_cases, date<=plot_date)
    
    if (lag=="18-day") {
        max_scale = max(get_max(cv_cases$outcome))
        g = ggplot(data=plot_df[!is.na(plot_df$outcome),], aes(x = date, y = outcome, colour = region, group = 1,
                                text = paste0("Date: ", format(date, "%d %B %Y"), 
                                              "\n", "Region: ", region, 
                                              "\n", "Y-axis variable (day t) per 100 active cases (day t-18): ", outcome))) +
            ylim(0, max_scale) + xlab("Date") + ylab("Y-axis variable (day t) per 100 active cases (day t-18)") 
    }
    
    if (lag=="14-day") {
        max_scale = max(get_max(cv_cases$outcome))
        g = ggplot(data=plot_df[!is.na(plot_df$outcome),], aes(x = date, y = outcome, colour = region, group = 1,
                                text = paste0("Date: ", format(date, "%d %B %Y"), 
                                              "\n", "Region: ", region, 
                                              "\n", "Y-axis variable (day t) per 100 active cases (day t-14): ", outcome))) +
            ylim(0, max_scale) + xlab("Date") + ylab("Y-axis variable (day t) per 100 active cases (day t-14)") 
    }
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 2, alpha = 0.8) +
        theme_bw() + scale_colour_manual(values=province_cols) + xlim(cv_min_date, current_date) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), axis.title=element_text(size=9,face="bold"))
    ggplotly(g1, tooltip = c("text"), width = 900) %>% layout(legend = list(font = list(size=11)))
}

# ------------------------------
# function to plot economic impact comparisons
province_plot_cumulative_impact1 = function(cv_cases, plot_date, covid, eimpact) {
    plot_df = subset(cv_cases, date<=plot_date)
    max_scale1 = get_max(cv_cases$outcome1)
    min_scale2 = get_min(cv_cases$outcome2)
    g1 = ggplot(plot_df, aes(x = date, y = outcome1,
                             text = paste0("Date: ", format(date, "%d %B %Y"), 
                                           "\n", "Region: ", region, 
                                           "\n", covid, ": ", outcome1,
                                           "\n", eimpact, ": ", outcome2))) +
        geom_line(data=plot_df[!is.na(plot_df$outcome1),], aes(colour = region, group = 1), alpha=0.8) + 
        geom_point(data=plot_df[!is.na(plot_df$outcome1),], aes(colour = region, group = 1), size = 2, alpha = 0.8) +
        xlab("Date") + ylab(paste(covid, "(persons, thousands)")) + theme_bw() + scale_colour_manual(values=province_cols) + ylim(0, max_scale1) + xlim(cv_min_date, current_date) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), axis.title=element_text(size=10,face="bold"))
    ggplotly(g1, tooltip = c("text"), width = 900, height=400) %>% layout(legend = list(font = list(size=12)))
}

province_plot_cumulative_impact2 = function(cv_cases, plot_date, covid, eimpact) {
    plot_df = subset(cv_cases, date<=plot_date)
    max_scale2 = get_max(cv_cases$outcome2)
    min_scale2 = get_min(cv_cases$outcome2)
    g2 = ggplot(plot_df, aes(x = date, y = outcome2,
                             text = paste0("Date: ", format(date, "%d %B %Y"), 
                                           "\n", "Region: ", region, 
                                           "\n", covid, ": ", outcome1,
                                           "\n", eimpact, ": ", outcome2))) +
        geom_line(data=plot_df[!is.na(plot_df$outcome2),], aes(colour = region, group = 1), alpha=0.8) + 
        geom_point(data=plot_df[!is.na(plot_df$outcome2),], aes(colour = region, group = 1), size = 3, alpha = 0.8, shape = 2) +
        xlab("Date") + ylab(eimpact) + theme_bw() + scale_colour_manual(values=province_cols) + ylim(min(min_scale2, 0), max_scale2) + xlim(cv_min_date, current_date) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), axis.title=element_text(size=10,face="bold"))
    
    ggplotly(g2, tooltip = c("text"), width = 900, height=400) %>% layout(legend = list(font = list(size=12)))
    
}

province_plot_cumulative_impact_log1 = function(cv_cases, plot_date, covid, eimpact) {
    plot_df = subset(cv_cases, date<=plot_date)
    g1 = ggplot(plot_df, aes(x = date, y = outcome1,
                             text = paste0("Date: ", format(date, "%d %B %Y"), 
                                           "\n", "Region: ", region, 
                                           "\n", covid, ": ", outcome1, 
                                           "\n", eimpact, ": ", outcome2))) +
        geom_line(data=plot_df[!is.na(plot_df$outcome1),], aes(colour = region, group = 1), alpha=0.8) + 
        geom_point(data=plot_df[!is.na(plot_df$outcome1),], aes(colour = region, group = 1), size = 2, alpha = 0.8) +
        xlab("Date") + ylab(paste("Log of", covid, "(persons, thousands)")) + theme_bw() + scale_colour_manual(values=province_cols) + scale_y_continuous(trans="log10", labels = scales::number_format(accuracy = 0.1)) +
        xlim(cv_min_date, current_date) + theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), axis.title=element_text(size=10,face="bold"))
    
    ggplotly(g1, tooltip = c("text"), width = 900) %>% layout(legend = list(font = list(size=12)))
}

province_plot_cumulative_impact_log2 = function(cv_cases, plot_date, covid, eimpact) {
    plot_df = subset(cv_cases, date<=plot_date)
    g1 = ggplot(plot_df, aes(x = date, y = outcome2,
                             text = paste0("Date: ", format(date, "%d %B %Y"), 
                                           "\n", "Region: ", region, 
                                           "\n", covid, ": ", outcome1, 
                                           "\n", eimpact, ": ", outcome2))) +
        geom_line(data=plot_df[!is.na(plot_df$outcome2),], aes(colour = region, group = 1), alpha=0.8) + 
        geom_point(data=plot_df[!is.na(plot_df$outcome2),], aes(colour = region, group = 1), size = 3, alpha = 0.8, shape = 2) + xlim(cv_min_date, current_date) +
        xlab("Date") + ylab(paste("Log of", eimpact)) + theme_bw() + scale_colour_manual(values=province_cols) + scale_y_continuous(trans="log10", labels = scales::number_format(accuracy = 0.1)) +
        xlim(cv_min_date, current_date) + theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), axis.title=element_text(size=10,face="bold"))
    
    ggplotly(g1, tooltip = c("text"), width = 900) %>% layout(legend = list(font = list(size=12)))
}

# ------------------------------
# front page graphing functions (ggplot version of first two functions)
# function to plot cumulative COVID cases by date
cumulative_cases_plot = function(dataset, plot_date) {
    plot_df = subset(dataset, date<=plot_date)
    g1 = ggplot(data=plot_df[!is.na(plot_df$cases),], aes(x = date, y = cases/1000, color = region)) + geom_line() + geom_point(size = 2, alpha = 0.8) +
        ylab("Cumulative (x1000)") + xlab("Date") + theme_bw() + scale_colour_manual(values=c(covid_col)) +
        ylim(0, get_max(dataset$cases/1000)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=20), axis.text=element_text(size=11),
              axis.title=element_text(size=11,face="bold"), plot.margin = margin(5, 12, 5, 5))
    g1
}

new_cases_plot = function(dataset, plot_date) {
    plot_df = subset(dataset, date<=plot_date)
    g1 = ggplot(data=plot_df[!is.na(plot_df$new_cases),], aes(x = date, y = new_cases/1000, fill = region, text = paste0("Date: ", date , "\n", "Cases: ", cases))) + geom_bar(position="stack", stat="identity") + 
        ylab("New cases (x1000)") + xlab("Date") + theme_bw() + scale_fill_manual(values=c(covid_col)) + ylim(0, get_max(dataset$new_cases/1000)) + scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=20), axis.text=element_text(size=11), 
              axis.title=element_text(size=11,face="bold"), plot.margin = margin(5, 12, 5, 5)) 
    g1
}

###################################################################################################
# DATA PROCESSING

# clean date variables and extract min/max dates in data
cv_cases$date = as.Date(cv_cases$date,"%Y-%m-%d") 
cv_cases_region$date = as.Date(cv_cases_region$date,"%Y-%m-%d") 
cv_cases_province$date = as.Date(cv_cases_province$date,"%Y-%m-%d")
cv_cases_canada$date = as.Date(cv_cases_canada$date,"%Y-%m-%d")
geo_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
cv_min_date = as.Date(min(cv_cases_canada$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases_canada$date),"%Y-%m-%d")

# extract time stamp for labeling
update = current_date

# map labeling
cv_cases_canada$region = "Global"

# create health region variable
# cv_cases_region$healthregion = paste(cv_cases_region$region, cv_cases_region$province, sep=", ")

# create basemap for front page
basemap = leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'bottomright' }).addTo(this)}") %>%
    addTiles() %>% 
    addLayersControl(
        position = "bottomright",
        overlayGroups = c("2019-COVID (cumulative)", "2019-COVID (new)", "2019-COVID (deaths)", "2019-COVID (recovered)"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("2019-COVID (new)", "2019-COVID (deaths)", "2019-COVID (recovered)"))  %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(-105,42,-90,65)

# assign colours to provinces to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_names = c(as.character(unique(cv_cases_province$province)), as.character(unique(cv_cases_canada$country)), as.character(unique(cv_cases_region$healthregion)))
province_cols = cls[1:length(cls_names)]
names(province_cols) = cls_names

cls = c(covid_col, death_col, recovered_col)
cls_names = c("Cases", "Deaths", "Recovered")
graphs_cols = cls[1:length(cls_names)]
names(graphs_cols) = cls_names

###################################################################################################
# SERVER

ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("yeti"), collapsible = TRUE,
               "Canada COVID-19 tracker", id="nav",
               
               # ------------------------------
               # front page panel
               tabPanel("Map & summary",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 80, left = 20, width = 300, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          h4(textOutput("reactive_case_count"), align="right", style="color:#000000"),
                                          span(h5(textOutput("reactive_recovered_count"), align = "right"), style="color:#358f3b"),
                                          span(h5(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                          h5(textOutput("reactive_death_count"), align = "right"),
                                          h6(textOutput("clean_date_reactive"), align = "right"),
                                          h6("Updated once daily. For global cases, refer to:", align = "right"),
                                          tags$h6(tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard"), align = "right"),
                                          plotOutput("epi_curve", height="140px", width="100%"),
                                          plotOutput("cumulative_plot", height="140px", width="100%"),
                                          
                                          sliderInput("plot_date",
                                                      label = h6("Select mapping date"),
                                                      min = as.Date(geo_min_date,"%Y-%m-%d"),
                                                      max = as.Date(current_date,"%Y-%m-%d"),
                                                      value = as.Date(current_date),
                                                      width = "100%",
                                                      timeFormat = "%d %b", 
                                                      animate=animationOptions(interval = 750, loop = FALSE))
                                          
                            ))
               ),
               
               # ------------------------------
               # general regional graphs panel
               
               tabPanel("General graphs",
                        
                        titlePanel("General COVID-19 graphs"),
                        tags$br(),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select", "Select region level:",   
                                            choices = c("Country", "Province", "Health region"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select", "Select country/province/region:",   
                                            choices = c("Canada"), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = "Canada",
                                            multiple = TRUE),
                                
                                pickerInput("outcome_select", "Select y-axis variable:",   
                                            choices = c("Cases", "Deaths", "Recovered"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                "Select mapping date:",
                                
                                sliderInput("plot_date_region",
                                            label = "",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value = as.Date(current_date),
                                            width = "100%",
                                            timeFormat = "%d %b", 
                                            animate=animationOptions(interval = 500, loop = FALSE))
                                
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Cumulative", plotlyOutput("province_plot_cumulative")),
                                    tabPanel("New", plotlyOutput("province_plot_new")),
                                    tabPanel("Cumulative (log10)", plotlyOutput("province_plot_cumulative_log"))
                                )
                            )
                        )
               ),
               
               # ------------------------------
               # dynamics graphs panel
               
               tabPanel("Dynamics",
                        
                        titlePanel("COVID-19 dynamics"),
                        tags$br(),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select_dynamics", "Select region level:",   
                                            choices = c("Country", "Province", "Health region"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select_dynamics", "Select country/province/region:",   
                                            choices = c("Canada"), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = "Canada",
                                            multiple = TRUE),
                                
                                pickerInput("outcome_select_dynamics", "Select y-axis COVID variable:",   
                                            choices = c("Deaths", "Recovered", "Deaths & Recovered"), 
                                            selected = c("Deaths"),
                                            multiple = FALSE),
                                
                                pickerInput("stat", "Select lag period of active cases:",   
                                            choices = c("18-day", "14-day"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = "18-day",
                                            multiple = FALSE), 
                                
                                "Select mapping date:",
                                
                                sliderInput("plot_date_dynamics",
                                            label = "",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value = as.Date(current_date),
                                            width = "100%",
                                            timeFormat = "%d %b", 
                                            animate=animationOptions(interval = 500, loop = FALSE)),
                                "The graph on the right attempts to illustrate the dynamics of the y-variable at the selected date to active cases from 18 and 14 days prior. Recent studies have found that, on average, the duration from displaying COVID-19 symptoms to death is 17.8 days, and provide motivation for these graphs."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Ratio", plotlyOutput("ratio_plot")),
                                    tabPanel("Scatter", plotlyOutput("scatter_plot"))
                                )
                            )
                        )
               ),
               
               # ------------------------------
               # moving average dynamics graphs panel
               
               tabPanel("Moving average dynamics",
                        
                        titlePanel("Moving average COVID-19 dynamics"),
                        tags$br(),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select_mv", "Select region level:",   
                                            choices = c("Country", "Province", "Health region"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select_mv", "Select country/province/region:",   
                                            choices = c("Canada"), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = "Canada",
                                            multiple = TRUE),
                                
                                pickerInput("outcome_select_mv", "Select y-axis COVID variable:",   
                                            choices = c("Deaths", "Recovered", "Deaths & Recovered"), 
                                            selected = c("Deaths"),
                                            multiple = FALSE),
                                
                                pickerInput("stat_mv", "Select lag period of active cases:",   
                                            choices = c("18-day", "14-day"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = "18-day",
                                            multiple = FALSE), 
                                
                                "Select mapping date:",
                                
                                sliderInput("plot_date_mv",
                                            label = "",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value = as.Date(current_date),
                                            width = "100%",
                                            timeFormat = "%d %b", 
                                            animate=animationOptions(interval = 500, loop = FALSE)),
                                "All series in this graph are 7-day backwards moving averages. The graph on the right attempts to illustrate the dynamics of the y-variable at the selected date to active cases from 18 and 14 days prior. Recent studies have found that, on average, the duration from displaying COVID-19 symptoms to death is 17.8 days, and provide motivation for these graphs."
                                
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Ratio (moving average)", plotlyOutput("ratio_plot_mv")),
                                    tabPanel("Scatter (moving average)", plotlyOutput("scatter_plot_mv"))
                                )
                            )
                        )
               ),
               
               # ------------------------------
               # economic impact graphs panel
               
               tabPanel("Economic impact",
                        
                        titlePanel("Mobility"),
                        tags$br(),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select_impact2", "Select region level:",   
                                            choices = c("Country", "Province"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select_impact2", "Select country/province:",   
                                            choices = c("Canada"), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = "Canada",
                                            multiple = TRUE),
                                
                                pickerInput("outcome_select_impact2", "Select y-axis COVID variable:",   
                                            choices = c("Cases", "Deaths", "Recovered"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                pickerInput("stat_impact2", "Select economic impact variable:",   
                                            choices = c("Google Maps Mobility (% change from baseline), retail & recreation","Google Maps Mobility (% change from baseline), grocery & pharmacy", "Google Maps Mobility (% change from baseline), workplaces", "Google Maps Mobility (% change from baseline), transit stations", "Google Maps Mobility (% change from baseline), residential", "Google Maps Mobility (% change from baseline), parks", "OpenTable Reservations (year-on-year % change)", "Transit App Usage (year-on-year % change)"),
                                            selected = c("Google Maps Mobility (% change from baseline), retail & recreation"),
                                            multiple = FALSE), 
                                
                                "Select mapping date:",
                                
                                sliderInput("plot_date_impact2",
                                            label = "",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value = as.Date(current_date),
                                            width = "100%",
                                            timeFormat = "%d %b", 
                                            animate=animationOptions(interval = 500, loop = FALSE)),
                                "The graph on the right attempts to illustrate the impact of COVID-19 on people's mobility as a result of social distancing measures."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Compare",plotlyOutput("province_plot_cumulative_impact1"), plotlyOutput("province_plot_cumulative_impact2")),
                                    tabPanel("Compare (log10)", plotlyOutput("province_plot_cumulative_impact_log1"), plotlyOutput("province_plot_cumulative_impact_log2"), tags$br(), tags$h5("Please note: log transformations cannot be applied to negative percentage changes."))
                                ), tags$br(), tags$br()
                            )
                        ),
                        
                        tags$br(),
                        titlePanel("Consumer Confidence"),
                        tags$br(),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select_impact3", "Select region level:",   
                                            choices = c("Country", "Province"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select_impact3", "Select country/province:",   
                                            choices = c("Canada"), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = "Canada",
                                            multiple = TRUE),
                                
                                pickerInput("outcome_select_impact3", "Select y-axis COVID variable:",   
                                            choices = c("Cases", "Deaths", "Recovered"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                pickerInput("stat_impact3", "Select Bloomberg Nanos Consumer Confidence index:",   
                                            choices = c("Economic Mood Index", "Pocketbook Index", "Expectations Index"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = c("Economic Mood Index"),
                                            multiple = FALSE), 
                                
                                "Select mapping date:",
                                
                                sliderInput("plot_date_impact3",
                                            label = "",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value = as.Date(current_date),
                                            width = "100%",
                                            timeFormat = "%d %b", 
                                            animate=animationOptions(interval = 500, loop = FALSE)),
                                "The graph on the right attempts to illustrate the impact of COVID-19 on consumer confidence.",
                                tags$br(), tags$br(),
                                "The Economic Mood Index is an aggregate of two sub-incides: the Pocketbook Index (personal finances and job security) and the Expectations Index (economic strength and real estate value)."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Compare", plotlyOutput("province_plot_cumulative_impact_cc1"), plotlyOutput("province_plot_cumulative_impact_cc2")),
                                    tabPanel("Compare (log10)", plotlyOutput("province_plot_cumulative_impact_log_cc1"), plotlyOutput("province_plot_cumulative_impact_log_cc2"))
                                )
                            )
                        ),
                        
                        tags$br(),
                        titlePanel("General unemployment"),
                        tags$br(),
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select_impact", "Select region level:",   
                                            choices = c("Country", "Province"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select_impact", "Select country/province:",   
                                            choices = c("Canada"), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = "Canada",
                                            multiple = TRUE),
                                
                                pickerInput("outcome_select_impact", "Select y-axis COVID variable:",   
                                            choices = c("Cases", "Deaths", "Recovered"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                pickerInput("stat_impact", "Select economic impact variable:",   
                                            choices = c("Unemployment (persons, thousands)", "Unemployment (year-on-year % change)", "Layoffs (persons, thousands)"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = c("Unemployment (persons, thousands)"),
                                            multiple = FALSE), 
                                
                                "Select mapping date:",
                                
                                sliderInput("plot_date_impact",
                                            label = "",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value = as.Date(current_date),
                                            width = "100%",
                                            timeFormat = "%d %b", 
                                            animate=animationOptions(interval = 500, loop = FALSE)),
                                "The graph on the right attempts to illustrate the impact of COVID-19 on unemployment."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Compare", plotlyOutput("province_plot_cumulative_impact_top1"), plotlyOutput("province_plot_cumulative_impact_top2")),
                                    tabPanel("Compare (log10)", plotlyOutput("province_plot_cumulative_impact_log_top1"), plotlyOutput("province_plot_cumulative_impact_log_top2"))
                                )
                            )
                        )
               ),
               
               # ------------------------------
               # government response indices panel
               
               tabPanel("Government response",
                        titlePanel("Government response indices"),
                        tags$br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                                pickerInput("level_select_govt", "Select region level:",   
                                            choices = c("Country", "Province"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select_govt", "Select country/province:",   
                                            choices = c("Canada"),
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = "Canada",
                                            multiple = TRUE),
                                
                                pickerInput("outcome_select_govt", "Select y-axis COVID variable:",   
                                            choices = c("Cases", "Deaths", "Recovered"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                pickerInput("stat_govt", "Select government response variable:",   
                                            choices = c("Government response, stringency index", "Government response, stringency index, including spending", "Government response, volume index, total", "Government response, volume index, foreign travel limitations", "Government response, volume index, self-isolation measures", "Government response, volume index, domestic travel limitations/social distancing measures", "Government response, volume index, economic recovery"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = c("Government response, stringency index"),
                                            multiple = FALSE), 
                                
                                "Select mapping date:",
                                
                                sliderInput("plot_date_govt",
                                            label = "",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value = as.Date(current_date),
                                            width = "100%",
                                            timeFormat = "%d %b", 
                                            animate=animationOptions(interval = 500, loop = FALSE)),
                                "The graph on the right attempts to illustrate the effectiveness of government responses to COVID-19.",
                                tags$br(), tags$br(),
                                "Government response measures are constructed based on information from publicly available sources such as news articles and government press releases and briefings.",
                                tags$br(), tags$br(),
                                "Government stringency index is constructed using a similar technique to one described in Hale et al. (2020)."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Compare", plotlyOutput("province_plot_cumulative_govt1"), plotlyOutput("province_plot_cumulative_govt2")), 
                                    tabPanel("Compare (log10)", plotlyOutput("province_plot_cumulative_govt_log1"), plotlyOutput("province_plot_cumulative_govt_log2"))
                                ), tags$br(), tags$br()
                            )
                        )
               ),
               
               # ------------------------------
               # download covid data panel
               
               tabPanel("COVID-19 Data",
                        tags$br(),
                        titlePanel("Country-level data"),
                        numericInput("maxrows", "Rows to show", 15),
                        verbatimTextOutput("rawtable"),
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        tags$br(),
                        titlePanel("Province-level data"),
                        numericInput("maxrows", "Rows to show", 15),
                        verbatimTextOutput("rawtableprov"),
                        downloadButton("downloadCsvprov", "Download as CSV"),tags$br(),tags$br(), tags$br(),
                        "Please note: data above is adapted from publicly-sourced COVID-19 data published by ", tags$a(href="https://github.com/ishaberry/Covid19Canada", 
                                                                           "Epidemiological Data from the COVID-19 Outbreak in Canada."), tags$br(), tags$br()
                        
               ),
               
               # ------------------------------
               # about page panel 
               
               tabPanel("About",
                        tags$div(
                            tags$h3("ABOUT THE DASHBOARD"), 
                            h5(paste0("Last update: ",update)),
                            "This site is updated once daily. The aim of this site is to complement the resources below by providing several interactive features and metrics currently not available elsewhere for Canada.", tags$br(), tags$br(),
                            "The following resources offer the latest numbers of known cases globally:",tags$br(),
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"), tags$br(),
                            tags$a(href="https://vac-lshtm.shinyapps.io/ncov_tracker/", "LSHTM COVID-19 tracker by Edward Parker"), tags$br(),tags$br(),
                            tags$h3("LATEST UPDATES"),
                            tags$b("May 1:"), "Default government response stringency index no longer includes financial measures. Coding on cancellations of public events/limitations on public gatherings changed.", tags$br(), tags$br(),
                            tags$b("May 6:"), "Front page map is now graphed based on a time series data set rather than a one-day snapshot. As a result, animated features on the left-panel will also animate the geographic progression of the virus using the map.", tags$br(), tags$br(),
                            tags$b("May 7:"), "\"General graphs\", \"Dynamics\", and \"Moving average dynamics\" tabs now feature disaggregation at the health region level.", tags$br(), tags$br(),
                            tags$b("May 7:"), "Google Mobility metrics now available on the \"Economic impact\" tab, under \'Mobility\'.", tags$br(), tags$br(),
                            tags$h3("DATA SOURCES"),
                            "Berry, I., Soucy, J.-P. R., Tuite, A., Fisman, D. 14 April 2020.", tags$b("Open access epidemiologic data and an interactive dashboard to monitor the COVID-19 outbreak in Canada."), "CMAJ 192(15):E420. doi:", tags$a(href="https://doi.org/10.1503/cmaj.75262", "https://doi.org/10.1503/cmaj.75262"), tags$br(), tags$br(),
                            "Hemmadi, M., Syed, F., Schwartz, Z.", tags$b("The Logic's COVID-19 layoffs database."), tags$a(href="https://thelogic.co/news/why-axis/130000-and-counting-tracking-covid-19-layoffs-across-canada/", "Link"), tags$br(), tags$br(),
                            "Statistics Canada. ", tags$b("Labour force characteristics by province, monthly, seasonally adjusted."), tags$a(href="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410028703", "Table: 14-10-0287-03"), tags$br(), tags$br(),
                            tags$b("OpenTable's state of the restaurant industry database."), tags$a(href="https://www.opentable.com/state-of-industry", "Link"), tags$br(), tags$br(),
                            tags$b("Transit app's frequency of app opens database."), tags$a(href="https://www.transitapp.com/coronavirus", "Link"), tags$br(), tags$br(),
                            tags$b("Bloomberg Nanos Consumer Confidence Index."), tags$a(href="https://www.nanos.co/dataportal/nanos-bloomberg-tracking-methodology/", "Link"), tags$br(),tags$br(),
                            tags$b("Google COVID-19 Community Mobility Reports."), tags$a(href="https://www.google.com/covid19/mobility/", "Link"), tags$br(),tags$br(),
                            tags$h3("REFERENCES"),
                            "Hale, T., Petherick, A., Phillips, T., Webster, S. 2020.", tags$b("Variation in government responses to COVID-19."), tags$i("Blavatnik School of Government Working Paper Series 2020/031."), tags$a(href="https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker", "Link"), tags$br(), tags$br(),
                            "Verity, R. et al. 2020.", tags$b("Estimates of the severity of coronavirus disease 2019: a model-based analysis."), tags$i("The Lancet: Infectious Diseases."), tags$a(href="https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext", "Link"),
                            tags$br(),tags$br(),tags$h3("AUTHORS"),
                            "Minnie Cui",tags$br(),
                            tags$a(href="mailto:minniehcui@gmail.com", "Email"), tags$br(),tags$br()
                            #tags$h3("GITHUB"),
                            #"Code and data can be found at the project's ", tags$a(href="https://github.com/minnzc/Canada_COVID19", "GitHub page."), tags$br(), tags$br()
                        )
               )
               
    )          
)


###################################################################################################
# UI

server <- function(input, output, session) {
    
    # Front page general counts 
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(input$plot_date),"%d %B %Y")
    })
    
    reactive_db = reactive({
        cv_cases %>% filter(date == input$plot_date)
    })
    
    reactive_db_last24h = reactive({
        cv_cases %>% filter(date == input$plot_date & new_cases>0)
    })
    
    reactive_db_tot = reactive({
        cv_cases_canada %>% filter(date == input$plot_date)
    })
    
    output$reactive_case_count <- renderText({
        paste0(prettyNum(sum(reactive_db_tot()$cases), big.mark=","), " cases")
    })
    
    output$reactive_death_count <- renderText({
        paste0(prettyNum(sum(reactive_db_tot()$deaths), big.mark=","), " deaths")
    })
    
    output$reactive_recovered_count <- renderText({
        paste0(prettyNum(sum(reactive_db_tot()$recovered), big.mark=","), " recovered")
    })
    
    output$reactive_active_count <- renderText({
        paste0(prettyNum(sum(reactive_db_tot()$active), big.mark=","), " active")
    })
    
    # Front page graphs
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            
            addCircleMarkers(data=reactive_db(), lat = ~ lat, lng = ~ lon, weight = 2, radius = ~sqrt(cases+10), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                             label = sprintf("<strong>%s</strong><br/>Confirmed cases: %g<br/>New cases: %g<br/>Deaths: %d<br/>Recovered: %d", reactive_db()$address, reactive_db()$cases, reactive_db()$new_cases, reactive_db()$deaths,reactive_db()$recovered) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#000000"),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data=reactive_db_last24h(), lat = ~ lat, lng = ~ lon, weight = 2, radius = ~sqrt(new_cases+10), 
                             fillOpacity = 0.1, color = new_col, group = "2019-COVID (new)",
                             label = sprintf("<strong>%s</strong><br/>Confirmed cases: %g<br/>New cases: %g<br/>Deaths: %d<br/>Recovered: %d", reactive_db()$address, reactive_db()$cases, reactive_db()$new_cases, reactive_db()$deaths,reactive_db()$recovered) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#000000"),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data=reactive_db(), lat = ~ lat, lng = ~ lon, weight = 3, radius = ~sqrt(deaths+10), 
                             fillOpacity = 0.1, color = death_col, group = "2019-COVID (deaths)",
                             label = sprintf("<strong>%s</strong><br/>Confirmed cases: %g<br/>New cases: %g<br/>Deaths: %d<br/>Recovered: %d", reactive_db()$address, reactive_db()$cases, reactive_db()$new_cases, reactive_db()$deaths,reactive_db()$recovered) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#000000"),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data=reactive_db(), lat = ~ lat, lng = ~ lon, weight = 3, radius = ~sqrt(recovered+10), 
                             fillOpacity = 0.1, color = recovered_col, group = "2019-COVID (recovered)",
                             label = sprintf("<strong>%s</strong><br/>Confirmed cases: %g<br/>New cases: %g<br/>Deaths: %d<br/>Recovered: %d", reactive_db()$address, reactive_db()$cases, reactive_db()$new_cases, reactive_db()$deaths,reactive_db()$recovered) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#000000"),
                                 textsize = "15px", direction = "auto")) 
    })
    
    output$epi_curve <- renderPlot({
        new_cases_plot(cv_cases_canada, input$plot_date)
    })
    
    output$cumulative_plot <- renderPlot({
        cumulative_cases_plot(cv_cases_canada, input$plot_date)
    })
    
    # ------------------------------
    # Province-specific general plots
    
    # create reactive dataframe for health regions data
    reactive_region_db = reactive({
        cv_cases_region %>% filter(province == input$region_select)
    })
    
    # update region selections
    observeEvent(input$level_select, {
        if (input$level_select=="Country") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = c("Canada"), 
                              selected = "Canada")
        }
        
        if (input$level_select=="Province") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland & Labrador", "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), 
                              selected = c( "Ontario", "Quebec", "British Columbia"))
        }
        
        if (input$level_select=="Health region") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = unique(as.character(cv_cases_region[order(cv_cases_region$province),]$healthregion)), 
                              selected = c("Toronto, Ontario", "Montréal, Quebec", "Vancouver Coastal, British Columbia"))
        }
        
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db = reactive({
        if (input$level_select=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$level_select=="Health region") { 
            db = cv_cases_region
            db$region = db$healthregion
        }
        
        if (input$outcome_select=="Cases") { 
            db$outcome = db$cases/1000
            db$new_outcome = db$new_cases
        }
        
        if (input$outcome_select=="Deaths") { 
            db$outcome = db$deaths/1000 
            db$new_outcome = db$new_deaths 
        }
        
        if (input$outcome_select=="Recovered") { 
            db$outcome = db$recovered/1000
            db$new_outcome = db$new_recovered
        }
        
        db %>% filter(region %in% input$region_select)
    })
    
    output$province_plot_new <- renderPlotly({
        province_plot_new(country_reactive_db(), input$plot_date_region, ylabel = input$outcome_select)
    })
    
    output$province_plot_cumulative <- renderPlotly({
        province_plot_cumulative(country_reactive_db(), input$plot_date_region, ylabel = input$outcome_select)
    })
    
    output$province_plot_cumulative_log <- renderPlotly({
        province_plot_cumulative_log(country_reactive_db(), input$plot_date_region, ylabel = input$outcome_select)
    })
    
    # ------------------------------
    # Provincial-specific dynamics graphs
    # update region selections
    observeEvent(input$level_select_dynamics, {
        if (input$level_select_dynamics=="Country") {
            updatePickerInput(session = session, inputId = "region_select_dynamics", 
                              choices = c("Canada"), selected = "Canada")
        }
        
        if (input$level_select_dynamics=="Province") {
            updatePickerInput(session = session, inputId = "region_select_dynamics", 
                              choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland & Labrador", "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
        
        if (input$level_select_dynamics=="Health region") {
            updatePickerInput(session = session, inputId = "region_select_dynamics", 
                              choices = unique(as.character(cv_cases_region[order(cv_cases_region$province),]$healthregion)), 
                              selected = c("Toronto, Ontario", "Montréal, Quebec", "Vancouver Coastal, British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db_dynamics = reactive({
        if (input$level_select_dynamics=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_dynamics=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$level_select_dynamics=="Health region") { 
            db = cv_cases_region
            db$region = db$healthregion
        }
        
        if (input$outcome_select_dynamics=="Deaths" & input$stat=="18-day") { 
            db$outcome = db$deathactive18
        }
        
        if (input$outcome_select_dynamics=="Deaths" & input$stat=="14-day") { 
            db$outcome = db$deathactive14
        }
        
        if (input$outcome_select_dynamics=="Recovered" & input$stat=="18-day") { 
            db$outcome = db$recoveredactive18
        }
        
        if (input$outcome_select_dynamics=="Recovered" & input$stat=="14-day") { 
            db$outcome = db$recoveredactive14
        }
        
        if (input$outcome_select_dynamics=="Deaths & Recovered" & input$stat=="18-day") { 
            db$outcome = db$deathrecoveredactive18
        }
        
        if (input$outcome_select_dynamics=="Deaths & Recovered" & input$stat=="14-day") { 
            db$outcome = db$deathrecoveredactive14
        }
        
        db %>% filter(region %in% input$region_select_dynamics)
    })
    
    output$ratio_plot <- renderPlotly({
        ratio_plot(country_reactive_db_dynamics(), input$plot_date_dynamics, lag=input$stat)
    })
    
    country_reactive_db_dynamics_scatter = reactive({
        if (input$level_select_dynamics=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_dynamics=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$level_select_dynamics=="Health region") { 
            db = cv_cases_region
            db$region = db$healthregion
        }
        
        if (input$outcome_select_dynamics=="Deaths") { 
            db$outcome = db$deaths
        }
        
        if (input$outcome_select_dynamics=="Recovered") { 
            db$outcome = db$recovered
        }
        
        if (input$outcome_select_dynamics=="Deaths & Recovered") { 
            db$outcome = db$deathrecovered
        }
        
        db %>% filter(region %in% input$region_select_dynamics)
    })
    
    output$scatter_plot <- renderPlotly({
        scatter_plot(country_reactive_db_dynamics_scatter(), input$plot_date_dynamics, lag=input$stat)
    })
    
    # ------------------------------
    # Provincial-specific moving-average dynamics graphs
    # update region selections
    observeEvent(input$level_select_mv, {
        if (input$level_select_mv=="Country") {
            updatePickerInput(session = session, inputId = "region_select_mv", 
                              choices = c("Canada"), selected = "Canada")
        }
        
        if (input$level_select_mv=="Province") {
            updatePickerInput(session = session, inputId = "region_select_mv", 
                              choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland & Labrador", "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
        
        if (input$level_select_mv=="Health region") {
            updatePickerInput(session = session, inputId = "region_select_mv", 
                              choices = unique(as.character(cv_cases_region[order(cv_cases_region$province),]$healthregion)), 
                              selected = c("Toronto, Ontario", "Montréal, Quebec", "Vancouver Coastal, British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db_mv = reactive({
        if (input$level_select_mv=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_mv=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$level_select_mv=="Health region") { 
            db = cv_cases_region
            db$region = db$healthregion
        }
        
        if (input$outcome_select_mv=="Deaths" & input$stat_mv=="18-day") { 
            db$outcome = db$deathactive18mv7
        }
        
        if (input$outcome_select_mv=="Deaths" & input$stat_mv=="14-day") { 
            db$outcome = db$deathactive14mv7
        }
        
        if (input$outcome_select_mv=="Recovered" & input$stat_mv=="18-day") { 
            db$outcome = db$recoveredactive18mv7
        }
        
        if (input$outcome_select_mv=="Recovered" & input$stat_mv=="14-day") { 
            db$outcome = db$recoveredactive14mv7
        }
        
        if (input$outcome_select_mv=="Deaths & Recovered" & input$stat_mv=="18-day") { 
            db$outcome = db$deathrecoveredactive18mv7
        }
        
        if (input$outcome_select_mv=="Deaths & Recovered" & input$stat_mv=="14-day") { 
            db$outcome = db$deathrecoveredactive14mv7
        }
        
        db %>% filter(region %in% input$region_select_mv)
    })
    
    output$ratio_plot_mv <- renderPlotly({
        ratio_plot(country_reactive_db_mv(), input$plot_date_mv, lag=input$stat_mv)
    })
    
    country_reactive_db_mv_scatter = reactive({
        if (input$level_select_mv=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_mv=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$level_select_mv=="Health region") { 
            db = cv_cases_region
            db$region = db$healthregion
        }
        
        if (input$outcome_select_mv=="Deaths") { 
            db$outcome = db$deaths
        }
        
        if (input$outcome_select_mv=="Recovered") { 
            db$outcome = db$recovered
        }
        
        if (input$outcome_select_mv=="Deaths & Recovered") { 
            db$outcome = db$deathrecovered
        }
        
        db %>% filter(region %in% input$region_select_mv)
    })
    
    output$scatter_plot_mv <- renderPlotly({
        scatter_plot(country_reactive_db_mv_scatter(), input$plot_date_mv, lag=input$stat_mv)
    })
    
    # ------------------------------
    # Provincial economic impact graphs
    # update region selections
    observeEvent(input$level_select_impact3, {
        if (input$level_select_impact3=="Country") {
            updatePickerInput(session = session, inputId = "region_select_impact3", 
                              choices = c("Canada"), selected = "Canada")
        }
        
        if (input$level_select_impact3=="Province") {
            updatePickerInput(session = session, inputId = "region_select_impact3", 
                              choices = c("British Columbia", "Ontario", "Quebec"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db_impact3 = reactive({
        if (input$level_select_impact3=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_impact3=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$outcome_select_impact3=="Cases") { 
            db$outcome1 = db$cases/1000
        }
        
        if (input$outcome_select_impact3=="Deaths") { 
            db$outcome1 = db$deaths/1000
        }
        
        if (input$outcome_select_impact3=="Recovered") { 
            db$outcome1 = db$recovered/1000
        }
        
        if (input$stat_impact3=="Economic Mood Index") { 
            db$outcome2 = db$bnccindx
        }
        
        if (input$stat_impact3=="Pocketbook Index") { 
            db$outcome2 = db$bnccpbk
        }
        
        if (input$stat_impact3=="Expectations Index") { 
            db$outcome2 = db$bnccexp
        }
        
        db %>% filter(region %in% input$region_select_impact3)
    })
    
    output$province_plot_cumulative_impact_cc1 <- renderPlotly({
        province_plot_cumulative_impact1(country_reactive_db_impact3(), input$plot_date_impact3, covid = input$outcome_select_impact3, eimpact=input$stat_impact3)
    })
    
    output$province_plot_cumulative_impact_cc2 <- renderPlotly({
        province_plot_cumulative_impact2(country_reactive_db_impact3(), input$plot_date_impact3, covid = input$outcome_select_impact3, eimpact=input$stat_impact3)
    })
    
    output$province_plot_cumulative_impact_log_cc1 <- renderPlotly({
        province_plot_cumulative_impact_log1 (country_reactive_db_impact3(), input$plot_date_impact3, covid = input$outcome_select_impact3, eimpact=input$stat_impact3)
    })
    
    output$province_plot_cumulative_impact_log_cc2 <- renderPlotly({
        province_plot_cumulative_impact_log2 (country_reactive_db_impact3(), input$plot_date_impact3, covid = input$outcome_select_impact3, eimpact=input$stat_impact3)
    })
    
    # update region selections
    observeEvent(input$level_select_impact, {
        if (input$level_select_impact=="Country") {
            updatePickerInput(session = session, inputId = "region_select_impact", 
                              choices = c("Canada"), selected = "Canada")
        }
        
        if (input$level_select_impact=="Province") {
            updatePickerInput(session = session, inputId = "region_select_impact", 
                              choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland & Labrador", "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    observeEvent(input$stat_impact, {
        if (input$level_select_impact=="Country" & (input$stat_impact=="Unemployment (persons)" | input$stat_impact=="Unemployment (year-on-year % change)")) {
            updatePickerInput(session = session, inputId = "region_select_impact", 
                              choices = c("Canada"), selected = "Canada")
        }
        
        if (input$level_select_impact=="Province" & (input$stat_impact=="Unemployment (persons)" | input$stat_impact=="Unemployment (year-on-year % change)")) {
            updatePickerInput(session = session, inputId = "region_select_impact", 
                              choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland & Labrador", "New Brunswick", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db_impact = reactive({
        if (input$level_select_impact=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_impact=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$outcome_select_impact=="Cases") { 
            db$outcome1 = db$cases/1000
        }
        
        if (input$outcome_select_impact=="Deaths") { 
            db$outcome1 = db$deaths/1000
        }
        
        if (input$outcome_select_impact=="Recovered") { 
            db$outcome1 = db$recovered/1000
        }
        
        if (input$stat_impact=="Layoffs (persons, thousands)") { 
            db$outcome2 = db$layoffs/1000
        }
        
        if (input$stat_impact=="Unemployment (persons, thousands)") { 
            db$outcome2 = db$unemployment
        }
        
        if (input$stat_impact=="Unemployment (year-on-year % change)") { 
            db$outcome2 = db$unemployment_yoy
        }
        
        db %>% filter(region %in% input$region_select_impact)
    })
    
    output$province_plot_cumulative_impact_top1 <- renderPlotly({
        province_plot_cumulative_impact1(country_reactive_db_impact(), input$plot_date_impact, covid = input$outcome_select_impact, eimpact=input$stat_impact)
    })
    
    output$province_plot_cumulative_impact_top2 <- renderPlotly({
        province_plot_cumulative_impact2(country_reactive_db_impact(), input$plot_date_impact, covid = input$outcome_select_impact, eimpact=input$stat_impact)
    })
    
    output$province_plot_cumulative_impact_log_top1 <- renderPlotly({
        province_plot_cumulative_impact_log1 (country_reactive_db_impact(), input$plot_date_impact, covid = input$outcome_select_impact, eimpact=input$stat_impact)
    })
    
    output$province_plot_cumulative_impact_log_top2 <- renderPlotly({
        province_plot_cumulative_impact_log2 (country_reactive_db_impact(), input$plot_date_impact, covid = input$outcome_select_impact, eimpact=input$stat_impact)
    })
    
    # update region selections
    observeEvent(input$level_select_impact2, {
        if (input$level_select_impact2=="Country") {
            updatePickerInput(session = session, inputId = "region_select_impact2", 
                              choices = c("Canada"), selected = "Canada")
        }
        
        if (input$level_select_impact2=="Province") {
            updatePickerInput(session = session, inputId = "region_select_impact2", 
                              choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland & Labrador", "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    # update regions for variables
    observeEvent(input$stat_impact2, {
        if (input$stat_impact2=="OpenTable Reservations (year-on-year % change)" & input$level_select_impact2=="Province") {
            updatePickerInput(session = session, inputId = "region_select_impact2", 
                              choices = c("Alberta", "Quebec", "Ontario", "British Columbia"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
        
        if (input$stat_impact2=="Transit App Usage (year-on-year % change)" & input$level_select_impact2=="Province") {
            updatePickerInput(session = session, inputId = "region_select_impact2", 
                              choices = c("Alberta", "Quebec", "Ontario", "British Columbia"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    observeEvent(input$level_select_impact2, {
        if (input$stat_impact2=="OpenTable Reservations (year-on-year % change)" & input$level_select_impact2=="Province") {
            updatePickerInput(session = session, inputId = "region_select_impact2", 
                              choices = c("Alberta", "Quebec", "Ontario", "British Columbia"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
        
        if (input$stat_impact2=="Transit App Usage (year-on-year % change)" & input$level_select_impact2=="Province") {
            updatePickerInput(session = session, inputId = "region_select_impact2", 
                              choices = c("Alberta", "Quebec", "Ontario", "British Columbia"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db_impact2 = reactive({
        if (input$level_select_impact2=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_impact2=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$outcome_select_impact2=="Cases") { 
            db$outcome1 = db$cases/1000
        }
        
        if (input$outcome_select_impact2=="Deaths") { 
            db$outcome1 = db$deaths/1000
        }
        
        if (input$outcome_select_impact2=="Recovered") { 
            db$outcome1 = db$recovered/1000
        }
        
        if (input$stat_impact2=="Google Maps Mobility (% change from baseline), retail & recreation") { 
            db$outcome2 = db$retail_and_recreation_percent_ch
        }
        
        if (input$stat_impact2=="Google Maps Mobility (% change from baseline), grocery & pharmacy") { 
            db$outcome2 = db$grocery_and_pharmacy_percent_cha
        }
        	
    	if (input$stat_impact2=="Google Maps Mobility (% change from baseline), workplaces") { 
    	    db$outcome2 = db$workplaces_percent_change_from_b
    	}
    	
    	if (input$stat_impact2=="Google Maps Mobility (% change from baseline), transit stations") { 
    	    db$outcome2 = db$transit_stations_percent_change_
    	}
    	
    	if (input$stat_impact2=="Google Maps Mobility (% change from baseline), residential") { 
    	    db$outcome2 = db$residential_percent_change_from_
    	}
    	
    	if (input$stat_impact2=="Google Maps Mobility (% change from baseline), parks") { 
    	    db$outcome2 = db$parks_percent_change_from_baseli
    	}
        
        if (input$stat_impact2=="OpenTable Reservations (year-on-year % change)") { 
            db$outcome2 = db$reservations
        }
        
        if (input$stat_impact2=="Transit App Usage (year-on-year % change)") { 
            db$outcome2 = db$ridership
        }
        
        db %>% filter(region %in% input$region_select_impact2)
    })
    
    output$province_plot_cumulative_impact1 <- renderPlotly({
        province_plot_cumulative_impact1(country_reactive_db_impact2(), input$plot_date_impact2, covid = input$outcome_select_impact2, eimpact=input$stat_impact2)
    })
    
    output$province_plot_cumulative_impact2 <- renderPlotly({
        province_plot_cumulative_impact2(country_reactive_db_impact2(), input$plot_date_impact2, covid = input$outcome_select_impact2, eimpact=input$stat_impact2)
    })
    
    output$province_plot_cumulative_impact_log1 <- renderPlotly({
        province_plot_cumulative_impact_log1 (country_reactive_db_impact2(), input$plot_date_impact2, covid = input$outcome_select_impact2, eimpact=input$stat_impact2)
    })
    
    output$province_plot_cumulative_impact_log2 <- renderPlotly({
        province_plot_cumulative_impact_log2 (country_reactive_db_impact2(), input$plot_date_impact2, covid = input$outcome_select_impact2, eimpact=input$stat_impact2)
    })
    
    # ------------------------------
    # Government response plots
    # update region selections
    observeEvent(input$level_select_govt, {
        if (input$level_select_govt=="Country") {
            updatePickerInput(session = session, inputId = "region_select_govt", 
                              choices = c("Canada"), selected = "Canada")
        }
        
        if (input$level_select_govt=="Province") {
            updatePickerInput(session = session, inputId = "region_select_govt", 
                              choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland & Labrador", "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), 
                              selected = c("Ontario", "Quebec", "British Columbia"))
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db_govt = reactive({
        if (input$level_select_govt=="Country") { 
            db = cv_cases_canada
            db$region = db$country
        }
        
        if (input$level_select_govt=="Province") { 
            db = cv_cases_province
            db$region = db$province
        }
        
        if (input$outcome_select_govt=="Cases") { 
            db$outcome1 = db$cases/1000
        }
        
        if (input$outcome_select_govt=="Deaths") { 
            db$outcome1 = db$deaths/1000
        }
        
        if (input$outcome_select_govt=="Recovered") { 
            db$outcome1 = db$recovered/1000
        }
        
        if (input$stat_govt=="Government response, stringency index") { 
            db$outcome2 = db$index_stringencynospend
        }
        
        if (input$stat_govt=="Government response, stringency index, including spending") { 
            db$outcome2 = db$index_stringency
        }
        
        if (input$stat_govt=="Government response, volume index, total") { 
            db$outcome2 = db$index_total
        }
        
        if (input$stat_govt=="Government response, volume index, foreign travel limitations") { 
            db$outcome2 = db$index_limittravel
        }
        
        if (input$stat_govt=="Government response, volume index, self-isolation measures") { 
            db$outcome2 = db$index_selfisolate
        }
        
        if (input$stat_govt=="Government response, volume index, domestic travel limitations/social distancing measures") { 
            db$outcome2 = db$index_limitsocialdomest
        }
        
        if (input$stat_govt=="Government response, volume index, economic recovery") { 
            db$outcome2 = db$index_econrelief
        }
        
        db %>% filter(region %in% input$region_select_govt)
    })
    
    output$province_plot_cumulative_govt1 <- renderPlotly({
        province_plot_cumulative_impact1(country_reactive_db_govt(), input$plot_date_govt, covid = input$outcome_select_govt, eimpact=input$stat_govt)
    })
    
    output$province_plot_cumulative_govt2 <- renderPlotly({
        province_plot_cumulative_impact2(country_reactive_db_govt(), input$plot_date_govt, covid = input$outcome_select_govt, eimpact=input$stat_govt)
    })
    
    output$province_plot_cumulative_govt_log1 <- renderPlotly({
        province_plot_cumulative_impact_log1 (country_reactive_db_govt(), input$plot_date_govt, covid = input$outcome_select_govt, eimpact=input$stat_govt)
    })
    
    output$province_plot_cumulative_govt_log2 <- renderPlotly({
        province_plot_cumulative_impact_log2 (country_reactive_db_govt(), input$plot_date_govt, covid = input$outcome_select_govt, eimpact=input$stat_govt)
    })
    
    # ------------------------------
    # COVID data download 
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_data_", current_date, ".csv", sep="")
        },
        content = function(file) {
            write.csv(cv_cases_canada %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                            recovered, new_recovered, active)), file)
        }
    )
    
    output$rawtable <- renderPrint({
        orig <- options(width = 1000)
        print(tail(cv_cases_canada %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                         recovered, new_recovered, active)), input$maxrows), row.names = FALSE)
        options(orig)
    })
    
    output$downloadCsvprov <- downloadHandler(
        filename = function() {
            paste("COVID_data_", current_date, ".csv", sep="")
        },
        content = function(file) {
            write.csv(cv_cases_province %>% select(c(province, date, cases, new_cases, deaths, new_deaths,
                                                   recovered, new_recovered, active)), file)
        }
    )
    
    output$rawtableprov <- renderPrint({
        orig <- options(width = 1000)
        print(tail(cv_cases_province %>% select(c(province, date, cases, new_cases, deaths, new_deaths,
                                                recovered, new_recovered, active)), input$maxrows), row.names = FALSE)
        options(orig)
    })
    
}

###################################################################################################
# FINAL RUN
# Run the application 
shinyApp(ui = ui, server = server)