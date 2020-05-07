###################################################################################################
## Gather geocodes for a list of regions for Canadian COVID-19 tracker
## Written by:   Minnie Cui
## Date created: April 14, 2020 
## Last updated: May 6, 2020
###################################################################################################
# SET REQUIRED VARIABLES

# set directory containing data
directory = "C:/Users/minni/OneDrive/Documents/R/shiny/nCoV_Canada_tracker/input_data"

# set input data file name
file = "covid_location.csv"

# set output data file name
output = "covid_geocodes.csv"

# load required packages
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")

# uncomment the code below to set your Google API key permanently
# register_google(key = "[your key]", write = TRUE)

# uncomment the code below to set your Google API key only for this session
# register_google(key = "[your key]")

###################################################################################################
# MAIN CODE

# set file location
setwd(directory)

# read in regional data for front page mapping
cases <- read.csv(file, encoding = "UTF-8", stringsAsFactors=FALSE)

# create subset of addresses and remove duplicates
addresses <- cases["address"]
addresses <- unique(addresses)

# set Google API for getting geocodes
geocodes <- mutate_geocode(addresses, address)

# merge geocodes with regional data 
final <- merge(cases, geocodes, by="address")

# write final dataset to file
write.csv(final, output, fileEncoding = "UTF-8")
