###################################################################################################
## Gather geocodes for a list of regions for Canadian nCoV tracker
## Written by:   Minnie Cui
## Date created: April 14, 2020 
## Last updated: ---
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

# uncomment the code below to set your Google API key
# register_google(key = "[your key]", write = TRUE)

###################################################################################################
# MAIN CODE

# set file location
setwd(directory)

# read in insolvency trustees data
cases <- read.csv(file, encoding = "UTF-8", stringsAsFactors=FALSE)

# set Google API for getting geocodes
geocodes <- mutate_geocode(cases, address)

# write final dataset to file
write.csv(geocodes, output, fileEncoding = "UTF-8")
