*This do file cleans Ontario COVID data for importing into the dashboard
*Written by:   Minnie Cui
*Created on:   14 April 2020
*Last updated: 8 May 2020

********************************************************************************

*Set file location and file name
global MAIN "C:/Users/minni/OneDrive/Documents/R/shiny/nCoV_Canada_tracker/input_data"
cd "$MAIN"

*Set file name for Ontario cases by location data
global DATA "Public_COVID-19_Canada.xlsx"

*Set file name for total cases and status data to be geocoded by region
global OUTPUT1 "covid_location.csv"
global OUTPUT1_1 "covid_region.csv"

*Set file name for provincial total cases and status data
global OUTPUT2 "covid_province.csv"

*Set file name for national total cases and status data
global OUTPUT3 "covid_canada.csv"

*Set file name for other data sets file names
global JOBS "COVID-19 Job Losses.xlsx"
global FOOD "Year-Over-Year_Percent_Change_in_Restaurant_Reserv_data.csv"
global UNEM "Unemployment.xlsx"
global TRAN "Estimated_Ridership_on_Public_Transit_data.csv"
global GRNO "Govt_Response.xlsx"
global GRST "Govt_Stringency.xlsx"
global CONF "Economic_Mood_Index_data.csv"
global CONP "Pocketbook_index_data.csv"
global CONE "Expectations_Index_data.csv"
global GOOG "Google_Mobility.csv"

********************************************************************************
************************** CLEAN GOOGLE MOBILITY DATA **************************
********************************************************************************
*CREATE PROVINCIAL AND NATIONAL DATASET

*Save region variable to drop name
local province_region country
local canada_region province

*Save condition for keeping observations
local province_condition "!missing(province)"
local canada_condition "missing(province)"

*Use loop to clean both data sets
foreach c in province canada {

*Load data
import delimited "$MAIN/$GOOG", bindq(strict) encoding("UTF-8") varnames(1) clear

*Keep only necessary variables and observations
rename country_region country
rename sub_region_1 province
ds country_region_code sub_region_2
drop `r(varlist)'
keep if country=="Canada"
replace province = "Newfoundland & Labrador" if province == "Newfoundland and Labrador"

*Keep different observations for provincial and national datasets
keep if ``c'_condition'
drop ``c'_region'

*Save for merging later
save "$MAIN/goog_`c'", replace
}

********************************************************************************
****************** CLEAN GOVERNMENT RESPONSE VOLUME DATA ***********************
********************************************************************************
*CREATE PROVINCIAL AND NATIONAL DATASET

*Save Excel sheet names
local province_sheet "Province"
local canada_sheet "Canada"

*Save region variable names
local province_region province
local canada_region country

*Use loop to clean both datasets
foreach c in province canada {

*Load data
import excel "$MAIN/$GRNO", sheet(``c'_sheet') first clear

*Generate date string variable for R
rename date Date
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)
tostring dd mm yy, replace
gen date = ""
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Select volume variables
ds socialdistance*, not(type string) 
egen socialdistance_total = rowtotal(`r(varlist)')

*Generate indicies using total numbers
foreach v in limittravel selfisolate limitsocialdomest econrelief total {
gen index_`v' = .
sum socialdistance_`v'
local max = `r(max)'
replace index_`v' = socialdistance_`v'/`max'*100
}

*Keep relevant variables
keep date ``c'_region' index*
order date ``c'_region' index*

*Save for merging later
save "$MAIN/grindex_`c'", replace
}

********************************************************************************
**************** CLEAN GOVERNMENT RESPONSE STRINGENCY DATA *********************
********************************************************************************

*CREATE PROVINCEDATASET

*Load data
import excel "$MAIN/$GRST", sheet("Province") first clear

*Change units of spending variables to billions
foreach v in s10 s12 s13 {
replace `v' = `v'/1000
}

*Localize variable max levels for index
local s1 = 3
local s2 = 3
local s3 = 4
local s4 = 3
local s5 = 2
local s6 = 3
local s7 = 4
local s8 = 3
local s9 = 3
sum s10
local s10 = `r(max)'
sum s11
replace s11 = `r(max)' - s11 
sum s11
local s11 = `r(max)'
sum s12
local s12 = `r(max)'
sum s13
local s13 = `r(max)'
local s14 = 3
local s15 = 2

*Generate total index
ds s*, not(type string) 
local VLIST `r(varlist)'

foreach v in `VLIST' {
replace `v' = `v'/``v''*100
}

egen index_stringency = rowmean(`VLIST')

*Generate index excluding spending
ds date province s10 s12 s13 index_stringency, not 
local VLIST `r(varlist)'
egen index_stringencynospend = rowmean(`VLIST')

*Generate date string variable for R
rename date Date
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
gen date = ""
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Keep relevant variables
keep date province index*
order date province index*

*Save for merging later
*export excel "$MAIN/stringency_province.xlsx", first(variables) replace
save "$MAIN/grstindex_province", replace

*************************
*CREATE CANADA-WIDE DATASET

*Load data
import excel "$MAIN/$GRST", sheet("Canada") first clear

*Change units of spending variables to billions
foreach v in s10 s12 s13 {
replace `v' = `v'/1000
}

*Localize variable max levels for index
local s1 = 3
local s2 = 3
local s3 = 4
local s4 = 3
local s5 = 2
local s6 = 3
local s7 = 4
local s8 = 3
local s9 = 3
sum s10
local s10 = `r(max)'
sum s11
replace s11 = `r(max)' - s11 
sum s11
local s11 = `r(max)'
sum s12
local s12 = `r(max)'
sum s13
local s13 = `r(max)'
local s14 = 3
local s15 = 2

*Generate total index
ds s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s15 
local VLIST `r(varlist)'

foreach v in `VLIST' {
replace `v' = `v'/``v''*100
}

egen index_stringency = rowmean(`VLIST')

*Generate index excluding spending
ds date country s10 s12 s13 s1 s14 index_stringency, not 
local VLIST `r(varlist)'
egen index_stringencynospend = rowmean(`VLIST')

*Generate date string variable for R
rename date Date
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
gen date = ""
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Keep relevant variables
keep date country index*
order date country index*

*Save for merging later
*export excel "$MAIN/stringency_canada.xlsx", first(variables) replace
save "$MAIN/grstindex_canada", replace

********************************************************************************
********************** CLEAN CONSUMER CONFIDENCE DATA **************************
********************************************************************************

*Use loop to clean all three data sets using a loop
foreach data in CONF CONP CONE {

*Create provincial data set

*Load data
import delimited "$MAIN/$`data'", bindq(strict) encoding("UTF-8") clear

*Keep only recent data
gen Date = date(date, "MDY")
format Date %td
gen yy = year(Date)
gen mm = month(Date)
gen dd = day(Date)
sum Date if yy == 2020 & mm == 1 & dd == 24
keep if Date > `r(mean)'

*Keep only provincial data
drop if category == "Canada"

*Clean dates
tostring yy mm dd, replace
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Rename variables
rename category province
ds bncc*
local var `r(varlist)'
keep date province `var'

*Save for merging later
save "$MAIN/`var'_province", replace

***************
*Create national data set

*Load data
import delimited "$MAIN/$`data'", bindq(strict) encoding("UTF-8") clear

*Keep only recent data
gen Date = date(date, "MDY")
format Date %td
gen yy = year(Date)
gen mm = month(Date)
gen dd = day(Date)
sum Date if yy == 2020 & mm == 1 & dd == 24
keep if Date > `r(mean)'

*Keep only national data
keep if category == "Canada"

*Clean dates
tostring yy mm dd, replace
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Rename variables
rename category country
ds bncc*
local var `r(varlist)'
keep date country `var'

*Save for merging later
save "$MAIN/`var'_canada", replace
}

********************************************************************************
*********************** CLEAN PUBLIC JOB LOSSES DATA ***************************
********************************************************************************

*CREATE PROVINCIAL DATASET

*Load data
import excel "$MAIN/$JOBS", sheet("Confirmed") first clear

*Clean provinces data
gen province = Geography
gen ncommas = length(Geography) - length(subinstr(Geography, ",", "", .))
expand = ncommas + 1 if ncommas > 0, gen(expand)
sort Date Company Layoff province expand
gen commaspos = strpos(province, ",") if ncommas > 0 & expand == 0
replace commaspos = ustrpos(province, ",", commaspos[_n-1] + 1) if ncommas > 0 & expand == 1
replace province = substr(province, 1, commaspos-1) if ncommas > 0 & expand == 0 & commaspos != 0
replace province = substr(province, commaspos[_n-1] + 2, commaspos-commaspos[_n-1]-2) if ncommas > 0 & expand == 1 & commaspos != 0
replace province = substr(province, commaspos[_n-1] + 2, .) if ncommas > 0 & commaspos == 0
replace province = strtrim(province)

*Relabel province variable
replace province = "Alberta" if province == "AB"
replace province = "British Columbia" if province == "BC"
replace province = "Manitoba" if province == "MB"
replace province = "New Brunswick" if province == "NB"
replace province = "Newfoundland & Labrador" if province == "NFL"
replace province = "Nova Scotia" if province == "NS"
replace province = "Northwest Territories" if province == "NWT"
replace province = "Nunavut" if province == "NU"
replace province = "Ontario" if province == "ON"
replace province = "Prince Edward Island" if province == "PEI"
replace province = "Quebec" if province == "QC"
replace province = "Saskatchewan" if province == "SK"
replace province = "Yukon" if province == "YK"

*Generate date string variable for R
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
gen date = ""
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Collapse to province level data
rename Layoff new_layoffs
collapse(sum) new_layoffs, by(province date)
gen layoffs = new_layoffs
replace layoffs = layoffs + layoffs[_n-1] if _n != 1

*Drop Canada-wide layoffs because cannot identify in which provinces it's occurring in (these numbers are included in the Canada-wide series generated below)
drop if province == "CA"

*Export provincial data
*export delimited "$MAIN/$OUTPUT2", replace
save "$MAIN/layoffs_province", replace
clear

*************************
*CREATE CANADA-WIDE DATASET

*Load data
import excel "$MAIN/$JOBS", sheet("Confirmed") first clear

*Rename variables
rename Geography country
replace country = "Canada"
rename Layoff new_layoffs

*Generate date string variable for R
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
gen date = ""
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Collapse to country level data
collapse(sum) new_layoffs, by(country date)
gen layoffs = new_layoffs
replace layoffs = layoffs + layoffs[_n-1] if _n != 1

*Export national data
*export delimited "$MAIN/$OUTPUT3", replace
save "$MAIN/layoffs_canada", replace
clear

********************************************************************************
********************* CLEAN STATSCAN UNEMPLOYMENT DATA *************************
********************************************************************************

*CREATE PROVINCIAL AND NATIONAL DATASET

*Save sheet names
local province_sheet "Province"
local canada_sheet "Canada"

*Use loop to create provincial and national data
foreach v in province canada {

*Load data
import excel "$MAIN/$UNEM", sheet("``v'_sheet'") first clear

*Generate date string variable for R
rename date Date
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
gen date = ""
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1
drop Date dd mm yy

*Export national data
save "$MAIN/unem_`v'", replace
clear
}

********************************************************************************
******************** CLEAN RESTUARANT RESERVATIONS DATA ************************
********************************************************************************
*CREATE PROVINCIAL AND NATIONAL DATASET

*Save percentage change variable names
local province_percent percentchangeyoy
local canada_percent countrypercentchangeyoy 

*Save region variable names
local province_region province
local canada_region country

*Use loop to clean both data sets
foreach v in province canada {

*Load data
import delimited "$MAIN/$FOOD", bindq(strict) encoding("UTF-8") clear

*Date variable
gen Date = date(date, "MDY")
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Code provinces
gen province = ""
replace province = "Ontario" if city == "Toronto"
replace province = "Alberta" if city == "Calgary"
replace province = "Alberta" if city == "Edmonton"
replace province = "Quebec" if city == "Montréal"
replace province = "British Columbia" if city == "Vancouver"

*Collapse to province level data
rename ``v'_percent' reservations
collapse(mean) reservations, by(``v'_region' date)

*Save for merging later
save "$MAIN/reservations_`v'", replace
}

********************************************************************************
*********************** CLEAN TRANSIT RIDERSHIP DATA ***************************
********************************************************************************
*CREATE PROVINCIAL AND NATIONAL DATASET

*Save percentage change variable names
local province_percent percentchangeyoy 
local canada_percent percentchangeyoycountry

*Save region variable names
local province_region province
local canada_region country

*Use loop to clean both data sets
foreach v in province canada {

*Load data
import delimited "$MAIN/$TRAN", bindq(strict) encoding("UTF-8") clear

*Date variable
gen ndash = 1 if (length(date) - length(subinstr(date, "-", "", .))) > 0
gen Date = .
replace Date = date(date, "YDM") if ndash == 1
replace Date = date(date, "MDY") if Date == .
format Date %td
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Code provinces
gen province = ""
replace province = "Ontario" if city == "Toronto"
replace province = "Ontario" if city == "Ottawa"
replace province = "Alberta" if city == "Calgary"
replace province = "Quebec" if city == "Montréal"
replace province = "British Columbia" if city == "Vancouver"

*Collapse to province level data
rename ``v'_percent' ridership
collapse(mean) ridership, by(``v'_region' date)

*Save for merging later
save "$MAIN/ridership_`v'", replace
}

********************************************************************************
****************** GENERATE REGIONAL DATA SET FOR GEO-CODING *******************
********************************************************************************

*CLEAN REGIONAL CASES AND DEATHS DATA

*Save Excel sheet names
local cases_sheet "Cases"
local deaths_sheet "Mortality"

*Save date variable names 
local cases date_report
local deaths date_death_report

*Use loop to clean cases and deaths data
foreach v in cases deaths {

*Load data
import excel "$MAIN/$DATA", sheet("``v'_sheet'") cellra(A4) first clear

*Delete unnecessary observations
rename ``v'' date
drop if date == . 

*Drop repatriated cases
drop if province == "Repatriated"

*Recode province names
replace province = "British Columbia" if province == "BC"
replace province = "Newfoundland & Labrador" if province == "NL"
replace province = "Northwest Territories" if province == "NWT"
replace province = "Prince Edward Island" if province == "PEI"
rename health_region region
replace region = "Not Reported" if region == "NWT" & province == "Northwest Territories"
replace region = "Not Reported" if region == "Yukon" & province == "Yukon"
replace region = "Not Reported" if region == "Nunavut" & province == "Nunavut"

*Generate address variable for geocoding in R
gen address = region + ", " + province
replace address = province + ", Canada" if region == "Not Reported"
replace address = province + ", Canada" if region == "Interior" & province == "British Columbia" //For some reason Google can't find BC Interior geocodes

*Consolidate new cases by province
gen new_`v' = 1
ds address date
collapse(sum) new_`v', by(`r(varlist)')

*Generate cumulative cases variable
encode address, gen(address_code)
tsset address_code date
bysort address_code: egen day_code = rank(date)
gen `v' = new_`v'
replace `v' = `v' + `v'[_n-1] if day_code != 1

*Keep only relevant variables 
keep address date new_`v' `v'

*Save for merging later
save "$MAIN/`v'", replace
}

*************************
*CLEAN REGIONAL RECOVERED DATA

*Load data
import excel "$MAIN/$DATA", sheet("Recovered") cellra(A4) first clear

*Delete unnecessary observations
rename date_recovered date
drop if date == . 

*Drop repatriated cases
drop if province == "Repatriated"

*Recode province names
replace province = "British Columbia" if province == "BC"
replace province = "Newfoundland & Labrador" if province == "NL"
replace province = "Northwest Territories" if province == "NWT"
replace province = "Prince Edward Island" if province == "PEI"
gen region = "Not Reported"

*Generate address variable for geocoding in R
gen address = region + ", " + province
replace address = province + ", Canada" if region == "Not Reported"

*Clean cumulative recoveries variable
rename cumulative_recovered recovered
replace recovered = "" if recovered == "NA"
destring recovered, replace
replace recovered = 0 if recovered == .

*Generate cumulative cases variable
encode address, gen(address_code)
tsset address_code date
bysort address_code: egen day_code = rank(date)
gen new_recovered = .
replace new_recovered = 0 if day_code == 1
replace new_recovered = recovered - recovered[_n-1] if day_code != 1

*Keep only relevant variables 
keep address date new_recovered recovered

*Save for merging later
save "$MAIN/recovered", replace

*************************
*Merge all datasets 
use cases, clear
foreach data in deaths recovered {
merge 1:1 address date using `data', nogen
}

*Fill in missing dates
encode address, gen(address_code)
tsset address_code date
tsfill, full
drop address
decode address_code, gen(address)

*Generate region and province for labelling 
gen region = strtrim(substr(address, 1, strpos(address, ",")-1))
gen province = strtrim(substr(address, strpos(address, ",")+2, .))
replace province = region if province == "Canada"
replace region = "Not Reported" if region == province
gen healthregion = region + ", " + province

*Generate date string variable for R
rename date Date
gen dd = day(Date)
gen mm = month(Date)
gen yy = year(Date)

tostring dd mm yy, replace
gen date = ""
replace date = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace date = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace date = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace date = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Save temporary dataset
save "$MAIN/location_temp", replace

*Fill in missing variables
sort address date
bysort address: gen day_code = _n
foreach v in cases deaths recovered {
	replace `v' = 0 if day_code == 1 & `v' == .
	replace `v' = `v'[_n-1] if day_code != 1 & `v' == .
	replace new_`v' = 0 if day_code == 1 & new_`v' == .
	replace new_`v' = `v' - `v'[_n-1] if day_code != 1 & new_`v' == .
}

*Drop unnecessary variables
drop Date dd mm yy day_code

*Export data
export delimited "$MAIN/$OUTPUT1", replace
clear

*************************
*CLEAN UP FILE DIRECTORY

*Remove intermediate datasets from directory
foreach data in cases deaths recovered {
	rm `data'.dta
}

********************************************************************************
******* GENERATE REGIONAL, PROVINCIAL AND NATIONAL TIME SERIES DATA SETS *******
********************************************************************************
*CREATE REGIONAL DATA SET

*Load temp data set
use "$MAIN/location_temp", clear

*Rename date variables
drop dd mm yy
rename date dateStr
rename Date date

*Clean variables after merging, fill in missing variables
sort address_code date
bysort address: gen day_code = _n
foreach v in cases deaths recovered {
	replace `v' = `v'[_n-1] if day_code != 1 & `v' == .
	replace `v' = 0 if `v' == .
	replace new_`v' = `v' - `v'[_n-1] if day_code != 1 & new_`v' == .
}

*Keep only relevant variables
keep date* day_code province* *region* address* cases new_cases deaths new_deaths recovered new_recovered
order date* day_code province* *region* address* cases new_cases deaths new_deaths recovered new_recovered
gen lcases = log(cases)
gen ldeaths = log(deaths)
gen lrecovered = log(recovered)
gen deathrecovered = deaths + recovered
*gen ldeathrecovered = log(deathrecovered)

*Generate last update
sort date
gen last_update = dateStr[_N]

*Generate active cases
gen active = cases - recovered - deaths

*Set time panel
tsset address_code date

*************************
*GENERATE MOVING AVERAGES SERIES
sort address_code date
foreach v in cases deaths recovered active {
	gen `v'mv7 = (1/7) * (`v' + `v'[_n-1] + `v'[_n-2] + `v'[_n-3] + `v'[_n-4] + `v'[_n-5] + `v'[_n-6]) if day_code > 6
}

*************************
*GENERATE DYNAMICS DATA SET

*Create active cases lagged 14 & 18 days
gen active14 = l14.active
gen active18 = l18.active
gen lactive14 = log(active14)
gen lactive18 = log(active18)

*Generate ratios
gen deathactive14 = deaths/active14 * 100
gen recoveredactive14 = recovered/active14 * 100
gen deathrecoveredactive14 = (deaths + recovered)/active14 * 100
gen deathactive18 = deaths/active18 * 100
gen recoveredactive18 = recovered/active18 * 100
gen deathrecoveredactive18 = (deaths + recovered)/active18 * 100

*Create moving average active cases lagged 14 & 18 days
gen active14mv7 = l14.activemv7
gen active18mv7 = l18.activemv7
gen lactive14mv7 = log(active14mv7)
gen lactive18mv7 = log(active18mv7)

*Generate moving ratios
gen deathactive14mv7 = deathsmv7/active14mv7 * 100
gen recoveredactive14mv7 = recoveredmv7/active14mv7 * 100
gen deathrecoveredactive14mv7 = (deathsmv7 + recoveredmv7)/active14mv7 * 100
gen deathactive18mv7 = deathsmv7/active18mv7 * 100
gen recoveredactive18mv7 = recoveredmv7/active18mv7 * 100
gen deathrecoveredactive18mv7 = (deathsmv7 + recoveredmv7)/active18mv7 * 100

*Drop date variable
drop date day_code address*
rename dateStr date

drop if date > last_update
drop last_update

*Export provincial data
export delimited "$MAIN/$OUTPUT1_1", replace
clear

********************************************************************************
*CREATE PROVINCIAL DATA SET

*CLEAN PROVINCIAL CASES AND DEATHS DATA

*Save Excel sheet names
local cases_sheet "Cases"
local deaths_sheet "Mortality"

*Save date variable names 
local cases date_report
local deaths date_death_report

*Use loop to clean both cases and deaths data
foreach v in cases deaths {

*Load data
import excel "$MAIN/$DATA", sheet("``v'_sheet'") cellra(A4) first clear

*Delete unnecessary observations
rename ``v'' date
drop if date == . 

*Drop repatriated cases
drop if province == "Repatriated"

*Recode province names
replace province = "British Columbia" if province == "BC"
replace province = "Newfoundland & Labrador" if province == "NL"
replace province = "Northwest Territories" if province == "NWT"
replace province = "Prince Edward Island" if province == "PEI"

*Consolidate new cases by province
gen new_`v' = 1
ds province date
collapse(sum) new_`v', by(`r(varlist)')

*Generate cumulative cases variable
encode province, gen(province_code)
tsset province_code date
bysort province_code: egen day_code = rank(date)
gen `v' = new_`v'
replace `v' = `v' + `v'[_n-1] if day_code != 1

*Keep only relevant variables 
keep province date new_`v' `v'

*Save for merging later
save "$MAIN/`v'", replace
}

*************************
*CLEAN PROVINCIAL RECOVERED DATA

*Load data
import excel "$MAIN/$DATA", sheet("Recovered") cellra(A4) first clear

*Delete unnecessary observations
rename date_recovered date
drop if date == . 

*Drop repatriated cases
drop if province == "Repatriated"

*Recode province names
replace province = "British Columbia" if province == "BC"
replace province = "Newfoundland & Labrador" if province == "NL"
replace province = "Northwest Territories" if province == "NWT"
replace province = "Prince Edward Island" if province == "PEI"

*Clean cumulative recoveries variable
rename cumulative_recovered recovered
replace recovered = "" if recovered == "NA"
destring recovered, replace
replace recovered = 0 if recovered == .

*Generate cumulative cases variable
encode province, gen(province_code)
tsset province_code date
bysort province_code: egen day_code = rank(date)
gen new_recovered = .
replace new_recovered = 0 if day_code == 1
replace new_recovered = recovered - recovered[_n-1] if day_code != 1

*Keep only relevant variables 
keep province date new_recovered recovered

*Save for merging later
save "$MAIN/recovered", replace

*************************
*MERGE ALL CLEANED PROVINCIAL DATA

*Merge all data
use cases, clear
foreach data in deaths recovered {
	merge 1:1 date province using `data', nogen
}

*Regenerate province_code
encode province, gen(province_code)

*Clean variables after merging, fill in missing variables
sort province_code date
bysort province_code: egen day_code = rank(date)
foreach v in cases deaths recovered {
	replace `v' = `v'[_n-1] if day_code != 1 & `v' == .
	replace `v' = 0 if `v' == .
	replace new_`v' = `v' - `v'[_n-1] if day_code != 1 & new_`v' == .
}

*Generate date string variable for R
gen dd = day(date)
gen mm = month(date)
gen yy = year(date)

tostring dd mm yy, replace
gen dateStr = ""
replace dateStr = yy + "-" + mm + "-" + dd if strlen(mm) == 2 & strlen(dd) == 2
replace dateStr = yy + "-" + "0" + mm + "-" + dd if strlen(mm) == 1 & strlen(dd) == 2
replace dateStr = yy + "-" + mm + "-" + "0" + dd if strlen(mm) == 2 & strlen(dd) == 1
replace dateStr = yy + "-" + "0" + mm + "-" + "0" + dd if strlen(mm) == 1 & strlen(dd) == 1

*Keep only relevant variables
keep date* day_code province* cases new_cases deaths new_deaths recovered new_recovered
order date* day_code province* cases new_cases deaths new_deaths recovered new_recovered
gen lcases = log(cases)
gen ldeaths = log(deaths)
gen lrecovered = log(recovered)
gen deathrecovered = deaths + recovered
*gen ldeathrecovered = log(deathrecovered)

*Save intermediate dataset
save "$MAIN/temp", replace

*Generate active cases
gen active = cases - recovered - deaths

*Set time panel
tsset province_code date

*************************
*GENERATE MOVING AVERAGES SERIES
sort province_code date
foreach v in cases deaths recovered active {
	gen `v'mv7 = (1/7) * (`v' + `v'[_n-1] + `v'[_n-2] + `v'[_n-3] + `v'[_n-4] + `v'[_n-5] + `v'[_n-6]) if day_code > 6
}

*************************
*GENERATE DYNAMICS DATA SET

*Create active cases lagged 14 & 18 days
gen active14 = l14.active
gen active18 = l18.active
gen lactive14 = log(active14)
gen lactive18 = log(active18)

*Generate ratios
gen deathactive14 = deaths/active14 * 100
gen recoveredactive14 = recovered/active14 * 100
gen deathrecoveredactive14 = (deaths + recovered)/active14 * 100
gen deathactive18 = deaths/active18 * 100
gen recoveredactive18 = recovered/active18 * 100
gen deathrecoveredactive18 = (deaths + recovered)/active18 * 100

*Create moving average active cases lagged 14 & 18 days
gen active14mv7 = l14.activemv7
gen active18mv7 = l18.activemv7
gen lactive14mv7 = log(active14mv7)
gen lactive18mv7 = log(active18mv7)

*Generate moving ratios
gen deathactive14mv7 = deathsmv7/active14mv7 * 100
gen recoveredactive14mv7 = recoveredmv7/active14mv7 * 100
gen deathrecoveredactive14mv7 = (deathsmv7 + recoveredmv7)/active14mv7 * 100
gen deathactive18mv7 = deathsmv7/active18mv7 * 100
gen recoveredactive18mv7 = recoveredmv7/active18mv7 * 100
gen deathrecoveredactive18mv7 = (deathsmv7 + recoveredmv7)/active18mv7 * 100

*Drop date variable
drop date day_code province_code
rename dateStr date

*Merge with layoffs data
foreach data in layoffs reservations ridership unem grindex grstindex bnccindx bnccexp bnccpbk goog {
	merge 1:1 date province using `data'_province, nogen
}

*Regenerate province_code
encode province, gen(province_code)

*Clean variables after merging, fill in missing variables
sort province_code date
bysort province_code: gen day_code = _n
foreach v in cases deaths recovered {
	replace `v' = `v'[_n-1] if day_code != 1 & `v' == .
	replace new_`v' = `v' - `v'[_n-1] if day_code != 1 & new_`v' == .
}
drop province_code day_code 

*Export provincial data
export delimited "$MAIN/$OUTPUT2", replace
*save "$MAIN/covid_province", replace
clear

********************************************************************************
*CREATE CANADA-WIDE DATA SET

*Load dataset
use "$MAIN/temp", clear

*Create national dataset
gen country = "Canada"

*Aggregate cases to national level
ds cases deaths recovered new*
collapse(sum) `r(varlist)', by(date dateStr country)
gen lcases = log(cases)
gen ldeaths = log(deaths)
gen lrecovered = log(recovered)
gen deathrecovered = deaths + recovered
*gen ldeathrecovered = log(deathrecovered)

*Generate active cases
gen active = cases - recovered - deaths

*Set time panel
tsset date

*************************
*GENERATE MOVING AVERAGES SERIES
egen day_code = rank(date)
sort date
foreach v in cases deaths recovered active {
	gen `v'mv7 = (1/7) * (`v' + `v'[_n-1] + `v'[_n-2] + `v'[_n-3] + `v'[_n-4] + `v'[_n-5] + `v'[_n-6]) if day_code > 6
}

*************************
*GENERATE DYNAMICS DATA SET

*Create active cases lagged 14 & 18 days
gen active14 = l14.active
gen active18 = l18.active
gen lactive14 = log(active14)
gen lactive18 = log(active18)

*Generate ratios
gen deathactive14 = deaths/active14 * 100
gen recoveredactive14 = recovered/active14 * 100
gen deathrecoveredactive14 = (deaths + recovered)/active14 * 100
gen deathactive18 = deaths/active18 * 100
gen recoveredactive18 = recovered/active18 * 100
gen deathrecoveredactive18 = (deaths + recovered)/active18 * 100

*Create moving average active cases lagged 14 & 18 days
gen active14mv7 = l14.activemv7
gen active18mv7 = l18.activemv7
gen lactive14mv7 = log(active14mv7)
gen lactive18mv7 = log(active18mv7)

*Generate moving ratios
gen deathactive14mv7 = deathsmv7/active14mv7 * 100
gen recoveredactive14mv7 = recoveredmv7/active14mv7 * 100
gen deathrecoveredactive14mv7 = (deathsmv7 + recoveredmv7)/active14mv7 * 100
gen deathactive18mv7 = deathsmv7/active18mv7 * 100
gen recoveredactive18mv7 = recoveredmv7/active18mv7 * 100
gen deathrecoveredactive18mv7 = (deathsmv7 + recoveredmv7)/active18mv7 * 100

*Drop date variable
drop date day_code
rename dateStr date

*Merge with layoffs data
foreach data in layoffs reservations ridership unem grindex grstindex bnccindx bnccexp bnccpbk goog {
	merge 1:1 date country using `data'_canada, nogen
}
*Clean variables after merging, fill in missing variables
sort date
foreach v in cases deaths recovered {
	replace `v' = `v'[_n-1] if _n != 1 & `v' == .
	replace new_`v' = `v' - `v'[_n-1] if _n != 1 & new_`v' == .
}

*Export national data
export delimited "$MAIN/$OUTPUT3", replace
*save "$MAIN/covid_canada", replace
clear

*************************
*CLEAN UP FILE DIRECTORY

*Remove intermediate datasets from directory
foreach data in temp location_temp cases deaths recovered layoffs_canada layoffs_province reservations_canada reservations_province unem_province unem_canada ridership_province ridership_canada grindex_province grindex_canada grstindex_province grstindex_canada bnccindx_province bnccindx_canada bnccexp_province bnccexp_canada bnccpbk_province bnccpbk_canada goog_province goog_canada {
	rm `data'.dta
}

