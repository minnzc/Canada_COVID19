*This do file cleans Canada COVID data for importing into the dashboard

*Written by:   Minnie Cui
*Created on:   14 April 2020
*Last updated: 9 May 2022

********************************************************************************
************** PLEASE UPDATE VARIABLES BELOW BEFORE RUNNING CODE ***************
********************************************************************************

*Set file location and file name
global MAIN "./input_data"
cd "$MAIN"

*Set file name for total cases and status data to be geocoded by region
global OUTPUT1_1 "covid_location.csv"
global OUTPUT1_2 "covid_region.csv"

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
global POP "Population.csv"

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
	import delimited "$GOOG", bindq(strict) encoding("UTF-8") varn(1) clear

	*Keep only necessary variables and observations
	rename country_region country
	rename sub_region_1 province
	keep if sub_region_2 == ""
	ds country province date *percent*, not
	drop `r(varlist)'
	keep if country=="Canada"
	replace province = "Newfoundland & Labrador" if province == "Newfoundland and Labrador"

	*Keep different observations for provincial and national datasets
	keep if ``c'_condition'
	drop ``c'_region'

	*Save for merging later
	save "goog_`c'", replace
}

*KEEP ONLY CANADIAN DATA IN STORAGE
import delimited "$GOOG", bindq(strict) encoding("UTF-8") varn(1) clear
keep if country_region=="Canada"
export delimited "$GOOG", quote replace

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
	import excel "$GRNO", sheet(``c'_sheet') first clear

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
	save "grindex_`c'", replace
}

********************************************************************************
**************** CLEAN GOVERNMENT RESPONSE STRINGENCY DATA *********************
********************************************************************************

*CREATE PROVINCEDATASET

*Load data
import excel "$GRST", sheet("Province") first clear

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
*export excel "stringency_province.xlsx", first(variables) replace
save "grstindex_province", replace

*************************
*CREATE CANADA-WIDE DATASET

*Load data
import excel "$GRST", sheet("Canada") first clear

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
*export excel "stringency_canada.xlsx", first(variables) replace
save "grstindex_canada", replace

********************************************************************************
********************** CLEAN CONSUMER CONFIDENCE DATA **************************
********************************************************************************

*Use loop to clean all three data sets using a loop
foreach data in CONF CONP CONE {

	*Create provincial data set

	*Load data
	import delimited "$`data'", varn(1) bindq(strict) encoding("UTF-8") clear

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
	save "`var'_province", replace

	***************
	*Create national data set

	*Load data
	import delimited "$`data'", bindq(strict) encoding("UTF-8") clear

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
	save "`var'_canada", replace
}

********************************************************************************
*********************** CLEAN PUBLIC JOB LOSSES DATA ***************************
********************************************************************************

*CREATE PROVINCIAL DATASET

*Load data
import excel "$JOBS", sheet("Confirmed") first clear

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
*export delimited "$OUTPUT2", replace
save "layoffs_province", replace
clear

*************************
*CREATE CANADA-WIDE DATASET

*Load data
import excel "$JOBS", sheet("Confirmed") first clear

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
*export delimited "$OUTPUT3", replace
save "layoffs_canada", replace
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
	import excel "$UNEM", sheet("``v'_sheet'") first clear

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
	save "unem_`v'", replace
}

********************************************************************************
************************** CLEAN POPULATION DATA *******************************
********************************************************************************
*Create provincial data set
*Load data
import delimited "$POP", bindq(strict) encoding("UTF-8") varn(1) clear

*Keep relevant variables
keep province pop
replace province = "British Columbia" if province == "BC"
replace province = "Newfoundland & Labrador" if province == "NL"
replace province = "Northwest Territories" if province == "NWT"
replace province = "Prince Edward Island" if province == "PEI"

*Generate sum by province
collapse(sum) pop, by(province) fast

*Save for merge
save "pop_province", replace

*Generate health region
gen healthregion = "Not Reported, " + province
drop province

*Save for append
save "pop_province_nr", replace

*************************
*Create regional data set
*Load data
import delimited "$POP", bindq(strict) encoding("UTF-8") varn(1) clear

*Recode province names
rename healthregion region
replace province = "British Columbia" if province == "BC"
replace province = "Newfoundland & Labrador" if province == "NL"
replace province = "Northwest Territories" if province == "NWT"
replace province = "Prince Edward Island" if province == "PEI"
drop if region == "NWT" & province == "Northwest Territories"
drop if region == "Yukon" & province == "Yukon"
drop if region == "Nunavut" & province == "Nunavut"
drop if region == "Prince Edward Island" & province == "Prince Edward Island"
drop if region == "Interior" & province == "British Columbia"

*Keep relevant variables
gen healthregion = region + ", " + province
keep healthregion pop

*Append
append using "pop_province_nr"
rm "pop_province_nr.dta"

*Save for merge
save "pop_region", replace

*************************
*Create national data set
*Load data
import delimited "$POP", bindq(strict) encoding("UTF-8") varn(1) clear

*Keep relevant variables
gen country = "Canada"

*Generate sum by country
collapse(sum) pop, by(country) fast

*Save for merge
save "pop_canada", replace

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
	import delimited "$FOOD", varn(1) bindq(strict) encoding("UTF-8") clear

	*Date variable
	gen Date = date(date, "DMY")
	*gen Date = date(date, "MDY")
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
	save "reservations_`v'", replace
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
	import delimited "$TRAN", varn(1) bindq(strict) encoding("UTF-8") clear

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
	save "ridership_`v'", replace
}

********************************************************************************
****************** GENERATE REGIONAL DATA SET FOR GEO-CODING *******************
********************************************************************************

*CLEAN REGIONAL CASES AND DEATHS DATA

*Save health region info
import delimited "health_regions.csv", varn(1) bindq(strict) encoding(utf-8) clear
save healthregion, replace

*Use loop to clean cases and deaths data
foreach v in cases deaths {
	
	*Load data
	import delimited "`v'_timeseries_hr.csv", varn(1) bindq(strict) encoding(utf-8) clear
	
	*Delete unnecessary observations
	rename date Date
	gen date = date(Date, "YMD")
	format date %td
	drop if date == . 

	*Drop repatriated cases
	drop if region == "Repatriated"
	rename region province
	rename sub_region_1 subregion

	*Recode province names
	replace province = "Alberta" if province == "AB"
	replace province = "British Columbia" if province == "BC"
	replace province = "Manitoba" if province == "MB"
	replace province = "New Brunswick" if province == "NB"
	replace province = "Newfoundland & Labrador" if province == "NL"
	replace province = "Nova Scotia" if province == "NS"
	replace province = "Northwest Territories" if province == "NT"
	replace province = "Nunavut" if province == "NU"
	replace province = "Ontario" if province == "ON"
	replace province = "Prince Edward Island" if province == "PE"
	replace province = "Quebec" if province == "QC"
	replace province = "Saskatchewan" if province == "SK"
	replace province = "Yukon" if province == "YT"
	merge m:1 province subregion using "healthregion"
	count if _merge != 3 & subregion == 9999
	replace region = "Not Reported" if subregion == 9999

	*Generate address variable for geocoding in R
	gen address = region + ", " + province
	replace address = province + ", Canada" if region == "Not Reported"
	replace address = province + ", Canada" if region == "Interior" & province == "British Columbia" //For some reason Google can't find BC Interior geocodes
	
	*Keep only relevant variables 
	rename value_daily new_`v'
	rename value `v'
	collapse(sum) `v' new_`v', by(address date)

	*Save for merging later
	save "`v'", replace
}

*************************
*CLEAN REGIONAL RECOVERED DATA

foreach v in recovered {

	*Load data
	import delimited "`v'_timeseries_prov.csv", varn(1) bindq(strict) clear

	*Delete unnecessary observations
	rename date_`v' Date
	gen date = date(Date, "DMY")
	format date %td
	drop if date == .

	*Recode province names
	drop if province == "Repatriated"
	replace province = "British Columbia" if province == "BC"
	replace province = "Newfoundland & Labrador" if province == "NL"
	replace province = "Northwest Territories" if province == "NWT"
	replace province = "Prince Edward Island" if province == "PEI"
	gen region = "Not Reported"

	*Generate address variable for geocoding in R
	gen address = region + ", " + province
	replace address = province + ", Canada" if region == "Not Reported"

	*Clean cumulative variable
	rename `v' new_`v'
	rename cumulative_`v' `v'

	*Keep only relevant variables 
	keep address date new_`v' `v'

	*Save for merging later
	save `v', replace
}

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

*Merge population data
merge m:1 healthregion using pop_region
drop if _merge == 2
drop _merge

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
save "location_temp", replace

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

*Generate per capita variables
gen casespc = cases/pop
gen new_casespc = new_cases/pop
gen deathspc = deaths/pop
gen recoveredpc = recovered/pop

*Export data
export delimited "$OUTPUT1_1", replace
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
use "location_temp", clear

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

*Merge population data
merge m:1 healthregion using pop_region
drop if _merge == 2
drop _merge

*Generate some interesting variables
gen lcases = log(cases)
gen ldeaths = log(deaths)
gen lrecovered = log(recovered)
gen casespc = cases/pop
gen deathspc = deaths/pop
gen recoveredpc = recovered/pop
gen new_casespc = new_cases/pop
gen new_deathspc = new_deaths/pop
gen new_recoveredpc = new_recovered/pop
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
foreach v in new_cases cases new_deaths deaths new_recovered recovered active {
	gen `v'mv7 = (1/7) * (`v' + `v'[_n-1] + `v'[_n-2] + `v'[_n-3] + `v'[_n-4] + `v'[_n-5] + `v'[_n-6]) if day_code > 6
}

*************************
*GENERATE DYNAMICS DATA SET

*Create active cases lagged 14 & 18 days
gen active14 = l14.new_cases
gen active18 = l18.new_cases

*Generate ratios
gen deathactive14 = new_deaths/active14 * 1000
gen recoveredactive14 = new_recovered/active14 * 1000
gen deathrecoveredactive14 = (new_deaths + new_recovered)/active14 * 1000
gen deathactive18 = new_deaths/active18 * 1000
gen recoveredactive18 = new_recovered/active18 * 1000
gen deathrecoveredactive18 = (new_deaths + new_recovered)/active18 * 1000

*Create moving average active cases lagged 14 & 18 days
gen active14mv7 = l14.new_casesmv7
gen active18mv7 = l18.new_casesmv7

*Generate moving ratios
gen deathactive14mv7 = new_deathsmv7/active14mv7 * 1000
gen recoveredactive14mv7 = new_recoveredmv7/active14mv7 * 1000
gen deathrecoveredactive14mv7 = (new_deathsmv7 + new_recoveredmv7)/active14mv7 * 1000
gen deathactive18mv7 = new_deathsmv7/active18mv7 * 1000
gen recoveredactive18mv7 = new_recoveredmv7/active18mv7 * 1000
gen deathrecoveredactive18mv7 = (new_deathsmv7 + new_recoveredmv7)/active18mv7 * 1000

*Drop date variable
drop date day_code address*
rename dateStr date

drop if date > last_update
drop last_update

*Export provincial data
export delimited "$OUTPUT1_2", replace
clear

********************************************************************************
*CREATE PROVINCIAL DATA SET

*CLEAN PROVINCIAL CASES AND DEATHS DATA

*Save Excel sheet names
local cases "cases"
local deaths "deaths"
local recovered "recovered"
local testing "tests_completed"
local avaccine "vaccine_administration"
local dvaccine "vaccine_distribution"
local cvaccine "vaccine_completion"

*Create an empty data set to merge files to
gen province = ""
gen date = .
save "temp", replace

*Use loop to clean both cases and deaths data
foreach v in cases deaths testing {

	*Load data
	import delimited "``v''_timeseries_prov.csv", varn(1) bindq(strict) clear

	*Delete unnecessary observations
	rename date Date
	gen date = date(Date, "YMD")
	format date %td
	drop if date == . 

	*Recode province names
	rename region province
	replace province = "Alberta" if province == "AB"
	replace province = "British Columbia" if province == "BC"
	replace province = "Manitoba" if province == "MB"
	replace province = "New Brunswick" if province == "NB"
	replace province = "Newfoundland & Labrador" if province == "NL"
	replace province = "Nova Scotia" if province == "NS"
	replace province = "Northwest Territories" if province == "NT"
	replace province = "Nunavut" if province == "NU"
	replace province = "Ontario" if province == "ON"
	replace province = "Prince Edward Island" if province == "PE"
	replace province = "Quebec" if province == "QC"
	replace province = "Saskatchewan" if province == "SK"
	replace province = "Yukon" if province == "YT"

	*Consolidate new cases by province
	rename value_daily new_`v'
	rename value `v'

	*Keep only relevant variables 
	keep province date new_`v' `v'

	*Save for merging later
	merge 1:1 province date using "temp", nogen keep(1 2 3)
	save "temp", replace
}


foreach v in recovered avaccine dvaccine cvaccine {

	*Load data
	import delimited "``v''_timeseries_prov.csv", varn(1) bindq(strict) clear

	*Delete unnecessary observations
	ds date*
	rename `r(varlist)' Date
	gen date = date(Date, "DMY")
	format date %td
	drop if date == . 

	*Recode province names
	replace province = "British Columbia" if province == "BC"
	replace province = "Newfoundland & Labrador" if province == "NL"
	replace province = "Northwest Territories" if province == "NWT"
	replace province = "Prince Edward Island" if province == "PEI"

	*Consolidate new cases by province
	rename `v' new_`v'
	rename cumulative_`v' `v'

	*Keep only relevant variables 
	keep province date new_`v' `v'

	*Save for merging later
	merge 1:1 province date using "temp", nogen keep(1 2 3)
	save "temp", replace
}

*Clean vaccines administered and vaccines completed based on distributed
foreach v in avaccine cvaccine {
	replace `v' = 0 if `v' == . & dvaccine != . & dvaccine != 0
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
keep date* province* cases new_cases deaths new_deaths recovered new_recovered testing new_testing *vaccine
order date* province* cases new_cases deaths new_deaths recovered new_recovered testing new_testing *vaccine

*Merge with population data
merge m:1 province using pop_province
drop if _merge == 2
drop _merge

*Save intermediate dataset
save "temp", replace
drop if province == "Repatriated"

*Generate some interesting variables
gen casespc = cases/pop
gen deathspc = deaths/pop
gen recoveredpc = recovered/pop
gen new_casespc = new_cases/pop
gen new_deathspc = new_deaths/pop
gen new_recoveredpc = new_recovered/pop
gen lcases = log(cases)
gen ldeaths = log(deaths)
gen lrecovered = log(recovered)
gen deathrecovered = deaths + recovered
gen testingpc = testing/pop
gen new_testingpc = new_testing/pop
gen ltesting = log(testing)
*gen ldeathrecovered = log(deathrecovered)
gen advaccine = avaccine/dvaccine
gen cdvaccine = cvaccine/dvaccine
gen new_advaccine = new_avaccine/new_dvaccine
gen newa_dvaccine = new_avaccine/dvaccine
gen new_cdvaccine = new_cvaccine/new_dvaccine
gen newc_dvaccine = new_cvaccine/dvaccine
gen avaccinepc = avaccine/pop
gen cvaccinepc = avaccine/pop
gen dvaccinepc = dvaccine/pop
gen new_avaccinepc = new_avaccine/pop
gen new_cvaccinepc = new_cvaccine/pop
gen new_dvaccinepc = new_dvaccine/pop

*Generate active cases
gen active = cases - recovered - deaths

*Set time panel
encode province, gen(province_code)
tsset province_code date

*************************
*GENERATE MOVING AVERAGES SERIES
sort province_code date
bysort province_code: gen day_code = _n
foreach v in new_cases cases new_deaths deaths new_recovered recovered active {
	gen `v'mv7 = (1/7) * (`v' + `v'[_n-1] + `v'[_n-2] + `v'[_n-3] + `v'[_n-4] + `v'[_n-5] + `v'[_n-6]) if day_code > 6
}

*************************
*GENERATE DYNAMICS DATA SET

*Create active cases lagged 14 & 18 days
gen active14 = l14.new_cases
gen active18 = l18.new_cases

*Generate ratios
gen deathactive14 = new_deaths/active14 * 1000
gen recoveredactive14 = new_recovered/active14 * 1000
gen deathrecoveredactive14 = (new_deaths + new_recovered)/active14 * 1000
gen deathactive18 = new_deaths/active18 * 1000
gen recoveredactive18 = new_recovered/active18 * 1000
gen deathrecoveredactive18 = (new_deaths + new_recovered)/active18 * 1000

*Create moving average active cases lagged 14 & 18 days
gen active14mv7 = l14.new_casesmv7
gen active18mv7 = l18.new_casesmv7

*Generate moving ratios
gen deathactive14mv7 = new_deathsmv7/active14mv7 * 1000
gen recoveredactive14mv7 = new_recoveredmv7/active14mv7 * 1000
gen deathrecoveredactive14mv7 = (new_deathsmv7 + new_recoveredmv7)/active14mv7 * 1000
gen deathactive18mv7 = new_deathsmv7/active18mv7 * 1000
gen recoveredactive18mv7 = new_recoveredmv7/active18mv7 * 1000
gen deathrecoveredactive18mv7 = (new_deathsmv7 + new_recoveredmv7)/active18mv7 * 1000

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
foreach v in cases deaths recovered testing {
	replace `v' = `v'[_n-1] if day_code != 1 & `v' == .
	replace new_`v' = `v' - `v'[_n-1] if day_code != 1 & new_`v' == .
}
drop province_code day_code 

*Export provincial data
export delimited "$OUTPUT2", replace
*save "covid_province", replace
clear

********************************************************************************
*CREATE CANADA-WIDE DATA SET

*Load dataset
use "temp", clear
drop cases new_cases deaths new_deaths testing new_testing

*Create national dataset
gen country = "Canada"

*Aggregate cases to national level
ds recovered avaccine dvaccine cvaccine new*
collapse(sum) `r(varlist)', by(date dateStr country)
save "temp", replace

*Merge with cases, deaths, testing

foreach v in cases deaths testing {
	import delimited "``v''_timeseries_can.csv", varn(1) bindq(strict) clear
	gen country = "Canada"
	rename date dateStr 
	rename value `v'
	rename value_daily new_`v'
	keep country dateStr `v' new_`v'
	merge 1:1 country dateStr using "temp"
	drop _merge
	save "temp", replace
}

*Load
use "temp", clear

foreach var in avaccine dvaccine cvaccine {
	replace `var' = . if date < 22263 // 14 December 2020
	replace new_`var' = . if date < 22263 
}

*Merge with population data
merge m:1 country using pop_canada
drop if _merge == 2
drop _merge

*Generate some interesting variables
gen casespc = cases/pop
gen deathspc = deaths/pop
gen recoveredpc = new_recovered/pop
gen new_casespc = new_cases/pop
gen new_deathspc = new_deaths/pop
gen new_recoveredpc = recovered/pop
gen lcases = log(cases)
gen ldeaths = log(deaths)
gen lrecovered = log(recovered)
gen deathrecovered = deaths + recovered
gen testingpc = testing/pop
gen new_testingpc = new_testing/pop
gen ltesting = log(testing)
*gen ldeathrecovered = log(deathrecovered)
gen advaccine = avaccine/dvaccine
gen cdvaccine = cvaccine/dvaccine
gen new_advaccine = new_avaccine/new_dvaccine
gen newa_dvaccine = new_avaccine/dvaccine
gen new_cdvaccine = new_cvaccine/new_dvaccine
gen newc_dvaccine = new_cvaccine/dvaccine
gen avaccinepc = avaccine/pop
gen cvaccinepc = avaccine/pop
gen dvaccinepc = dvaccine/pop
gen new_avaccinepc = new_avaccine/pop
gen new_cvaccinepc = new_cvaccine/pop
gen new_dvaccinepc = new_dvaccine/pop

*Generate active cases
gen active = cases - recovered - deaths

*Set time panel
tsset date

*************************
*GENERATE MOVING AVERAGES SERIES
egen day_code = rank(date)
sort date
foreach v in new_cases cases new_deaths deaths new_recovered recovered active {
	gen `v'mv7 = (1/7) * (`v' + `v'[_n-1] + `v'[_n-2] + `v'[_n-3] + `v'[_n-4] + `v'[_n-5] + `v'[_n-6]) if day_code > 6
}

*************************
*GENERATE DYNAMICS DATA SET

*Create active cases lagged 14 & 18 days
gen active14 = l14.new_cases
gen active18 = l18.new_cases

*Generate ratios
gen deathactive14 = new_deaths/active14 * 1000
gen recoveredactive14 = new_recovered/active14 * 1000
gen deathrecoveredactive14 = (new_deaths + new_recovered)/active14 * 1000
gen deathactive18 = new_deaths/active18 * 1000
gen recoveredactive18 = new_recovered/active18 * 1000
gen deathrecoveredactive18 = (new_deaths + new_recovered)/active18 * 1000

*Create moving average active cases lagged 14 & 18 days
gen active14mv7 = l14.new_casesmv7
gen active18mv7 = l18.new_casesmv7

*Generate moving ratios
gen deathactive14mv7 = new_deathsmv7/active14mv7 * 1000
gen recoveredactive14mv7 = new_recoveredmv7/active14mv7 * 1000
gen deathrecoveredactive14mv7 = (new_deathsmv7 + new_recoveredmv7)/active14mv7 * 1000
gen deathactive18mv7 = new_deathsmv7/active18mv7 * 1000
gen recoveredactive18mv7 = new_recoveredmv7/active18mv7 * 1000
gen deathrecoveredactive18mv7 = (new_deathsmv7 + new_recoveredmv7)/active18mv7 * 1000

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
export delimited "$OUTPUT3", replace
*save "covid_canada", replace
clear

*************************
*CLEAN UP FILE DIRECTORY

*Remove intermediate datasets from directory
foreach data in temp healthregion location_temp layoffs_canada layoffs_province reservations_canada reservations_province unem_province unem_canada ridership_province ridership_canada grindex_province grindex_canada grstindex_province grstindex_canada bnccindx_province bnccindx_canada bnccexp_province bnccexp_canada bnccpbk_province bnccpbk_canada goog_province goog_canada pop_region pop_province pop_canada {
	rm `data'.dta
}

