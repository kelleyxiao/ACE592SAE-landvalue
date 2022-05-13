cd "/Users/amazingkelley/Downloads"

forvalues i = 1998(1)2012 {
import excel using Stormdata_`i'.xlsx, firstrow
drop BEGIN_YEARMONTH EVENT_ID CZ_TYPE
rename STATE state
rename YEAR year
rename MONTH_NAME month
rename EVENT_TYPE disaster
rename CZ_NAME cz_name
tostring STATE_FIPS, gen(state_fips)
tostring CZ_FIPS, gen(cz_fips)
gen county = state_fips+ cz_fips
drop CZ_FIPS STATE_FIPS
save disaster_`i', replace
clear
}

forvalues i = 1998(1)2012 {
use disaster_1997, clear
append using disaster_`i'
save disaster_1997, replace
}

use disaster_1997, clear
gen num = 1 
bysort county year disaster: egen disaster_dct = sum(num)
bysort county disaster: egen disaster_dc = sum(num)
bysort year disaster: egen disaster_dt = sum(num)
bysort disaster: egen disaster_d = sum(num) 
bysort county year: egen disaster_ct = sum(num) 
drop num
duplicates drop county year disaster, force
gsort -disaster_d

gen category = "top10"
replace category = "top2" if disaster_d>=200000 
replace category = "top20" if disaster_d>=7000 &disaster_d<=20000
replace category = "top30" if disaster_d>=1200 &disaster_d<=7000
replace category = "top30" if disaster_d<=1200 &disaster_d>=250
replace category = "last" if disaster_d<=250

save disaster_1997, replace

gen merge_id = county+yr
duplicates drop merge_id, force
save disaster97-12, replace
sort merge_id
merge 1:1 merge_id using land_df
drop month merge_id _merge yr
export excel using disasters_continuous.xlsx, replace

use disaster97-12, clear
keep if year == 1997 | year == 2012
duplicates drop year county disaster, force
keep year county disaster disaster_dct

foreach x in "Hail" "Flash Flood" "Winter Storm" "Heavy Snow" "High Wind" "Drought" "Flood" "Winter Weather" "Tornado"{
	preserve 
	keep if disaster == "`x'"
	reshape wide disaster_dct, i(county) j(year)
	gen pct = (disaster_dct2012-disaster_dct1997)/disaster_dct1997
	save pct_`x', replace
	restore
}

forvalues i = 1(1)10{
	use pct`i', clear
	keep county disaster pct
	rename county fips
	export excel using "pct`i'.xlsx", firstrow(variables)
}


use disaster_1997, clear
keep if year == 1997 |year ==2002| year == 2007 | year == 2012 
keep year fips disaster_ct 
save disaster_discrete, replace
reshape wide disaster_ct, i(fips) j(year)
replace disaster_ct1997 = 0 if  disaster_ct1997 == .
replace disaster_ct2012 = 0 if  disaster_ct2012 == .
gen pct = (disaster_ct2012-disaster_ct1997)/disaster_ct1997
keep fips pct disaster_ct1997 disaster_ct2012
export excel using "disaster_discrete_pctxlsx", firstrow(variables) replace


use disaster_discrete, clear
sort merge_id
merge 1:1 merge_id using land_df
gen merge = 1 
replace merge = 2 if _merge == 2
replace merge = 3 if _merge == 3
keep year disaster_ct landvalue_acre merge fips
rename landvalue_acre landvalue
export excel using "disaster_landvalue.xlsx", firstrow(variables)



use disaster_discrete, clear
keep if disaster == "Thunderstorm Wind"
keep year fips disaster_dct merge_id
sort merge_id
merge 1:1 merge_id using land_df
gen merge = 1 
replace merge = 2 if _merge == 2
replace merge = 3 if _merge == 3
drop _merge merge_id
export excel using "Thunder_landvalue.xlsx", firstrow(variables)


foreach x in "Hail" "Flash Flood" "Winter Storm" "Heavy Snow" "High Wind" "Drought" "Flood" "Winter Weather" "Tornado"{
	use disaster_discrete, clear
	keep if disaster == "`x'"
	keep year fips disaster_dct merge_id
	sort merge_id
	merge 1:1 merge_id using land_df
	gen merge = 1 
	replace merge = 2 if _merge == 2
	replace merge = 3 if _merge == 3
	drop _merge merge_id
	export excel using "`x'_landvalue.xlsx", firstrow(variables) replace

}



