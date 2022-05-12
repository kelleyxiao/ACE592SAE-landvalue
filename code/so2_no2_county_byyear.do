
	******************************************************************************************	
	/*Code to get data on air contaminants at the county level for 1997, 2002, 2007, 2012*/
	******************************************************************************************
	
	
	glo s02 "/Users/juliandiaz/Box/ACE 592-Python project/Paper data/data/so2_data/downloaded_so2"
	
	glo n02 "/Users/juliandiaz/Box/ACE 592-Python project/Paper data/data/no2_data/downloaded_no2"
	
	glo output "/Users/juliandiaz/Box/ACE 592-Python project/project data/climate"
	
	glo geo_input "/Users/juliandiaz/Box/ACE 592-Python project/Paper data/data/location_data"
	
	
	foreach type in s02 n02{
	
	dis in red "*****`type'*****"
	
	forval x=1997(5)2012{
	
	dis in red "`x'"
	
	
	if "`type'"=="s02" import delimited "$s02/daily_42401_`x'", encoding(ISO-8859-1) clear
	else 				  import delimited "$n02/daily_42602_`x'", encoding(ISO-8859-1) clear
	
	
	gen year = substr(datelocal, 1, 4)
	destring year, replace force
	
	gen month = substr(datelocal, 6, 2)
	destring month, replace force
	tab month, missing
	
	gen day = substr(datelocal, 9, 2)
	destring day, replace force
	tab day, missing

	tab sampleduration
	
	** Keep only 1 hours durations . . . they're 97% of the data, and the usual way to measure SO2;
	keep if sampleduration == "1 HOUR"

	
	** Some locations have multiple reading spots using different sample methods. Here we keep only the main readings at any one site – also robust to collapsing to site average;
	keep if poc == 1
	
	
	** Create a unique identifier for each sensor . . . defined by combination of statecode, countycode, and sitenum;
	** Drop any without a statecode, countycode, or sitenumber;
	destring statecode, replace force
	drop if statecode == . | countycode == . | sitenum == .
	egen id=group(statecode countycode sitenum)
	label var id "Sensor id"

	** Keep only those with no events;
	keep if eventtype == "None"		

	
	** Test for duplicates - none;
	duplicates tag id day month year, g(dupes)
	tab dupes
	drop dupes
	
	** Location variables – rename and drop sensors with no location data;
	rename latitude sen_lat
	rename longitude sen_lon
	replace sen_lat=. if sen_lat==0
	replace sen_lon=. if sen_lon==0
	drop if sen_lat == . | sen_lon == .

	
	rename statecode stfips
	rename countycode countyfips

	
	**Drop territory/protectorate sensors or those with note state data;
	drop if stfips > 56 | stfips == .

	** Name standard pollutant value appropriately;
	rename arithmeticmean `type'
	
	
	save "$output/`type'_daily_fromstation_`x'.dta" , replace
	
	
	*************************************
	***Create distance for collapsing****
	*************************************
	
	**Limit to sensors within radius of 100 miles . . . will trim further later;
	local radius=50
	
	**generate sensor fips
	gen sen_fips = stfips*1000 + countyfips

	drop if stfips > 56
	
	
	** Check for duplicates;		
	duplicates tag sen_lat sen_lon year month day, g(dupes)
	tab dupes
	** Minimal odd duplicates, almost all from Tennesee – drop here;
	drop if dupes ~= 0
	
	**keep one censor per id to calculate distance
	bysort id: keep if _n == 1
	** Check for unique id: no return means unique;
	isid id
	keep id sen_lat sen_lon sen_fips	

	save "$output/sensorlist_`type'_`x'.dta", replace
	
	
	***Bring geo data at the county level***
	
	insheet using "$geo_input/county2k.csv", clear
	
	
	gen fips = substr(v1,-5,5)
	destring fips, replace force
	sum fips
	count if fips == .
	
	rename v9 county_lat
	rename v10 county_lon

	keep fips county_lat county_lon
	
	cross using "$output/sensorlist_`type'_`x'.dta"

	**COMPUTE DISTANCE BETWEEN TWO POINTS
	
	qui vincenty sen_lat sen_lon county_lat county_lon,  hav(dist)
	label var dist "Distance between county and sensor"
	qui compress
	
	loc radius=200
	
	drop if dist>=`radius' 
	
	
	tempfile dist_cent_`x'
	save `dist_cent_`x'', replace
	
		
	/*create collapsed datasets*/
	
	use "$output/`type'_daily_fromstation_`x'.dta", clear
	
	keep id `type' year month day
	
	
	joinby id using `dist_cent_`x'' , unmatched(both)
	
	tab _merge
	drop if _merge!=3
	drop _merge
	
	keep id `type' dist fips year month day
	
	egen sensor_count_100 = nvals(id), by(fips year)
	
	
	gcollapse `type'_d100_daily = `type' (max) sensor_count_100 [aw = 1/dist], by(fips year month day) fast
	sort fips year month day
	
	
	** Now collapse to yearly levels;
	gcollapse (mean) `type'_d100_allyear = `type'_d100_daily (max) sensor_count_*, by(fips year) fast

	**********************
		**append all**
	**********************
	
	if `x'==1997{
	
	cap rm "$output/`type'_1997_2012.dta"
	save "$output/`type'_1997_2012.dta", replace
	
	}
		
	else{
	append using "$output/`type'_1997_2012.dta"
	save "$output/`type'_1997_2012.dta", replace
	
	}

	
	
	}
	
	
} 	

	use "$output/s02_1997_2012.dta",clear
	ren s02_d100_allyear s02
	drop sensor*
	sort fips year
	export delimited using "$output/df_s02_1997_2012.csv", replace
	
		
	use "$output/n02_1997_2012.dta",clear
	ren n02_d100_allyear n02
	drop sensor*
	sort fips year
	export delimited using "$output/df_n02_1997_2012.csv", replace


