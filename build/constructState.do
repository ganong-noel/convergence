/*
This pulls together at a state level (50 obs per year)
--decadal house prices to 1960 (can also get 1930 and 1940 if needed)
--BEA income pop (1929-2010)
--Census Haines inc pop price (1930-1970)
--Census IPUMS income price rent (1950-2010)
--Saiz
*/

******************
*NET MIGRATION 
****************
*goes back to 1940-1950
insheet using $src/netmigration/birthdeath.csv, comma clear
keep v* name
*** ADDED DROP HI
drop if name == "Alaska" | name == "DistrictOfColumbia" | name == "Hawaii"
reshape long v, i(name) j(year)
replace year = year*10+1930
rename v netMigBDK
append using $src/vital/netmigBDK
sort name year
save $work/bd, replace

*goes back to 1930-1940
insheet using $src/netmigration/survival.csv, comma clear
keep v2-v7 v10
rename v10 name
drop if name == "Alaska" | name == "DistrictOfColumbia" | name == "Hawai'i" | name == "NorthAndSouthDakota"
reshape long v, i(name) j(year)
replace year = year*10+1920
rename v netMigSurvivalK
sort name year
save $work/sur, replace 

*Fishback and Kantor
insheet using "$src/netmigration/New Deal county migration paper data set.csv", comma clear
foreach var of varlist  netmigra  gcnetmra  ad2netma {
	replace `var' = `var'*pop30t
}
collapse (sum) netmigra ad2netma, by(state)
label var netmigra "Net Mig, Birth-Death, 1930-1940 (Fishback Kantor)"
label var ad2netma "Net Mig, Birth-Death, Births Corrected 1930-1940 (Fishback Kantor)"
merge 1:1 state using $src/netmigration/stateIcpsrXwalk
tab statefip if _m == 2
drop if _m == 2
drop _m
rename statefip statefips
gen year = 1940
sort statefips year
save $work/netmig3040, replace


********
*SAIZ
********
use pop unaval elasticity year fips WRLURI using $work/countyData if year == 1960, clear
gen statefips = floor(fips/1000)
collapse saizelasticity=elasticity unaval WRLURI [w=pop], by(statefips)
save $work/saiz, replace


********
*POSTAL CODE
********
insheet using $src/stateAbbrev.csv, comma clear
drop statename
rename fipscode statefips 
rename stateabbreviation stateabbrev
drop if statefips >= 60
sort statefips
save $work/statefips, replace


********
*HAINES
********
//cleaned version of file assembled by Haines (Historical Statistics of the US)
use fips value incMedian year permit rent rentCount valueCount shareBa using $src/haines if mod(fips, 1000) == 0, clear
gen statefips = floor(fips/1000)
drop fips
sort statefips year
save $work/haines, replace


*****************
*CENSUS PUBLISHED STATS
*****************
//cleaned version of county-level statistics from USA Counties
use value fips year incMedHH inc permit rent rentCount valueCount shareBa using $src/usaCounties if mod(fips, 1000) == 0 & fips != 0, clear
rename inc incMedian
replace incMedian = incMedHH if year == 2010
drop incMedHH
gen statefips = floor(fips/1000)
drop fips
sort statefips year
save $work/cen, replace


******************
*BEA INCOME AND POP FILES
******************
//cleaned version of Table SA-04
use $src/stateSA04, clear
drop beaTra beaEmp beaWage
rename beaPop pop
save $work/stateSA04, replace

**********************
*LAND USE
*********************
*land use is missing in 1940. replace with 1941 value
use WRLURI documentcount total_annual_docs stateabbrev year using "$src/landuse" , clear
expand 2 if year == 1941, gen(dup)
replace year = 1940 if dup == 1
rename documentcount docLu
rename total_annual_docs docTot
drop if year >= 2011
drop dup
save $work/landuse, replace

**********************
*ZONING
*********************
*zoning is missing for 2010. assume same flows as in 2009.
use documentcount year stateabbrev using "$src/zoning" , clear
rename documentcount docZone
expand 2 if year == 2009, gen(dup)
replace year = 2010 if dup == 1
drop dup
save $work/zoning, replace


**********************
* Planner Survey Prep*
**********************
use "$src/planner_survey", clear
sort item
merge item using "$src/planner_explanation"
keep if count==1
rename value planner
replace planner=1 if planner==.5
collapse (sum) planner, by (stateabbrev)
sort stateabbrev
save $work/american_institute_planners, replace



**************************
*BUILD UNAVAILABILITY FILE
**************************
use incMedian unaval year fips lma using $work/countyData.dta, clear
replace unaval = 0 if unaval == .
g liInc=ln(incMedian)
drop if liInc==.
xtile UBinFive = unaval, n(100)
replace UBinFive = (UBinFive-1)/100
gen interactUBinFive = UBinFive*liInc
global post = 1965
gen post = year > $post
gen liIncPost = liInc*post
gen interactUBinFivepost = UBinFive*liInc*post
tsset fips year
g dliInc =100*(F20.liInc-liInc)/20
save $work/countyUnaval, replace

************************
*HC DATA 
*********************
use year statefip hcAggBaseBplY hcAggBaseResY using $work/lucasHcConverge_im if statefip != 99, clear
gen dHc = hcAggBaseResY - hcAggBaseBplY
gen dlogHc = log(hcAggBaseResY) - log(hcAggBaseBplY)
save $work/dhc, replace

***************
*MERGE FILES
***************
use $work/cen, clear

merge 1:1 statefips year using $work/haines, nogen update

merge 1:1 statefips year using $work/stateSA04, nogen 

merge 1:1 statefips year using $work/netmig3040, nogen

gen name = subinstr(nameBea," ","",.)
merge 1:1 name year using $work/bd, assert(1 3) nogen

merge 1:1 name year using $work/sur, assert(1 3) nogen

merge 1:1 statefips year using $src/easterlin, nogen

drop if statefips == 99
merge m:1 statefips using $work/statefips, assert(3) nogen

merge m:1 statefips using $work/saiz, assert(1 3) nogen

merge m:1 year using $work/inflate, assert(2 3)
drop if _m == 2
drop _m

//XXXX stopped here
*this file comes from chetty/shoag/census/convTable.do
rename statefips statefip
merge 1:1 statefip year using $src/convTable, nogen keepusing(perwtCenNonblack incwageCen)
drop if statefip == 99


merge 1:1 statefip year using $work/dhc, nogen

merge 1:1 statea year using $work/landuse, assert(1 3) nogen

merge 1:1 statea year using $work/zoning, assert(1 3) nogen

drop if statea == "DC"
merge m:1 stateabbrev using $work/american_institute_planners, assert(3) nogen

drop if statefip == 2 | statefip == 15

*********************************
**construct vars of interest*****
*********************************
tsset statefip year

label var incMedian "Median Income (Family, 1950-2000 & HH 2010)"
label var saizelasticity "Saiz Housing Supply Elasticity"
label var netMigBDK  "Net Mig, Birth-Death Measure, 000's"
label var netMigSurvivalK  "Net Mig, Survival Measure, 000's"
label var capCoW   "Capital, Census of Wealth"
label var value   "Median House Value"
label var incwageCen   "Wage inc per capita"
label var perwtCenNonblack   "Nonblack Pop"

gen stock = rentCount+valueCount
foreach num of numlist 1/9 {
	local fnum = 10-`num'
	replace stock = (1-`num'/10)*(l`num'.rentCount+l`num'.valueCount)+(`num'/10)*(f`fnum'.rentCount+f`fnum'.valueCount) if mod(year,10) == `num'
}
gen permitsPerStock = permit/stock

foreach var of varlist beaInc incEasterlin incMedian value rent incwageCen {
	replace `var' = `var'*index
}

gen liEasterlin = log(1000000*incEasterlin/pop)
gen liBea = log(1000*beaInc/pop)
gen liInc = liBea
replace liInc = liEasterlin if year <= 1920
gen liCen = log(incMedian)
gen lpop = log(pop)
gen lvalue = log(value)
gen liWageCen = log(incwageCen)


foreach var of varlist lpop liBea liWageCen liInc {
	gen lag`var' = l20.`var'
	gen d`var' = (`var' - l20.`var')*100/20
}
gen dpopBD = (log(netMigBDK*1000 + l10.netMigBDK*1000 + l20.pop) - log(l20.pop))*100/20
replace dpopBD = (log(netMigBDK*1000 + l10.ad2netma + l20.pop) - log(l20.pop))*100/20 if year == 1950
gen dpopSur = (log(netMigSurvivalK*1000 + l10.netMigSurvivalK*1000 + l20.pop) - log(l20.pop))*100/20

replace saizelasticity =2.557092 if  statea=="AR"


************************
*BUILD ELASTICITY MEASURES
**************************
gen luPc = 10^6*docLu/pop
gen casePc = 10^6*docTot/pop
gen zonePc = 10^6*docZone/pop

***create denominator of total cases
encode stateabbrev, gen(sid)
tsset sid year
foreach var of varlist luPc casePc docLu docTot zonePc {
	gen `var'Cum = `var'
	foreach n of numlist 1/9 {
		qui replace `var'Cum = `var'Cum + max(l`n'.`var',0)
	}
	qui replace `var'Cum = `var'Cum/min(year-1939,10)
}


***TABLE 2***
xtile BinFive = luPc, n(5)
xtile BinFiveCum = luPcCum, n(5)
xtile BinFivePlacebo = casePc, n(5)
xtile BinFiveCumPlacebo = casePcCum, n(5)
xtile BinHundred = luPc, n(100)
xtile BinHundredCum = luPcCum, n(100)
xtile BinHundredPlacebo = casePc, n(100)
xtile BinHundredCumPlacebo = casePcCum, n(100)

***TABLE 3***
xtile roll55t = luPcCum if year == 1965, n(48)
bys statea: egen roll55 = max(roll55t)
xtile roll2005t = luPcCum if year == 2005, n(48)
bys statea: egen roll05 = max(roll2005t)

***APPENDIX TABLE 5**
xtile BinZone = zonePc, n(100)
xtile BinZoneCum = zonePcCum, n(100)
xtile BinTwo = luPc, n(2)
xtile BinTwoCum = luPcCum, n(2)
xtile BinTen = luPc, n(10)
xtile BinTenCum = luPcCum, n(10)

qui sum luPc if luPc > 0, d
gen winsor = min(luPc, r(p90)) / r(p90) if luPc != .
qui sum luPcCum if luPcCum > 0, d
gen winsorCum = min(luPcCum, r(p90)) / r(p90) if luPc != .

xtile BinFiveRatio = docLuCum/docTotCum, n(5)


*build an index separately for each year
bys year: egen luPcYear = mean(luPc)
gen BinFiveYear = .
foreach year of numlist 1940/1990 {
	cap drop temp
	qui xtile temp = luPc if year == `year', n(5)
	qui replace BinFiveYear = temp if year == `year'
}

****BUILD INTERACTIONS****
foreach var of varlist  BinFive BinFiveCum BinFivePlacebo BinFiveCumPlacebo BinFiveRatio  BinFiveYear {
	replace `var' = (`var'-1)/4
	gen inter`var' = `var'*liInc
}

foreach var of varlist  BinTwo BinTwoCum  {
	replace `var' = (`var'-1)
	gen inter`var' = `var'*liInc
}

foreach var of varlist  BinTen BinTenCum  {
	replace `var' = (`var'-1)/10
	gen inter`var' = `var'*liInc
}

foreach var of varlist  roll55 roll05 {
	replace `var' = (`var'-1)/48
	gen inter`var' = `var'*liInc
}


foreach var of varlist  BinHundred BinHundredCum BinHundredPlacebo BinHundredCumPlacebo BinZone BinZoneCum {
	replace `var' = (`var'-1)/100
	gen inter`var' = `var'*liInc
}

gen interwinsor = winsor*liInc
gen interwinsorCum = winsorCum*liInc
 
global post = 1965
gen post = year > $post
gen interroll55post = post*interroll55
gen interroll05post = post*interroll05
gen liIncPost = post*liInc

replace permitsPerStock = permitsPerStock*100

sort sid year
save $work/state, replace
