
clear all
set mem 5g
set more off

*****************
*CONV TABLE
*********************
use ${work}/summarySamp, clear
collapse incwageCen=incwage (rawsum) perwt* [w=perwtCen], by(statefip year)
save $work/convTable, replace


*****************
*INEQUALITY
*********************
use ${work}/summarySamp if age >= 18 & age <= 65 & statefip != 99, clear
keep if qinc == 0 | qearn == 0 | year == 1960
replace incwage = . if incwage == 0 | incwage == 999999
bys hrswork2: egen hrsLastWkFromInterval = mean(hrswork1)
bys uhrswork: egen hrsLastWkFromUsual = mean(hrswork1)
gen hoursConsistent = hrswork1
replace hoursConsistent = hrsLastWkFromInterval if year == 1960 | year == 1970
replace hoursConsistent = hrsLastWkFromUsual if year >= 2000
 
bys wkswork2: egen wks = mean(wkswork1)
gen wksConsistent = wkswork1
replace wksConsistent = wks if year == 1960 | year == 1970 | year == 2010
gen agHours = wksConsistent*hoursConsistent
drop hrswork1 hrswork2 uhrswork wkswork1 wkswork2 perwtCenNonblack hrsLastWkFromInterval hrsLastWkFromUsual wks

gen wage = log(incwage/agHours) if wksConsistent >= 40 & hoursConsistent >= 30 & sex == 1

foreach year of numlist 1940(10)2010 {
	_pctile wage if year == `year', p(1 99)
	replace wage = r(r1) if wage < r(r1)  & year == `year'
	replace wage = r(r2) if wage > r(r2) & wage != . & year == `year'
}

save $work/inequality, replace


preserve
collapse wageMean=wageMale [w=perwt], by(statefip year)
save $work/temp2, replace
keep if year == 1940 | year == 1980
reshape wide wageMean, i(statefip) j(year)
save $work/temp, replace
restore

use $work/inequality, clear
merge m:1 statefip using $work/temp, nogen
sort statefip year
merge m:1 statefip year using $work/temp2

gen demean = wageMale-wageMean
gen counter1980 = wageMale+wageMean1940*(1-0.35) if year == 1980
gen counter2010 = wageMale-wageMean1980*(1-0.35) if year == 2010
bys statefip year: egen ybar = mean(wageMale)

collapse (sd) ybar wageMale  demean counter* [w=perwtCen], by(year)
list
outsheet using ${out}/inequality.csv, comma replace
