clear all
set mem 5g
set more off
global cond = "gq == 1 & datanum != 3"
global age = "age >= 25 & age <= 64"

*****************
*CONSTRUCT SAMPLE FOR CONV TABLE AND INEQUALITY
*********************
use incwage statefip year race perwt gq datanum uhrs* hrs* wks* sex age qearn qincwage using $src/usa_00088 if $cond & year >= 1940, clear
set seed 1
keep if runiform() <= .2 | year != 1980
replace incwage = 0 if incwage == 999999
rename perwt perwtCen
replace perwtCen = perwtCen*5 if year == 1980
gen perwtCenNonblack = perwtCen if race != 2
save $work/summarySamp, replace


*****************
*CONSTRUCT SAMPLE FOR HUMAN CAPITAL
*********************
use $src/usa_00088 if year >= 1940 & $age & $cond, clear
drop mig* cntygp98 puma sea
set seed 1
keep if runiform() <= .2 | year != 1980
drop if slrec == 1 & year == 1950

gen fb = bpl >= 100
replace bpl = 0 if fb

recode educ (0 = 1 "0 or NA") (1 = 2 "Elem") (2 = 3 "Middle")(3/5= 4 "<HS") (6 = 5 "HS") (7/9 = 6 "<BA") (10/11 = 7 "BA"), gen(eduBin)
recode age (25/34 = 1) (35/44 = 2) (45/54 = 3) (55/65 = 4), gen(ageBin)
recode age (25/44 = 1) (45/65 = 2), gen(ageBin2)
gen black = race == 2
gen hispanic = hispan > 0
gen female = sex == 2
replace incwage = . if incwage == 999999
gen linc = log(incwage)

foreach var of varlist linc  {
foreach year of numlist 1940(10)2010 {
	_pctile `var' if year == `year', p(1 99)
	replace `var' = r(r1) if `var' < r(r1) & `var' != . & year == `year'
	replace `var' = r(r2) if `var' > r(r2) & `var' != . & year == `year'
}
}

tab year, gen(yeard)
drop yeard1

save $work/hcSamp, replace



