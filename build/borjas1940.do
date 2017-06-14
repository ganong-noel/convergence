
clear all
set mem 4g
set more off

use serial statefip year gq migplac5 migsea rent valueh educ incwage sea age perwt hhwt labforce using $src/data17

keep if year==1940
keep if gq==1
drop if statefip==2 | statefip==11 | statefip==15
mvdecode rent valueh incwage, mv(9999999)
mvdecode rent valueh incwage, mv(999999)
drop if migsea>502

****NOTE -- CHANGING DEF OF SKILLED IN 1940
gen skill = educ >= 10
replace skill=1 if educ>=6 & year==1940


****Create Housing Costs****
gen cost = valueh/20 if valueh < 999990 & valueh!=0
replace cost = 12*rent if cost==. & rent>0 & rent<9999

***Create Wage Income
egen hhinc = sum(incwage), by (serial statefip)


**************
*INDIVIDUAL LEVEL MIGRATION CALCULATIONS
***************
*Define Migration
gen mig = migsea!=sea


**** Count Stayer by Skill****
preserve
keep if age>=25 & age<=65
collapse (rawsum) stay = perwt if mig == 0, by(statefip sea skill)
sort statefip sea skill
save $work/stay, replace
restore


***** Count InMigrants by Skill***
preserve
keep if age>=25 & age<=65
collapse (rawsum) inMig = perwt if mig == 1, by(statefip sea skill)
sort statefip sea skill
save $work/inMig, replace
restore

******Count OutMigrants by Skill***
preserve
keep if age>=25 & age<=65
collapse (rawsum) outMig = perwt if mig == 1, by(migplac5 migsea skill)
rename migplac5 statefip
rename migsea sea
sort statefip sea skill
save $work/outMig, replace
restore


**************
*HH LEVEL NOMINAL AND REAL INCOME CALCULATIONS
***************
***** Create unconditional household real and nominal income by SEA for non-migrant families****
preserve
gen adultworker= labforce==2 &  age>=25 & age<=65
collapse (mean) adultworker cost hhinc hhwt mig, by (serial statefip sea)
drop if adultworker<=0 | adultworker==.
collapse (mean) costShared=cost incShared=hhinc  if mig == 0 [w=hhwt], by(statefip sea)
sort statefip sea
save $work/value, replace
restore


***** Create Skill Specific Real Income , NOW LIMITING SAMPLE
gen adultworker= labforce==2 &  age>=25 & age<=65
gen skilledadultworker = adultworker*skill

collapse (mean) cost hhinc hhwt mig (sum) adultworker skilledadultworker, by (serial statefip sea)
drop if adultworker==. | adultworker==0
gen skill= skilledadultworker/adultworker
* 86% of sample is at the poles
keep if skill==1 | skill==0
collapse(mean) hhinc cost if mig == 0 [w=hhwt], by(statefip sea skill)


sort statefip sea
merge m:1 statefip sea using $work/value, assert(3) nogen


sort statefip sea skill
merge 1:1 statefip sea skill using $work/stay,
keep if _merge==3
drop _merge


sort statefip sea skill
merge 1:1 statefip sea skill using $work/inMig,
replace inMig=0 if _merge==1
drop _merge

sort statefip sea skill
merge 1:1 statefip sea skill using $work/outMig
replace outMig=0 if _merge==1
drop _m

gen year =1940
sort year
merge year using $work/inflate
keep if _merge==3
drop _merge
replace hhinc = hhinc*index
replace cost=cost*index
replace incShared=incShared*index

gen basePop = stay+outMig
bys statefip sea: egen basePopTot = sum(basePop)
gen netMig = (inMig-outMig)/basePopTot

gen lr = log(hhinc-cost)
gen lincShared=log(incShared)
save $work/BORJAS1940FINAL, replace


