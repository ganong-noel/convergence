clear all
set mem 4g
set more off
tempfile priceindex
use year index using $work/stateData ,clear
duplicates drop
sort year
save `priceindex',replace


forv j=1/5 {


use serial statefip year bpl race gq migplac5 pumares2mig rent valueh educ incwage migpuma age perwt hhwt labforce using $src/data18, clear
keep if year==2000
keep if gq==1
drop if statefip==2 | statefip==11 | statefip==15
mvdecode rent valueh incwage, mv(9999999)
mvdecode rent valueh incwage, mv(999999)


****NOTE -- CHANGING DEF OF SKILLED IN 1940
gen skill = educ >= 10
replace skill=1 if educ>=6 & year==1940


***** Create Housing Costs****
gen cost = valueh/20 if valueh!=0
replace cost = 12*rent if cost==. & rent>0

if `j'==2{
replace cost = 2*cost
}

***Create Wage Income
egen hhinc = sum(incwage), by (serial statefip)

*Define Migration
gen mig =  migpuma>0

if `j'==3 {
drop mig
gen mig =  migpuma>0 & statefip!=migplac5
}



if `j'==4 {
drop mig
gen mig =  migpuma>0 & race==1
}

if `j'==5 {
drop mig
gen mig =   bpl!=statefip
}


**** Count Stayer by Skill****
preserve
keep if age>=25 & age<=65
collapse (rawsum) stay = perwt if mig == 0, by(statefip pumares2mig skill)
sort statefip pumares2mig skill
save $work/stay, replace
restore

***** Count InMigrants by Skill***
preserve
keep if age>=25 & age<=65
collapse (rawsum) inMig = perwt if mig == 1, by(statefip pumares2mig skill)
sort statefip pumares2mig skill
save $work/inMig, replace
restore

******Count OutMigrants by Skill***
preserve
keep if age>=25 & age<=65
collapse (rawsum) outMig = perwt if mig == 1, by(migplac5 migpuma skill)
rename migpuma pumares2mig
rename migplac5 statefip
sort statefip pumares2mig skill
save $work/outMig, replace
restore



***** Create unconditional household real and nominal income by pumares2mig for non-migrant families****
preserve
gen adultworker= labforce==2 &  age>=25 & age<=65
collapse (mean) adultworker cost hhinc hhwt mig, by (serial statefip pumares2mig)
drop if adultworker<=0 | adultworker==.
collapse (mean) costShared=cost incShared=hhinc  if mig == 0 [w=hhwt], by(statefip pumares2mig)
sort statefip pumares2mig
save $work/value, replace
restore


***** Create Skill Specific Real Income , NOW LIMITING SAMPLE
gen adultworker= labforce==2 &  age>=25 & age<=65
gen skilledadultworker = adultworker*skill

collapse (mean) cost hhinc hhwt mig (sum) adultworker skilledadultworker, by (serial statefip pumares2mig)
drop if adultworker==. | adultworker==0
gen skill= skilledadultworker/adultworker
* 86% at the poles
keep if skill==1 | skill==0
collapse(mean) hhinc cost if mig == 0 [w=hhwt], by(statefip pumares2mig skill)

sort statefip pumares2mig
merge m:1 statefip pumares2mig using $work/value, assert(3) nogen

sort statefip pumares2mig skill
merge 1:1 statefip pumares2mig skill using $work/stay
keep if _merge==3
drop _merge

sort statefip pumares2mig skill
merge 1:1 statefip pumares2mig skill using $work/inMig
replace inMig=0 if _merge==1
drop _merge

sort statefip pumares2mig skill
merge 1:1 statefip pumares2mig skill using $work/outMig
replace outMig=0 if _merge==1
drop _m

gen year=2000
sort year
merge year using `priceindex'
keep if _merge==3
drop _merge
replace hhinc = hhinc*index
replace cost=cost*index
replace incShared=incShared*index

gen basePop = stay+outMig
bys statefip pumares2mig: egen basePopTot = sum(basePop)
gen netMig = (inMig-outMig)/basePopTot

gen lr = log(hhinc-cost)
gen lincShared=log(incShared)
save $work/BORJAS2000ROBUST`j', replace

}
