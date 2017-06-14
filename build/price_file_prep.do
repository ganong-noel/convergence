


clear all
set more off

********Creates State-Year Means********
********Keep only HH with an adult worker in average
********This matches the requirement we need below to define skill
*****************************************

forv j =19/21 {

use age labforce educ incwage race bpl year statefip valueh rent hhwt sex gq serial using $src/data`j',clear
keep if gq==1
drop if statefip==2 | statefip==11 | statefip==15 | statefip>56
mvdecode  valueh incwage rent, mv(9999999)
mvdecode  valueh incwage rent, mv(999999)
save $work/ddd`j', replace


*********************************************************
****Create Average HH Income by State and Birth State****
*********************************************************
gen adultworker= labforce==2 & age>=25 & age<=65

collapse (sum) incwage adultworker  (mean) rent valueh hhwt, by (statefip serial year)


**** Make Consistent Sample****
keep if adultworker>=0 
*******************************

collapse (mean) incwage hhwt[pw=hhwt], by (statefip year)

rename incwage avginc
sort statefip year
save $work/faminc`j', replace

rename avginc birth_avginc
rename statefip bpl
sort bpl year
save $work/bpl_faminc`j',replace

}

use $work/faminc19, clear
append using $work/faminc20
append using $work/faminc21
duplicates drop
sort statefip year
save $work/faminc, replace

use $work/bpl_faminc19, clear
append using $work/bpl_faminc20
append using $work/bpl_faminc21
duplicates drop
sort bpl year
save $work/bpl_faminc, replace

**********************************************************

forv j=19/21 {
use $work/ddd`j', clear

**** Create Demographics
gen adultworker= labforce==2 & age>=25 & age<=65
gen people =1
gen white= race==1
gen black = race==2
gen male = sex==1

*** SKILL DEFINITION
gen     skill_person = educ >=10
replace skill_person = educ>=6 if year==1940

drop educ sex race labforce

***  ADULT WORKER CHARACTERISTICS
gen ageworker   =  age*adultworker
gen skillworker =  skill_person*adultworker
gen maleworker  =  male*adultworker
gen whiteworker =  white*adultworker
gen blackworker =  black*adultworker

*** Merge in Adult Birthplace data
sort bpl year
merge bpl year using $work/bpl_faminc
gen nonmissingworker = adultworker==1 & birth_avginc!=. & birth_avginc!=0
gen birthincomeworker = birth_avginc*nonmissingworker
replace birthincomeworker=. if adultworker==.  | birth_avginc==. | birth_avginc==0 |_merge!=3
drop _merge


collapse (sum) nonmissingworker adultworker skillworker ageworker maleworker whiteworker blackworker birthincomeworker people incwage  (mean)  rent valueh hhwt , by (statefip serial year)

*** Keep only HH with an Adult Worker
drop if adultworker<=0

**** Merge in State Averages
sort statefip year
merge statefip year using $work/faminc
drop _merge

*** Create HH Worker Demos
gen fractionblack = blackworker/adultworker
gen fractionwhite = whiteworker/adultworker
gen fractionmale = maleworker/adultworker
gen agepw = ageworker/adultworker
gen agepw2 = agepw^2


**Create HH Skill
gen skill=skillworker/adultworker
gen unskill_interaction = (1-skill)*avginc
gen skill_interaction = skill*avginc
*keep if skill==0 | skill==.5 | skill==1

gen birth_avginc = birthincomeworker/nonmissingworker

**** GIVING VERY STRANGE RESULTS!
replace birth_avginc = . if birth_avginc==0 | adultworker==0 | birth_avginc==.
drop if birth_avginc==.

gen instrument_inter = birth_avginc*skill
gen instrument_uninter = birth_avginc*(1-skill)


*** CONSTRUCT HOUSING COST
gen     housingcost = rent*12 if rent<9998
replace housingcost = .05*valueh if valueh!=0 & valueh<9999990
replace housingcost =. if housingcost==0
gen scaled_income = incwage-housingcost

save $work/lastsave`j',replace
}
