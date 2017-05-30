
use $work/lastsave19,clear
keep if skill==0 | skill==1
collapse (mean) incwage [pw=hhwt], by (year statefip skill)
gen lny = ln(incwage)
tempfile a
save `a', replace

use $work/lastsave20
keep if skill==0 | skill==1 
collapse (mean) incwage  [pw=hhwt], by (year statefip skill)
gen lny = ln(incwage)
tab year
append using `a'
save `a', replace

use $work/lastsave21
keep if skill==0 | skill==1 
collapse (mean) incwage [pw=hhwt], by (year statefip skill)
gen lny = ln(incwage)
tab year
append using `a'
drop if statefip>56
save `a', replace


sort statefip year skill
save $work/stateskillavg, replace




*use lastsave24, clear
*append using lastsave21
use $work/lastsave21, clear
append using $work/lastsave20
append using $work/lastsave19
keep if skill==0 | skill==1
sort statefip year skill
merge statefip year skill using $work/stateskillavg
gen lnhouse = ln(housingcost)
gen interaction = lny*skill
replace lny = lny*(1-skill)


local demographics ="fractionwhite fractionblack fractionmale agepw agepw2 skill people"

reg lnhouse `demographics' lny interaction [pw=hhwt] if year==1940, cl(statefip)
outreg2 lny interaction using "$out/migReturnPrice.txt",  replace ctitle(1940)
reg lnhouse `demographics' lny interaction [pw=hhwt] if year==1960, cl(statefip)
outreg2 lny interaction using "$out/migReturnPrice.txt",  append ctitle(1960)
reg lnhouse `demographics' lny interaction [pw=hhwt] if year==1970, cl(statefip)
outreg2 lny interaction using "$out/migReturnPrice.txt",  append ctitle(1970)
reg lnhouse `demographics' lny interaction [pw=hhwt] if year==1980, cl(statefip)
outreg2 lny interaction using "$out/migReturnPrice.txt",  append ctitle(1980)
reg lnhouse `demographics' lny interaction [pw=hhwt] if year==1990, cl(statefip)
outreg2 lny interaction using "$out/migReturnPrice.txt",  append ctitle(1990)
reg lnhouse `demographics' lny interaction [pw=hhwt] if year==2000, cl(statefip)
outreg2 lny interaction using "$out/migReturnPrice.txt",  append ctitle(2000)
reg lnhouse `demographics' lny interaction [pw=hhwt] if year==2010, cl(statefip)
outreg2 lny interaction using "$out/migReturnPrice.txt",  append ctitle(2010) excel
