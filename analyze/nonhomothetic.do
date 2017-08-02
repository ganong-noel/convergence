
clear all
set more off
set matsize 8000

*** Prep
use ${src}/usa_00034.dta if gq == 1 & age>= 25 & age<=65 , clear

* Education Dummy and Demeaning
qui tab educd, gen(educdum) 

forv j=1/24 {
egen m`j' = wtmean(educdum`j'), weight(perwt)
replace educdum`j' = educdum`j' - m`j'
}

** Get Individual Predicted Income
areg incwage educdum1-educdum23 [pw=perwt],a(metarea)
gen predictedincome = _b[_cons]
forv j=1/23 {
replace predictedincome =predictedincome+ _b[educdum`j']*educdum`j'
}

*
summ incwage predictedincome [w=perwt]

replace predictedincome =. if  labforce!=2
collapse (sum) predictedincome (mean)   hhwt hhincome rent valueh [w=perwt], by (serial metarea statefip)

replace valueh=0 if valueh==9999999
gen yearrent = rent*12
gen yearprice = valueh*.05
gen housingcost = yearrent+yearprice

// housing fraction
gen housingfrac = housingcost/hhincome

* get rid of outliers (drop 4.32%)
drop if housingfrac<0 | housingfrac>1

qui tab metarea, gen(metareaD)

#delimit;
binscatter housingfrac predictedincome [w=hhwt], controls(metareaD*)  n(50) line(none)
ytitle("Fraction of Household Income Spent on Housing") 
xtitle("Average Income per Adult in Household (Instrumented with Education)") 
subtitle("MSA Fixed Effects") title("Household Level: Housing Share of Income") legend(off);
gr export $out/non-homotheticity.eps;
