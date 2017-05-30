
clear all
set more off
set matsize 8000

*** Prep
use src/usa_00034.dta if gq == 1 & age>= 25 & age<=65, clear

* Education Dummy and Demeaning
qui tab educd, gen(educdum) 

forv j=1/24 {
cap egen m`j' = wtmean(educdum`j'), weight(perwt)
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
collapse (mean) predictedincome  hhwt hhincome rent valueh [w=perwt], by (serial metarea statefip)

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
binscatter housingfrac predictedincome, controls(metareaD*) weight(hhwt) line(qfit)  lcolors(maroon) reportreg 
ytitle("Fraction of Household Income Spent on Housing") 
xtitle("Average Income per Adult in HH (Instrumented with Education)") 
subtitle("MSA Fixed Effects") title("Household Level: Housing Share of Income") legend(off);
	gr save "../graph/t/panelb.gph", replace;

	#delimit;
collapse (mean)  predictedincome housingfrac [w=hhwt], by (metarea);
binscatter housingfrac predictedincome, lcolors(maroon) ytitle("Fraction of Household Income Spent on Housing") 
xtitle("Average MSA Income (Instrumented with Education)") 
title("MSA-Level: High Housing Share vs. High Education") legend(off);
	gr save "../graph/t/panela.gph", replace;
#delimit cr;

gr combine ../graph/t/panela.gph ../graph/t/panelb.gph, xsize(5) ysize(7) graphregion(fcolor(white)) cols(1)
gr export ../graph/nonhomo.pdf, replace


gr use ../graph/t/panelb.gph
gr export ../graph/nonhomoBottom.pdf, replace






