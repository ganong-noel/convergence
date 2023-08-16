ssc install outreg2
use $work/state, clear
 keep if year >= 1940
***********************
*TABLE 1 BASELINE RESULTS
*********************

sort sid year
qui xi: areg permits i.year*BinHundred liInc  interBinHundred ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundred using "$out/reg.txt",  replace
qui xi: areg lvalue i.year*BinHundredCum liInc  interBinHundredCum ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundredCum using "$out/reg.txt",  append
qui xi: areg f20.dlpop i.year*BinHundred liInc  interBinHundred ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundred using "$out/reg.txt",  append
qui xi: areg f20.dlogHc i.year*BinHundredCum liInc  interBinHundredCum ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundredCum using "$out/reg.txt",  append
qui xi: areg f20.dliInc i.year*BinHundred liInc  interBinHundred ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundred using "$out/reg.txt",  append excel

qui xi: areg permits i.year*BinHundredPlacebo liInc  interBinHundredPlacebo ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundredPlacebo using "$out/reg.txt",  append
qui xi: areg lvalue i.year*BinHundredCumPlacebo liInc  interBinHundredCumPlacebo ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundredCumPlacebo using "$out/reg.txt",  append
qui xi: areg f20.dlpop i.year*BinHundredPlacebo liInc  interBinHundredPlacebo ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundredPlacebo using "$out/reg.txt",  append
qui xi: areg f20.dlogHc i.year*BinHundredCumPlacebo liInc  interBinHundredCumPlacebo ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundredCumPlacebo using "$out/reg.txt",  append
qui xi: areg f20.dliInc i.year*BinHundredPlacebo liInc  interBinHundredPlacebo ,a(year) cl(stateabbrev)
outreg2 liInc interBinHundredPlacebo using "$out/reg.txt",  append excel



***********************
*TABLE 2 LATENCY RESULTS
*********************

global post = 1965
sort sid year
qui xi: areg f20.dliInc i.year*roll05 liInc  interroll05 if year <= $post ,a(year) cl(stateabbrev)
outreg2 liInc interroll05 using "$out/regXsec.txt", replace  dec(2)
qui xi: areg f20.dliInc i.year*roll05 liInc  interroll05 if year > $post ,a(year) cl(stateabbrev)
outreg2 liInc interroll05 using "$out/regXsec.txt",  append  dec(2)

qui xi: areg f20.dliInc i.year*roll55 liInc  interroll55 if year <= $post ,a(year) cl(stateabbrev)
outreg2 liInc interroll55 using "$out/regXsec.txt",  append dec(2)
qui xi: areg f20.dliInc i.year*roll55 liInc  interroll55 if year > $post ,a(year) cl(stateabbrev)
outreg2 liInc interroll55 using "$out/regXsec.txt",  append  dec(2)

sort sid year
qui xi: areg f20.dliInc i.year*roll05 liInc liIncPost interroll05 interroll05post, a(year)  cl(stateabbrev)
test interroll05post
qui xi: areg f20.dliInc i.year*roll55 liInc liIncPost interroll55 interroll55post, a(year)  cl(stateabbrev)
test  interroll55post

preserve
use $work/countyUnaval, clear
qui xi: areg dliInc i.year*UBinFive liInc  interactUBinFive if year <= $post ,a(year) cl(lma)
outreg2 liInc interactUBinFive using "$out/regXsec.txt",  append dec(2)
qui xi: areg dliInc i.year*UBinFive liInc  interactUBinFive if year > $post ,a(year) cl(lma)
outreg2 liInc interactUBinFive using "$out/regXsec.txt",  append dec(2) excel
qui xi: areg dliInc i.year*UBinFive liInc liIncPost interactUBinFive interactUBinFivepost ,a(year) cl(lma)
test interactUBinFivepost
restore



***********************
*APPENDIX TABLE 5 ROBUSTNESS
*********************
sort sid year
qui xi: areg permits i.year*winsor liInc  interwinsor ,a(year) cl(stateabbrev)
outreg2 liInc interwinsor using "$out/regRobust.txt",  replace
qui xi: areg lvalue i.year*winsorCum liInc  interwinsorCum ,a(year) cl(stateabbrev)
outreg2 liInc interwinsorCum using "$out/regRobust.txt",  append
qui xi: areg f20.dlpop i.year*winsor liInc  interwinsor ,a(year) cl(stateabbrev)
outreg2 liInc interwinsor using "$out/regRobust.txt",  append
qui xi: areg f20.dlogHc i.year*winsorCum liInc  interwinsorCum ,a(year) cl(stateabbrev)
outreg2 liInc interwinsorCum using "$out/regRobust.txt",  append
qui xi: areg f20.dliInc i.year*winsor liInc  interwinsor ,a(year) cl(stateabbrev)
outreg2 liInc interwinsor using "$out/regRobust.txt",  append excel


sort sid year
qui xi: areg permits i.year*BinTwo liInc  interBinTwo ,a(year) cl(stateabbrev)
outreg2 liInc interBinTwo using "$out/regRobust.txt",  append
qui xi: areg lvalue i.year*BinTwoCum liInc  interBinTwoCum ,a(year) cl(stateabbrev)
outreg2 liInc interBinTwoCum using "$out/regRobust.txt",  append
qui xi: areg f20.dlpop i.year*BinTwo liInc  interBinTwo ,a(year) cl(stateabbrev)
outreg2 liInc interBinTwo using "$out/regRobust.txt",  append
qui xi: areg f20.dlogHc i.year*BinTwoCum liInc  interBinTwoCum ,a(year) cl(stateabbrev)
outreg2 liInc interBinTwoCum using "$out/regRobust.txt",  append
qui xi: areg f20.dliInc i.year*BinTwo liInc  interBinTwo ,a(year) cl(stateabbrev)
outreg2 liInc interBinTwo using "$out/regRobust.txt",  append excel


qui xi: areg permits i.year*BinZone liInc  interBinZone ,a(year) cl(stateabbrev)
outreg2 liInc interBinZone using "$out/regRobust.txt",  append
qui xi: areg lvalue i.year*BinZoneCum liInc  interBinZoneCum ,a(year) cl(stateabbrev)
outreg2 liInc interBinZoneCum using "$out/regRobust.txt",  append
qui xi: areg f20.dlpop i.year*BinZone liInc  interBinZone ,a(year) cl(stateabbrev)
outreg2 liInc interBinZone using "$out/regRobust.txt",  append
qui xi: areg f20.dlogHc i.year*BinZoneCum liInc  interBinZoneCum ,a(year) cl(stateabbrev)
outreg2 liInc interBinZoneCum using "$out/regRobust.txt",  append
qui xi: areg f20.dliInc i.year*BinZone liInc  interBinZone ,a(year) cl(stateabbrev)
outreg2 liInc interBinZone using "$out/regRobust.txt",  append excel




***************************
*BEGIN GRAPHS
*************************

**************
*FIGURE 7
**************

use $work/state, clear
global xvar = "luPcCum"
sum $xvar, d
gen lowE = $xvar >= r(p50)
gen interactLowE = lowE*liInc


**********************************************
**** Make Time Series of Regulation Graph*****
***********************************************
preserve
collapse (sum) pop docLu if year >= 1940, by(year)
gen LUpc = 10^6*docLu/pop
sort year
#delimit;
line LUpc year , graphregion(fcolor(white)) lp("-") 
ytitle("Cases Per Million People") xtitle(" ") legend(order(1 "Land Use Cases")
ring(0) pos(11) region(lstyle(none)) cols(1)) 
subtitle("Land Use Cases Per Million People") xlabel(1940(10)2010) 
saving($work/timeseries.gph, replace) nodraw;
#delimit cr;
restore



**********************************************
**** Make Wharton and AIP Graph*****
***********************************************
qui reg WRLURI roll2005t if year==2005
local b =round(_b[roll2005t],.001)
local se =round(_se[roll2005t],.001)
local firsty =round(e(r2),.01)
#delimit;
scatter WRLURI roll2005t if year == 2005, mlabel(statea) msymbol(i) || 
lfit  WRLURI roll2005t if year == 2005, graphregion(fcolor(white)) lwidth(thick)
xsize(5.5) ysize(4.25) ytitle("Wharton Index of Regulation") 
xtitle("Rank of Land Use Cases Per Capita, 1995-2005") 
subtitle("Land Use Cases vs 2005 Survey, Coef: `b' SE: `se'") 
legend(off) saving($work/Wrluri.gph, replace) nodraw;
#delimit cr;

xtile roll75t = luPcCum if year == 1975, n(48)
qui reg planner roll75t if year==1975
local b =round(_b[roll75t],.01)
local se =round(_se[roll75t],.01)
#delimit;
scatter planner roll75t if year == 1975, mlabel(statea) msymbol(i) || 
lfit planner roll75t  if year == 1975, graphregion(fcolor(white))  lwidth(thick)
xsize(5.5) ysize(4.25) ytitle("American Institute of Planners Measure") 
xtitle("Rank of Land Use Cases Per Capita, 1965-1975") 
subtitle("Land Use Cases vs 1975 Survey, Coef: `b' SE: `se'") 
legend(off) saving($work/Aip.gph, replace) nodraw;
#delimit cr;


***************************
****GRAPH THE FIRST STAGE****
***************************

preserve
sum liInc
scalar meanInc = r(mean)
sum lvalue
scalar meanValue = r(mean)

keep if mod(year,10) == 0 & year >= 1940

qui xi: reg liInc i.year*lowE   interactLowE if lvalue != .
predict incResidBase, r
qui xi: reg interactLowE  i.year*lowE liInc  if lvalue != .  
predict incResidInteract, r
reg lvalue incResidBase
reg lvalue  incResidInteract

qui xi: reg lvalue i.year*lowE
predict lvalueResid, r

gen resid = incResidBase if lowE == 0
replace resid = incResidInteract if lowE == 1
xtile liIncBin =  resid, n(10) 
collapse lvalue lvalueResid resid , by(liIncBin lowE) 
replace resid = resid+meanInc
replace lvalueResid = lvalueResid+meanValue

#delimit;
sc lvalueResid  resid if lowE == 0 || 
sc lvalueResid  resid if lowE == 1, m(D) || 
lfit lvalueResid  resid  if lowE == 0, color(navy) || 
lfit lvalueResid  resid  if lowE == 1, color(maroon)
graphregion(fcolor(white)) xtitle(Log Income) ytitle(Log Housing Value)
legend(order(1 "Low Reg State-Years" 2 "High Reg State-Years")
ring(0) pos(11) cols(1) region(lstyle(none))) ylabel(11(0.5)12)
subtitle(Regulations Capitalize Incomes into Prices)
saving($work/FirstStage.gph, replace) nodraw;
#delimit cr;
restore


gr combine $work/timeseries.gph $work/Aip.gph $work/Wrluri.gph  $work/FirstStage.gph, graphregion(fcolor(white)) iscale(*0.8) cols(2)
gr export $out/westlawValidity.eps, replace

foreach name in timeseries Aip Wrluri FirstStage {
	gr use $work/`name'.gph
	gr export $out/`name'.eps, replace
}

**************
*FIGURE 8
**************
import excel using $src/beaPersonalInc.xls, clear cellrange(B6:O57) first
foreach var of varlist _all {
	rename `var' beaIncNew`var'
}

rename beaIncNewA nameBea
reshape long beaIncNew, i(nameBea) j(yearString) string
bys nameBea: gen year = _n + 1999
save $work/bea_personal_inc, replace

use $work/state if year >= 1940, clear
expand 2 if year == 2010, gen(new)
replace year = 2011 if new == 1
expand 2 if year == 2011, gen(new2)
replace year = 2012 if new2 == 1
merge 1:1 nameBea year using $work/bea_personal_inc, nogen update
sort nameBea year
replace sid = sid[_n-1] if year >= 2011
duplicates tag sid year, gen(dup)
drop if dup > 0

sum luPcCum if year == 1965, d
gen highRegTemp = luPcCum > r(p50) if year == 1965
bys statea: egen highReg = max(highRegTemp)

****************************
*EXAMPLES FROM TWO SPECIFIC YEARS
****************************
sort sid year
#delimit; 
scatter dliInc lagliInc if year == 1960 & highReg == 0, mlabel(stateabbrev) mlabcolor(edkblue) msymbol(i)
|| scatter dliInc lagliInc if year == 1960 & highReg == 1, mlabel(stateabbrev) mlabcolor(maroon) msymbol(i)
|| lfit dliInc lagliInc if year == 1960 & highReg == 0, lcolor(edkblue)
|| lfit dliInc lagliInc if year == 1960 & highReg == 1, lcolor(maroon) lpattern("-")
ytitle("{&Delta}Inc, 1940-1960 (Annual Rate)", size(large))  
legend(order(3 "Low Reg" 4 "High Reg") ring(0) pos(7) region(lstyle(none)) cols(1))
xtitle("Log Income Per Cap, 1940", size(large))
ysc(r(1.5 5.5)) ylabel(1 3 5, nogrid) xsc(r(8 10)) xlabel(8(0.5)10) graphregion(fcolor(white))
saving($work/incWest1960.gph, replace) nodraw;


#delimit; 
scatter dliInc lagliInc if year == 2000 & highReg == 0, mlabel(stateabbrev) mlabcolor(edkblue) msymbol(i)
|| scatter dliInc lagliInc if year == 2000 & highReg == 1, mlabel(stateabbrev) mlabcolor(maroon) msymbol(i)
|| lfit dliInc lagliInc if year == 2000 & highReg == 0, lcolor(edkblue)
|| lfit dliInc lagliInc if year == 2000 & highReg == 1, lcolor(maroon) lpattern("-")
ytitle("{&Delta}Inc, 1980-2000 (Annual Rate)", size(large)) 
legend(order(3 "Low Reg" 4 "High Reg") ring(0) pos(11) region(lstyle(none)) cols(1))
xtitle("Log Income Per Cap, 1980", size(large)) graphregion(fcolor(white))
saving($work/incWest2010.gph, replace) nodraw;


**************
*CONSTRUCT ROLLING COEFS AND SIGMA CONVERGENCE
**************
gen liIncNew = log(beaIncNew)
sort sid year
replace dliInc = liIncNew - l20.liInc if year == 2012

* Define and populate the coef matrix
mat coef = J(64, 8, .)
local row = 1
foreach year of numlist 1960/2010 {
    qui reg dliInc lagliInc if year == `year' & saiz > 2 , r
    mat coef[`row', 4] = _b[lagliInc]
    qui reg dliInc lagliInc if year == `year' & saiz < 2 , r
    mat coef[`row', 5] = _b[lagliInc]
    qui reg dliInc lagliInc if year == `year' & !highReg  , r
    mat coef[`row', 7] = _b[lagliInc]
    qui reg dliInc lagliInc if year == `year' & highReg, r
    mat coef[`row', 8] = _b[lagliInc]
    local row = `row' + 1
}

* Extract coefficients as local macros
local coef8 coef[*, 8]
local coef7 coef[*, 7]
local coef5 coef[*, 5]
local coef4 coef[*, 4]

* Scatter Plot 1: Convergence of Coefficients by Regulation Level
scatter `coef8' year if year >= 1960 , mcolor(maroon)  m(D) ///
    || scatter `coef7' year if year >= 1960 & year <= 2010, mcolor(edkblue) ///
    xlabel(1960(10)2010, ) xtitle(" ") ylabel(,nogrid) ///
    ytitle("Convergence for 20-Year Windows at Annual Rate") ///
    legend(order(1 "Coef High Reg States" 2 "Coef Low Reg States") ///
    ring(0) pos(11) rows(2) region(lstyle(none)) size(small)) ///
    subtitle("Split by Land Use Instrument") graphregion(fcolor(white)) ///
    saving("$out/nomConvergeSplitRegs.gph", replace)  nodraw;

* Scatter Plot 2: Convergence of Coefficients by Housing Supply Elasticity
scatter `coef5' year if year >= 1950, mcolor(maroon)  m(D) ///
    || scatter `coef4' year if year >= 1950, mcolor(edkblue) ///
    xlabel(1960(10)2010, ) xtitle(" ") ylabel(,nogrid) ///
    ytitle("Convergence for 20-Year Windows at Annual Rate") ///
    legend(order(1 "Coef Constrained States" 2 "Coef Unconstrained States") ///
    ring(0) pos(11) rows(2) region(lstyle(none)) size(small)) ///
    subtitle("Split by Saiz Housing Supply Elasticity") graphregion(fcolor(white)) ///
    saving("$out/nomConvergeSplit.gph", replace) nodraw;




#delimit;
gr combine $work/incWest1960.gph $work/incWest2010.gph 
$work/nomConvergeSplitRegs.gph $work/nomConvergeSplit.gph, graphregion(fcolor(white)) iscale(*0.8);
graph export $out/incWest.eps, replace;
#delimit cr;

foreach name in incWest1960 incWest2010 nomConvergeSplitRegs nomConvergeSplit {
	gr use $work/`name'.gph
	gr export $out/`name'.eps, replace
}

