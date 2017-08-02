
global all = "graphregion(fcolor(white))"
************
*GRAPH
***********

use hcAggBaseBplY hcAggBaseResY dHc statea year using $work/state, clear

mat x = I(8)
local row = 1
foreach year of numlist 1960(10)2010 {
	qui reg  dHc hcAggBaseBplY if year == `year'
	mat x[`row',4] = _b[hcAggBaseBplY]
	mat x[`row',5] = _se[hcAggBaseBplY]	
	local row = `row'+1
}

global coef1 = substr(string(x[1,4]),1,4) 
global se1 = substr(string(x[1,5]),1,3) 
global rho1 = substr(string(x[1,3]),1,4) 
global coef2 = substr(string(x[6,4]),1,4) 
global se2 = substr(string(x[6,5]),1,3) 
global rho2 = substr(string(x[6,3]),1,4) 

mat list x

preserve
clear
svmat x
#delimit;
keep x4-x5;
gen year = _n*10+1950;
scatter x4 year if year <= 2010, mcolor(edkblue) c(l) ||
scatter x4 year if year == 1960, mcolor(maroon) msize(huge) ||
scatter x4 year if year == 2010, mcolor(purple) msize(huge)
ytitle("Human Capital Convergence, 20-Year Window")  xlabel(1960(10)2010) 
ylabel(-0.3(0.1)0,nogrid) legend(off) 
subtitle("Extent of Human Capital Convergence due to Migration Over Time") xtitle(" ")
saving($work/hcConvergeCoefs.gph, replace) graphregion(fcolor(white));
gr export $out/hcConvergeCoefs.eps, replace ;
restore;

************
*GRAPH
**********;

#delimit;
scatter  dHc hcAggBaseBplY  if year == 1960, mlabel(statea) msymbol(i) 
|| lfit  dHc hcAggBaseBplY  if year == 1960, legend(off)  lcolor(maroon) 
xtitle("Human Capital of People Born in State")
ytitle("Human Cap of Residents - Human Cap of People Born in State")
subtitle("1960  Coef: $coef1 SE: $se1 ", size(large)) 
saving($work/hcConverge1960.gph, replace) graphregion(fcolor(white));


#delimit;
scatter dHc hcAggBaseBplY   if year == 2010, mlabel(statea) msymbol(i) 
|| lfit  dHc hcAggBaseBplY  if year == 2010, legend(off) lcolor(purple)
ytitle("Human Cap of Residents - Human Cap of People Born in State")
xtitle("Human Capital of People Born in State") ysc(r(-1000 1000)) ylabel(-1000(500)1000)
subtitle("2010 Coef: $coef2 SE: $se2", size(large))
saving($work/hcConverge2010.gph, replace) graphregion(fcolor(white));



#delimit;
gr combine $work/hcConverge1960.gph $work/hcConverge2010.gph,
graphregion(fcolor(white)) cols(2) xsize(4.5) ysize(3.5);
gr export $out/hcConverge.eps, replace ;
