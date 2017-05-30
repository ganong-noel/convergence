
set more off
clear all


use $work/state if statea != "AK" & statea != "HI" & statea != "DC", clear

global all = "graphregion(fcolor(white))"
global allCombine = "graphregion(fcolor(white)) c(2)"

**************
*CONSTRUCT ROLLING COEFS
**************
mat x = I(64)
mat coef=x[1..64,1..8]
mat se=x[1..64,1..8]



local row = 1
foreach year of numlist 1900 1920 1940 1950/2010 {

		qui reg dliInc lagliInc if year == `year', r
		mat coef[`row',1] = _b[lagliInc]
		mat se[`row',1] = _se[lagliInc]

		 qui reg dlpop lagliInc if year == `year', r
		mat coef[`row',2] = _b[lagliInc]
		mat se[`row',2] = _se[lagliInc]
		if `year' >= 1950 {
			 qui reg dliInc lagliInc if year == `year' & saiz > 2 , r
			mat coef[`row',4] = _b[lagliInc]
			 qui reg dliInc lagliInc if year == `year' & saiz < 2 , r
			mat coef[`row',5] = _b[lagliInc]
		}

		local row = `row'+1
}

local row = 4
foreach year of numlist 1950(10)2010 {
		qui reg lvalue liInc if year == `year', r
		mat coef[`row',6] = _b[liInc]
		mat se[`row',6] = _se[liInc]		
		local row = `row'+10
}


***************
*CALIBRATE SLIDE
*************
*when using all years, weighting was 20, 20,20,10
mat define A = (0,0,0,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
mat B = coef[1..34,1]
mat sumC = A*B
di "Coef conv 1950-1980:" sumC[1,1]/31

mat B = coef[1..34,2]
mat sumC = A*B
di "Coef pop 1950-1980:" sumC[1,1]/31

mat define A = (1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
mat B = coef[35..64,1]
mat sumC = A*B
di "Coef conv 1981-2010:" sumC[1,1]/30

mat B = coef[35..64,2]
mat sumC = A*B
di "Coef pop 1981-2010:" sumC[1,1]/30





#delimit;
preserve;
clear;
svmat coef;
gen year = _n+1946 if _n >= 4;

drop if _n <= 3;
foreach num of numlist 1/6 {;
	replace coef`num' = . if coef`num' == 0 | coef`num' == 1;
};


#delimit;
scatter coef1 year if year != 1960 & year != 2010, mcolor(edkblue) ||
scatter coef1 year if year == 1960, mcolor(maroon) msize(huge) ||
scatter coef1 year if year == 2010, mcolor(purple) msize(huge)
ytitle(" ") xtitle(" ") xlabel(1950(10)2010) ylabel(-3 -2 -1 0, nogrid) 
 subtitle("Convergence Rates Over Time") yline(0, lcolor(gray))
ytitle("Convergence for 20-Year Windows at Annual Rate") $all
legend( order(1 "Annual Inc Conv Rate") region(lcolor(white)) rows(2) ring(0) pos(5))
saving($work/nomConverge.gph, replace)  xsize(5.5) ysize(4.5);
gr export $out/nomConverge.pdf, replace ;
//gr export ../slides/t/nomConverge.pdf, replace ;



scatter coef1 year if year != 1960 & year != 2010, mcolor(edkblue) ||
scatter coef1 year if year == 1960, mcolor(maroon) msize(huge) ||
scatter coef1 year if year == 2010, mcolor(purple) msize(huge) ||
scatter coef2 year if year != 1960 & year != 2010, mcolor(edkblue) mfcolor(white) ||
scatter coef2 year if year == 1960, mcolor(maroon) msize(huge) mfcolor(white) ||
scatter coef2 year if year == 2010, mcolor(purple) msize(huge) mfcolor(white)
ytitle(" ") xtitle(" ") xlabel(1950(10)2010, ) ylabel(-3 -2 -1 0 1 2, nogrid )
yline(0, lcolor(gray))
subtitle("Convergence and Directed Migration Rates Over Time")
ytitle("Coefs for 20-Year Windows at Annual Rate")
legend( order(1 "Annual Inc Conv Rate" 4 "Annual Directed Mig Rate") region(lcolor(white)) rows(2) ring(0) pos(5)) $all
saving($work/nomConvergePop.gph, replace) ;
gr export $out/nomConvergePop.pdf, replace ;
//gr export ../slides/t/nomConvergePop.pdf, replace;


scatter coef1 year if year != 1960 & year != 2010 & year <= 1980, mcolor(edkblue) ||
scatter coef1 year if year != 1960 & year != 2010 & year > 1980, mcolor(edkblue*0.2) ||
scatter coef1 year if year == 1960, mcolor(maroon) msize(huge) ||
scatter coef1 year if year == 2010, mcolor(purple*0.2) msize(huge) ||
scatter coef2 year if year != 1960 & year != 2010 & year <= 1980, mcolor(edkblue) mfcolor(white) ||
scatter coef2 year if year != 1960 & year != 2010 & year > 1980, mcolor(edkblue*0.2) mfcolor(white) ||
scatter coef2 year if year == 1960, mcolor(maroon) msize(huge) mfcolor(white) ||
scatter coef2 year if year == 2010, mcolor(purple*0.2) msize(huge) mfcolor(white)
ytitle(" ") xtitle(" ") xlabel(1950(10)2010, ) ylabel(-3 -2 -1 0 1 2, nogrid )
yline(0, lcolor(gray))
subtitle("Convergence and Directed Migration Rates Over Time")
ytitle("Coefs for 20-Year Windows at Annual Rate")
legend( order(1 "Annual Inc Conv Rate" 5 "Annual Directed Mig Rate") region(lcolor(white)) rows(2) ring(0) pos(5)) $all;
//graph export ../slides/t/nomConvergePopFade.pdf, replace;

scatter coef1 year if year != 1960 & year != 2010 & year <= 1980, mcolor(edkblue*0.2) ||
scatter coef1 year if year != 1960 & year != 2010 & year > 1980, mcolor(edkblue) ||
scatter coef1 year if year == 1960, mcolor(maroon*0.2) msize(huge) ||
scatter coef1 year if year == 2010, mcolor(purple) msize(huge) ||
scatter coef2 year if year != 1960 & year != 2010 & year <= 1980, mcolor(edkblue*0.2) mfcolor(white) ||
scatter coef2 year if year != 1960 & year != 2010 & year > 1980, mcolor(edkblue) mfcolor(white) ||
scatter coef2 year if year == 1960, mcolor(maroon*0.2) msize(huge) mfcolor(white) ||
scatter coef2 year if year == 2010, mcolor(purple) msize(huge) mfcolor(white)
ytitle(" ") xtitle(" ") xlabel(1950(10)2010, ) ylabel(-3 -2 -1 0 1 2, nogrid )
yline(0, lcolor(gray))
subtitle("Convergence and Directed Migration Rates Over Time")
ytitle("Coefs for 20-Year Windows at Annual Rate")
legend( order(2 "Annual Inc Conv Rate" 6 "Annual Directed Mig Rate") region(lcolor(white)) rows(2) ring(0) pos(5)) $all;
//graph export ../slides/t/nomConvergePopFade2.pdf, replace;



***Price graphs***;
scatter coef6 year , mcolor(edkblue) c(l) ||
scatter coef6 year if year == 1960, mcolor(maroon) msize(huge) ||
scatter coef6 year if year == 2010, mcolor(purple) msize(huge)
ytitle(" ") xtitle(" ") xlabel(1950(10)2010, ) ylabel(,nogrid)
subtitle("Timeseries of Coefs")
legend(off) ylabel(1 1.5 2) $all
saving($work/priceCoef.gph, replace) ;
gr export $out/priceCoef.pdf, replace ;


restore;

**************
*CONSTRUCT SINGLE-YEAR GRAPHS
**************;



#delimit; 
local coef60 = substr(string(coef[14,1]),1,5);
local se60 = substr(string(se[14,1]),1,3);
scatter dliInc lagliInc if year == 1960, mlabel(statea)  msymbol(i)
|| lfit dliInc lagliInc if year == 1960, lcolor(maroon) 
ytitle("Annual Inc Growth Rate, 1940-1960", size(large))
xtitle("Log Income Per Cap, 1940", size(large))
subtitle("1940-1960, Coef: `coef60' SE: `se60'", size(large)) 
legend(off) ylabel(1 3 5, nogrid) xlabel(#3) $all
saving($work/inc1960.gph, replace) nodraw;

#delimit; 
local coef10 = substr(string(coef[64,1]),1,4);
local se10 = substr(string(se[64,1]),1,3);
scatter dliInc lagliInc if year == 2010, mlabel(statea) msymbol(i)
|| lfit dliInc lagliInc if year == 2010, lcolor(purple) 
ytitle("Annual Inc Growth Rate, 1990-2010", size(large))
xtitle("Log Income Per Cap, 1990", size(large))
subtitle("1990-2010, Coef: `coef10' SE: `se10'", size(large)) legend(off) 
ysc(r(0 4)) xlabel(10 10.4 10.8, nogrid ) ylabel(#3 )
 $all saving($work/inc2010.gph, replace) nodraw;

#delimit;
gr combine $work/inc1960.gph $work/inc2010.gph, $allCombine xsize(5.5) ysize(3.5);
gr export $out/inc.pdf, replace;


#delimit; 
local coef60 = substr(string(coef[14,2]),1,4);
local se60 = substr(string(se[14,2]),1,3);
scatter dlpop lagliInc if year == 1960, mlabel(statea) msymbol(i)
|| lfit dlpop lagliInc if year == 1960,  lcolor(maroon) 
ytitle("Annual Pop Growth Rate, 1940-1960", size(large))
xtitle("Log Income Per Cap, 1940", size(large))
subtitle("1940-1960, Coef: `coef60' SE: `se60'", size(large))
 legend(off) ylabel(#3, nogrid) xlabel(#3) $all
 saving($work/pop1960.gph, replace) nodraw;

#delimit; 
local coef10 = substr(string(coef[64,2]),1,4);
local se10 = substr(string(se[64,2]),1,3);
scatter dlpop lagliInc if year == 2010, mlabel(statea) msymbol(i)
|| lfit dlpop lagliInc if year == 2010,  lcolor(purple) 
ytitle("Annual Pop Growth Rate, 1990-2010", size(large))
xtitle("Log Income Per Cap, 1990", size(large))
subtitle("1990-2010, Coef: `coef10'  SE: `se10'", size(large))
legend(off)  ylabel(#3, nogrid) xlabel(10 10.4 10.8) $all
saving($work/pop2010.gph, replace) nodraw;


#delimit;
gr combine $work/pop1960.gph $work/pop2010.gph,  $allCombine xsize(5.5) ysize(3.5);
gr export $out/popYears.pdf, replace;



#delimit; 
local coef60 = substr(string(coef[14,6]),1,3);
local se60 = substr(string(se[14,6]),1,3);
scatter lvalue liInc if year == 1960, mlabel(statea) msymbol(i)
|| lfit lvalue liInc if year == 1960, lcolor(maroon) 
ytitle("Log Housing Value, 1960", size(large))
xtitle("Log Income Per Cap, 1960", size(large))
subtitle("1960, Coef: `coef60' SE: `se60'", size(large)) 
legend(off) ylabel(10.5(0.5)12, nogrid) xlabel(9.2(0.2)10) $all
saving($work/price1960.gph, replace) nodraw;

#delimit; 
local coef10 = substr(string(coef[64,6]),1,4);
local se10 = substr(string(se[64,6]),1,3);
scatter  lvalue liInc if year == 2010, mlabel(statea) msymbol(i)
|| lfit  lvalue liInc  if year == 2010, lcolor(purple) 
ytitle("Log Housing Value, 2010", size(large))
xtitle("Log Income Per Cap, 2010", size(large))
subtitle("2010, Coef: `coef10' SE: `se10'", size(large)) legend(off) 
ylabel(11.5(0.5)13, nogrid ) xlabel(10.3(0.2)11.1, ) $all
saving($work/price2010.gph, replace) nodraw;

#delimit;
gr combine $work/price1960.gph $work/price2010.gph,  $allCombine xsize(5.5) ysize(3.5);
gr export $out/price.pdf, replace;




