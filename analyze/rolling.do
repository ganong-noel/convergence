
use $work/state, clear

keep if statea != "AK" & statea != "HI" & statea != "DC"

******************
*MAKE SUMMARY STATS
*********************
preserve
#delimit;
gen percap = beaInc/pop;
replace value = value/1000;
collapse (mean) percapMean=percap (sd) percapSd = percap
(mean) popMean=pop (sd) popSd = pop //(mean) hcMean=hc (sd) hcSd = hc
(mean) valueCenMean=value (sd) valueCenSd = value
(mean) luPcMean = luPc (sd) luPcSd = luPc
if year >= 1940 & mod(year,20) == 0, by(year);
reshape long percap pop hc valueCen luPc, i(year) j(type) string;
gen yeartype = string(year) + type;
drop year type;
xpose, clear varname;
#delimit cr;
outsheet using $out/summary.csv, replace comma
restore

mat x = I(16)
mat coefSe = x[1..16, 1..7]
mat fstat = x[1..2, 1..7]
local col = 1
foreach year of numlist 1950(10)2010 {
	qui reg dliBea lagliBea if year == `year', r
	mat coefSe[1,`col'] = _b[lagliBea]
	mat coefSe[2,`col'] = _se[lagliBea]
	if `year' >= 1960 {
	qui reg dliWageCen lagliWageCen if year == `year', r
	mat coefSe[3,`col'] = _b[lagliWageCen]
	mat coefSe[4,`col'] = _se[lagliWageCen]
	qui ivreg dliBea (lagliBea=lagliWageCen) if year == `year', r
	mat coefSe[5,`col'] = _b[lagliBea]
	mat coefSe[6,`col'] = _se[lagliBea]
		qui  reg lagliBea lagliWageCen if year == `year'
		mat fstat[1,`col'] = e(F)
	qui ivreg dliWageCen (lagliWageCen = lagliBea) if year == `year', r
	mat coefSe[7,`col'] = _b[lagliWageCen]
	mat coefSe[8,`col'] = _se[lagliWageCen]
		qui  reg lagliWageCen lagliBea if year == `year'
		mat fstat[2,`col'] = e(F)
	}
	qui reg dlpop lagliBea if year == `year', r
	mat coefSe[9,`col'] = _b[lagliBea]
	mat coefSe[10,`col'] = _se[lagliBea]

	qui reg dpopBD lagliBea if year == `year', r
	mat coefSe[13,`col'] = _b[lagliBea]
	mat coefSe[14,`col'] = _se[lagliBea]
	if `year' <= 1990 {
	qui reg dpopSur lagliBea if year == `year', r
	mat coefSe[15,`col'] = _b[lagliBea]
	mat coefSe[16,`col'] = _se[lagliBea]
	}
	local col = `col'+1
}
 
mat list coefSe
mat list fstat

preserve
clear
svmat coefSe
outsheet using $out/rolling.csv, comma replace
restore

preserve
collapse (sd) liInc if year == 1950 | year == 1960 | year == 1970 | year == 1980 | year == 1990 | year == 2000 | year == 2010, by(year)
xpose, clear
outsheet using $out/sigma.csv, comma replace
restore



*************
*MAKE TABLE FOR PAPER
*************
use $work/lmaData, clear

foreach var of varlist liBea lvalue lpop liCen {
	gen lag`var' = l20.`var'
	gen diff`var' = (`var'-lag`var')*100/20
}

mat coef = I(6)
local col = 1
foreach year of numlist 1960(10)2010 {
	qui reg diffliCen lagliCen if year == `year' [w=l20.pop], r
	mat coef[1,`col'] = _b[lagliCen]
	mat coef[2,`col'] = _se[lagliCen]
	qui reg difflpop lagliCen if year == `year' [w=l20.pop], r
	mat coef[3,`col'] = _b[lagliCen]
	mat coef[4,`col'] = _se[lagliCen]
	
	local col = `col'+1
}

mat list coef
preserve
clear
svmat coef
outsheet using $out/rollingLma.csv, replace comma
restore
