

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
