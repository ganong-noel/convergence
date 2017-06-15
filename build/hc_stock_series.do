clear all
set mem 5g
set more off

use $work/hcSamp.dta, clear
 
global vars = "eduBin black hispanic"
egen hcBin = group($vars)
qui sum hcBin
global max = r(max)

preserve
gen count = 1
collapse count, by(hcBin $vars)
drop count
save "$work/xwalkHcConverge", replace
restore

qui tab hcBin, gen(hcBind)
reg linc hcBind* if year == 1980 [w=perwt], nocons

	preserve
	mat y = e(b)
	clear
	svmat y
	keep y1-y$max
	xpose, clear
	rename v1 incGroup
	gen hcBin = _n
sort hcBin
merge m:1 hcBin using "$work/xwalkHcConverge", assert(3) nogen
sort $vars 
save "$work/xwalkHcConverge2", replace
restore


sort $vars
merge m:1 $vars  using "$work/xwalkHcConverge2", assert(3) nogen
gen hcAgg = exp(incGroup)
gen hcAggY = hcAgg if age <= 34

preserve
collapse hcAggBaseRes = hcAgg hcAggBaseResY = hcAggY [w=perwt] if statefip != 99, by(statefip year)
save "$work/lucasHcConverge_im", replace	
restore


keep if bpl != 99 & bpl != 0 & bpl != 2 & bpl != 15 & bpl !=11
collapse  hcAggBaseBpl = hcAgg  hcAggBaseBplY = hcAggY [w=perwt], by(bpl year)
rename bpl statefip
merge 1:1 statefip year using "$work/lucasHcConverge_im", nogen
save "$work/lucasHcConverge_im", replace

u "$work/lucasHcConverge_im", clear
gen dHc = hcAggBaseResY - hcAggBaseBplY
gen dlogHc = log(hcAggBaseResY) - log(hcAggBaseBplY)
bys year: summ dlogHc






