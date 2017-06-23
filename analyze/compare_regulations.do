

* Create Early 80s Wharton Measure
u $src/wudpdat2_from_raven_saks, clear
sort metro
merge 1:1 metro using $src/metro_state, nogen keep(match)
egen oldwhartonindex=rowmean(dlandus1 dlandus2)
collapse (mean) oldwhartonindex [w=pop82], by(stateabbrev)
sort stateabbrev

* Merge in Raven Saks Measure
merge 1:1 stateabbrev using $src/other_regs_raven_saks, nogen
sort stateabbrev
tempfile a
save `a', replace

use "${work}/state.dta", clear

merge m:1 stateabbrev using `a'

pwcorr planner oldwhartonindex WRLURI raven BinHundred if year==1990, sig
