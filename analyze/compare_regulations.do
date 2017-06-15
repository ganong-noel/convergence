

* Create Early 80s Wharton Measure
u wudpdat2, clear
sort metro
merge 1:1 metro using metro_state, nogen keep(match)
egen oldwhartonindex=rowmean(dlandus1 dlandus2)
collapse (mean) oldwhartonindex [w=pop82], by(stateabbrev)
sort stateabbrev

* Merge in Raven Saks Measure
merge 1:1 stateabbrev using raven, nogen
sort stateabbrev
tempfile a
save `a', replace

if "`c(username)'" == "peterganong" {
	use "../../draft3/state/state.dta", clear
}
else {
	use "..\..\draft3\state\state.dta", clear
}
sort stateabbrev

merge m:1 stateabbrev using `a'

pwcorr planner oldwhartonindex WRLURI raven BinHundred if year==1990, sig
