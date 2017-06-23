insheet using dataforgraphcalib516nh.csv,clear
rename v1 lnh
rename v2 lnl
rename v3 lsh
rename v4 lsl
rename v5 qh
rename v6 ql
rename v7 wnh
rename v8 wnl
rename v9 wsh
rename v10 wsl
rename v11 p
rename v12 ps
rename v13 nnh
rename v14 nnl
rename v15 nsh
rename v16 nsl
gen t=_n
drop if t<3 | t>13
replace t=t-2
save  newperiod1, replace

insheet using dataforgraphcalib516nhb.csv,clear
rename v1 lnh
rename v2 lnl
rename v3 lsh
rename v4 lsl
rename v5 qh
rename v6 ql
rename v7 wnh
rename v8 wnl
rename v9 wsh
rename v10 wsl
rename v11 p
rename v12 ps
rename v13 nnh
rename v14 nnl
rename v15 nsh
rename v16 nsl
gen t=_n+11
append using newperiod1
save newdate, replace

tsset t

g ynh = wnh*lnh
g ynl = wnl*lnl
g ysh = wsh*lsh
g ysl = wsl*lsl

g popn = nnh + nnl
g pops = nsh + nsl

g incpcn=(ynh*nnh+ynl*nnl)/popn
g incpcs=(ysh*nsh+ysl*nsl)/pops


g housenh =p*( .25+ (1-.94)*(wnh*lnh/p ))/ynh
g housenl =p*( .25+ (1-.94)*(wnl*lnl/p ))/ynl


g housesh =( .25+ (1-.94)*(wsh*lsh))/ysh
g housesl =( .25+ (1-.94)*(wsl*lsl))/ysl

g slope =ln(p)/ln(incpcn/incpcs)

gen dlnpop=( ln(popn/L.popn)    - ln(pops/L.pops)   ) /  ln(L.incpcn/L.incpcs)
gen dlnpoph=( ln(nnh/L.nnh)     - ln(nsh/L.nsh)     ) /  ln(L.incpcn/L.incpcs)
gen dlnpopl=( ln(nnl/L.nnl)     - ln(nsl/L.nsl)     ) /  ln(L.incpcn/L.incpcs)
gen dlninc=( ln(incpcn/L.incpcn)     - ln(incpcs/L.incpcs)     ) /  ln(L.incpcn/L.incpcs)

drop if t<3 | t>30
scatter dlninc t || scatter dlnpop t || scatter dlnpopl t || sc dlnpoph t 

drop if t == 12 | t == 13
#delimit ; 
twoway sc dlninc t, mcolor(edkblue) || sc dlnpopl t , mcolor(edkblue) msymbol(oh) || 
sc dlnpoph t , mcolor(edkblue) msymbol(dh) 
xline(12, lcolor(black)) yline(0, lcolor(gs14))
graphregion(fcolor(white)) , legend(off)
ylabel(0, nogrid) xlabel(3 "t{sub:0}" 12 "t{sub:1}") 
text(-.008 3 "Inc Converge Rate", place(e))
text(.02 12 "Unanticipated Reg Increase", place(se))
text(.01 30 "Mig Rate S{&rarr}N (Skilled)", place(w))
text(.023 3 "Mig S{&rarr}N (Unskilled)", place(e))
text(.016 3 "Mig S{&rarr}N (Skilled)", place(e))
text(.003 30 "Inc Converge Rate", place(w))
text(-.003 30 "Mig Rate S{&rarr}N (Unskilled)", place(w))
xtitle("Time") ytitle(Rate of Convergence / Migration)
ylabel(-.01(.01).02) ysc(r(.025));
gr export ../graph/calib.pdf, replace;

