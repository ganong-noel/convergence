
foreach year of numlist 1940 2000 {
forv j=1/5 {
use $work/BORJAS`year'ROBUST`j', clear
drop if statefip==2 | statefip==11 | statefip==15
replace netMig = netMig*100

foreach var of varlist lincShared lr  {
forv skill=0/1 {
qui reg netMig `var' if skill == `skill' [w=basePop], cl(statefip)

if `j'==1 {
outreg2 `var' using $out/robust`year'`skill'`var', cttop(`j') replace
}

if `j'==5 {
outreg2 `var' using $out/robust`year'`skill'`var', cttop(`j')  excel
}

if `j'!=5 & `j'!=1 {
outreg2 `var' using $out/robust`year'`skill'`var', cttop(`j') 
}
}
}

}
}
