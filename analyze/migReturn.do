insheet using $out/migReturnOls.txt, clear tab

foreach var of varlist v2-v8 {
	replace `var' = substr(`var',1,5) if  substr(`var',-1,1) =="*"
	replace `var' = substr(`var',2,6) if  substr(`var',1,1) =="("
	replace `var' = substr(`var',1,5) if  substr(`var',-1,1) ==")"
}
destring _all, replace force
 keep if _n == 2 | _n >= 4 & _n <= 7
 xpose, clear

 rename v1 year
 rename v2 coefUnskill
 rename v4 coefSkill
 gen unskillLow = coefUnskill + 1.96*v3
gen unskillHigh = coefUnskill - 1.96*v3
gen skillLow = coefSkill + 1.96*v5
gen skillHigh = coefSkill - 1.96*v5
 
gen yearJit = year+1
 
 #delimit;
 sc coefUnskill year || rcap unskillLow unskillHigh year, color(navy) ||
sc coefSkill  yearJit, color(maroon) m(D) || rcap skillLow skillHigh yearJit, color(maroon)
graphregion(fcolor(white)) xtitle("") ytitle("Coef") xlabel(1940 1960(10)2010)
legend(order(1 "Unskilled HH" 3 "Skilled HH") ring(0) pos(11) region(lstyle(none)))
subtitle(Effect of $1 of Statewide Inc on Skill-Specific Inc Net of Housing);
gr export $out/migReturn.pdf, replace;
