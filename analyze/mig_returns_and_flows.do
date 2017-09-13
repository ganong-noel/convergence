
***************
***FIGURE 4 -- 1940****
***************

use $work/BORJAS1940FINAL, clear

replace netMig = netMig*100

mat x = I(6)
mat coef=x[1..2,1..6]

local row = 1
foreach skill of numlist 0/1 {
	local col = 1
	foreach var of varlist lincShared lr  {
		qui reg netMig `var' if skill == `skill' [w=basePop], cl(statefip)
		mat coef[`row',`col'] = _b[`var']
		mat coef[`row',`col'+1] = _se[`var']
		local col = `col'+2
	}	
	local row = `row'+1
}
mat list coef

global allSkill = `"graphregion(fcolor(white)) legend(off)  ysc(r(-2 2))  ylabel(-2 0 2) "'
global allUnskill = `"graphregion(fcolor(white)) legend(off) ysc(r(-2 2)) ylabel(-2 0 2) "'

local coef = substr(string(coef[2,1]),1,3)
local se = substr(string(coef[2,2]),1,3)

#delimit; 
binscatter netMig lincShared if skill == 1 [w=basePop], 
xtitle("Log Nominal Income") lcolors(maroon)
 ytitle("Net Migration") nodraw
 title("Skilled Coef: `coef' SE: `se'", size(medsmall)) $allSkill;
graph save $work/nomSkill1940.gph, replace;

local coef = substr(string(coef[1,1]),1,4);
local se = substr(string(coef[1,2]),1,3);
binscatter netMig lincShared if skill == 0 [w=basePop], 
xtitle("Log Nominal Income") lcolors(maroon) 
ytitle("Net Migration")   nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall))  $allUnskill;
graph save $work/nomUnskill1940.gph, replace;

#delimit;
local coef = substr(string(coef[2,3]),1,3);
local se = substr(string(coef[2,4]),1,3);
binscatter netMig lr if skill == 1 [w=basePop],  $allSkill lcolors(maroon)
xtitle("Log (Income-Housing Cost) for Skilled") 
ytitle("Net Migration") nodraw
title("Skilled Coef: `coef' SE: `se'", size(medsmall));
graph save $work/realSkill1940.gph, replace;


#delimit;
local coef = substr(string(coef[1,3]),1,4);
local se = substr(string(coef[1,4]),1,3);
binscatter netMig lr if skill == 0 [w=basePop],  $allUnskill  lcolors(maroon)
xtitle("Log (Income-Housing Cost) for Unskilled") 
ytitle("Net Migration")  nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall));
graph save  $work/realUnskill1940.gph, replace;
#delimit cr;

gr combine $work/nomUnskill1940.gph $work/nomSkill1940.gph $work/realUnskill1940.gph $work/realSkill1940.gph, rows(2) cols(2) graphregion(fcolor(white)) xsize(6) ysize(5)
gr export $out/borjas1940.eps, replace

***************
***FIGURE 5 -- 2000****
***************

global all = "graphregion(fcolor(white))"

use $work/BORJAS2000FINAL, clear

local bin_num =20
replace netMig = netMig*100
local range = "& netMig<40 & netMig>-10"

local weight = "[w=basePopTot]"
mat x = I(6)
mat coef=x[1..2,1..6]

local row = 1
foreach skill of numlist 0/1 {
	local col = 1
	foreach var of varlist lincShared lr  {
		qui reg netMig `var' if skill == `skill' [w=basePop], cl(statefip)
		mat coef[`row',`col'] = _b[`var']
		mat coef[`row',`col'+1] = _se[`var']
		local col = `col'+2
	}	
	local row = `row'+1
}
mat list coef

global allSkill = `" graphregion(fcolor(white)) legend(off)  ysc(r(-4 6)) ylabel(-4 0 4 8 )"'
global allUnskill = `"graphregion(fcolor(white))  legend(off)  ysc(r(-4 6)) ylabel(-4 0 4 8)"'


local coef = substr(string(coef[2,1]),1,4)
local se = substr(string(coef[2,2]),1,3)

#delimit;
binscatter netMig lincShared if skill == 1 [w=basePop], xtitle("Log Nominal Income") 
 ytitle("Net Migration") lcolors(maroon) nodraw
 title("Skilled Coef: `coef' SE: `se'", size(medsmall)) $allSkill;
graph save $work/nomSkill2000.gph, replace;


local coef = substr(string(coef[1,1]),1,5);
local se = substr(string(coef[1,2]),1,4); 
binscatter netMig lincShared if skill == 0 [w=basePop],  xtitle("Log Nominal Income") 
ytitle("Net Migration")  lcolors(maroon) nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall))  $allUnskill;
graph save $work/nomUnskill2000.gph, replace;

#delimit;
local coef = substr(string(coef[2,3]),1,4);
local se = substr(string(coef[2,4]),1,3);
binscatter netMig lr if skill == 1  [w=basePop], $allSkill
xtitle("Log (Income-Housing Cost) for Skilled") 
ytitle("Net Migration") lcolors(maroon) nodraw
title("Skilled Coef: `coef' SE: `se'", size(medsmall));
graph save $work/realSkill2000.gph, replace;

local coef = substr(string(coef[1,3]),1,4);
local se = substr(string(coef[1,4]),1,4);
#delimit;
binscatter netMig lr if skill == 0  [w=basePop], $allUnskill 
xtitle("Log (Income-Housing Cost) for Unskilled") 
ytitle("Net Migration")  lcolors(maroon) nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall));
graph save  $work/realUnskill2000.gph, replace;
#delimit cr;

gr combine $work/nomUnskill2000.gph $work/nomSkill2000.gph $work/realUnskill2000.gph $work/realSkill2000.gph, rows(2) cols(2) graphregion(fcolor(white)) xsize(6) ysize(5)
gr export $out/borjas2000.eps, replace


***************
***APPENDIX TABLE 4****
***************

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
***************
***APPENDIX TABLE 3****
***************
global demo ="fractionwhite fractionblack fractionmale agepw agepw2 skill people"

use $work/lastsave19,clear
ivreg scaled_income ${demo} (avginc skill_interaction =birth_avginc instrument_inter) [pw=hhwt] if year==1980, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnIv.txt",  replace ctitle(1980)
ivreg scaled_income ${demo} (avginc skill_interaction =birth_avginc instrument_inter) [pw=hhwt] if year==1990, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnIv.txt",  append ctitle(1990)
reg scaled_income  avginc skill_interaction ${demo} [pw=hhwt] if year==1980, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnOls.txt",  replace ctitle(1980)
reg scaled_income  avginc skill_interaction ${demo} [pw=hhwt] if year==1990, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnOls.txt",  append ctitle(1990)
reg scaled_income  unskill_interaction skill_interaction ${demo} [pw=hhwt] if year==1980, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnFigure.txt",  replace ctitle(1980)
reg scaled_income  unskill_interaction skill_interaction ${demo} [pw=hhwt] if year==1990, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnFigure.txt",  append ctitle(1990)


use $work/lastsave20, clear
ivreg scaled_income ${demo} (avginc skill_interaction =birth_avginc instrument_inter) [pw=hhwt] if year==1960, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnIv.txt",  append ctitle(1960)
ivreg scaled_income ${demo} (avginc skill_interaction =birth_avginc instrument_inter) [pw=hhwt] if year==1970, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnIv.txt",  append ctitle(1970)
reg scaled_income ${demo} avginc skill_interaction  [pw=hhwt] if year==1960, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnOls.txt",  append ctitle(1960)
reg scaled_income ${demo} avginc skill_interaction  [pw=hhwt] if year==1970, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnOls.txt",  append ctitle(1970)
reg scaled_income ${demo} unskill_interaction skill_interaction  [pw=hhwt] if year==1960, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnFigure.txt",  append ctitle(1960)
reg scaled_income ${demo} unskill_interaction skill_interaction  [pw=hhwt] if year==1970, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnFigure.txt",  append ctitle(1970)

use $work/lastsave21, clear
ivreg scaled_income ${demo} (avginc skill_interaction =birth_avginc instrument_inter) [pw=hhwt] if year==1940, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnIv.txt",  append ctitle(1940)
ivreg scaled_income ${demo} (avginc skill_interaction =birth_avginc instrument_inter) [pw=hhwt] if year==2000, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnIv.txt",  append ctitle(2000)
ivreg scaled_income ${demo} (avginc skill_interaction =birth_avginc instrument_inter) [pw=hhwt] if year==2010, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnIv.txt",  append ctitle(2010) excel
reg scaled_income ${demo} avginc skill_interaction [pw=hhwt] if year==1940, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnOls.txt",  append ctitle(1940)
reg scaled_income ${demo} avginc skill_interaction [pw=hhwt] if year==2000, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnOls.txt",  append ctitle(2000)
reg scaled_income ${demo} avginc skill_interaction [pw=hhwt] if year==2010, cl(statefip)
outreg2 avginc skill_interaction using "$out/migReturnOls.txt",  append ctitle(2010) excel
reg scaled_income ${demo} unskill_interaction skill_interaction [pw=hhwt] if year==1940, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnFigure.txt",  append ctitle(1940)
reg scaled_income ${demo} unskill_interaction skill_interaction [pw=hhwt] if year==2000, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnFigure.txt",  append ctitle(2000)
reg scaled_income ${demo} unskill_interaction skill_interaction [pw=hhwt] if year==2010, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnFigure.txt",  append ctitle(2010) excel
***************
***FIGURE 3****
***************
insheet using $out/migReturnFigure.txt, clear tab

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
graphregion(fcolor(white)) xtitle("") ytitle("Coefficient") xlabel(1940 1960(10)2010)
legend(order(1 "Unskilled Household" 3 "Skilled Household") ring(0) pos(11) region(lstyle(none)))
subtitle(Effect of $1 of Statewide Income on Skill-Specific Inc Net of Housing);
gr export $out/migReturn.eps, replace;
#delimit cr;






