


global all = "graphregion(fcolor(white))"

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

**************
*CONSTRUCT EARLY JOINT GRAPH
**************;
global allSkill = `"$all legend(off)  ysc(r(-2 2))  ylabel(-2 0 2) "'
global allUnskill = `"$all legend(off) ysc(r(-2 2)) ylabel(-2 0 2) "'

local coef = substr(string(coef[2,1]),1,3)
local se = substr(string(coef[2,2]),1,3)

#delimit; 
binscatter netMig lincShared if skill == 1 [w=basePop], 
xtitle("Log Nominal Income") lcolors(maroon)
 ytitle("Net Migration as % Pop") nodraw
 title("Skilled Coef: `coef' SE: `se'", size(medsmall)) $allSkill;
graph save $work/nomSkill1940.gph, replace;

local coef = substr(string(coef[1,1]),1,4);
local se = substr(string(coef[1,2]),1,3);
binscatter netMig lincShared if skill == 0 [w=basePop], 
xtitle("Log Nominal Income") lcolors(maroon) 
ytitle("Net Migration as % Pop")   nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall))  $allUnskill;
graph save $work/nomUnskill1940.gph, replace;

#delimit;
local coef = substr(string(coef[2,3]),1,3);
local se = substr(string(coef[2,4]),1,3);
binscatter netMig lr if skill == 1 [w=basePop],  $allSkill lcolors(maroon)
xtitle("Log (Inc-Housing Cost) for Skilled") 
ytitle("Net Migration as % Pop") nodraw
title("Skilled Coef: `coef' SE: `se'", size(medsmall));
graph save $work/realSkill1940.gph, replace;


#delimit;
local coef = substr(string(coef[1,3]),1,4);
local se = substr(string(coef[1,4]),1,3);
binscatter netMig lr if skill == 0 [w=basePop],  $allUnskill  lcolors(maroon)
xtitle("Log (Inc-Housing Cost) for Unskilled") 
ytitle("Net Migration as % Pop")  nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall));
graph save  $work/realUnskill1940.gph, replace;
#delimit cr;

gr combine $work/nomUnskill1940.gph $work/nomSkill1940.gph $work/realUnskill1940.gph $work/realSkill1940.gph, rows(2) cols(2) graphregion(fcolor(white)) xsize(5.5) ysize(5)
gr export $out/borjas1940.pdf, replace

***************************
*BEGIN 2000 GRAPHS
*********************

global all = "graphregion(fcolor(white))"

use $work/BORJAS2000FINAL, clear

local bin_num =20
replace netMig = netMig*100
local range = "& netMig<40 & netMig>-10"

local weight = "[w=basePopTot]"
mat x = I(6)
mat coef=x[1..2,1..6]

*fwage1 value realWage

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

**************
*CONSTRUCT EARLY JOINT GRAPH
**************;
*#delimit;
*xsc(r(4 6.5)) xlabel(4 6.5, labsize(small)) ;
global allSkill = `" graphregion(fcolor(white)) $all legend(off)  ysc(r(-4 6)) ylabel(-4 0 4 8 )"'
global allUnskill = `"graphregion(fcolor(white)) $all legend(off)  ysc(r(-4 6)) ylabel(-4 0 4 8)"'


local coef = substr(string(coef[2,1]),1,4)
local se = substr(string(coef[2,2]),1,3)

#delimit;
binscatter netMig lincShared if skill == 1 [w=basePop], xtitle("Log Nominal Income") 
 ytitle("Net Migration as % Pop") lcolors(maroon) nodraw
 title("Skilled Coef: `coef' SE: `se'", size(medsmall)) $allSkill;
graph save $work/nomSkill2000.gph, replace;


local coef = substr(string(coef[1,1]),1,5);
local se = substr(string(coef[1,2]),1,4); 
binscatter netMig lincShared if skill == 0 [w=basePop],  xtitle("Log Nominal Income") 
ytitle("Net Migration as % Pop")  lcolors(maroon) nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall))  $allUnskill;
graph save $work/nomUnskill2000.gph, replace;

#delimit;
local coef = substr(string(coef[2,3]),1,4);
local se = substr(string(coef[2,4]),1,3);
binscatter netMig lr if skill == 1  [w=basePop], $allSkill
xtitle("Log (Inc-Housing Cost) for Skilled") 
ytitle("Net Migration as % Pop") lcolors(maroon) nodraw
title("Skilled Coef: `coef' SE: `se'", size(medsmall));
graph save $work/realSkill2000.gph, replace;

local coef = substr(string(coef[1,3]),1,4);
local se = substr(string(coef[1,4]),1,4);
#delimit;
binscatter netMig lr if skill == 0  [w=basePop], $allUnskill 
xtitle("Log (Inc-Housing Cost) for Unskilled") 
ytitle("Net Migration as % Pop")  lcolors(maroon) nodraw
title("Unskilled Coef: `coef' SE: `se'", size(medsmall));
graph save  $work/realUnskill2000.gph, replace;
#delimit cr;

gr combine $work/nomUnskill2000.gph $work/nomSkill2000.gph $work/realUnskill2000.gph $work/realSkill2000.gph, rows(2) cols(2) graphregion(fcolor(white)) xsize(5.5) ysize(5)
gr export $out/borjas2000.pdf, replace

