global demo ="fractionwhite fractionblack fractionmale agepw agepw2 skill people"

use $work/lastsave19,clear
ivreg scaled_income ${demo} (unskill_interaction skill_interaction =instrument_uninter instrument_inter) [pw=hhwt] if year==1980, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnIv.txt",  replace ctitle(1980)
ivreg scaled_income ${demo} (unskill_interaction skill_interaction =instrument_uninter instrument_inter) [pw=hhwt] if year==1990, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnIv.txt",  append ctitle(1990)

reg scaled_income  unskill_interaction skill_interaction ${demo} [pw=hhwt] if year==1980, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnOls.txt",  replace ctitle(1980)
reg scaled_income  unskill_interaction skill_interaction ${demo} [pw=hhwt] if year==1990, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnOls.txt",  append ctitle(1990)


use $work/lastsave20, clear
ivreg scaled_income ${demo} (unskill_interaction skill_interaction =instrument_uninter instrument_inter) [pw=hhwt] if year==1960, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnIv.txt",  append ctitle(1960)
ivreg scaled_income ${demo} (unskill_interaction skill_interaction =instrument_uninter instrument_inter) [pw=hhwt] if year==1970, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnIv.txt",  append ctitle(1970)
reg scaled_income ${demo} unskill_interaction skill_interaction  [pw=hhwt] if year==1960, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnOls.txt",  append ctitle(1960)
reg scaled_income ${demo} unskill_interaction skill_interaction  [pw=hhwt] if year==1970, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnOls.txt",  append ctitle(1970)

use $work/lastsave21, clear
ivreg scaled_income ${demo} (unskill_interaction skill_interaction =instrument_uninter instrument_inter) [pw=hhwt] if year==1940, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnIv.txt",  append ctitle(1940)
ivreg scaled_income ${demo} (unskill_interaction skill_interaction =instrument_uninter instrument_inter) [pw=hhwt] if year==2000, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnIv.txt",  append ctitle(2000)
ivreg scaled_income ${demo} (unskill_interaction skill_interaction =instrument_uninter instrument_inter) [pw=hhwt] if year==2010, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnIv.txt",  append ctitle(2010) excel

reg scaled_income ${demo} unskill_interaction skill_interaction [pw=hhwt] if year==1940, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnOls.txt",  append ctitle(1940)
reg scaled_income ${demo} unskill_interaction skill_interaction [pw=hhwt] if year==2000, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnOls.txt",  append ctitle(2000)
reg scaled_income ${demo} unskill_interaction skill_interaction [pw=hhwt] if year==2010, cl(statefip)
outreg2 unskill_interaction skill_interaction using "$out/migReturnOls.txt",  append ctitle(2010) excel




