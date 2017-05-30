/*
This pulls together at a county level
--annual house prices to 1975 FHFA (1000 counties)
--housing supply elasticity (770 counties)
--crosswalk to aggregate up to 400 labor market areas
--BEA income pop and emp
If time, also build in county business patterns industry emp
*/

/*
BEA COUNTY ISSUES
4012 LaPaz, AZ starts in 1983
35006 Cibola, NM appears in 1982
55078 Menominee, WI and 55115 Shawano, WI appear in 1989
	through 1988 they are combined as 55901
8014 Broomfield, CO appears in 2002

This appears in the footnote to the BEA tables:
9. Cibola, NM was separated from Valencia in June 1981, but in these estimates, 
Valencia includes Cibola through the end of 1981.
10. La Paz County, AZ was separated from Yuma County on January 1, 1983. 
The Yuma, AZ MSA contains the area that became La Paz County, AZ through 1982 
and excludes it beginning with 1983.
11. Shawano, WI and Menominee, WI are combined as Shawano (incl. Menominee), WI 
for the years prior to 1989.
12. Broomfield County, CO, was created from parts of Adams, Boulder, Jefferson, 
and Weld counties effective November 15, 2001. Estimates for Broomfield county 
begin with 2002.
*/

/*
ad hoc fixes:
*Haines only prints DC once in 1970 - replace fips = 11001 if fips == 11000 & year == 1970
*BEA manual modification: replace fips = 12025 if fips == 12086
*usaCounties did the same
*countyPrices does same

*************
I have forced the following fix up to lma cz:
set obs 3142
replace fips = 51129 if _n == 3142
replace lma = 20 if _n == 3142
replace cz = 2000 if _n == 3142

Haines has the follow that don't match to the LMA list (perhaps disappeared)
	 	11010
	 	16089
	 	29193
	 	32025
	 	46133
	 	51055
	 	51123
	 	51151
	 	51154
	 	51189
	 	51695
	 	51785
	 	56047

*list fips m* if x == . & floor(fips/1000) != 2
*list nameH fips year if x == . & floor(fips/1000) != 2 & mHaines == 2
*/


clear all
set mem 1g


insheet using $src/usaCountiesVirginia.csv, comma clear
replace beafips = fips if beafips == .
drop if fips == 51000
sort fips 
save $work/vaCty, replace


*construct an alternative lma crosswalk, to be applied after the bea merge
*http://usa.ipums.org/usa/volii/1980LMAascii.txt
*OLD CODE NO NAMES
insheet using $src/lma1980.csv, comma clear
gen lma = floor(lmacz/100)
drop totalpopulation laborforce lmacz
drop if fips == .
*n=3137
sort fips
merge 1:1 fips using  $work/vaCty, assert(1 3) nogen
rename fips fipsOld
rename beafips fips
replace fips = fipsOld if fips == .
duplicates drop fips lma, force
drop fipsOld areaname
save $work/lmacz, replace
count

/*
*xx this appears to be just missing about 131 counties! email IPUMS later
*NEW CODE WITH NAMES
insheet using $src/lma1980b.csv, comma clear
destring v1, gen(lma) force
replace lma = floor(lma/100)
gen nameLmaShort = v1 if lma == .
replace lma = lma[_n+2] if lma == .
drop if v1 == "LMA/CZ" | v2 == "Market Area Total"
bys lma: egen nameLma = mode(nameLmaShort)
drop if v2 == ""
destring v2, gen(fips)
keep lma fips nameLma
*n = 3006
sort fips
merge 1:1 fips using  $work/vaCty, assert(1 3) nogen
rename fips fipsOld
rename beafips fips
replace fips = fipsOld if fips == .
duplicates drop fips lma, force
drop fipsOld areaname
save $work/lmacz, replace
count

*/


*xx put 1999 MSA den's here
*http://www.census.gov/population/estimates/metro-city/99mfips.txt

insheet using $src/msa1999.csv, comma clear
gen nameMsaShort = name if fips == .
replace pmsa = msa if pmsa == .
bys pmsa: egen nameMsa = mode(nameMsaShort)
drop if fips == .
bys fips: egen pmsaFinal = mode(pmsa)
*it's confused about fips 9001 in CT. Assigning it to Stamford.
replace pmsaFinal=8040 if fips == 9001
drop nameMsaShort name msa pmsa
duplicates drop fips pmsaFinal, force
sort fips
merge 1:1 fips using  $work/vaCty, keep(1 3) nogen
rename fips fipsOld
rename beafips fips
replace fips = fipsOld if fips == .
drop fipsOld areaname
drop if fips > 72000
duplicates drop
sort fips
save $work/msa, replace


***************
*MERGE USING NORMAL FIPS CODES
***************
*from data/haines/clean.do
use $src/haines, clear
rename name nameHaines

*merge in usa counties from data/usaCounties
sort fips year
merge 1:1 fips year using $src/usaCounties, update
replace incMedian = inc if year >= 1980
replace incMedian = incMedHH if year == 2010
replace incMedian = incPerWorkerMfg if year == 1940
rename _m mUsaCounties

sort fips
merge m:1 fips using $src/saiz_county_crosswalk, assert(1 3) keepusing(unaval elasticity WRLURI)
rename _m mSaiz

*drop states -- can pull them out later if you want. appear in haines and usacounties
drop if mod(fips, 1000) == 0


***************
*MERGE USING AUGMENTED BEA FIPS CODES
***************
sort fips
merge m:1 fips using  $work/vaCty
count if floor(fips/1000) == 51 & _m != 3
rename _m mBeaVirginia
rename fips fipsOld
rename beafips fips
replace fips = fipsOld if fips == .
sort year fips
list fips nameHaines nameUsaCounties year if mod(year, 10) == 0 & year <= 1970 & year >= 1940 & totpop == . & floor(fips/1000) != 2
bys fips year: egen nameUsaCountiesMode = mode(nameUsaCounties)
bys fips year: egen nameHainesMode = mode(nameHaines)


*implies equal weighting for all these variables among the Virginia counties to be combined
replace totpop = 1 if totpop == .
#delimit;
collapse (rawsum) emp totpop valueCount rentCount area adult lf fb count65 permit permitValue 
(mean) m* share65 incMedian shareHS netMig value rent 
incPerWorkerSS incPerWorkerMfg incPerWorkerRetail incPerWorkerServices shareBa incMedHH density 
commute ssRecip edu elasticity WRLURI unaval
 [w=totpop], by(fips year nameUsaCountiesMode nameHainesMode);
#delimit cr;


**MERGE ON BEA DATA -- this file comes from: bea/insheetCounty.do
sort fips year
merge 1:1 fips year using $src/countySA04
rename _m mBeaCounty
list nameBea nameU fips mBeaCounty if mBeaCounty <= 2 & year == 1970 & floor(fips/1000) != 2


********************************
***construct vars of interest***
********************************
gen pop = totpop if year == 1940 | year == 1950 | year == 1960
replace pop = vpop if year >= 1969
replace pop = . if pop <= 3

**ad hoc fix for not having 2010 county populations
sort fips year
replace pop = pop[_n-1] if year == 2010
foreach name in inc tra cap wage {
	rename v`name' `name'Bea
}

*ad hoc fix for not having 1950 unit counts
sort fips year
replace valueCount = (valueCount[_n-1] + valueCount[_n+2])/2 if year == 1950
replace rentCount = (rentCount[_n-1] + rentCount[_n+2])/2 if year == 1950


*evaluate merges
table year mSaiz if year <= 1970 & mod(fips, 1000) != 0, c(sum pop)
table year mBeaCounty if year == 1970 | year == 2000, c(sum pop)


save $work/countyDataPre, replace


********************
*CHOOSE GEOGRAPHIC AGGREGATION LEVEL
************************
use  $work/countyDataPre, clear
sort fips 


merge m:1 fips using $work/msa
*drop HI and AKI
drop if fips == 2020 | fips == 15003
rename pmsaFinal msa
rename _m mMsa

merge m:1 fips using $work/lmacz
*drop alaska
sum fips if lma == 1
drop if lma == 1
rename _m mLma

*this is missing about 1/3 of the pop in 1940 and about 1/4 in 2010
table year mLma  if mod(year,10)==0, c(sum pop)
table year mMsa  if mod(year,10)==0, c(sum pop)


/*
tab fips if floor(fips/1000) != 2 & lma == .
tab nameBea if floor(fips/1000) != 2 & lma == .
tab nameUsa if floor(fips/1000) != 2 & lma == .
tab nameHaines if floor(fips/1000) != 2 & lma == .
*/

compress
save $work/countyData, replace

*****DENSITY CALCULATIONS*******
use $work/countyData if pop != 1, clear
bys msa: egen nameMode = mode(nameMsa)
bys msa: egen stateMode = mode(statefips)
collapse (sum) pop area incBea (mean) value rent, by(msa nameMode year stateMode)
bys msa: egen areaAll = sum(area*(year==1970))
 gen density = pop/areaAll
 gen incPc = 1000*incBea/pop
 tsset msa year
 gen lagdensity = l20.density
 gen lagpop = l20.pop
 gen dlpop = log(pop) - log(lagpop)
 
 merge m:1 year using  $work/inflate
 gen valueReal = value*index
 gen rentReal = rent*index
 
list year pop density name value rent index valueReal rentReal  if msa == 875 & mod(year,10) == 0
gsort year -lagdensity
list name dlpop lagdensity if lagdensity >= 1000 & dlpop < .11 & year == 2010


********************************
***COLLAPSE TO LMA LEVEL***
********************************
/*
Note that there are some housing statistics which should be every year
and valueCount weighted. For now, the housing supply measures are every 10 years
and indexFhfa is pop weighted
*/
*xx A MISSING VALUE WILL NOT SHOW UP IN THE GROUP MEAN -- THIS IS IDEAL
*HOWEVER, IT WILL SHOW UP IN A SUM AS A 0 (MAYBE?) NEED TO PLAY AROUND WITH THIS

use $work/countyData, clear
tsset fips year
gen dlpop = log(pop)-log(l20.pop)
gen linc = log(l20.incMedian)
replace pop = pop/100
/*
#delimit;
scatter dlpop linc if year == 1980 & msa == . & dlpop < 2 [w=l20.pop], msymbol(Oh) msize(*0.5) || 
lfit dlpop linc if year == 1980 & msa == . & dlpop < 2  [w=l20.pop] || 
scatter dlpop linc if year == 1980 & msa != . & dlpop < 2  [w=l20.pop], msymbol(Oh) ||
lfit dlpop linc if year == 1980 & msa != . & dlpop < 2 [w=l20.pop],
legend(order(3 "In MSA" 1 "Not in MSA")) 
ytitle("Log Change in Pop, 1960-1980") xtitle("Log Income, 1960")
title("County-Level Directed Migration");
gr export countyOvb.pdf, replace;
#delimit cr;
*/



*****************
*KEY LINE WHICH MANIPULATES WHICH GEO IS USED
*****************
gen geo = lma
*drop if msa == .

preserve
collapse value elasticity  WRLURI unaval [w=valueCount], by(geo year)
save $work/value, replace
restore

*bys lma year: egen nameCbsaMode = mode(nameLma), maxmode
#delimit;
collapse (rawsum) emp pop area adult* lf fb incBea traBea capBea wageBea permit permitValue valueCount rentCount
(mean)   netMig incMed* incPer* shareBa 
share65 shareHS commute edu
[w=pop], by(geo year);
#delimit cr;

sort geo year
merge 1:1 geo year using $work/value, assert(1 3) nogen


********************************
***construct vars of interest 2 ***
********************************
tsset geo year
sort geo year

gen valuePerPermit = permitValue*1000/permit

*population measures
gen lpop = log(pop)
gen lfb = log(fb)
gen lnative = log(pop-fb)
gen llf = log(lf)

*nominal income measures
gen liBea = log(1000*incBea/pop)
gen liBeaNoCap = log(1000*(incBea-capBea)/pop)
gen liBeaWage = log(1000*wageBea/pop)
gen liBeaTransfer = log(1000*traBea/pop)
gen liCen = log(incMedian)
gen liCenTot = log(incMedian*pop)

*price measures
gen pCen = value/20
gen lvalue = log(value)
*topcode Saiz
replace elasticity = 6 if elasticity > 6 & elasticity != .

*real income measures
gen liBeapCen = log(1000*incBea/pop-pCen) if year >= 1975
replace liBeapCen = log(1000*l.incBea/l.pop-pCen) if year == 2010
gen liCenpCen = log(incMedian-pCen)
gen liCenpCenHighUserCost = log(incMedian-pCen*2)
gen liCenpRent = log(incMedian-rent)

rename elasticity saiz

compress

*save lmaMsaOnlyData, replace
*save msaData, replace
save $work/lmaData, replace
