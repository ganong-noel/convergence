  /*
This pulls together at a county level
--annual house prices to 1975 FHFA (1000 counties)
--housing supply elasticity (770 counties)
--crosswalk to aggregate up to 400 labor market areas
--BEA income pop and emp
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

*http://www.census.gov/population/estimates/metro-city/99mfips.txt
insheet using $src/msa1999.csv, comma clear
gen nameMsaShort = name if fips == .
replace pmsa = msa if pmsa == .
bys pmsa: egen nameMsa = mode(nameMsaShort)
drop if fips == .
bys fips: egen pmsaFinal = mode(pmsa)
*fips 9001 in CT. Assigning it to Stamford.
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

**2010 county populations are missing. substitute 2009.
sort fips year
replace pop = pop[_n-1] if year == 2010
foreach name in inc tra cap wage {
	rename v`name' `name'Bea
}

**1950 unit counts are missing. substitute average of 1940 and 1970.
sort fips year
replace valueCount = (valueCount[_n-1] + valueCount[_n+2])/2 if year == 1950
replace rentCount = (rentCount[_n-1] + rentCount[_n+2])/2 if year == 1950


*evaluate merges


*table year mSaiz if year <= 1970 & mod(fips, 1000) != 0, c(sum(pop))
*table year mBeaCounty if year == 1970 | year == 2000, c(sum(pop))



tabulate year mSaiz if year <= 1970 & mod(fips, 1000) != 0, summarize(pop)
tabulate year mBeaCounty if year == 1970 | year == 2000, summarize(pop)



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
table year mLma  if mod(year,10)==0
table year mMsa  if mod(year,10)==0

compress
save $work/countyData, replace


********************************
***COLLAPSE TO LMA LEVEL***
********************************

use $work/countyData, clear
tsset fips year
gen dlpop = log(pop)-log(l20.pop)
gen linc = log(l20.incMedian)
replace pop = pop/100


gen geo = lma

preserve
collapse value elasticity  WRLURI unaval [w=valueCount], by(geo year)
save $work/value, replace
restore

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

save $work/lmaData, replace
