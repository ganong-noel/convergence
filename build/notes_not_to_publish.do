
***************
*master.do
***************

/*
Files needed:
--stateData
--data17, data18, data19, data21 (last two still in prior directory)
--statebplcross

cd /Users/ganong/Dropbox/convergence/draft3/micro
global path = "~/dropbox/convergence/draft3"
*/

***************
*constructLMA.do
***************
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


***************
*constructState.do
***************
/*
Notes for data appendix on income:
--var will be median hh wage income for 1940
--var is median _family_ income 1950-2000
--var is median hh income from IPE in 2010

**xx reconstruct census measure of nonblack pop to go back to 1930
*xx drop statefip == 99 from convTable.do

Notes to self: 
(1) I have double-checked and the pop measures from 
Haines/Usacounties are perfectly correlated with the sum of the IPUMS weights
(2) the net mig survival measure "gcnetmra" in Fishback and Kantor from 
Gardner and Cohen is perfectly correlated with the one I got from historical statistics
*/
