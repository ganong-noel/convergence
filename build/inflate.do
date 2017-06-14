

******************
*INFLATION
****************
*use data from historical statistics, 1880-2003
tempfile t 
insheet using $src/priceIndex.csv, comma clear
keep v1 v2
drop if _n <= 110
rename v1 year
rename v2 index
destring year index, replace
sort year
save `t'

*use data from month of January for 2003 forward. Data from ftp://ftp.bls.gov/pub/special.requests/cpi/cpiai.txt
insheet using $src/priceIndex_post2003.csv, comma clear
keep if _n >= 4 & v1 != ""
keep v1 v2
destring v1 v2, replace
rename v1 year
rename v2 indexNew
sort year
merge 1:1 year using `t'

*fix at seam in 2003
egen index2003 = sum(index*(year == 2003))
egen indexNew2003 = sum(indexNew*(year == 2003))
replace index = indexNew*index2003/indexNew2003 if year >= 2004
egen index2012 = sum(index*(year == 2012))
replace index = 1/(index/index2012)
sort year
keep year index
save $work/inflate, replace

