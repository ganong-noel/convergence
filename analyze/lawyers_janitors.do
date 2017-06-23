clear all
set mem 5g
do F:\usa_00078.do
keep if gq==1 & age>=25 & age<65
keep if statefip==1 | statefip==5 | statefip==13 | statefip==28 | statefip==45 | statefip==9 | statefip==36 | statefip==34
g newyork =  statefip==9 | statefip==36 | statefip==34
g janitor= (occ==4220 & year==2010) | (year==1960 & occ==834)
g lawyer = (occ==2100 & year==2010) | (year==1960 & occ==105)
keep if lawyer==1 | janitor==1

replace valueh=. if valueh>9999998

g housecost=12*rent
replace housecost=.05*valueh if valueh!=.
replace housecost=. if housecost==0
replace incwage= . if incwage==0
replace inctot=. if inctot==0
g scaled_income =  inctot- housecost

g scaled_wage  = incwage-housecost

collapse (mean) scaled_wage scaled_income inctot incwage [w=perwt], by(newyork janitor lawyer year)

