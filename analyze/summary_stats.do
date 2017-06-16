
global path "C:\Users\shoag\dropbox\danny\"

u "$path\draft3\state\state.dta", clear
sort statefip year

merge 1:1 statefip year using "$path\draft4\code_shoag\lucasHcConverge_im", nogen keep(match)
summ hcAggBaseRes
replace hcAggBaseRes=hcAggBaseRes/r(mean)


collapse (mean) MBinHundred =BinHundred /*Mpermits=permits*/ Mhc=hcAggBaseRes  (sd) Sdhc=hcAggBaseRes SdBinHundred=BinHundred /*Sdpermits=permits*/ , by(year)
keep if year==1940|year==1960 |year==1980|year==2000


