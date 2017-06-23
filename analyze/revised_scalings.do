 
 
 
* Load in New Data From Census

* State square miles
 u  $src/sqmi.dta , clear
 ren statename name
 replace name=subinstr(name," ","",.)
 sort name
 merge 1:m name using "$work/state.dta", nogen keep(match)
 sort name year
 tempfile a
 save `a', replace
 
* Number of Local Gov'ts Per State 
 u  $src/govts_cen1.dta, clear
 replace statename=subinstr(statename," ","",.)
 rename statename name
 merge 1:1 name year using `a'   //, keep(match)
   
   
  * Test if this procedure creates the correct scaling 
 gen LUtest = docLu/ pop 
 xtile   BinHundredtest = LUtest, n(100)
 replace BinHundredtest= (BinHundredtest/100) -.01
 g interBinHundredtest= BinHundredtest*liInc
 corr interBinHundredtest interBinHundred
 corr BinHundredtest BinHundred
 
 * Scaling by square mile
 destring area_sqmi, ignore(",") replace
 gen LUsq = docLu/ area_sqmi 
 xtile   BinHundredSq = LUsq, n(100)
 replace BinHundredSq= (BinHundredSq/100) -.01
 g interBinHundredSq= BinHundredSq*liInc
 
 * Scaling by # Local Governments
 gen LUlg = docLu/ lg2 
 xtile   BinHundredlg = LUlg, n(100)
 replace BinHundredlg= (BinHundredlg/100) -.01
 g interBinHundredlg= BinHundredlg*liInc
   
  * Scaling by total documents 
 gen LUtot = docLu/ docTot
 xtile   BinHundredtot = LUtot, n(100)
 replace BinHundredtot= (BinHundredtot/100) -.01
 g interBinHundredtot= BinHundredtot*liInc
  
   duplicates tag statefip year,g(hm)
   drop if hm>0
 tsset statefip year
 
 xi: areg f20.dliInc i.year*BinHundred     liInc   interBinHundred        ,a(year) cl(stateabbrev)
 outreg2 liInc interBinHundred using "$out/scalings.txt",  replace

 xi: areg f20.dliInc i.year*BinHundredtest liInc   interBinHundredtest    ,a(year) cl(stateabbrev)
  outreg2 liInc interBinHundred using "$out/scalings.txt",  append

 xi: areg f20.dliInc i.year*BinHundredSq   liInc   interBinHundredSq      ,a(year) cl(stateabbrev)
 outreg2 liInc interBinHundred using "$out/scalings.txt",  append

 xi: areg f20.dliInc i.year*BinHundredlg   liInc   interBinHundredlg      ,a(year) cl(stateabbrev)
 outreg2 liInc interBinHundred using "$out/scalings.txt",  append

 xi: areg f20.dliInc i.year*BinHundredtot   liInc  interBinHundredtot     ,a(year) cl(stateabbrev)
 outreg2 liInc interBinHundred using "$out/scalings.txt",  append
 
 
 
 xi: areg f20.dlpop i.year*BinHundred     liInc   interBinHundred        ,a(year) cl(stateabbrev)
  outreg2 liInc interBinHundred using "$out/scalings.txt",  append

 xi: areg f20.dlpop i.year*BinHundredtest liInc   interBinHundredtest    ,a(year) cl(stateabbrev)
  outreg2 liInc interBinHundred using "$out/scalings.txt",  append

 xi: areg f20.dlpop i.year*BinHundredSq   liInc   interBinHundredSq      ,a(year) cl(stateabbrev)
 outreg2 liInc interBinHundred using "$out/scalings.txt",  append

 xi: areg f20.dlpop i.year*BinHundredlg   liInc   interBinHundredlg      ,a(year) cl(stateabbrev)
 outreg2 liInc interBinHundred using "$out/scalings.txt",  append

 xi: areg f20.dlpop i.year*BinHundredtot   liInc  interBinHundredtot     ,a(year) cl(stateabbrev)
 outreg2 liInc interBinHundred using "$out/scalings.txt",  append excel

