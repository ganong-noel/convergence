/*
Files needed from Odyssey:
--incApp.csv, wageApp.csv, inequality.csv
--pull hcSummaryYoung as a new file for same purpose
--convTable.dta????

from data dir
--haines/haines.dta
--usacounties/usaCounties.dta
--SA04/stateSA04.dta

*/

set more off

************
*SERVER*
**********
*APPENDIX TABLE 10 -- inequality.csv*

*************
*STATE/LMA LEVEL ANALYSIS
*************
do build/inflate
do build/constructLma
do build/constructState
do build/hc_stock_series

do analyze/rolling //***TABLE 1, APPENDIX TABLES 1-2***
do analyze/analyzeLma //***APPENDIX TABLES 1-2***
do analyze/graphStateSlides //***FIGURES 1, 2***
do analyze/hcConverge //***FIGURE A2***
do analyze/analysis  //***TABLE 2, TABLE 3, FIGURE 7, FIGURE 8, APPENDIX TABLE 5***

****************
*MICRO DATA ANALYSIS
*****************

do build/borjas1940.do
do build/borjas1940robustness.do
do build/borjas2000.do  //Note: this code is slow!
do build/borjas2000robustness.do //Note: this code is extremely slow!
do build/price_file_prep //Note: this code is extremely slow!

do analyze/migReturn.do //***FIGURE 3****
do analyze/borjasGraphs.do. //***FIGURE 4, 5***
do analyze/migReturnRegs2.do //*APPENDIX TABLE 4**
do analyze/migReturnPrice.do //*APPENDIX TABLE 3**
do analyze/borjasrobustnesstables.do //***APPENDIX TABLE 5**




********BEGIN SLOW CODE**********

***APPENDIX FIGURE 1***
//uses --usa00034
//do analyze/housingfrac.do XXX this cold makes an old version

/*
Files needed:
--stateData
--data17, data18, data19, data21 (last two still in prior directory)
--statebplcross

cd /Users/ganong/Dropbox/convergence/draft3/micro
global path = "~/dropbox/convergence/draft3"
*/

***APPENDIX TABLE 8***
