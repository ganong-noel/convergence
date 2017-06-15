set more off
do path


***STATE/LMA LEVEL ANALYSIS***
do build/inflate
do build/constructLma
do build/constructState
do build/prepHc
do build/hc_stock_series

do analyze/rolling //TABLE 1, APPENDIX TABLES 1-2
do analyze/graphStateSlides //FIGURES 1, 2
do analyze/hcConverge //FIGURE A2
do analyze/analysis  //TABLE 2, TABLE 3, FIGURE 7, FIGURE 8, APPENDIX TABLE 5
do analyze/inequality //APPENDIX TABLE 7 
//do analyze/housingfrac.do  //APPENDIX FIGURE 1 XXX this code makes an old version

do analyze/compare_regulations.do // Footnote 26
do analyze/revised_scalings.do // Appendix Table 6

***MICRO DATA ANALYSIS***
do build/borjas1940.do
do build/borjas1940robustness.do

do build/borjas2000.do  //Note: this code is slow!
do build/borjas2000robustness.do //Note: this code is extremely slow!
do build/price_file_prep //Note: this code is extremely slow!

do analyze/mig_returns_and_flows.do //FIGURE 3, 4, 5, APPENDIX TABLE 3, 4*

