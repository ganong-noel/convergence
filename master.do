set more off
do path

cap ssc inst _gwtmean

***STATE/LMA LEVEL ANALYSIS***
do build/inflate
do build/prepHc //Note: this code is slow!
do build/hc_stock_series
do build/constructLma
do build/constructState

set more off
do analyze/rolling //TABLE 1, APPENDIX TABLES 1-2
do analyze/graphStateSlides //FIGURES 1, 2
do analyze/hcConverge //FIGURE A2
do analyze/analysis  //TABLE 2, TABLE 3, FIGURE 7, FIGURE 8, APPENDIX TABLE 5

do analyze/inequality //APPENDIX TABLE 7 
do analyze/nonhomothetic.do  //APPENDIX FIGURE 1 
do analyze/summary_stats.do  //Table 1
do analyze/compare_regulations.do // Footnote 26
do analyze/revised_scalings.do // Appendix Table 6
do analyze/lawyers_and_janitors.do

***MICRO DATA ANALYSIS***
do build/borjas1940.do
do build/borjas1940robustness.do

do build/borjas2000.do  //Note: this code is slow!
do build/borjas2000robustness.do //Note: this code is extremely slow!
do build/price_file_prep //Note: this code is extremely slow!

do analyze/mig_returns_and_flows.do //FIGURE 3, 4, 5, APPENDIX TABLE 3, 4*

