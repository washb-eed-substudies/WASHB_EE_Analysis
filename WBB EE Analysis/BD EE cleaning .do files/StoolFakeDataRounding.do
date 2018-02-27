gen int aat1b = floor(aat1)
gen int aat2b = floor(aat2)
gen int aat3b = floor(aat3)
gen int mpo1b = floor(mpo1)
gen int mpo2b = floor(mpo2)
gen int mpo3b = floor(mpo3)
gen int neo1b = floor(neo1)
gen int neo2b = floor(neo2)
gen int neo3b = floor(neo3)
gen int reg1b2b = floor(reg1b2)

drop aat1 mpo1 neo1 aat2 mpo2 neo2 reg1b2 aat3 mpo3 neo3



rename aat1b aat1 
rename aat2b aat2
rename aat3b aat3
rename mpo1b mpo1 
rename mpo2b mpo2
rename mpo3b mpo3
rename neo1b neo1
rename neo2b neo2 
rename neo3b neo3
rename reg1b2b reg1b2


cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp"
use "washb-BD-EE-sim-urine-outcomes.dta", clear


gen int Lact1b = floor(Lact1)
gen int Lact2b = floor(Lact2)
gen int Lact3b = floor(Lact3)
gen int Mann1b = floor(Mann1)
gen int Mann2b = floor(Mann2)
gen int Mann3b = floor(Mann3)


drop Lact1 Mann1 Lact2 Mann2 Lact3 Mann3



rename Lact1b Lact1
rename Lact2b Lact2
rename Lact3b Lact3
rename Mann1b Mann1
rename Mann2b Mann2
rename Mann3b Mann3

save washb-BD-EE-sim-urine-outcomes, replace
saveold washb-BD-EE-sim-urine-outcomes-stata12, version(12) replace
