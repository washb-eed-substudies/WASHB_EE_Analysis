
*--------------------------
*telo
*--------------------------

clear 
cd "C:\Users\andre\Dropbox\WASHB-EE-analysis\WBB-EE-analysis\Data\Cleaned\Andrew"
use BD-EE-telo.dta

*drop identifying variables
drop union svydate date2 date3 _merge DOB




*Save file 
cd "C:\Users\andre\Documents\WBB_EE_Analysis_Andrew\OSF sharing data"
label data "BD EE telomere dataset, created by BD-EE-dm-telo.do"
saveold "BD-EE-telo.dta", replace version(12)
outsheet using "BD-EE-telo.csv", comma replace
clear



*--------------------------
*ipcw
*--------------------------


clear 
cd "C:\Users\andre\Dropbox\WASHB-EE-analysis\WBB-EE-analysis\Data\Cleaned\Andrew"
use BD-EE-ipcw.dta

*drop identifying variables
drop union svydate dob DOB




*Save file 
cd "C:\Users\andre\Documents\WBB_EE_Analysis_Andrew\OSF sharing data"
label data "BD EE telomere dataset, created by BD-EE-dm-ipcw.do"
saveold "BD-EE-ipcw.dta", replace version(12)
outsheet using "BD-EE-ipcw.csv", comma replace
clear
