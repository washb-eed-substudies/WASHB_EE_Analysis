capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-consort.log", text replace

*--------------------------------------------
* BD-EE-dm-consort.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the WASH Benefits Bangladesh EED 
* substudy and main trial tracking information
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  Treatment assignments 
*    washb-bangladesh-blind-tr.dta
* 
*
*
*--------------------------------------------

*--------------------------------------------
* format the treatment assignment information
*--------------------------------------------
use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-tr.dta", clear


destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'


*--------------------------------------------
* Merge in main trial tracking data
*--------------------------------------------
clear
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched"
use washb-bangladesh-track-compound.dta, clear
gen maintrial_origin=1
codebook miss1reason

gen childno=1 //Assume tracking applies to non-twin

destring clusterid, replace
sort dataid childno
tempfile main_tracking
save `main_tracking'


*--------------------------------------------
* Merge in EED medical history forms 
*--------------------------------------------
cd "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/"

use "Baseline/Baseline_ChildID_CLEANED_VersionIncorporated_20Oct15.dta", clear
destring childno clusterid, replace

sort dataid childno
tempfile baselineMedhistory
save `baselineMedhistory'




use Midline/ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
destring childno clusterid, replace

sort dataid childno
tempfile midlineMedhistory
save `midlineMedhistory'



use Endline/EE_Endline_ChildID&MedHistory_CLEANED_data_22June2016, clear
rename childNo childno
destring childno, replace


sort dataid childno
tempfile endlineMedhistory
save `endlineMedhistory'

*--------------------------------------------
* Load in enrollment records, clean, 
* and merge in medical history
*--------------------------------------------
import excel using "Enrollment Records/EE Baseline Enrollment Record v5_27-01-2017 FINAL_AL_corrected.xlsx", sheet("Individual Sample Record") firstrow clear
rename ConsentGiven miss1_ee
rename IfNoConsentGiveReason miss1reason_ee
rename ClusterID clusterid
gen svy=1
gen baseline_origin=1


*generate dataid and child number
gen dataid=substr(ChildBarcodeIDClusterIDH, 1, 5)
gen childno=substr(ChildBarcodeIDClusterIDH, 7, 7)
destring childno, replace force
replace childno=1 if childno==.

*Merge in medical history to check merge
sort dataid childno
merge dataid childno using `baselineMedhistory'
tab _merge 
drop _merge

drop if dataid=="07304"
drop if dataid=="26708"
drop if dataid=="08903"
drop if dataid=="13808"
drop if dataid=="EXAMP"

codebook miss1reason_ee
tab miss1reason_ee
replace miss1reason_ee="1" if miss1reason_ee=="No live birth" | miss1reason_ee=="Miscarriage" | miss1reason_ee=="Abortion" | miss1reason_ee=="Stillbirth" | miss1reason_ee=="livebirth" | miss1reason_ee=="Unborn"
replace miss1reason_ee="2" if miss1reason_ee=="Withdrew" | miss1reason_ee=="Refused" | miss1reason_ee=="All Refused" | miss1reason_ee=="Refuse" 
replace miss1reason_ee="3" if miss1reason_ee=="Moved away" | miss1reason_ee=="Migrant" | miss1reason_ee=="migrant"
replace miss1reason_ee="4" if miss1reason_ee=="Child death" | miss1reason_ee=="Death"
replace miss1reason_ee="5" if miss1reason_ee=="Absent"
replace miss1reason_ee="0" if miss1reason_ee=="Not lost" | miss1reason_ee==""
destring miss1reason_ee, replace force
	label define miss1reason_ee 0 "Not lost" 1 "No live birth" 2 "Withdrew" 3 "Moved away" 4 "Child death" 5 "Absent"
	label values miss1reason_ee miss1reason_ee
	label var miss1reason_ee "EE 3 month child tracking"


	
tab miss1reason_ee
sort dataid childno 
tempfile ee_tracking1
save `ee_tracking1'






import excel using "Enrollment Records/EE Midline Enrollment Record v6_27Jan2017_FINAL_AL_corrected.xlsx", sheet("Individual Sample Record") firstrow clear
rename ConsentGiven miss1_ee
rename IfNoConsentGiveReason miss1reason_ee
rename ClusterID clusterid
gen svy=2
gen midline_origin=1

*generate dataid and child number
gen dataid=substr(ChildBarcodeIDClusterIDH, 1, 5)
gen childno=substr(ChildBarcodeIDClusterIDH, 7, 7)
destring childno, replace force
replace childno=1 if childno==.

*Merge in medical history to check merge
sort dataid childno
merge dataid childno using `midlineMedhistory'
tab _merge 
drop _merge

codebook miss1reason_ee
tab miss1reason_ee

replace miss1reason_ee="1" if miss1reason_ee=="No live birth" | miss1reason_ee=="Miscarriage" | miss1reason_ee=="Abortion" | miss1reason_ee=="Stillbirth" | miss1reason_ee=="livebirth" | miss1reason_ee=="Unborn" | miss1reason_ee=="False pregnant" 
replace miss1reason_ee="2" if miss1reason_ee=="Withdrew" | miss1reason_ee=="Refused" | miss1reason_ee=="All Refused" | miss1reason_ee=="Refuse" | miss1reason_ee==" Refused" 
replace miss1reason_ee="3" if miss1reason_ee=="Moved away" | miss1reason_ee=="Migrant" | miss1reason_ee=="migrant"
replace miss1reason_ee="4" if miss1reason_ee=="Child death" | miss1reason_ee=="Death" | miss1reason_ee=="Death "
replace miss1reason_ee="5" if miss1reason_ee=="Absent"
replace miss1reason_ee="0" if miss1reason_ee=="Not lost" | miss1reason_ee==""
destring miss1reason_ee, replace force
	label define miss1reason_ee 0 "Not lost" 1 "No live birth" 2 "Withdrew" 3 "Moved away" 4 "Child death" 5 "Absent"
	label values miss1reason_ee miss1reason_ee
	label var miss1reason_ee "EE 3 month child tracking"
codebook miss1reason_ee

sort dataid
tempfile ee_tracking2
save `ee_tracking2'




import excel using "Enrollment Records/EE Endline Enrollment Record_27Jan2017_AL_corrected.xlsx", sheet("Individual Sample Record") firstrow clear
rename ConsentGiven miss1_ee
rename IfNoConsentGiveReason miss1reason_ee
rename ClusterID clusterid
drop if clusterid==""
gen svy=3
codebook miss1reason_ee
tab miss1reason_ee

*generate dataid and child number
gen dataid=substr(ChildBarcodeIDClusterIDH, 1, 5)
gen childno=substr(ChildBarcodeIDClusterIDH, 7, 7)
destring childno, replace force
replace childno=1 if childno==.
gen endline_origin=1

*Merge in medical history to check merge
sort dataid childno
merge dataid childno using `endlineMedhistory'
tab _merge 
drop _merge

replace miss1reason_ee="1" if miss1reason_ee=="No live birth" | miss1reason_ee=="Miscarriage" | miss1reason_ee=="Abortion" | miss1reason_ee=="Stillbirth" | miss1reason_ee=="livebirth" | miss1reason_ee=="Unborn" | miss1reason_ee=="Still birth"
replace miss1reason_ee="2" if miss1reason_ee=="Withdrew" | miss1reason_ee=="Refused" | miss1reason_ee=="All Refused" | miss1reason_ee=="Refuse" | miss1reason_ee=="All refuse" | miss1reason_ee=="All refuse (Baby was sick)" | miss1reason_ee==" All Refuse" | miss1reason_ee=="Implementation team refused HH" 
replace miss1reason_ee="3" if miss1reason_ee=="Moved away" | miss1reason_ee=="Migrant"
replace miss1reason_ee="4" if miss1reason_ee=="Child death" | miss1reason_ee=="Death"
replace miss1reason_ee="5" if miss1reason_ee=="Absent"
replace miss1reason_ee="0" if miss1reason_ee=="Not lost" | miss1reason_ee==""

*Drop those with "None use Id" unless dataid==
*FIX when miss2reason_ee = "None used Id" and miss2reason = "withdrew” (dataid 40004 @ endline)
replace miss1reason_ee = "2" if miss1reason_ee=="None use Id" & dataid=="40004"
drop if miss1reason_ee=="None use Id" 


destring miss1reason_ee, replace force
	label define miss1reason_ee 0 "Not lost" 1 "No live birth" 2 "Withdrew" 3 "Moved away" 4 "Child death" 5 "Absent"
	label values miss1reason_ee miss1reason_ee
	label var miss1reason_ee "EE 3 month child tracking"
	
tab miss1reason_ee

sort dataid
tempfile ee_tracking3
save `ee_tracking3'





*--------------------------------------------
*Identify tracking ID's in earlier rounds not in later rounds to create cumulative missingness
*--------------------------------------------

use `ee_tracking2'

*merge baseline 
sort dataid childno 
merge dataid childno using `ee_tracking1'
tab _merge
keep if _merge==2
br
replace svy=2
drop _merge

* Add back in midline data
append using `ee_tracking2'
sort dataid childno 
tab miss1reason_ee
tempfile ee_tracking2
save `ee_tracking2'

use `ee_tracking3'
sort dataid childno 
merge dataid childno using `ee_tracking2'
tab _merge
keep if _merge==2
replace svy=3
drop _merge

* Add back in endline data
append using `ee_tracking3'
sort dataid
tempfile ee_tracking3
save `ee_tracking3'



*Append tracking datasets together
use `ee_tracking1'

destring clusterid, replace
sort clusterid
merge clusterid using `trdata'
tab _merge
drop if _merge==2
drop _merge
tab tr
tab tr miss1reason_ee
sort dataid childno
tempfile ee_tracking1
save `ee_tracking1'


use `ee_tracking2'
drop if dataid=="07304"
drop if dataid=="26708"
drop if dataid=="08903"
drop if dataid=="13808"
drop if dataid=="EXAMP"
drop if dataid=="40007"
destring clusterid, replace
sort clusterid
merge clusterid using `trdata'
drop if _merge==2
tab _merge
drop _merge
tab tr

sort dataid childno
tempfile ee_tracking2
save `ee_tracking2'


use `ee_tracking3'
drop if dataid=="07304"
drop if dataid=="26708"
drop if dataid=="08903"
drop if dataid=="13808"
drop if dataid=="EXAMP"
drop if dataid=="40007"
destring clusterid, replace
sort clusterid
merge clusterid using `trdata'
drop if _merge==2
tab _merge
drop _merge
tab tr

sort dataid childno
tempfile ee_tracking3
save `ee_tracking3'


use `ee_tracking1'
append using `ee_tracking2', force nolabel
append using `ee_tracking3', force nolabel 

*Drop erroneous dataid's (From Audrie's data cleaning)
drop if dataid=="07304"
drop if dataid=="26708"
drop if dataid=="08903"
drop if dataid=="13808"
drop if dataid=="EXAMP"



keep dataid childno miss1_ee miss1reason_ee svy baseline_origin midline_origin endline_origin tr
sort dataid

*--------------------------------------------
* Merge in main trial tracking and EE tracking
*--------------------------------------------

sort dataid childno
merge dataid childno using `main_tracking'
tab _merge

*MISS1REASON_EE has coded “no live birth” in some cases, where there are other correct reasons that the child was not there.
drop if _merge==2
*keep if svy==1
codebook miss1reason_ee miss1reason
tab miss1reason svy



codebook miss1reason_ee
*MISS1REASON “not lost” children are alive, so in cases where miss1reason_ee==“no live birth”, they need to recode miss1reason_ee as “absent”
replace miss1reason_ee = 5 if miss1reason_ee==1 & miss1reason== 0

*MISS1REASON probably has a more likely reason for an absence than EE “Absent”:
replace miss1reason_ee = miss1reason if miss1reason==4 & miss1reason_ee==5
replace miss1reason_ee = miss1reason if miss1reason==3 & miss1reason_ee==5
replace miss1reason_ee = miss1reason if miss1reason==2 & miss1reason_ee==5
replace miss1reason_ee = miss1reason if miss1reason==1 & miss1reason_ee==5

*MISS1REASON_EE has coded “no live birth” in some cases, where there are other correct reasons that the child was not there.
replace miss1reason_ee = miss1reason if miss1reason_ee==1 & miss1reason!=1

*MISS1REASON_EE is not no live birth but main study tracking is, replace miss1reason_ee with main study reason
replace miss1reason_ee = miss1reason if miss1reason_ee!=1 & miss1reason==1

* FIX when miss1reason = "Withdrew" and miss1reason_ee != "Withdrew"
replace miss1reason_ee = miss1reason if miss1reason==2 & miss1reason_ee==3
replace miss1reason_ee = miss1reason if miss1reason==2 & miss1reason_ee==4


codebook miss1reason_ee miss1reason


tab miss1reason_ee tr if svy==2 
sort miss1reason_ee dataid
*br if svy==3 & tr=="WSH"
*tab miss1reason_ee if svy==2 & tr=="WSH" 


tab miss1reason_ee tr if svy==3

*br if dataid=="20107"


*Fix missing clusterid's
*br if clusterid==.

gen temp_clusterid=substr(dataid,1,3)
destring temp_clusterid, replace
replace clusterid=temp_clusterid if clusterid==.


keep dataid childno clusterid block svy tr miss1_ee miss1reason_ee maintrial_origin baseline_origin midline_origin endline_origin


help reshape



br if dataid=="44106" | dataid=="44107"




*Save file 
label data "BD EE consort dataset, created by BD-EE-dm-consort.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-consort.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-consort.csv", comma replace
clear






*Tabulate number of added children per round (new children in enrollment not in previous enrollment survey).

use `ee_tracking1'
merge dataid childno using `ee_tracking2'
tab _merge

tab tr if _merge==2

clear
use `ee_tracking2'
merge dataid childno using `ee_tracking3'
tab _merge











clear
