capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WASHB-EE-Analysis/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-anthro.log", text replace

*--------------------------------------------
* 4-kenya-dm-anthro.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Bangladesh EED substudy time 1,2, and 3
* anthropometry data for analysis
*
*--------------------------------------------




*--------------------------------------------
* input files:
*
*  Treatment assignments (Year 2 directory)
*    02. WASHB_Endline_Arm_Identification.dta
* 
*
*  ENROLLMENT
*  1. WASHB_Baseline_main_survey.dta
*  3. WASHB_Baseline_childinfo.dta
*
*  YEAR 1
*  1. WASHB_Midline_main_survey_cleaned.dta
*  3. WASHB_Midline_childinfo_cleaned.dta
*  8. WASHB_Midline_anthropometry_cleaned.dta
*
*  YEAR 2
*  04. WASHB_Endline_main_survey_cleaned.dta
*  07. WASHB_Endline_childinformation_cleaned.dta
*  09. WASHB_Endline_anthropometry_cleaned.dta
*
* output files:
*  final/washb-bangladesh-anthro.dta / .csv
*
*--------------------------------------------

*--------------------------------------------
* format the treatment assignment information
*--------------------------------------------
use "C:/Users/andre/Dropbox/WASHB-EE-Analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-blind-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'


*--------------------------------------------
* Append 3 rounds of child anthro and rename variables
*--------------------------------------------
clear
cd "C:/Users/andre/Dropbox/WASHB-EE-Analysis/WBB-EE-analysis/Data/Untouched"


use Endline/EE_Endline_Anthro_CLEANED_data_26June2016, clear
rename q7 Weight1
rename q8 Weight2
rename q9 Weight3
rename q13 Len1
rename q14 Len2
rename q15 Len3
rename q16 hc1
rename q17 hc2
rename q18 hc3
rename q19 muac1
rename q20 muac2
rename q21 muac3
rename q12 length_method
	label var length_method "Length or height of child"

*destring anthro variables
destring Weight1, replace
destring Weight2, replace
destring Weight3, replace
destring Len1, replace
destring Len2, replace
destring Len3, replace
destring hc1, replace
destring hc2, replace
destring hc3, replace
destring muac1, replace
destring muac2, replace
destring muac3, replace

	
*generate surveyyear variable
gen svy=3

keep svy dataid mid clusterid childNo SampleColDate length_method Weight1 Weight2 Weight3 Len1 Len2 Len3 hc1 hc2 hc3 muac1 muac2 muac3

tempfile c_anthr3
save `c_anthr3'

use Midline/Anthro_Midline_Cleaned_MatchedwEnrollment_26Jan16, clear
rename q7 Weight1
rename q8 Weight2
rename q9 Weight3
rename q13 Len1
rename q14 Len2
rename q15 Len3
rename q16 hc1
rename q17 hc2
rename q18 hc3
rename q19 muac1
rename q20 muac2
rename q21 muac3
rename childno childNo
rename samplecoldate SampleColDate
rename q12 length_method
	label var length_method "Length or height of child"

*destring anthro variables
destring Weight1, replace
destring Weight2, replace
destring Weight3, replace
destring Len1, replace
destring Len2, replace
destring Len3, replace
destring hc1, replace
destring hc2, replace
destring hc3, replace
destring muac1, replace
destring muac2, replace
destring muac3, replace
destring clusterid, replace
*generate surveyyear variable
gen svy=2

keep svy dataid mid clusterid childNo SampleColDate length_method Weight1 Weight2 Weight3 Len1 Len2 Len3 hc1 hc2 hc3 muac1 muac2 muac3

tempfile c_anthr2
save `c_anthr2'

use Baseline/Baseline_Anthro_CLEANED_24Sept14_NamesStripped, clear
rename q7 Weight1
rename q8 Weight2
rename q9 Weight3
rename q13 Len1
rename q14 Len2
rename q15 Len3
rename q16 hc1
rename q17 hc2
rename q18 hc3
rename q19 muac1
rename q20 muac2
rename q21 muac3
rename childno childNo
rename samplecoldate SampleColDate
rename q12 length_method
	label var length_method "Length or height of child"
	
*destring anthro variables
destring Weight1, replace
destring Weight2, replace
destring Weight3, replace
destring Len1, replace
destring Len2, replace
destring Len3, replace
destring hc1, replace
destring hc2, replace
destring hc3, replace
destring muac1, replace
destring muac2, replace
destring muac3, replace

*generate surveyyear variable
gen svy=1

keep svy dataid mid clusterid childNo SampleColDate length_method Weight1 Weight2 Weight3 Len1 Len2 Len3 hc1 hc2 hc3 muac1 muac2 muac3


*Append midline and endline anthropometry
append using `c_anthr2', force nolabel
append using `c_anthr3', force nolabel

destring clusterid, replace

tempfile EEanthro
save `EEanthro'


********************************************************************************
*Calculate child age at each sample collection
********************************************************************************
*First, merge together main study DOB from anthro and diar datasets
use washb-bangladesh-anthro, clear
duplicates tag dataid childid, generate(dup)
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1) //change childid to match EE
rename dob anthrodob
gen byte MainStudyDataset_anthro_DOB=1 
keep dataid childNo clusterid svy anthrodob MainStudyDataset_anthro_DOB
sort dataid childNo svy

duplicates drop dataid childNo anthrodob, force
duplicates list dataid childNo

tempfile main_anthro
save `main_anthro'

use washb-bangladesh-diar, clear
keep if childid=="T1" | childid=="T2"

gen childNo= substr(childid,2,1)
rename dob diardob
gen byte MainStudyDataset_diar_DOB=1 
keep dataid childNo svy clusterid diardob MainStudyDataset_diar_DOB
sort dataid childNo svy

duplicates tag dataid childNo, generate(dup)
duplicates drop dataid childNo diardob, force
duplicates list dataid childNo

merge dataid childNo svy using `main_anthro' 
tab _merge
drop _merge dup
sort dataid childNo svy
duplicates list dataid childNo
duplicates tag dataid childNo, generate(dup)
drop if dup==1 & svy!=1

tempfile mainDOB
save `mainDOB' 





********************************************************************************
*Merge in the EE medical history files for the child date of birth for the 58 children
*not in the main study
********************************************************************************
    *Save relevent parts of baseline childid tracking
use Baseline/Baseline_ChildID_CLEANED_VersionIncorporated_20Oct15, clear
rename q15 EEdob
rename childno childNo
gen byte EE_BLdataset_DOB=1 
gen female= q14-1
keep dataid childNo EEdob EE_BLdataset_DOB female

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d


tempfile BL_EE_dob
save `BL_EE_dob'

    *Save relevent parts of midline childid tracking
use Midline/ChildID_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
rename q15 EEdob
rename childno childNo
gen female= q14-1
gen byte EE_MLdataset_DOB=1 
keep dataid childNo clusterid EEdob EE_MLdataset_DOB female

gen EE_dob = date(EEdob, "DMY")
drop EEdob
format EE_dob %d

tempfile ML_EE_dob
save `ML_EE_dob'
	
    *Save relevent parts of endline childid tracking
use Endline/EE_Endline_ChildID&MedHistory_CLEANED_data_22June2016, clear
rename q15 EEdob
gen female= q14-1
gen byte EE_ELdataset_DOB=1 
keep dataid childNo clusterid EEdob EE_ELdataset_DOB female

format EEdob %d
rename EEdob EE_dob

append using `BL_EE_dob', force nolabel
append using `ML_EE_dob', force nolabel 
sort dataid childNo

*Only keep midline and endline DOBs not found at baseline
duplicates tag dataid childNo EE_dob ,generate(dup)
tab dup	
duplicates tag dataid childNo, generate(dup2)
tab dup2

*List out where DOB is not consistent between EE rounds
list dataid childNo EE_dob  EE_BLdataset_DOB EE_MLdataset_DOB EE_ELdataset_DOB  dup dup2 if dup!=dup2

*Keep preferentially baseline, then midline, then endline DOB where DOBs aren't consistent across rounds
drop if (EE_BLdataset_DOB!=1 & dup2==2) | (EE_ELdataset_DOB==1 & dup2==1) 
duplicates tag dataid childNo, generate(dup3)
drop if dup3==1 & EE_MLdataset_DOB==1

duplicates list dataid childNo
drop dup dup2 dup3 EE_BLdataset_DOB EE_MLdataset_DOB EE_ELdataset_DOB
gen byte EEdataset_DOB=1
	
	
*Merge in childid tracking with main study DOBs
sort dataid childNo 
merge dataid childNo using `mainDOB' 
tab _merge

*list if dates mismatch between datasets
list dataid childNo svy anthrodob diardob if (MainStudyDataset_anthro_DOB==1 & MainStudyDataset_diar_DOB==1) & anthrodob!=diardob
list dataid childNo svy anthrodob EE_dob if (MainStudyDataset_anthro_DOB==1 & EEdataset_DOB==1) & anthrodob!=EE_dob
list dataid childNo svy diardob EE_dob if (EEdataset_DOB==1 & MainStudyDataset_diar_DOB==1) & EE_dob!=diardob

*Create single DOB variable, preferentially using main study DOB, then EEdob for the 37 subjects without a
*main study DOB
generate DOB=anthrodob
replace DOB=diardob if MainStudyDataset_anthro_DOB!=1
replace DOB=EE_dob if MainStudyDataset_anthro_DOB!=1 & MainStudyDataset_diar_DOB!=1
format DOB %d

*Create indicator for whether dob comes from EE or main study
generate DOBfromEE byte=1 if EEdataset_DOB==1 & (MainStudyDataset_diar_DOB!=1 & MainStudyDataset_anthro_DOB!=1)
drop anthrodob diardob EE_dob MainStudyDataset_anthro_DOB MainStudyDataset_diar_DOB EEdataset_DOB _merge svy


*Merge DOBs into EEanthro dataset
sort dataid childNo
merge 1:m dataid childNo using `EEanthro'
tab _merge
drop if _merge==1
list dataid childNo svy if _merge==2
list dataid childNo svy if DOBfromEE==1
drop _merge




*Temporarily fix incorrect collection dates
replace SampleColDate="27 Jun 2013" if dataid=="17605" & svy==1 & SampleColDate=="26 Jun 1900" 



********************************************************************************
*Generate child ages
********************************************************************************
gen date = date(SampleColDate, "DMY")
format date %d
	label var date "Date of sample collection"
	
gen aged = date-DOB
	label var aged "Age in days (anthro meas)"
gen double agem = aged/30.4375
	label var agem "Age in months (anthro meas)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas)"
codebook agey

* Month of measurement
gen month = month(date)
	label var month "Month of sample collection"


	
*--------------------------------------------
* birth order of target children
* collected at year 2, so backfill
*--------------------------------------------
/*
gen int birthord = .
	replace birthord = q4020_1b if childid=="T1"
	replace birthord = q4021_1b if childid=="T2"
	label var birthord "Birth order (target child)"
	
sort dataid childid svy
by dataid childid: egen _x = min(birthord)
replace birthord = _x if birthord==.
drop _x

* some missing values in the measure
* 247 at year 1, 5 at year 2
tab birthord if svy==1
tab birthord if svy==2
*/

*Rename anthro variables to match main trial conventions
rename Weight1 weight1
rename Weight2 weight2
rename Weight3 weight3
rename Len1 len1
rename Len2 len2 
rename Len3 len3



*--------------------------------------------
* ensure that all the anthro measurements 
* are rounded to the correct level of precision
* no more than 2 decimal places
*--------------------------------------------
for any hc1 hc2 hc3 muac1 muac2 muac3 weight1 weight2 weight3 len1 len2 len3: replace X = round(X,0.01)

*--------------------------------------------
* Calculate median length measurements
*--------------------------------------------
replace len1 = . if len1>999 | len1<=0
replace len2 = . if len2>999 | len2<=0
replace len3 = . if len3>999 | len3<=0


egen double length = rowmedian(len1 len2 len3)
	replace length = . if length>999
	replace length = round(length,0.001)
	label var length "Child length (median), cm"
	notes length: Median of replicate measures
drop len1-len3
*format %16.11g length

*--------------------------------------------
* Calculate median weight measurements
*--------------------------------------------

*Need to add mother's weight later
for any  weight1 weight2 weight3: replace X = . if (X>99 | X<=0)

*Note: Check with Jade if we need to do this
* create median of mom + child and mom alone
* we have to do it this way because this reflects
* the method used in the field to determine if a 
* 3rd measurement was required
*egen double wmc = rowmedian(c408 c409 c410)
*egen double wmm = rowmedian(c404 c405 c406)

* create median of the child (if measured alone)
egen double wch = rowmedian(weight1 weight2 weight3)
*154 missing values
 
/*
gen double weight = wch
	replace weight = wmc-wmm if (weight==.) & (wmc!=.) & (wmm!=.)
	replace weight = round(weight,0.001)
	label var weight "Child weight (median), kg"
	notes weight: Median of replicate measures

drop  wmc wmm wch
*/

*--------------------------------------------
* Calculate median head circumference measurements
*--------------------------------------------
replace hc1 = . if hc1>99 | hc1<=0
replace hc2 = . if hc2>99 | hc2<=0
replace hc3 = . if hc3>99 | hc3<=0

egen double headcir = rowmedian(hc1 hc2 hc3)
	replace headcir = . if headcir>99
	replace headcir = round(headcir,0.001)
	label var headcir "Child head circumference (median), cm"
	notes headcir: Median of replicate measures


*--------------------------------------------
* Calculate laz, waz, whz, using the zscore06
* add-on package. do not specify oedema
*--------------------------------------------
codebook female
generate sex=female+1 
*recode sex(0=2)
codebook sex

*Run zcore function
zscore06, a(agem) s(sex) h(length) w(wch) female(2) male(1) measure(length_method) recum(1) stand(2)

* save a temporary dataset to merge with the WHO igrowup output
save "C:/Users/andre/Dropbox/WASHB-EE-Analysis/WBB-EE-analysis/Data/Temp/washb-working-anthro-Andrew.dta", replace


*--------------------------------------------
* Calculate laz, waz, whz, and wcz using the 
* WHO -igrowup- macro. 
*
* this is necessary because the zscore06 
* package does not calculate z-scores for 
* head circumference.  we are using both
* approaches because the zscore06 package is
* ostensibly more accurate (see the package's documentation)
* though as demonstrated below they provide identical results
* (just a good internal validity check)
*
* The WHO -igrowup- macro requires 15 parameters.
* Refer to the documentation for details on this
* finicky macro.
*
* We don't have all of the measurements that
* it processes, so need to create empty
* variables that allow it to run
*---------------------------------------------

/// Indicate to the Stata compiler where the igrowup_standard.ado file is stored ***
adopath + "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup stata/"

*cd ~"\Dropbox\Kenya Primary Analysis\igrowup_stata"
gen str reflib="~/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup stata/"
	lab var reflib "Directory of reference tables"

gen str datalib="~/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/"
	lab var datalib "Directory for datafiles"

gen str datalab="TEMPanthro"
	lab var datalab "Working file"

gen str ageunit="days"
gen str measure="L" if length_method==1
replace measure="H" if length_method==2
replace measure=" " if measure == ""
	label var measure "Height measured lying -L- or standing -H-"

* create a temporary sex variable for WHO coding
*gen whosex = sex
*	replace whosex = 2 if sex==0

* create missing variables so that macro will run
for any uac triskin subskin oedema: gen X = .

* set sampling wgtghts to negative to make the prevalence
* calculations blow up -- impossible to run that piece of 
* code w/o Stata SE b/c requires 10,000 variables
gen sw = -10



*---------------------------------------------
* Save a temporary dataset and run -igrowup-
* (note: igrowup adds variables to the data in
* memory and saves a copy dataset with the 
* suffix "_z_st.dta"). Dataset name must correspond
* to the "datalab" variable (defined in last chunk)
*---------------------------------------------

keep headcir dataid childNo date svy clusterid aged sex ageunit wch length measure headcir uac triskin subskin oedema sw  reflib datalib datalab 
rename wch weight

save "~/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro.dta", replace

*igrowup_standard reflib datalib datalab sex aged ageunit weight length measure headcir uac triskin subskin oedema sw

#delimit;
igrowup_standard reflib datalib datalab 
sex aged ageunit weight length measure headcir uac triskin subskin oedema sw;
#delimit cr



*---------------------------------------------
* Retrieve WHO calculated output
* "_f" variables identify variables outside of
* reasonable bounds
*
* merge back to the main anthro dataset
*---------------------------------------------
use "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro_z_st", clear

keep headcir childNo dataid svy clusterid _zwei _zlen _zbmi _zwfl _zhc _fwei _flen _fbmi _fwfl _fhc
sort dataid childNo svy
save  "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro", replace

use "~/Dropbox/Kenya Primary Analysis/Data-selected/temp/washb-working-anthro", clear
sort dataid childNo svy
drop _merge
destring clusterid, replace
merge 1:1 dataid childNo svy using "C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/Kenya cleaning .do files/WHO igrowup workdata/TEMPanthro"
assert _merge==3
drop _merge


*Replace "99" missing codes with .
for any haz06 waz06 whz06 bmiz06: replace X = . if X==99


* compare measurements from the 2 packages to ensure they are identical
* (correlation = 1)
corr haz06 _zlen 
corr waz06 _zwei
corr whz06 _zwfl //Don't match
corr bmiz06 _zbmi


* delete tempfiles
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/washb-working-anthro.dta"
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/TEMPanthro.dta"
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/TEMPanthro_z_st.dta"
*erase "~/dropbox/washbenefits/bangladesh/trial/data/temp/TEMPanthro_z_st.xls"


*--------------------------------------------
* Set extreme values to missing and flag them
* based on the WHO 2006 standards
*--------------------------------------------
rename haz06 laz
rename waz06 waz
rename whz06 whz
rename bmiz06 bmiz
rename _zhc hcz

gen laz_x = (laz < -6 | laz >6)
	replace laz_x = . if laz==.
	label var laz_x "abs(LAZ)>6, set to missing"
	
gen waz_x = (waz < -6 | waz >5)
	replace waz_x = . if waz==.
	label var waz_x "WAZ < -6 or WAZ > 5, set to missing"

gen whz_x = (whz < -5 | whz >5)
	replace whz_x = . if whz==.
	label var whz_x "abs(WHZ)>5, set to missing"
	
gen bmiz_x = (bmiz < -5 | bmiz >5)
	replace bmiz_x = . if bmiz==.
	label var bmiz_x "abs(BMIZ)>5, set to missing"

gen hcz_x = _fhc
	replace hcz_x = . if hcz==.
	label var hcz_x "abs(HCZ)>5, set to missing"

* list extreme values before setting them to missing
/*
list childid aged length weight laz waz whz if laz_x==1
list childid aged length weight laz waz whz if waz_x==1
list childid aged length weight laz waz whz if whz_x==1
list childid aged length weight laz waz whz if bmiz_x==1
list childid aged headcir hcz if hcz_x==1
*/
replace laz = . if laz_x==1
replace waz = . if waz_x==1
replace whz = . if whz_x==1
replace bmiz = . if bmiz_x==1
replace hcz = . if hcz_x==1

* drop extra Z-score calculation variables
drop _z* _f*

*--------------------------------------------
* Identify children who are stunted, 
* underweight, or wasted based on their Z-scores
*--------------------------------------------

gen byte lazminus2 = laz < -2
	replace lazminus2 =. if laz==. | laz_x==1
	label var lazminus2 "Stunted (LAZ<-2)"
gen byte lazminus3 = laz < -3
	replace lazminus3 =. if laz==. | laz_x==1
	label var lazminus3 "Severely Stunted (LAZ<-3)"

gen byte wazminus2 = waz < -2
	replace wazminus2 =. if waz==. | waz_x==1
	label var wazminus2 "Underweight (WAZ<-2)"
gen byte wazminus3 = waz < -3
	replace wazminus3 =. if waz==. | waz_x==1
	label var wazminus3 "Severely Underweight (WAZ<-3)"

gen byte whzminus2 = whz < -2
	replace whzminus2 =. if whz==. | whz_x==1
	label var whzminus2 "Wasted (WHZ<-2)"
gen byte whzminus3 = whz < -3
	replace whzminus3 =. if whz==. | whz_x==1
	label var whzminus3 "Severely Wasted (WHZ<-3)"
	
	
*--------------------------------------------
* Save an analysis dataset
*--------------------------------------------


* restrict to variables used in the analysis
keep dataid childNo date DOB month length svy aged agem agey month headcir laz waz whz bmiz hcz laz_x waz_x whz_x hcz_x lazminus2 bmiz_x lazminus3 wazminus2 wazminus3 whzminus3 whzminus2

*check childid formatting for R merge
*format childid %9.0f


*reshape to wide
*Reshape to wide
reshape wide date aged agem agey month length headcir laz waz whz bmiz hcz laz_x waz_x whz_x hcz_x lazminus2 bmiz_x lazminus3 wazminus2 wazminus3 whzminus3 whzminus2, i(dataid childNo) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 
duplicates list dataid


compress
sort dataid childNo 
label data "Bangladesh EE anthropometry analysis dataset, created by BD-EE-dm-anthro.do"
saveold "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-anthro.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-anthro.csv", comma replace




* write a codebook for the dataset
log close
log using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/washb-eed-bd-anthro-codebook.txt", text replace
desc
codebook, c
codebook
log close

exit

