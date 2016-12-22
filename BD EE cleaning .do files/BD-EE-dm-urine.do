capture log close
set more off
clear all


log using "C:/Users/andre/Dropbox/WBB-EE-analysis/Logs/Andrew/BD-dm-EE-urine.log", text replace

*--------------------------------------------
* 4-kenya-dm-urine.do
*
* andrew mertens (amertens@berkeley.edu)
*
* process the Bangladesh EED substudy time 1,2, and 3
* urine data for analysis
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
use "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Untouched/washb-bangladesh-blind-tr.dta", clear

destring clusterid, replace
sort clusterid
tempfile trdata
save `trdata'


*--------------------------------------------
* Append 3 rounds of stool collection surveys and rename variables
*--------------------------------------------
clear
cd "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Untouched"

use Endline/EE_Endline_Urine_MainForm_CLEANED_data_11July2016, clear
rename FaId staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol
rename q31s1hour2 h2sampid1
rename q31s2hour2 h2sampid2
rename q31s3hour2 h2sampid3
rename q31s4hour2 h2sampid4
rename q31s5hour2 h2sampid5
rename q31s6hour2 h2sampid6
rename q31s7hour5 h5sampid7
rename q31s8hour5 h5sampid8
rename q31s9hour5 h5sampid9
rename q31s10hour5 h5sampid10
rename q31s11hour5 h5sampid11
rename q31s12hour5 h5sampid12
rename q34s1hour2 h2aliqout1_t 
rename q34s2hour2 h2aliqout2_t
rename q34s3hour2 h2aliqout3_t
rename q34s4hour2 h2aliqout4_t
rename q34s5hour2 h2aliqout5_t
rename q34s6hour2 h2aliqout6_t
rename q34s7hour5 h5aliqout7_t
rename q34s8hour5 h5aliqout8_t
rename q34s9hour5 h5aliqout9_t
rename q34s10hour5 h5aliqout10_t
rename q34s11hour5 h5aliqout11_t
rename q34s12hour5 h5aliqout12_t
       

*destring clusterid, replace

keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t




*generate surveyyear variable
gen svy=3

tempfile c_urine3
save `c_urine3'

use Midline/Urine_Main_Midline_Cleaned_MatchedwEnrollment_2Feb16, clear
rename faid staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol
rename q31s1hour2 h2sampid1
rename q31s2hour2 h2sampid2
rename q31s3hour2 h2sampid3
rename q31s4hour2 h2sampid4
rename q31s5hour2 h2sampid5
rename q31s6hour2 h2sampid6
rename q31s7hour5 h5sampid7
rename q31s8hour5 h5sampid8
rename q31s9hour5 h5sampid9
rename q31s10hour5 h5sampid10
rename q31s11hour5 h5sampid11
rename q31s12hour5 h5sampid12
rename childno childNo
rename samplecoldate SampleColDate
rename numchildren NumChildren
rename q34s1hour2 h2aliqout1_t 
rename q34s2hour2 h2aliqout2_t
rename q34s3hour2 h2aliqout3_t
rename q34s4hour2 h2aliqout4_t
rename q34s5hour2 h2aliqout5_t
rename q34s6hour2 h2aliqout6_t
rename q34s7hour5 h5aliqout7_t
rename q34s8hour5 h5aliqout8_t
rename q34s9hour5 h5aliqout9_t
rename q34s10hour5 h5aliqout10_t
rename q34s11hour5 h5aliqout11_t
rename q34s12hour5 h5aliqout12_t

destring clusterid, replace

keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t

*generate surveyyear variable
gen svy=2

tempfile c_urine2
save `c_urine2'

use Baseline/Baseline_Urine_CLEANED_VersionIncorporated_30Sept14, clear
rename faid staffid
rename q7 consent
rename q8 nonconsent_reason 
rename q9 LMvol
rename q31s1hour2 h2sampid1
rename q31s2hour2 h2sampid2
rename q31s3hour2 h2sampid3
rename q31s4hour2 h2sampid4
rename q31s5hour2 h2sampid5
rename q31s6hour2 h2sampid6
rename q31s7hour5 h5sampid7
rename q31s8hour5 h5sampid8
rename q31s9hour5 h5sampid9
rename q31s10hour5 h5sampid10
rename q31s11hour5 h5sampid11
rename q31s12hour5 h5sampid12
rename childno childNo
rename samplecoldate SampleColDate
rename numchildren NumChildren
rename q34s1hour2 h2aliqout1_t 
rename q34s2hour2 h2aliqout2_t
rename q34s3hour2 h2aliqout3_t
rename q34s4hour2 h2aliqout4_t
rename q34s5hour2 h2aliqout5_t
rename q34s6hour2 h2aliqout6_t
rename q34s7hour5 h5aliqout7_t
rename q34s8hour5 h5aliqout8_t
rename q34s9hour5 h5aliqout9_t
rename q34s10hour5 h5aliqout10_t
rename q34s11hour5 h5aliqout11_t
rename q34s12hour5 h5aliqout12_t

destring clusterid, replace

keep dataid childNo clusterid staffid SampleColDate NumChildren consent nonconsent_reason LMvol h2sampid1 h2sampid2 h2sampid3 h2sampid4 h2sampid5 h2sampid6 h5sampid7 h5sampid8 h5sampid9 h5sampid10 h5sampid11 h5sampid12 h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t


*generate surveyyear variable
gen svy=1



*Append midline and endline anthropometry
append using `c_urine2', force nolabel
append using `c_urine3', force nolabel


*Temporarily limit variables in dataset to help with reshape
keep dataid clusterid svy nonconsent_reason childNo  h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t

*Reshape to wide
reshape wide nonconsent_reason  h2aliqout1_t h2aliqout2_t h2aliqout3_t h2aliqout4_t h2aliqout5_t h2aliqout6_t h5aliqout7_t h5aliqout8_t h5aliqout9_t h5aliqout10_t h5aliqout11_t h5aliqout12_t, i(dataid childNo) j(svy)
*Check for any duplicates after reshaping
duplicates list dataid childNo 

*Save file 
label data "BD EE urine dataset, created by BD-EE-dm-urine.do"
saveold "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-urine.dta", replace version(12)
outsheet using "C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Cleaned/Andrew/BD-EE-urine.csv", comma replace
clear



