




*Clean raw WBK EED data files
*Save raw data files as csv files to load into R

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eebl_med_audrie_20180228.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_med.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eebl_stool_audrie_20180228.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_stool.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeml_med_audrie_20180228.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_med.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeml_stool_audrie_20180228.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_stool.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeel_med_audrie_20180228.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_med.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeel_stool_audrie_20180228.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_stool.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_randTreatment_audrie_20180301.dta", clear
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_blindTR.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washb-kenya-tr.dta", clear
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_TR.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_request_audrie_adjcovs_20180228.dta", clear
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_covariates.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_request_audrie_allchild_20180228.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_allchild_covariates.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_ee_childLM_20180210.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_LM.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_smallchildvars_20180210.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_smallchildvars.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/ee_bl_append_ID_clean_urine_20180210.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_urine.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/ee_ml_append_ID_clean_urine_20170831.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_urine.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/ee_el_append_ID_clean_urine_20170831.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_urine.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/Kenya-Baseline-LM-Data-September 2017 (1).dta", clear
tostring Child_ID, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_LM.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/Kenya-Midline-LM Data-September 25, 2017.dta", clear
tostring Child_ID, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_LM.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/Kenya Endline LM Data-Revised-September 25, 2017.dta", clear
tostring Child_ID, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_LM.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eebl_blood_audrie_20180301.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_blood.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeml_blood_audrie_20180301.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_blood.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeel_blood_audrie_20180306.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_blood.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eebl_anthro_audrie_20180301.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_anthro.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeml_anthro_audrie_20180301.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_anthro.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washk_eeel_anthro_audrie_20180301.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_anthro.csv", comma replace




use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Baseline_ELISA_AAT.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_aat.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Baseline_ELISA_MPO.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_mpo.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Baseline_ELISA_NEO.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_neo.csv", comma replace


use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Midline_ELISA_AAT.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_aat.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Midline_ELISA_MPO.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_mpo.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Midline_ELISA_NEO.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_neo.csv", comma replace


use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Endline_ELISA_AAT.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_aat.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Endline_ELISA_MPO.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_mpo.csv", comma replace

use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/cleanedkenyaeestooldata/Compiled_Endline_ELISA_NEO.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_neo.csv", comma replace


use "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/washb-kenya-ee-stool-lab-t1-t2-t3.dta", clear
tostring childid, replace 
outsheet using "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_stool.csv", comma replace










