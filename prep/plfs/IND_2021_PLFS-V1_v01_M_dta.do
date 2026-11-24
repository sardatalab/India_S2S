* IND_2021_PLFS-V1_v01_M_dta.do
* Same as IND_2021_PLFS-V1_v01_M.do, but reads the official SARRAW .dta files
* (P:\...\WORKINGDATA\IND\IND_2021_PLFS\IND_2021_PLFS_v01_M\Data\Original) instead of the
* MoSPI text files, which are not in the official collection for 2021. Called by prep/00_master.do.
*<_Program setup_>
set more off

	
*<_Datalibweb request_>

****************************************
**** HOUSEHOLD LEVEL FIRST VISIT 
****************************************
use "${input}\hhvisit1_2021-22.dta", clear
* Official SARRAW file (datalibweb / P:): same data as MoSPI HHV1.TXT, checked with
* check_plfs_official_dta.do (no_qtr is the original value; it is recoded below)
foreach v in hhid year hhwt pwt {          // built again below (one by one: cap drop of a list drops nothing if one is missing)
	cap drop `v'
}
gen numero_quarter = no_qtr 

replace no_qtr = 1 	if  quarter=="Q1"
replace no_qtr = 2 	if  quarter=="Q2"
replace no_qtr = 3 	if  quarter=="Q3"
replace no_qtr = 4 	if  quarter=="Q4"

* Household ID
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
isid hhid

gen year = "2021-22"
gen 	hhwt = mult/200
replace hhwt = mult/100 	if  nss==nsc 
replace hhwt = hhwt/numero_quarter
gen 	 pwt = hhwt*hhsize
save "${output}\hh_visit1_2021-22.dta", replace


****************************************
**** PERSON LEVEL FIRST VISIT 
****************************************
use "${input}\personvisit1_2021-22.dta", clear
* Official SARRAW file (datalibweb / P:): same data as MoSPI PERV1.txt, checked with
* check_plfs_official_dta.do (household variables come from the household file below)
foreach v in hhid pid year hhsize hhwt pwt _merge {   // one by one: cap drop of a list drops nothing if one is missing
	cap drop `v'
}
tostring no_qtr, replace     // numeric in the official file, string in the text-file version

replace no_qtr = "1" 	if  quarter=="Q1"
replace no_qtr = "2" 	if  quarter=="Q2"
replace no_qtr = "3" 	if  quarter=="Q3"
replace no_qtr = "4" 	if  quarter=="Q4"

* Household ID
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
egen  pid = concat(quarter visit fsu sample_sg stage2_stratumno hhno personno)
isid  pid
gen year = "2021-22"

destring mult nss nsc no_qtr, replace
keep if !missing(relationtohead)
save "${output}\person_visit1_2021-22.dta", replace

merge m:1 hhid using "${output}\hh_visit1_2021-22.dta", keepusing(surveycode hhsize hhtype month religion socialgroup hh_usual_cons_gs hh_imputed_cons_rs_hg hh_imputed_cons_rs_ik hh_exp_clothes hh_exp_dur hh_usual_cons_rs hhwt pwt)
drop _merge
destring, replace
save "${output}\IND_2021_PLFS-V1_v01_M.dta", replace

