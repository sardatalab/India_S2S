
*<_Program setup_>
*clear all
set more off


*</_Program setup_>

	
*<_Datalibweb request_>

****************************************
**** HOUSEHOLD LEVEL FIRST VISIT 
****************************************
clear
#delimit ;
infix  							///			
str fileid 1-4 					///
str sch 5-7 						///
str quarter 8-9 					///
str visit 10-11 					///
sector 12-12 					///
state 13-14 						///
str district 15-16 				///
str nssregion 17-19 				///
str stratum 20-21 				///
str substratum 22-23 				///
str subsample 24-24 				///
str fodsub_region 25-28 			///
str fsu 29-33 					///
str sample_sg 34-34 				///
str stage2_stratumno 35-35 		///
str hhno 36-37 					///
str month 38-39 					///
str responsecode 40-40 			///
str surveycode 41-41 				///
str reasonforsubstitution 42-42 	///
hhsize 43-44 					///
hhtype 45-45 					///
religion 46-46 					///
socialgroup 47-47 				///
hh_usual_cons_gs 48-55 			///
str hh_imputed_cons_rs_hg 56-63 	///
str hh_imputed_cons_rs_ik 64-71 	///
hh_exp_clothes 72-79 				///
hh_exp_dur 80-87 				///
hh_usual_cons_rs 88-95 			///
str informantno 96-97 			///
str surveydate 98-105 			///
str timetaken 106-109 			///
nss 110-112 						///
nsc 113-115 						///
mult 116-125 					///
no_qtr 126-126 					///
using "${input}\HHV1.txt";
#delimit cr

* Household ID
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
isid hhid

gen year = "2020-21"
gen 	hhwt = mult/200
replace hhwt = mult/100 	if  nss==nsc 
replace hhwt = hhwt/no_qtr
gen 	 pwt = hhwt*hhsize
save "${output}\hh_visit1_2020-21.dta", replace


****************************************
**** PERSON LEVEL FIRST VISIT 
****************************************
clear 
#delimit ;
infix  											///
str fileid 1-4	 								///
str sch 5-7 										///
str quarter 8-9 									///
str visit 10-11 									///
sector 12-12 									///
state 13-14 										///
district 15-16 									///
nss_region 17-19 								///
stratum 20-21 									///
substratum 22-23 								///
subsample 24-24 									///
str fod_sub_region 25-28 							///
str fsu 29-33 									///
str sample_sg 34-34 								///
str stage2_stratumno 35-35 						///
str hhno 36-37 									///
str personno 38-39 								///
relationtohead 40-40 								///
sex 41-41 										///
age 42-44 										///
maritalstat 45-45 								///
geneducation 46-47 								///
techeducation 48-49 								///
noyrsformaled 50-51 								///
edu_status 52-53 								///
vocat 54-54 										///
str vocat_365 55-55 								///
str training_field 56-57 							///
str training_duration 58-58 						///
str training_type 59-59 							///
str training_fund_source 60-60 					///
prn_status 61-62 								///
str prn_industry 63-67 							///
str prn_occ 68-70 								///
str subsidiary_work 71-71 						///
str prn_workplace 72-73 							///
str prn_enterprise 74-75 							///
str prn_n_workers 76-76 							///
str prn_job_contract 77-77 						///
str prn_paid_leave 78-78 							///
str prn_soc_security 79-79 						///
str prn_product_usage 80-80						///
str sub_status 81-82 								///
str sub_industry 83-87 							///
str sub_occupation 88-90 							///
str sub_workplace 91-92 							///
str sub_enterprise 93-94 							///
str sub_n_workers 95-95 							///
str sub_job_contract 96-96 						///
str sub_paid_leave 97-97 							///
str sub_soc_security 98-98 						///
str sub_product_usage 99-99						///
str ever_workedpre365 100-100						///
str duration_prn_act 101-101						///
str duration_sub_act 102-102						///
str search_effort 103-103							///
str duration_unemp 104-104						///
str ever_worked 105-105							///
str not_work_reason 106-106						///
str reason_act_lf 107-107							///
str status_act1_day7 108-109						///
str industry_act1_day7 110-111 					///
str hours_act1_day7 112-113 						///
str wage_act1_day7 114-118 						///
str status_act2_day7 119-120 						///
str industry_act2_day7 121-122 					///
str hours_act2_day7 123-124 						///
str wage_act2_day7 125-129 						///
str total_hours_day7 130-131 						///
str hours_avlble_day7 132-133 						///
str status_act1_day6 134-135 						///
str industry_act1_day6 136-137 					///
str hours_act1_day6 138-139 						///
str wage_act1_day6 140-144 						///
str status_act2_day6 145-146 						///
str industry_act2_day6 147-148 					///
str hours_act2_day6 149-150 						///
str wage_act2_day6 151-155 						///
str total_hours_day6 156-157 						///
str hours_avlble_day6 158-159 						///
str status_act1_day5 160-161 						///
str industry_act1_day5 162-163 					///
str hours_act1_day5 164-165 						///
str wage_act1_day5 166-170 						///
str status_act2_day5 171-172 						///
str industry_act2_day5 173-174 					///
str hours_act2_day5 175-176 						///
str wage_act2_day5 177-181 						///
str total_hours_day5 182-183 						///
str hours_avlble_day5 184-185 						///
str status_act1_day4 186-187 						///
str industry_act1_day4 188-189 					///
str hours_act1_day4 190-191 						///
str wage_act1_day4 192-196 						///
str status_act2_day4 197-198 						///
str industry_act2_day4 199-200 					///
str hours_act2_day4 201-202 						///
str wage_act2_day4 203-207 						///
str total_hours_day4 208-209 						///
str hours_avlble_day4 210-211 						///
str status_act1_day3 212-213 						///
str industry_act1_day3 214-215 					///
str hours_act1_day3 216-217 						///
str wage_act1_day3 218-222 						///
str status_act2_day3 223-224 						///
str industry_act2_day3 225-226 					///
str hours_act2_day3 227-228 						///
str wage_act2_day3 229-233 						///
str total_hours_day3 234-235 						///
str hours_avlble_day3 236-237 						///
str status_act1_day2 238-239 						///
str industry_act1_day2 240-241 					///
str hours_act1_day2 242-243 						///
str wage_act1_day2 244-248 						///
str status_act2_day2 249-250 						///
str industry_act2_day2 251-252 					///
str hours_act2_day2 253-254 						///
str wage_act2_day2 255-259 						///
str total_hours_day2 260-261 						///
str hours_avlble_day2 262-263 						///
str status_act1_day1 264-265 						///
str industry_act1_day1 266-267 					///
str hours_act1_day1 268-269 						///
str wage_act1_day1 270-274 						///
str status_act2_day1 275-276 						///
str industry_act2_day1 277-278 					///
str hours_act2_day1 279-280 						///
str wage_act2_day1 281-285 						///
str total_hours_day1 286-287 						///
str hours_avlble_day1 288-289 						///
status_cws 290-291 								///
nic_cws 292-293 									///
str cws_occupation 294-296 						///
str earnings_sal 297-304 							///
str earnings_selfemp 305-312 						///
str present_enuplace_diff 313-313					///           
str moved_postMar20 314-314						///
str present_enuplace_usual 315-315					///
str enu_last_usual_place 316-316					///
str enu_st_code_last_place 317-318					///
str enu_moving_reason 319-320						///
str enu_will_move 321-321							///
str enu_prn_act_last_place 322-323					///
str enu_nic_division 324-325						///
str enu_noc_group 326-328							///
str highest_edu_level 329-330						///
str present_res_diff 331-331						///
str present_res_usual 332-332						///
str res_last_usual_place 333-333					///
str res_st_code_last_place 334-335					///	
str res_moving_reason 336-337						///
str res_will_move 338-338							///
str res_prn_act_last_place 339-340					///
str res_nic_division 341-342						///
str res_noc_group 343-345							///
str nss 346-348 									///
str nsc 349-351 									///
str mult 352-361 								///
str no_qtr 362-362 								///
using "${input}\PERV1.txt";
#delimit cr


* Household ID
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
egen  pid = concat(quarter visit fsu sample_sg stage2_stratumno hhno personno)
isid  pid
gen year = "2020-21"

destring mult nss nsc no_qtr, replace
keep if !missing(relationtohead)
save "${output}\person_visit1_2020-21.dta", replace

merge m:1 hhid using "${output}\hh_visit1_2020-21.dta", keepusing(surveycode hhsize hhtype month religion socialgroup hh_usual_cons_gs hh_imputed_cons_rs_hg hh_imputed_cons_rs_ik hh_exp_clothes hh_exp_dur hh_usual_cons_rs hhwt pwt)
drop _merge
destring, replace
save "${output}\IND_2020_PLFS-V1_v01_M.dta", replace

