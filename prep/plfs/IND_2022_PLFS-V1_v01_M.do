*<_Program setup_>
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
egen hhid2 = concat(quarter visit fsu sample_sg stage2_stratumno hhno)

gen year = "2022-23"
gen 	hhwt = mult/200
replace hhwt = mult/100 	if  nss==nsc 
replace hhwt = hhwt/no_qtr
gen 	 pwt = hhwt*hhsize
save "${output}\hh_visit1_2022-23.dta", replace


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
str prn_product_usage 80-80 						///
str sub_status 81-82 								///
str sub_industry 83-87 							///
str sub_occupation 88-90 							///
str sub_workplace 91-92 							///
str sub_enterprise 93-94 							///
str sub_n_workers 95-95 							///
str sub_job_contract 96-96 						///
str sub_paid_leave 97-97 							///
str sub_soc_security 98-98 						///
str sub_product_usage 99-99 						///
str ever_workedpre365 100-100	 					///
str duration_prn_act 101-101 						///
str duration_sub_act 102-102 						///
str search_effort 103-103 						///
str duration_unemp 104-104 						///
str ever_worked 105-105 							///
str not_work_reason 106-107 						///
str reason_act_lf 108-108 						///
str status_act1_day7 109-110 						///
str industry_act1_day7 111-112 					///
str hours_act1_day7 113-114 						///
str wage_act1_day7 115-119 						///
str status_act2_day7 120-121 						///
str industry_act2_day7 122-123 					///
str hours_act2_day7 124-125 						///
str wage_act2_day7 126-130 						///
str total_hours_day7 131-132 						///
str hours_avlble_day7 133-134 						///
str status_act1_day6 135-136 						///
str industry_act1_day6 137-138 					///
str hours_act1_day6 139-140 						///
str wage_act1_day6 141-145 						///
str status_act2_day6 146-147 						///
str industry_act2_day6 148-149 					///
str hours_act2_day6 150-151 						///
str wage_act2_day6 152-156 						///
str total_hours_day6 157-158 						///
str hours_avlble_day6 159-160 						///
str status_act1_day5 161-162 						///
str industry_act1_day5 163-164 					///
str hours_act1_day5 165-166 						///
str wage_act1_day5 167-171 						///
str status_act2_day5 172-173 						///
str industry_act2_day5 174-175 					///
str hours_act2_day5 176-177 						///
str wage_act2_day5 178-182 						///
str total_hours_day5 183-184 						///
str hours_avlble_day5 185-186 						///
str status_act1_day4 187-188 						///
str industry_act1_day4 189-190 					///
str hours_act1_day4 191-192 						///
str wage_act1_day4 193-197 						///
str status_act2_day4 198-199 						///
str industry_act2_day4 200-201 					///
str hours_act2_day4 202-203 						///
str wage_act2_day4 204-208 						///
str total_hours_day4 209-210 						///
str hours_avlble_day4 211-212 						///
str status_act1_day3 213-214 						///
str industry_act1_day3 215-216 					///
str hours_act1_day3 217-218 						///
str wage_act1_day3 219-223 						///
str status_act2_day3 224-225 						///
str industry_act2_day3 226-227 					///
str hours_act2_day3 228-229 						///
str wage_act2_day3 230-234 						///
str total_hours_day3 235-236 						///
str hours_avlble_day3 237-238 						///
str status_act1_day2 239-240 						///
str industry_act1_day2 241-242 					///
str hours_act1_day2 243-244 						///
str wage_act1_day2 245-249 						///
str status_act2_day2 250-251 						///
str industry_act2_day2 252-253 					///
str hours_act2_day2 254-255 						///
str wage_act2_day2 256-260 						///
str total_hours_day2 261-262 						///
str hours_avlble_day2 263-264 						///
str status_act1_day1 265-266 						///
str industry_act1_day1 267-268 					///
str hours_act1_day1 269-270 						///
str wage_act1_day1 271-275 						///
str status_act2_day1 276-277 						///
str industry_act2_day1 278-279 					///
str hours_act2_day1 280-281 						///
str wage_act2_day1 282-286 						///
str total_hours_day1 287-288 						///
str hours_avlble_day1 289-290 						///
status_cws 291-292 								///
nic_cws 293-294 									///
str cws_occupation 295-297 						///
str earnings_sal 298-305 							///
str earnings_selfemp 306-313 						///
str nss 314-316 									///
str nsc 317-319 									///
str mult 320-329 								///
str no_qtr 330-330 								///
using "${input}\PERV1.txt";
#delimit cr

* Household ID
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
egen  pid = concat(quarter visit fsu sample_sg stage2_stratumno hhno personno)
isid  pid
gen year = "2022-23"

destring mult nss nsc no_qtr, replace
keep if !missing(relationtohead)
save "${output}\person_visit1_2022-23.dta", replace

merge m:1 hhid using "${output}\hh_visit1_2022-23.dta", keepusing(surveycode hhid2 hhsize hhtype month religion socialgroup hh_usual_cons_gs hh_imputed_cons_rs_hg hh_imputed_cons_rs_ik hh_exp_clothes hh_exp_dur hh_usual_cons_rs hhwt pwt)
drop _merge
destring, replace
save "${output}\IND_2022_PLFS-V1_v01_M.dta", replace

