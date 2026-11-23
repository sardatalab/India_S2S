*<_Program setup_>
set more off

	
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
str prn_produc_usage 80-80							///
str prn_works_given_prod 81-81						///
str prn_prod_specification 82-82					///
str sub_status 83-84 								///
str sub_industry 85-89 							///
str sub_occupation 90-92 							///
str sub_workplace 93-94 							///
str sub_enterprise 95-96 							///
str sub_n_workers 97-97 							///
str sub_job_contract 98-98 						///
str sub_paid_leave 99-99 							///
str sub_soc_security 100-100						///
str sub_product_usage 101-101						///
str sub_works_given_prod 102-102					///
str sub_prod_specification 103-103					///
str ever_workedpre365 104-104						///
str duration_prn_act 105-105						///
str duration_sub_act 106-106						///
str search_effort 107-107							///
str duration_unemp 108-108						///
str ever_worked 109-109							///
str not_work_reason 110-110						///
str reason_act_lf 111-111							///
str status_act1_day7 112-113						///
str industry_act1_day7 114-115 					///
str hours_act1_day7 116-117 						///
str wage_act1_day7 118-122 						///
str status_act2_day7 123-124 						///
str industry_act2_day7 125-126 					///
str hours_act2_day7 127-128 						///
str wage_act2_day7 129-133 						///
str total_hours_day7 134-135 						///
str hours_avlble_day7 136-137 						///
str status_act1_day6 138-139 						///
str industry_act1_day6 140-141 					///
str hours_act1_day6 142-143 						///
str wage_act1_day6 144-148 						///
str status_act2_day6 149-150 						///
str industry_act2_day6 151-152 					///
str hours_act2_day6 153-154 						///
str wage_act2_day6 155-159 						///
str total_hours_day6 160-161 						///
str hours_avlble_day6 162-163 						///
str status_act1_day5 164-165 						///
str industry_act1_day5 166-167 					///
str hours_act1_day5 168-169 						///
str wage_act1_day5 170-174 						///
str status_act2_day5 175-176 						///
str industry_act2_day5 177-178 					///
str hours_act2_day5 179-180 						///
str wage_act2_day5 181-185 						///
str total_hours_day5 186-187 						///
str hours_avlble_day5 188-189 						///
str status_act1_day4 190-191 						///
str industry_act1_day4 192-193 					///
str hours_act1_day4 194-195 						///
str wage_act1_day4 196-200 						///
str status_act2_day4 201-202 						///
str industry_act2_day4 203-204 					///
str hours_act2_day4 205-206 						///
str wage_act2_day4 207-211 						///
str total_hours_day4 212-213 						///
str hours_avlble_day4 214-215 						///
str status_act1_day3 216-217 						///
str industry_act1_day3 218-219 					///
str hours_act1_day3 220-221 						///
str wage_act1_day3 222-226 						///
str status_act2_day3 227-228 						///
str industry_act2_day3 229-230 					///
str hours_act2_day3 231-232 						///
str wage_act2_day3 233-237 						///
str total_hours_day3 238-239 						///
str hours_avlble_day3 240-241 						///
str status_act1_day2 242-243 						///
str industry_act1_day2 244-245 					///
str hours_act1_day2 246-247 						///
str wage_act1_day2 248-252 						///
str status_act2_day2 253-254 						///
str industry_act2_day2 255-256 					///
str hours_act2_day2 257-258 						///
str wage_act2_day2 259-263 						///
str total_hours_day2 264-265 						///
str hours_avlble_day2 266-267 						///
str status_act1_day1 268-269 						///
str industry_act1_day1 270-271 					///
str hours_act1_day1 272-273 						///
str wage_act1_day1 274-278 						///
str status_act2_day1 279-280 						///
str industry_act2_day1 281-282 					///
str hours_act2_day1 283-284 						///
str wage_act2_day1 285-289 						///
str total_hours_day1 290-291 						///
str hours_avlble_day1 292-293 						///
status_cws 294-295 								///
nic_cws 296-297 									///
str cws_occupation 298-300 						///
str earnings_sal 301-308 							///
str earnings_selfemp 309-316 						///
str nss 317-319 									///
str nsc 320-322 									///
str mult 323-332 								///
str no_qtr 333-333 								///
using "${input}\PERV1.txt";
#delimit cr

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

