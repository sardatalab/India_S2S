
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
str reasonforsubstitution 42-42	///
hhsize 43-44 					///
hhtype 45-45 					///
religion 46-46 					///
socialgroup 47-47 				///
hh_usual_cons_rs 48-55 			///
str informantno 56-57 			///
str surveydate 58-65 				///
str timetaken 66-69 				///
nss 70-72 						///
nsc 73-75 						///
mult 76-85 						///
no_qtr 86-86 					///
using "${input}\HHV1.txt";
#delimit cr

* Household ID
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
isid hhid

gen year = "2019-20"
gen 	hhwt = mult/200
replace hhwt = mult/100 	if  nss==nsc 
replace hhwt = hhwt/no_qtr
gen 	 pwt = hhwt*hhsize
save "${output}\hh_visit1_2019-20.dta", replace


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
str sub_status 80-81 								///
str sub_industry 82-86 							///
str sub_occupation 87-89 							///
str sub_workplace 90-91 							///
str sub_enterprise 92-93 							///
str sub_n_workers 94-94 							///
str sub_job_contract 95-95 						///
str sub_paid_leave 96-96 							///
str sub_soc_security 97-97 						///
str status_act1_day7 98-99 						///
str industry_act1_day7 100-101 					///
str hours_act1_day7 102-103 						///
str wage_act1_day7 104-108 						///
str status_act2_day7 109-110 						///
str industry_act2_day7 111-112 					///
str hours_act2_day7 113-114 						///
str wage_act2_day7 115-119 						///
str total_hours_day7 120-121 						///
str hours_avlble_day7 122-123 						///
str status_act1_day6 124-125 						///
str industry_act1_day6 126-127 					///
str hours_act1_day6 128-129 						///
str wage_act1_day6 130-134 						///
str status_act2_day6 135-136 						///
str industry_act2_day6 137-138 					///
str hours_act2_day6 139-140 						///
str wage_act2_day6 141-145 						///
str total_hours_day6 146-147 						///
str hours_avlble_day6 148-149 						///
str status_act1_day5 150-151 						///
str industry_act1_day5 152-153 					///
str hours_act1_day5 154-155 						///
str wage_act1_day5 156-160 						///
str status_act2_day5 161-162 						///
str industry_act2_day5 163-164 					///
str hours_act2_day5 165-166 						///
str wage_act2_day5 167-171 						///
str total_hours_day5 172-173 						///
str hours_avlble_day5 174-175 						///
str status_act1_day4 176-177 						///
str industry_act1_day4 178-179 					///
str hours_act1_day4 180-181 						///
str wage_act1_day4 182-186 						///
str status_act2_day4 187-188 						///
str industry_act2_day4 189-190 					///
str hours_act2_day4 191-192 						///
str wage_act2_day4 193-197 						///
str total_hours_day4 198-199 						///
str hours_avlble_day4 200-201 						///
str status_act1_day3 202-203 						///
str industry_act1_day3 204-205 					///
str hours_act1_day3 206-207 						///
str wage_act1_day3 208-212 						///
str status_act2_day3 213-214 						///
str industry_act2_day3 215-216 					///
str hours_act2_day3 217-218 						///
str wage_act2_day3 219-223 						///
str total_hours_day3 224-225 						///
str hours_avlble_day3 226-227 						///
str status_act1_day2 228-229 						///
str industry_act1_day2 230-231 					///
str hours_act1_day2 232-233 						///
str wage_act1_day2 234-238 						///
str status_act2_day2 239-240 						///
str industry_act2_day2 241-242 					///
str hours_act2_day2 243-244 						///
str wage_act2_day2 245-249 						///
str total_hours_day2 250-251 						///
str hours_avlble_day2 252-253 						///
str status_act1_day1 254-255 						///
str industry_act1_day1 256-257 					///
str hours_act1_day1 258-259 						///
str wage_act1_day1 260-264 						///
str status_act2_day1 265-266 						///
str industry_act2_day1 267-268 					///
str hours_act2_day1 269-270 						///
str wage_act2_day1 271-275 						///
str total_hours_day1 276-277 						///
str hours_avlble_day1 278-279 						///
status_cws 280-281 								///
nic_cws 282-283 									///
str cws_occupation 284-286 						///
str earnings_sal 287-294 							///
str earnings_selfemp 295-302 						///
str nss 303-305 									///
str nsc 306-308 									///
str mult 309-318 								///
str no_qtr 319-319 								///
using "${input}\PERV1.txt";
#delimit cr


* Household ID
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
egen  pid = concat(quarter visit fsu sample_sg stage2_stratumno hhno personno)
isid  pid
gen year = "2019-20"

destring mult nss nsc no_qtr, replace
keep if !missing(relationtohead)
save "${output}\person_visit1_2019-20.dta", replace

merge m:1 hhid using "${output}\hh_visit1_2019-20.dta", keepusing(surveycode hhsize hhtype month religion socialgroup hh_usual_cons_rs hhwt pwt)
drop _merge
destring, replace
save "${output}\IND_2019_PLFS-V1_v01_M.dta", replace
