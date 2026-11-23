global rootdatalib	"D:\Dropbox\s2s"

*<_Program setup_>
clear all
set more off

local code         	"IND"
local year         	"2022-23"
local survey       	"HCES"
local vm           	"01"
local type         	"SARMD"
local yearfolder   	"`code'_`year'_`survey'"
global input       	"${rootdatalib}\\`code'\\`yearfolder'\\`yearfolder'_v`vm'_M\Data\Original"
global output      	"${rootdatalib}\\`code'\\`yearfolder'\\`yearfolder'_v`vm'_M\Data\Stata"
*</_Program setup_>


*<_Datalibweb request_>

************************************************
**** HCES22_LVL_01: HOUSEHOLD CHARACTERISTICS 
************************************************
clear
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 survey_code 42-42 reason_substitute 43-43 mult 44-58 using "${input}\hces22_lvl_01.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

order hhid
save "${output}\LVL_01.dta", replace


************************************************
**** HCES22_LVL_02: HOUSEHOLD CHARACTERISTICS 
************************************************
clear
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 str personno 42-43 relation_to_head 44-44 sex 45-45 age 46-48 marital_status 49-49 highest_edu 50-51 years_edu 52-53 internet 54-54 days_home_away 55-56 meals_per_day_usual 57-57 meals_balwadi 58-59 meals_employer 60-61 meals_other 62-63 meals_paid 64-65 meals_home 66-67 str member_status 68-68 str fdq_og_member 69-69 mult 70-84 using "${input}\hces22_lvl_02.TXT"

egen hhid = concat(fsu sss hhno)
egen pid = concat(fsu sss hhno personno)
gen pwt = mult/100
	
order hhid pid
save "${output}\LVL_02.dta", replace


************************************************
**** HCES22_LVL_03: HOUSEHOLD CHARACTERISTICS 
************************************************	
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 hhsize 42-43 hhmember_economic_activity 44-44 str nco_code 45-47 str nic_code 48-52 maxincome_activity 53-53 majorincome_selfemployed 54-54 majorincome_salaried 55-55 majorincome_casual 56-56 str hhtype 57-57 religion 58-58 caste 59-59 land_ownership 60-60 typeof_land 61-61 area_owned 62-70 dwelling_unit 71-71 typeof_dwelling 72-72 wall_material 73-73 roof_material 74-74 floor_material 75-75 cooking 76-77 lighting 78-78 drinking_water 79-80 str time_taken_water 81-83 latrine_access 84-84 latrine_type 85-86 ration_card_type 87-87 rent_rate 88-88 pmgky_beneficiary 89-89 anydeath_below18 90-90 str noofdeath_below18 91-92 mult 93-107 using "${input}\hces22_lvl_03.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

order hhid
save "${output}\LVL_03.dta", replace


************************************************
**** HCES22_LVL_04: FOOD ITEMS 
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 ration_purchase 42-42 str ration_rice 43-43 str ration_wheat 44-44 str ration_grain 45-45 str ration_sugar 46-46 str ration_pulses 47-47 str ration_oil 48-48 str ration_other 49-49 str online_groceries 50-50 str online_milk 51-51 str online_vegetables 52-52 str online_fruits_fresh 53-53 str online_fruits_dry 54-54 str online_eggs 55-55 str online_processed_served 56-56 str online_processed_packed 57-57 str online_other 58-58 ceremony 59-59 meals_nonhh 60-63 mult 64-78 using "${input}\hces22_lvl_04.TXT"
	
egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100
	
order hhid
save "${output}\LVL_04.dta", replace


************************************************
**** HCES22_LVL_05: FOOD ITEMS (LAST 30 DAYS)
************************************************
clear 
infix str survey 1-4 str year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 item 42-44 str home_cons_quant 45-54 str home_cons_value 55-62 str qty_total 63-72 value_total 73-80 source 81-81 mult 82-96 using "${input}\hces22_lvl_05.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

replace  home_cons_quant = "." 	if  home_cons_quant=="M" | home_cons_quant=="NA" | home_cons_quant=="n"
destring home_cons_quant, replace

replace  home_cons_value = "." 	if  home_cons_value=="NA" | home_cons_value=="n"
destring home_cons_value, replace

replace  qty_total = "0.800" 	if  qty_total=="0.8OO"
destring qty_total, replace

order hhid 
save "${output}\LVL_05.dta", replace

collapse (sum) value_total, by(hhid)
tempfile level_5 
save "`level_5'"

************************************************
**** HCES22_LVL_06: FOOD ITEMS (LAST 7 DAYS) 
************************************************
clear
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 item 42-44 qty_total 45-54 value_total 55-62 source 63-63 mult 64-78 using "${input}\hces22_lvl_06.TXT"
	
egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

order hhid 
save "${output}\LVL_06.dta", replace

collapse (sum) value_total, by(hhid)

merge 1:1 hhid using "`level_5'"
count
keep hhid
tempfile control
save `control'
count
aaa

************************************************
**** HCES22_LVL_07: CONSUMABLE AND SERVICES 
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 kerosene 42-42 lpg 43-43 num_lpg 44-45 electricity_free 46-46 edu_institution 47-47 num_govt_institution 48-50 num_pvt_institution 51-53 books_free 54-54 num_books_free 55-57 stationary_free 58-58 num_stationary_free 59-61 schoolbag_free 62-62 num_schoolbag_free 63-65 other_free 66-66 num_other_free 67-69 fees_reimbursed 70-70 num_fees_reimbursed 71-72 pmjay_otherbenefit 73-73 num_beneficiaries_pmjay_other 74-75 hospitalization 76-76 medical_pmjay_other 77-77 num_medical_pmjay_other 78-79 amt_medical_pmjay_other 80-87 fuel_light_online 88-88 toilet_consumables_online 89-89 edu_online 90-90 medicine_online 91-91 services_online 92-92 internet_hh 93-93 mult 94-108 using "${input}\hces22_lvl_07.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100
	
order hhid
save "${output}\LVL_07.dta", replace


************************************************
**** HCES22_LVL_08: CONSUMABLES AND SERVICES 
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 item 42-44 str home_cons_quant 45-54 home_cons_value 55-62 str qty_total 63-72 value_total 73-80 source 81-81 mult 82-96 using "${input}\hces22_lvl_08.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

replace  home_cons_quant = "." 	if  home_cons_quant=="." 
destring home_cons_quant, replace

replace  qty_total  = "." 		if  qty_total=="10..2" 
destring qty_total , replace

order hhid 
save "${output}\LVL_08.dta", replace


************************************************
**** HCES22_LVL_09: CONSUMABLES AND SERVICES
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 item 42-44 value_total 45-52 mult 53-67 using "${input}\hces22_lvl_09.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

order hhid 
save "${output}\LVL_09.dta", replace


************************************************
**** HCES22_LVL_10: CONSUMABLES AND SERVICES 
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 item 42-44 home_cons_quant 45-54 str home_cons_value 55-62 qty_total 63-72 value_total 73-80 str source 81-81 mult 82-96 using "${input}\hces22_lvl_10.TXT"
	
egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

replace  home_cons_value = "." 	if  home_cons_value=="N" | home_cons_value=="o"
destring home_cons_value, replace

order hhid 
save "${output}\LVL_10.dta", replace


************************************************
**** HCES22_LVL_11: DURABLE GOODS 
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 clothing_online 42-42 footwear_online 43-43 furniture_online 44-44 mobile_online 45-45 personal_goods_online 46-46 recreation_goods_online 47-47 hh_appliances_online 48-48 utensils_online 49-49 sports_good_online 50-50 medical_equipment_online 51-51 bedding_online 52-52 laptop_free 53-53 num_laptop_free 54-56 tablet_free 57-57 num_tablet_free 58-60 mobile_free 61-61 num_mobile_free 62-64 bicycle_free 65-65 num_bicycle_free 66-68 motorcycle_free 69-69 num_motorcycle_free 70-72 clothing_free 73-73 num_clothing_free 74-76 footwear_free 77-77 num_footwear_free 78-80 other_items_free 81-81 num_other_items_free 82-84 television 85-85 radio 86-86 laptop 87-87 mobile 88-88 bicycle 89-89 motorcycle 90-90 car 91-91 trucks 92-92 animal_cart 93-93 refrigerator 94-94 washing_machine 95-95 air_conditioner 96-96 multichanel_tv 97-97 mult 98-112 using "${input}\hces22_lvl_11.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

order hhid
save "${output}\LVL_11.dta", replace


************************************************
**** HCES22_LVL_12: DURABLE GOODS
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 str item 42-44 qty_total 45-54 value_total 55-62 mult 63-77 using "${input}\hces22_lvl_12.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

order hhid 
save "${output}\LVL_12.dta", replace


************************************************
**** HCES22_LVL_13: DURABLE GOODS 
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 item 42-44 first_hand_num 45-47 hired 48-48 first_hand_value 49-56 repair_maint_cost 57-64 second_hand_num 65-67 second_hand_value 68-75 value_total 76-83 mult 84-98 using "${input}\hces22_lvl_13.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

order hhid
save "${output}\LVL_13.dta", replace


************************************************
**** HCES22_LVL_14: FDQ-CSQ-DGQ 
************************************************
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 str section 42-46 item 47-49 value_total 50-59 mult 60-74 using "${input}\hces22_lvl_14.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100
label variable section "section code"
label variable item "item code"	
label variable value_total "value"

order hhid item value_total	
save "${output}\LVL_14.dta", replace


***********************************************************
*** CONSUMPTION COLLAPSED

****** FOOD
* 129: cereal
* 139: cereal substitutes 
* 159: pulse and pulse products
* 179: salt and sugar 
* 169: milk and milk products
* 189: edible oil
* 199: egg, fish and meat 
* 219: vegetables
* 239: fruits (fresh)
* 249: fruits (dry)
* 269: spices 
* 279: beverages
* 289: served processed food
* 299: packaged processed food 


****** SERVICES
* 309: pan
* 319: tobacco
* 329: intoxicants 

* 349: fuel and light
* 459: toilet articles 
* 479: other household consumables 

* 429: medical - non hospitalisation
* 519: conveyance 
* 499: consumer services excluding conveyance
* 439: entertainment
* 529: rent
* 539: house/garage rent 

* 409: education
* 419: medical - hospitalisation 


***** CLOTHING 
* 379: clothing 
* 399: footwear 


***** DURABLES
* 389: bedding, etc.
* 629: other personal goods 
* 609: personal transport equipment
*  99: sport goods
* 619: medical equipment
* 599: cooking and other household appliances
* 579: crockery and utensils
* 559: furniture and fixtures  
* 569: goods for recreation
* 639: residential building, land and other durables 
* 649: jewellery and ornaments


* 899: other consumer taxes, cesses

preserve
replace value_total = value_total*4.3	if item==169 | item==189 | item==199 | item==219 | item==239 | item==249 | item==269 | item==279 | item==289 | item==299 | item==309 | item==319 | item==329
replace value_total = value_total/12	if item==379 | item==399 | item==389 | item==629 | item==609 | item==99 | item==619 | item==599 | item==579 | item==559 | item==569 | item==639 | item==649 | item==409 | item==419 | item==899

gen food_lht = value_total 		    if  item>=129 & item<=299
gen serv_lht = value_total  			if  item==309 | item==319 | item==329 | item==349 | item==459 | item==479 | item==429 | item==519 | item==499 | item==439 | item==529 | item==539 | item==409 | item==419
gen clot_lht = value_total 			if  item==379 | item==399
gen dura_lht = value_total 			if  item==389 | item==629 | item==609 | item==99 | item==619 | item==599 | item==579 | item==559 | item==569 | item==639 | item==649 


rename value_total total_expenditure_lht
collapse (sum) food_lht serv_lht clot_lht dura_lht total_expenditure_lht, by(hhid)
save "${output}\LVL_14_lht.dta", replace
restore 


************************************************
**** HCES22_LVL_15: HCQ-FDQ-CSQ-DGQ
************************************************	
clear 
infix str survey 1-4 year 5-8 str fsu 9-13 sector 14-14 state 15-16 str nss_region 17-19 str district 20-21 str stratum 22-23 str substratum 24-25 panel 26-27 str subsample 28-28 str fod_subsample 29-32 str sample_su 33-34 str sample_subdivision 35-35 sss 36-36 str hhno 37-38 str questionnaire 39-39 str level 40-41 str section 42-43 str time_taken 44-46 total_hhcons 47-54 total_online_exp 55-62 str informant_code 63-64 response_code 65-65 hhsize 66-68 mult 69-83 using "${input}\hces22_lvl_15.TXT"

egen hhid = concat(fsu sss hhno)
gen hhwt = mult/100

label variable section "section code"
label variable time_taken "time taken to canvass the questionnaire (in minutes)"
label variable total_hhcons "household's usual consumption expenditure in a month (in Rs.)"
label variable total_online_exp "total expenditure incurred on online purchase/payment in last 30 days"
label variable informant_code "informant code"
label variable response_code "response code"
label variable hhsize "household size"

encode section, gen(section_new)
destring section_new, replace
label define sect 1 "Household characteristics" 2 "Food expenditure" 3 "Consumables and services expenditure" 4 "Durable goods expenditure" 
label values section_new "sect"
	

label define response 1 "Informant co-operative and capable" 2 "Informant co-operative but not capable" 3 "Informant busy" 4 "Informant reluctant" 9 "Others"
label values response_code "response"	

order hhid section_new total_hhcons total_online_exp hhsize response_code
unique hhid section
save "${output}\LVL_15.dta", replace


**********************************************************************************
**********************************************************************************
* LVL 01:    261,746 observations (household level)
* LVL 02:  1,127,039 observations (individual level)
* LVL 03:    261,746 observations (household level) 
* LVL 04:    261,746 observations (household level) 
* LVL 05: 12,056,839 observations (item level)  			  ---> reshape  
* LVL 06:  1,637,576 observations (item level)  			  ---> reshape 
* LVL 07:    261,746 observations (household level)
* LVL 08:  1,410,652 observations (item level)  			  ---> reshape 
* LVL 09:  7,942,402 observations (item level)  			  ---> reshape 
* LVL 10:    762,523 observations (item level)  			  ---> reshape 
* LVL 11:    261,746 observations (household level)
* LVL 12:  4,410,298 observations (item level)  			  ---> reshape
* LVL 13:  4,500,902 observations (item level)  			  ---> reshape
* LVL 14:  8,075,528 observations (item level) 				  ---> reshape
* LVL 15:  1,046,984 observations (household x section level) ---> reshape

use "${output}\LVL_02.dta", clear
merge m:1 hhid using "${output}\LVL_01.dta"
drop _merge
merge m:1 hhid using "${output}\LVL_03.dta"
drop _merge
merge m:1 hhid using "${output}\LVL_04.dta"
drop _merge
merge m:1 hhid using "${output}\LVL_07.dta"
drop _merge
merge m:1 hhid using "${output}\LVL_11.dta"
drop _merge
merge m:1 hhid using "${output}\LVL_14_lht.dta"
drop _merge
save "${output}\IND_2022-23_HCES_v01_M.dta", replace

forvalues i = 1(1)9 { 
	erase "${output}\LVL_0`i'.dta"
	} 
forvalues i = 10(1)15 { 
	erase "${output}\LVL_`i'.dta"
	} 
erase "${output}\LVL_14_lht.dta"
