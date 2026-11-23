*</_Program setup_>


*<_Datalibweb request_>

************************************************
**** HCES22_LVL_01: HOUSEHOLD CHARACTERISTICS 
************************************************
use "${input}\level01_2023.dta", clear 
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid
save "${output}\LVL_01.dta", replace


************************************************
**** HCES22_LVL_02: HOUSEHOLD CHARACTERISTICS 
************************************************
use "${input}\level02_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
egen pid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No Person_Serial_No)
gen pwt = Multiplier/100
order hhid pid
save "${output}\LVL_02.dta", replace


************************************************
**** HCES22_LVL_03: HOUSEHOLD CHARACTERISTICS 
************************************************	
use "${input}\level03_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid
save "${output}\LVL_03.dta", replace


************************************************
**** HCES22_LVL_04: FOOD ITEMS 
************************************************
use "${input}\level04_2023.dta", clear 
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid
save "${output}\LVL_04.dta", replace


************************************************
**** HCES22_LVL_05: FOOD ITEMS (LAST 30 DAYS)
************************************************
use "${input}\level05_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid 
save "${output}\LVL_05.dta", replace

collapse (sum) Total_Consumption_Value, by(hhid)
tempfile level_5 
save "`level_5'"

************************************************
**** HCES22_LVL_06: FOOD ITEMS (LAST 7 DAYS) 
************************************************
use "${input}\level06_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid 
save "${output}\LVL_06.dta", replace

destring Total_Consumption_Value, replace

collapse (sum) Total_Consumption_Value, by(hhid)
merge 1:1 hhid using "`level_5'"
keep hhid
tempfile control
save `control'


************************************************
**** HCES22_LVL_07: CONSUMABLE AND SERVICES 
************************************************
use "${input}\level07_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid
save "${output}\LVL_07.dta", replace


************************************************
**** HCES22_LVL_08: CONSUMABLES AND SERVICES 
************************************************
use "${input}\level08_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid 
save "${output}\LVL_08.dta", replace


************************************************
**** HCES22_LVL_09: CONSUMABLES AND SERVICES
************************************************
use "${input}\level09_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid 
save "${output}\LVL_09.dta", replace


************************************************
**** HCES22_LVL_10: CONSUMABLES AND SERVICES 
************************************************
use "${input}\level10_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid 
save "${output}\LVL_10.dta", replace


************************************************
**** HCES22_LVL_11: DURABLE GOODS 
************************************************
use "${input}\level11_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = Multiplier/100
order hhid
save "${output}\LVL_11.dta", replace


************************************************
**** HCES22_LVL_12: DURABLE GOODS
************************************************
use "${input}\level12_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = MULTIPLIER/100
order hhid 
save "${output}\LVL_12.dta", replace


************************************************
**** HCES22_LVL_13: DURABLE GOODS 
************************************************
use "${input}\level13_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = MULTIPLIER/100
order hhid
save "${output}\LVL_13.dta", replace


************************************************
**** HCES22_LVL_14: FDQ-CSQ-DGQ 
************************************************
use "${input}\level14_2023.dta", clear

egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = MULTIPLIER/100
label variable SECTION "section code"
label variable ITEM_CODE "item code"	
label variable VALUE_RS "value"

order hhid ITEM_CODE VALUE_RS	
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
rename ITEM_CODE item
rename VALUE_RS value_total
destring item, replace 

preserve
replace value_total = value_total*4.3	if  item==169 | item==189 | item==199 | item==219 | item==239 | item==249 | item==269 | item==279 | item==289 | item==299 | item==309 | item==319 | item==329
replace value_total = value_total/12	if  item==99 | item==379 | item==389 | item==399 | item==409 | item==419 | item==559 | item==569 | item==579 | item==599 | item==609 | item==619 | item==629 | item==639 | item==649 | item==899

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
use "${input}\level15_2023.dta", clear
egen hhid = concat(FSU_Serial_No Second_Stage_Stratum_No Sample_Household_No)
gen hhwt = MULTIPLIER/100

label variable SECTION "section code"
label variable TIME_TAKEN "time taken to canvass the questionnaire (in minutes)"
label variable MONTHLY_CONSUMPTION_EXP "household's usual consumption expenditure in a month (in Rs.)"
label variable ONLINE_EXPENDITURE "total expenditure incurred on online purchase/payment in last 30 days"
label variable INFORMANT_CODE "informant code"
label variable RESPONSE_CODE "response code"
label variable HOUSEHOLD_SIZE "household size"

encode SECTION, gen(section_new)
destring section_new, replace
label define sect 1 "Household characteristics" 2 "Food expenditure" 3 "Consumables and services expenditure" 4 "Durable goods expenditure" 
label values section_new "sect"
	

label define RESPONSE_CODE 1 "Informant co-operative and capable" 2 "Informant co-operative but not capable" 3 "Informant busy" 4 "Informant reluctant" 9 "Others"
destring RESPONSE_CODE, replace 
label values RESPONSE_CODE "response"	

order hhid section_new MONTHLY_CONSUMPTION_EXP ONLINE_EXPENDITURE HOUSEHOLD_SIZE RESPONSE_CODE

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
merge m:1 hhid using `control'
drop if _merge!=3
drop _merge

merge m:1 hhid using "${output}\IND_PRIMUS_2023-24_v2.dta", keepusing(welfaredef_final)

save "${output}\IND_2023-24_HCES_v02_M.dta", replace

forvalues i = 1(1)9 { 
	erase "${output}\LVL_0`i'.dta"
	} 
forvalues i = 10(1)15 { 
	erase "${output}\LVL_`i'.dta"
	} 
erase "${output}\LVL_14_lht.dta"
