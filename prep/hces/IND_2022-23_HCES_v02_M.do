*</_Program setup_>


*<_Datalibweb request_>

************************************************
**** HCES22_LVL_01: HOUSEHOLD CHARACTERISTICS 
************************************************
use "${input}\level01_2022.dta", clear 
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid
save "${output}\LVL_01.dta", replace


************************************************
**** HCES22_LVL_02: HOUSEHOLD CHARACTERISTICS 
************************************************
use "${input}\level02_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
egen pid = concat(fsu b1q1pt11 b1q1pt12 b3q1)
gen pwt = mult/100
order hhid pid
save "${output}\LVL_02.dta", replace


************************************************
**** HCES22_LVL_03: HOUSEHOLD CHARACTERISTICS 
************************************************	
use "${input}\level03_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid
save "${output}\LVL_03.dta", replace


************************************************
**** HCES22_LVL_04: FOOD ITEMS 
************************************************
use "${input}\level04_2022.dta", clear 
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid
save "${output}\LVL_04.dta", replace


************************************************
**** HCES22_LVL_05: FOOD ITEMS (LAST 30 DAYS)
************************************************
use "${input}\level05_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid 
save "${output}\LVL_05.dta", replace

collapse (sum) b5pt1q6, by(hhid)
tempfile level_5 
save "`level_5'"

************************************************
**** HCES22_LVL_06: FOOD ITEMS (LAST 7 DAYS) 
************************************************
use "${input}\level06_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid 
save "${output}\LVL_06.dta", replace

collapse (sum) b7pt1q4, by(hhid)
merge 1:1 hhid using "`level_5'"
keep hhid
tempfile control
save `control'


************************************************
**** HCES22_LVL_07: CONSUMABLE AND SERVICES 
************************************************
use "${input}\level07_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid
save "${output}\LVL_07.dta", replace


************************************************
**** HCES22_LVL_08: CONSUMABLES AND SERVICES 
************************************************
use "${input}\level08_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid 
save "${output}\LVL_08.dta", replace


************************************************
**** HCES22_LVL_09: CONSUMABLES AND SERVICES
************************************************
use "${input}\level09_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid 
save "${output}\LVL_09.dta", replace


************************************************
**** HCES22_LVL_10: CONSUMABLES AND SERVICES 
************************************************
use "${input}\level10_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid 
save "${output}\LVL_10.dta", replace


************************************************
**** HCES22_LVL_11: DURABLE GOODS 
************************************************
use "${input}\level11_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid
save "${output}\LVL_11.dta", replace


************************************************
**** HCES22_LVL_12: DURABLE GOODS
************************************************
use "${input}\level12_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid 
save "${output}\LVL_12.dta", replace


************************************************
**** HCES22_LVL_13: DURABLE GOODS 
************************************************
use "${input}\level13_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
order hhid
save "${output}\LVL_13.dta", replace


************************************************
**** HCES22_LVL_14: FDQ-CSQ-DGQ 
************************************************
use "${input}\level14_2022.dta", clear

egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100
label variable ba1b1c1_1 "section code"
label variable ba1b1c1_2 "item code"	
label variable ba1b1c1_3 "value"

order hhid ba1b1c1_2 ba1b1c1_3	
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
rename ba1b1c1_2 item
rename ba1b1c1_3 value_total
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
use "${input}\level15_2022.dta", clear
egen hhid = concat(fsu b1q1pt11 b1q1pt12)
gen hhwt = mult/100

label variable section "section code"
label variable b1pt1a2b2c2 "time taken to canvass the questionnaire (in minutes)"
label variable ba2b2c2q5 "household's usual consumption expenditure in a month (in Rs.)"
label variable ba2b2c2q6 "total expenditure incurred on online purchase/payment in last 30 days"
label variable ba2b2c2q7 "informant code"
label variable ba2b2c2q8 "response code"
label variable ba2b2c2q9 "household size"

encode section, gen(section_new)
destring section_new, replace
label define sect 1 "Household characteristics" 2 "Food expenditure" 3 "Consumables and services expenditure" 4 "Durable goods expenditure" 
label values section_new "sect"
	

label define response 1 "Informant co-operative and capable" 2 "Informant co-operative but not capable" 3 "Informant busy" 4 "Informant reluctant" 9 "Others"
destring ba2b2c2q8, replace 
label values ba2b2c2q8 "response"	

order hhid section_new ba2b2c2q5 ba2b2c2q6 ba2b2c2q9 ba2b2c2q8

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

merge m:1 hhid using "${output}\IND_PRIMUS_2022-23.dta", keepusing(welfaredef_final)

save "${output}\IND_2022-23_HCES_v02_M.dta", replace

forvalues i = 1(1)9 { 
	erase "${output}\LVL_0`i'.dta"
	} 
forvalues i = 10(1)15 { 
	erase "${output}\LVL_`i'.dta"
	} 
erase "${output}\LVL_14_lht.dta"
