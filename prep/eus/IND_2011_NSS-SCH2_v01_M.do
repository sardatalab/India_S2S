global rootdatalib	"D:\Dropbox\s2s"

*<_Program setup_>
clear all
set more off

local code         	"IND"
local year         	"2011"
local survey       	"NSS-SCH2"
local vm           	"01"
local type         	"SARMD"
local yearfolder   	"`code'_`year'_`survey'"
global input       	"${rootdatalib}\\`code'\\`yearfolder'\\`yearfolder'_v`vm'_M\Data\Stata"
global output      	"${rootdatalib}\\`code'\\`yearfolder'\\`yearfolder'_v`vm'_M\Data\Stata"
*</_Program setup_>


*<_Datalibweb request_>

************************************************
**** nsso68ce1typ2: GENERAL INFO 
************************************************
use "${input}\nsso68ce1typ2.dta", clear
rename a18 level
rename a19 filler
rename a20 serial_num
rename a21 response_code
rename a22 survey_code
rename a23 substi_code
rename a24 date_of_survey
rename a25 despatch_date
rename a26 interview_time
sort   hhid
tempfile general_info
save `general_info', replace

************************************************
**** nsso68ce2typ2: HOUSEHOLD CHARACTERISTICS 
************************************************
use "${input}\nsso68ce2typ2.dta", clear
drop a1
sort hhid
tempfile household_characteristics1
save `household_characteristics1', replace

************************************************
**** nsso68ce3typ2: HOUSEHOLD CHARACTERISTICS 
************************************************	
use "${input}\nsso68ce3typ2.dta", clear
drop a1
sort hhid
tempfile household_characteristics2
save `household_characteristics2', replace

************************************************
**** nsso68ce4typ2: INDIVIDUAL CHARACTERISTICS
************************************************
use "${input}\nsso68ce4typ2.dta", clear
drop a1
rename a2 level
rename a5 relation
rename a6 sex
rename a7 age
rename a8 marital_status
rename a9 education
rename a10 days_away
rename a11 meals_daily
rename a12 meals_school
rename a13 meals_employer
rename a14 meals_others
rename a15 meals_paid
rename a16 meals_home
rename a19 nss
rename a20 nsc
rename a21 mult	
destring level a3 a17, replace
sort hhid indid
tempfile individual_characteristics
save `individual_characteristics', replace

************************************************
**** nsso68ce5typ2: FOOD ITEMS 
************************************************
use "${input}\nsso68ce5typ2.dta", clear
sort hhid item
tempfile food_items1
save `food_items1', replace

************************************************
**** nsso68ce6typ2: FOOD ITEMS 
************************************************
use "${input}\nsso68ce6typ2.dta", clear
sort hhid item
tempfile food_items2
save `food_items2', replace

************************************************
**** nsso68ce7typ2: CONSUMABLE AND SERVICES 
************************************************
use "${input}\nsso68ce7typ2.dta", clear
sort hhid itemno
tempfile consumable_items1
save `consumable_items1', replace

************************************************
**** nsso68ce8typ2: CONSUMABLE AND SERVICES 
************************************************
use "${input}\nsso68ce8typ2.dta", clear
sort hhid itemno
tempfile consumable_items2
save `consumable_items2', replace

************************************************
**** nsso68ce9typ2: CONSUMABLE AND SERVICES 
************************************************
use "${input}\nsso68ce9typ2.dta", clear
sort hhid itemno
tempfile consumable_items3
save `consumable_items3', replace

************************************************
**** nsso68ce10typ2: ALTERNATIVE HEALTH?
************************************************
use "${input}\nsso68ce10typ2.dta", clear
sort hhid 
tempfile health
save `health', replace

************************************************
**** nsso68ce11typ2: CONSUMABLE AND SERVICES 
************************************************
use "${input}\nsso68ce11typ2.dta", clear
sort hhid itemno
tempfile consumable_items4
save `consumable_items4', replace


**********************************************************************************
**********************************************************************************
* 01:   101,651 observations (household level)
* 02:   101,651 observations (household level)
* 03:   101,651 observations (household level) 
* 04:   464,730 observations (individual level) 
* 05: 5,277,850 observations (item level)               ---> reshape  
* 06: 1,493,313 observations (item level)  			    ---> reshape 
* 07:   370,071 observations (household level)
* 08: 2,343,672 observations (item level)  			  	---> reshape 
* 09: 3,436,786 observations (item level)  			  	---> reshape 
* 10:   101,647 observations (household level) 
* 11: 3,317,537 observations (item level)             	---> reshape 

use `general_info', clear
merge 1:1 hhid using `household_characteristics1'
drop _merge
merge 1:1 hhid using `household_characteristics2'
drop _merge
merge 1:m hhid using `individual_characteristics'
drop _merge

destring sector, replace
label define sector 1 "Rural" 2 "Urban" 
label value  sector "sector"

****************************************************************************************************************
****************************************************************************************************************
rename indid pid
order hhid pid
sort  hhid pid
*</_Datalibweb request_>


*<_Save data file_>
compress
save "${output}/`yearfolder'_v`vm'_M.dta", replace
*</_Save data file_>	
	
	
	
	