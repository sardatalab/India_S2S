/*-----------------------------------------------------------------------------------
  s2s Harmonization
-------------------------------------------------------------------------------------
<_Program name_>   	IND_2023_HCES_v01_M_v01_A_s2s_HCES_to_PLFS.do	</_Program name_>
<_Application_>    	STATA 17.0									 	  <_Application_>
<_Author(s)_>      	Leo Tornarolli <tornarolli@gmail.com>		  	   </_Author(s)_>
<_Date created>   	10-2026						  	  		  	   </_Date modified_>
-------------------------------------------------------------------------------------
<_Country_>        	IND											         </_Country_>
<_Survey Title_>   	PLFS								  			</_Survey Title_>
<_Survey Year_>    	2023											 </_Survey Year_>
-----------------------------------------------------------------------------------*/

*<_Program setup_>
clear
set more off
local countrycode	"IND"
local year1			"2022"
local year2			"2023"
local survey		"HCES"
local va			"01"
local vm			"02"
local vm1			"03"
local vm2			"02"
local type			"SARMD"
local surveyfolder1	"`countrycode'_`year1'_`survey'"
local surveyfolder2	"`countrycode'_`year2'_`survey'"
global surveydata1	"${rootsarmd}/`countrycode'/`surveyfolder1'/`surveyfolder1'_v`vm1'_M/Data/Stata"
global surveydata2	"${rootsarmd}/`countrycode'/`surveyfolder2'/`surveyfolder2'_v`vm2'_M/Data/Stata"
global output		"${rootsarmd}/`countrycode'/`surveyfolder'/`surveyfolder'_v`vm'_M_v`va'_A_`type'/Data/Harmonized"
global rootlabels   "${rootsarmddo}/_aux"
cap mkdir 			"${rootsarmd}/`countrycode'/`surveyfolder'"
cap mkdir 			"${rootsarmd}/`countrycode'/`surveyfolder'/`surveyfolder'_v`vm'_M_v`va'_A_`type'"
cap mkdir 			"${rootsarmd}/`countrycode'/`surveyfolder'/`surveyfolder'_v`vm'_M_v`va'_A_`type'/Data"
cap mkdir 			"${rootsarmd}/`countrycode'/`surveyfolder'/`surveyfolder'_v`vm'_M_v`va'_A_`type'/Data/Harmonized"
cap mkdir 			"${rootsarmd}/`countrycode'/`surveyfolder'/`surveyfolder'_v`vm'_M_v`va'_A_`type'/Program"
*<_Program setup_>

*<_Datalibweb request_>
use "${surveydata1}/`countrycode'_`year1'_`survey'_v`vm1'_M.dta", clear
keep if panel>=6 & panel<=10
tempfile base_2022_2023

keep hhid pid hhwt panel sector b2q2pt1 state b4q4pt10 b3q3 b3q4 b3q5 b3q6 b3q7 b3q8 b4q4pt11 b4q4pt12 b4q4pt5 b4q4pt6 food_lht serv_lht clot_lht dura_lht welfare*
destring, replace
save `base_2022_2023', replace

use "${surveydata2}/`countrycode'_`year2'_`survey'_v`vm2'_M.dta", clear
destring panel, replace 
keep if panel>=1 & panel<=5
 
keep hhid pid hhwt panel sector hhsize state household_type relation_to_head gender age marital_status religion social_group education_level years_of_education nic_2008_code max_income_activity food_lht serv_lht clot_lht dura_lht welfare*

rename hhsize b2q2pt1
rename household_type b4q4pt10
rename relation_to_head b3q3 
rename age b3q5
rename gender b3q4
rename marital_status b3q6
rename religion b4q4pt11
rename social_group b4q4pt12
rename education_level b3q7 
rename years_of_education b3q8 
rename nic_2008_code b4q4pt5
rename max_income_activity b4q4pt6
destring, replace 
append using `base_2022_2023'
*<_Datalibweb request_>

* Weights adjustment to match PLFS 2023 calendar year population (and state/sector distribution)
replace hhwt = hhwt*1.02825		if  state==1 & sector==1
replace hhwt = hhwt*0.88087		if  state==1 & sector==2
replace hhwt = hhwt*0.89531		if  state==2 & sector==1
replace hhwt = hhwt*1.08520		if  state==2 & sector==2
replace hhwt = hhwt*1.00588		if  state==3 & sector==1
replace hhwt = hhwt*0.91719		if  state==3 & sector==2
replace hhwt = hhwt*0.53061		if  state==4 & sector==1
replace hhwt = hhwt*1.09884		if  state==4 & sector==2
replace hhwt = hhwt*0.83245		if  state==5 & sector==1
replace hhwt = hhwt*0.91291		if  state==5 & sector==2
replace hhwt = hhwt*0.96893		if  state==6 & sector==1
replace hhwt = hhwt*0.94263		if  state==6 & sector==2
replace hhwt = hhwt*1.25598		if  state==7 & sector==1
replace hhwt = hhwt*1.09230		if  state==7 & sector==2
replace hhwt = hhwt*0.94807		if  state==8 & sector==1
replace hhwt = hhwt*1.03376		if  state==8 & sector==2
replace hhwt = hhwt*1.00832		if  state==9 & sector==1
replace hhwt = hhwt*0.95143		if  state==9 & sector==2
replace hhwt = hhwt*1.00789		if  state==10 & sector==1
replace hhwt = hhwt*0.99524		if  state==10 & sector==2
replace hhwt = hhwt*1.07568		if  state==11 & sector==1
replace hhwt = hhwt*0.97064		if  state==11 & sector==2
replace hhwt = hhwt*1.16613		if  state==12 & sector==1
replace hhwt = hhwt*0.89991		if  state==12 & sector==2
replace hhwt = hhwt*1.21195		if  state==13 & sector==1
replace hhwt = hhwt*1.09491		if  state==13 & sector==2
replace hhwt = hhwt*1.00265		if  state==14 & sector==1
replace hhwt = hhwt*1.20519		if  state==14 & sector==2
replace hhwt = hhwt*1.04978		if  state==15 & sector==1
replace hhwt = hhwt*0.95481		if  state==15 & sector==2
replace hhwt = hhwt*1.01675		if  state==16 & sector==1
replace hhwt = hhwt*0.85116		if  state==16 & sector==2
replace hhwt = hhwt*1.08108		if  state==17 & sector==1
replace hhwt = hhwt*1.14802		if  state==17 & sector==2
replace hhwt = hhwt*1.05470		if  state==18 & sector==1
replace hhwt = hhwt*0.90053		if  state==18 & sector==2
replace hhwt = hhwt*1.01419		if  state==19 & sector==1
replace hhwt = hhwt*0.97172		if  state==19 & sector==2
replace hhwt = hhwt*0.97492		if  state==20 & sector==1
replace hhwt = hhwt*0.90479		if  state==20 & sector==2
replace hhwt = hhwt*0.96189		if  state==21 & sector==1
replace hhwt = hhwt*0.88105		if  state==21 & sector==2
replace hhwt = hhwt*0.98199		if  state==22 & sector==1
replace hhwt = hhwt*1.08131		if  state==22 & sector==2
replace hhwt = hhwt*1.03050		if  state==23 & sector==1
replace hhwt = hhwt*0.97255		if  state==23 & sector==2
replace hhwt = hhwt*1.07931		if  state==24 & sector==1
replace hhwt = hhwt*1.07200		if  state==24 & sector==2
replace hhwt = hhwt*0.84333		if  state==25 & sector==1
replace hhwt = hhwt*0.92274		if  state==25 & sector==2
replace hhwt = hhwt*0.96876		if  state==27 & sector==1
replace hhwt = hhwt*1.02741		if  state==27 & sector==2
replace hhwt = hhwt*1.07000		if  state==28 & sector==1
replace hhwt = hhwt*0.94620		if  state==28 & sector==2
replace hhwt = hhwt*0.98917		if  state==29 & sector==1
replace hhwt = hhwt*1.03945		if  state==29 & sector==2
replace hhwt = hhwt*1.18530		if  state==30 & sector==1
replace hhwt = hhwt*1.01693		if  state==30 & sector==2
replace hhwt = hhwt*0.51809		if  state==31 & sector==1
replace hhwt = hhwt*1.11507		if  state==31 & sector==2
replace hhwt = hhwt*1.04486		if  state==32 & sector==1
replace hhwt = hhwt*0.98220		if  state==32 & sector==2
replace hhwt = hhwt*0.96702		if  state==33 & sector==1
replace hhwt = hhwt*0.98388		if  state==33 & sector==2
replace hhwt = hhwt*1.01137		if  state==34 & sector==1
replace hhwt = hhwt*0.91760		if  state==34 & sector==2
replace hhwt = hhwt*0.84551		if  state==35 & sector==1
replace hhwt = hhwt*1.22466		if  state==35 & sector==2
replace hhwt = hhwt*1.01619		if  state==36 & sector==1
replace hhwt = hhwt*0.86770		if  state==36 & sector==2
replace hhwt = hhwt*0.96518		if  state==37 & sector==1
replace hhwt = hhwt*0.75219		if  state==37 & sector==2

gen countrycode = "IND"
gen survname = "HCES"
gen year = 2023 	
gen 	month = 1	if  panel==6
replace month = 2	if  panel==7
replace month = 3   if  panel==8
replace month = 4   if  panel==9
replace month = 5   if  panel==10
replace month = 8   if  panel==1
replace month = 9   if  panel==2
replace month = 10  if  panel==3
replace month = 11  if  panel==4
replace month = 12  if  panel==5

* Urban Dummy
destring sector, replace
gen     urb = 0		if  sector==1
replace urb = 1		if  sector==2

* Household Size and Squared Household Size
gen hh_size = b2q2pt1
gen hh_size_sq = b2q2pt1*b2q2pt1

* State Variable
destring state, replace
gen 	str_state = ""
replace str_state = "1 - Jammu & Kashmir" 						   	if  state==1 
replace str_state = "2 - Himachal Pradesh"                          if  state==2 
replace str_state = "3 - Punjab"                                    if  state==3 
replace str_state = "4 - Chandigarh"                                if  state==4 
replace str_state = "5 - Uttarakhand"                               if  state==5 
replace str_state = "6 - Haryana"                                   if  state==6 
replace str_state = "7 - Delhi"                                     if  state==7 
replace str_state = "8 - Rajasthan"                                 if  state==8 
replace str_state = "9 - Uttar Pradesh"                             if  state==9 
replace str_state = "10 - Bihar"                                    if  state==10 
replace str_state = "11 - Sikkim"                                   if  state==11 
replace str_state = "12 - Arunachal Pradesh"                        if  state==12 
replace str_state = "13 - Nagaland"                                 if  state==13 
replace str_state = "14 - Manipur"                                  if  state==14 
replace str_state = "15 - Mizoram"                                  if  state==15 
replace str_state = "16 - Tripura"                                  if  state==16 
replace str_state = "17 - Meghalaya"                                if  state==17 
replace str_state = "18 - Assam"                                    if  state==18 
replace str_state = "19 - West Bengal"                              if  state==19 
replace str_state = "20 - Jharkhand"                                if  state==20 
replace str_state = "21 - Odisha"                                   if  state==21 
replace str_state = "22 - Chhattisgarh"                             if  state==22 
replace str_state = "23 - Madhya Pradesh"                           if  state==23 
replace str_state = "24 - Gujarat"                                  if  state==24 
replace str_state = "25 - Dadra & Nagar Haveli & Daman & Diu"       if  state==25 
replace str_state = "27 - Maharastra"                               if  state==27 
replace str_state = "28 - Andhra Pradesh"                           if  state==28 
replace str_state = "29 - Karnataka"                                if  state==29 
replace str_state = "30 - Goa"                                      if  state==30 
replace str_state = "31 - Lakshadweep"                              if  state==31 
replace str_state = "32 - Kerala"                                   if  state==32 
replace str_state = "33 - Tamil Nadu"                               if  state==33 
replace str_state = "34 - Puduchery"                                if  state==34 
replace str_state = "35 - Andaman & Nicober"                        if  state==35 
replace str_state = "36 - Telangana"                                if  state==36 
replace str_state = "37 - Ladakh"                                   if  state==37 

* Dummies state
levelsof state, local(variable)
foreach t of local variable {
	gen     state_`t' = 0
	replace state_`t' = 1	if  state==`t'
	}

* Household size
gen hh_memb_t = b2q2pt1

* Household type 
rename b4q4pt10 hhtype
destring hhtype, replace
gen     hh_type = 1			if  hhtype==1 & sector==1					/* self-employed rural agriculture 		*/
replace hh_type = 2			if  hhtype==2 & sector==1					/* self-employed rural non-agriculture	*/ 
replace hh_type = 3			if  hhtype==1 & sector==2					/* self-employed urban 					*/
replace hh_type = 4			if  hhtype>=3 & hhtype<=4 & sector==1  	/* regular wage/salary earnings			*/
replace hh_type = 4			if  hhtype==2 & sector==2  				/* regular wage/salary earnings			*/
replace hh_type = 5			if  hhtype==5 & sector==1					/* casual labor rural agriculture   	*/
replace hh_type = 6			if  hhtype==6 & sector==1					/* casual labor rural non-agriculture	*/
replace hh_type = 7			if  hhtype==3 & sector==2					/* casual labor urban					*/
replace hh_type = 8			if  hhtype==9							/* other								*/

* Dummies household type
levelsof hh_type, local(variable)
foreach t of local variable {
	gen     hhtype_`t' = 0
	replace hhtype_`t' = 1	if  hh_type==`t'
	}

* Household Head Age
rename b3q3 relation_to_head
rename b3q5 age 
destring relation_to_head, replace 
gen aux_head_age = age 		if  relation_to_head==1  
egen hh_head_age = max(aux_head_age), by(hhid)

* Household Mean Age 
egen hh_age_avg = mean(age), by(hhid)

* Number of members aged 0
gen     auxi = 0
replace auxi = 1			if  age==0
egen hh_size_age0 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 1
gen     auxi = 0
replace auxi = 1			if  age==1
egen hh_size_age1 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 2
gen     auxi = 0
replace auxi = 1			if  age==2
egen hh_size_age2 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 3
gen     auxi = 0
replace auxi = 1			if  age==3
egen hh_size_age3 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 4
gen     auxi = 0
replace auxi = 1			if  age==4
egen hh_size_age4 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 0-14
gen     auxi = 0
replace auxi = 1			if  age>=0 & age<=14
egen hh_size_age0_14 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 15-64
gen     auxi = 0
replace auxi = 1			if  age>=15 & age<=64
egen hh_size_age15_64 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 65 and more
gen     auxi = 0
replace auxi = 1			if  age>=65 & age<.
egen hh_size_age65plus = sum(auxi), by(hhid)
drop auxi

* Share of members aged 0
gen hh_sh_age0 = hh_size_age0/hh_size

* Share of members aged 1
gen hh_sh_age1 = hh_size_age1/hh_size

* Share of members aged 2
gen hh_sh_age2 = hh_size_age2/hh_size

* Share of members aged 3
gen hh_sh_age3 = hh_size_age3/hh_size

* Share of members aged 4
gen hh_sh_age4 = hh_size_age4/hh_size

* Share of members aged 0-14
gen hh_sh_age0_14 = hh_size_age0_14/hh_size

* Share of members aged 15-64
gen hh_sh_age15_64 = hh_size_age15_64/hh_size

* Share of members aged 65 and more
gen hh_sh_age65plus = hh_size_age65plus/hh_size

* Share of members aged 0-4
gen hh_sh_age0_4 = (hh_size_age0+hh_size_age1+hh_size_age2+hh_size_age3+hh_size_age4)/hh_size

* Dependency ratio
gen hh_dep_ratio = (hh_size_age0_14+hh_size_age65plus)/hh_size_age15_64

* Male Household Head 
rename b3q4 sex 
gen 	auxi_head_male = 0
replace auxi_head_male = 1	if  relation_to_head==1 & sex==1 
egen hh_head_male = max(auxi_head_male), by(hhid)

* Share of female members
gen     auxi = 0
replace auxi = 1			if  sex==2
egen    hh_sh_female = sum(auxi), by(hhid)
replace hh_sh_female = hh_sh_female/hh_size
drop auxi

* Share of male members
gen     auxi = 0
replace auxi = 1			if  sex==1
egen    hh_sh_male = sum(auxi), by(hhid)
replace hh_sh_male = hh_sh_male/hh_size
drop auxi

* Sex ratio
gen     auxim = 0
replace auxim = 1			if  sex==1
egen    males = sum(auxim), by(hhid)
gen     auxif = 0
replace auxif = 1			if  sex==2
egen    females = sum(auxif), by(hhid)
gen     hh_sex_ratio = males/females
drop auxi* males females

* Household Head Never Married
rename b3q6 marital_status
destring marital_status, replace  
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & marital_status==1
egen hh_head_nm = max(auxi), by(hhid)
drop auxi

* Household Head Married 
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & marital_status==2
egen hh_head_m = max(auxi), by(hhid)
drop auxi

* Household Head Widow/Widower
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & marital_status==3
egen hh_head_w = max(auxi), by(hhid)
drop auxi

* Household Head Divorced/Separated
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & marital_status==4
egen hh_head_d = max(auxi), by(hhid)
drop auxi

* Household Head Religion: hindu
rename b4q4pt11 religion
destring religion, replace 
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & religion==1
egen hh_head_hin = max(auxi), by(hhid)
drop auxi

* Household Head Religion: non-hindu
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & religion!=1
egen hh_head_nhin = max(auxi), by(hhid)
drop auxi

* Household Head Religion: muslim
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & religion==2
egen hh_head_msl = max(auxi), by(hhid)
drop auxi

* Household Head Religion: buddhist 
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & religion==6
egen hh_head_bud = max(auxi), by(hhid)
drop auxi

* Household Head Religion: christian
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & religion==3
egen hh_head_chr = max(auxi), by(hhid)
drop auxi

* Household Head Religion: sikh
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & religion==4
egen hh_head_sik = max(auxi), by(hhid)
drop auxi

* Household Head Religion: other 
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & (religion==5 | religion==7 | religion==9)
egen hh_head_oth = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: scheduled tribe
rename b4q4pt12 caste
destring caste, replace
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & caste==1
egen hh_head_tribe = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: scheduled caste
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & caste==2
egen hh_head_caste = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: other backward class
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & caste==3
egen hh_head_back = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: other
gen 	auxi = 0
replace auxi = 1			if  relation_to_head==1 & caste==9
egen hh_head_other = max(auxi), by(hhid)
drop auxi

*******************************************************************************************
rename b3q8 years_edu
rename b3q7 highest_edu
destring highest_edu, replace 
replace years_edu = 4 	if   highest_edu==3 &  years_edu>=5 & years_edu<=9
replace years_edu = 7 	if   highest_edu==4 &  years_edu>=8 & years_edu<=12
replace years_edu = 10 	if   highest_edu==5 & years_edu>=11 & years_edu<=14
replace years_edu = 12	if   highest_edu==6 & years_edu>=13 & years_edu<=18
replace years_edu = 14	if   highest_edu==7 & years_edu>=15 & years_edu<=19
replace years_edu = 14	if   highest_edu>=8 & highest_edu<=11 & years_edu>=15 & years_edu<=20  /* chequear */
replace years_edu = 17	if  highest_edu==12 & years_edu>=18 & years_edu<=20
replace years_edu = 21	if  highest_edu==13 & years_edu>=22 & years_edu<=26
replace years_edu = 0	if   highest_edu<=2 & years_edu==.
*******************************************************************************************

* Household Head Literacy: literate
gen     auxi = 0
replace auxi = 1				if  relation_to_head==1 & highest_edu!=1
egen hh_head_lit = max(auxi), by(hhid)
drop auxi 			

* Household Head Literacy: iliterate
gen     auxi = 0
replace auxi = 1				if  relation_to_head==1 & highest_edu==1
egen hh_head_ilit = max(auxi), by(hhid)
drop auxi 				

* Household share of members 5+ literate
gen     auxilit = 0
replace auxilit = 1			if  highest_edu!=1 & age>=5 & age<.
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1			if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_lit = literate/members
drop auxi* literate members			

* Household share of members 5+ iliterate
gen     auxiilit = 0
replace auxiilit = 1			if  highest_edu==1 & age>=5 & age<.
egen    iliterate = sum(auxiilit), by(hhid)
gen     auximem = 0
replace auximem = 1			if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_ilit = iliterate/members
drop auxi* iliterate members	

* Household share of members 15-64 literate
gen     auxilit = 0
replace auxilit = 1			if  highest_edu!=1 & age>=15 & age<=64
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1			if  age>=15 & age<=64
egen    members = sum(auximem), by(hhid)
gen hh_sh1564_lit = literate/members
drop auxi* literate members	

* Household Head highest years of education
gen auxi = years_edu 			if  relation_to_head==1 
egen hh_head_educ = max(auxi), by(hhid)
drop auxi		

* Household max. years of education
egen hh_mx_educ = max(years_edu), by(hhid)

* Household avg. years of education by members
egen hh_avg_educ = mean(years_edu), by(hhid)			

* Household Spouse highest years of education
gen auxi = years_edu 			if  relation_to_head==2 
egen hh_sp_educ = max(auxi), by(hhid)
drop auxi	

* Household Head with no education
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==1 & highest_edu>=1 & highest_edu<=2
egen hh_head_educ0 = max(auxi), by(hhid)
drop auxi		

* Household Head with primary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==1 & (highest_edu==3 | (highest_edu==4 & years_edu>=4 & years_edu<=5))
egen hh_head_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Head with primary education complete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==1 & highest_edu==4 & years_edu>=6 & years_edu<=7 
egen hh_head_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==1 & (highest_edu==5 | (highest_edu==6 & years_edu>=9 & years_edu<=10) | (highest_edu==7 & years_edu>=10 & years_edu<=11))
egen hh_head_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education complete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==1 & ((highest_edu==6 & years_edu>=11 & years_edu<=12) | (highest_edu==7 & years_edu>=12) | (highest_edu>=8 & highest_edu<=11))
egen hh_head_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Head with tertiary education
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==1 & highest_edu>=12 & highest_edu<=13
egen hh_head_educ_ter = max(auxi), by(hhid)
drop auxi

* Household Spouse with no education
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==2 & highest_edu>=1 & highest_edu<=2
egen hh_sp_educ0 = max(auxi), by(hhid)
drop auxi		
* Household Spouse with primary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==2 & (highest_edu==3 | (highest_edu==4 & years_edu>=4 & years_edu<=5))
egen hh_sp_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Spouse with primary education complete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==2 & highest_edu==4 & years_edu>=6 & years_edu<=7 
egen hh_sp_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==2 & (highest_edu==5 | (highest_edu==6 & years_edu>=9 & years_edu<=10) | (highest_edu==7 & years_edu>=10 & years_edu<=11))
egen hh_sp_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education complete
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==2 & ((highest_edu==6 & years_edu>=11 & years_edu<=12) | (highest_edu==7 & years_edu>=12) | (highest_edu>=8 & highest_edu<=11))
egen hh_sp_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Spouse with tertiary education
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==2 & highest_edu>=12 & highest_edu<=13
egen hh_sp_educ_ter = max(auxi), by(hhid)
drop auxi	

* Households without Spouse	
gen     auxi = 0
replace auxi = 1 			if  relation_to_head==2 
egen auxi_spouse = max(auxi), by(hhid)
gen   	hh_without_spouse = 0
replace hh_without_spouse = 1	if  auxi_spouse==0
drop auxi*	


***  MAXINCOME_ACTIVIY 
***  NIC_CODE 
rename b4q4pt5 nic_code 
destring nic_code, replace
rename b4q4pt6 maxincome_activity
destring maxincome_activity, replace

* Max income labor type: working for wage
gen 	max_inc_wrk_sal = 0     	if  maxincome_activity>=1 & maxincome_activity<=3
replace max_inc_wrk_sal = 1		if  maxincome_activity>=2 & maxincome_activity<=3
replace max_inc_wrk_sal = 0		if  max_inc_wrk_sal==.

* Max income labor type: working for wage
gen 	max_inc_wrk_self = 0    	if  maxincome_activity>=1 & maxincome_activity<=3
replace max_inc_wrk_self = 1		if  maxincome_activity==1
replace max_inc_wrk_self = 0		if  max_inc_wrk_self==.


* Max income labor field: working for wage/profit in agriculture
gen 	max_inc_wrk_agr = 0		if  nic_code>=1111 & nic_code<=99000
replace max_inc_wrk_agr = 1		if  nic_code>=1111 & nic_code<=9900
replace max_inc_wrk_agr = 0		if  max_inc_wrk_agr==.

* Max income labor field: working for wage/profit in industry
gen 	max_inc_wrk_ind = 0		if  nic_code>=1111  & nic_code<=99000
replace max_inc_wrk_ind = 1		if  nic_code>=10101 & nic_code<=43900
replace max_inc_wrk_ind = 0		if  max_inc_wrk_ind==.

* Max income labor field: working for wage/profit in service
gen 	max_inc_wrk_srv = 0		if  nic_code>=1111  & nic_code<=99000
replace max_inc_wrk_srv = 1		if  nic_code>=45101 & nic_code<=99000
replace max_inc_wrk_srv = 0		if  max_inc_wrk_srv==.


* No labor income in the household
gen     max_null = 0
replace max_null = 1				if  max_inc_wrk_agr==0 & max_inc_wrk_ind==0 & max_inc_wrk_srv==0

* Welfare variable 
egen total_total = rsum(food_lht serv_lht clot_lht dura_lht)
gen consumption_pc = total_total/hh_size
cuantiles consumption_pc [w=hhwt], n(10) orden_aux(hhid pid) g(decile_national)

*** SHARES CONSUMPTION 
egen   good_srvs = rsum(food_lht serv_lht)
gen exp_clothing = clot_lht
gen exp_durables = dura_lht
egen   exp_total = rsum(good_srvs exp_clothing exp_durables)

gen shr_goodsvrs = good_srvs/exp_total
gen shr_clothing = exp_clothing/exp_total
gen shr_durables = exp_durables/exp_total

keep if relation_to_head==1 

keep  hhid year month hhwt hh_size hh_size_sq urb state_1-state_37 hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0 hh_size_age1 hh_size_age2 hh_size_age3 hh_size_age4 hh_size_age0_14 hh_size_age15_64 hh_size_age65plus hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 hh_sh_age4 hh_sh_age0_14 hh_sh_age15_64 hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio hh_head_male hh_sh_female hh_sh_male hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter hh_without_spouse max_inc_wrk_sal max_inc_wrk_self max_inc_wrk_agr max_inc_wrk_ind max_inc_wrk_srv max_null decile_national shr* consumption_pc* welfaredef_fin
order hhid year month hhwt hh_size hh_size_sq urb state_1-state_37 hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0 hh_size_age1 hh_size_age2 hh_size_age3 hh_size_age4 hh_size_age0_14 hh_size_age15_64 hh_size_age65plus hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 hh_sh_age4 hh_sh_age0_14 hh_sh_age15_64 hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio hh_head_male hh_sh_female hh_sh_male hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter hh_without_spouse max_inc_wrk_sal max_inc_wrk_self max_inc_wrk_agr max_inc_wrk_ind max_inc_wrk_srv max_null decile_national shr* consumption_pc* welfaredef_fin
  
save "${roots2s}/`countrycode'/data/`surveyfolder2'_v01_M_v01_A_s2s_HCES_to_PLFS.dta", replace	

sum [w=hhwt] 
	