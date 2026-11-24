
*<_Datalibweb request_>
use "${input}\IND_2023-24_HCES_v02_M.dta", clear 
*<_Datalibweb request_>

gen countrycode = "IND"
gen survname = "HCES"
rename Year year_hces
gen year = 2023 	

* Urban Dummy
destring Sector, replace
gen     urb = 0		if  Sector==1
replace urb = 1		if  Sector==2

* Household Size and Squared Household Size
gen hh_size = HH_Size_FDQ
gen hh_size_sq = HH_Size_FDQ*HH_Size_FDQ

* State Variable
destring State, replace
gen 	str_State = ""
replace str_State = "1 - Jammu & Kashmir" 						   if  State==1 
replace str_State = "2 - Himachal Pradesh"                          if  State==2 
replace str_State = "3 - Punjab"                                    if  State==3 
replace str_State = "4 - Chandigarh"                                if  State==4 
replace str_State = "5 - Uttarakhand"                               if  State==5 
replace str_State = "6 - Haryana"                                   if  State==6 
replace str_State = "7 - Delhi"                                     if  State==7 
replace str_State = "8 - Rajasthan"                                 if  State==8 
replace str_State = "9 - Uttar Pradesh"                             if  State==9 
replace str_State = "10 - Bihar"                                    if  State==10 
replace str_State = "11 - Sikkim"                                   if  State==11 
replace str_State = "12 - Arunachal Pradesh"                        if  State==12 
replace str_State = "13 - Nagaland"                                 if  State==13 
replace str_State = "14 - Manipur"                                  if  State==14 
replace str_State = "15 - Mizoram"                                  if  State==15 
replace str_State = "16 - Tripura"                                  if  State==16 
replace str_State = "17 - Meghalaya"                                if  State==17 
replace str_State = "18 - Assam"                                    if  State==18 
replace str_State = "19 - West Bengal"                              if  State==19 
replace str_State = "20 - Jharkhand"                                if  State==20 
replace str_State = "21 - Odisha"                                   if  State==21 
replace str_State = "22 - Chhattisgarh"                             if  State==22 
replace str_State = "23 - Madhya Pradesh"                           if  State==23 
replace str_State = "24 - Gujarat"                                  if  State==24 
replace str_State = "25 - Dadra & Nagar Haveli & Daman & Diu"       if  State==25 
replace str_State = "27 - Maharastra"                               if  State==27 
replace str_State = "28 - Andhra Pradesh"                           if  State==28 
replace str_State = "29 - Karnataka"                                if  State==29 
replace str_State = "30 - Goa"                                      if  State==30 
replace str_State = "31 - Lakshadweep"                              if  State==31 
replace str_State = "32 - Kerala"                                   if  State==32 
replace str_State = "33 - Tamil Nadu"                               if  State==33 
replace str_State = "34 - Puduchery"                                if  State==34 
replace str_State = "35 - Andaman & Nicober"                        if  State==35 
replace str_State = "36 - Telangana"                                if  State==36 
replace str_State = "37 - Ladakh"                                   if  State==37 

* Dummies State
levelsof State, local(variable)
foreach t of local variable {
	gen     State_`t' = 0
	replace State_`t' = 1	if  State==`t'
	}

* Household size
gen hh_memb_t = HH_Size_FDQ

* Household type 
rename Household_Type hhtype
destring hhtype, replace
gen     hh_type = 1			if  hhtype==1 & Sector==1					/* self-employed rural agriculture 		*/
replace hh_type = 2			if  hhtype==2 & Sector==1					/* self-employed rural non-agriculture	*/ 
replace hh_type = 3			if  hhtype==1 & Sector==2					/* self-employed urban 					*/
replace hh_type = 4			if  hhtype>=3 & hhtype<=4 & Sector==1  	/* regular wage/salary earnings			*/
replace hh_type = 4			if  hhtype==2 & Sector==2  				/* regular wage/salary earnings			*/
replace hh_type = 5			if  hhtype==5 & Sector==1					/* casual labor rural agriculture   	*/
replace hh_type = 6			if  hhtype==6 & Sector==1					/* casual labor rural non-agriculture	*/
replace hh_type = 7			if  hhtype==3 & Sector==2					/* casual labor urban					*/
replace hh_type = 8			if  hhtype==9							/* other								*/

* Dummies household type
levelsof hh_type, local(variable)
foreach t of local variable {
	gen     hhtype_`t' = 0
	replace hhtype_`t' = 1	if  hh_type==`t'
	}

* Household Head Age
rename Relation_to_Head relation_to_head
rename Age age 
destring relation_to_head, replace 
destring age, replace 
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
rename Gender sex 
destring sex, replace
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
rename Marital_Status marital_status
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
rename Religion_of_HH_Head religion
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
rename Social_Group_of_HH_Head caste
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
rename Years_of_Education years_edu
destring years_edu, replace 
rename Education_Level highest_edu
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
rename NIC_2008_Code nic_code 
destring nic_code, replace
rename Max_Income_Activity maxincome_activity
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
xtile decile_national= consumption_pc [w=hhwt], nq(10)   

*** SHARES CONSUMPTION 
egen   good_srvs = rsum(food_lht serv_lht)
gen exp_clothing = clot_lht
gen exp_durables = dura_lht
egen   exp_total = rsum(good_srvs exp_clothing exp_durables)

gen shr_goodsvrs = good_srvs/exp_total
gen shr_clothing = exp_clothing/exp_total
gen shr_durables = exp_durables/exp_total

keep if relation_to_head==1 

keep  hhid year hhwt hh_size hh_size_sq urb State_1-State_37 hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0 hh_size_age1 hh_size_age2 hh_size_age3 hh_size_age4 hh_size_age0_14 hh_size_age15_64 hh_size_age65plus hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 hh_sh_age4 hh_sh_age0_14 hh_sh_age15_64 hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio hh_head_male hh_sh_female hh_sh_male hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter hh_without_spouse max_inc_wrk_sal max_inc_wrk_self max_inc_wrk_agr max_inc_wrk_ind max_inc_wrk_srv max_null decile_national shr* consumption_pc* welfaredef_fin
order hhid year hhwt hh_size hh_size_sq urb State_1-State_37 hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0 hh_size_age1 hh_size_age2 hh_size_age3 hh_size_age4 hh_size_age0_14 hh_size_age15_64 hh_size_age65plus hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 hh_sh_age4 hh_sh_age0_14 hh_sh_age15_64 hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio hh_head_male hh_sh_female hh_sh_male hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter hh_without_spouse max_inc_wrk_sal max_inc_wrk_self max_inc_wrk_agr max_inc_wrk_ind max_inc_wrk_srv max_null decile_national shr* consumption_pc* welfaredef_fin
  
*save "${output}\IND_2022-23_HCES_v02_M_v02_A_s2s_HCES_to_PLFS.dta", replace	
gen pop_wgt=hhwt*hh_size
drop hhwt
ren welfaredef_final mpce_sp_def_ind
ren hh_size_sq hh_sizesq
*check dep_ratio 7650 missings because sh_1664 is zero
drop hh_size_*  hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 ///
hh_sh_age4 hh_sh_male hh_sex_ratio hh_head_ilit hh_sh_ilit ///
hh_sh1564_lit  hh_sp_educ consumption_pc ///
hh_dep_ratio 

ren hh_sizesq hh_size_sq

rename State_* state_*      // the R code expects state_1 ... state_37 (as in HCES 2022-23)
save "${output}\HCES23_s2s.dta", replace

use "${s2s_inputs}\IND_PRIMUS_2023-24_v2.dta", clear
gen spdef = welfarenom_final / welfaredef_final	
collapse (mean) spdef [aw=pwt], by(state sector)
replace sector = sector -1
rename sector urb
save "${output}\Sp_deflators_HCES_23.dta", replace
	