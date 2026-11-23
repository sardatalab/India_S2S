	
*<_Datalibweb request_>
use "C:\Users\wb587758\OneDrive - WBG\IND\S2S\2023-24\Data\Stage 2\Raw\IND_2023_PLFS\IND_2023_PLFS_v01_M\Data\Stata\IND_2023_PLFS-V1_v01_M.dta", clear 
*<_Datalibweb request_>

gen countrycode = "IND"
gen survname = "PLFS"
rename year year_plfs
gen year = 2023 	if  year_plfs=="2023-24"

* Urban Dummy
gen     urb = 0		if  sector==1
replace urb = 1		if  sector==2

* Household Size and Squared Household Size
gen hh_size = hhsize
gen hh_size_sq = hhsize*hhsize

* State Variable
gen 	str_state = ""
replace str_state = "1 - Jammu & Kashmir" 						   if  state==1 
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
gen hh_memb_t = hhsize

* Household type 
gen     hh_type = 1			if  hhtype==1 & sector==1		/* self-employed rural agriculture 		*/
replace hh_type = 2			if  hhtype==2 & sector==1		/* self-employed rural non-agriculture	*/ 
replace hh_type = 3			if  hhtype==1 & sector==2		/* self-employed urban 					*/
replace hh_type = 4			if  hhtype==3 & sector==1  	/* regular wage/salary earnings			*/
replace hh_type = 4			if  hhtype==2 & sector==2  	/* regular wage/salary earnings			*/
replace hh_type = 5			if  hhtype==4 & sector==1		/* casual labor rural agriculture   	*/
replace hh_type = 6			if  hhtype==5 & sector==1		/* casual labor rural non-agriculture	*/
replace hh_type = 7			if  hhtype==3 & sector==2		/* casual labor urban					*/
replace hh_type = 8			if  hhtype==9				/* other								*/

* Dummies household type
levelsof hh_type, local(variable)
foreach t of local variable {
	gen     hhtype_`t' = 0
	replace hhtype_`t' = 1	if  hh_type==`t'
	}

* Household Head Age
gen aux_head_age = age 		if  relationtohead==1  
egen hh_head_age = max(aux_head_age), by(hhid)

* Household Mean Age 
egen hh_age_avg = mean(age), by(hhid)

* Number of members aged 0
gen     auxi = 0
replace auxi = 1				if  age==0
egen hh_size_age0 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 1
gen     auxi = 0
replace auxi = 1				if  age==1
egen hh_size_age1 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 2
gen     auxi = 0
replace auxi = 1				if  age==2
egen hh_size_age2 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 3
gen     auxi = 0
replace auxi = 1				if  age==3
egen hh_size_age3 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 4
gen     auxi = 0
replace auxi = 1				if  age==4
egen hh_size_age4 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 0-14
gen     auxi = 0
replace auxi = 1				if  age>=0 & age<=14
egen hh_size_age0_14 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 15-64
gen     auxi = 0
replace auxi = 1				if  age>=15 & age<=64
egen hh_size_age15_64 = sum(auxi), by(hhid)
drop auxi

* Number of members aged 65 and more
gen     auxi = 0
replace auxi = 1				if  age>=65 & age<.
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
gen 	auxi_head_male = 0
replace auxi_head_male = 1	if  relationtohead==1 & sex==1 
egen hh_head_male = max(auxi_head_male), by(hhid)

* Share of female members
gen     auxi = 0
replace auxi = 1				if  sex==2
egen    hh_sh_female = sum(auxi), by(hhid)
replace hh_sh_female = hh_sh_female/hh_size
drop auxi

* Share of male members
gen     auxi = 0
replace auxi = 1				if  sex==1
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
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & marital==1
egen hh_head_nm = max(auxi), by(hhid)
drop auxi

* Household Head Married 
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & marital==2
egen hh_head_m = max(auxi), by(hhid)
drop auxi

* Household Head Widow/Widower
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & marital==3
egen hh_head_w = max(auxi), by(hhid)
drop auxi

* Household Head Divorced/Separated
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & marital==4
egen hh_head_d = max(auxi), by(hhid)
drop auxi

* Household Head Religion: hindu
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & religion==1
egen hh_head_hin = max(auxi), by(hhid)
drop auxi

* Household Head Religion: non-hindu
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & religion!=1
egen hh_head_nhin = max(auxi), by(hhid)
drop auxi

* Household Head Religion: muslim
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & religion==2
egen hh_head_msl = max(auxi), by(hhid)
drop auxi

* Household Head Religion: buddhist 
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & religion==6
egen hh_head_bud = max(auxi), by(hhid)
drop auxi

* Household Head Religion: christian
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & religion==3
egen hh_head_chr = max(auxi), by(hhid)
drop auxi

* Household Head Religion: sikh
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & religion==4
egen hh_head_sik = max(auxi), by(hhid)
drop auxi

* Household Head Religion: other 
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & (religion==5 | religion==7 | religion==9)
egen hh_head_oth = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: scheduled tribe
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & socialgroup==1
egen hh_head_tribe = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: scheduled caste
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & socialgroup==2
egen hh_head_caste = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: other backward class
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & socialgroup==3
egen hh_head_back = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: other
gen 	auxi = 0
replace auxi = 1				if  relationtohead==1 & socialgroup==9
egen hh_head_other = max(auxi), by(hhid)
drop auxi

*******************************************************************************************
replace noyrsformaled = 4 	if   geneducation==5 &  noyrsformaled>=5 & noyrsformaled<=9
replace noyrsformaled = 7 	if   geneducation==6 &  noyrsformaled>=8 & noyrsformaled<=12
replace noyrsformaled = 10 	if   geneducation==7 & noyrsformaled>=11 & noyrsformaled<=13
replace noyrsformaled = 12	if   geneducation==8 & noyrsformaled>=13 & noyrsformaled<=15
replace noyrsformaled = 10	if   geneducation==8 & noyrsformaled==9
replace noyrsformaled = 14	if  geneducation==10 & noyrsformaled>=15 & noyrsformaled<=17
replace noyrsformaled = 12	if  geneducation==10 & noyrsformaled>=10 & noyrsformaled<=11
replace noyrsformaled = 14	if  geneducation==11 & noyrsformaled>=15 & noyrsformaled<=18
replace noyrsformaled = 17	if  geneducation==12 & noyrsformaled>=18 & noyrsformaled<=20
replace noyrsformaled = 15	if  geneducation==12 & noyrsformaled>=11 & noyrsformaled<=14
replace noyrsformaled = 21	if  geneducation==13 & noyrsformaled>=22 & noyrsformaled<=26
replace noyrsformaled = 17	if  geneducation==13 & noyrsformaled==16
*******************************************************************************************

* Household Head Literacy: literate
gen     auxi = 0
replace auxi = 1				if  relationtohead==1 & geneducation!=1
egen hh_head_lit = max(auxi), by(hhid)
drop auxi 			

* Household Head Literacy: iliterate
gen     auxi = 0
replace auxi = 1				if  relationtohead==1 & geneducation==1
egen hh_head_ilit = max(auxi), by(hhid)
drop auxi 				

* Household share of members 5+ literate
gen     auxilit = 0
replace auxilit = 1			if  geneducation!=1 & age>=5 & age<.
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1			if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_lit = literate/members
drop auxi* literate members			

* Household share of members 5+ iliterate
gen     auxiilit = 0
replace auxiilit = 1			if  geneducation==1 & age>=5 & age<.
egen    iliterate = sum(auxiilit), by(hhid)
gen     auximem = 0
replace auximem = 1			if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_ilit = iliterate/members
drop auxi* iliterate members	

* Household share of members 15-64 literate
gen     auxilit = 0
replace auxilit = 1			if  geneducation!=1 & age>=15 & age<=64
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1			if  age>=15 & age<=64
egen    members = sum(auximem), by(hhid)
gen hh_sh1564_lit = literate/members
drop auxi* literate members	

* Household Head highest years of education
gen auxi = noyrsformaled 		if  relationtohead==1 
egen hh_head_educ = max(auxi), by(hhid)
drop auxi		

* Household max. years of education
egen hh_mx_educ = max(noyrsformaled), by(hhid)

* Household avg. years of education by members
egen hh_avg_educ = mean(noyrsformaled), by(hhid)			

* Household Spouse highest years of education
gen auxi = noyrsformaled 		if  relationtohead==2 
egen hh_sp_educ = max(auxi), by(hhid)
drop auxi	

* Household Head with no education
gen     auxi = 0
replace auxi = 1 			if  relationtohead==1 & geneducation>=1 & geneducation<=4
egen hh_head_educ0 = max(auxi), by(hhid)
drop auxi		

* Household Head with primary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==1 & (geneducation==5 | (geneducation==6 & noyrsformaled>=4 & noyrsformaled<=5))
egen hh_head_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Head with primary education complete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==1 & geneducation==6 & noyrsformaled>=6 & noyrsformaled<=7 
egen hh_head_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==1 & (geneducation==7 | (geneducation==8 & noyrsformaled>=9 & noyrsformaled<=10) | (geneducation==10 & noyrsformaled>=10 & noyrsformaled<=11))
egen hh_head_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education complete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==1 & ((geneducation==8 & noyrsformaled>=11 & noyrsformaled<=12) | (geneducation==10 & noyrsformaled>=12) | geneducation==11)
egen hh_head_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Head with tertiary education
gen     auxi = 0
replace auxi = 1 			if  relationtohead==1 & geneducation>=12 & geneducation<=13
egen hh_head_educ_ter = max(auxi), by(hhid)
drop auxi

* Household Spouse with no education
gen     auxi = 0
replace auxi = 1 			if  relationtohead==2 & geneducation>=1 & geneducation<=4
egen hh_sp_educ0 = max(auxi), by(hhid)
drop auxi		
* Household Spouse with primary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==2 & (geneducation==5 | (geneducation==6 & noyrsformaled>=4 & noyrsformaled<=5))
egen hh_sp_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Spouse with primary education complete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==2 & geneducation==6 & noyrsformaled>=6 & noyrsformaled<=7 
egen hh_sp_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education incomplete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==2 & (geneducation==7 | (geneducation==8 & noyrsformaled>=9 & noyrsformaled<=10) | (geneducation==10 & noyrsformaled>=10 & noyrsformaled<=11))
egen hh_sp_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education complete
gen     auxi = 0
replace auxi = 1 			if  relationtohead==2 & ((geneducation==8 & noyrsformaled>=11 & noyrsformaled<=12) | (geneducation==10 & noyrsformaled>=12) | geneducation==11)
egen hh_sp_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Spouse with tertiary education
gen     auxi = 0
replace auxi = 1 			if  relationtohead==2 & geneducation>=12 & geneducation<=13
egen hh_sp_educ_ter = max(auxi), by(hhid)
drop auxi	

* Households without Spouse	
gen     auxi = 0
replace auxi = 1 			if  relationtohead==2 
egen auxi_spouse = max(auxi), by(hhid)
gen   	hh_without_spouse = 0
replace hh_without_spouse = 1	if  auxi_spouse==0
drop auxi*	

/* STATUS_CWS:
 Worked in Household enterprise (self-employed): 
	 11 - own account worker 
	 12 - employer
	 21 - worked as helper in Household enterprise (unpaid family worker)
	 31 - worked as regular salaried/wage employee 
 Worked as casual wage labour: 
	 41 - worked as casual wage labour in public works other than MGNREG works
	 42 - worked as casual wage labour in MGNREG works
	 51 - in other types of  work 
	 61 - had work in h.h. enterprise but did not work due to sickness
	 62 - other reasons
	 71 - had regular salaried/wage employment but did not work due to sickness
	 72 - other reasons
	 81 - sought work
	 82 - did not seek but was available for work
	 91 - attended educational institution 
	 92 - attended domestic duties only 
	 93 - attended domestic duties and also engaged in free collection of goods (vegetables, roots, firewood, cattle feed), sewing, tailoring, weaving for household use
     94 - rentiers, pensioners , remittance recipients
	 95 - not able to work due to disability 
	 97 - others (including begging, prostitution)  
	 98 - did not work due to temporary sickness (for casual workers only) 
		
	EARNINGS_SAL: Earnings for regular salaried/wage activity
EARNINGS_SELFEMP: Earnings for self-employed						
    WAGES_CASUAL: Earnings as casual wage labour					*/
egen wages_casual = rsum(wage*), missing
egen labor_total = rsum(wages_casual earnings_sal earnings_selfemp), missing

gen asal = 1 			if   status_cws>=31 & status_cws<=51
gen self = 1 			if   status_cws>=11 & status_cws<=12

egen max_asal = max(asal), by(hhid)
egen max_self = max(self), by(hhid)

egen max_labor = max(labor_total), by(hhid)

gen  	x = 1 			if   labor_total==max_labor 
egen 	y = sum(x), by(hhid)
replace x = 1       	if   max_asal==1 & max_self==1 & asal==1 & y>1 & y<.
replace x = .       	if   max_asal==1 & max_self==1 & asal!=1 & y>1 & y<.
replace x = 1       	if   max_asal!=1 & max_self==1 & self==1 & y>1 & y<.
drop y

replace x = 1			if  relationtohead==1 & max_labor==0 & x!=1
replace x = .			if  relationtohead!=1 & max_labor==0 
egen 	y = sum(x), by(hhid)
replace x = 1			if  relationtohead==1 & y>1 & y<.
replace x = .			if  relationtohead!=1 & y>1 & y<.
drop y
egen y = sum(x), by(hhid)
ta y
rename x maxincome


* Household Head Labor Status: Employed
gen 	auxi_emp = 0
replace auxi_emp = 1			if  relationtohead==1 & status_cws>=11 & status_cws<=51
egen hh_head_ls_wrk = max(auxi_emp), by(hhid)
drop auxi*	

* Household Head Labor Status: Unemployed
gen 	auxi_une = 0
replace auxi_une = 1			if  relationtohead==1 & status_cws>=81 & status_cws<=82
egen hh_head_ls_u = max(auxi_une), by(hhid)
drop auxi*

* Household Head Labor Status: Not in Labor Force
gen 	auxi_nlf = 0
replace auxi_nlf = 1			if  relationtohead==1 & ((status_cws>=91 & status_cws<=98) | (status_cws>=61 & status_cws<=72))
egen hh_head_ls_nlf = max(auxi_nlf), by(hhid)
drop auxi*

* Household share of members employed
gen 	auxi_emp = 0
replace auxi_emp = 1			if  status_cws>=11 & status_cws<=51
egen    auxi_wrk = sum(auxi_emp), by(hhid)
gen hh_sh_ls_wrk = auxi_wrk/hh_size
drop auxi*	


* Max income labor type: working for wage
gen 	auxi_emp = 0        	if  maxincome==1 & status_cws>=11 & status_cws<=51
replace auxi_emp = 1			if  maxincome==1 & status_cws>=31 & status_cws<=51
egen    max_inc_wrk_sal = max(auxi_emp), by(hhid)
replace max_inc_wrk_sal = 0		if  max_inc_wrk_sal==.
drop auxi*	

* Max income labor type: self employment
gen 	auxi_emp = 0        	if  maxincome==1 & status_cws>=11 & status_cws<=51
replace auxi_emp = 1			if  maxincome==1 & status_cws>=11 & status_cws<=30
egen    max_inc_wrk_self = max(auxi_emp), by(hhid)
replace max_inc_wrk_self = 0	if  max_inc_wrk_self==.
drop auxi*	

* Max income labor field: working for wage/profit in agriculture
gen 	auxi_emp = 0			if  maxincome==1 & status_cws>=11 & status_cws<=51
replace auxi_emp = 1			if  maxincome==1 & status_cws>=11 & status_cws<=51 & nic_cws>=1 & nic_cws<=9
egen    max_inc_wrk_agr = max(auxi_emp), by(hhid)
replace max_inc_wrk_agr = 0		if  max_inc_wrk_agr==.
drop auxi*

* Max income labor field: working for wage/profit in industry
gen 	auxi_emp = 0			if  maxincome==1 & status_cws>=11 & status_cws<=51
replace auxi_emp = 1			if  maxincome==1 & status_cws>=11 & status_cws<=51 & nic_cws>=10 & nic_cws<=43
egen    max_inc_wrk_ind = max(auxi_emp), by(hhid)
replace max_inc_wrk_ind = 0		if  max_inc_wrk_ind==.
drop auxi*

* Max income labor field: working for wage/profit in service
gen 	auxi_emp = 0			if  maxincome==1 & status_cws>=11 & status_cws<=51
replace auxi_emp = 1			if  maxincome==1 & status_cws>=11 & status_cws<=51 & nic_cws>=45 & nic_cws<=99
egen    max_inc_wrk_srv = max(auxi_emp), by(hhid)
replace max_inc_wrk_srv = 0	if  max_inc_wrk_srv==.
drop auxi*

* No labor income in the household
gen     max_null = 0
replace max_null = 1			if  max_inc_wrk_agr==0 & max_inc_wrk_ind==0 & max_inc_wrk_srv==0


* Household share of members by labor types: working for wage
gen 	auxtot = 0
replace auxtot = 1			if  status_cws>=11 & status_cws<=51 
egen 	total = sum(auxtot), by(hhid)

gen 	auxi_emp = 0
replace auxi_emp = 1			if  status_cws>=31 & status_cws<=51
egen 	hh_sh_wrk_s = max(auxi_emp), by(hhid)
replace hh_sh_wrk_s = hh_sh_wrk_s/total
drop auxi*

* Household share of working members in agriculture
gen 	auxi_emp = 0
replace auxi_emp = 1			if  status_cws>=11 & status_cws<=51 & nic_cws>=1 & nic_cws<=9
egen 	hh_sh_wrk_agr = sum(auxi_emp), by(hhid)
replace hh_sh_wrk_agr = hh_sh_wrk_agr/total
drop auxi*

* Household share of working members in industry
gen 	auxi_emp = 0
replace auxi_emp = 1			if  status_cws>=11 & status_cws<=51 & nic_cws>=10 & nic_cws<=43
egen 	hh_sh_wrk_ind = sum(auxi_emp), by(hhid)
replace hh_sh_wrk_ind = hh_sh_wrk_ind/total
drop auxi*

* Household share of working members in service
gen 	auxi_emp = 0
replace auxi_emp = 1			if  status_cws>=11 & status_cws<=51 & nic_cws>=45 & nic_cws<=99
egen 	hh_sh_wrk_srv = sum(auxi_emp), by(hhid)
replace hh_sh_wrk_srv = hh_sh_wrk_srv/total
drop auxi* total auxtot

* Welfare variable
gen consumption_pc = hh_usual_cons_rs/hh_size

* Temporal deflation
preserve 
use "${rootdatalib2}\\Aux_Data\\IND_allyears_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS_cpi.dta", clear  
keep if h_head==1
keep year hhid cpi_monthly
tempfile cpi 
save `cpi', replace
restore 

merge m:1 year hhid using `cpi', keepusing(cpi_monthly)  keep(match master)
tab _merge
drop _merge

notes: Average CPI during HCES 2022-23 months was 177.7042

replace consumption_pc = consumption_pc*177.7042/cpi_monthly



/*
*India team corrected weights for Assam
preserve
use "${input}\PLFS 23 24 new weights.dta", clear
capture drop hhid pid
rename secondstagestratumno stage2_stratumno
rename sbno sample_sg
rename samplehouseholdnumber hhno
rename person personno
egen hhid = concat(quarter visit fsu sample_sg stage2_stratumno hhno)
egen  pid = concat(quarter visit fsu sample_sg stage2_stratumno hhno personno)

keep hhid pid hhwt_new
tempfile temp
save `temp'
restore 

merge 1:1 hhid pid using `temp'
drop _merge

drop hhwt
rename hhwt_new hhwt
rename urb urban

*/


* Spatial deflation
preserve 
use "${rootdatalib2}\\Aux_Data\\Sp_deflators_HCES_23.dta", clear
*rename urb urban
tempfile spdef
save `spdef', replace
restore 

capture drop _merge
merge m:1 state urb using `spdef'
drop if _merge!=3 
drop _merge 

sum   spdef [aw=hhwt] 
local mean_nat = r(mean)

local lista "consumption_pc"
foreach var in `lista' {
	sum `var' [aw=hhwt] if relationtohead==1
	local avg = r(mean)

	gen   `var'_adj = `var'*`mean_nat'/spdef
	sum   `var'_adj [aw=hhwt] if relationtohead==1 
	local avg2 = r(mean)
	replace `var'_adj = `var'_adj*`avg'/`avg2'  
	}

*rename urban urb
*** SHARES CONSUMPTION 
egen   good_srvs = rsum(hh_usual_cons_gs hh_imputed_cons_rs_hg hh_imputed_cons_rs_ik)
gen exp_clothing = hh_exp_clothes/12
gen exp_durables = hh_exp_du/12
egen   exp_total = rsum(good_srvs exp_clothing exp_durables)

gen shr_goodsvrs = good_srvs/exp_total
gen shr_clothing = exp_clothing/exp_total
gen shr_durables = exp_durables/exp_total

keep if relationtohead==1 

keep hhid year hhwt hh_size hh_size_sq urb state_1-state_37 hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0 hh_size_age1 hh_size_age2 hh_size_age3 hh_size_age4 hh_size_age0_14 hh_size_age15_64 hh_size_age65plus hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 hh_sh_age4 hh_sh_age0_14 hh_sh_age15_64 hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio hh_head_male hh_sh_female hh_sh_male hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter hh_without_spouse max_inc_wrk_sal max_inc_wrk_self max_inc_wrk_agr max_inc_wrk_ind max_inc_wrk_srv max_null consumption_pc* shr*

order hhid year hhwt hh_size hh_size_sq urb state_1-state_37 hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0 hh_size_age1 hh_size_age2 hh_size_age3 hh_size_age4 hh_size_age0_14 hh_size_age15_64 hh_size_age65plus hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 hh_sh_age4 hh_sh_age0_14 hh_sh_age15_64 hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio hh_head_male hh_sh_female hh_sh_male hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter hh_without_spouse max_inc_wrk_sal max_inc_wrk_self max_inc_wrk_agr max_inc_wrk_ind max_inc_wrk_srv max_null consumption_pc* shr*
  
* Consumption deciles

xtile decile_national= consumption_pc_adj [w=hhwt], nq(10)   
  
save "${output}\IND_2023_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.dta", replace

