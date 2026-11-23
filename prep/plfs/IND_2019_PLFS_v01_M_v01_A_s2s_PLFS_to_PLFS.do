*<_Datalibweb request_>
use "${input}\IND_2019_PLFS-V1_v01_M.dta", clear 
replace  prn_occ = ""	if  prn_occ=="X99" | prn_occ=="X10"
destring prn_occ, replace
*<_Datalibweb request_>

*** Countrycode 
gen countrycode = "IND"

*** Survey name 
gen survname = "PLFS"

*** Year of the survey
rename year year_plfs
gen year = 2019 				if  year_plfs=="2019-20"

* Household weight
gen weight = hhwt

* Urban Dummy
gen     urban = 0			if  sector==1
replace urban = 1			if  sector==2

* STRATA 
gen strata = stratum

* PSU
gen psu = fsu

* Household Size and Squared Household Size
gen hh_size = hhsize
gen hh_size_sq = hhsize*hhsize

* State Variable
replace state = 25			if  state==26
replace state = 1			if  state==37 

* Dummies state
levelsof state, local(variable)
foreach t of local variable {
	gen     state_`t' = 0
	replace state_`t' = 1	if  state==`t'
	}

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

* Households without members aged 15-64
gen 	flag1_depen = 0
replace flag1_depen = 1			if  hh_size_age15_64==0

* Male Household Head 
gen 	auxi_head_male = 0
replace auxi_head_male = 1		if  relationtohead==1 & sex==1 
egen hh_head_male = max(auxi_head_male), by(hhid)

* Share of female members
gen     auxi = 0
replace auxi = 1					if  sex==2
egen    hh_sh_female = sum(auxi), by(hhid)
replace hh_sh_female = hh_sh_female/hh_size
drop auxi

* Share of male members
gen     auxi = 0
replace auxi = 1					if  sex==1
egen    hh_sh_male = sum(auxi), by(hhid)
replace hh_sh_male = hh_sh_male/hh_size
drop auxi

* Sex ratio
gen     auxim = 0
replace auxim = 1				if  sex==1
egen    males = sum(auxim), by(hhid)
gen     auxif = 0
replace auxif = 1				if  sex==2
egen    females = sum(auxif), by(hhid)
gen     hh_sex_ratio = males/females

* Households without females 
gen 	flag2_sex = 0
replace flag2_sex = 1			if  females==0
drop auxi* males females

* Household Head Never Married
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & marital==1
egen hh_head_nm = max(auxi), by(hhid)
drop auxi

* Household Head Married 
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & marital==2
egen hh_head_m = max(auxi), by(hhid)
drop auxi

* Household Head Widow/Widower
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & marital==3
egen hh_head_w = max(auxi), by(hhid)
drop auxi

* Household Head Divorced/Separated
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & marital==4
egen hh_head_d = max(auxi), by(hhid)
drop auxi

* Household Head Religion: hindu
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & religion==1
egen hh_head_hin = max(auxi), by(hhid)
drop auxi

* Household Head Religion: non-hindu
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & religion!=1
egen hh_head_nhin = max(auxi), by(hhid)
drop auxi

* Household Head Religion: muslim
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & religion==2
egen hh_head_msl = max(auxi), by(hhid)
drop auxi

* Household Head Religion: buddhist 
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & religion==6
egen hh_head_bud = max(auxi), by(hhid)
drop auxi

* Household Head Religion: christian
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & religion==3
egen hh_head_chr = max(auxi), by(hhid)
drop auxi

* Household Head Religion: sikh
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & religion==4
egen hh_head_sik = max(auxi), by(hhid)
drop auxi

* Household Head Religion: other 
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & (religion==5 | religion==7 | religion==9)
egen hh_head_oth = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: scheduled tribe
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & socialgroup==1
egen hh_head_tribe = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: scheduled caste
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & socialgroup==2
egen hh_head_caste = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: other backward class
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & socialgroup==3
egen hh_head_back = max(auxi), by(hhid)
drop auxi

* Household Head Social Group: other
gen 	auxi = 0
replace auxi = 1					if  relationtohead==1 & socialgroup==9
egen hh_head_other = max(auxi), by(hhid)
drop auxi

*******************************************************************************************
replace noyrsformaled = 4 		if  geneducation==5 & noyrsformaled>=5 & noyrsformaled<=9
replace noyrsformaled = 7 		if  geneducation==6 & noyrsformaled>=8 & noyrsformaled<=12
replace noyrsformaled = 10 		if  geneducation==7 & noyrsformaled>=11 & noyrsformaled<=13
replace noyrsformaled = 12		if  geneducation==8 & noyrsformaled>=13 & noyrsformaled<=15
replace noyrsformaled = 10		if  geneducation==8 & noyrsformaled==9
replace noyrsformaled = 14		if  geneducation==10 & noyrsformaled>=15 & noyrsformaled<=17
replace noyrsformaled = 12		if  geneducation==10 & noyrsformaled>=10 & noyrsformaled<=11
replace noyrsformaled = 14		if  geneducation==11 & noyrsformaled>=15 & noyrsformaled<=18
replace noyrsformaled = 17		if  geneducation==12 & noyrsformaled>=18 & noyrsformaled<=20
replace noyrsformaled = 15		if  geneducation==12 & noyrsformaled>=11 & noyrsformaled<=14
replace noyrsformaled = 21		if  geneducation==13 & noyrsformaled>=22 & noyrsformaled<=26
replace noyrsformaled = 17		if  geneducation==13 & noyrsformaled==16
*******************************************************************************************

* Household Head Literacy: literate
gen     auxi = 0
replace auxi = 1					if  relationtohead==1 & geneducation!=1
egen hh_head_lit = max(auxi), by(hhid)
drop auxi 			

* Household Head Literacy: iliterate
gen     auxi = 0
replace auxi = 1					if  relationtohead==1 & geneducation==1
egen hh_head_ilit = max(auxi), by(hhid)
drop auxi 				

* Household share of members 5+ literate
gen     auxilit = 0
replace auxilit = 1				if  geneducation!=1 & age>=5 & age<.
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1				if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_lit = literate/members
drop auxi* literate members			

* Household share of members 5+ iliterate
gen     auxiilit = 0
replace auxiilit = 1				if  geneducation==1 & age>=5 & age<.
egen    iliterate = sum(auxiilit), by(hhid)
gen     auximem = 0
replace auximem = 1				if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_ilit = iliterate/members
drop auxi* iliterate members	

* Household share of members 15-64 literate
gen     auxilit = 0
replace auxilit = 1				if  geneducation!=1 & age>=15 & age<=64
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1				if  age>=15 & age<=64
egen    members = sum(auximem), by(hhid)
gen hh_sh1564_lit = literate/members
drop auxi* literate members	

* Household Head years of education
gen auxi = noyrsformaled 			if  relationtohead==1 
egen hh_head_educ = max(auxi), by(hhid)
drop auxi		

* Household max. years of education among adult members
egen hh_mx_educ = max(noyrsformaled), by(hhid)

* Household avg. years of education of adults members
gen  educyears = noyrsformaled 	if  age>=15 & age<.
egen hh_avg_educ = mean(educyears), by(hhid)			

* Household Spouse years of education
gen auxi = noyrsformaled 			if  relationtohead==2  
egen hh_sp_educ = max(auxi), by(hhid)
drop auxi	

* Household Head with no education
gen     auxi = 0
replace auxi = 1 				if  relationtohead==1 & geneducation>=1 & geneducation<=4
egen hh_head_educ0 = max(auxi), by(hhid)
drop auxi		

* Household Head with primary education incomplete
gen     auxi = 0
replace auxi = 1 				if  relationtohead==1 & (geneducation==5 | (geneducation==6 & noyrsformaled>=4 & noyrsformaled<=5))
egen hh_head_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Head with primary education complete
gen     auxi = 0
replace auxi = 1 				if  relationtohead==1 & geneducation==6 & noyrsformaled>=6 & noyrsformaled<=7 
egen hh_head_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education incomplete
gen     auxi = 0
replace auxi = 1 				if  relationtohead==1 & (geneducation==7 | (geneducation==8 & noyrsformaled>=9 & noyrsformaled<=10) | (geneducation==10 & noyrsformaled>=10 & noyrsformaled<=11))
egen hh_head_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education complete
gen     auxi = 0
replace auxi = 1 				if  relationtohead==1 & ((geneducation==8 & noyrsformaled>=11 & noyrsformaled<=12) | (geneducation==10 & noyrsformaled>=12) | geneducation==11)
egen hh_head_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Head with tertiary education
gen     auxi = 0
replace auxi = 1 				if  relationtohead==1 & geneducation>=12 & geneducation<=13
egen hh_head_educ_ter = max(auxi), by(hhid)
drop auxi

* Household Spouse with no education
gen     auxi = 0					if  relationtohead==2
replace auxi = 1 				if  relationtohead==2 & geneducation>=1 & geneducation<=4
egen hh_sp_educ0 = max(auxi), by(hhid)
drop auxi		
* Household Spouse with primary education incomplete
gen     auxi = 0					if  relationtohead==2
replace auxi = 1 				if  relationtohead==2 & (geneducation==5 | (geneducation==6 & noyrsformaled>=4 & noyrsformaled<=5))
egen hh_sp_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Spouse with primary education complete
gen     auxi = 0					if  relationtohead==2
replace auxi = 1 				if  relationtohead==2 & geneducation==6 & noyrsformaled>=6 & noyrsformaled<=7 
egen hh_sp_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education incomplete
gen     auxi = 0					if  relationtohead==2
replace auxi = 1 				if  relationtohead==2 & (geneducation==7 | (geneducation==8 & noyrsformaled>=9 & noyrsformaled<=10) | (geneducation==10 & noyrsformaled>=10 & noyrsformaled<=11))
egen hh_sp_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education complete
gen     auxi = 0					if  relationtohead==2
replace auxi = 1 				if  relationtohead==2 & ((geneducation==8 & noyrsformaled>=11 & noyrsformaled<=12) | (geneducation==10 & noyrsformaled>=12) | geneducation==11)
egen hh_sp_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Spouse with tertiary education
gen     auxi = 0					if  relationtohead==2
replace auxi = 1 				if  relationtohead==2 & geneducation>=12 & geneducation<=13
egen hh_sp_educ_ter = max(auxi), by(hhid)
drop auxi	

* Households without Spouse	
gen     auxi = 0
replace auxi = 1 				if  relationtohead==2 
egen auxi_spouse = max(auxi), by(hhid)
gen   	flag3_spouse = 0
replace flag3_spouse = 1			if  auxi_spouse==0
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
egen    earnings_cas = rsum(wage_act1_* wage_act2_*)
replace earnings_cas = earnings_cas*4.3

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

* Household Head Labor Relation: working for regular wage
gen 	auxi_salreg = 0
replace auxi_salreg = 1		if  relationtohead==1 & status_cws==31
egen hh_head_wrk_salreg = max(auxi_salreg), by(hhid)
drop auxi*

* Household Head Labor Relation: working for casual wage
gen 	auxi_salcas = 0
replace auxi_salcas = 1		if  relationtohead==1 & status_cws>=41 & status_cws<=51
egen hh_head_wrk_salcas = max(auxi_salcas), by(hhid)
drop auxi*

* Household Head Labor Relation: working as employer/self-employed
gen 	auxi_self = 0
replace auxi_self = 1		if  relationtohead==1 & status_cws>=11 & status_cws<=12
egen hh_head_wrk_self = max(auxi_self), by(hhid)
drop auxi*

* Household Head Labor Relation: working as unpaid family worker
gen 	auxi_unpaid = 0
replace auxi_unpaid = 1		if  relationtohead==1 & status_cws==21
egen hh_head_wrk_unp = max(auxi_unpaid), by(hhid)
drop auxi*

* Household Head Employment Sector: agriculture
gen    industrycat10 = nic_cws
recode industrycat10 (1/3=1) (5/9=2) (10/33=3) (35/39=4) (41/43=5) (45/47 55/56=6) (49/53 58/63 79=7) (64/68=8) (84=9) (69/78 80/82 85/99=10) (*=.)
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==1
egen hh_head_sect_1 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: mining
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==2
egen hh_head_sect_2 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: manufacture
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==3
egen hh_head_sect_3 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: public utilities
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==4
egen hh_head_sect_4 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: construction
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==5
egen hh_head_sect_5 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: commerce
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==6
egen hh_head_sect_6 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: transport and communication
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==7
egen hh_head_sect_7 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: financial business
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==8
egen hh_head_sect_8 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: public administration
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==9
egen hh_head_sect_9 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Employment Sector: other services
gen 	auxi_sector = 0
replace auxi_sector = 1		if  relationtohead==1 & industrycat10==10
egen hh_head_sect_10 = max(auxi_sector), by(hhid)
drop auxi*

* Household Head Occupation: managers
replace cws_occupation = "." 	if  cws_occupation=="X10" | cws_occupation=="X99" 
destring cws_occupation, replace
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=111 & cws_occupation<=149
egen hh_head_occ_1 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: professionals
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=211 & cws_occupation<=249
egen hh_head_occ_2 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: technicians
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=311 & cws_occupation<=349
egen hh_head_occ_3 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: clerks
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=411 & cws_occupation<=449
egen hh_head_occ_4 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: services and shop and sales
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=511 & cws_occupation<=549
egen hh_head_occ_5 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: skilled agricultural
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=611 & cws_occupation<=649
egen hh_head_occ_6 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: craft and trade
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=711 & cws_occupation<=749
egen hh_head_occ_7 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: machine operators and assemblers
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=811 & cws_occupation<=849
egen hh_head_occ_8 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: elementary occupations
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=911 & cws_occupation<=949
egen hh_head_occ_9 = max(auxi_occup), by(hhid)
drop auxi*

* Household Head Occupation: not classified
gen 	auxi_occup = 0
replace auxi_occup = 1		if  relationtohead==1 & cws_occupation>=970 & cws_occupation<=990
egen hh_head_occ_10 = max(auxi_occup), by(hhid)
drop auxi*

* Household Heas has a Written Job Contract
gen 	auxi_jobcon = 0		if  relationtohead==1 & prn_job_contract>=1 & prn_job_contract<=4
replace auxi_jobcon = 1		if  relationtohead==1 & prn_job_contract>=2 & prn_job_contract<=4
egen hh_head_job_contract = max(auxi_jobcon), by(hhid)
drop auxi*

* Household Head is eligible for Paid Leave
gen 	auxi_paidle = 0		if  relationtohead==1 & prn_paid_leave==2
replace auxi_paidle = 1		if  relationtohead==1 & prn_paid_leave==1
egen hh_head_paidleave = max(auxi_paidle), by(hhid)
drop auxi*

* Household Head is eligible for Pension
gen 	auxi_pension = 0		if  relationtohead==1 & prn_soc_security>=1 & prn_soc_security<=9
replace auxi_pension = 1		if  relationtohead==1 & (prn_soc_security==1 | prn_soc_security==4 | prn_soc_security==5 | prn_soc_security==7)
egen hh_head_pension = max(auxi_pension), by(hhid)
drop auxi*

* Household Head is eligible for Gratuity
gen 	auxi_gratuity = 0		if  relationtohead==1 & prn_soc_security>=1 & prn_soc_security<=9
replace auxi_gratuity = 1		if  relationtohead==1 & (prn_soc_security==2 | prn_soc_security==4 | prn_soc_security==6 | prn_soc_security==7)
egen hh_head_gratuity = max(auxi_gratuity), by(hhid)
drop auxi*

* Household Head is eligible for Health Care and Maternity Benefits
gen 	auxi_health = 0		if  relationtohead==1 & prn_soc_security>=1 & prn_soc_security<=9
replace auxi_health = 1		if  relationtohead==1 & (prn_soc_security==3 | prn_soc_security==5 | prn_soc_security==6 | prn_soc_security==7)
egen hh_head_health = max(auxi_health), by(hhid)
drop auxi*

* Household Head is asked the question on labor benefits
gen 	auxi_flag = 0		if  relationtohead==1 
replace auxi_flag = 1		if  relationtohead==1 & prn_paid_leave==.
egen flag4_head_lab = max(auxi_flag), by(hhid)
drop auxi*

* Household share of members employed
gen 	auxi_emp = 0
replace auxi_emp = 1			if  status_cws>=11 & status_cws<=51
egen    auxi_wrk = sum(auxi_emp), by(hhid)
gen hh_sh_ls_wrk = auxi_wrk/hh_size
drop auxi*	

* Household share of members by labor relation: working for regular wage
gen 	auxtot = 0
replace auxtot = 1				if  status_cws>=11 & status_cws<=51 
egen 	total = sum(auxtot), by(hhid)

gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws==31 
egen 	hh_sh_wrk_salreg = sum(auxi_emp), by(hhid)
replace hh_sh_wrk_salreg = hh_sh_wrk_salreg/total
replace hh_sh_wrk_salreg = 0		if  hh_sh_wrk_salreg==.
drop auxi* auxtot

* Household share of members by labor relation: working for casual wage
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=41 & status_cws<=51 
egen 	hh_sh_wrk_salcas = sum(auxi_emp), by(hhid)
replace hh_sh_wrk_salcas = hh_sh_wrk_salcas/total
replace hh_sh_wrk_salcas = 0		if  hh_sh_wrk_salcas==.
drop auxi*

* Household share of members by labor relation: working as employer/self-employed
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=12 
egen 	hh_sh_wrk_selfem = sum(auxi_emp), by(hhid)
replace hh_sh_wrk_selfem = hh_sh_wrk_selfem/total
replace hh_sh_wrk_selfem = 0		if  hh_sh_wrk_selfem==.
drop auxi*

* Household share of members by labor relation: unpaid family worker
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws==21 
egen 	hh_sh_wrk_unpaid = sum(auxi_emp), by(hhid)
replace hh_sh_wrk_unpaid = hh_sh_wrk_unpaid/total
replace hh_sh_wrk_unpaid = 0		if  hh_sh_wrk_unpaid==.
drop auxi*

* Household share of working members in agriculture
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==1
egen 	hh_sh_sect_1 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_1 = hh_sh_sect_1/total
replace hh_sh_sect_1 = 0			if  hh_sh_sect_1==.
drop auxi*

* Household share of working members in mining
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==2
egen 	hh_sh_sect_2 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_2 = hh_sh_sect_2/total
replace hh_sh_sect_2 = 0			if  hh_sh_sect_2==.
drop auxi*

* Household share of working members in manufactures
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==3
egen 	hh_sh_sect_3 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_3 = hh_sh_sect_3/total
replace hh_sh_sect_3 = 0			if  hh_sh_sect_3==.
drop auxi* 

* Household share of working members in public utilities
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==4
egen 	hh_sh_sect_4 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_4 = hh_sh_sect_4/total
replace hh_sh_sect_4 = 0			if  hh_sh_sect_4==.
drop auxi* 

* Household share of working members in construction
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==5
egen 	hh_sh_sect_5 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_5 = hh_sh_sect_5/total
replace hh_sh_sect_5 = 0			if  hh_sh_sect_5==.
drop auxi* 

* Household share of working members in commerce
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==6
egen 	hh_sh_sect_6 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_6 = hh_sh_sect_6/total
replace hh_sh_sect_6 = 0			if  hh_sh_sect_6==.
drop auxi* 

* Household share of working members in transport and communication
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==7
egen 	hh_sh_sect_7 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_7 = hh_sh_sect_7/total
replace hh_sh_sect_7 = 0			if  hh_sh_sect_7==.
drop auxi* 

* Household share of working members in financial and business
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==8
egen 	hh_sh_sect_8 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_8 = hh_sh_sect_8/total
replace hh_sh_sect_8 = 0			if  hh_sh_sect_8==.
drop auxi* 

* Household share of working members in public administration
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==9
egen 	hh_sh_sect_9 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_9 = hh_sh_sect_9/total
replace hh_sh_sect_9 = 0			if  hh_sh_sect_9==.
drop auxi* 

* Household share of working members in other services
gen 	auxi_emp = 0
replace auxi_emp = 1				if  status_cws>=11 & status_cws<=51 & industrycat10==10
egen 	hh_sh_sect_10 = sum(auxi_emp), by(hhid)
replace hh_sh_sect_10 = hh_sh_sect_10/total
replace hh_sh_sect_10 = 0			if  hh_sh_sect_10==.
drop auxi* 

* Household share of members occupied as managers
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=111 & cws_occupation<=149
egen 	hh_sh_occ_1 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_1 = hh_sh_occ_1/total
drop auxi* 

* Household share of members occupied as professionals
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=211 & cws_occupation<=249
egen 	hh_sh_occ_2 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_2 = hh_sh_occ_2/total
drop auxi* 

* Household share of members occupied as technicians
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=311 & cws_occupation<=349
egen 	hh_sh_occ_3 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_3 = hh_sh_occ_3/total
drop auxi* 

* Household share of members occupied clerks
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=411 & cws_occupation<=449
egen 	hh_sh_occ_4 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_4 = hh_sh_occ_4/total
drop auxi* 

* Household share of members occupied as services and shop and sales
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=511 & cws_occupation<=549
egen 	hh_sh_occ_5 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_5 = hh_sh_occ_5/total
drop auxi* 

* Household share of members occupied as skilled agriculturals
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=611 & cws_occupation<=649
egen 	hh_sh_occ_6 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_6 = hh_sh_occ_6/total
drop auxi* 

* Household share of members occupied as craft and trade
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=711 & cws_occupation<=749
egen 	hh_sh_occ_7 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_7 = hh_sh_occ_7/total
drop auxi* 

* Household share of members occupied as machine operators and assemblers
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=811 & cws_occupation<=849
egen 	hh_sh_occ_8 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_8 = hh_sh_occ_8/total
drop auxi* 

* Household share of members occupied in elementary occupations
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=911 & cws_occupation<=949
egen 	hh_sh_occ_9 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_9 = hh_sh_occ_9/total
drop auxi* 

* Household share of members occupied in not classified occupations
gen 	auxi_occ = 0
replace auxi_occ = 1				if  status_cws>=11 & status_cws<=51 & cws_occupation>=970 & cws_occupation<=990
egen 	hh_sh_occ_10 = sum(auxi_occ), by(hhid)
replace hh_sh_occ_10 = hh_sh_occ_10/total
drop auxi* total

* Household share of member having a Written Job Contract
gen 	auxtot = 0
replace auxtot = 1				if  prn_paid_leave>=1 & prn_paid_leave<=2 
egen 	total = sum(auxtot), by(hhid)

gen 	auxi_jobcon = 0		
replace auxi_jobcon = 1			if  prn_job_contract>=2 & prn_job_contract<=4
egen 	hh_sh_job_contract = sum(auxi_jobcon), by(hhid)
replace hh_sh_job_contract = hh_sh_job_contract/total
replace hh_sh_job_contract = .	if  total==0
drop auxi* 

* Household share of members being eligible for Paid Leave
gen 	auxi_paidle = 0		
replace auxi_paidle = 1			if  prn_paid_leave==1
egen 	hh_sh_paidleave = sum(auxi_paidle), by(hhid)
replace hh_sh_paidleave = hh_sh_paidleave/total
replace hh_sh_paidleave = .		if  total==0
drop auxi*

* Household share of members being eligible for Pension
gen 	auxi_pension = 0		
replace auxi_pension = 1			if  prn_soc_security==1 | prn_soc_security==4 | prn_soc_security==5 | prn_soc_security==7
egen 	hh_sh_pension = sum(auxi_pension), by(hhid)
replace hh_sh_pension = hh_sh_pension/total
replace hh_sh_pension = .			if  total==0
drop auxi*

* Household share of members being eligible for Gratuity
gen 	auxi_gratuity = 0		
replace auxi_gratuity = 1			if  prn_soc_security==2 | prn_soc_security==4 | prn_soc_security==6 | prn_soc_security==7
egen 	hh_sh_gratuity = sum(auxi_gratuity), by(hhid)
replace hh_sh_gratuity = hh_sh_gratuity/total
replace hh_sh_gratuity = .		if  total==0
drop auxi*

* Household share of members being eligible for Health Care and Maternity Benefits
gen 	auxi_health = 0		
replace auxi_health = 1			if  prn_soc_security==3 | prn_soc_security==5 | prn_soc_security==6 | prn_soc_security==7
egen 	hh_sh_health = sum(auxi_health), by(hhid)
replace hh_sh_health = hh_sh_health/total
replace hh_sh_health = .			if  total==0
drop auxi* auxtot

* Household Head is asked the question on labor benefits
gen 	auxi_flag = 0			
replace auxi_flag = 1			if  prn_paid_leave==.
egen flag5_all_lab = min(auxi_flag), by(hhid)
drop auxi*

* Household Income from Regular Salaried/Wage activities
egen 	regular_wages = sum(earnings_sal), by(hhid)

* Household Income from Casual Salaried/Wage activities
egen 	casual_wages = sum(earnings_cas), by(hhid)

* Household Income from Employer/Self-Employment activities
egen 	self_income = sum(earnings_self), by(hhid)
replace self_income = 0		if  self_income<0

* Share of household labor income from regular salaried/wage activities
egen total_labor = rsum(regular_wages casual_wages self_income), missing
gen 	shr_regwages = regular_wages/total_labor
replace shr_regwages = 0		if  shr_regwages==. & total_labor!=0

* Share of household labor income from casual wage activities
gen 	shr_caswages = casual_wages/total_labor
replace shr_caswages = 0		if  shr_caswages==. & total_labor!=0

* Share of household labor income from employer/self-employmnet activities
gen 	shr_selfinc = self_income/total_labor
replace shr_selfinc = 0		if  shr_selfinc==. & total_labor!=0

* No labor Income
gen     flag6_income = 0
replace flag6_income = 1		if  total_labor==0 

* Welfare variable
gen consumption_pc = hh_usual_cons_rs/hh_size

* Temporal deflation
preserve 
use "${rootdatalib}\\Aux_Data\\IND_allyears_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS_cpi.dta", clear  
keep if h_head==1
keep year hhid cpi_monthly
tempfile cpi 
save `cpi', replace
restore 

merge m:1 year hhid using `cpi', keepusing(cpi_monthly)  keep(match master)
tab _merge
drop _merge


notes: Average CPI during HCES 2022-23 months was 177.7042

replace regular_wages = regular_wages*177.7042/cpi_monthly
replace casual_wages = casual_wages*177.7042/cpi_monthly
replace self_income = self_income*177.7042/cpi_monthly
replace total_labor = total_labor*177.7042/cpi_monthly
replace consumption_pc = consumption_pc*177.7042/cpi_monthly

keep hhid pid strata psu weight year hh_size hh_size_sq urban state state_1-state_36 hh_type hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0-hh_size_age65plus hh_sh_age0-hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio flag1_depen hh_head_male hh_sh_female hh_sh_male flag2_sex hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter flag3_spouse hh_head_ls_wrk hh_head_ls_u hh_head_ls_nlf hh_head_wrk_salreg hh_head_wrk_salcas hh_head_wrk_self hh_head_wrk_unp hh_head_sect_1-hh_head_sect_10 hh_head_occ_1-hh_head_occ_10 hh_head_job_contract hh_head_paidleave hh_head_pension hh_head_gratuity hh_head_health flag4_head_lab hh_sh_ls_wrk hh_sh_wrk_salreg hh_sh_wrk_salcas hh_sh_wrk_selfem hh_sh_wrk_unpaid hh_sh_sect_1-hh_sh_sect_10 hh_sh_occ_1-hh_sh_occ_10 hh_sh_job_contract hh_sh_paidleave hh_sh_pension hh_sh_gratuity hh_sh_health flag5_all_lab regular_wages casual_wages self_income total_labor shr_regwages shr_caswages shr_selfinc flag6_income consumption_pc relationtohead

order hhid pid strata psu weight year hh_size hh_size_sq urban state state_1-state_36 hh_type hhtype_1-hhtype_8 hh_head_age hh_age_avg hh_size_age0-hh_size_age65plus hh_sh_age0-hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio flag1_depen hh_head_male hh_sh_female hh_sh_male flag2_sex hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_head_hin hh_head_nhin hh_head_msl hh_head_bud hh_head_chr hh_head_sik hh_head_oth hh_head_tribe hh_head_caste hh_head_back hh_head_other hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ hh_mx_educ hh_avg_educ hh_sp_educ hh_head_educ0 hh_head_educ_pri hh_head_educ_prc hh_head_educ_sei hh_head_educ_sec hh_head_educ_ter hh_sp_educ0 hh_sp_educ_pri hh_sp_educ_prc hh_sp_educ_sei hh_sp_educ_sec hh_sp_educ_ter flag3_spouse hh_head_ls_wrk hh_head_ls_u hh_head_ls_nlf hh_head_wrk_salreg hh_head_wrk_salcas hh_head_wrk_self hh_head_wrk_unp hh_head_sect_1-hh_head_sect_10 hh_head_occ_1-hh_head_occ_10 hh_head_job_contract hh_head_paidleave hh_head_pension hh_head_gratuity hh_head_health flag4_head_lab hh_sh_ls_wrk hh_sh_wrk_salreg hh_sh_wrk_salcas hh_sh_wrk_selfem hh_sh_wrk_unpaid hh_sh_sect_1-hh_sh_sect_10 hh_sh_occ_1-hh_sh_occ_10 hh_sh_job_contract hh_sh_paidleave hh_sh_pension hh_sh_gratuity hh_sh_health flag5_all_lab regular_wages casual_wages self_income total_labor shr_regwages shr_caswages shr_selfinc flag6_income

gen regular_wages_pc = regular_wages/hh_size
gen  casual_wages_pc = casual_wages/hh_size
gen   self_income_pc = self_income/hh_size
gen   total_labor_pc = total_labor/hh_size

* Spatial deflation
preserve 
use "${rootdatalib}\\Aux_Data\\Sp_deflators_HCES_22.dta", clear
rename urb urban
tempfile spdef
save `spdef', replace
restore 

capture drop _merge
merge m:1 state urban using `spdef'
drop if _merge!=3 
drop _merge 

sum   spdef [aw=weight] 
local mean_nat = r(mean)

local lista "consumption_pc total_labor_pc"
foreach var in `lista' {
	sum `var' [aw=weight] if relationtohead==1 
	local avg = r(mean)

	gen   `var'_adj = `var'*`mean_nat'/spdef
	sum   `var'_adj [aw=weight] if relationtohead==1 
	local avg2 = r(mean)
	replace `var'_adj = `var'_adj*`avg'/`avg2'  
	}
	
* Consumption deciles
xtile decile_national= consumption_pc_adj [w=weight], nq(10) 
keep if relationtohead==1
drop spdef relationtohead pid
save "${output}\IND_2019_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta", replace

