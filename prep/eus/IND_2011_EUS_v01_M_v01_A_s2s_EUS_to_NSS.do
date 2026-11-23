

*<_Datalibweb request_>
use "${input}\IND_2011_EUS_v01_M.dta", clear 
*<_Datalibweb request_>

*** Countrycode 
gen countrycode = "IND"

*** Survey name 
gen survname = "EUS"

*** Year of the survey
gen year = 2011 			

* Household weight
gen weight = Multiplier_comb 					// check this //

* Urban Dummy
destring Sector, replace
gen     urban = 0			if  Sector==1
replace urban = 1			if  Sector==2

* STRATA 
gen strata = Stratum

* PSU
gen psu = FSU_Serial_No

* Household Size and Squared Household Size
gen hh_size = HH_Size
gen hh_size_sq = hh_size*hh_size

* State Variable
rename State state
destring state, replace
replace state = 25			if  state==26

* Dummies state
levelsof state, local(variable)
foreach t of local variable {
	gen     state_`t' = 0
	replace state_`t' = 1	if  state==`t'
	}

* Household type 
destring HH_Type, replace
gen     hh_type = 1			if  HH_Type==1 & Sector==1		/* self-employed rural agriculture 		*/
replace hh_type = 2			if  HH_Type==2 & Sector==1		/* self-employed rural non-agriculture	*/ 
replace hh_type = 3			if  HH_Type==1 & Sector==2		/* self-employed urban 					*/
replace hh_type = 4			if  HH_Type==3 & Sector==1  		/* regular wage/salary earnings			*/
replace hh_type = 4			if  HH_Type==2 & Sector==2  		/* regular wage/salary earnings			*/
replace hh_type = 5			if  HH_Type==4 & Sector==1		/* casual labor rural agriculture   	*/
replace hh_type = 6			if  HH_Type==5 & Sector==1		/* casual labor rural non-agriculture	*/
replace hh_type = 7			if  HH_Type==3 & Sector==2		/* casual labor urban					*/
replace hh_type = 8			if  HH_Type==9					/* other								*/

* Dummies household type
levelsof hh_type, local(variable)
foreach t of local variable {
	gen     hhtype_`t' = 0
	replace hhtype_`t' = 1	if  hh_type==`t'
	}

destring NIC_2008, replace 
replace  NCO_2004 = "." 		if  NCO_2004=="X00" | NCO_2004=="X10" | NCO_2004=="X99"
destring NCO_2004, replace

* At least 1 salary earner in the household 
destring Usual_Principal_Activity_Status, replace
gen 	auxi = 0
replace auxi = 1				if  wage_total>0 & wage_total<. & Usual_Principal_Activity_Status==31
egen salary_earner = max(auxi), by(hhid)
drop auxi

* Household Employment Sector: agriculture
gen    industrycat10 = floor(NIC_2008/1000)
recode industrycat10 (1/3=1) (4/9=2) (10/33=3) (35/39=4) (41/43=5) (45/47 55/56=6) (49/53 58/63 79=7) (64/68 83=8) (84=9) (69/78 80/82 85/99=10) (*=.)
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==1
egen hh_sect_1 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: mining
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==2
egen hh_sect_2 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: manufacture
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==3
egen hh_sect_3 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: public utilities
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==4
egen hh_sect_4 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: construction
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==5
egen hh_sect_5 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: commerce
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==6
egen hh_sect_6 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: transport and communication
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==7
egen hh_sect_7 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: financial business
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==8
egen hh_sect_8 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: public administration
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==9
egen hh_sect_9 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: other services
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==10
egen hh_sect_10 = max(auxi_sector), by(hhid)
drop auxi*

* Household Employment Sector: no employment or undefined
gen 	auxi_sector = 0
replace auxi_sector = 1		if  industrycat10==.
egen hh_sect_und = max(auxi_sector), by(hhid)
drop auxi*

* Household Occupation: managers
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=111 & NCO_2004<=149
egen hh_occ_1 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: professionals
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=211 & NCO_2004<=249
egen hh_occ_2 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: technicians
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=311 & NCO_2004<=349
egen hh_occ_3 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: clerks
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=411 & NCO_2004<=449
egen hh_occ_4 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: services and shop and sales
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=511 & NCO_2004<=549
egen hh_occ_5 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: skilled agricultural
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=611 & NCO_2004<=649
egen hh_occ_6 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: craft and trade
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=711 & NCO_2004<=749
egen hh_occ_7 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: machine operators and assemblers
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=811 & NCO_2004<=849
egen hh_occ_8 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: elementary occupations
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=911 & NCO_2004<=949
egen hh_occ_9 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: not classified
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004>=970 & NCO_2004<=990
egen hh_occ_10 = max(auxi_occup), by(hhid)
drop auxi*

* Household Occupation: no employment or undefined
gen 	auxi_occup = 0
replace auxi_occup = 1		if  NCO_2004==.
egen hh_occ_und = max(auxi_occup), by(hhid)
drop auxi*

* Household Religion: hindu
destring Religion, replace 
gen 	auxi = 0
replace auxi = 1					if  Religion==1
egen hh_hin = max(auxi), by(hhid)
drop auxi

* Household Religion: non-hindu
gen 	auxi = 0
replace auxi = 1					if  Religion!=1
egen hh_nhin = max(auxi), by(hhid)
drop auxi

* Household Religion: muslim
gen 	auxi = 0
replace auxi = 1					if  Religion==2
egen hh_msl = max(auxi), by(hhid)
drop auxi

* Household Religion: buddhist 
gen 	auxi = 0
replace auxi = 1					if  Religion==5
egen hh_bud = max(auxi), by(hhid)
drop auxi

* Household Religion: christian
gen 	auxi = 0
replace auxi = 1					if  Religion==3
egen hh_chr = max(auxi), by(hhid)
drop auxi

* Household Religion: sikh
gen 	auxi = 0
replace auxi = 1					if  Religion==4
egen hh_sik = max(auxi), by(hhid)
drop auxi

* Household Religion: other 
gen 	auxi = 0
replace auxi = 1					if  Religion==5 | Religion==7 | Religion==9
egen hh_othrel = max(auxi), by(hhid)
drop auxi

* Household Social Group: scheduled tribe
destring Social_Group, replace
gen 	auxi = 0
replace auxi = 1					if  Social_Group==1
egen hh_tribe = max(auxi), by(hhid)
drop auxi

* Household Social Group: scheduled caste
gen 	auxi = 0
replace auxi = 1					if  Social_Group==2
egen hh_caste = max(auxi), by(hhid)
drop auxi

* Household Social Group: other backward class
gen 	auxi = 0
replace auxi = 1					if  Social_Group==3
egen hh_back = max(auxi), by(hhid)
drop auxi

* Household Social Group: other
gen 	auxi = 0
replace auxi = 1					if  Social_Group==9
egen hh_othersoc = max(auxi), by(hhid)
drop auxi

* Land Owned
gen		own_land = 0				
replace own_land = 1						if  Land_Owned>0 & Land_Owned<.

* Land Owned Hectareas
gen		own_land_ha = 0
replace own_land_ha = Land_Owned 			if  Land_Owned>0 & Land_Owned<.  

* Land Possessed
gen	 	land_possessed = 0			
replace land_possessed = Land_Possessed		if  Land_Possessed>0 & Land_Possessed<.

* Land Cultivated
gen	 	land_cultivated = 0			
replace land_cultivated = Land_Cultivated	if  Land_Cultivated>0 & Land_Cultivated<.

* Household Head Age
rename Age age
destring Relation_to_Head, replace
gen aux_head_age = age 			if  Relation_to_Head==1  
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
destring Sex, replace 
gen 	auxi_head_male = 0
replace auxi_head_male = 1		if  Relation_to_Head==1 & Sex==1 
egen hh_head_male = max(auxi_head_male), by(hhid)

* Share of female members
gen     auxi = 0
replace auxi = 1					if  Sex==2
egen    hh_sh_female = sum(auxi), by(hhid)
replace hh_sh_female = hh_sh_female/hh_size
drop auxi

* Share of male members
gen     auxi = 0
replace auxi = 1					if  Sex==1
egen    hh_sh_male = sum(auxi), by(hhid)
replace hh_sh_male = hh_sh_male/hh_size
drop auxi

* Sex ratio
gen     auxim = 0
replace auxim = 1				if  Sex==1
egen    males = sum(auxim), by(hhid)
gen     auxif = 0
replace auxif = 1				if  Sex==2
egen    females = sum(auxif), by(hhid)
gen     hh_sex_ratio = males/females

* Households without females 
gen 	flag2_sex = 0
replace flag2_sex = 1			if  females==0
drop auxi* males females

* Household Head Never Married
destring Marital_Status, replace
gen 	auxi = 0
replace auxi = 1					if  Relation_to_Head==1 & Marital_Status==1
egen hh_head_nm = max(auxi), by(hhid)
drop auxi

* Household Head Married 
gen 	auxi = 0
replace auxi = 1					if  Relation_to_Head==1 & Marital_Status==2
egen hh_head_m = max(auxi), by(hhid)
drop auxi

* Household Head Widow/Widower
gen 	auxi = 0
replace auxi = 1					if  Relation_to_Head==1 & Marital_Status==3
egen hh_head_w = max(auxi), by(hhid)
drop auxi

* Household Head Divorced/Separated
gen 	auxi = 0
replace auxi = 1					if  Relation_to_Head==1 & Marital_Status==4
egen hh_head_d = max(auxi), by(hhid)
drop auxi

* Household Head Literacy: literate
destring General_Education, replace
gen     auxi = 0
replace auxi = 1					if  Relation_to_Head==1 & General_Education!=1
egen hh_head_lit = max(auxi), by(hhid)
drop auxi 			

* Household Head Literacy: iliterate
gen     auxi = 0
replace auxi = 1					if  Relation_to_Head==1 & General_Education==1
egen hh_head_ilit = max(auxi), by(hhid)
drop auxi 				

* Household share of members 5+ literate
gen     auxilit = 0
replace auxilit = 1				if  General_Education!=1 & age>=5 & age<.
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1				if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_lit = literate/members
drop auxi* literate members			

* Household share of members 5+ iliterate
gen     auxiilit = 0
replace auxiilit = 1				if  General_Education==1 & age>=5 & age<.
egen    iliterate = sum(auxiilit), by(hhid)
gen     auximem = 0
replace auximem = 1				if  age>=5 & age<.
egen    members = sum(auximem), by(hhid)
gen hh_sh_ilit = iliterate/members
drop auxi* iliterate members	

* Household share of members 15-64 literate
gen     auxilit = 0
replace auxilit = 1				if  General_Education!=1 & age>=15 & age<=64
egen    literate = sum(auxilit), by(hhid)
gen     auximem = 0
replace auximem = 1				if  age>=15 & age<=64
egen    members = sum(auximem), by(hhid)
gen hh_sh1564_lit = literate/members
drop auxi* literate members	

* Household Head with no education
gen     auxi = 0
replace auxi = 1 				if  Relation_to_Head==1 & General_Education>=1 & General_Education<=4
egen hh_head_educ0 = max(auxi), by(hhid)
drop auxi		

* Household Head with primary education incomplete
gen     auxi = 0
replace auxi = 1 				if  Relation_to_Head==1 & (General_Education==5 | General_Education==6)
egen hh_head_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Head with primary education complete
gen     auxi = 0
replace auxi = 1 				if  Relation_to_Head==1 & General_Education==6  
egen hh_head_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education incomplete
gen     auxi = 0
replace auxi = 1 				if  Relation_to_Head==1 & (General_Education==7 | General_Education==8 | General_Education==10)
egen hh_head_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Head with secondary education complete
gen     auxi = 0
replace auxi = 1 				if  Relation_to_Head==1 & (General_Education==8 | General_Education==10 | General_Education==11)
egen hh_head_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Head with tertiary education
gen     auxi = 0
replace auxi = 1 				if  Relation_to_Head==1 & General_Education>=12 & General_Education<=13
egen hh_head_educ_ter = max(auxi), by(hhid)
drop auxi

* Household Spouse with no education
gen     auxi = 0					if  Relation_to_Head==2
replace auxi = 1 				if  Relation_to_Head==2 & General_Education>=1 & General_Education<=4
egen hh_sp_educ0 = max(auxi), by(hhid)
drop auxi		
* Household Spouse with primary education incomplete
gen     auxi = 0					if  Relation_to_Head==2
replace auxi = 1 				if  Relation_to_Head==2 & (General_Education==5 | General_Education==6)
egen hh_sp_educ_pri = max(auxi), by(hhid)
drop auxi
* Household Spouse with primary education complete
gen     auxi = 0					if  Relation_to_Head==2
replace auxi = 1 				if  Relation_to_Head==2 & General_Education==6  
egen hh_sp_educ_prc = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education incomplete
gen     auxi = 0					if  Relation_to_Head==2
replace auxi = 1 				if  Relation_to_Head==2 & (General_Education==7 | General_Education==8 | General_Education==10)
egen hh_sp_educ_sei = max(auxi), by(hhid)
drop auxi
* Household Spouse with secondary education complete
gen     auxi = 0					if  Relation_to_Head==2
replace auxi = 1 				if  Relation_to_Head==2 & (General_Education==8 | General_Education==10 | General_Education==11)
egen hh_sp_educ_sec = max(auxi), by(hhid)
drop auxi
* Household Spouse with tertiary education
gen     auxi = 0					if  Relation_to_Head==2
replace auxi = 1 				if  Relation_to_Head==2 & General_Education>=12 & General_Education<=13
egen hh_sp_educ_ter = max(auxi), by(hhid)
drop auxi	

* Households without Spouse	
gen     auxi = 0
replace auxi = 1 				if  Relation_to_Head==2 
egen auxi_spouse = max(auxi), by(hhid)
gen   	flag3_spouse = 0
replace flag3_spouse = 1			if  auxi_spouse==0
drop auxi*	

* Deciles/Quintiles
gen consumption_pc = Value_of/hh_size

xtile decile_national= consumption_pc [w=weight], nq(10) 

keep hhid strata psu weight year hh_size hh_size_sq urban state state_1-state_35 hh_type hhtype_1-hhtype_8 own_land own_land_ha land_possessed land_cultivated salary_earner hh_head_age hh_age_avg hh_size_age0-hh_size_age65plus hh_sh_age0-hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio flag1_depen hh_head_male hh_sh_female hh_sh_male flag2_sex hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_hin hh_nhin hh_msl hh_bud hh_chr hh_sik hh_othrel hh_tribe hh_caste hh_back hh_othersoc hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ* hh_sp_educ* flag3_spouse hh_sect_1-hh_sect_und hh_occ_1-hh_occ_und decile_national consumption_pc Relation


order hhid strata psu weight year hh_size hh_size_sq urban state state_1-state_35 hh_type hhtype_1-hhtype_8 own_land own_land_ha land_possessed land_cultivated salary_earner hh_head_age hh_age_avg hh_size_age0-hh_size_age65plus hh_sh_age0-hh_sh_age65plus hh_sh_age0_4 hh_dep_ratio flag1_depen hh_head_male hh_sh_female hh_sh_male flag2_sex hh_sex_ratio hh_head_nm hh_head_m hh_head_w hh_head_d hh_hin hh_nhin hh_msl hh_bud hh_chr hh_sik hh_othrel hh_tribe hh_caste hh_back hh_othersoc hh_head_lit hh_head_ilit hh_sh_lit hh_sh_ilit hh_sh1564_lit hh_head_educ* hh_sp_educ* flag3_spouse hh_sect_1-hh_sect_und hh_occ_1-hh_occ_und decile_national consumption_pc

/*
preserve 
use "D:\INDPOV_2025\Sp_deflators_HCES_22.dta", clear
rename urb urban
tempfile spdef
save `spdef', replace
restore 

capture drop _merge
merge m:1 state urban using `spdef'
drop if _merge!=3 
drop _merge 

* Deflating by CPI
sum   spdef [aw=weight] 
local mean_nat = r(mean)

local lista "consumption_pc"
foreach var in `lista' {
sum `var' [aw=weight] 
local avg = r(mean)

gen   `var'_adj = `var'*`mean_nat'/spdef
sum   `var'_adj [aw=weight] 
local avg2 = r(mean)
replace `var'_adj = `var'_adj*`avg'/`avg2'  
	}
*/

keep if Relation_to_Head==1
drop Relation_to_Head
save "${output}\\IND_2011_EUS_v01_M_v01_A_s2s_EUS_to_NSS.dta", replace


sum [w=weight]

