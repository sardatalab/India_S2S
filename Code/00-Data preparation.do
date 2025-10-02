**===International Poverty 2021 PPP INDIA==============================
**===S2S Database preparation =========================================
*Author:        SAR Data and Stats Team
*Last update:	7/23/25
*----------------------------------------------------------------------
*====================================================================
clear all

*=== Set up =================================================================*
	glo PPP = 2021   // or 2017
	if $PPP == 2021 {
		glo povlines 	300 420 830  // current poverty lines PPP 2021
		local cpi = "cpi2021"
		local ppp = "icp2021"
	} 
	else {
		glo povlines 	215 365 685  // previous poverty lines PPP 2017
		local cpi = "cpi2017"
		local ppp = "icp2017"
	}
	
	local code="IND"    // country code
	local svyear = 2022   // survey year
	local cpiversion="13" // current version on Datalibweb, please do not change
	gl path "C:\Users\wb553773\OneDrive - WBG\Stats Team\IND S2S imputation"
	gl data "${path}\Data"
	
	
*=== CPI and PPP from datalibweb=============================================*
	datalibweb, country(Support) year(2005) type(GMDRAW) ///
	surveyid(Support_2005_CPI_v`cpiversion'_M) ///
	filename(Final_CPI_PPP_to_be_used.dta)
	keep if code=="`code'" & year == `svyear'
	keep code `cpi' `ppp' 
	duplicates drop
	tempfile dlwcpi
	save `dlwcpi', replace
	
*=== Merge with harmonized dataset=============================================*	
*Load harmonized dataset
use "${path}\Data\IND_2022-23_HCES_v02_M_v02_A_s2s_HCES_to_PLFS", clear

gen pop_wgt=hhwt*hh_size
drop hhwt
gen code = "IND"
merge m:1 code using `dlwcpi', keep(match) nogen
gen welfare_ppp =(12/365)*welfaredef_final/ `cpi' / `ppp'
*=== Intl. Poverty ===========================================================*	
	foreach i of global povlines {
		g double poor_`i' 	= welfare_ppp < (`i' / 100)
		replace poor_`i' = . if welfare_ppp ==.
	}
	summ poor_* [aw=pop_wgt]
*=== Inequality ==============================================================*	
	ainequal welfare_ppp [w=pop_wgt]

drop code

ren welfaredef_final mpce_sp_def_ind
ren hh_size_sq hh_sizesq
*check dep_ratio 7650 missings because sh_1664 is zero
drop hh_size_*  hh_sh_age0 hh_sh_age1 hh_sh_age2 hh_sh_age3 ///
hh_sh_age4 hh_sh_male hh_sex_ratio hh_head_ilit hh_sh_ilit ///
hh_sh1564_lit  hh_sp_educ consumption_pc ///
hh_dep_ratio 

ren hh_sizesq hh_size_sq

save "${path}\Data\HCES22_s2s.dta", replace
/*
*=== Save spdef database ======================================================*
collapse (mean) spdef [aw=$weight], by(state urb)
save "${path}\Data\Sp_deflators_HCES_22.dta", replace