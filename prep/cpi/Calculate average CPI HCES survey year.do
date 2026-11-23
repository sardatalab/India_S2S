use "C:/Users/wb587758/WBG/ESAPV Files - DATA/INTPOV/cpi/_archive/cpi_master_monthly_2011_2023.dta", clear

drop year_svy*
g year_svy = 2011 if inrange(survey_month, tm(2011m7), tm(2012m6))
		replace year_svy = 2022 if inrange(survey_month, tm(2022m8), tm(2023m7))
		replace year_svy = 2023 if inrange(survey_month, tm(2023m8), tm(2024m7))
	* In PLFS, they always work from July to June
		replace year_svy = 2017 if inrange(survey_month, tm(2017m7), tm(2018m6))
		replace year_svy = 2018 if inrange(survey_month, tm(2018m7), tm(2019m6))
		replace year_svy = 2019 if inrange(survey_month, tm(2019m7), tm(2020m6))
		replace year_svy = 2020 if inrange(survey_month, tm(2020m7), tm(2021m6))
		replace year_svy = 2021 if inrange(survey_month, tm(2021m7), tm(2022m6)) 
		
		g year_svy_hces = "2011-12" if year_svy == 2011 
		replace year_svy_hces = "2022-23" if year_svy == 2022 
		replace year_svy_hces = "2023-24" if year_svy == 2023
 
	g year_svy_plfs = "2017" if year_svy == 2017 
		replace year_svy_plfs = "2018" if year_svy == 2018 
		replace year_svy_plfs = "2019" if year_svy == 2019 
		replace year_svy_plfs = "2020" if year_svy == 2020 
		replace year_svy_plfs = "2021" if year_svy == 2021 
		replace year_svy_plfs = "2022" if inrange(survey_month, tm(2022m7), tm(2023m6))
		replace year_svy_plfs = "2023" if inrange(survey_month, tm(2023m7), tm(2024m6)) 
*Calculate yearly CPI averages at state sector scode level 

	bys state sector scode year_svy_hces : egen cpi_avg_year_svy_hces = mean(cpi_monthly)

			replace cpi_avg_year_svy_hces = . if year_svy_hces==""


	bys state scode year_svy_hces : egen cpi_avg_year_svy_hces_nat = mean(cpi_monthly)

			replace cpi_avg_year_svy_hces_nat = . if year_svy_hces==""

	sum cpi_avg_year_svy_hces_nat if state==38 & scode==7000 & year_svy_hces=="2022-23"			
 
   sum cpi_avg_year_svy_hces_nat if state==38 & scode==7000 & year_svy_hces=="2023-24"