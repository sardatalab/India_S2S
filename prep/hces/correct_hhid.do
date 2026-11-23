
clear all
 use "C:\Users\wb587758\OneDrive - WBG\IND\S2S\2023-24\Data\Stage 1\Cleaned\HCES23_s2s.dta", clear
 

 
 gen FSU_Serial_No = substr(hhid, 1, 5)
 gen Second_Stage_Stratum_No = substr(hhid, 6, 1)
 gen Sample_Household_No =  substr(hhid, 7, 2)
 
 destring FSU_Serial_No, gen(fsu)
 destring Second_Stage_Stratum_No, gen(sss)
 destring Sample_Household_No, gen(hh_number)
 
 
 
  gen hhid_v2 = string(fsu) + string(sss) + string(hh_number)
  
  drop hhid
  destring(hhid_v2), gen(hhid)
  tostring(hhid), replace
  
  drop FSU_Serial_No fsu Second_Stage_Stratum_No sss Sample_Household_No hh_number hhid_v2
  
  drop mpce_sp_def_ind
  
  *Check with GMD
 * tostring(hhid), replace
  
 * merge 1:m hhid using "C:\Users\wb587758\WBG\ESAPV Files - DATA\GMD\IND_2023_HCES_v01_M_v01_A_GMD_IND.dta"
 
 merge 1:1 hhid using "C:\Users\wb587758\WBG\ESAPV Files - DATA\INTPOV\2023-24\reproducibility package\data\processed\ces\mpce_2023_all.dta", keepusing(mpce_sp_def_ind)
 
 drop _merge
  save "C:\Users\wb587758\OneDrive - WBG\IND\S2S\2023-24\Data\Stage 1\Cleaned\HCES23_s2s_v2.dta", replace
 