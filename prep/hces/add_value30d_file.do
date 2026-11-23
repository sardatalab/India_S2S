

use "C:\Users\wb587758\WBG\ESAPV Files - DATA\INTPOV\2023-24\reproducibility package\data\processed\ces\mpce_2023.dta", clear
keep hhid sector state /*district*/ stratum hhsize mpce_nom
gen value_30d= mpce_nom*hhsize


save "C:\Users\wb587758\OneDrive - WBG\IND\S2S\2023-24\Data\Stage 1\Raw\IND_2023-24_HCES\IND_2023-24_HCES_v01_M\Data\Original\2023_mpce_hh.dta", replace