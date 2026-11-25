*===============================================================================
* IND_2023_HCES_CY_s2s_from_FY.do : HCES calendar year 2023, from the validated FY files
* Called by prep/00_master.do (exercise cy2023_forward). Diego decided this route (Nov 2026)
* instead of the SARMD files: the HCES 2023-24 SARMD file on P: has no food/services/
* clothing/durables components, and this keeps the covariates identical to FY2022 / FY2023.
*
*   harmonized\FY2022\HCES22_s2s.dta  + panel (FY2022\_work M file) ─► panels 6-10 (listed Jan-May 2023)
*   harmonized\FY2023\HCES23_s2s.dta  + Panel (FY2023\_work M file) ─► panels 1-5  (listed Aug-Dec 2023)
*        welfare to CY2023 prices: × ${anchor_cpi} / round CPI (177.7042 | 186.7333)
*        weights: × PLFS CY2023 population / HCES CY sample population, by state × sector
*   PRIMUS 2022-23 + 2023-24 v2 (nominal and real welfare) ─► Sp_deflators_HCES_CY23.dta
*   ─► ${output}\HCES23CY_s2s.dta
*
* Globals from the master: ${fy22} ${fy23} (harmonized FY folders), ${anchor_cpi},
* ${cpi_fy22} ${cpi_fy23}, ${s2s_inputs}, ${plfs_cy23} (SARLAB PLFS CY2023 file), ${output}
*===============================================================================

*--- 1. Households of each round in CY2023 (panel = 3-month window, listed in its first month)
use hhid panel using "${fy22}\_work\IND_2022-23_HCES_v02_M.dta", clear
bys hhid: keep if _n == 1
destring panel, replace
keep if inrange(panel, 6, 10)                    // Jan-Mar 2023 ... May-Jul 2023
gen month = panel - 5                            // listing month 1 ... 5
tempfile p22
save `p22'

use hhid Panel using "${fy23}\_work\IND_2023-24_HCES_v02_M.dta", clear
bys hhid: keep if _n == 1
rename Panel panel
destring panel, replace
keep if inrange(panel, 1, 5)                     // Aug-Oct 2023 ... Dec 2023-Feb 2024
gen month = panel + 7                            // listing month 8 ... 12
tempfile p23
save `p23'

*--- 2. FY s2s files, CY households only, welfare in CY2023 prices
use "${fy22}\HCES22_s2s.dta", clear
merge 1:1 hhid using `p22', keep(match) nogen
gen round = 2022
gen double price_cy = ${anchor_cpi} / ${cpi_fy22}
tempfile h22
save `h22'

use "${fy23}\HCES23_s2s.dta", clear
merge 1:1 hhid using `p23', keep(match) nogen
gen round = 2023
gen double price_cy = ${anchor_cpi} / ${cpi_fy23}

append using `h22'
isid hhid                                        // checked: no hhid in both rounds
replace mpce_sp_def_ind = mpce_sp_def_ind * price_cy
notes mpce_sp_def_ind: CY2023 prices: round welfare x ${anchor_cpi} / round CPI (${cpi_fy22} | ${cpi_fy23})
replace year = 2023
tab round month

* state from the dummies (state 26 does not exist)
gen state = 0
forvalues s = 1/37 {
	cap confirm variable state_`s'
	if !_rc replace state = `s' if state_`s' == 1
}
assert state > 0

*--- 3. Weights: the two half-rounds to the PLFS CY2023 population, by state × sector
preserve
use state sector hhwt using "${plfs_cy23}", clear
gen urb = sector - 1
collapse (sum) pop_plfs = hhwt, by(state urb)
tempfile plfs
save `plfs'
restore
preserve
collapse (sum) pop_hces = pop_wgt, by(state urb)
merge 1:1 state urb using `plfs', keep(master match)
count if _merge == 1
if r(N) > 0 di as error "`r(N)' state x sector cells of the HCES are not in PLFS CY2023: factor 1"
gen double wfactor = cond(_merge == 3, pop_plfs / pop_hces, 1)
keep state urb wfactor pop_hces pop_plfs
list, sep(0) noobs
tempfile wf
save `wf'
restore
merge m:1 state urb using `wf', keepusing(wfactor) nogen
replace pop_wgt = pop_wgt * wfactor
notes pop_wgt: x wfactor = PLFS CY2023 population / HCES CY sample population (state x sector)

*--- 4. Spatial deflators CY2023 (nominal / real welfare in CY2023 prices, by state × sector)
preserve
keep hhid round price_cy pop_wgt state urb
tempfile cyh
save `cyh'
use hhid welfarenom_final welfaredef_final using "${s2s_inputs}\IND_PRIMUS_2022-23.dta", clear
gen round = 2022
tempfile pr22
save `pr22'
use hhid welfarenom_final welfaredef_final using "${s2s_inputs}\IND_PRIMUS_2023-24_v2.dta", clear
gen round = 2023
append using `pr22'
merge 1:1 hhid round using `cyh', keep(match) nogen
gen double spdef = welfarenom_final / (welfaredef_final * price_cy)
collapse (mean) spdef [aw=pop_wgt], by(state urb)
save "${output}\Sp_deflators_HCES_CY23.dta", replace
restore

*--- 5. Save
drop price_cy
order hhid year round panel month
save "${output}\HCES23CY_s2s.dta", replace
di as result "HCES CY2023: `=_N' households"
