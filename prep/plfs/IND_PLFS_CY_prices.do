*===============================================================================
* IND_PLFS_CY_prices.do : PLFS calendar-year rounds to CY2023 prices, and spatial adjustment
* Called by the CY PLFS do-files (Stage 1 receiver and Stage 2 files), on the person-level data.
* Replaces Leo's commented block (harm_plfs.dta, year==7, 177.792, Sp_deflators_HCES_22).
*
*   inputs\IND_CYyears_PLFS_V1_final_v01_M_cpi.dta: CPI by state × sector × month, 2023-2025
*      (the household CPI of the PLFS: by hhid and year; by state x sector x month where the hhid
*       does not match, e.g. 2025, whose hhid are written differently)
*   ─► incomes and consumption × ${anchor_cpi} / cpi_monthly
*   ${harmonized}/${spdef_file} (HCES CY2023, state × sector) ─► *_adj (as in FY)
*
* Arguments: 1 = weight variable, 2 = urban dummy variable, 3... = variables to deflate
* Globals: ${s2s_inputs}, ${anchor_cpi}, ${harmonized}, ${spdef_file}
*===============================================================================
args w u
local defl : list 0 - w
local defl : list defl - u

*--- Temporal: CPI of the household (by hhid and year; if not found, by year × month × state × sector)
preserve
use hhid year month state sector cpi_monthly using "${s2s_inputs}\IND_CYyears_PLFS_V1_final_v01_M_cpi.dta", clear
tempfile cpiall
save `cpiall'
* (a) household CPI: one value per household and year
bys hhid year: keep if _n == 1
keep hhid year cpi_monthly
rename cpi_monthly cpi_hh
tempfile cpih
save `cpih'
* (b) cell CPI
use `cpiall', clear
collapse (sd) cpi_sd = cpi_monthly (mean) cpi_cell = cpi_monthly, by(year month state sector)
qui sum cpi_sd
di as text "CPI within year x month x state x sector cells: max sd = " %9.6f r(max) "  (0 = one CPI per cell)"
drop cpi_sd
gen `u' = sector - 1
drop sector
tempfile cpic
save `cpic'
restore

cap drop cpi_monthly cpi_hh cpi_cell
cap drop _merge
merge m:1 hhid year using `cpih', keep(master match) nogen
merge m:1 year month state `u' using `cpic', keep(master match) nogen
gen double cpi_monthly = cond(!missing(cpi_hh), cpi_hh, cpi_cell)
qui count if !missing(cpi_hh)
di as text "CPI: `r(N)' persons by hhid, " _c
qui count if missing(cpi_hh) & !missing(cpi_cell)
di as text "`r(N)' by state x sector x month"
qui count if !missing(cpi_hh) & !missing(cpi_cell) & abs(cpi_hh - cpi_cell) > 1e-6
di as text "  persons whose household CPI differs from the cell CPI: `r(N)'"
count if missing(cpi_monthly)
if r(N) > 0 {
	di as error "`r(N)' persons without CPI. Their year, month, state and sector:"
	tab month `u' if missing(cpi_monthly)
	tab state if missing(cpi_monthly)
	exit 459
}
drop cpi_hh cpi_cell
foreach v of local defl {
	replace `v' = `v' * ${anchor_cpi} / cpi_monthly
}
notes: Prices: CY2023 average (${anchor_cpi}); monthly CPI by state x sector (inputs\IND_CYyears_PLFS_V1_final_v01_M_cpi.dta)

*--- Spatial: HCES CY2023 deflators, normalized as in FY
preserve
use "${harmonized}/${spdef_file}", clear
rename urb `u'
tempfile spdef
save `spdef'
restore
cap drop spdef
merge m:1 state `u' using `spdef', keep(master match)
count if _merge == 1
if r(N) > 0 {
	di as error "`r(N)' persons without spatial deflator (state, sector)"
	exit 459
}
drop _merge
