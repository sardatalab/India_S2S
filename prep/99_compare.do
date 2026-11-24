*===============================================================================
* prep/99_compare.do : compare a test run of 00_master.do with the files R uses
*
*   harmonized\FY2022_test\*.dta  (new, from 00_master.do with test = 1)
*   harmonized\FY2022\*.dta       (old, the files copied by hand, used for the note)
*        └──────────────┬──────────────┘
*                       ▼
*   harmonized\FY2022_test\_work\compare_FY2022.csv  + summary in the Results window
*
* For every .dta in both folders: number of observations, variables only in one
* file, and per variable the number of households that differ
* (numeric: relative difference > 1e-6, or missing in only one of them).
* Households are matched on hhid (state and urb for the spatial deflators).
*
* HOW TO RUN: set the folder in section 1, open in Stata, click "Do".
* Paths: the same user block as 00_master.do.
*===============================================================================
clear all
set more off

*--- 1. Paths and folder to compare
local user = lower(c(username))
if "`user'" == "wb587758" glo s2s_root "P:\SARMD\SARDATABANK\S2S\IND"
else if "`user'" == "wb553773" glo s2s_root "P:\SARMD\SARDATABANK\S2S\IND"
else {
	di as error "User `user' has no block in prep/99_compare.do: add yours."
	exit 198
}
local folder "FY2022"

local new "${s2s_root}\harmonized\\`folder'_test"
local old "${s2s_root}\harmonized\\`folder'"
local out "`new'\_work\compare_`folder'.csv"

*--- 2. One file
cap program drop compare_one
program define compare_one
	args file newdir olddir postname

	* key
	local key "hhid"
	if strpos("`file'", "Sp_deflators") local key "state urb"

	* old file: rename everything except the key to __o1, __o2, ...
	use "`olddir'\\`file'", clear
	local N_old = _N
	qui ds `key', not
	local vars `r(varlist)'
	local i 0
	foreach v of local vars {
		local ++i
		rename `v' __o`i'
	}
	cap isid `key'
	if _rc {
		di as error "`file': `key' does not identify the observations: compared in sort order"
		sort `key', stable
		gen long __seq = _n
		local key "__seq"
	}
	tempfile o
	save `o'

	use "`newdir'\\`file'", clear
	local N_new = _N
	qui ds
	local vars_new `r(varlist)'
	if "`key'" == "__seq" {
		sort hhid, stable
		gen long __seq = _n
	}
	merge 1:1 `key' using `o'
	qui count if _merge == 1
	local only_new = r(N)
	qui count if _merge == 2
	local only_old = r(N)
	post `postname' ("`file'") ("_N") (`N_new') (`N_old') (`only_new' + `only_old') (.)
	keep if _merge == 3

	local i 0
	local ndiffvars 0
	foreach v of local vars {
		local ++i
		cap confirm variable `v', exact
		if _rc {
			post `postname' ("`file'") ("`v'") (.) (.) (.) (.)    // only in old
			local ++ndiffvars
			continue
		}
		cap confirm numeric variable `v', exact
		local num_new = (_rc == 0)
		cap confirm numeric variable __o`i', exact
		local num_old = (_rc == 0)
		if `num_new' & `num_old' {
			qui gen double __d = abs(`v' - __o`i')
			qui count if (__d > 1e-6 * max(1, abs(__o`i')) & !missing(__d)) | (missing(`v') != missing(__o`i'))
			local nd = r(N)
			qui sum __d
			local mx = cond(r(N) > 0, r(max), 0)
			drop __d
		}
		else if !`num_new' & !`num_old' {
			qui count if `v' != __o`i'
			local nd = r(N)
			local mx = .
		}
		else {
			local nd = _N        // numeric in one file, string in the other
			local mx = .
		}
		if `nd' > 0 local ++ndiffvars
		post `postname' ("`file'") ("`v'") (.) (.) (`nd') (`mx')
	}
	* variables only in the new file
	foreach v of local vars_new {
		if "`v'" == "__seq" continue
		local found 0
		foreach w of local vars {
			if "`v'" == "`w'" local found 1
		}
		foreach k in hhid state urb {
			if "`v'" == "`k'" local found 1
		}
		if !`found' {
			post `postname' ("`file'") ("`v' (only in new)") (.) (.) (.) (.)
			local ++ndiffvars
		}
	}

	if `N_new' == `N_old' & `only_new' + `only_old' == 0 & `ndiffvars' == 0 {
		di as result "IDENTICAL   `file'  (N = `N_new')"
	}
	else {
		di as error  "DIFFERENT   `file'  N new/old = `N_new'/`N_old', unmatched = `=`only_new'+`only_old'', variables that differ = `ndiffvars'"
	}
end

*--- 3. All files
tempname P
tempfile res
postfile `P' str60 file str60 variable N_new N_old n_diff max_abs_diff using `res'

local files : dir "`new'" files "*.dta"
foreach f of local files {
	cap confirm file "`old'\\`f'"
	if _rc {
		di as text "NEW ONLY    `f'  (not in `old')"
		continue
	}
	compare_one "`f'" "`new'" "`old'" `P'
}
* spatial deflators: also against the reference copy in inputs\
cap confirm file "${s2s_root}\inputs\Sp_deflators_HCES_22.dta"
if !_rc & "`folder'" == "FY2022" {
	di as text "(Sp_deflators_HCES_22.dta against inputs\)"
	compare_one "Sp_deflators_HCES_22.dta" "`new'" "${s2s_root}\inputs" `P'
}
postclose `P'

use `res', clear
export delimited using "`out'", replace
di as result _n "Details: `out'"
list file variable n_diff max_abs_diff if n_diff > 0, sep(0) noobs abbreviate(30)
