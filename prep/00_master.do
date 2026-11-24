*===============================================================================
* prep/00_master.do : India S2S data preparation (Stata)
*
*   P:\...\WORKINGDATA\IND   (MoSPI survey files)  ─┐
*   P:\...\S2S\IND\inputs    (PRIMUS, CPI, weights) ─┼─► 00_master.do ─► P:\...\S2S\IND\harmonized\<FY2022|...>\ ─► RUN_S2S.R
*   prep/hces, prep/plfs     (the do-files)         ─┘
*
* HOW TO RUN
*   1. Your paths: section 1 (one block per user, like RUN_S2S.R).
*   2. Choose the exercise and the test switch: section 2.
*   3. Open this file in Stata and click "Do" (or Ctrl+D). It runs everything.
*   It can also be run from RUN_S2S.R (run_prep <- TRUE), which passes
*   the exercise, s2s_root and the repo path as arguments.
*
* Status
*   fy2022_backcast : validated (Nov 2026): reproduces the note's harmonized files exactly
*   fy2023          : ready to test (HCES 2023-24 → PLFS 2023-24, Stage 1)
*   cy2023_forward  : not yet
*===============================================================================

args arg_exercise arg_root arg_repo      // only used when called from RUN_S2S.R

clear all
set more off

*-------------------------------------------------------------------------------
* 1. Paths (one block per user)
*-------------------------------------------------------------------------------
local user = lower(c(username))

if "`user'" == "wb587758" {                                    // Diego
	glo repo        "C:\Users\wb587758\WBG_DEV\Github\India_S2S"
	glo rootdatalib "P:\SARMD\SARDATABANK\WORKINGDATA"
	glo s2s_root    "P:\SARMD\SARDATABANK\S2S\IND"
}
else if "`user'" == "wb553773" {
	glo repo        "C:\Users\wb553773\GitHub\India_S2S"      // <- check
	glo rootdatalib "P:\SARMD\SARDATABANK\WORKINGDATA"
	glo s2s_root    "P:\SARMD\SARDATABANK\S2S\IND"
}
else {
	di as error "User `user' has no block in section 1 of prep/00_master.do: add yours."
	exit 198
}
glo rootsarmd  "${rootdatalib}"
glo rootsarlab "${rootdatalib}"

*-------------------------------------------------------------------------------
* 2. Exercise and test switch
*-------------------------------------------------------------------------------
local exercise "fy2022_backcast"   // fy2022_backcast | fy2023 | cy2023_forward
local test     1                   // 1 = write to harmonized\<FOLDER>_test\ (compare first with 99_compare.do)
                                   // 0 = write to harmonized\<FOLDER>\ (the folder RUN_S2S.R reads)
local run_hces  1                   // 0 = skip the HCES step (its outputs must already be in the output folder)
local plfs_from 2017                // first PLFS year to run (to restart after an error)

if "`arg_exercise'" != "" local exercise "`arg_exercise'"
if "`arg_root'"     != "" glo s2s_root   "`arg_root'"
if "`arg_repo'"     != "" glo repo       "`arg_repo'"

*-------------------------------------------------------------------------------
* 3. Parameters per exercise
*-------------------------------------------------------------------------------
if "`exercise'" == "fy2022_backcast" {
	glo harm_folder "FY2022"
	glo anchor_cpi  177.7042                    // average CPI over the HCES 2022-23 months (Aug 2022 - Jul 2023)
	glo spdef_file  "Sp_deflators_HCES_22.dta"   // built by the HCES step, used by every PLFS step
	local hces_raw  "${rootdatalib}\IND\IND_2022_HCES\IND_2022_HCES_v04_M\Data\Stata"
	local hces_m    "IND_2022-23_HCES_v02_M.do"
	local hces_s2s  "IND_2022-23_HCES_v02_M_v02_A_s2s_HCES_to_PLFS.do"
	local hces_lvl1 "level01_2022.dta"
	local primus    "IND_PRIMUS_2022-23.dta"
	local plfs_years 2017 2018 2019 2020 2021 2022   // Stage 2 files (PLFS_to_PLFS)
	local plfs_dta_years 2020 2021               // official collection has .dta (SARRAW), not the MoSPI text files
	local s1_year   2022                         // Stage 1 receiver (HCES_to_PLFS)
	local s1_do     "IND_2022_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.do"
	local extra_inputs `""PLFS 22 23 new weights.dta""'   // corrected Assam weights (2022-23 only)
}
else if "`exercise'" == "fy2023" {
	glo harm_folder "FY2023"
	glo anchor_cpi  186.7333                    // average CPI over the HCES 2023-24 months (Aug 2023 - Jul 2024)
	glo spdef_file  "Sp_deflators_HCES_23.dta"
	local hces_raw  "${rootdatalib}\IND\IND_2023_HCES\IND_2023_HCES_v01_M\Data\Original"   // <- MoSPI levels: to confirm
	local hces_m    "IND_2023-24_HCES_v01_M.do"
	local hces_s2s  "IND_2023-24_HCES_v01_M_v01_A_s2s_HCES_to_PLFS.do"
	local hces_lvl1 "level01_2023.dta"
	local primus    "IND_PRIMUS_2023-24_v2.dta"
	local plfs_years 2023                        // only for the Stage 1 figures (05_stage1_outputs.R); no Stage 2
	local plfs_dta_years
	local s1_year   2023
	local s1_do     "IND_2023_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.do"
	local extra_inputs
}
else if "`exercise'" == "cy2023_forward" {
	di as error "Exercise `exercise' is not in the master yet (next steps)."
	exit 198
}
else {
	di as error "Unknown exercise: `exercise'"
	exit 198
}

*-------------------------------------------------------------------------------
* 4. Folders and checks
*-------------------------------------------------------------------------------
glo prep       "${repo}\prep"
glo s2s_inputs "${s2s_root}\inputs"
glo harmonized "${s2s_root}\harmonized\\${harm_folder}"
if `test' == 1 glo harmonized "${harmonized}_test"
glo work       "${harmonized}\_work"            // intermediate files (survey levels, M files)

cap mkdir "${harmonized}"
cap mkdir "${work}"

cap program drop need_dir
program define need_dir
	args p
	mata: st_local("ok", strofreal(direxists("`p'")))
	if `ok' == 0 {
		di as error "Folder not found: `p'"
		exit 601
	}
end
cap program drop need_file
program define need_file
	args p
	cap confirm file "`p'"
	if _rc {
		di as error "File not found: `p'"
		exit 601
	}
end

need_dir  "${prep}"
need_dir  "${s2s_inputs}"
need_dir  "${work}"
need_dir  "`hces_raw'"
need_file "`hces_raw'\\`hces_lvl1'"
need_file "${s2s_inputs}\\`primus'"
need_file "${s2s_inputs}\IND_allyears_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS_cpi.dta"
foreach f of local extra_inputs {
	need_file "${s2s_inputs}\\`f'"
}
foreach y of local plfs_years {
	need_dir "${rootdatalib}\IND\IND_`y'_PLFS\IND_`y'_PLFS_v01_M\Data\Original"
}
foreach y of local plfs_dta_years {
	local sy = "`y'-" + string(mod(`y' + 1, 100))
	need_file "${rootdatalib}\IND\IND_`y'_PLFS\IND_`y'_PLFS_v01_M\Data\Original\hhvisit1_`sy'.dta"
	need_file "${rootdatalib}\IND\IND_`y'_PLFS\IND_`y'_PLFS_v01_M\Data\Original\personvisit1_`sy'.dta"
}

cap log close prep
log using "${work}\00_master_`exercise'.log", replace text name(prep)

local t0 = c(current_time)
di as result "Exercise:   `exercise'"
di as result "User:       `user'"
di as result "Repo:       ${repo}"
di as result "Raw data:   ${rootdatalib}"
di as result "Inputs:     ${s2s_inputs}"
di as result "Harmonized: ${harmonized}"
di as result "Anchor CPI: ${anchor_cpi}"

*===============================================================================
* 5. Run (the same steps for every exercise; files and parameters from section 3)
*===============================================================================

*--- 5.1 HCES: survey levels ─► M file ─► HCES<yy>_s2s.dta + spatial deflators
if `run_hces' {
	glo input  "`hces_raw'"
	glo output "${work}"
	do "${prep}\hces\\`hces_m'"

	glo input  "${work}"
	glo output "${harmonized}"
	do "${prep}\hces\\`hces_s2s'"
}
else {
	di as text "HCES step skipped (run_hces = 0): using ${harmonized}\\${spdef_file}"
	need_file "${harmonized}/${spdef_file}"
}

*--- 5.2 PLFS: text files (or official .dta) ─► M file ─► IND_<y>_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.dta
foreach y of local plfs_years {
	if `y' < `plfs_from' continue
	glo input  "${rootdatalib}\IND\IND_`y'_PLFS\IND_`y'_PLFS_v01_M\Data\Original"
	glo output "${work}"
	local src : list y in plfs_dta_years
	if `src' do "${prep}\plfs\IND_`y'_PLFS-V1_v01_M_dta.do"     // official .dta
	else     do "${prep}\plfs\IND_`y'_PLFS-V1_v01_M.do"         // MoSPI text files

	glo input  "${work}"
	glo output "${harmonized}"
	do "${prep}\plfs\IND_`y'_PLFS_v01_M_v01_A_s2s_PLFS_to_PLFS.do"
	di as result "PLFS `y' done"
}

*--- 5.3 PLFS anchor round, Stage 1 receiver ─► IND_<y>_PLFS_..._HCES_to_PLFS.dta (uses the M file of 5.2)
glo input  "${work}"
glo output "${harmonized}"
do "${prep}\plfs\\`s1_do'"

*-------------------------------------------------------------------------------
* 6. Done
*-------------------------------------------------------------------------------
di as result _n "Data preparation finished: `exercise' (started `t0', ended `c(current_time)')"
di as result "Files in ${harmonized}:"
local files : dir "${harmonized}" files "*.dta"
foreach f of local files {
	di as text "  `f'"
}
if `test' == 1 di as result _n "Test run: now run prep/99_compare.do to compare with harmonized\\${harm_folder}"
log close prep
