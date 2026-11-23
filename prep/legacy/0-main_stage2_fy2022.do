**===S2S Database preparation Stage 2 ====================================
*Author:        SAR Data and Stats Team
*Last update:	2/10/26
*----------------------------------------------------------------------
*====================================================================
clear all
global path         "C:/Users/wb587758/OneDrive - WBG/IND/S2S/2022-23/Data/Stage 2"
global rootdatalib  "${path}\Raw"


*****************************
******** EUS 2011 ***********
*****************************
local code          "IND"
local survey        "EUS"
local vm            "01"
local year          2011
local yearfolder    "`code'_`year'_`survey'"

*di in red `yearfolder'
global input  "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Original"
global output "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Stata"


* Run preparation do-file

do "${rootdatalib}\1-preparing data\IND_2011_EUS_v01_M.do"

* Clear input/output globals and reset
global input ""
global output ""
global input        "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Stata"
global output       "${path}\\Cleaned"
    
* Run harmonization do-file
do "${rootdatalib}\2-harmonizing variables\IND_`year'_EUS_v`vm'_M_v`vm'_A_s2s_EUS_to_NSS.do"

global input ""
global output ""
macro drop yearfolder code vm year
/*
**********************************
******** PLFS 2017 - 2022 ********
**********************************

* Use this part to run only one year**************

local year 2023
local code          "IND"
local survey        "PLFS"
local vm            "01"
local yearfolder    "`code'_`year'_`survey'"
global input        "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Original"
global output       "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\Stata"
do "${rootdatalib}\1-preparing data\IND_`year'_PLFS-V1_v`vm'_M.do"
global input ""
global output ""
global input        "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\Stata"
global output       "${path}\\Cleaned"
do "${rootdatalib}\2-harmonizing variables\IND_`year'_PLFS_v`vm'_M_v`vm'_A_s2s_PLFS_to_PLFS.do"
***************************************************
*/
local survey  "PLFS"
* Loop over years 2017 to 2022
forvalues year = 2017/2022 {
    
    local yearfolder    "`code'_`year'_`survey'"
    global input        "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Original"
    global output       "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\Stata"
    
    * Run preparation do-file
    cap do "${rootdatalib}\1-preparing data\IND_`year'_PLFS-V1_v`vm'_M.do"
    if _rc != 0 {
        display as error "Error processing survey `survey' year `year' in preparation step"
    }
    
    * Clear input/output globals and reset
    global input ""
    global output ""
    global input        "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\Stata"
    global output       "${path}\\Cleaned"
    
    * Run harmonization do-file
    cap do "${rootdatalib}\2-harmonizing variables\IND_`year'_PLFS_v`vm'_M_v`vm'_A_s2s_PLFS_to_PLFS.do"
    if _rc != 0 {
        display as error "Error processing survey `survey' year `year' in harmonization step"
    }
    
    display as result "Completed processing for year `year'"
}

display as result "All years processed successfully"