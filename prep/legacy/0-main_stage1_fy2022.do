**===S2S Database preparation Stage 1 ====================================
*Author:        SAR Data and Stats Team
*Last update:	2/10/26
*----------------------------------------------------------------------
*====================================================================
clear all
global path         "C:/Users/wb587758/OneDrive - WBG/IND/S2S/2022-23/Data"
global rootdatalib  "${path}\Stage 1\Raw"


*=== HCES 2022===============================================================*
	
local code="IND"    // country code
local survey        "HCES"
local vm            "02"
local year          2022
local svyear		"2022-23"
local yearfolder    "`code'_`svyear'_`survey'"

*di in red `yearfolder'
global input  "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Original"
global output "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Stata"

* Run preparation do-file

do "${rootdatalib}\IND_2022-23_HCES_v02_M.do"

* Clear input/output globals and reset
global input ""
global output ""
global input        "${rootdatalib}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Stata"
global output       "${path}\\Stage 1\\Cleaned"
    
* Run harmonization do-file
do "${rootdatalib}\IND_2022-23_HCES_v02_M_v02_A_s2s_HCES_to_PLFS.do"

global input ""
global output ""
global rootdatalib2  "${path}\Stage 2\Raw"
macro drop yearfolder year code vm year	
	

*=== PLFS 2022===============================================================*

local year          2022
local code          "IND"
local survey        "PLFS"
local vm            "01"
local yearfolder    "`code'_`year'_`survey'"
global input        "${rootdatalib2}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\\Original"
global output       "${rootdatalib2}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\Stata"
do "${rootdatalib2}\1-preparing data\IND_`year'_PLFS-V1_v`vm'_M.do"
global input ""
global output ""
global input        "${rootdatalib2}\\`yearfolder'\\`yearfolder'_v`vm'_M\\Data\Stata"
global output       "${path}\\Stage 1\\Cleaned"
do "${rootdatalib}\IND_2022_PLFS_v01_M_v02_A_s2s_HCES_to_PLFS.do"