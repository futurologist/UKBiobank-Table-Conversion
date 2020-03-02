# UKBiobank-Table-Conversion
Conversion of data-tables extracted from the UKBiobank data-files into new regression-friendly data tables  

The current R code, containing functions and scripts, is designed to work specifically for the data fields: 

Data-Field 20002 
Description:	Non-cancer illness code, self-reported
Category:	Non-cancer illness code, self-reported <- Medical conditions <- Verbal interview <- UK Biobank Assessment Centre

Data-Field 20002 
Description:	Non-cancer illness code, self-reported
Category:	Non-cancer illness code, self-reported <- Medical conditions <- Verbal interview <- UK Biobank Assessment Centre

Data-Field 6159
Description:	Pain type(s) experienced in last month
Category:	Pain - Health and medical history - Touchscreen - UK Biobank Assessment Centre

as well as for some of the additional demographic and geontypic infromation

The data available in an original UKBiobank file is presented in a way that allows for space to be saved (fiugratively speaking it is in a somewhat more compressed format). This data, however, is not presented in a format suitable for data analysis (e.g. regressions), which may requires binary column representation of variables of cathegorical nature or numerical column representation of continuous variables.
     
The R code provided performs this conversion.   

How to Use:
-----------------------------------------------------------------------------------------------
Step 1: Use script "First_from_UKB.R" to extract a raw subtable from the big raw UKB data file;

The scripts discussed below, import and use the file of R functions "Set\_of\_functions.R". Make sure to download this file.

Step 2: After the raw subtatble is being extracted:
   2.1. For field 20002 and related: Use "Table_20002.R"
   2.2. For field 6159 and related: Use "Table_6159.R"
   2.3. For fields of demographic data (e.g. Age, Gender, Principal Components, etc): Use "Table_demogr_data.R"

Step 3: Further conversions:
   3.1. For a medical condition from field 20002 together with categorization for Age of Onset: Use "Table_condition_age_grp.R"
   3.2. For cross-tabulation of two tables: Use "Cross\_Tabulation.R"
   3.3. For Chi-Squared test of each pair of variables from two respective lists of variables from two respective tables: Use "Table_chi_squared.R" 
