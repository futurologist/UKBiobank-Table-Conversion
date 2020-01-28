# UKBiobank-Table-Conversion
Conversion of data-tables extracted from the UKBiobank data-files into new regression-friendly data tables  

The current R code, containing functions and scripts, is designed to work specifically for the data field: 

Data-Field 20002 
Description:	Non-cancer illness code, self-reported
Category:	Non-cancer illness code, self-reported <- Medical conditions <- Verbal interview <- UK Biobank Assessment Centre


The data available in an original UKBiobank file is presented in a way that allows for space to be saved (fiugratively speaking it is in a somewhat more compressed format). This data, however, is not presented in a format suitable for data analysis (e.g. regressions), which may requires binary column representation of variables of cathegorical nature or numerical column representation of continuous variables.

For example, after preliminary direct extraction (we call it phase 1) of a sub-tble, that contains all fields with code 20002 from an original UKBiobank data-file, the resulting trable may have the format:

Say we are interested in self-reported conditions and the ages of onset recorded during the three separate visits to the Data Center. The sub-table of the original UKB data-table, will look something like this:

| ID  | NumCondit x 3 | (Condition0 | Condition1 | ...| Condit28) x3	|  AgeOfCond0 x3 |	... | AgeOfCond28 x3 |
| 1   | 3 fields      |     29 X 3 fields                             |  29 x 3 fields                         |

|f.eid| f.135.visit.0 |    f.20002.visit.count                        |  f.20009.visit.count                   | 
(the latter are headers from the original table)


(Phase 2:) If we are interested in whether the participants in the UKB, during at least one of the three visits, daclared having Asthma, Hay Fever/Rhinitis or Eczema, as well as we want to capture the corresponding recorded age of onset, we need to find (from the website: http://biobank.ctsu.ox.ac.uk/showcase/index.cgi) the codes of these three conditions:
Asthma  Hayfev/Rhinitis  Eczema
1111    1387             1452
and use them to creat a table of the format:

ID    |  Asthma | Hayfev/Rhinitis |  Eczema  |  age_Asthma_v0  |  age_Asthma_v1 | age_Asthma_v2 | age_Hayf_Rhin_v0 | age_Hayf_Rhin_v1 | 
ID_num| 0 or 1  |   0 or 1        |  0 or 1  |  age            |      age       |     age       |     age          |     age          | 


age_Hayf_Rhin_v2 | age_Ecz_v0 | age_Ecz_v1 | age_Ecz_v2 |
     age         | age        | age        | age        |
     
The R code privede performs this conversion. It also has the potential to add extra,  auxiliary variables which do not need to be converted but included in their original format to the data table.        
