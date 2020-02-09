library(dplyr)
library(data.table)

# pain and drugs tables

create_frequency_table <- function(drugs, pain){
  id <- intersect(select(drugs, 'ID'), select(pain, 'ID'))
  pain_loc <- left_join(id, pain, by='ID')
  drugs_loc <- left_join(id, drugs, by='ID')
  pain_loc <- pain_loc[,c(1:11)]
  pain_loc[is.na(pain_loc)] <- 0
  
  D <- data.matrix(drugs_loc[,c(2:length(drugs_loc[1,]))])
  P <- data.matrix(pain_loc[,c(2:length(pain_loc[1,]))])
  R <- t(D) %*% P
  R <- as.data.table(R, keep.rownames = TRUE)
  R
}

##############################################################################################3

pain_drug <- readRDS("C:\\MY_FOLDERS\\Asthma_and_Pain\\Input_Data\\UKB_pain_drug.RDS")

# extracted from the table f.eid | f.6159.0.0 | ... | f.6159.2.6| ...
# resutl is binary 0, 1 and NA table t_pain = f.eid | Pain_St0_v0 | Pain_St1_v0 | ... 
pain <- t_pain
names(pain)[1] <- 'ID'

drugs <- pain_drug[,c(1,c(14:length(pain_drug[1,])))]

# input: drugs = ID  |  Drug1  |  Drug2  |  .... | Drug_n  |
# contains only 1 or 0
# input:  pain = ID  |  Pain Site 1 visit 0  |  Pain Site 2 visit 0  |  .... | Pain site 10 visit 2  |
# contains 1, 0 or NA

freq_drugs_vs_pain <- create_frequency_table(drugs, pain)
