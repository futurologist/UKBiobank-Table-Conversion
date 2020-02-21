library(dplyr)
library(data.table)

# THIS SCRIPT USES THE REUSLTS FROM THE SCRIPTS 'Table_condition_age_grp.R' and 'Table_6159.R',
# OR ALTERNATIEVLY, SOME OTHER SCRIPTS OF YOURS, 
# SO YOU MAY HAVE TO RUN THESE SCRIPTS FIRST.

create_frequency_table <- function(pain, conditions){
  id <- intersect(select(pain, 'ID'), select(conditions, 'ID'))
  pain_loc <- left_join(id, pain, by='ID')
  conditions_loc <- left_join(id, conditions, by='ID')
  pain_loc <- pain_loc[,(2:length(pain[1,]))]
  conditions_loc <- conditions_loc[,(2:length(conditions_loc[1,]))]
  pain_loc[is.na(pain_loc)] <- 0
  conditions_loc[is.na( conditions_loc)] <- 0
  
  D <- data.matrix(pain_loc)
  P <- data.matrix(conditions_loc)
  R <- t(D) %*% P
  R <- as.data.table(R, keep.rownames = TRUE)
  return(R)
}

###################################################################################################

################################# PROVIDE OUPUT LOCATION ##########################################

filepath_out <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Output_data_ph2\\cross_table_pain_vs_cond.txt"

###################################################################################################

conditions <- t_cond_agegrp_bin
pain <- t_pain

frq_pain_vs_cond <- create_frequency_table(pain, conditions)

write.table(frq_pain_vs_cond,
            filepath_out, 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

###################################################################################################
