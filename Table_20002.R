source("C:\\MY_FOLDERS\\Asthma_and_Pain\\R_code\\Table_creation\\Set_of_functions.R")

##############################  INPUT FOR CONDITIONS ###############################################
filepath20002_in <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Output_data_ph1\\ukb20002.txt"

list_of_conditions <- c(1111, 1387, 1452)

list_of_labels <- c('Asthma', 'Hayf_Rhin', 'Eczema')

filepath20002_out <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Output_data_ph2\\asth_rhin_ecz.txt" 
####################################################################################################


############################## NOT TO BE CHANGED ###################################################
n_visits <- 3
l_array <- 29  # there are 29 fields for conditions per visit
start_pos <- 5 # the columns with codes for conditions start with column 5 and end with columns 5+3*29-1 = 91  
####################################################################################################

############################## EXECUTE #############################################################
t20002 <- fread(filepath20002_in)  #502599

t20002_bin <- build_cond_and_age_diag_table(t20002,
                                            list_of_conditions, 
                                            list_of_labels, 
                                            n_visits, 
                                            l_array, 
                                            start_pos)
names(t20002_bin)[1] <- 'ID'

t20002_bin_reord <- group_by_visit(t20002_bin, list_of_conditions, n_visits)

t20002_cond_age <- group_cond_age(t20002_bin, list_of_conditions, n_visits)

write.table(t20002_bin,
            filepath20002_out, 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
####################################################################################################

