library(dplyr)
library(data.table)

##############################################################################################33
#creates a table of four columns: id and one column of condition (= cond) per visit, 
#i.e. it is a table of 502599 rows and 4 columns
#the first column is the id column, 
# the entries in the three columns are either: 
#0 if the person does not have the condition = cond, 
#1 if they have the condition = cond or 
#NA if no information is available
build_id_cond_v_columns <- function(t_main, t_ids, cond, cond_lbl, n_v, l_arry, start_pos){
  cond_found_in_row <- function(r){
    any(r == cond)
  }
  #t_out <- select(t_main, 'f.eid')
  t_out <- t_ids
  for(i in 1:n_v){
    a <- start_pos + (i-1)*l_arry
    b <- a + l_arry - 1 
    col <- 1+i
    t_nonNA <- filter(t_main, !is.na(t_main[, ..col]))
    t_nonNA_ids <- select(t_nonNA, 'f.eid')
    t <- filter(t_nonNA_ids, apply(t_nonNA[,a:b], 1, cond_found_in_row))
    t <- mutate(t, new = 1)
    names(t)[2] <- paste(cond_lbl, "_v", i-1,  sep = "")
    t <- left_join(t_nonNA_ids, t, by='f.eid') 
    t[is.na(t)] <- 0 
    t_out <- left_join(t_out, t, by='f.eid')
    #names(t_out)[2] <- headers[,as.character(cond)][[1]]
  }
  t_out
}

build_bin_table <- function(t_main, conds, cond_lbls, n_v, l_arry, start_pos){
  t_ids <- select(t_main, 'f.eid')
  t_out <- t_ids
  n_conds <- length(conds)
  for(j in 1:n_conds){
    t <- build_id_cond_v_columns(t_main, t_ids, conds[j], cond_lbls[j], n_v, l_arry, start_pos)
    t_out <- left_join(t_out, t, by='f.eid')
  }
  #if(length(other) != 0){
  #  t_curr <- columns_id_other(t_main, other, other_lbl)
  #  t_out <- full_join(t_out, t_curr, by='f.eid')
  #}
  #t_out[is.na(t_out)] <- 0
  t_out
}

####################################################################################################

add_1_age_diag_to_table <- function(t_main, t_bin, cond, cond_lbl, n_v, l_arry, start_pos){
  find_indx_in_row <- function(r){
    j <- (match(cond, r[a:b]) + a-1 + 3*l_arry)
    r[j]
  }
  cond_found_in_row <- function(r){any(r == cond)}
  t_out <- t_bin
  for(i in 1:n_v){
    a <- start_pos + (i-1)*l_arry
    b <- a + l_arry - 1 
    t_filtered <- filter(t_main, apply(t_main[,a:b], 1, cond_found_in_row))
    t_age <-  select(t_filtered, 'f.eid')
    t_age <- mutate(t_age,  as.character(i))
    t_age[,2] <- apply(t_filtered, 1, find_indx_in_row)
    names(t_age)[2] <- paste("age_", cond_lbl, "_v", i-1,  sep = "")
    t_out <- left_join(t_out, t_age, by='f.eid') 
  }
  t_out
}

#given a list of conditions, conds, create a table with columns orginized as follows:
# 1. one column per condition, with binary entry, 
# 2 column orginized in triple visits per condition, that contain the age of onset, declared during each visit     
add_age_diag_to_table <- function(t_main, t_bin, conds, cond_lbls, n_v, l_arry, start_pos){
  t_out <- t_bin
  n <- length(conds)
  for(i in 1:n){
    t_out <- add_1_age_diag_to_table(t_main, t_out, conds[i], cond_lbls[i], n_v, l_arry, start_pos)
  }
  t_out
}

####################################################################################################

build_cond_and_age_diag_table <- function(t_main,list_of_conditions, list_of_labels, n_visits, l_array, start_pos){
  t_cond_age_diag <- build_bin_table(t_main, list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
  t_cond_age_diag <- add_age_diag_to_table(t_main, t_cond_age_diag, list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
}

############################## INPUTS ########################################
filename <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukb20002.txt"
l_array <- 29  # there are 29 fields for conditions per visit
start_pos <- 5 # the columns with codes for conditions start with column 5 and end with columns 3*29-1 = 91  
list_of_conditions <- c(1111, 1387, 1452)
list_of_labels <- c('Asthma', 'Hayf_Rhin', 'Eczema')
n_visits <- 3
##############################################################################

############################## EXECUTE #######################################
t20002_full <- fread(filename)  #502599
t_asth_rhin_ecz <- build_cond_and_age_diag_table(t20002_full,list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
#write.table(t_asth_rhin_ecz, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\asth_rhin_ecz.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
##############################################################################

############################## ADD SOME DEMOGRAPHIC AND GENETIC INFO ###########################
filename <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukbAux.txt" 
t_Aux <- fread(filename)  #502599
t_asth_rhin_ecz_aux <- left_join(t_asth_rhin_ecz, t_Aux, by='f.eid')
##############################################################################

######################## CHANGE ID LABEL FROM 'f.eid' TO 'ID' ###########################
names(t_asth_rhin_ecz)[1] <- 'ID' #Do this only after being done with averything else
names(t_asth_rhin_ecz_aux)[1] <- 'ID' #Do this only after being done with averything else
##############################################################################

########################   EXPORT AS TEXT FILES  ###########################
write.table(t_asth_rhin_ecz, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\asth_rhin_ecz.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
write.table(t_asth_rhin_ecz_aux, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\asth_rhin_ecz_aux.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
##############################################################################

