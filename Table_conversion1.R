library(dplyr)
library(data.table)

####################################################################################################
# selects from table t_main all people (rows) who have lsited 
# NA in their columns of number of conditions declared for visits 0,1,2
non_compliant_table <- function(t_main){
  t_out <- filter(t_main, is.na(t_main[, "f.135.0.0"])) #862 NA on first visit
  t_out <- filter(t_out, is.na(t_out[ , "f.135.1.0"])) #858 NA on first and second visit
  t_out <- filter(t_out, is.na(t_out[ , "f.135.2.0"])) #853 NA on all three visits
  t_out
}

# selects from table t_mian all people (rows) who have at least one entry, different from NA,
# in their columns of number of conditions declared for visits 0,1,2. 
# The resulting table is the complement of the table from funciton non_comliant_table
compliant_table <- function(t_main){
  t_out <- non_compliant_table(t_main)
  t_out <- setdiff(t_main, t_out)
  t_out
}
####################################################################################################

#creates a table of two columns: id and condition = cond, 
# with entries either 0 if the person does not have the condition = cond or 1 if they have it 
columns_id_cond <- function(t_main, cond, cond_lbl){
  cond_found_in_row <- function(r){
    any(r == cond)
  }
  t_out <- select(t_main, 'f.eid')
  t_out <- filter(t_out, apply(t_main[,5:91], 1, cond_found_in_row))
  t_out <- mutate(t_out, new = 1)
  names(t_out)[2] <- cond_lbl
  #names(t_out)[2] <- headers[,as.character(cond)][[1]]
  t_out
}

#given the main table t_main, create a new table with columns: one column for each code 
# listed in conds (list of conditions' codes) and one column for the group of conditions other 
create_bin_table <- function(t_main, conds, cond_lbls, other, other_lbl){
  t_out <- select(t_main, 'f.eid')
  n_conds <- length(conds)
  for(j in 1:n_conds){
    t_curr <- columns_id_cond(t_main, conds[j], cond_lbls[j])
    t_out <- full_join(t_out, t_curr, by='f.eid')
  }
  if(length(other) != 0){
    t_curr <- columns_id_other(t_main, other, other_lbl)
    t_out <- full_join(t_out, t_curr, by='f.eid')
  }
  t_out[is.na(t_out)] <- 0
  t_out
}
####################################################################################################

#for a given codnition, for all three visits, create three columns with
#the age of onset recorded during each visit
add_agecond_to_table <- function(t_main, t_bin, cond, cond_lbl, n_v){
  find_indx_in_row <- function(r){
    j <- (match(cond, r[a:b]) + a-1 + 87)
    r[j]
  }
  cond_found_in_row <- function(r){any(r == cond)}
  t_out <- t_bin
  for(i in 1:n_v){
    a <- 5 + (i-1)*29
    b <- a + 28 
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
create_age_conds_table <- function(t_main, t_bin, conds, cond_lbls, n_v){
  t_out <- t_bin
  n <- length(conds)
  for(i in 1:n){
    t_out <- add_agecond_to_table(t_main, t_out, conds[i], cond_lbls[i], n_v)
  }
  t_out
}
#####################################################################################################
#####################################################################################################
#####################################################################################################


############################## INPUTS ########################################
#type the path to the subtable extracted from an original UKBiobank file, as a string
filename <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukb20002.txt"
list_of_conditions <- c(1111, 1387, 1452)
list_of_labels <- c('Asthma', 'Hayf_Rhin', 'Eczema')
n_visits <- 3
##############################################################################

############################## EXECUTE #######################################
t20002_full <- fread(filename)  #502599
t20002 <- compliant_table(t20002_full)
#write.table(t20002, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukb20002comp.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

t_asth_rhin_ecz <- create_bin_table(t20002, list_of_conditions, list_of_labels, c(), "")
t_asth_rhin_ecz <- create_age_conds_table(t20002, t_asth_rhin_ecz, list_of_conditions, list_of_labels, n_visits)
t_asth_rhin_ecz <- left_join(select(t20002_full, 'f.eid'), t_asth_rhin_ecz, by = "f.eid")
##############################################################################


############################## ADD AUXILIARY TABLE ###########################
filename <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukbAux.txt" 
t_Aux <- fread(filename)  #502599
t_asth_rhin_ecz_aux <- left_join(t_asth_rhin_ecz, t_Aux, by='f.eid')
##############################################################################


######################## CHANGE ID LABEL FROM 'f.eid' TO 'ID' ###########################
names(t_asth_rhin_ecz)[1] <- 'ID' #Do this only after being done with averything else
names(t_asth_rhin_ecz_aux)[1] <- 'ID' #Do this only after being done with averything else
##############################################################################

########################        EXPORT AS TEXT FILES         ###########################
write.table(t_asth_rhin_ecz, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\asth_rhin_ecz.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
write.table(t_asth_rhin_ecz_aux, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\asth_rhin_ecz_aux.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
##############################################################################

