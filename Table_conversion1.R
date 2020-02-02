library(dplyr)
library(data.table)

####################################################################################################

#creates a table of four columns: id and one column of condition (= cond) per visit,
# | f.eid | Condition_v0 | Condition_v1 | Condition_v2 |
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
  }
  t_out
}

# Builds a "binary" type table: given a list of conditions and a list of labels for these conditions
# for each condition from the list creates a 4 column table, using the preceding function 'build_id_cond_v_columns'
# and then pastes them togehter aligning them together by their first columns 'f.eid' 
build_bin_table <- function(t_main, conds, cond_lbls, n_v, l_arry, start_pos){
  t_ids <- select(t_main, 'f.eid')
  t_out <- t_ids
  n_conds <- length(conds)
  for(j in 1:n_conds){
    t <- build_id_cond_v_columns(t_main, t_ids, conds[j], cond_lbls[j], n_v, l_arry, start_pos)
    t_out <- left_join(t_out, t, by='f.eid')
  }
  t_out
}

####################################################################################################
# for one chosen condition during each visist, this function finds the corresponding age of diagnosis of the condition, 
# declared during the respective visit. As a result, for a given condition for each visit, we gat a tables
# | f.eid | Age_cond_v0 | Age_cond_v1 | Age_cond_v2 |
# and we attach it to the alrady existing full table t_out by aligning the first columns 'f.eid' of t_out and 
# the new four-column table  
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
# 1. one column per condition per visit, each with entry either 0, 1 or NA; 
# 2. columns per condition per visits, that contain the age of onset, declared during each visit     
add_age_diag_to_table <- function(t_main, t_bin, conds, cond_lbls, n_v, l_arry, start_pos){
  t_out <- t_bin
  n <- length(conds)
  for(i in 1:n){
    t_out <- add_1_age_diag_to_table(t_main, t_out, conds[i], cond_lbls[i], n_v, l_arry, start_pos)
  }
  t_out
}

####################################################################################################

build_cond_and_age_diag_table <- function(t_main, list_of_conditions, list_of_labels, n_visits, l_array, start_pos){
  t_cond_age_diag <- build_bin_table(t_main, list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
  t_cond_age_diag <- add_age_diag_to_table(t_main, t_cond_age_diag, list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
}

####################################################################################################

relabel <- function(table, fields, array_length, instances, labels){
  headers <- names(table)
  #n_headers <- length(headers)
  n_fields <- length(fields)
  h <- 2
  for(i in 1:n_fields){
    for(v in 1:instances[i]){
      if(arrays_length[i] != 1){
        for(l in 1:arrays_length[i]){
          if(instances[i] == 1){
            headers[h] <- paste(labels[i], "_", as.character(l), sep = "")
          }
          else{
            headers[h] <- paste(labels[i], "_", as.character(l),"_v", as.character(v-1), sep = "")
          }
          h <- h+1
        }
      } 
      else{
        if(instances[i] == 1){
          headers[h] <- labels[i]
        }
        else{
          headers[h] <- paste(labels[i], "_v", as.character(v-1), sep = "")
        }
        h <- h+1
      }
    }
  }
  headers
}

####################################################################################################


############################## INPUTS FOR CONDITIONS ###############################################
filename <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukb20002.txt"
l_array <- 29  # there are 29 fields for conditions per visit
start_pos <- 5 # the columns with codes for conditions start with column 5 and end with columns 3*29-1 = 91  
list_of_conditions <- c(1111, 1387, 1452)
list_of_labels <- c('Asthma', 'Hayf_Rhin', 'Eczema')
n_visits <- 3
####################################################################################################

#################  INPUTS FOR ADDITIONAL DATA, E.G. DEMOGRAPHIC INFO  ##############################
filename <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukb_demogr_geno_info.txt" 
fields <- c(31, 21000, 21003, 22001, 22006, 22009, 22010, 22018)    
arrays_length <- c(1, 1, 1, 1, 1, 40, 1, 1)
instances   <-   c(1, 3, 3, 1, 1,  1, 1, 1)
labels <- c("Sex", "Ethnic_backgr",  "Age_at_Visit", "Genetic_sex",
            "Gen_ethnic_grp", "PC", "Geno_analys_exclns",
            "Rel_exclns")
####################################################################################################

############################## EXECUTE #############################################################
t20002_full <- fread(filename)  #502599
t_asth_rhin_ecz <- build_cond_and_age_diag_table(t20002_full,list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
####################################################################################################

############################## EXECUTE #############################################################
t_demogr_geno_info <- fread(filename) #502599
names(t_demogr_geno_info) <- relabel(t_demogr_geno_info, fields, array_length, instances, labels)

t_asth_rhin_ecz_plus <- left_join(t_asth_rhin_ecz, t_demogr_geno_info, by='f.eid')

names(t_asth_rhin_ecz)[1] <- 'ID'
names(t_asth_rhin_ecz_plus)[1] <- 'ID'

write.table(t_asth_rhin_ecz, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\asth_rhin_ecz.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
write.table(t_asth_rhin_ecz_plus, "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\asth_rhin_ecz_plus.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)
####################################################################################################
