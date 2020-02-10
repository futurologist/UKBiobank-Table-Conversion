library(dplyr)
library(data.table)

#################################################################################################################
new_column <- function(t_main_ids, t_ids, site_lbl, visit){
  t <- mutate(t_ids, new = 1)
  names(t)[2] <- paste(site_lbl, "_v", visit-1,  sep = "")
  t <- left_join(t_main_ids, t, by='f.eid')
  t[is.na(t)] <- 0
  t
}

build_id_and_site_columns <- function(t_clean, t_clean_ids, site, site_lbl, visit, l_array, start_pos){
  a <- start_pos + (visit-1) * l_array
  b <- a + l_array - 1
  t <- filter(t_clean_ids, apply(t_clean[,a:b], 1, function(r){any(r == site)}))
  t <- new_column(t_clean_ids, t, site_lbl, visit)
  t
}

stack_all_sites_per_visit <- function(t_clean, t_clean_ids, sites, site_lbls, visit, l_array, start_pos){
  t <- t_clean_ids
  n_sites <- length(sites)
  for(i in 1:n_sites){
    t_new_clmn <- build_id_and_site_columns(t_clean, t_clean_ids, sites[i], site_lbls[i], visit, l_array, start_pos)
    t <- left_join(t, t_new_clmn, by='f.eid') 
  }
  t
}

build_bin_table <- function(t_original, sites, site_lbls, n_v, l_array, start_pos){
  
  t_out <- select(t_original, 'f.eid')
  
  ind <- which(!(sites %in% c(-3,8))) # use for sites[ind] and site_lbls[ind] 
  ind_noAnsr <- match(-3, sites)
  ind_All <- match(8, sites)
  
  for(i in 1:n_v){
    
    c <- start_pos + (i-1) * l_array
    
    t_clean <- filter(t_original, !is.na(t_original[,..c]))  
    t_noNA_ids <- select(t_clean ,'f.eid')
    t_clean <- filter(t_clean, (t_clean[, c] != -3))
    t_Ansrs_noNA_ids <- select(t_clean, 'f.eid')
    
    t_temp <- setdiff(t_noNA_ids, t_Ansrs_noNA_ids)
    t_temp <- new_column(t_noNA_ids, t_temp, site_lbls[ind_noAnsr], i)
    
    t_clean <- filter(t_clean, (t_clean[, c] != 8))
    t_clean_ids <- select(t_clean, 'f.eid')
    t_All <- setdiff(t_Ansrs_noNA_ids, t_clean_ids)
    t_All <- new_column(t_Ansrs_noNA_ids, t_All, site_lbls[ind_All], i)
    t_temp <- left_join(t_temp, t_All, by='f.eid')
    
    t_clean <- stack_all_sites_per_visit(t_clean, t_clean_ids, sites[ind], site_lbls[ind], i, l_array, start_pos)
    
    t_temp <- left_join(t_temp, t_clean, by='f.eid')
    t_out <- left_join(t_out, t_temp, by='f.eid')
  
  }
  t_out
}


#################################################################################################################

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

reorganize <- function(t_in){
  t <- t_in
  n_col <- length(t[1,])
  cols <- c(1)
  for(i in 0:2){
    cols <- c(cols, seq((2+i), n_cols, by=3))
  }
  t <- t[ , ..cols]
  t
}

#################################################################################################################


############################## INPUTS FOR PAIN SITES ###############################################
filename2 <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Output_data_ph1\\ukb6159.txt"
t6159 <- fread(filename2)
l_array <- 7  # there are 7 fields for pain siates per visit
start_pos <- 2 # the columns with codes for pain sites start with column 2 and ends with columns 2+3*7-1 = 22 
#list_of_codes <- c("f.6159.", "f.2956.", "f.3404.", "f.3414.", "f.3571.", "f.3741.", "f.3773.", "f.3799.", "f.4067.")
list_of_labels <- c('Prf_no_Ansr', 'Non_Abve', 'All_over', 'Neck_Shldr_pn', 'Hip_pn', 'Back_pn', 'Stom_Abdmn_pn', 'Knee_pn', 'Headch', 'Face_pn')
list_of_sites <- c(-3, -7, 8, 3, 6, 4, 5, 7, 1, 2)
n_visits <- 3
####################################################################################################

############################## INPUTS FOR PAIN SITES ###############################################
#fields <- c("f.3799.", "f.4067.", "f.3404.",  "f.3571.", "f.3741.", "f.3414.",  "f.3773.", "f.2956.")
fields <- c("f.2956.", "f.3404.", "f.3414.", "f.3571.", "f.3741.", "f.3773.", "f.3799.", "f.4067.")
arrays_length <- c(1, 1, 1, 1, 1, 1, 1, 1)
instances   <-   3 * arrays_length #c(1, 3, 3, 1, 1,  1, 1, 1)
labels <- c('All_over_3+m', 'Neck_Shldr_pn_3+m', 'Hip_pn_3+m', 'Back_pn_3+m', 'Stom_Abdmn_pn_3+m', 'Knee_pn_3+m', 'Headch_3+m', 'Face_pn_3+m')
####################################################################################################
####################################################################################################

##############################   EXECUTE    ########################################################
t_pain <- build_bin_table(t6159, list_of_sites, list_of_labels, n_visits, l_array, start_pos)

cols <- c( 1, c( (start_pos + n_visits * l_array) : length(t6159[1,]) ) )
t_duration <- t6159[ , ..cols]
names(t_duration) <- relabel(t_duration, fields, arrays_length, instances, labels)
t_duration <- reorganize(t_duration)  
t_pain_duration <- left_join(t_pain, t_duration, by='f.eid')
names(t_pain_duration)[1] <- 'ID'
####################################################################################################
