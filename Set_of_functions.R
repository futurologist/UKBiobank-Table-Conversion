library(dplyr)
library(data.table)

#################################################################################################################
###############################################################################################################

extract_subtable <- function(tbl, list_of_columns){
  headers <- names(tbl)
  pos <- c(1)
  for(code in list_of_columns){
    pos <- c(pos, grep(code, headers))
  }
  subt <- table[, ..pos]
  subt <- subt[order(subt[, 1]), ]
  return(subt)
}

#################################################################################################################
#################################  20002  #######################################################################
#################################################################################################################

#creates a table of four columns: id and one column of condition (= cond) per visit, 
#i.e. it is a table of 502599 rows and 4 (in fact n_v) columns
#the first column is the id column, 
# the entries in the three columns are either: 
#0 if the person does not have the condition = cond, 
#1 if they have the condition = cond or 
#NA if no information is available
build_id_cond_v_columns <- function(t_main, t_ids, cond, cond_lbl, n_v, l_arry, start_pos){
  cond_found_in_row <- function(r){
    any(r[a:b] == cond)
  }
  t_out <- t_ids
  for(i in 1:n_v){
    a <- start_pos + (i-1)*l_arry
    b <- a + l_arry - 1 
    col <- names(t_main)[1+i]
    t_nonNA <- t_main[ !is.na(t_main[, get(col)]), ]
    t_nonNA_ids <- t_nonNA[, 1]
    t <- t_nonNA_ids[ apply(t_nonNA, 1, cond_found_in_row), ]
    t <- mutate(t, new = 1)
    names(t)[2] <- paste(cond_lbl, "_v", i-1,  sep = "")
    t <- left_join(t_nonNA_ids, t, by=names(t)[1]) 
    t[is.na(t)] <- 0 
    t_out <- left_join(t_out, t, by=names(t)[1])
  }
  return(t_out)
}

build_bin_20002 <- function(t_main, conds, cond_lbls, n_v, l_arry, start_pos){
  t_ids <- t_main[, 1]
  t_out <- t_ids
  n_conds <- length(conds)
  for(j in 1:n_conds){
    t <- build_id_cond_v_columns(t_main, t_ids, conds[j], cond_lbls[j], n_v, l_arry, start_pos)
    t_out <- left_join(t_out, t, by=names(t)[1])
  }
  return(t_out)
} #name clashes with a different function from 6159 

#####################################################################################################

add_1_age_diag_to_table <- function(t_main, t_bin, cond, cond_lbl, n_v, l_arry, start_pos){
  find_indx_in_row <- function(r){
    j <- (match(cond, r[a:b]) + a-1 + 3*l_arry)
    return(r[j])
  }
  cond_found_in_row <- function(r){any(r == cond)}
  t_out <- t_bin
  for(i in 1:n_v){
    a <- start_pos + (i-1)*l_arry
    b <- a + l_arry - 1 
    t_filtered <- t_main[ apply(t_main, 1, cond_found_in_row), ]
    t_age <-  t_filtered[, 1]
    t_age <- mutate(t_age,  as.character(i))
    t_age[,2] <- apply(t_filtered, 1, find_indx_in_row)
    names(t_age)[2] <- paste("age_", cond_lbl, "_v", i-1,  sep = "")
    t_out <- left_join(t_out, t_age, by=names(t_out)[1]) 
  }
  return(t_out)
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
  return(t_out)
}

####################################################################################################

build_cond_and_age_diag_table <- function(t_main, list_of_conditions, list_of_labels, n_visits, l_array, start_pos){
  t_cond_age_diag <- build_bin_20002(t_main, list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
  t_cond_age_diag <- add_age_diag_to_table(t_main, t_cond_age_diag, list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
  return(t_cond_age_diag)
}

####################################################################################################

####################################################################################################

relabel <- function(table, fields, arrays_length, instances, labels){
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

group_by_visit <- function(t_cond, l_cond, n_vis, age_onset){
  n_cond <- length(list_of_conditions)
  cl <- c(seq(1, n_vis*n_cond, by=n_vis)) + 1
  if(age_onset){cl <- c(cl, cl + n_vis*n_cond)}
  col <- c(1, cl)
  i <- 1
  while(i < n_vis){
    col <- c(col, cl + i)
    i <- i + 1
  }
  cols <- names(t_cond)[col]
  t <- t_cond[, ..cols]
  return(t)
}

group_cond_age <- function(t_cond, l_cond, n_vis){
  n_cond <- length(l_cond)
  trs <- n_cond * n_vis 
  ct <- c(1:(2*trs))
  cc <- c(1:trs)
  ct[2*cc - 1] <- (cc + 1)
  ct[2*cc] <- (cc + trs + 1)
  cc <- c(1, ct)
  cols <- names(t_cond)[cc]
  t <- t_cond[ , ..cols]
  return(t)
}

#################################################################################################################
#################################################################################################################



#################################################################################################################
#######################################  6159  ##################################################################
#################################################################################################################

new_column <- function(t_main_ids, t_ids, site_lbl, visit){
  t <- mutate(t_ids, new = 1)
  names(t)[2] <- paste(site_lbl, "_v", visit-1,  sep = "")
  t <- left_join(t_main_ids, t, by=names(t_ids)[1])
  t[is.na(t)] <- 0
  return(t)
}

build_id_and_site_columns <- function(t_clean, t_clean_ids, site, site_lbl, visit, l_array, start_pos){
  a <- start_pos + (visit-1) * l_array
  b <- a + l_array - 1
  t <- t_clean_ids[ apply(t_clean[,a:b], 1, function(r){any(r == site)}), ]
  t <- new_column(t_clean_ids, t, site_lbl, visit)
  return(t)
}

stack_all_sites_per_visit <- function(t_clean, t_clean_ids, sites, site_lbls, visit, l_array, start_pos){
  t <- t_clean_ids
  n_sites <- length(sites)
  for(i in 1:n_sites){
    t_new_clmn <- build_id_and_site_columns(t_clean, t_clean_ids, sites[i], site_lbls[i], visit, l_array, start_pos)
    t <- left_join(t, t_new_clmn, by=names(t)[1]) 
  }
  return(t)
}


build_bin_6159 <- function(t_original, sites, site_lbls, n_v, l_array, start_pos){
  
  t_out <- t_original[, 1]
  
  ind <- which(!(sites %in% c(-3,8))) # use for sites[ind] and site_lbls[ind] 
  ind_noAnsr <- match(-3, sites)
  ind_All <- match(8, sites)
  
  for(i in 1:n_v){
    
    c <- names(t_original)[start_pos + (i-1) * l_array]
    
    t_clean <- t_original[!is.na(t_original[ , get(c)]),  ]  
    t_noNA_ids <- t_clean[, 1]
    t_clean <- t_clean[ (t_clean[, get(c)] != -3), ]
    t_Ansrs_noNA_ids <- t_clean[, 1]
    
    t_temp <- setdiff(t_noNA_ids, t_Ansrs_noNA_ids)
    t_temp <- new_column(t_noNA_ids, t_temp, site_lbls[ind_noAnsr], i)
    
    t_clean <- t_clean[(t_clean[, get(c)] != 8), ]
    t_clean_ids <- t_clean[, 1]
    t_All <- setdiff(t_Ansrs_noNA_ids, t_clean_ids)
    t_All <- new_column(t_Ansrs_noNA_ids, t_All, site_lbls[ind_All], i)
    t_temp <- left_join(t_temp, t_All, by=names(t_temp)[1])
    
    t_clean <- stack_all_sites_per_visit(t_clean, t_clean_ids, sites[ind], site_lbls[ind], i, l_array, start_pos)
    
    t_temp <- left_join(t_temp, t_clean, by=names(t_temp)[1])
    
    # experimental:
    cols <- c(1:ncol(t_temp))
    cols[3:4] <- c(4,3)
    t_temp <- t_temp[, ..cols]
    # end experimental
    
    t_out <- left_join(t_out, t_temp, by=names(t_out)[1])
    
  }
  return(t_out)
} #name clashes with a different function from 20002 


#################################################################################################################

reorganize <- function(t_in){
  t <- t_in
  n_cols <- ncol(t_in)
  cols <- c(1)
  for(i in 0:2){
    cols <- c(cols, seq((2+i), n_cols, by=3))
  }
  t <- t[, ..cols]
  return(t)
}

#cols <- c( 1, c( (start_pos + n_visits * l_array) : length(t6159[1,]) ) )
#t_duration <- t6159[ , ..cols]
#names(t_duration) <- relabel(t_duration, fields, arrays_length, instances, labels)
#t_duration <- reorganize(t_duration)  

build_duration <- function(t_main, fields, instances, labels, n_visits, l_array, start_pos){
  cols <- c( 1, c( (start_pos + n_visits * l_array) : ncol(t6159)) ) 
  t_duration <- t_main[ , ..cols]
  names(t_duration) <- relabel(t_duration, fields, arrays_length, instances, labels)
  t_duration <- reorganize(t_duration)  
  return(t_duration)
}

#t_pain_duration <- left_join(t_pain, t_duration, by='f.eid')

#################################################################################################################




#################################################################################################################
########################### Creates table: Condition and Age-of-Onset Groups ####################################
#################################################################################################################

add_col <- function(t, entry, lbl){ 
  t_out <- mutate(t, new = entry)
  names(t_out)[ncol(t_out)] <- paste(lbl) 
  return(t_out)
}


create_cond_agegrp <- function(t_main, cond, age_cond, age_groups, age_lbls, keepNA){
  ID <- names(t_main)[1]
  cols <- c(ID, cond, age_cond)
  t_cases <- t_main[ , ..cols]
  t_cases <- t_cases[ !is.na(t_main[, get(cond)]) , ]
  t_noNA_id <- t_cases[ , ..ID]
  t_cases <- t_cases[ t_cases[,get(cond)] == 1, ]
  t_cases_age <- t_cases[ , ..age_cond]
  cols <- c(ID, cond)
  t_cases <- t_cases[, ..cols]
  get_i <- function(r){
    for(i in seq_along(age_groups)){
      if(age_groups[[i]][1] <= r[age_cond] &  r[age_cond] < age_groups[[i]][2]){return(i)}
    }
    if(r[age_cond] < (-2)){return( -3)}
    else{ return( -1) }
  }
  indxs <- apply(t_cases_age, 1, get_i) 
  for(i in seq_along(age_lbls)){
    t <- t_cases[ indxs==i , ..ID] 
    t <- add_col(t, 1, age_lbls[i])  
    t_cases <- left_join( t_cases, t, by=ID)
  }
  if(!keepNA){
    t_cases[ (indxs == -3), age_lbls] <- -3
    t_cases[ (indxs == -1), age_lbls] <- -1
  }
  t_cases <- left_join(t_noNA_id, t_cases, by = ID) 
  cols <-  c(cond, age_lbls)
  t_cases[is.na(t_cases[, get(cond)]), cols] <- 0
  return(t_cases)
}


build_cond_agegrp_bin <- function(t_cond, condition, age_groups, age_group_lbls, n_visits, keep_NA){
  t_out <- t_cond[ , 1] #select(t_cond, 1)
  for(i in 1:n_visits){ 
    visit <- i - 1
    cond <- paste(condition, "_v", visit, sep='') 
    age_lbls <- paste(condition, '_', age_group_lbls, '_v', visit, sep='')
    age_cond <- paste('age_', condition, "_v", visit, sep='') 
    t  <- create_cond_agegrp(t_cond, cond, age_cond, age_groups, age_lbls, keep_NA)
    t_out  <- left_join(t_out, t, by = names(t)[1])
  }
  return(t_out)
}

#col <- grep(condition, names(t_cond),ignore.case = TRUE)
#t_cond <- select(t_cond, 1, col)


#################################################################################################################
#################################################################################################################


