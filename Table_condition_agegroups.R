library(dplyr)
library(data.table)

##########################################################################################

add_col <- function(t, entry, lbl){ 
  t_out <- mutate(t, new = entry)
  names(t_out)[length(t[1,])+1] <- paste(lbl) 
  return(t_out)
}

create_cond_agegrp_bin <- function(t_main, cond, age_cond, age_lbls, keepNA){
  #t_main <- t_cond
  #t_noNA <- filter(t_main, !is.na(t_main[,cond])) 
  t_cases <- filter(t_main, !is.na(t_main[,cond])) 
  t_noNA_id <- select(t_cases, ID)
  #t_out <- select(t_noNA, ID, cond)
  t_cases <- select(t_cases, ID, cond, age_cond)
  t_cases <- filter(t_cases, t_cases[,cond] == 1)
  t_cases_age <- select(t_cases, age_cond)
  t_cases <- select(t_cases, ID, cond)
  t_cases_id <- select(t_cases, ID)
  get_i <- function(r){
    for(i in seq_along(age_groups)){
      if(age_groups[[i]][1] <= r[age_cond] &  r[age_cond] < age_groups[[i]][2]){return(i)}
    }
    if(r[age_cond] < (-2)){return( -3)}
    else{ return( -1) }
  }
  indxs <- apply(t_cases_age, 1, get_i) 
  for(i in seq_along(age_lbls)){
    t <- filter(t_cases_id, indxs == i)  #t <- filter(t_in_id, indxs == ) 
    t <- add_col(t, 1, age_lbls[i])      #t <- add_col(t, 1, age) 
    t_cases <- left_join( t_cases, t, by='ID')
  }
  if(!keepNA){
    t_cases[ (indxs == -3), age_lbls] <- -3
    t_cases[ (indxs == -1), age_lbls] <- -1
  }
  t_cases <- left_join(t_noNA_id, t_cases, by = 'ID') #t_out is t_cases
  t_cases[is.na(t_cases[,cond]), c(cond, age_lbls)] <- 0 #t_out is t_cases
  return(t_cases)
}

##########################################################################################

############################## INPUTS FOR CONDITIONS ###############################################
filename <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Output_data_ph1\\ukb20002.txt"
l_array <- 29  # there are 29 fields for conditions per visit
start_pos <- 5 # the columns with codes for conditions start with column 5 and end with columns 5+3*29-1 = 91  
list_of_conditions <- c(1111)
list_of_labels <- c('Asthma')
n_visits <- 1
####################################################################################################

############################## EXECUTE #############################################################
t20002 <- fread(filename)  #502599
t_cond <- build_cond_and_age_diag_table(t20002,list_of_conditions, list_of_labels, n_visits, l_array, start_pos)
names(t_cond)[1] <- 'ID' 
rm(t20002)

# now we have extracted t_cond = ID | cond | age_cond |
visit <- 0
cond <- paste(list_of_labels, "_v", visit, sep='') 
age_cond <- paste('age_', list_of_labels[1], "_v", visit, sep='') 

age_groups <- list(c(0,18), c(18,40), c(40, 120))
age_group_lbls <- c('Age_6', 'Age_4', 'Age_5' )
age_lbls <- paste(cond, '_', age_group_lbls, sep='')

t_cond_agegrp_bin  <- create_cond_agegrp_bin(t_cond, cond, age_cond, age_lbls, keepNA = TRUE)
####################################################################################################


