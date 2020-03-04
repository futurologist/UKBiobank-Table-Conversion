library(dplyr)
library(data.table)

####################################################################################
chi_sq_table_inner <- function(T1, T2){
  unfold <- function(l){
    return(data.table(matrix(l, nrow = 1)))
  }
  n1 <- length(T1[1,])
  n2 <- length(T2[1,])
  hT1 <- names(T1)
  hT2 <- names(T2)
  k = 19
  T_res <- bind_cols(data.table(matrix(rep("", len=n1*n2), nrow = n1*n2, ncol = 2)), 
                     data.table(matrix(rep(0, len=n1*n2*k), nrow = n1*n2, ncol = k)))
  names(T_res) <- c('Var_1', 'Var_2', 'chi_sq_stats', 'deg_of_frdm','p_value', 
                    'observed_No_and_No', 'observed_Yes_and_No', 'observed_No_and_Yes', 'observed_Yes_and_Yes',
                    'expected_No_and_No', 'expected_Yes_and_No', 'expected_No_and_Yes', 'expected_Yes_and_Yes',
                    'residuals_No_and_No', 'residuals_Yes_and_No', 'residuals_No_and_Yes', 'residuals_Yes_and_Yes',
                    'stdres_No_and_No', 'stdres_Yes_and_No', 'stdres_No_and_Yes', 'stdres_Yes_and_Yes')
  r <- 1
  for(i in 1:n1){
    C1 <- pull(T1, i)
    for(j in 1:n2){
      C2 <- pull(T2, j)
      ch.sq.ts <- chisq.test(table(C1, C2))
      T_res[r, c(1,2)] <- list(hT1[i], hT2[j])
      T_res[r, c(3:5)] <- ch.sq.ts[c(1:3)]
      T_res[r, c(6:21)] <- sapply(ch.sq.ts[c(6:9)], unfold)
      r <- r + 1
    }
  }
  return(T_res)
}


chi_sq_table <- function(Tbl1, list1, Tbl2, list2){
  if (nrow(Tbl1) != nrow(Tbl2)){
    ids <- inner_join(select(Tbl1, 1), select(Tbl2, 1), by=names(Tbl1)[1])
    T1 <- left_join(ids, Tbl1, by=names(Tbl1)[1])
    T2 <- left_join(ids, Tbl2, by=names(Tbl2)[1])
    return(chi_sq_table_inner(select(T1, list1), select(T2, list2)))
  } else if(sum(Tbl1[,1] != Tbl2[,1])){
    ids <- inner_join(select(Tbl1, 1), select(Tbl2, 1), by=names(Tbl1)[1])
    T1 <- left_join(ids, Tbl1, by=names(Tbl1)[1])
    T2 <- left_join(ids, Tbl2, by=names(Tbl2)[1])
    return(chi_sq_table_inner(select(T1, list1), select(T2, list2)))
  }
  return(chi_sq_table_inner(select(Tbl1, list1), select(Tbl2, list2)))
}

####################################################################################

############################   PROVIDE INPUT HERE:  ################################

# provide two data tables (the same table twice is also allowed)
# both tables should have a column IDs as their first column  
table_1 <- t_pain[c(1:(nrow(t_pain) - 3)), ]
table_2 <- t_pain[c(4:nrow(t_pain))]

# from the two data tables above, provide a pair of lists of columns  
# that you would like to compare 
# for pairwise dependence, using chi-square test 

list_1 <- c('All_over_v0', 'All_over_v1', 'Headch_v0','Face_pn_v2')
list_2 <- c('Stom_Abdmn_pn_v1', 'Knee_pn_v1', 'Knee_pn_v2')

# If the two tables have different number of rows or there is a discirpancy in
# the IDs in the respective first columns, the function will keep only the IDs both tables 
# have in common and continue with the calculation of the chi-squared test. 
# Have in mind that whenever at least on of a pair of columns that are being tested 
# have NA entries, the corresponding row is excluded by the chi-squared test, i.e.
# the chi-sared function considers only rows in twhich both columns that are being 
# tested have values 0 or 1 and no NA
# Warnings of innacuracy may appear due to the fact that a whole row or a whole column 
# of entries of the binary 2 x 2 chi-sqared freqiuency table contins zeros, 
# when all the entries of a column variable are all 0 or all 1, etc
######################################################################################

####################### EXECUTE: #####################################################

T_chi_sq <- chi_sq_table(table_1, list_1, table_2, list_2)

######################################################################################

