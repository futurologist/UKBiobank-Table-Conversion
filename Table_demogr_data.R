source("C:\\MY_FOLDERS\\Asthma_and_Pain\\R_code\\Table_creation\\Set_of_functions.R")

#If you would like to extract additional data, such as demographic and pricniple components from UKB, use this script
#The tables are extracted "as they are" columns are selected as desired and 
#################  INPUTS FOR ADDITIONAL DATA, E.G. DEMOGRAPHIC INFO  ##############################

pathfile_in <- "C:\\MY_FOLDERS\\Asthma_and_Pain\\Test_data\\ukb_demogr_geno_info.txt" 

#fields <- c(31, 21000, 21003, 22001, 22006, 22009, 22010, 22018)    
#arrays_length <- c(1, 1, 1, 1, 1, 40, 1, 1)
#instances   <-   c(1, 3, 3, 1, 1,  1, 1, 1)
#labels <- c("Sex", "Ethnic_backgr",  "Age_at_Visit", "Genetic_sex",
#            "Gen_ethnic_grp", "PC", "Geno_analys_exclns",
#            "Relat_exclns")

fields <- c(21000, 22006, 22009)    
arrays_length <- c( 1, 1, 40)
instances   <-   c( 3,  1, 1)
labels <- c("Ethnic_backgr",  "Gen_ethnic_grp", "PC")
####################################################################################################

############################## EXECUTE #############################################################
t_demogr_geno_info <- fread(pathfile_in) #502599
t_demogr_geno_info <- extract_subtable(t_demogr_geno_info, fields)
names(t_demogr_geno_info) <- relabel(t_demogr_geno_info, fields, array_length, instances, labels)
names(t_demogr_geno_info)[1] <- 'ID'
