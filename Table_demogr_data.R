source("C:\\MY_FOLDERS\\Asthma_and_Pain\\R_code\\Table_creation\\Set_of_functions.R")

#If you would like to extract additional data, such as demographic and pricniple components from UKB, use this script
#The tables are extracted "as they are" columns are selected as desired
#When selecting a subset of the fields, make sure the order of the fileds is in ascending order 
#(as it is in the raw UKB table) and the array lengths, the instances and the lables correpsond to the respective fields
# also make sure, that the fields you are choosing are present in the raw UKB data table at pathfile_in

#################  INPUTS FOR ADDITIONAL DATA, E.G. DEMOGRAPHIC INFO  ##############################

pathfile_in <- "write the path of your intput file"

fields <- c('f.31.', 'f.21000.', 'f.21003.', 'f.22001.', 'f.22006.', 'f.22009.', 'f.22010.', 'f.22018.')    
arrays_length <- c(1, 1, 1, 1, 1, 40, 1, 1)
instances   <-   c(1, 3, 3, 1, 1,  1, 1, 1)
labels <- c("Sex", "Ethnic_backgr",  "Age_at_Visit", "Genetic_sex",
            "Gen_ethnic_grp", "PC", "Geno_analys_exclns",
            "Relat_exclns")

#fields <- c(21000, 22006, 22009)    
#arrays_length <- c( 1, 1, 40)
#instances   <-   c( 3, 1,  1)
#labels <- c("Ethnic_backgr",  "Gen_ethnic_grp", "PC")

filepath_out <- "write the path of your output file"
####################################################################################################

############################## EXECUTE #############################################################
t_demogr_geno_info <- fread(pathfile_in) #502599
t_demogr_geno_info <- extract_subtable(t_demogr_geno_info, fields)
names(t_demogr_geno_info) <- relabel(t_demogr_geno_info, fields, array_length, instances, labels)
names(t_demogr_geno_info)[1] <- 'ID'

asth_rhin_ecz_demogr <- left_join(t20002_bin, t_demogr_geno_info, by = "ID")

write.table(asth_rhin_ecz_demogr,
            filepath_out, 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

####################################################################################################

