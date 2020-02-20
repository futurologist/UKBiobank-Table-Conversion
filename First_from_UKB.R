#!/usr/bin/env Rscript

library(dplyr)
library(data.table)

extract_UKB_subtable <- function(data_base, list_of_codes){
  headers <- names(data_base)
  pos <- c(1)
  for(code in list_of_codes){
    pos <- c(pos, grep(code, headers))
  } 
  subt <- data_base[, ..pos]
  subt <- subt[order(subt[,'f.eid']), ]
  subt
}


# For a set of fields that need to be extracted from the central UKB data table
# list a vector of strings, each string in the format "f.[Field_number].", for each field.
# Skip the first column, id column "f.eid", it is authomatically included

################################### YOUR INPUT GOES HERE: ###############################################

list_of_codes <- c("f.6159.", "f.2956.", "f.3404.", "f.3414.", "f.3571.", "f.3741.", "f.3773.", "f.3799.", "f.4067.")
#list_of_codes <- c("f.135.", "f.20002.", "f.20009.")
#list_of_codes <- c("f.137.", "f.20003.")

filepath_in <- "/mnt/nfs/backup/data/uk_biobank/ukb22741.r.tab" #string
filepath_out <- "/home/ndimit2/Asthma_and_Pain/Output_data_ph1/ukb6159.txt"
#########################################################################################################

data_base <- fread(filepath_in)
subtable <- extract_UKB_subtable(data_base, list_of_codes)
write.table(subtable, filepath_out, append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

#To run this from the UNIX command line, type and execute: 
# chmod +x table_extraction.r
# ./table_extraction.r world
