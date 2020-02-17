#!/usr/bin/env Rscript

library(dplyr)
library(data.table)

extract_subtable <- function(data_base, list_of_codes){
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

#########################################################################################################


filename <- "/mnt/nfs/backup/data/uk_biobank/ukb22741.r.tab" #string
data_base <- fread(filename)
subtable <- extract_subtable(data_base, list_of_codes)
write.table(subtable, "/home/ndimit2/Asthma_and_Pain/Output_data_ph1/ukb6159.txt", append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

#To run this from the UNIX command line, type and execute: 
# chmod +x table_extraction.r
# ./table_extraction.r world
