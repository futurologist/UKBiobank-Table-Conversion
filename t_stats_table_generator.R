library(data.table)
library(dplyr)

# import data for Asthma age groups
# import data for Chronic pain, binary, have pain do not have pain

##################### Important functions: #####################

remove_excl <- function(dt, ex){ 
  colnames(ex) <- colnames(dt)[1]
  ex <- setdiff(dt[,1], ex, by=colnames(ex) ) 
  dt <- left_join(ex, dt, by=colnames(ex))
  return(dt)
}

build_stats_table <- function(dt_1, dt_2, row_names, col_names){
  names(dt_2)[1] <- names(dt_1)[1]
  dt <- inner_join(dt_1, dt_2, by=names(dt_1)[1])
  calc_stats <- function(label){
    # label stands for variable, like pollution, r stands for case-controll binary variable, like has asthma or doesn't
    t <- t.test( dt[,get(label)] ~ dt[,get(r)], # quantitative variable ~ binary case-controll segmentation
                 alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
    
    #tt <- c(tt[[c(1:3)]], tt[[4]], tt[[5]], tt[])
    return( list(t[[1]][[1]], t[[2]][[1]],t[[3]][[1]],t[[4]][[1]], t[[4]][[2]], 
                 t[[5]][[1]], t[[5]][[2]], t[[6]][[1]], t[[7]]) ) #, t[[8]], t[[9]], t[[10]])
  }
  
  dt_stats <- NULL
  table_names <- NULL
  names_stats <- c("statistic", "parameter.df",  "p.value", "conf.int.lower.bound", "conf.int.upper.bound",
                   "mean.controls", "mean.cases", "null.hyp.difference.of.means", "stderr") 
  
  for( r in row_names ){
    dt_stats_row <-  as.data.table( c(r, do.call(c, lapply(col_names, calc_stats)) ) )
    #dt_stats_row <- dt_stats_row[ , := ]
    dt_stats <- bind_rows(dt_stats, dt_stats_row)
  }
  
  for(cl in col_names){
    table_names <-  c(table_names, paste( names_stats, '_', cl, sep=''))
  }
  
  names(dt_stats) <- c( 'med_condition', table_names)
  return( dt_stats )
  
}

build_simple_stats_table <- function(dt_1, dt_2, row_names, col_names){
  
  names(dt_2)[1] <- names(dt_1)[1]
  dt <- inner_join(dt_1, dt_2, by=names(dt_1)[1])
  
  calc_stats <- function(lbl){
    dt_label <- dt[!is.na(get(r)) & !is.na(get(lbl)),][,get(lbl)]
    return( list(mean(dt_label), sd(dt_label), min(dt_label), max(dt_label), median(dt_label)) )
  }
  
  dt_stats <- NULL
  table_names <- NULL
  #names_stats <- c("mean", "sd",  "min", "max", "median") 
  
  for( r in row_names ){
    dt_stats_row <-  as.data.table( c(r, do.call(c, lapply(col_names, calc_stats)) ) )
    dt_stats <- bind_rows(dt_stats, dt_stats_row)
  }
  
  for(cl in col_names){
    table_names <-  c(table_names, paste( c('mean', 'sd', 'min', 'max', 'median'), '_', cl, sep=''))
  }
  
  names(dt_stats) <- c( 'med_condition', table_names)
  return( dt_stats )
}

################### Importing and formatting relevant datasets, plus removing excluded samples: #####################

# ethn is uploded in order to  extract all Europeans from UKB calculated by using the genetic data of the bgen sample
ethn <- fread('F:\\NIKO\\Genomics\\UK_Biobank\\ethnicity_and_samples\\urb_rur_ethn.txt', header=TRUE)
# excl are the excluded samples:
excl <- fread('F:\\NIKO\\Genomics\\UK_Biobank\\ethnicity_and_samples\\exclusions_2021_02_02.txt', header=FALSE)
# import the pollution data:
poll <- fread('F:\\NIKO\\Genomics\\UK_Biobank\\Phenotypes\\air_pollution\\pollution_vars.tsv', header=TRUE)
# import the full sample from the bgen files: 
bgen_sample <- fread('F:\\NIKO\\Genomics\\UK_Biobank\\ethnicity_and_samples\\ukb_bgen_sample.txt', header=FALSE)
names(bgen_sample)[1] <- 'IID'

bgen_sample <- remove_excl(bgen_sample, excl)

poll <- remove_excl(poll, excl)
ethn <- remove_excl(ethn, excl)

ids_bgen_eur <- intersect(ethn[European==1,][,'IID'], bgen_sample)
ids_bgen_eur <- ids_bgen_eur[order(IID),]

colnames_poll <- colnames(poll)[c(2:ncol(poll))] 

################### Start with Asthma: ###################

A <- fread('H:\\Genomics\\Resources\\Output_data_ph2\\asthma_age_groups.txt', header=TRUE)


cols <- which(grepl('v0', colnames(A)))
cols <- c(1, cols)
# remove all samples with NA because they have no info for asthma at all:
A <- A[, ..cols][!is.na(Asthma_v0),]
# generate Asthma 8 group from Asthma 6 and 4:
A <- A[ , Asthma_Age_8_v0:=ifelse(Asthma_v0==0, 0, ifelse(Asthma_Age_4_v0 == 1 | Asthma_Age_6_v0 == 1, 1, NA))]

A <- remove_excl(A, excl)
colnames(A)[1] <- 'IID'

### build the table of t statistics data:

colnames_A <- colnames(A)[c(2:ncol(A))] 

stats_table <- build_stats_table(A, poll, row_names=colnames_A, col_names=colnames_poll)

stats_table_bgen_eur <- build_stats_table(inner_join(ids_bgen_eur, A, by='IID'), poll, row_names=colnames_A, col_names=colnames_poll)

simple_stats_table <- build_simple_stats_table(A, poll, row_names=colnames_A, col_names=colnames_poll)

simple_stats_table_bgen_eur <- build_simple_stats_table(inner_join(ids_bgen_eur, A, by='IID'), poll, row_names=colnames_A, col_names=colnames_poll)


################### Continue with Pain: ###################

##### Import and proccess the pain dataset:

P <- fread('F:\\NIKO\\Genomics\\UK_Biobank\\Output_data_ph2\\pain_duration_2021_02_04.txt', header=TRUE)

cols <- which(grepl('v0', colnames(P)))
cols <- c(1, cols)
P <- P[, ..cols][!(Prf_no_Ansr_v0 == 1 | is.na(Prf_no_Ansr_v0)),]
cols <- ( c(1, c(3: length(names(P)))) )
P <- P[, ..cols]
colnames(P)[2] <- 'No_pain_v0'

P <- remove_excl(P, excl)
colnames(P)[1] <- 'IID'

####### Build the pain pheontypes: chronic and acute

cols <- which(grepl("m_v0", colnames(P)) )

colnames(P)[ cols ]  <-  c("All_over_3m_v0", 
                          "Neck_Shldr_pn_3m_v0", "Hip_pn_3m_v0",        
                          "Back_pn_3m_v0", "Stom_Abdmn_pn_3m_v0",
                          "Knee_pn_3m_v0", "Headch_3m_v0", "Face_pn_3m_v0")

cols <- colnames(P)[ cols ]

P <- P[order(IID),]
# all controls, which otherwise were NA, now are set to 3
P[No_pain_v0==1 & is.na(P),] <- 3 
ids <- P[,'IID']

P_chr <- P[,..cols]
P_acu <- P[,..cols]

cols <- c("All_over_v0", 
          "Neck_Shldr_pn_v0", "Hip_pn_v0",        
          "Back_pn_v0", "Stom_Abdmn_pn_v0",
          "Knee_pn_v0", "Headch_v0", "Face_pn_v0")


names(P_chr) <-  paste(cols, '_chronic', sep='')
names(P_acu) <-  paste(cols, '_acute', sep='')


# chr==1 and acc==0 and control==3 else -3, -1, NA

# chronic pain: chr==1 control==3 and unknown==-3 or -1 or NA 
P_chr[P_chr!=1 & P_chr!=3] <- NA  
P_chr[P_chr==3] <- 0  
P_chr <- bind_cols(ids, P_chr)

# acute pain: acute==0 control==3 and unknown==-3 or -1 or NA 
P_acu[P_acu!=0 & P_acu!=3] <- NA 
P_acu[P_acu==0] <- 1 
P_acu[P_acu==3] <- 0  
P_acu <- bind_cols(ids, P_acu)

write.table(P_chr,'F:\\NIKO\\Genomics\\UK_Biobank\\Output_data_ph2\\pain_chronic_2021_02_04.txt', 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

write.table(P_acu,'F:\\NIKO\\Genomics\\UK_Biobank\\Output_data_ph2\\pain_acute_2021_02_04.txt', 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

####### Bulid the table of t-stats for the pain phenotypes and pollution:

colnames_P_chr <- names(P_chr)[c(2: ncol(P_chr))]

stats_table_pain <- build_stats_table(P_chr, poll, row_names=colnames_P_chr, col_names=colnames_poll)

stats_table_pain_bgen_eur <- build_stats_table(inner_join(ids_bgen_eur, P_chr, by='IID'), poll, row_names=colnames_P_chr, col_names=colnames_poll)

stats_table <- bind_rows(stats_table, stats_table_pain)

stats_table_bgen_eur <- bind_rows(stats_table_bgen_eur, stats_table_pain_bgen_eur)


simple_stats_table_pain <- build_simple_stats_table(P_chr, poll, row_names=colnames_P_chr, col_names=colnames_poll)

simple_stats_table_pain_bgen_eur <- build_simple_stats_table(inner_join(ids_bgen_eur, P_chr, by='IID'), poll, row_names=colnames_P_chr, col_names=colnames_poll)

simple_stats_table <- bind_rows(simple_stats_table, simple_stats_table_pain)

simple_stats_table_bgen_eur <- bind_rows(simple_stats_table_bgen_eur, simple_stats_table_pain_bgen_eur)




colnames_P_acu <- names(P_acu)[c(2: ncol(P_acu))]

stats_table_pain <- build_stats_table(P_acu, poll, row_names=colnames_P_acu, col_names=colnames_poll)

stats_table_pain_bgen_eur <- build_stats_table(inner_join(ids_bgen_eur, P_acu, by='IID'), poll, row_names=colnames_P_acu, col_names=colnames_poll)

stats_table <- bind_rows(stats_table, stats_table_pain)

stats_table_bgen_eur <- bind_rows(stats_table_bgen_eur, stats_table_pain_bgen_eur)

simple_stats_table_pain <- build_simple_stats_table(P_acu, poll, row_names=colnames_P_acu, col_names=colnames_poll)

simple_stats_table_pain_bgen_eur <- build_simple_stats_table(inner_join(ids_bgen_eur, P_acu, by='IID'), poll, row_names=colnames_P_acu, col_names=colnames_poll)

simple_stats_table <- bind_rows(simple_stats_table, simple_stats_table_pain)

simple_stats_table_bgen_eur <- bind_rows(simple_stats_table_bgen_eur, simple_stats_table_pain_bgen_eur)


write.table(stats_table,'F:\\NIKO\\Genomics\\UK_Biobank\\Sumstats\\t_tests_pollution_in_asthma_pain\\pollution_in_asthma_pain_t_stats.txt', 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

write.table(stats_table_bgen_eur,'F:\\NIKO\\Genomics\\UK_Biobank\\Sumstats\\t_tests_pollution_in_asthma_pain\\pollution_in_asthma_pain_eur_t_stats.txt', 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

write.table(simple_stats_table,'F:\\NIKO\\Genomics\\UK_Biobank\\Sumstats\\t_tests_pollution_in_asthma_pain\\pollution_in_asthma_pain_simple_stats.txt', 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

write.table(simple_stats_table_bgen_eur,'F:\\NIKO\\Genomics\\UK_Biobank\\Sumstats\\t_tests_pollution_in_asthma_pain\\pollution_in_asthma_pain_eur_simple_stats.txt', 
            append = FALSE, sep = "\t", quote = FALSE, col.names=TRUE, row.names=FALSE)

