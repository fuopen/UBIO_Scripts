#Author: Ioannis Pandis

# Script with parses the U-BIOPRED adult Screening file and subsets it, based on a list of subjects which is user defined

# Read data in
alldata <- read.csv("~/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/May2014/raw/2014-05-16-adult_screening.csv")

# Set subject id's (sujid) as row index
row.names(alldata) <- alldata$subjid

# list of subjects which is user defined
adult_subj_list <- c("A_547","A_675","A_700")

# Lookup and subset
alldata[match(adult_subj_list,row.names(alldata)),] -> adult_subset

# Write the file
write.csv(adult_subset, file = "adult_subset.csv", row.names=F)