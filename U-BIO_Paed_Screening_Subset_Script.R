#Author: Ioannis Pandis

# Script with parses the U-BIOPRED adult Screening file and subsets it, based on a list of subjects which is user defined

# Read data in
screening <- read.csv("~/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/March_2014/raw_data/2014-03-24-pead_screening.csv")

# Set subject id's (sujid) as row index
row.names(screening) <- screening$subjid

# list of subjects which is user defined
paed_list = c("P_074","P_145","P_247")

# Lookup and subset
screening[match(paed_list,row.names(screening)),] -> screening_subset

# Write the file
write.csv(screening_subset, file = "paed_subset.csv", row.names=F)