## Script to map subjid to longitudinal periostin data set and write the loading file

#Read the dataset
periostin_long = read.csv("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Periostin_Data/Longitudinal/UBIOPRED_Periostin\ Longitudinal\ data_v2.csv")

#Read Adult Pseudocodes
adult_pseudo_code = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/Adult_Subject_List_FINAL_20141127+kitIDs.xlsx")

#Read longitudinal_ids
long_pseuso_tmp = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/adult_longitudinal_ids.xlsx")

#Subset longitudinal_ids frame
long_id_tmp2 = subset(long_pseuso_tmp,,c(box_id, pseudo_id))

#Merge based on subjid
test = merge(adult_pseudo_code,long_id_tmp2, by.x = "subjid", by.y = "pseudo_id", all=T)

# Cut out all of null rows
test2 = test[1:620,]

#Delete subjid.1 column
test2$subjid.1 = NULL

#Rename column
library(plyr)
test2 = rename(test2, c("box_id"="long_kit"))

#Cast kitID to string
test2$long_kit = as.character(test2$long_kit)

#Save the complete pseudo db
adult_pseudo_code = test2

#Add subjid to periostin_long DF by merging
test = merge(periostin_long,adult_pseudo_code, by.x="Kit.ID", by.y="long_kit")

#Check for duplicates
unique(test$subjid)

#Create loading file
periostin_long_load = subset(periostin_long,,select=c(subjid, Value))
write.table(periostin_long_load, file = "/Users/ip304/Documents/UBIOPRED/Server_upload_folder/periostin_long.txt", row.names = FALSE, sep = "\t", quote=F)



