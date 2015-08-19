# Long Sputum

# !!!NB Delete and reorder the columns in excel after writing files

sputum = read.delim("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/sputum_long.txt", header = T)

merge(x = sputum, y = adult_pseudo_code[ , c("subjid","long_kit")], by.x = "Cytospins.UBIOPRED", by.y = "long_kit", all=F) -> sputum


sputum_lt1 = read.delim("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/sputum_long_lt1.txt", header = T)

merge(x = sputum_lt1, y = adult_pseudo_code[ , c("subjid","long_kit")], by.x = "Cytospins.UBIOPRED", by.y = "long_kit", all=F) -> sputum_lt1

sputum_lt2 = read.delim("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/sputum_long_lt2.txt", header = T)

merge(x = sputum_lt2, y = adult_pseudo_code[ , c("subjid","long_kit")], by.x = "Cytospins.UBIOPRED", by.y = "long_kit", all=F) -> sputum_lt2

rownames(sputum_lt1) <- sputum_lt1$subjid

rownames(sputum_lt2) <- sputum_lt2$subjid

aggregate(sputum, by = list(Subjid = sputum$subjid), FUN = mean, na.rm=T) -> sputum_unique

sputum_lt1$Cytospins.UBIOPRED = NULL
sputum_lt2$Cytospins.UBIOPRED = NULL
sputum_unique$Cytospins.UBIOPRED = NULL



write.table(sputum_lt1, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/sputum_long_lt1_load.txt", sep = "\t", row.names= F, na = ".", quote =F)

write.table(sputum_lt2, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/sputum_long_lt2_load.txt", sep = "\t", row.names= F, na = ".", quote =F)

write.table(sputum_unique, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/sputum_long_unique_load.txt", sep = "\t", row.names= F, na = ".", quote =F)


