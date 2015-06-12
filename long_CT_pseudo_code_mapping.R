# Mapping pseudo-codes to CT scan data

long_ct_ids = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/GWAS/airprom_exchange/UBIOPRED\ longitudinal\ scans\ 2015-05-28.xlsx")

long_ct_pseudo = merge(long_ct_ids,small_pseudo, by = "study_id", all=F)

long_ct_pseudo$study_id = NULL
long_ct_pseudo$Doctor = NULL
long_ct_pseudo$Date = NULL
long_ct_pseudo$site = NULL

write.csv(long_ct_pseudo, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/GWAS/airprom_exchange/UBIOPRED\ longitudinal\ scans\ 2015-05-29_IP.csv", row.names = F)
  
  