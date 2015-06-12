## Body Surface Area Calculation

paed_airprom_pheno = read.csv(file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/GWAS/airprom_exchange/Nick\ Shrine/ubiopred_paedeatric_phenotype_data.csv")

# Formula
paed_airprom_pheno$bsa = sqrt((as.numeric(as.character(paed_airprom_pheno$Height_.cm.)) * as.numeric(as.character(paed_airprom_pheno$Weight_.kg.)) / 3600))

write.csv(paed_airprom_pheno, "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/GWAS/airprom_exchange/Nick\ Shrine/ubiopred_paedeatric_phenotype_data_v2.csv", row.names = F)
