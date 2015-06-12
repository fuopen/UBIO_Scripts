## 20150512 Adding screening dates to freetext medication data for Ana Sousa##

#Excel file import
adu
## Kids:

#Screening_info for paed cohort
dt_screen_paed = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/Longitutinal_Data/20150225/dt_screen.xlsx")

#Freetext medication data for paed cohort
paed_freetxt_med = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/Longitutinal_Data/Medication/20150512_paed_free_txt_meds_v2.xlsx")

#Subset only codes and visit dates
paed_screen_dates = subset(dt_screen_paed,,select=c(patient_code, visit_date))

#Append screening dates to new df
paed_free_txt_meds_v3 <- merge(paed_freetxt_med, paed_screen_dates, by="patient_code")

#Write the same output to file
!#write.csv(paed_free_txt_meds_v3, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/Longitutinal_Data/Medication/20150512_paed_free_txt_meds_v3.csv", row.names=F)


################################################################
## Adults

#Screening_info for adult cohort
dt_screen = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/dt_screen.xlsx")

#Freetext medication data for adult cohort
adult_freetxt_med = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/Medication/20150512_adult_free_txt_meds_v2.xlsx")

#Subset only codes and visit dates
adult_screen_dates = subset(dt_screen,,select=c(patient_code, visit_date))

#Append screening dates to new df
adult_free_txt_meds_v3 <- merge(adult_freetxt_med, adult_screen_dates, by="patient_code")

#Write the same output to file
!#write.csv(adult_free_txt_meds_v3, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/Medication/20150512_adult_free_txt_meds_v3.csv", row.names = F)

################################
