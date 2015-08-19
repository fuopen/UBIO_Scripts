# Derived variable calcs

# Exac2

library(gdata)

adult_exac2 = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/dt_exac2.xlsx", na.strings = c("999", "999.99",""))

adult_exac2$comment = NULL
adult_exac2$investigator =NULL
adult_exac2$principal_investigator = NULL


# levels(adult_exac2$c_reactive_protein)[levels(adult_exac2$c_reactive_protein)=='< 5'] <- "4.99"
# levels(adult_exac2$c_reactive_protein)[levels(adult_exac2$c_reactive_protein)=='<5'] <- "4.99"
# levels(adult_exac2$c_reactive_protein)[levels(adult_exac2$c_reactive_protein)=='<1'] <- "0.99"
# levels(adult_exac2$c_reactive_protein)[levels(adult_exac2$c_reactive_protein)==''] <- NA
# adult_exac2$c_reactive_protein <- as.numeric(levels(adult_exac2$c_reactive_protein)[adult_exac2$c_reactive_protein])

names(adult_exac2)[1] <- "study_id"

data = read.table(file="/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/2014-05-16-adult_clinical_scr_bd1.txt", header = TRUE, sep = "\t")

adult_predicted_lung_functions = subset(data,,select = c(subjid, fev1_predicted, fvc_predicted, fev1_fvc_ratio_predicted,age_at_consent,fef2575_predicted,tlc_actual_predicted_perc,weight,height) )
names(adult_predicted_lung_functions)[2] <- "fev1_predicted_all"
names(adult_predicted_lung_functions)[3] <- "fvc_predicted_all"
names(adult_predicted_lung_functions)[4] <- "fev1_fvc_ratio_predicted_all"
names(adult_predicted_lung_functions)[6] <- "fef2575_predicted_all"

# 
# adult_psedo = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/Adult_Subject_List_FINAL_20141127+kitIDs.xlsx")
# adult_psedo$subjid.1 = NULL

adult_exac2 = merge(adult_exac2,adult_psedo,by = "study_id", ALL = F)
adult_exac2 = merge(adult_exac2,adult_predicted_lung_functions,by = "subjid", ALL = F)

adult_exac2$tlc_actual_predicted_perc[adult_exac2$tlc_actual_predicted_perc == '.'] <- 'NA'
adult_exac2$tlc_actual_predicted_perc <- as.numeric(levels(adult_exac2$tlc_actual_predicted_perc)[adult_exac2$tlc_actual_predicted_perc])

#Harmonise Units for pef variables
summary(adult_exac2$pef_pre_salbutamol)
timeAdjust=1*(adult_exac2$pef_pre_salbutamol_um == "l_sec")+60*(adult_exac2$pef_pre_salbutamol_um == "l_min")
adult_exac2$pef_pre_salbutamol=round(adult_exac2$pef_pre_salbutamol/timeAdjust,digit=2)
summary(adult_exac2$pef_pre_salbutamol)

summary(adult_exac2$pef_actual)
timeAdjust=1*(adult_exac2$pef_actual_um == "l_sec")+60*(adult_exac2$pef_actual_um == "l_min")
adult_exac2$pef_actual=round(adult_exac2$pef_actual/timeAdjust,digit=2)
summary(adult_exac2$pef_actual)

summary(adult_exac2$pef_predicted)
timeAdjust=1*(adult_exac2$pef_predicted_um == "l_sec")+60*(adult_exac2$pef_predicted_um == "l_min")
adult_exac2$pef_predicted=round(adult_exac2$pef_predicted/timeAdjust, digit=2)
summary(adult_exac2$pef_predicted)

#fev1
adult_exac2$fev1_pre_salbutamol_percentage <- round(adult_exac2$fev1_pre_salbutamol/adult_exac2$fev1_predicted_all*100,digit=2)
adult_exac2$fev1_post_salbutamol_percentage <- round(adult_exac2$fev1_post_salbutamol/adult_exac2$fev1_predicted_all*100,digit=2)
adult_exac2$fev1_abs_change <- round(100*(adult_exac2$fev1_post_salbutamol-adult_exac2$fev1_pre_salbutamol)/adult_exac2$fev1_predicted_all,digit=2)

#fvc
adult_exac2$fvc_pre_salbutamol_percentage <- round(adult_exac2$fvc_pre_salbutamol/adult_exac2$fvc_predicted_all*100,digit=2)
adult_exac2$fvc_post_salbutamol_percentage <- round(adult_exac2$fvc_post_salbutamol/adult_exac2$fvc_predicted_all*100,digit=2)
adult_exac2$fvc_abs_change <- round(100*(adult_exac2$fvc_post_salbutamol-adult_exac2$fvc_pre_salbutamol)/adult_exac2$fvc_predicted_all,digit=2)

#fev1/fvc percentage
adult_exac2$pre_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_exac2$fev1_pre_salbutamol/adult_exac2$fvc_pre_salbutamol,digit=2)
adult_exac2$post_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_exac2$fev1_post_salbutamol/adult_exac2$fvc_post_salbutamol,digit=2)
# a3 <- c(-0.18,-0.19)
# b3 <- c(87.21,89.1)
# names(a3)=names(a1)
# names(b3)=names(a1)
# adult_exac2$fev1_fvc_ratio_predicted <- a3[adult_exac2$gender]*adult_exac2$age_at_consent+b3[adult_exac2$gender]
adult_exac2$pre_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_exac2$pre_salbutamol_fev1_fvc_ratio_actual/adult_exac2$fev1_fvc_ratio_predicted_all,digit=2)
adult_exac2$post_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_exac2$post_salbutamol_fev1_fvc_ratio_actual/adult_exac2$fev1_fvc_ratio_predicted_all,digit=2)


#fef2575 percentage
adult_exac2$fef2575_pre_salbutamol_percentage <- round(adult_exac2$fef2575_pre_salbutamol/adult_exac2$fef2575_predicted_all*100,digit=2)
adult_exac2$fef2575_post_salbutamol_percentage <- round(adult_exac2$fef2575_post_salbutamol/adult_exac2$fef2575_predicted_all*100,digit=2)
adult_exac2$fef2575_abs_change <- round(100*(adult_exac2$fef2575_post_salbutamol-adult_exac2$fef2575_pre_salbutamol)/adult_exac2$fef2575_predicted_all,digit=2)



#airflow_limit_quanjer

##fev1_fvc_ratio_LLN
for (i in 1:length(row.names(adult_exac2))){
  if(adult_exac2$gender[i] == 'male'){
    adult_exac2$fev1_fvc_ration_LLN[i] = (-0.18*adult_exac2$age_at_consent[i])+75.41
  }
  if(adult_exac2$gender[i] == 'female'){
    adult_exac2$fev1_fvc_ration_LLN[i] = (-0.19*adult_exac2$age_at_consent[i])+78.4
  }
}
rm(i)

for (i in 1:length(row.names(adult_exac2))){
  if(is.na(adult_exac2$post_salbutamol_fev1_fvc_ratio_actual[i])){
    adult_exac2$airflow_limit_quanjer[i] <- 'NA'
  }
  if(!is.na(adult_exac2$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_exac2$post_salbutamol_fev1_fvc_ratio_actual[i] < adult_exac2$fev1_fvc_ration_LLN[i]){
    adult_exac2$airflow_limit_quanjer[i] <- 'yes'
  }
  if(!is.na(adult_exac2$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_exac2$post_salbutamol_fev1_fvc_ratio_actual[i] > adult_exac2$fev1_fvc_ration_LLN[i]){
    adult_exac2$airflow_limit_quanjer[i] <- 'no'
  }
}
rm(i)  

#airflow_limit_brinke

airflow_limit_brinke = 'NA'
sel1 <- !is.na(adult_exac2$post_salbutamol_fev1_fvc_ratio_percentage) & adult_exac2$post_salbutamol_fev1_fvc_ratio_percentage < 75
sel2 <- !is.na(adult_exac2$fev1_post_salbutamol_percentage) & adult_exac2$fev1_post_salbutamol_percentage < 75
sel3 <- !is.na(adult_exac2$tlc_actual_predicted_perc) & adult_exac2$tlc_actual_predicted_perc > 75
sel <- (sel1|sel2)&sel3
airflow_limit_brinke <- rep('no',nrow(adult_exac2))
airflow_limit_brinke[sel] <- 'yes'
adult_exac2$airflow_limit_brinke <- airflow_limit_brinke

#Blood cell count calculations

# adult_exac2$"neutrophils (x10^3/uL)" = round((adult_exac2$neutrophils * adult_exac2$wbcs) / 100, digit = 2)
# adult_exac2$"lymphocytes (x10^3/uL)" = round((adult_exac2$lymphocytes * adult_exac2$wbcs) / 100, digit = 2)
# adult_exac2$"monocytes (x10^3/uL)" = round((adult_exac2$monocytes * adult_exac2$wbcs) / 100, digit = 2)
# adult_exac2$"basophils (x10^3/uL)" = round((adult_exac2$basophils * adult_exac2$wbcs) / 100, digit = 2)
# adult_exac2$"eosinophils (x10^3/uL)" = round((adult_exac2$eosinophils * adult_exac2$wbcs) / 100, digit = 2)

# Body Surface Area (BSA)
adult_exac2$bsa = round(sqrt((adult_exac2$height * adult_exac2$weight) / 3600),digit = 2)



write.table(adult_exac2, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/dt_exac2.txt", sep = "\t", row.names= F, na = ".", quote =F)

################################
# Exac 1

# Adults

# library(gdata)
# 
# adult_exac1 = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/dt_exac1.xlsx", na.strings = c("999", "999.99",""))
# 
# adult_exac1$comment = NULL
# adult_exac1$investigator =NULL
# adult_exac1$principal_investigator = NULL
# 
# 
# # levels(adult_exac1$c_reactive_protein)[levels(adult_exac1$c_reactive_protein)=='< 5'] <- "4.99"
# # levels(adult_exac1$c_reactive_protein)[levels(adult_exac1$c_reactive_protein)=='<5'] <- "4.99"
# # levels(adult_exac1$c_reactive_protein)[levels(adult_exac1$c_reactive_protein)=='<1'] <- "0.99"
# # levels(adult_exac1$c_reactive_protein)[levels(adult_exac1$c_reactive_protein)==''] <- NA
# # adult_exac1$c_reactive_protein <- as.numeric(levels(adult_exac1$c_reactive_protein)[adult_exac1$c_reactive_protein])
# 
# names(adult_exac1)[1] <- "study_id"
# 
# # data = read.table(file="/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/2014-05-16-adult_clinical_scr_bd1.txt", header = TRUE, sep = "\t")
# 
# adult_predicted_lung_functions = subset(data,,select = c(subjid, fev1_predicted, fvc_predicted, fev1_fvc_ratio_predicted,age_at_consent,fef2575_predicted,tlc_actual_predicted_perc,weight,height) )
# names(adult_predicted_lung_functions)[2] <- "fev1_predicted_all"
# names(adult_predicted_lung_functions)[3] <- "fvc_predicted_all"
# names(adult_predicted_lung_functions)[4] <- "fev1_fvc_ratio_predicted_all"
# names(adult_predicted_lung_functions)[6] <- "fef2575_predicted_all"
# 
# # 
# # adult_psedo = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/Adult_Subject_List_FINAL_20141127+kitIDs.xlsx")
# # adult_psedo$subjid.1 = NULL
# 
# adult_exac1 = merge(adult_exac1,adult_psedo,by = "study_id", ALL = F)
# adult_exac1 = merge(adult_exac1,adult_predicted_lung_functions,by = "subjid", ALL = F)
# 
# adult_exac1$tlc_actual_predicted_perc[adult_exac1$tlc_actual_predicted_perc == '.'] <- 'NA'
# adult_exac1$tlc_actual_predicted_perc <- as.numeric(levels(adult_exac1$tlc_actual_predicted_perc)[adult_exac1$tlc_actual_predicted_perc])
# 
# #Harmonise Units for pef variables
# summary(adult_exac1$pef_pre_salbutamol)
# timeAdjust=1*(adult_exac1$pef_pre_salbutamol_um == "l_sec")+60*(adult_exac1$pef_pre_salbutamol_um == "l_min")
# adult_exac1$pef_pre_salbutamol=round(adult_exac1$pef_pre_salbutamol/timeAdjust,digit=2)
# summary(adult_exac1$pef_pre_salbutamol)
# 
# summary(adult_exac1$pef_actual)
# timeAdjust=1*(adult_exac1$pef_actual_um == "l_sec")+60*(adult_exac1$pef_actual_um == "l_min")
# adult_exac1$pef_actual=round(adult_exac1$pef_actual/timeAdjust,digit=2)
# summary(adult_exac1$pef_actual)
# 
# summary(adult_exac1$pef_predicted)
# timeAdjust=1*(adult_exac1$pef_predicted_um == "l_sec")+60*(adult_exac1$pef_predicted_um == "l_min")
# adult_exac1$pef_predicted=round(adult_exac1$pef_predicted/timeAdjust, digit=2)
# summary(adult_exac1$pef_predicted)
# 
# #fev1
# adult_exac1$fev1_pre_salbutamol_percentage <- round(adult_exac1$fev1_pre_salbutamol/adult_exac1$fev1_predicted_all*100,digit=2)
# adult_exac1$fev1_post_salbutamol_percentage <- round(adult_exac1$fev1_post_salbutamol/adult_exac1$fev1_predicted_all*100,digit=2)
# adult_exac1$fev1_abs_change <- round(100*(adult_exac1$fev1_post_salbutamol-adult_exac1$fev1_pre_salbutamol)/adult_exac1$fev1_predicted_all,digit=2)
# 
# #fvc
# adult_exac1$fvc_pre_salbutamol_percentage <- round(adult_exac1$fvc_pre_salbutamol/adult_exac1$fvc_predicted_all*100,digit=2)
# adult_exac1$fvc_post_salbutamol_percentage <- round(adult_exac1$fvc_post_salbutamol/adult_exac1$fvc_predicted_all*100,digit=2)
# adult_exac1$fvc_abs_change <- round(100*(adult_exac1$fvc_post_salbutamol-adult_exac1$fvc_pre_salbutamol)/adult_exac1$fvc_predicted_all,digit=2)
# 
# #fev1/fvc percentage
# adult_exac1$pre_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_exac1$fev1_pre_salbutamol/adult_exac1$fvc_pre_salbutamol,digit=2)
# adult_exac1$post_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_exac1$fev1_post_salbutamol/adult_exac1$fvc_post_salbutamol,digit=2)
# # a3 <- c(-0.18,-0.19)
# # b3 <- c(87.21,89.1)
# # names(a3)=names(a1)
# # names(b3)=names(a1)
# # adult_exac1$fev1_fvc_ratio_predicted <- a3[adult_exac1$gender]*adult_exac1$age_at_consent+b3[adult_exac1$gender]
# adult_exac1$pre_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_exac1$pre_salbutamol_fev1_fvc_ratio_actual/adult_exac1$fev1_fvc_ratio_predicted_all,digit=2)
# adult_exac1$post_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_exac1$post_salbutamol_fev1_fvc_ratio_actual/adult_exac1$fev1_fvc_ratio_predicted_all,digit=2)
# 
# 
# #fef2575 percentage
# adult_exac1$fef2575_pre_salbutamol_percentage <- round(adult_exac1$fef2575_pre_salbutamol/adult_exac1$fef2575_predicted_all*100,digit=2)
# adult_exac1$fef2575_post_salbutamol_percentage <- round(adult_exac1$fef2575_post_salbutamol/adult_exac1$fef2575_predicted_all*100,digit=2)
# adult_exac1$fef2575_abs_change <- round(100*(adult_exac1$fef2575_post_salbutamol-adult_exac1$fef2575_pre_salbutamol)/adult_exac1$fef2575_predicted_all,digit=2)
# 
# 
# 
# #airflow_limit_quanjer
# 
# ##fev1_fvc_ratio_LLN
# for (i in 1:length(row.names(adult_exac1))){
#   if(adult_exac1$gender[i] == 'male'){
#     adult_exac1$fev1_fvc_ration_LLN[i] = (-0.18*adult_exac1$age_at_consent[i])+75.41
#   }
#   if(adult_exac1$gender[i] == 'female'){
#     adult_exac1$fev1_fvc_ration_LLN[i] = (-0.19*adult_exac1$age_at_consent[i])+78.4
#   }
# }
# rm(i)
# 
# for (i in 1:length(row.names(adult_exac1))){
#   if(is.na(adult_exac1$post_salbutamol_fev1_fvc_ratio_actual[i])){
#     adult_exac1$airflow_limit_quanjer[i] <- 'NA'
#   }
#   if(!is.na(adult_exac1$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_exac1$post_salbutamol_fev1_fvc_ratio_actual[i] < adult_exac1$fev1_fvc_ration_LLN[i]){
#     adult_exac1$airflow_limit_quanjer[i] <- 'yes'
#   }
#   if(!is.na(adult_exac1$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_exac1$post_salbutamol_fev1_fvc_ratio_actual[i] > adult_exac1$fev1_fvc_ration_LLN[i]){
#     adult_exac1$airflow_limit_quanjer[i] <- 'no'
#   }
# }
# rm(i)  
# 
# #airflow_limit_brinke
#  
# airflow_limit_brinke = 'NA'
# sel1 <- !is.na(adult_exac1$post_salbutamol_fev1_fvc_ratio_percentage) & adult_exac1$post_salbutamol_fev1_fvc_ratio_percentage < 75
# sel2 <- !is.na(adult_exac1$fev1_post_salbutamol_percentage) & adult_exac1$fev1_post_salbutamol_percentage < 75
# sel3 <- !is.na(adult_exac1$tlc_actual_predicted_perc) & adult_exac1$tlc_actual_predicted_perc > 75
# sel <- (sel1|sel2)&sel3
# airflow_limit_brinke <- rep('no',nrow(adult_exac1))
# airflow_limit_brinke[sel] <- 'yes'
# adult_exac1$airflow_limit_brinke <- airflow_limit_brinke
# 
# #Blood cell count calculations
# 
# # adult_exac1$"neutrophils (x10^3/uL)" = round((adult_exac1$neutrophils * adult_exac1$wbcs) / 100, digit = 2)
# # adult_exac1$"lymphocytes (x10^3/uL)" = round((adult_exac1$lymphocytes * adult_exac1$wbcs) / 100, digit = 2)
# # adult_exac1$"monocytes (x10^3/uL)" = round((adult_exac1$monocytes * adult_exac1$wbcs) / 100, digit = 2)
# # adult_exac1$"basophils (x10^3/uL)" = round((adult_exac1$basophils * adult_exac1$wbcs) / 100, digit = 2)
# # adult_exac1$"eosinophils (x10^3/uL)" = round((adult_exac1$eosinophils * adult_exac1$wbcs) / 100, digit = 2)
# 
# # Body Surface Area (BSA)
# adult_exac1$bsa = round(sqrt((adult_exac1$height * adult_exac1$weight) / 3600),digit = 2)
# 
# 
# 
# write.table(adult_exac1, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/dt_exac11.txt", sep = "\t", row.names= F, na = ".", quote =F)

#########################################################




# Bronch_visit

# Adults

# library(gdata)
# 
# #Column contents deleted before reading due to parsing error cause of writing: new_biopsy_deviations, acq_not_completed, comment
# 
# adult_bronch = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/dt_bronc_comment_col_deleted.xlsx", strip.white=T ,na.strings = c("999", "999.99",""))
# 
# adult_bronch$comment = NULL
# 
# levels(adult_bronch$c_reactive_protein)[levels(adult_bronch$c_reactive_protein)=='< 5'] <- "4.99"
# levels(adult_bronch$c_reactive_protein)[levels(adult_bronch$c_reactive_protein)=='<5'] <- "4.99"
# levels(adult_bronch$c_reactive_protein)[levels(adult_bronch$c_reactive_protein)=='<1'] <- "0.99"
# levels(adult_bronch$c_reactive_protein)[levels(adult_bronch$c_reactive_protein)==''] <- NA
# adult_bronch$c_reactive_protein <- as.numeric(levels(adult_bronch$c_reactive_protein)[adult_bronch$c_reactive_protein])
# 
# names(adult_bronch)[1] <- "study_id"
# 
# # data = read.table(file="/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/2014-05-16-adult_clinical_scr_bd1.txt", header = TRUE, sep = "\t")
# 
# adult_predicted_lung_functions = subset(data,,select = c(subjid, fev1_predicted, fvc_predicted, fev1_fvc_ratio_predicted,age_at_consent,fef2575_predicted,tlc_actual_predicted_perc,weight,height) )
# names(adult_predicted_lung_functions)[2] <- "fev1_predicted_all"
# names(adult_predicted_lung_functions)[3] <- "fvc_predicted_all"
# names(adult_predicted_lung_functions)[4] <- "fev1_fvc_ratio_predicted_all"
# names(adult_predicted_lung_functions)[6] <- "fef2575_predicted_all"
# 
# 
# adult_psedo = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/Adult_Subject_List_FINAL_20141127+kitIDs.xlsx")
# adult_psedo$subjid.1 = NULL
# 
# adult_bronch = merge(adult_bronch,adult_psedo,by = "study_id", ALL = F)
# adult_bronch = merge(adult_bronch,adult_predicted_lung_functions,by = "subjid", ALL = F)
# 
# adult_bronch$tlc_actual_predicted_perc[adult_bronch$tlc_actual_predicted_perc == '.'] <- 'NA'
# adult_bronch$tlc_actual_predicted_perc <- as.numeric(levels(adult_bronch$tlc_actual_predicted_perc)[adult_bronch$tlc_actual_predicted_perc])
# 
# #Harmonise Units for pef variables
# summary(adult_bronch$pef_pre_salbutamol)
# timeAdjust=1*(adult_bronch$pef_pre_salbutamol_um == "l_sec")+60*(adult_bronch$pef_pre_salbutamol_um == "l_min")
# adult_bronch$pef_pre_salbutamol=round(adult_bronch$pef_pre_salbutamol/timeAdjust,digit=2)
# summary(adult_bronch$pef_pre_salbutamol)
# 
# summary(adult_bronch$pef_actual)
# timeAdjust=1*(adult_bronch$pef_actual_um == "l_sec")+60*(adult_bronch$pef_actual_um == "l_min")
# adult_bronch$pef_actual=round(adult_bronch$pef_actual/timeAdjust,digit=2)
# summary(adult_bronch$pef_actual)
# 
# summary(adult_bronch$pef_predicted)
# timeAdjust=1*(adult_bronch$pef_predicted_um == "l_sec")+60*(adult_bronch$pef_predicted_um == "l_min")
# adult_bronch$pef_predicted=round(adult_bronch$pef_predicted/timeAdjust, digit=2)
# summary(adult_bronch$pef_predicted)
# 
# #fev1
# adult_bronch$fev1_pre_salbutamol_percentage <- round(adult_bronch$fev1_pre_salbutamol/adult_bronch$fev1_predicted_all*100,digit=2)
# adult_bronch$fev1_post_salbutamol_percentage <- round(adult_bronch$fev1_post_salbutamol/adult_bronch$fev1_predicted_all*100,digit=2)
# adult_bronch$fev1_abs_change <- round(100*(adult_bronch$fev1_post_salbutamol-adult_bronch$fev1_pre_salbutamol)/adult_bronch$fev1_predicted_all,digit=2)
# 
# #fvc
# adult_bronch$fvc_pre_salbutamol_percentage <- round(adult_bronch$fvc_pre_salbutamol/adult_bronch$fvc_predicted_all*100,digit=2)
# adult_bronch$fvc_post_salbutamol_percentage <- round(adult_bronch$fvc_post_salbutamol/adult_bronch$fvc_predicted_all*100,digit=2)
# adult_bronch$fvc_abs_change <- round(100*(adult_bronch$fvc_post_salbutamol-adult_bronch$fvc_pre_salbutamol)/adult_bronch$fvc_predicted_all,digit=2)
# 
# #fev1/fvc percentage
# adult_bronch$pre_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_bronch$fev1_pre_salbutamol/adult_bronch$fvc_pre_salbutamol,digit=2)
# adult_bronch$post_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_bronch$fev1_post_salbutamol/adult_bronch$fvc_post_salbutamol,digit=2)
# # a3 <- c(-0.18,-0.19)
# # b3 <- c(87.21,89.1)
# # names(a3)=names(a1)
# # names(b3)=names(a1)
# # adult_bronch$fev1_fvc_ratio_predicted <- a3[adult_bronch$gender]*adult_bronch$age_at_consent+b3[adult_bronch$gender]
# adult_bronch$pre_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_bronch$pre_salbutamol_fev1_fvc_ratio_actual/adult_bronch$fev1_fvc_ratio_predicted_all,digit=2)
# adult_bronch$post_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_bronch$post_salbutamol_fev1_fvc_ratio_actual/adult_bronch$fev1_fvc_ratio_predicted_all,digit=2)
# 
# 
# #fef2575 percentage
# adult_bronch$fef2575_pre_salbutamol_percentage <- round(adult_bronch$fef2575_pre_salbutamol/adult_bronch$fef2575_predicted_all*100,digit=2)
# adult_bronch$fef2575_post_salbutamol_percentage <- round(adult_bronch$fef2575_post_salbutamol/adult_bronch$fef2575_predicted_all*100,digit=2)
# adult_bronch$fef2575_abs_change <- round(100*(adult_bronch$fef2575_post_salbutamol-adult_bronch$fef2575_pre_salbutamol)/adult_bronch$fef2575_predicted_all,digit=2)
# 
# #airflow_limit_quanjer
# 
# adult_bronch$gender[87] <- 'male'
# adult_bronch$gender[165] <- 'female'
# 
# ##fev1_fvc_ratio_LLN
# for (i in 1:length(row.names(adult_bronch))){
#   if(adult_bronch$gender[i] == 'male'){
#     adult_bronch$fev1_fvc_ration_LLN[i] = (-0.18*adult_bronch$age_at_consent[i])+75.41
#   }
#   if(adult_bronch$gender[i] == 'female'){
#     adult_bronch$fev1_fvc_ration_LLN[i] = (-0.19*adult_bronch$age_at_consent[i])+78.4
#   }
# }
# rm(i)
# 
# for (i in 1:length(row.names(adult_bronch))){
#   if(is.na(adult_bronch$post_salbutamol_fev1_fvc_ratio_actual[i])){
#     adult_bronch$airflow_limit_quanjer[i] <- 'NA'
#   }
#   if(!is.na(adult_bronch$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_bronch$post_salbutamol_fev1_fvc_ratio_actual[i] < adult_bronch$fev1_fvc_ration_LLN[i]){
#     adult_bronch$airflow_limit_quanjer[i] <- 'yes'
#   }
#   if(!is.na(adult_bronch$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_bronch$post_salbutamol_fev1_fvc_ratio_actual[i] > adult_bronch$fev1_fvc_ration_LLN[i]){
#     adult_bronch$airflow_limit_quanjer[i] <- 'no'
#   }
# }
# rm(i)  
# 
# #airflow_limit_brinke
# 
# airflow_limit_brinke = 'NA'
# sel1 <- !is.na(adult_bronch$post_salbutamol_fev1_fvc_ratio_percentage) & adult_bronch$post_salbutamol_fev1_fvc_ratio_percentage < 75
# sel2 <- !is.na(adult_bronch$fev1_post_salbutamol_percentage) & adult_bronch$fev1_post_salbutamol_percentage < 75
# sel3 <- !is.na(adult_bronch$tlc_actual_predicted_perc) & adult_bronch$tlc_actual_predicted_perc > 75
# sel <- (sel1|sel2)&sel3
# airflow_limit_brinke <- rep('no',nrow(adult_bronch))
# airflow_limit_brinke[sel] <- 'yes'
# adult_bronch$airflow_limit_brinke <- airflow_limit_brinke
# 
# #Blood cell count calculations
# 
# adult_bronch$"neutrophils (x10^3/uL)" = round((adult_bronch$neutrophils * adult_bronch$wbcs) / 100, digit = 2)
# adult_bronch$"lymphocytes (x10^3/uL)" = round((adult_bronch$lymphocytes * adult_bronch$wbcs) / 100, digit = 2)
# adult_bronch$"monocytes (x10^3/uL)" = round((adult_bronch$monocytes * adult_bronch$wbcs) / 100, digit = 2)
# adult_bronch$"basophils (x10^3/uL)" = round((adult_bronch$basophils * adult_bronch$wbcs) / 100, digit = 2)
# adult_bronch$"eosinophils (x10^3/uL)" = round((adult_bronch$eosinophils * adult_bronch$wbcs) / 100, digit = 2)
# 
# # Body Surface Area (BSA)
# adult_bronch$bsa = round(sqrt((adult_bronch$height * adult_bronch$weight) / 3600),digit = 2)
# 
# 
# 
# write.table(adult_bronch, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/dt_bronch1.txt", sep = "\t", row.names= F, na = ".", quote =F)
# 
# rm(sel,sel1,sel2,sel3,timeAdjust, adult_predicted_lung_functions)

#########################################################


# Derived variable calcs

# Longitudinal 11

# Adults

# library(gdata)
# 
# adult_long1 = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/dt_long11.xlsx", na.strings = c("999", "999.99",""))
# 
# adult_long1$comment = NULL
# 
# levels(adult_long1$c_reactive_protein)[levels(adult_long1$c_reactive_protein)=='< 5'] <- "4.99"
# levels(adult_long1$c_reactive_protein)[levels(adult_long1$c_reactive_protein)=='<5'] <- "4.99"
# levels(adult_long1$c_reactive_protein)[levels(adult_long1$c_reactive_protein)=='<1'] <- "0.99"
# levels(adult_long1$c_reactive_protein)[levels(adult_long1$c_reactive_protein)==''] <- NA
# adult_long1$c_reactive_protein <- as.numeric(levels(adult_long1$c_reactive_protein)[adult_long1$c_reactive_protein])
# 
# names(adult_long1)[1] <- "study_id"
# 
# # data = read.table(file="/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/2014-05-16-adult_clinical_scr_bd1.txt", header = TRUE, sep = "\t")
# 
# adult_predicted_lung_functions = subset(data,,select = c(subjid, fev1_predicted, fvc_predicted, fev1_fvc_ratio_predicted,age_at_consent,fef2575_predicted,tlc_actual_predicted_perc,weight,height) )
# names(adult_predicted_lung_functions)[2] <- "fev1_predicted_all"
# names(adult_predicted_lung_functions)[3] <- "fvc_predicted_all"
# names(adult_predicted_lung_functions)[4] <- "fev1_fvc_ratio_predicted_all"
# names(adult_predicted_lung_functions)[6] <- "fef2575_predicted_all"
# 
# 
# adult_psedo = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/Adult_Subject_List_FINAL_20141127+kitIDs.xlsx")
# adult_psedo$subjid.1 = NULL
# 
# adult_long1 = merge(adult_long1,adult_psedo,by = "study_id", ALL = F)
# adult_long1 = merge(adult_long1,adult_predicted_lung_functions,by = "subjid", ALL = F)
# 
# adult_long1$tlc_actual_predicted_perc[adult_long1$tlc_actual_predicted_perc == '.'] <- 'NA'
# adult_long1$tlc_actual_predicted_perc <- as.numeric(levels(adult_long1$tlc_actual_predicted_perc)[adult_long1$tlc_actual_predicted_perc])
# 
# #Harmonise Units for pef variables
# summary(adult_long1$pef_pre_salbutamol)
# timeAdjust=1*(adult_long1$pef_pre_salbutamol_um == "l_sec")+60*(adult_long1$pef_pre_salbutamol_um == "l_min")
# adult_long1$pef_pre_salbutamol=round(adult_long1$pef_pre_salbutamol/timeAdjust,digit=2)
# summary(adult_long1$pef_pre_salbutamol)
# 
# summary(adult_long1$pef_actual)
# timeAdjust=1*(adult_long1$pef_actual_um == "l_sec")+60*(adult_long1$pef_actual_um == "l_min")
# adult_long1$pef_actual=round(adult_long1$pef_actual/timeAdjust,digit=2)
# summary(adult_long1$pef_actual)
# 
# summary(adult_long1$pef_predicted)
# timeAdjust=1*(adult_long1$pef_predicted_um == "l_sec")+60*(adult_long1$pef_predicted_um == "l_min")
# adult_long1$pef_predicted=round(adult_long1$pef_predicted/timeAdjust, digit=2)
# summary(adult_long1$pef_predicted)
# 
# #fev1
# adult_long1$fev1_pre_salbutamol_percentage <- round(adult_long1$fev1_pre_salbutamol/adult_long1$fev1_predicted_all*100,digit=2)
# adult_long1$fev1_post_salbutamol_percentage <- round(adult_long1$fev1_post_salbutamol/adult_long1$fev1_predicted_all*100,digit=2)
# adult_long1$fev1_abs_change <- round(100*(adult_long1$fev1_post_salbutamol-adult_long1$fev1_pre_salbutamol)/adult_long1$fev1_predicted_all,digit=2)
# 
# #fvc
# adult_long1$fvc_pre_salbutamol_percentage <- round(adult_long1$fvc_pre_salbutamol/adult_long1$fvc_predicted_all*100,digit=2)
# adult_long1$fvc_post_salbutamol_percentage <- round(adult_long1$fvc_post_salbutamol/adult_long1$fvc_predicted_all*100,digit=2)
# adult_long1$fvc_abs_change <- round(100*(adult_long1$fvc_post_salbutamol-adult_long1$fvc_pre_salbutamol)/adult_long1$fvc_predicted_all,digit=2)
# 
# #fev1/fvc percentage
# adult_long1$pre_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_long1$fev1_pre_salbutamol/adult_long1$fvc_pre_salbutamol,digit=2)
# adult_long1$post_salbutamol_fev1_fvc_ratio_actual <- round(100*adult_long1$fev1_post_salbutamol/adult_long1$fvc_post_salbutamol,digit=2)
# # a3 <- c(-0.18,-0.19)
# # b3 <- c(87.21,89.1)
# # names(a3)=names(a1)
# # names(b3)=names(a1)
# # adult_long1$fev1_fvc_ratio_predicted <- a3[adult_long1$gender]*adult_long1$age_at_consent+b3[adult_long1$gender]
# adult_long1$pre_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_long1$pre_salbutamol_fev1_fvc_ratio_actual/adult_long1$fev1_fvc_ratio_predicted_all,digit=2)
# adult_long1$post_salbutamol_fev1_fvc_ratio_percentage <- round(100*adult_long1$post_salbutamol_fev1_fvc_ratio_actual/adult_long1$fev1_fvc_ratio_predicted_all,digit=2)
# 
# 
# #fef2575 percentage
# adult_long1$fef2575_pre_salbutamol_percentage <- round(adult_long1$fef2575_pre_salbutamol/adult_long1$fef2575_predicted_all*100,digit=2)
# adult_long1$fef2575_post_salbutamol_percentage <- round(adult_long1$fef2575_post_salbutamol/adult_long1$fef2575_predicted_all*100,digit=2)
# adult_long1$fef2575_abs_change <- round(100*(adult_long1$fef2575_post_salbutamol-adult_long1$fef2575_pre_salbutamol)/adult_long1$fef2575_predicted_all,digit=2)
# 
# 
# 
# #airflow_limit_quanjer
# 
# ##fev1_fvc_ratio_LLN
# for (i in 1:length(row.names(adult_long1))){
#   if(adult_long1$gender[i] == 'male'){
#     adult_long1$fev1_fvc_ration_LLN[i] = (-0.18*adult_long1$age_at_consent[i])+75.41
#   }
#   if(adult_long1$gender[i] == 'female'){
#     adult_long1$fev1_fvc_ration_LLN[i] = (-0.19*adult_long1$age_at_consent[i])+78.4
#   }
# }
# rm(i)
# 
# for (i in 1:length(row.names(adult_long1))){
#   if(is.na(adult_long1$post_salbutamol_fev1_fvc_ratio_actual[i])){
#     adult_long1$airflow_limit_quanjer[i] <- 'NA'
#   }
#   if(!is.na(adult_long1$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_long1$post_salbutamol_fev1_fvc_ratio_actual[i] < adult_long1$fev1_fvc_ration_LLN[i]){
#     adult_long1$airflow_limit_quanjer[i] <- 'yes'
#   }
#   if(!is.na(adult_long1$post_salbutamol_fev1_fvc_ratio_actual[i]) & adult_long1$post_salbutamol_fev1_fvc_ratio_actual[i] > adult_long1$fev1_fvc_ration_LLN[i]){
#     adult_long1$airflow_limit_quanjer[i] <- 'no'
#   }
# }
# rm(i)  
# 
# #airflow_limit_brinke
#  
# airflow_limit_brinke = 'NA'
# sel1 <- !is.na(adult_long1$post_salbutamol_fev1_fvc_ratio_percentage) & adult_long1$post_salbutamol_fev1_fvc_ratio_percentage < 75
# sel2 <- !is.na(adult_long1$fev1_post_salbutamol_percentage) & adult_long1$fev1_post_salbutamol_percentage < 75
# sel3 <- !is.na(adult_long1$tlc_actual_predicted_perc) & adult_long1$tlc_actual_predicted_perc > 75
# sel <- (sel1|sel2)&sel3
# airflow_limit_brinke <- rep('no',nrow(adult_long1))
# airflow_limit_brinke[sel] <- 'yes'
# adult_long1$airflow_limit_brinke <- airflow_limit_brinke
# 
# #Blood cell count calculations
# 
# adult_long1$"neutrophils (x10^3/uL)" = round((adult_long1$neutrophils * adult_long1$wbcs) / 100, digit = 2)
# adult_long1$"lymphocytes (x10^3/uL)" = round((adult_long1$lymphocytes * adult_long1$wbcs) / 100, digit = 2)
# adult_long1$"monocytes (x10^3/uL)" = round((adult_long1$monocytes * adult_long1$wbcs) / 100, digit = 2)
# adult_long1$"basophils (x10^3/uL)" = round((adult_long1$basophils * adult_long1$wbcs) / 100, digit = 2)
# adult_long1$"eosinophils (x10^3/uL)" = round((adult_long1$eosinophils * adult_long1$wbcs) / 100, digit = 2)
# 
# # Body Surface Area (BSA)
# adult_long1$bsa = round(sqrt((adult_long1$height * adult_long1$weight) / 3600),digit = 2)
# 
# 
# 
# write.table(adult_long1, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/dt_long11.txt", sep = "\t", row.names= F, na = ".", quote =F)

#########################################################
