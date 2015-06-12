SB1 <- read.csv('/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/New_Baseline_Data_to_be_included_Nov_2014_release/processed/2014-05-16-pead_screening_bd1_merged_15_subjects.csv', na.strings = ".")

#Harmonise Units for pef variables
timeAdjust=1*(SB1$pef_pre_salbutamol_um == "l_sec")+60*(SB1$pef_pre_salbutamol_um == "l_min")
SB1$pef_pre_salbutamol=SB1$pef_pre_salbutamol/timeAdjust

timeAdjust=1*(SB1$pef_actual_um == "l_sec")+60*(SB1$pef_actual_um == "l_min")
SB1$pef_actual=SB1$pef_actual/timeAdjust

timeAdjust=1*(SB1$pef_predicted_um == "l_sec")+60*(SB1$pef_predicted_um == "l_min")
SB1$pef_predicted=SB1$pef_predicted/timeAdjust

#Harmonise nicotine_breath_test_unit 
#nicotine_unit=1*(SB1$nicotine_breath_test_unit == "ppm")+1000*(SB1$nicotine_breath_test_unit == "ppb")
#SB1$nicotine_breath_test=SB1$nicotine_breath_test/nicotine_unit

#BMI
SB1$bmi=round(SB1$weight/(SB1$height/100)^2,digit=2)

# tlc percentage
a1 <- c(7.99,6.60)
b1 <- c(-7.08,-5.79)
names(a1)=c("male","female")
names(b1)=names(a1)
SB1$tlc_predicted <- a1[SB1$gender]*SB1$height/100+b1[SB1$gender]
SB1$tlc_actual_predicted_perc <- round(100*SB1$plethysmography_tlc/SB1$tlc_predicted,digit=2)

#fev1 (REDO)
SB1$fev1_pre_salbutamol_percentage <- round(SB1$fev1_pre_salbutamol/SB1$fev1_predicted*100,digit=2)
SB1$fev1_post_salbutamol_percentage <- round(SB1$fev1_post_salbutamol/SB1$fev1_predicted*100,digit=2)
SB1$fev1_abs_change <- round(100*(SB1$fev1_post_salbutamol-SB1$fev1_pre_salbutamol)/SB1$fev1_predicted,digit=2)

#fvc (REDO)
SB1$fvc_pre_salbutamol_percentage <- round(SB1$fvc_pre_salbutamol/SB1$fvc_predicted*100,digit=2)
SB1$fvc_post_salbutamol_percentage <- round(SB1$fvc_post_salbutamol/SB1$fvc_predicted*100,digit=2)
SB1$fvc_abs_change <- round(100*(SB1$fvc_post_salbutamol-SB1$fvc_pre_salbutamol)/SB1$fvc_predicted,digit=2)

#fev1/fvc percentage
SB1$pre_salbutamol_fev1_fvc_ratio_actual <- 100*SB1$fev1_pre_salbutamol/SB1$fvc_pre_salbutamol
SB1$post_salbutamol_fev1_fvc_ratio_actual <- 100*SB1$fev1_post_salbutamol/SB1$fvc_post_salbutamol
a3 <- c(-0.18,-0.19)
b3 <- c(87.21,89.1)
names(a3)=names(a1)
names(b3)=names(a1)
SB1$fev1_fvc_ratio_predicted <- a3[SB1$gender]*SB1$age_at_consent+b3[SB1$gender]
SB1$pre_salbutamol_fev1_fvc_ratio_percentage <- round(100*SB1$pre_salbutamol_fev1_fvc_ratio_actual/SB1$fev1_fvc_ratio_predicted,digit=2)
SB1$post_salbutamol_fev1_fvc_ratio_percentage <- round(100*SB1$post_salbutamol_fev1_fvc_ratio_actual/SB1$fev1_fvc_ratio_predicted,digit=2)

#fev1_fvc_ration_LLN
a3 <- c(-0.18,-0.19)
b3 <- c(75.41,78.4)
names(a3)=names(a1)
names(b3)=names(a1)
SB1$fev1_fvc_ration_LLN <- a3[SB1$gender]*SB1$age_at_consent+b3[SB1$gender]

#fef2575 percentage
SB1$fef2575_pre_salbutamol_percentage <- round(SB1$fef2575_pre_salbutamol/SB1$fef2575_predicted*100,digit=2)
SB1$fef2575_post_salbutamol_percentage <- round(SB1$fef2575_post_salbutamol/SB1$fef2575_predicted*100,digit=2)
SB1$fef2575_abs_change <- round(100*(SB1$fef2575_post_salbutamol-SB1$fef2575_pre_salbutamol)/SB1$fef2575_predicted,digit=2)

#parental_hay_fever
parental_hay_fever <- rep('no',nrow(SB1))
sel_yes <- SB1$hay_fever_father=='yes'|SB1$hay_fever_mother=='yes'
parental_hay_fever[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$hay_fever_father=='uncertain'&SB1$hay_fever_mother=='uncertain'
sel_uncertain2 <- SB1$hay_fever_father=='no'&SB1$hay_fever_mother=='uncertain'
sel_uncertain3 <- SB1$hay_fever_father=='uncertain'&SB1$hay_fever_mother=='no'
parental_hay_fever[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_hay_fever <- parental_hay_fever

#parental_eczema
parental_eczema <- rep('no',nrow(SB1))
sel_yes <- SB1$eczema_father=='yes'|SB1$eczema_mother=='yes'
parental_eczema[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$eczema_father=='uncertain'&SB1$eczema_mother=='uncertain'
sel_uncertain2 <- SB1$eczema_father=='no'&SB1$eczema_mother=='uncertain'
sel_uncertain3 <- SB1$eczema_father=='uncertain'&SB1$eczema_mother=='no'
parental_eczema[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_eczema <- parental_eczema

#parental_copd
parental_copd <- rep('no',nrow(SB1))
sel_yes <- SB1$copd_father=='yes'|SB1$copd_mother=='yes'
parental_copd[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$copd_father=='uncertain'&SB1$copd_mother=='uncertain'
sel_uncertain2 <- SB1$copd_father=='no'&SB1$copd_mother=='uncertain'
sel_uncertain3 <- SB1$copd_father=='uncertain'&SB1$copd_mother=='no'
parental_copd[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_copd <- parental_copd

#parental_asthma
parental_asthma <- rep('no',nrow(SB1))
sel_yes <- SB1$asthma_father=='yes'|SB1$asthma_mother=='yes'
parental_asthma[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$asthma_father=='uncertain'&SB1$asthma_mother=='uncertain'
sel_uncertain2 <- SB1$asthma_father=='no'&SB1$asthma_mother=='uncertain'
sel_uncertain3 <- SB1$asthma_father=='uncertain'&SB1$asthma_mother=='no'
parental_asthma[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_asthma <- parental_asthma

#physical_activity
physical_activity <- rep('no',nrow(SB1))
sel_yes <- SB1$routine_physical_activities=='yes'|SB1$physical_exercise=='yes'
physical_activity[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$routine_physical_activities=='uncertain'&SB1$physical_exercise=='uncertain'
sel_uncertain2 <- SB1$routine_physical_activities=='no'&SB1$physical_exercise=='uncertain'
sel_uncertain3 <- SB1$routine_physical_activities=='uncertain'&SB1$physical_exercise=='no'
physical_activity[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$physical_activity <- physical_activity

#airflow_limit_quanjer
sel <- SB1$post_salbutamol_fev1_fvc_ratio_percentage < SB1$fev1_fvc_ration_LLN
airflow_limit_quanjer <- rep('no',nrow(SB1))
airflow_limit_quanjer[sel] <- 'yes'
SB1$airflow_limit_quanjer <- airflow_limit_quanjer

#airflow_limit_brinke
sel1 <- SB1$post_salbutamol_fev1_fvc_ratio_percentage < 75
sel2 <- SB1$fev1_post_salbutamol_percentage < 75
sel3 <- SB1$tlc_actual_predicted_perc > 75
sel <- (sel1|sel2)&sel3
airflow_limit_brinke <- rep('no',nrow(SB1))
airflow_limit_brinke[sel] <- 'yes'
SB1$airflow_limit_brinke <- airflow_limit_brinke

#$EOL Insert one last column containing EOL, so that removal of inner cell line breaks is possible
#eol <- 'eol'
#SB1$eol <- eol

#Write_data_file
write.csv(SB1, file="/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/New_Baseline_Data_to_be_included_Nov_2014_release/processed/2014-05-16-pead_screening_bd1_merged_15_subjects_scr_bd1.csv")
