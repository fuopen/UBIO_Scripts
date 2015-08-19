# Create list of issues for adult data from Nubilaria
# August 2015
# Aruna Bansal

setwd("C:/Users/Aruna/Acclarogen/UBIOPRED/2015_03/Adult")
source("adult.clean.180815.r")


IssueOut<-function(issue.line){
sink("issues.adult.20150818.csv",append=T)
cat(issue.line)
sink()
}

sink("issues.adult.20150818.csv",append=T)
cat("Filename,Record,patient_code,investigator,cohort,Field,Value,Issue\n")
sink()

L1("dt_long1.txt")
L1("dt_long11.txt")
B1("dt_bd1.txt")  # 
B2("dt_bd2.txt")  # 
BH("dt_bhrtc.txt")
LH("dt_lhrtc.txt")
ME("dt_md_exac.txt")
IA("dt_md_lge.txt")
MM("dt_md_med.txt")
AE("dt_md_sae.txt")
MS("dt_md_samp.txt")
SP("dt_md_prik.txt")
IR("dt_repeata.txt")     
SC("dt_screen.txt")
HA("dt_screen.txt")   # biochemistry haematology
CO("dt_st_comp.txt")

dates("dt_long1.txt","dt_bd1.txt","dt_bd2.txt","dt_md_sae.txt","dt_repeata.txt","dt_screen.txt") 

