# Create list of issues for paediatric data from Nubilaria
# August 2015
# Aruna Bansal

setwd("C:/Users/Aruna/Acclarogen/UBIOPRED/2015_03/Paed")
source("paed.clean.180815.r")


IssueOut<-function(issue.line){
sink("issues.paed.20150818.csv",append=T)
cat(issue.line)
sink()
}

sink("issues.paed.20150818.csv",append=T)
cat("Filename,Record,patient_code,investigator,cohort,Field,Value,Issue\n")
sink()

L1("dt_long11.txt")
B1("dt_base.txt")
ME("dt_md_exac.txt")
IA("dt_md_lge.txt")
MM("dt_md_med.txt")
AE("dt_md_sae.txt")
MS("dt_md_samp.txt")
SP("dt_md_prik.txt")
SC("dt_screen.txt")
HA("dt_screen.txt")   # biochemistry haematology
CO("dt_st_comp.txt")

dates("dt_long11.txt","dt_base.txt","dt_md_sae.txt","dt_screen.txt") 


