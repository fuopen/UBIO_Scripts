# require("transmartRClient")
# connectToTransmart("https://transmart.doc.ic.ac.uk/transmart")
# 
# studies <- getStudies()
# print(studies)
# 
# study<-"DEC2014_ADULT"
# 
# # Retrieve Clinical Data
# allObservations <- getObservations(study, as.data.frame = T)
# 
# #Store column names of observations
# colnames(allObservations$observations) -> columnnames
# 
# #Create a dataframe containing all observatios
# data <- data.frame(matrix(unlist(allObservations$observations), nrow=620, byrow=F))
# 
# #Apply Column names to data.frame
# for(i in 1:2886){names(data)[i] <- columnnames[i]}

library(gdata)
library(plotrix)

data = read.table(file="/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/2014-05-16-adult_clinical_scr_bd1.txt", header = TRUE, sep = "\t")

adult_pseudo_code = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/Adult_Subject_List_FINAL_20141127+kitIDs.xlsx")

adult_site_codes = read.delim("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Pseudokeys/adult_site_code_list.txt", sep = "\t", header = T)

adult_acq = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Clinical_Paper_site_effect_resutls/adult_acq5.xlsx")

merge(data, adult_pseudo_code, by = "subjid", all = F) -> data
merge(data, adult_site_codes, by = "subjid", all = F) -> data
merge(data, adult_acq, by = "subjid", all = T) -> data


chisq.test(data$age_at_consent, data$site_code)

# Pearson's Chi-squared test
# 
# data:  data$age_at_consent and data$site_code
# X-squared = 915.2692, df = 840, p-value = 0.0359

chisq.test(data$eosinophils, data$site_code)


# Pearson's Chi-squared test
# 
# data:  data$eosinophils and data$site_code
# X-squared = 5039.096, df = 4662, p-value = 6.905e-05

chisq.test(data$gender, data$site_code)

# Pearson's Chi-squared test
# 
# data:  data$gender and data$site_code
# X-squared = 11.636, df = 14, p-value = 0.6355

chisq.test(data$lge_total, data$site_code)

# Pearson's Chi-squared test
# 
# data:  data$lge_total and data$site_code
# X-squared = 5808.576, df = 5754, p-value = 0.3039

chisq.test(data$fev1_percentage, data$site_code)

# Pearson's Chi-squared test
# 
# data:  data$fev1_percentage and data$site_code
# X-squared = 8461.928, df = 8414, p-value = 0.3542

chisq.test(data$fev1_actual, data$site_code)

chisq.test(data$acq5_mean, data$site_code)



# Site i has a lower acq5 compared to other sites, when examining we saw that this was because site i has predominantly mild to moderate participants

# Pearson's Chi-squared test
# 
# data:  data$fev1_actual and data$site_code
# X-squared = 4531.672, df = 4424, p-value = 0.1266

# Pearson's Chi-squared test
# 
# data:  data$eosinophils and data$site_code
# X-squared = 5039.096, df = 4662, p-value = 6.905e-05

library(reshape2)
subset(data,,select = c("eosinophils", "site_code")) -> eos
melt(eos) -> eos.m

boxplot(as.numeric(as.character(eos.m$eosinophils))~as.factor(eos.m$site_code),na.action=na.omit,col="lightblue",cex.axis=0.6,main="Eosinophils By Site")

# scatterplot(as.numeric(as.character(eos.m$eosinophils))~as.factor(eos.m$site_code), na.action=na.omit,col="lightblue",cex.axis=0.6,main="Eosinophils By Site")
library(ggplot2)

showboxplot<-function(indata, inx, iny,steroidspos) {
  
  condition <- indata[,inx]
  steroids <- indata[,steroidspos]
  
  p <- ggplot(indata, aes(condition, y=indata[,iny]), environment = environment()) +
    geom_boxplot() +
    geom_jitter(aes(size=1,colour=factor(steroids)),position = position_jitter(width = 0.2)) +
    ylab(colnames(indata)[iny]) + xlab(colnames(indata)[inx]) 
  
  print(p)
}



# eos plot
showboxplot (data,607, 609, 593)

# age plot
showboxplot (data,607, 91, 593)

#

for (i in 1:ncol(data)){
  if(length(levels(data[,i]))>1 && chisq.test(data[,i], data$site_code)[3] < 0.05) {names(data[i]) -> site_effect[i]}
}

