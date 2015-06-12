require("transmartRClient")
connectToTransmart("https://transmart.doc.ic.ac.uk/transmart")

studies <- getStudies()
print(studies)

study<-"DEC2014_ADULT"

# Retrieve Clinical Data
allObservations <- getObservations(study, as.data.frame = T)

#Store column names of observations
colnames(allObservations$observations) -> columnnames

#Create a dataframe containing all observatios
alldata <- data.frame(matrix(unlist(allObservations$observations), nrow=620, byrow=F))

#Apply Column names to data.frame
for(i in 1:2886){names(alldata)[i] <- columnnames[i]}

#Create subsets based on cohort status
cohortA <- subset(alldata, alldata$"Study Groups_cohort_cohort_a" == "cohort_a", select = )
cohortB <- subset(alldata, alldata$"Study Groups_cohort_cohort_b" == "cohort_b", select = )
cohortC <- subset(alldata, alldata$"Study Groups_cohort_cohort_c" == "cohort_c", select = )
cohortD <- subset(alldata, alldata$"Study Groups_cohort_cohort_d" == "cohort_d", select = )

require(plotrix)

#Age stats
mean(as.numeric(levels(cohortA$"Demographic Data_Age"))[cohortA$"Demographic Data_Age"])
summary(as.numeric(levels(cohortA$"Demographic Data_Age"))[cohortA$"Demographic Data_Age"])
std.error(as.numeric(levels(cohortA$"Demographic Data_Age"))[cohortA$"Demographic Data_Age"])
length(as.numeric(levels(cohortA$"Demographic Data_Age"))[cohortA$"Demographic Data_Age"])

mean(as.numeric(levels(cohortB$"Demographic Data_Age"))[cohortB$"Demographic Data_Age"])
summary(as.numeric(levels(cohortB$"Demographic Data_Age"))[cohortB$"Demographic Data_Age"])
std.error(as.numeric(levels(cohortB$"Demographic Data_Age"))[cohortB$"Demographic Data_Age"])
length (as.numeric(levels(cohortB$"Demographic Data_Age"))[cohortB$"Demographic Data_Age"])

mean(as.numeric(levels(cohortC$"Demographic Data_Age"))[cohortC$"Demographic Data_Age"])
summary(as.numeric(levels(cohortC$"Demographic Data_Age"))[cohortC$"Demographic Data_Age"])
std.error(as.numeric(levels(cohortC$"Demographic Data_Age"))[cohortC$"Demographic Data_Age"])
length(as.numeric(levels(cohortC$"Demographic Data_Age"))[cohortC$"Demographic Data_Age"])

mean(as.numeric(levels(cohortD$"Demographic Data_Age"))[cohortD$"Demographic Data_Age"])
summary(as.numeric(levels(cohortD$"Demographic Data_Age"))[cohortD$"Demographic Data_Age"])
std.error(as.numeric(levels(cohortD$"Demographic Data_Age"))[cohortD$"Demographic Data_Age"])
length(as.numeric(levels(cohortD$"Demographic Data_Age"))[cohortD$"Demographic Data_Age"])

#Age at diagnosis
mean(as.numeric(levels(cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
summary(as.numeric(levels(cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
std.error(as.numeric(levels(cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
length(as.numeric(levels(cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortA$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"])

mean(as.numeric(levels(cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
summary(as.numeric(levels(cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
std.error(as.numeric(levels(cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
length (as.numeric(levels(cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortB$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"])

mean(as.numeric(levels(cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
summary(as.numeric(levels(cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
std.error(as.numeric(levels(cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
length(as.numeric(levels(cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortC$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"])

mean(as.numeric(levels(cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"])
summary(as.numeric(levels(cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
std.error(as.numeric(levels(cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)
length(as.numeric(levels(cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"))[cohortD$"Subject History_Respiratory History_Onset OR First Diagnosis Age (years)"],na.rm = T)

#Female: Calcs Performed on tranSMART

#BMI
mean(as.numeric(levels(cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"])
summary(as.numeric(levels(cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"])
std.error(as.numeric(levels(cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"])
length(as.numeric(levels(cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortA$"Subject Body Measurements_Body Mass Index (kg/m2)"])

mean(as.numeric(levels(cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"])
summary(as.numeric(levels(cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"])
std.error(as.numeric(levels(cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"])
length (as.numeric(levels(cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortB$"Subject Body Measurements_Body Mass Index (kg/m2)"])

mean(as.numeric(levels(cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"])
summary(as.numeric(levels(cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"])
std.error(as.numeric(levels(cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"])
length(as.numeric(levels(cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortC$"Subject Body Measurements_Body Mass Index (kg/m2)"])

mean(as.numeric(levels(cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"])
summary(as.numeric(levels(cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"])
std.error(as.numeric(levels(cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"])
length(as.numeric(levels(cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"))[cohortD$"Subject Body Measurements_Body Mass Index (kg/m2)"])

#IgE
summary(as.numeric(levels(cohortA$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortA$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])
length(as.numeric(levels(cohortA$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortA$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])

summary(as.numeric(levels(cohortB$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortB$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])
length (as.numeric(levels(cohortB$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortB$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])

summary(as.numeric(levels(cohortC$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortC$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])
length(as.numeric(levels(cohortC$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortC$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])

summary(as.numeric(levels(cohortD$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortD$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])
length(as.numeric(levels(cohortD$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"))[cohortD$"Clinical Data_Atopy_Immunoglobulins E (IgE)_IgE Total (IU/ml)"])

#FEV1 %
mean(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
length(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"])

mean(as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
length (as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"])

mean(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
length(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"])

mean(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"],na.rm = T)
length(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Pct (L)"])

#FVC %
mean(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
length(as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"])

mean(as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
length (as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"])

mean(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
length(as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"])

mean(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
summary(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
std.error(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"],na.rm = T)
length(as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FVC Pct (L)"])

#FEV1/FVC definition
cohortA_fev1_fvc <- as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"))[cohortA$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"] / as.numeric(levels(cohortA$"Clinical Data_Lung Function_Spirometry_FVC Actual"))[cohortA$"Clinical Data_Lung Function_Spirometry_FVC Actual"]
cohortB_fev1_fvc <- as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"))[cohortB$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"] / as.numeric(levels(cohortB$"Clinical Data_Lung Function_Spirometry_FVC Actual"))[cohortB$"Clinical Data_Lung Function_Spirometry_FVC Actual"]
cohortC_fev1_fvc <- as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"))[cohortC$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"] / as.numeric(levels(cohortC$"Clinical Data_Lung Function_Spirometry_FVC Actual"))[cohortC$"Clinical Data_Lung Function_Spirometry_FVC Actual"]
cohortD_fev1_fvc <- as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"))[cohortD$"Clinical Data_Lung Function_Spirometry_FEV1 Actual (L)"] / as.numeric(levels(cohortD$"Clinical Data_Lung Function_Spirometry_FVC Actual"))[cohortD$"Clinical Data_Lung Function_Spirometry_FVC Actual"]
  
#FEV1/FVC
mean(cohortA_fev1_fvc,na.rm = T)  
summary(cohortA_fev1_fvc,na.rm = T)  
std.error(cohortA_fev1_fvc,na.rm = T)  
length(cohortA_fev1_fvc)  
  
mean(cohortB_fev1_fvc,na.rm = T)  
summary(cohortB_fev1_fvc,na.rm = T)  
std.error(cohortB_fev1_fvc,na.rm = T)  
length(cohortB_fev1_fvc)  

mean(cohortC_fev1_fvc,na.rm = T)  
summary(cohortC_fev1_fvc,na.rm = T)  
std.error(cohortC_fev1_fvc,na.rm = T)  
length(cohortC_fev1_fvc)  

mean(cohortD_fev1_fvc,na.rm = T)  
summary(cohortD_fev1_fvc,na.rm = T)  
std.error(cohortD_fev1_fvc,na.rm = T)  
length(cohortD_fev1_fvc)  

#Exacerbations in Previous Year
mean(as.numeric(levels(cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
summary(as.numeric(levels(cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
std.error(as.numeric(levels(cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
length(as.numeric(levels(cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortA$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"])

mean(as.numeric(levels(cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
summary(as.numeric(levels(cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
std.error(as.numeric(levels(cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
length (as.numeric(levels(cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortB$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"])

mean(as.numeric(levels(cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
summary(as.numeric(levels(cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
std.error(as.numeric(levels(cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
length(as.numeric(levels(cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortC$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"])

mean(as.numeric(levels(cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
summary(as.numeric(levels(cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
std.error(as.numeric(levels(cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"],na.rm = T)
length(as.numeric(levels(cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"))[cohortD$"Subject History_Recent Asthma Exacerbation History_Excacerbation Number"])

#Nitric Oxide
summary(as.numeric(levels(cohortA$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortA$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"],na.rm = T)
length(as.numeric(levels(cohortA$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortA$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"])

summary(as.numeric(levels(cohortB$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortB$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"],na.rm = T)
length(as.numeric(levels(cohortB$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortB$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"])

summary(as.numeric(levels(cohortC$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortC$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"],na.rm = T)
length(as.numeric(levels(cohortC$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortC$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"])

summary(as.numeric(levels(cohortD$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortD$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"],na.rm = T)
length(as.numeric(levels(cohortD$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"))[cohortD$"Clinical Data_Exhaled nitric oxide (NO)_NO Standard Flow Rate"])

#Sputum eos %
summary(as.numeric(levels(cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"],na.rm = T)
length(as.numeric(levels(cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"])

summary(as.numeric(levels(cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"],na.rm = T)
length(as.numeric(levels(cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"])

summary(as.numeric(levels(cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"],na.rm = T)
length(as.numeric(levels(cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"])

summary(as.numeric(levels(cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"],na.rm = T)
length(as.numeric(levels(cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"))[cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Eosinophils"])

#Sputum neutrophils %
summary(as.numeric(levels(cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"],na.rm = T)
length(as.numeric(levels(cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortA$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"])

summary(as.numeric(levels(cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"],na.rm = T)
length(as.numeric(levels(cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortB$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"])

summary(as.numeric(levels(cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"],na.rm = T)
length(as.numeric(levels(cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortC$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"])

summary(as.numeric(levels(cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"],na.rm = T)
length(as.numeric(levels(cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"))[cohortD$"Clinical Data_Sputum Data_Master list for analyses_Pct Neutrophils"])

#Blood eos%
summary(as.numeric(levels(cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"],na.rm = T)
length(as.numeric(levels(cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"])

summary(as.numeric(levels(cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"],na.rm = T)
length(as.numeric(levels(cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"])

summary(as.numeric(levels(cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"],na.rm = T)
length(as.numeric(levels(cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"])

summary(as.numeric(levels(cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"],na.rm = T)
length(as.numeric(levels(cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"))[cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Eosinophils Pct"])

#Blood neutro %
summary(as.numeric(levels(cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"],na.rm = T)
length(as.numeric(levels(cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortA$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"])

summary(as.numeric(levels(cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"],na.rm = T)
length(as.numeric(levels(cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortB$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"])

summary(as.numeric(levels(cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"],na.rm = T)
length(as.numeric(levels(cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortC$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"])

summary(as.numeric(levels(cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"],na.rm = T)
length(as.numeric(levels(cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"))[cohortD$"Clinical Data_Haematology and biochemistry tests_Screening_Neutrophils Pct"])
