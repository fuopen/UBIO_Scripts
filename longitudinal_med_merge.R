#Script to merge medication data and assign the correct categories (NOT COMPLETE...USE the AWK script)

# Load the gdata library for reading writing to Excel
library("gdata", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

#Read in the Excel file that Ana produced after binning ("bucketing") to a df
adult_long_meds_ana = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/Medication/20150512_adult_free_txt_meds_v3\ ana.xlsx")

#Create medication start and end date variables from table

#Start date
for (i in 1:1666){
  if (!is.na(adult_long_meds_ana[i, "start_day"]) && !is.na(adult_long_meds_ana[i,"start_month"]) && !is.na(adult_long_meds_ana[i,"start_year"]))
  {
    adult_long_meds_ana[i,"start_date"] = paste(adult_long_meds_ana[i, "start_year"],adult_long_meds_ana[i,"start_month"],adult_long_meds_ana[i,"start_day"], sep ="-")
  }
  if (is.na(adult_long_meds_ana[i, "start_day"]) && !is.na(adult_long_meds_ana[i,"start_month"]) && !is.na(adult_long_meds_ana[i,"start_year"]))
  {
    adult_long_meds_ana[i,"start_date"] = paste(adult_long_meds_ana[i,"start_year"],adult_long_meds_ana[i,"start_month"],"1", sep ="-")
  }
  if (is.na(adult_long_meds_ana[i, "start_day"]) && is.na(adult_long_meds_ana[i,"start_month"]) && !is.na(adult_long_meds_ana[i,"start_year"]))
  {
    adult_long_meds_ana[i,"start_date"] = paste(adult_long_meds_ana[i,"start_year"],"1","1", sep ="-")
  }
}

adult_long_meds_ana$start_date = as.Date(adult_long_meds_ana$start_date) 
adult_long_meds_ana$screening.visit_date = as.Date(adult_long_meds_ana$screening.visit_date)

#Start date
for (i in 1:1666){
  if (!is.na(adult_long_meds_ana[i, "start_day"]) && !is.na(adult_long_meds_ana[i,"start_month"]) && !is.na(adult_long_meds_ana[i,"start_year"]))
  {
    adult_long_meds_ana[i,"start_date"] = paste(adult_long_meds_ana[i, "start_year"],adult_long_meds_ana[i,"start_month"],adult_long_meds_ana[i,"start_day"], sep ="-")
  }
  if (is.na(adult_long_meds_ana[i, "start_day"]) && !is.na(adult_long_meds_ana[i,"start_month"]) && !is.na(adult_long_meds_ana[i,"start_year"]))
  {
    adult_long_meds_ana[i,"start_date"] = paste(adult_long_meds_ana[i,"start_year"],adult_long_meds_ana[i,"start_month"],"1", sep ="-")
  }
  if (is.na(adult_long_meds_ana[i, "start_day"]) && is.na(adult_long_meds_ana[i,"start_month"]) && !is.na(adult_long_meds_ana[i,"start_year"]))
  {
    adult_long_meds_ana[i,"start_date"] = paste(adult_long_meds_ana[i,"start_year"],"1","1", sep ="-")
  }
}

adult_long_meds_ana$start_date = as.Date(adult_long_meds_ana$start_date) 
adult_long_meds_ana$screening.visit_date = as.Date(adult_long_meds_ana$screening.visit_date)

#End date
for (i in 1:1666){
  if (!is.na(adult_long_meds_ana[i, "end_day"]) && !is.na(adult_long_meds_ana[i,"end_month"]) && !is.na(adult_long_meds_ana[i,"end_year"]))
  {
    adult_long_meds_ana[i,"end_date"] = paste(adult_long_meds_ana[i, "end_year"],adult_long_meds_ana[i,"end_month"],adult_long_meds_ana[i,"end_day"], sep ="-")
  }
  if (is.na(adult_long_meds_ana[i, "end_day"]) && !is.na(adult_long_meds_ana[i,"end_month"]) && !is.na(adult_long_meds_ana[i,"end_year"]))
  {
    adult_long_meds_ana[i,"end_date"] = paste(adult_long_meds_ana[i,"end_year"],adult_long_meds_ana[i,"end_month"],"1", sep ="-")
  }
  if (is.na(adult_long_meds_ana[i, "end_day"]) && is.na(adult_long_meds_ana[i,"end_month"]) && !is.na(adult_long_meds_ana[i,"end_year"]))
  {
    adult_long_meds_ana[i,"end_date"] = paste(adult_long_meds_ana[i,"end_year"],"1","1", sep ="-")
  }
}

adult_long_meds_ana$end_date = as.Date(adult_long_meds_ana$end_date) 

#Import visit dates to compare with medication dates and identify when the meds started or stopped

#Removed all dates in file below that were NA-NA-NA to blank(na) and replaced all dates where the month or day were NA with 01
adult_med_ecrf_report = read.csv("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/Medication/Medications_20150607.csv")

#Calculate start and end visits of a drug
for (x in 1:5653){
  if (adult_med_ecrf_report[x,21] < 0 ){
    adult_med_ecrf_report[x, 'test'] <- "before_screening"
  }
  if (adult_med_ecrf_report[x,21] >= adult_med_ecrf_report[x,22] & adult_med_ecrf_report[x,21] < 0){
    adult_med_ecrf_report[x, 'test'] <- "before_screening"
  }
  if (adult_med_ecrf_report[x,21] ==  0 & adult_med_ecrf_report[x,21] < adult_med_ecrf_report[x,22]){
    adult_med_ecrf_report[x, 'test'] <- "at_OR_after_screening"
  }
  if (adult_med_ecrf_report[x,22] >=  0 & adult_med_ecrf_report[x,22] < adult_med_ecrf_report[x,23]){
    adult_med_ecrf_report[x, 'test'] <- "at_OR_after_baseline"
  }
}
table(adult_med_ecrf_report$test)
rm(x)

#Create unique identifiers by concatenating the following elements: patient_code, medication, dose, start & end dates ++ for both med dataframes

adult_long_meds_ana$unique_id = paste(adult_long_meds_ana$patient_code,adult_long_meds_ana$medication, adult_long_meds_ana$dose,adult_long_meds_ana$start_date,adult_long_meds_ana$end_date,sep = "|")

adult_med_ecrf_report$unique_id = paste(adult_med_ecrf_report$Patient.Code,adult_med_ecrf_report$Medication, adult_med_ecrf_report$Dose,adult_med_ecrf_report$start_day,adult_med_ecrf_report$start_month,adult_med_ecrf_report$start_year,adult_med_ecrf_report$end_day,adult_med_ecrf_report$end_month,adult_med_ecrf_report$end_year,sep = "|")

  
  
