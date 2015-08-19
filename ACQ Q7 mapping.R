# ACQ Q7 mapping

acq11 = read.delim("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/ACQ_1_1_June2015_loading.txt", header = T)

merge(acq11, adult_long1[ , c("subjid","fev1_percentage")], by.x ="Patient.Code", by.y = "subjid", all =F) -> acq11

for (i in 1:nrow(acq11)){
  if(is.na(acq11$fev1_percentage[i])){
    acq11$Question.7[i] = NA
  }
  if(!is.na(acq11$fev1_percentage[i]) & acq11$fev1_percentage[i] > 95){
    acq11$Question.7[i] = 0
  }
  if(!is.na(acq11$fev1_percentage[i]) & acq11$fev1_percentage[i] <= 95 & acq11$fev1_percentage[i] >= 90){
    acq11$Question.7[i] = 1
  }
  if(!is.na(acq11$fev1_percentage[i]) & acq11$fev1_percentage[i] <= 89 & acq11$fev1_percentage[i] >= 80){
    acq11$Question.7[i] = 2
  }
  if(!is.na(acq11$fev1_percentage[i]) & acq11$fev1_percentage[i] <= 79 & acq11$fev1_percentage[i] >= 70){
    acq11$Question.7[i] = 3
  }
  if(!is.na(acq11$fev1_percentage[i]) & acq11$fev1_percentage[i] <= 69 & acq11$fev1_percentage[i] >= 60){
    acq11$Question.7[i] = 4
  }
  if(!is.na(acq11$fev1_percentage[i]) & acq11$fev1_percentage[i] <= 59 & acq11$fev1_percentage[i] >= 50){
    acq11$Question.7[i] = 5
  }
  if(!is.na(acq11$fev1_percentage[i]) & acq11$fev1_percentage[i] < 50){
    acq11$Question.7[i] = 6
  }
}

write.table(acq11, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/Longitutinal_Data/20150225/load/ACQ_1_1_June2015_loading_plus7.txt", sep = "\t", row.names= F, na = ".", quote =F)


