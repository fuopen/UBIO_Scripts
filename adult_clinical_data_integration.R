
is.present=function(x,softFail=T){
  
  if (is.numeric(x)){
    !is.na(x)
  } else if (is.factor(x) | is.character(x)){
    x != "" & !is.na(x)
  } else {
    if (softFail){
      NULL
    } else {
      stop("is.present only supports numeric, factor or character")
    } 
  }
}
# merges S and B1 reconcilling missings of shared variables. If a variable
# is shared, the B1 value is assigned unless it is missing in which case the 
# screening value S is taken. Original copies of shared variables are retained
# via the suffices .s abd .b1
sb1Merge=function(S,B1,trace=F,removeSuffixVars=T){
  
  # a helper function to list rows from an array only when they
  # contain missing values
  
  listNaRows=function(x){
    naRows=apply(is.na(x),1,any)
    x[naRows,]
  }
  
  #suffixes: character(2) specifying the suffixes to be used for making non-by names() unique.
  SB1=merge(S,B1,all=T,by="subjid",suffixes=c(".s",".b1"))
  
  # see the suffixes option in merge()
  
  # assuming no .s suffices existed in the original data, a .s is always
  # matched by a .b1. We pick these out by grep(), construct sVar0 (with
  # the .s removed) and .b1 (with it replaced by a .b1) ...
  
  for (sVar in grep("\\.s$",colnames(SB1),value=T)){
    sVar0=sub("\\.s","",sVar)
    sVar1=paste(sVar0,".b1",sep="")
    
    # ... and then assign the unfixed value with the .s value ...
    
    SB1[[sVar0]]=SB1[[sVar]]
    
    # ... unless a baseline .b1 is present
    
    haveB1=is.present(SB1[[sVar1]])
    
    #  check output and report any error state (the variable concerned) and
    #  stop any further action
    if (is.null(haveB1)){
      if (trace){
        cat("** is.present() detected unsupported data type for variable",sVar0,"\n",
            "   This variable will be ignored.\n\n")
      }
      haveB1=F
    }
    
    if (any(haveB1)){
      
      
      if (is.factor(SB1[,sVar0])){
        
        # this is a mess - to potentially add new levels to a factor
        # variable we have to coherse into a character and then back
        # again
        
        SB1[[sVar0]]=as.character(SB1[[sVar]])
        SB1[haveB1,sVar0]=as.character(SB1[haveB1,sVar1])
        
        SB1[[sVar0]]=as.factor(SB1[[sVar0]])   
      } else {
        SB1[haveB1,sVar0]=SB1[haveB1,sVar1]
      }
    }
    
    if (trace){
      cat("any cases where screening or baseline values contained missings for:",sVar0,"\n")
      Temp=SB1[,c(sVar0,sVar1,sVar)]
      print(listNaRows(Temp))
    }
  }
  
  # and option to remove the temporary columns to avoid confusion over NAs.
  # Note, however, that the new variables still appear at the end of the of data frame. 
  
  if (removeSuffixVars){
    SB1=SB1[,-grep("(\\.s$|\\.b1)",colnames(SB1))]
  }
  
  SB1
}

B1 <- read.csv('/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/May2014/processed/2014-05-16-adult_baseline_1.csv')

S <- read.csv('/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/May2014/processed/2014-05-16-adult_screening.csv')
SB1 <- sb1Merge(S,B1)

#Harmonise Units for pef variables
timeAdjust=1*(SB1$pef_pre_salbutamol_um == "l_sec")+60*(SB1$pef_pre_salbutamol_um == "l_min")
SB1$pef_pre_salbutamol=SB1$pef_pre_salbutamol/timeAdjust

timeAdjust=1*(SB1$pef_actual_um == "l_sec")+60*(SB1$pef_actual_um == "l_min")
SB1$pef_actual=SB1$pef_actual/timeAdjust

timeAdjust=1*(SB1$pef_predicted_um == "l_sec")+60*(SB1$pef_predicted_um == "l_min")
SB1$pef_predicted=SB1$pef_predicted/timeAdjust

#Harmonise nicotine_breath_test_unit 
nicotine_unit=1*(SB1$nicotine_breath_test_unit == "ppm")+1000*(SB1$nicotine_breath_test_unit == "ppb")
SB1$nicotine_breath_test=SB1$nicotine_breath_test/nicotine_unit

#BMI
SB1$bmi=round(SB1$weight/(SB1$height/100)^2,digit=2)

# tlc percentage
a1 <- c(7.99,6.60)
b1 <- c(-7.08,-5.79)
names(a1)=c("male","female")
names(b1)=names(a1)
SB1$tlc_predicted <- a1[SB1$gender]*SB1$height/100+b1[SB1$gender]
SB1$tlc_actual_predicted_perc <- round(100*SB1$plethysmography_tlc/SB1$tlc_predicted,digit=2)

#fev1
SB1$fev1_pre_salbutamol_percentage <- round(SB1$fev1_pre_salbutamol/SB1$fev1_predicted*100,digit=2)
SB1$fev1_post_salbutamol_percentage <- round(SB1$fev1_post_salbutamol/SB1$fev1_predicted*100,digit=2)
SB1$fev1_abs_change <- round(100*(SB1$fev1_post_salbutamol-SB1$fev1_pre_salbutamol)/SB1$fev1_predicted,digit=2)

#fvc
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
parental_hay_fever <- rep('no',ncol(SB1))
sel_yes <- SB1$hay_fever_father=='yes'|SB1$hay_fever_mother=='yes'
parental_hay_fever[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$hay_fever_father=='uncertain'&SB1$hay_fever_mother=='uncertain'
sel_uncertain2 <- SB1$hay_fever_father=='no'&SB1$hay_fever_mother=='uncertain'
sel_uncertain3 <- SB1$hay_fever_father=='uncertain'&SB1$hay_fever_mother=='no'
parental_hay_fever[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_hay_fever <- parental_hay_fever

#parental_eczema
parental_eczema <- rep('no',ncol(SB1))
sel_yes <- SB1$eczema_father=='yes'|SB1$eczema_mother=='yes'
parental_eczema[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$eczema_father=='uncertain'&SB1$eczema_mother=='uncertain'
sel_uncertain2 <- SB1$eczema_father=='no'&SB1$eczema_mother=='uncertain'
sel_uncertain3 <- SB1$eczema_father=='uncertain'&SB1$eczema_mother=='no'
parental_eczema[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_eczema <- parental_eczema

#parental_copd
parental_copd <- rep('no',ncol(SB1))
sel_yes <- SB1$copd_father=='yes'|SB1$copd_mother=='yes'
parental_copd[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$copd_father=='uncertain'&SB1$copd_mother=='uncertain'
sel_uncertain2 <- SB1$copd_father=='no'&SB1$copd_mother=='uncertain'
sel_uncertain3 <- SB1$copd_father=='uncertain'&SB1$copd_mother=='no'
parental_copd[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_copd <- parental_copd

#parental_asthma
parental_asthma <- rep('no',ncol(SB1))
sel_yes <- SB1$asthma_father=='yes'|SB1$asthma_mother=='yes'
parental_asthma[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$asthma_father=='uncertain'&SB1$asthma_mother=='uncertain'
sel_uncertain2 <- SB1$asthma_father=='no'&SB1$asthma_mother=='uncertain'
sel_uncertain3 <- SB1$asthma_father=='uncertain'&SB1$asthma_mother=='no'
parental_asthma[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$parental_asthma <- parental_asthma

#physical_activity
physical_activity <- rep('no',ncol(SB1))
sel_yes <- SB1$routine_physical_activities=='yes'|SB1$physical_exercise=='yes'
physical_activity[sel_yes] <- 'yes'
sel_uncertain1 <- SB1$routine_physical_activities=='uncertain'&SB1$physical_exercise=='uncertain'
sel_uncertain2 <- SB1$routine_physical_activities=='no'&SB1$physical_exercise=='uncertain'
sel_uncertain3 <- SB1$routine_physical_activities=='uncertain'&SB1$physical_exercise=='no'
physical_activity[sel_uncertain1|sel_uncertain2|sel_uncertain3] <- 'uncertain'
SB1$physical_activity <- physical_activity

#airflow_limit_quanjer
sel <- SB1$post_salbutamol_fev1_fvc_ratio_percentage < SB1$fev1_fvc_ration_LLN
airflow_limit_quanjer <- rep('no',ncol(SB1))
airflow_limit_quanjer[sel] <- 'yes'
SB1$airflow_limit_quanjer <- airflow_limit_quanjer

#airflow_limit_brinke
sel1 <- SB1$post_salbutamol_fev1_fvc_ratio_percentage < 75
sel2 <- SB1$fev1_post_salbutamol_percentage < 75
sel3 <- SB1$tlc_actual_predicted_perc > 75
sel <- (sel1|sel2)&sel3
airflow_limit_brinke <- rep('no',ncol(SB1))
airflow_limit_brinke[sel] <- 'yes'
SB1$airflow_limit_brinke <- airflow_limit_brinke

#$EOL Insert one last column containing EOL, so that removal of inner cell line breaks is possible
#eol <- 'eol'
#SB1$eol <- eol

#Write_data_file
write.csv(SB1, file="//Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/May2014/processed/2014-05-16-adult_clinical_scr_bd1.csv")