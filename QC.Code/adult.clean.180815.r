# Functions for data quality checks on data downloaded by Nubilaria
# Clean version August 2015


# vw interim longitudinal 1 20121018.txt
L1<-function(fname){
l1<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(l1)){
    if(l1$patient_code[i]==""){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"patient_code","","No patient code\n",sep=","))}
    if(l1$investigator[i]==""){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"investigator","","No investigator code\n",sep=","))}
    if(l1$gender[i]==""){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"gender","","No gender code\n",sep=","))}
    if(!is.na(l1$systolic_blood_pressure[i])){
      if(as.numeric(as.character(l1$systolic_blood_pressure[i]))>200){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"systolic_blood_pressure",as.numeric(as.character(l1$systolic_blood_pressure[i])),"Greater than 200\n",sep=","))}else
      if(as.numeric(as.character(l1$systolic_blood_pressure[i]))<80){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"systolic_blood_pressure",as.numeric(as.character(l1$systolic_blood_pressure[i])),"Less than 80\n",sep=","))}
    }
    if(!is.na(l1$diastolic_blood_pressure[i])){
      if(as.numeric(as.character(l1$diastolic_blood_pressure[i]))>120){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"diastolic_blood_pressure",as.numeric(as.character(l1$diastolic_blood_pressure[i])),"Greater than 120\n",sep=","))}else
      if(as.numeric(as.character(l1$diastolic_blood_pressure[i]))<40){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"diastolic_blood_pressure",as.numeric(as.character(l1$diastolic_blood_pressure[i])),"Less than 40\n",sep=","))}
    }
    if(!is.na(l1$heart_rate[i])){
      if(as.numeric(as.character(l1$heart_rate[i]))>120){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"heart_rate",as.numeric(as.character(l1$heart_rate[i])),"Greater than 120\n",sep=","))} else
      if(as.numeric(as.character(l1$heart_rate[i]))<30){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"heart_rate",as.numeric(as.character(l1$heart_rate[i])),"Less than 30\n",sep=","))}
    }
    if(!is.na(l1$respiratory_rate[i])){
      if(as.numeric(as.character(l1$respiratory_rate[i]))>36){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"respiratory_rate",as.numeric(as.character(l1$respiratory_rate[i])),"Greater than 36\n",sep=","))}else
      if(as.numeric(as.character(l1$respiratory_rate[i]))<6){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"respiratory_rate",as.numeric(as.character(l1$respiratory_rate[i])),"Less than 6\n",sep=","))}
    }
    if(l1$valid_spirometry_date[i]!=""){
    day<-unlist(strsplit(as.character(l1$valid_spirometry_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(l1$valid_spirometry_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(l1$valid_spirometry_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"valid_spirometry_date",l1$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"valid_spirometry_date",l1$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"valid_spirometry_date",l1$valid_spirometry_date[i],"Incorrect date format\n",sep=","))}
    }
    if(!is.na(l1$fev1_actual[i])){
      if(as.numeric(as.character(l1$fev1_actual[i]))>6.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_actual",as.numeric(as.character(l1$fev1_actual[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(l1$fev1_actual[i]))<0.2){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_actual",as.numeric(as.character(l1$fev1_actual[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(l1$fev1_predicted[i])){
      if(as.numeric(as.character(l1$fev1_predicted[i]))>6.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_predicted",as.numeric(as.character(l1$fev1_predicted[i])),"Greater than 6.5\n",sep=","))} else
      if(as.numeric(as.character(l1$fev1_predicted[i]))<0.3){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_predicted",as.numeric(as.character(l1$fev1_predicted[i])),"Less than 0.3\n",sep=","))}
    }
    if(!is.na(l1$fev1_percentage[i])){
      if(as.numeric(as.character(l1$fev1_percentage[i]))>130){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_percentage",as.numeric(as.character(l1$fev1_percentage[i])),"Greater than 130%: Check race\n",sep=","))} else
      if(as.numeric(as.character(l1$fev1_percentage[i]))<10){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_percentage",as.numeric(as.character(l1$fev1_percentage[i])),"Less than 10%\n",sep=","))}
    }
    if(!is.na(l1$fvc_actual[i])){
      if(as.numeric(as.character(l1$fvc_actual[i]))>7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_actual",as.numeric(as.character(l1$fvc_actual[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(l1$fvc_actual[i]))<0.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_actual",as.numeric(as.character(l1$fvc_actual[i])),"Less than 0.5\n",sep=","))} # row 40
      }

    if(!is.na(l1$fvc_predicted[i])){
      if(as.numeric(as.character(l1$fvc_predicted[i]))>7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_predicted",as.numeric(as.character(l1$fvc_predicted[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(l1$fvc_predicted[i]))<1){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_predicted",as.numeric(as.character(l1$fvc_predicted[i])),"Less than 1\n",sep=","))}
    }
    if(!is.na(l1$fvc_percentage[i])){
      if(as.numeric(as.character(l1$fvc_percentage[i]))>140){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_percentage",as.numeric(as.character(l1$fvc_percentage[i])),"Greater than 140: Check race\n",sep=","))}else
      if(as.numeric(as.character(l1$fvc_percentage[i]))<30){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_percentage",as.numeric(as.character(l1$fvc_percentage[i])),"Less than 30\n",sep=","))}
    }
    if(!is.na(l1$fef2575_actual[i])){
      if(as.numeric(as.character(l1$fef2575_actual[i]))>6){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_actual",as.numeric(as.character(l1$fef2575_actual[i])),"Greater than 6: Check units\n",sep=","))}else
      if(as.numeric(as.character(l1$fef2575_actual[i]))<0.1){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_actual",as.numeric(as.character(l1$fef2575_actual[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(l1$fef2575_predicted[i])){
      if(as.numeric(as.character(l1$fef2575_predicted[i]))>7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_predicted",as.numeric(as.character(l1$fef2575_predicted[i])),"Greater than 7: Check units\n",sep=","))} else
      if(as.numeric(as.character(l1$fef2575_predicted[i]))<1.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_predicted",as.numeric(as.character(l1$fef2575_predicted[i])),"Less than 1.5: Check units\n",sep=","))}
    }
    if(!is.na(l1$fef2575_percentage[i])){
      if(as.numeric(as.character(l1$fef2575_percentage[i]))>140){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_percentage",as.numeric(as.character(l1$fef2575_percentage[i])),"Greater than 140%\n",sep=","))} else
      if(as.numeric(as.character(l1$fef2575_percentage[i]))<5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_percentage",as.numeric(as.character(l1$fef2575_percentage[i])),"Less than 5%\n",sep=","))}  # row 45
    }
    if(!is.na(l1$pef_actual[i])){
    if(l1$pef_actual_um[i]==""){
    IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_actual",as.numeric(as.character(l1$pef_actual[i])),"Units not specified\n",sep=","))
    } else
    if(l1$pef_actual_um[i]=="l_min"){
      if(as.numeric(as.character(l1$pef_actual[i]))/60>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_actual",as.numeric(as.character(l1$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_actual[i]))/60<0.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_actual",as.numeric(as.character(l1$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }else
    if(l1$pef_actual_um[i]=="l_sec"){
      if(as.numeric(as.character(l1$pef_actual[i]))>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_actual",as.numeric(as.character(l1$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_actual[i]))<0.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_actual",as.numeric(as.character(l1$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }
    }
    if(!is.na(l1$pef_predicted[i])){
    if(l1$pef_predicted_um[i]==""){
    IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_predicted",as.numeric(as.character(l1$pef_predicted[i])),"Units not specified\n",sep=","))
    } else
    if(l1$pef_predicted_um[i]=="l_min"){
        if(as.numeric(as.character(l1$pef_predicted[i]))/60>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_predicted",as.numeric(as.character(l1$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(l1$pef_predicted[i]))/60<0.7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_predicted",as.numeric(as.character(l1$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }else
    if(l1$pef_predicted_um[i]=="l_sec"){
        if(as.numeric(as.character(l1$pef_predicted[i]))>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_predicted",as.numeric(as.character(l1$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(l1$pef_predicted[i]))<0.7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_predicted",as.numeric(as.character(l1$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }
    }
    if(!is.na(l1$pef_percentage[i])){
      if(as.numeric(as.character(l1$pef_percentage[i]))>150){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_percentage",as.numeric(as.character(l1$pef_percentage[i])),"Greater than 150\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_percentage[i]))<10){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_percentage",as.numeric(as.character(l1$pef_percentage[i])),"Less than 10\n",sep=","))}      # row 50
    }
    
    if(!is.na(l1$fev1_pre_salbutamol[i])){
      if(as.numeric(as.character(l1$fev1_pre_salbutamol[i]))>6.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_pre_salbutamol",as.numeric(as.character(l1$fev1_pre_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(l1$fev1_pre_salbutamol[i]))<0.2){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_pre_salbutamol",as.numeric(as.character(l1$fev1_pre_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(l1$fev1_post_salbutamol[i])){
      if(as.numeric(as.character(l1$fev1_post_salbutamol[i]))>6.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_post_salbutamol",as.numeric(as.character(l1$fev1_post_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(l1$fev1_post_salbutamol[i]))<0.2){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_post_salbutamol",as.numeric(as.character(l1$fev1_post_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(l1$fev1_change[i])){
      if(as.numeric(as.character(l1$fev1_change[i]))>75){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_change",as.numeric(as.character(l1$fev1_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(l1$fev1_change[i]))+10<0){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fev1_change",as.numeric(as.character(l1$fev1_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(l1$fvc_pre_salbutamol[i])){
      if(as.numeric(as.character(l1$fvc_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_pre_salbutamol",as.numeric(as.character(l1$fvc_pre_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(l1$fvc_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_pre_salbutamol",as.numeric(as.character(l1$fvc_pre_salbutamol[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(l1$fvc_post_salbutamol[i])){
      if(as.numeric(as.character(l1$fvc_post_salbutamol[i]))>7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_post_salbutamol",as.numeric(as.character(l1$fvc_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(l1$fvc_post_salbutamol[i]))<0.6){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_post_salbutamol",as.numeric(as.character(l1$fvc_post_salbutamol[i])),"Less than 0.6: Check units\n",sep=","))}
    }
    if(!is.na(l1$fvc_change[i])){
      if(as.numeric(as.character(l1$fvc_change[i]))>75){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_change",as.numeric(as.character(l1$fvc_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(l1$fvc_change[i]))+10<0){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fvc_change",as.numeric(as.character(l1$fvc_change[i])),"Less than -10\n",sep=","))}    # row 59
    }

    if(!is.na(l1$fef2575_pre_salbutamol[i])){
      if(as.numeric(as.character(l1$fef2575_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(l1$fef2575_pre_salbutamol[i])),"Greater than 7: Check units\n",sep=","))}else
      if(as.numeric(as.character(l1$fef2575_pre_salbutamol[i]))<0.1){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(l1$fef2575_pre_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(l1$fef2575_post_salbutamol[i])){
      if(as.numeric(as.character(l1$fef2575_post_salbutamol[i]))>7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_post_salbutamol",as.numeric(as.character(l1$fef2575_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(l1$fef2575_post_salbutamol[i]))<0.1){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_post_salbutamol",as.numeric(as.character(l1$fef2575_post_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(l1$fef2575_change[i])){
      if(as.numeric(as.character(l1$fef2575_change[i]))>100){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_change",as.numeric(as.character(l1$fef2575_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(l1$fef2575_change[i]))+10<0){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"fef2575_change",as.numeric(as.character(l1$fef2575_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(l1$pef_pre_salbutamol[i])){
    if(l1$pef_pre_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_pre_salbutamol",as.numeric(as.character(l1$pef_pre_salbutamol[i])),"Units not specified\n",sep=","))
    } else
    if(l1$pef_pre_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(l1$pef_pre_salbutamol[i]))/60>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_pre_salbutamol",as.numeric(as.character(l1$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_pre_salbutamol[i]))/60<0.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_pre_salbutamol",as.numeric(as.character(l1$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }else
    if(l1$pef_pre_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(l1$pef_pre_salbutamol[i]))>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_pre_salbutamol",as.numeric(as.character(l1$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_pre_salbutamol",as.numeric(as.character(l1$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }
    }
    if(!is.na(l1$pef_post_salbutamol[i])){
    if(l1$pef_post_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_post_salbutamol",as.numeric(as.character(l1$pef_post_salbutamol[i])),"Units not specified\n",sep=","))
    } else
   if(l1$pef_post_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(l1$pef_post_salbutamol[i]))/60>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_post_salbutamol",as.numeric(as.character(l1$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_post_salbutamol[i]))/60<0.7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_post_salbutamol",as.numeric(as.character(l1$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    } else
   if(l1$pef_post_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(l1$pef_post_salbutamol[i]))>14){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_post_salbutamol",as.numeric(as.character(l1$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_post_salbutamol[i]))<0.7){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_post_salbutamol",as.numeric(as.character(l1$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    }
    }
    if(!is.na(l1$pef_change[i])){
      if(as.numeric(as.character(l1$pef_change[i]))>100){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_change",as.numeric(as.character(l1$pef_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(l1$pef_change[i]))+10 <0){IssueOut(paste(fname,i,l1[i,1],l1[i,2],l1[i,3],"pef_change",as.numeric(as.character(l1$pef_change[i])),"Less than -10\n",sep=","))}
    }

  } # i

} # function


########################################
# vw interim baseline day 1 20121018.txt
B1<-function(fname){
b1<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(b1)){
    if(b1$patient_code[i]==""){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"patient_code","","No patient code\n",sep=","))}    # row 70
    if(b1$investigator[i]==""){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"investigator","","No investigator code\n",sep=","))}
    if(b1$mars_not_completed[i]!=""){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"mars_not_completed",b1$mars_not_completed[i],"MARS incomplete\n",sep=","))}  # row 80

    if(b1$pregnancy_test_positive[i]=="no" && b1$gender[i]=="male"){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pregnancy_test_positive",b1$pregnancy_test_positive[i],"Gender is male\n",sep=","))}
    if(b1$urine_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$urine_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$urine_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$urine_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_sampling_date",b1$urine_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_sampling_date",b1$urine_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_sampling_date",b1$urine_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$cotinine_was_present[i]=="yes"){
    IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cotinine_was_present",b1$cotinine_was_present[i],"Positive cotinine\n",sep=","))
    }
    if(b1$nicotine_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$nicotine_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$nicotine_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$nicotine_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"nicotine_sampling_date",b1$nicotine_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"nicotine_sampling_date",b1$nicotine_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"nicotine_sampling_date",b1$nicotine_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
     if(!is.na(b1$systolic_blood_pressure[i])){
      if(as.numeric(as.character(b1$systolic_blood_pressure[i]))>200){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"systolic_blood_pressure",as.numeric(as.character(b1$systolic_blood_pressure[i])),"Greater than 200\n",sep=","))}else
      if(as.numeric(as.character(b1$systolic_blood_pressure[i]))<80){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"systolic_blood_pressure",as.numeric(as.character(b1$systolic_blood_pressure[i])),"Less than 80\n",sep=","))}
    }
    if(!is.na(b1$diastolic_blood_pressure[i])){
      if(as.numeric(as.character(b1$diastolic_blood_pressure[i]))>120){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"diastolic_blood_pressure",as.numeric(as.character(b1$diastolic_blood_pressure[i])),"Greater than 120\n",sep=","))}else
      if(as.numeric(as.character(b1$diastolic_blood_pressure[i]))<40){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"diastolic_blood_pressure",as.numeric(as.character(b1$diastolic_blood_pressure[i])),"Less than 40\n",sep=","))}
    }
    if(!is.na(b1$heart_rate[i])){
      if(as.numeric(as.character(b1$heart_rate[i]))>120){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"heart_rate",as.numeric(as.character(b1$heart_rate[i])),"Greater than 120\n",sep=","))} else
      if(as.numeric(as.character(b1$heart_rate[i]))<30){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"heart_rate",as.numeric(as.character(b1$heart_rate[i])),"Less than 30\n",sep=","))}
    }
    if(!is.na(b1$respiratory_rate[i])){
      if(as.numeric(as.character(b1$respiratory_rate[i]))>36){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"respiratory_rate",as.numeric(as.character(b1$respiratory_rate[i])),"Greater than 36\n",sep=","))}else
      if(as.numeric(as.character(b1$respiratory_rate[i]))<6){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"respiratory_rate",as.numeric(as.character(b1$respiratory_rate[i])),"Less than 6\n",sep=","))}
    }
    if(b1$valid_spirometry_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$valid_spirometry_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$valid_spirometry_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$valid_spirometry_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"valid_spirometry_date",b1$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"valid_spirometry_date",b1$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"valid_spirometry_date",b1$valid_spirometry_date[i],"Incorrect date format\n",sep=","))}
    }
    if(!is.na(b1$fev1_actual[i])){
      if(as.numeric(as.character(b1$fev1_actual[i]))>6.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_actual",as.numeric(as.character(b1$fev1_actual[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(b1$fev1_actual[i]))<0.2){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_actual",as.numeric(as.character(b1$fev1_actual[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(b1$fev1_predicted[i])){
      if(as.numeric(as.character(b1$fev1_predicted[i]))>6.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_predicted",as.numeric(as.character(b1$fev1_predicted[i])),"Greater than 6.5\n",sep=","))} else
      if(as.numeric(as.character(b1$fev1_predicted[i]))<0.3){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_predicted",as.numeric(as.character(b1$fev1_predicted[i])),"Less than 0.3\n",sep=","))}
    }
    if(!is.na(b1$fev1_percentage[i])){
      if(as.numeric(as.character(b1$fev1_percentage[i]))>130){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_percentage",as.numeric(as.character(b1$fev1_percentage[i])),"Greater than 130%: Check race\n",sep=","))} else
      if(as.numeric(as.character(b1$fev1_percentage[i]))<10){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_percentage",as.numeric(as.character(b1$fev1_percentage[i])),"Less than 10%\n",sep=","))}  # row 121
    }
    
    if(!is.na(b1$fvc_actual[i])){
      if(as.numeric(as.character(b1$fvc_actual[i]))>7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_actual",as.numeric(as.character(b1$fvc_actual[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(b1$fvc_actual[i]))<0.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_actual",as.numeric(as.character(b1$fvc_actual[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(b1$fvc_predicted[i])){
      if(as.numeric(as.character(b1$fvc_predicted[i]))>7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_predicted",as.numeric(as.character(b1$fvc_predicted[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(b1$fvc_predicted[i]))<1){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_predicted",as.numeric(as.character(b1$fvc_predicted[i])),"Less than 1\n",sep=","))}
    }
    if(!is.na(b1$fvc_percentage[i])){
      if(as.numeric(as.character(b1$fvc_percentage[i]))>140){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_percentage",as.numeric(as.character(b1$fvc_percentage[i])),"Greater than 140: Check race\n",sep=","))}else
      if(as.numeric(as.character(b1$fvc_percentage[i]))<30){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_percentage",as.numeric(as.character(b1$fvc_percentage[i])),"Less than 30\n",sep=","))}
    }
    if(!is.na(b1$fef2575_actual[i])){
      if(as.numeric(as.character(b1$fef2575_actual[i]))>6){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_actual",as.numeric(as.character(b1$fef2575_actual[i])),"Greater than 6: Check units\n",sep=","))}else
      if(as.numeric(as.character(b1$fef2575_actual[i]))<0.1){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_actual",as.numeric(as.character(b1$fef2575_actual[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(b1$fef2575_predicted[i])){
      if(as.numeric(as.character(b1$fef2575_predicted[i]))>7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_predicted",as.numeric(as.character(b1$fef2575_predicted[i])),"Greater than 7: Check units\n",sep=","))} else
      if(as.numeric(as.character(b1$fef2575_predicted[i]))<1.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_predicted",as.numeric(as.character(b1$fef2575_predicted[i])),"Less than 1.5: Check units\n",sep=","))}
    }
    if(!is.na(b1$fef2575_percentage[i])){
      if(as.numeric(as.character(b1$fef2575_percentage[i]))>140){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_percentage",as.numeric(as.character(b1$fef2575_percentage[i])),"Greater than 140%\n",sep=","))} else
      if(as.numeric(as.character(b1$fef2575_percentage[i]))<5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_percentage",as.numeric(as.character(b1$fef2575_percentage[i])),"Less than 5%\n",sep=","))}
    }
    if(!is.na(b1$pef_actual[i])){
    if(b1$pef_actual_um[i]==""){
    IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_actual",as.numeric(as.character(b1$pef_actual[i])),"Units not specified\n",sep=","))
    } else
    if(b1$pef_actual_um[i]=="l_min"){
      if(as.numeric(as.character(b1$pef_actual[i]))/60>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_actual",as.numeric(as.character(b1$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_actual[i]))/60<0.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_actual",as.numeric(as.character(b1$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }else
    if(b1$pef_actual_um[i]=="l_sec"){
      if(as.numeric(as.character(b1$pef_actual[i]))>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_actual",as.numeric(as.character(b1$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_actual[i]))<0.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_actual",as.numeric(as.character(b1$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }
    }
    if(!is.na(b1$pef_predicted[i])){
    if(b1$pef_predicted_um[i]==""){
    IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_predicted",as.numeric(as.character(b1$pef_predicted[i])),"Units not specified\n",sep=","))
    } else
    if(b1$pef_predicted_um[i]=="l_min"){
        if(as.numeric(as.character(b1$pef_predicted[i]))/60>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_predicted",as.numeric(as.character(b1$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(b1$pef_predicted[i]))/60<0.7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_predicted",as.numeric(as.character(b1$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }else
    if(b1$pef_predicted_um[i]=="l_sec"){
        if(as.numeric(as.character(b1$pef_predicted[i]))>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_predicted",as.numeric(as.character(b1$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(b1$pef_predicted[i]))<0.7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_predicted",as.numeric(as.character(b1$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }
    }
    if(!is.na(b1$pef_percentage[i])){
      if(as.numeric(as.character(b1$pef_percentage[i]))>150){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_percentage",as.numeric(as.character(b1$pef_percentage[i])),"Greater than 150\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_percentage[i]))<10){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_percentage",as.numeric(as.character(b1$pef_percentage[i])),"Less than 10\n",sep=","))}
    }
    if(b1$reversibility_test_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$reversibility_test_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$reversibility_test_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$reversibility_test_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"reversibility_test_date",b1$reversibility_test_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"reversibility_test_date",b1$reversibility_test_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"reversibility_test_date",b1$reversibility_test_date[i],"Incorrect date format\n",sep=","))}
    }
    if(!is.na(b1$fev1_pre_salbutamol[i])){
      if(as.numeric(as.character(b1$fev1_pre_salbutamol[i]))>6.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_pre_salbutamol",as.numeric(as.character(b1$fev1_pre_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(b1$fev1_pre_salbutamol[i]))<0.2){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_pre_salbutamol",as.numeric(as.character(b1$fev1_pre_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(b1$fev1_post_salbutamol[i])){
      if(as.numeric(as.character(b1$fev1_post_salbutamol[i]))>6.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_post_salbutamol",as.numeric(as.character(b1$fev1_post_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(b1$fev1_post_salbutamol[i]))<0.2){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_post_salbutamol",as.numeric(as.character(b1$fev1_post_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(b1$fev1_change[i])){
      if(as.numeric(as.character(b1$fev1_change[i]))>75){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_change",as.numeric(as.character(b1$fev1_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(b1$fev1_change[i]))+10<0){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fev1_change",as.numeric(as.character(b1$fev1_change[i])),"Less than -10\n",sep=","))}     # row 139
    }
    
    if(!is.na(b1$fvc_pre_salbutamol[i])){
      if(as.numeric(as.character(b1$fvc_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_pre_salbutamol",as.numeric(as.character(b1$fvc_pre_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(b1$fvc_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_pre_salbutamol",as.numeric(as.character(b1$fvc_pre_salbutamol[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(b1$fvc_post_salbutamol[i])){
      if(as.numeric(as.character(b1$fvc_post_salbutamol[i]))>7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_post_salbutamol",as.numeric(as.character(b1$fvc_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(b1$fvc_post_salbutamol[i]))<0.6){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_post_salbutamol",as.numeric(as.character(b1$fvc_post_salbutamol[i])),"Less than 0.6: Check units\n",sep=","))}
    }
    if(!is.na(b1$fvc_change[i])){
      if(as.numeric(as.character(b1$fvc_change[i]))>75){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_change",as.numeric(as.character(b1$fvc_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(b1$fvc_change[i]))+10<0){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fvc_change",as.numeric(as.character(b1$fvc_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(b1$fef2575_pre_salbutamol[i])){
      if(as.numeric(as.character(b1$fef2575_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(b1$fef2575_pre_salbutamol[i])),"Greater than 7: Check units\n",sep=","))}else
      if(as.numeric(as.character(b1$fef2575_pre_salbutamol[i]))<0.1){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(b1$fef2575_pre_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(b1$fef2575_post_salbutamol[i])){
      if(as.numeric(as.character(b1$fef2575_post_salbutamol[i]))>7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_post_salbutamol",as.numeric(as.character(b1$fef2575_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(b1$fef2575_post_salbutamol[i]))<0.1){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_post_salbutamol",as.numeric(as.character(b1$fef2575_post_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(b1$fef2575_change[i])){
      if(as.numeric(as.character(b1$fef2575_change[i]))>100){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_change",as.numeric(as.character(b1$fef2575_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(b1$fef2575_change[i]))+10<0){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fef2575_change",as.numeric(as.character(b1$fef2575_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(b1$pef_pre_salbutamol[i])){
    if(b1$pef_pre_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_pre_salbutamol",as.numeric(as.character(b1$pef_pre_salbutamol[i])),"Units not specified\n",sep=","))
    } else
    if(b1$pef_pre_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(b1$pef_pre_salbutamol[i]))/60>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_pre_salbutamol",as.numeric(as.character(b1$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_pre_salbutamol[i]))/60<0.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_pre_salbutamol",as.numeric(as.character(b1$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }else
    if(b1$pef_pre_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(b1$pef_pre_salbutamol[i]))>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_pre_salbutamol",as.numeric(as.character(b1$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_pre_salbutamol",as.numeric(as.character(b1$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }
    }
    if(!is.na(b1$pef_post_salbutamol[i])){
    if(b1$pef_post_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_post_salbutamol",as.numeric(as.character(b1$pef_post_salbutamol[i])),"Units not specified\n",sep=","))
    } else
   if(b1$pef_post_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(b1$pef_post_salbutamol[i]))/60>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_post_salbutamol",as.numeric(as.character(b1$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_post_salbutamol[i]))/60<0.7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_post_salbutamol",as.numeric(as.character(b1$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    } else
   if(b1$pef_post_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(b1$pef_post_salbutamol[i]))>14){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_post_salbutamol",as.numeric(as.character(b1$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_post_salbutamol[i]))<0.7){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_post_salbutamol",as.numeric(as.character(b1$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    }
    }
    if(!is.na(b1$pef_change[i])){
      if(as.numeric(as.character(b1$pef_change[i]))>100){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_change",as.numeric(as.character(b1$pef_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(b1$pef_change[i]))+10 <0){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"pef_change",as.numeric(as.character(b1$pef_change[i])),"Less than -10\n",sep=","))}   # row 150
    }
    
    if(b1$plethysmography_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$plethysmography_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$plethysmography_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$plethysmography_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_date",b1$plethysmography_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_date",b1$plethysmography_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_date",b1$plethysmography_date[i],"Incorrect date format\n",sep=","))}
    }
    if(!is.na(b1$plethysmography_tlc[i])){
      if(as.numeric(as.character(b1$plethysmography_tlc[i]))>12){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_tlc",as.numeric(as.character(b1$plethysmography_tlc[i])),"Greater than 12\n",sep=","))}else
      if(as.numeric(as.character(b1$plethysmography_tlc[i]))<3){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_tlc",as.numeric(as.character(b1$plethysmography_tlc[i])),"Less than 3\n",sep=","))}
    }
    if(!is.na(b1$plethysmography_rv[i])){
      if(as.numeric(as.character(b1$plethysmography_rv[i]))>8){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_rv",as.numeric(as.character(b1$plethysmography_rv[i])),"Greater than 8\n",sep=","))}else
      if(as.numeric(as.character(b1$plethysmography_rv[i]))<0.5){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_rv",as.numeric(as.character(b1$plethysmography_rv[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(b1$plethysmography_sgaw[i])){
      if(as.numeric(as.character(b1$plethysmography_sgaw[i]))>6){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_sgaw",as.numeric(as.character(b1$plethysmography_sgaw[i])),"Greater than 6\n",sep=","))}else
      if(as.numeric(as.character(b1$plethysmography_sgaw[i]))==0){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plethysmography_sgaw",as.numeric(as.character(b1$plethysmography_sgaw[i])),"Equals zero\n",sep=","))}
    }
   if(b1$fot_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$fot_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$fot_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$fot_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fot_date",b1$fot_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fot_date",b1$fot_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"fot_date",b1$fot_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$lge_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$lge_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$lge_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$lge_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"lge_sampling_date",b1$lge_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"lge_sampling_date",b1$lge_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"lge_sampling_date",b1$lge_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
   if(!is.na(b1$lge_total[i])){
    if(as.numeric(as.character(b1$lge_total[i]))>5000){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"lge_total",as.numeric(as.character(b1$lge_total[i])),"Greater than 5000: Check units\n",sep=","))}
   }
    if(b1$skin_prick_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$skin_prick_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$skin_prick_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$skin_prick_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"skin_prick_date",b1$skin_prick_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"skin_prick_date",b1$skin_prick_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"skin_prick_date",b1$skin_prick_date[i],"Incorrect date format\n",sep=","))}
    }
   if(b1$sputum_able[i]=="no" && b1$sputum_able_specify[i]==""){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_able_specify","","Sputum unavailable; no reason specified\n",sep=","))}   # row 189
   if(b1$sputum_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$sputum_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$sputum_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$sputum_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sampling_date",b1$sputum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sampling_date",b1$sputum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sampling_date",b1$sputum_sampling_date[i],"Incorrect date format\n",sep=","))}
   }
   if(b1$sputum_sampling_time[i]!=""){
    hr<-unlist(strsplit(as.character(b1$sputum_sampling_time[i]),"\\."))[1]
    mi<-unlist(strsplit(as.character(b1$sputum_sampling_time[i]),"\\."))[2]
    if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sampling_time",b1$sputum_sampling_time[i],"Incorrect time format\n",sep=","))} else
    if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sampling_time",b1$sputum_sampling_time[i],"Incorrect time format\n",sep=","))}
   }
   if(b1$sputum_able[i]=="yes" && b1$sputum_processing_time[i]==""){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_processing_time","","Sputum available: no processing time specified\n",sep=","))}
   if(b1$sputum_able[i]=="yes" && b1$sputum_consistency[i]==""){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_consistency","","Sputum available: no consistency specified\n",sep=","))}
   if(b1$sputum_able[i]=="yes" && b1$sputum_colour[i]==""){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_colour","","Sputum available: no colour specified\n",sep=","))}
    if(b1$sputum_sample_bacteriology[i]!=""){
    no<-unlist(strsplit(as.character(b1$sputum_sample_bacteriology[i]),"AB"))[2]
    no2<-unlist(strsplit(as.character(b1$sputum_sample_bacteriology[i]),"ab"))[2]
    if(is.na(no) && is.na(no2)){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sample_bacteriology",b1$sputum_sample_bacteriology[i],"Incorrect format: does not start with AB or ab\n",sep=","))}
     }
   if(!is.na(b1$sputum_weight[i]) && b1$sputum_weight[i] > 2){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_weight",b1$sputum_weight[i],"Greater than 2: Check units\n",sep=","))}
   if(!is.na(b1$sputum_weight[i]) && is.na(b1$resuspension_volume[i])){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"resuspension_volume","","Sputum weight but no resuspension volume\n",sep=","))}
    if(!is.na(b1$leukocyte_count[i])){
      if(as.numeric(as.character(b1$leukocyte_count[i]))==0){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"leukocyte_count",as.numeric(as.character(b1$leukocyte_count[i])),"Equals zero\n",sep=","))}else
      if(as.numeric(as.character(b1$leukocyte_count[i]))>70){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"leukocyte_count",as.numeric(as.character(b1$leukocyte_count[i])),"Greater than 70\n",sep=","))}  # row 200
    }
  if(b1$sputum_sample_viability[i]!=""){
    if(b1$sputum_sample_viability[i]=="na"){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sample_viability",b1$sputum_sample_viability[i],"Missing value\n",sep=","))} else
    if(b1$sputum_sample_viability[i]=="no"){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"sputum_sample_viability",b1$sputum_sample_viability[i],"Inform Biobank\n",sep=","))}
  }
    if(!is.na(b1$parent_sample_code[i])) {
    d1<-unlist(strsplit(as.character(b1$parent_sample_code[i]),""))[1]
    if(as.numeric(d1)!=5 && as.numeric(d1)!=8){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"parent_sample_code",b1$parent_sample_code[i],"Incorrect format: does not start with 5 or 8\n",sep=","))} else
    if(length(unlist(strsplit(as.character(b1$parent_sample_code[i]),"")))!=4){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"parent_sample_code",b1$parent_sample_code[i],"Incorrect format: is not of length 4\n",sep=","))}
     }
    if(b1$cytospin_processing_time[i]!=""){
    hr<-unlist(strsplit(as.character(b1$cytospin_processing_time[i]),"\\."))[1]
    mi<-unlist(strsplit(as.character(b1$cytospin_processing_time[i]),"\\."))[2]
    if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cytospin_processing_time",b1$cytospin_processing_time[i],"Incorrect time format\n",sep=","))} else
    if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cytospin_processing_time",b1$cytospin_processing_time[i],"Incorrect time format\n",sep=","))}
    }
    if(b1$supernatant_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$supernatant_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$supernatant_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"supernatant_time",b1$supernatant_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"supernatant_time",b1$supernatant_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"supernatant_time",b1$supernatant_time[i],"Incorrect time format\n",sep=","))}
    }
    if(!is.na(b1$supernatant_parent[i])){
    d1<-unlist(strsplit(as.character(b1$supernatant_parent[i]),""))[1]
# FOLLOWING LINE CAUSES NAS BY COERCION WARNING
    if(is.na(as.numeric(d1))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"supernatant_parent",b1$supernatant_parent[i],"Incorrect format: does not start with 5 or 8\n",sep=","))} else
    if(as.numeric(d1)!=5 && as.numeric(d1)!=8){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"supernatant_parent",b1$supernatant_parent[i],"Incorrect format: does not start with 5 or 8\n",sep=","))} else
    if(length(unlist(strsplit(as.character(b1$supernatant_parent[i]),"")))!=4){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"supernatant_parent",b1$supernatant_parent[i],"Incorrect format: is not of length 4\n",sep=","))}
     }
    if(b1$cell_pellet_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$cell_pellet_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$cell_pellet_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cell_pellet_time",b1$cell_pellet_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cell_pellet_time",b1$cell_pellet_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cell_pellet_time",b1$cell_pellet_time[i],"Incorrect time format\n",sep=","))}
    }
    if(!is.na(b1$cell_pellet_parent[i])){
    d1<-unlist(strsplit(as.character(b1$cell_pellet_parent[i]),""))[1]
    if(as.numeric(d1)!=5 && as.numeric(d1)!=8){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cell_pellet_parent",b1$cell_pellet_parent[i],"Incorrect format: does not begin with 5 or 8\n",sep=","))} else
    if(length(unlist(strsplit(as.character(b1$cell_pellet_parent[i]),"")))!=4){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"cell_pellet_parent",b1$cell_pellet_parent[i],"Incorrect format: is not of length 4\n",sep=","))}
     }
    if(b1$no_testing_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$no_testing_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$no_testing_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$no_testing_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"no_testing_date",b1$no_testing_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"no_testing_date",b1$no_testing_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"no_testing_date",b1$no_testing_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$no_testing_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$no_testing_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$no_testing_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"no_testing_time",b1$no_testing_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"no_testing_time",b1$no_testing_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"no_testing_time",b1$no_testing_time[i],"Incorrect time format\n",sep=","))}
    }
    if(b1$serum_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$serum_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$serum_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$serum_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sampling_date",b1$serum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sampling_date",b1$serum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sampling_date",b1$serum_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$serum_sampling_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$serum_sampling_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$serum_sampling_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sampling_time",b1$serum_sampling_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sampling_time",b1$serum_sampling_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sampling_time",b1$serum_sampling_time[i],"Incorrect time format\n",sep=","))}
    }
   if(b1$serum_sample_code[i]!="" && !is.na(b1$serum_sample_code[i])){
    d1<-unlist(strsplit(as.character(b1$serum_sample_code[i]),""))[1]
    if(as.numeric(d1)!=5 && as.numeric(d1)!=8){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sample_code",b1$serum_sample_code[i],"Incorrect format: does not begin with 5 or 8\n",sep=","))} else
    if(length(unlist(strsplit(as.character(b1$serum_sample_code[i]),"")))!=4){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_sample_code",b1$serum_sample_code[i],"Incorrect format: is not of length 4\n",sep=","))}
     }
    if(b1$serum_storage_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$serum_storage_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$serum_storage_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$serum_storage_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_storage_date",b1$serum_storage_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_storage_date",b1$serum_storage_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_storage_date",b1$serum_storage_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$serum_storage_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$serum_storage_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$serum_storage_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_storage_time",b1$serum_storage_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_storage_time",b1$serum_storage_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"serum_storage_time",b1$serum_storage_time[i],"Incorrect time format\n",sep=","))}
    }
    if(b1$plasma_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$plasma_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$plasma_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$plasma_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sampling_date",b1$plasma_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sampling_date",b1$plasma_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sampling_date",b1$plasma_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$plasma_sampling_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$plasma_sampling_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$plasma_sampling_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sampling_time",b1$plasma_sampling_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sampling_time",b1$plasma_sampling_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sampling_time",b1$plasma_sampling_time[i],"Incorrect time format\n",sep=","))}
    }
   if(b1$plasma_sample_code[i]!="" && !is.na(b1$plasma_sample_code[i])){
    d1<-unlist(strsplit(as.character(b1$plasma_sample_code[i]),""))[1]
    if(as.numeric(d1)!=5 && as.numeric(d1)!=8){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sample_code",b1$plasma_sample_code[i],"Incorrect format: does not begin with 5 or 8\n",sep=","))} else
    if(length(unlist(strsplit(as.character(b1$plasma_sample_code[i]),"")))!=4){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_sample_code",b1$plasma_sample_code[i],"Incorrect format: is not of length 4\n",sep=","))}
     }
    if(b1$plasma_storage_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$plasma_storage_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$plasma_storage_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$plasma_storage_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_storage_date",b1$plasma_storage_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_storage_date",b1$plasma_storage_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_storage_date",b1$plasma_storage_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$plasma_storage_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$plasma_storage_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$plasma_storage_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_storage_time",b1$plasma_storage_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_storage_time",b1$plasma_storage_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"plasma_storage_time",b1$plasma_storage_time[i],"Incorrect time format\n",sep=","))}
    }
    if(b1$paxgene_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$paxgene_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$paxgene_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$paxgene_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"paxgene_sampling_date",b1$paxgene_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"paxgene_sampling_date",b1$paxgene_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"paxgene_sampling_date",b1$paxgene_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
# note spelling mistake in variable name (following line)
    if(b1$paxgene_sampling_timr[i]!=""){
      hr<-unlist(strsplit(as.character(b1$paxgene_sampling_timr[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$paxgene_sampling_timr[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"paxgene_sampling_timr",b1$paxgene_sampling_timr[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"paxgene_sampling_timr",b1$paxgene_sampling_timr[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"paxgene_sampling_timr",b1$paxgene_sampling_timr[i],"Incorrect time format\n",sep=","))}
    }
    if(b1$genetics_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$genetics_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$genetics_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$genetics_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"genetics_sampling_date",b1$genetics_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"genetics_sampling_date",b1$genetics_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"genetics_sampling_date",b1$genetics_sampling_date[i],"Incorrect date format\n",sep=","))}
    }    # row 260

    if(b1$genetics_sampling_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$genetics_sampling_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$genetics_sampling_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"genetics_sampling_time",b1$genetics_sampling_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"genetics_sampling_time",b1$genetics_sampling_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"genetics_sampling_time",b1$genetics_sampling_time[i],"Incorrect time format\n",sep=","))}
    }
   if(!is.na(b1$genetics_sample_code[i])){
    if(length(unlist(strsplit(as.character(b1$genetics_sample_code[i]),"")))!=6){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"genetics_sample_code",b1$genetics_sample_code[i],"Incorrect format: not of length 6\n",sep=","))}
     }
    if(b1$urine_biomarkers_date[i]!=""){
    day<-unlist(strsplit(as.character(b1$urine_biomarkers_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b1$urine_biomarkers_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b1$urine_biomarkers_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_biomarkers_date",b1$urine_biomarkers_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_biomarkers_date",b1$urine_biomarkers_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_biomarkers_date",b1$urine_biomarkers_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b1$urine_biomarkers_time[i]!=""){
      hr<-unlist(strsplit(as.character(b1$urine_biomarkers_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b1$urine_biomarkers_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_biomarkers_time",b1$urine_biomarkers_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_biomarkers_time",b1$urine_biomarkers_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b1[i,1],b1[i,2],b1[i,3],"urine_biomarkers_time",b1$urine_biomarkers_time[i],"Incorrect time format\n",sep=","))}
    }

# up to row 268
  } # i

} # function



########################################
# vw interim baseline day 2 20121018.txt
B2<-function(fname){
b2<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(b2)){
   if(b2$patient_code[i]!=""){
    no<-unlist(strsplit(as.character(b2$patient_code[i]),"-"))[2]
    if(is.na(no)){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"patient_code",b2$patient_code[i],"Incorrect format: no hyphen\n",sep=","))}
     }
       # row 269
    if(b2$investigator[i]==""){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"investigator","","No investigator code\n",sep=","))}
    if(b2$cohort[i]==""){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cohort","","No cohort code\n",sep=","))}
    if(b2$STATUS[i]!="CLOSED"){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"STATUS",b2$STATUS[i],"Visit not closed\n",sep=","))}
    if(b2$visit_date[i]!=""){
    day<-unlist(strsplit(as.character(b2$visit_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b2$visit_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b2$visit_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"visit_date",b2$visit_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"visit_date",b2$visit_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"visit_date",b2$visit_date[i],"Incorrect date format\n",sep=","))}
    }
   if(b2$sputum_able[i]=="no" && b2$sputum_able_specify[i]==""){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_able_specify","","Sputum unavailable; no reason specified\n",sep=","))}
    if(b2$sputum_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(b2$sputum_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b2$sputum_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b2$sputum_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sampling_date",b2$sputum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sampling_date",b2$sputum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sampling_date",b2$sputum_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b2$sputum_sampling_time[i]!=""){
    hr<-unlist(strsplit(as.character(b2$sputum_sampling_time[i]),"\\."))[1]
    mi<-unlist(strsplit(as.character(b2$sputum_sampling_time[i]),"\\."))[2]
    if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sampling_time",b2$sputum_sampling_time[i],"Incorrect time format\n",sep=","))} else
    if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sampling_time",b2$sputum_sampling_time[i],"Incorrect time format\n",sep=","))}
    }
    if(b2$sputum_able[i]=="yes" && b2$sputum_processing_date[i]==""){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_processing_date","","Sputum available: processing date blank\n",sep=","))} else
    if(b2$sputum_processing_date[i]!=""){
    day<-unlist(strsplit(as.character(b2$sputum_processing_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b2$sputum_processing_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b2$sputum_processing_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_processing_date",b2$sputum_processing_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_processing_date",b2$sputum_processing_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_processing_date",b2$sputum_processing_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b2$sputum_able[i]=="yes" && b2$sputum_processing_time[i]==""){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_processing_time","","Sputum available: processing time blank\n",sep=","))} else
    if(b2$sputum_processing_time[i]!=""){
    hr<-unlist(strsplit(as.character(b2$sputum_processing_time[i]),"\\."))[1]
    mi<-unlist(strsplit(as.character(b2$sputum_processing_time[i]),"\\."))[2]
    if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_processing_time",b2$sputum_processing_time[i],"Incorrect time format\n",sep=","))} else
    if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_processing_time",b2$sputum_processing_time[i],"Incorrect time format\n",sep=","))}
    }
   if(b2$sputum_able[i]=="yes" && b2$sputum_consistency[i]==""){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_consistency","","Sputum available: no consistency specified\n",sep=","))}
   if(b2$sputum_able[i]=="yes" && b2$sputum_colour[i]==""){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_colour","","Sputum available: no colour specified\n",sep=","))}
    if(b2$sputum_sample_bacteriology[i]!=""){
    no<-unlist(strsplit(as.character(b2$sputum_sample_bacteriology[i]),"AB"))[2]
    no2<-unlist(strsplit(as.character(b2$sputum_sample_bacteriology[i]),"ab"))[2]
    if(is.na(no) && is.na(no2)){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sample_bacteriology",b2$sputum_sample_bacteriology[i],"Incorrect format: does not begin with AB or ab\n",sep=","))}
     }
   if(!is.na(b2$sputum_weight[i]) && b2$sputum_weight[i] > 2){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_weight",b2$sputum_weight[i],"Greater than 2: Check units\n",sep=","))}
   if(!is.na(b2$sputum_weight[i]) && is.na(b2$resuspension_volume)){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"resuspension_volume","","Sputum weight but no resuspension volume\n",sep=","))}
# JR SAYS TO QUERY CELL COUNT UNITS FOR TOTAL CELL COUNT - BUT NO CRITERIA
    if(!is.na(b2$leukocyte_count[i])){
      if(as.numeric(as.character(b2$leukocyte_count[i]))==0){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"leukocyte_count",as.numeric(as.character(b2$leukocyte_count[i])),"Equals zero\n",sep=","))}else
      if(as.numeric(as.character(b2$leukocyte_count[i]))>70){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"leukocyte_count",as.numeric(as.character(b2$leukocyte_count[i])),"Greater than 70\n",sep=","))}
    }
  if(b2$sputum_sample_viability[i]!=""){
    if(b2$sputum_sample_viability[i]=="na"){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sample_viability",b2$sputum_sample_viability[i],"Missing value\n",sep=","))} else
    if(b2$sputum_sample_viability[i]=="no"){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"sputum_sample_viability",b2$sputum_sample_viability[i],"Inform Biobank\n",sep=","))}   # row 290
  }
    if(b2$cytospin_processing_time[i]!=""){
    hr<-unlist(strsplit(as.character(b2$cytospin_processing_time[i]),"\\."))[1]
    mi<-unlist(strsplit(as.character(b2$cytospin_processing_time[i]),"\\."))[2]
    if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cytospin_processing_time",b2$cytospin_processing_time[i],"Incorrect time format\n",sep=","))} else
    if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cytospin_processing_time",b2$cytospin_processing_time[i],"Incorrect time format\n",sep=","))}
    }
    if(b2$supernatant_time[i]!=""){
      hr<-unlist(strsplit(as.character(b2$supernatant_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b2$supernatant_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"supernatant_time",b2$supernatant_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"supernatant_time",b2$supernatant_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"supernatant_time",b2$supernatant_time[i],"Incorrect time format\n",sep=","))}
    }
    if(!is.na(b2$supernatant_parent[i])){
    d1<-unlist(strsplit(as.character(b2$supernatant_parent[i]),""))[1]
    if(as.numeric(d1)!=5 && as.numeric(d1)!=8){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"supernatant_parent",b2$supernatant_parent[i],"Incorrect format: does not begin with 5 or 8\n",sep=","))} else
    if(length(unlist(strsplit(as.character(b2$supernatant_parent[i]),"")))!=4){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"supernatant_parent",b2$supernatant_parent[i],"Incorrect format: not of length 4\n",sep=","))}
     }
    if(b2$cell_pellet_time[i]!=""){
      hr<-unlist(strsplit(as.character(b2$cell_pellet_time[i]),"\\."))[1]
      mi<-unlist(strsplit(as.character(b2$cell_pellet_time[i]),"\\."))[2]
      if(is.na(as.numeric(hr))){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cell_pellet_time",b2$cell_pellet_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cell_pellet_time",b2$cell_pellet_time[i],"Incorrect time format\n",sep=","))} else
      if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cell_pellet_time",b2$cell_pellet_time[i],"Incorrect time format\n",sep=","))}
    }
   if(!is.na(b2$cell_pellet_parent[i])){
    d1<-unlist(strsplit(as.character(b2$cell_pellet_parent[i]),""))[1]
    if(as.numeric(d1)!=5 && as.numeric(d1)!=8){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cell_pellet_parent",b2$cell_pellet_parent[i],"Incorrect format: does not begin with 5 or 8\n",sep=","))} else
    if(length(unlist(strsplit(as.character(b2$cell_pellet_parent[i]),"")))!=4){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"cell_pellet_parent",b2$cell_pellet_parent[i],"Incorrect format: not of length 4\n",sep=","))}
     }
    if(b2$enose_testing_date[i]!=""){
    day<-unlist(strsplit(as.character(b2$enose_testing_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b2$enose_testing_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b2$enose_testing_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"enose_testing_date",b2$enose_testing_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"enose_testing_date",b2$enose_testing_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"enose_testing_date",b2$enose_testing_date[i],"Incorrect date format\n",sep=","))}
    }
    if(b2$enose_testing_time[i]!=""){
    hr<-unlist(strsplit(as.character(b2$enose_testing_time[i]),"\\."))[1]
    mi<-unlist(strsplit(as.character(b2$enose_testing_time[i]),"\\."))[2]
    if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"enose_testing_time",b2$enose_testing_time[i],"Incorrect time format\n",sep=","))} else
    if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"enose_testing_time",b2$enose_testing_time[i],"Incorrect time format\n",sep=","))}
    }
  # row 340

    if(b2$telemonitoring_informed_consent_date[i]!=""){
    day<-unlist(strsplit(as.character(b2$telemonitoring_informed_consent_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(b2$telemonitoring_informed_consent_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(b2$telemonitoring_informed_consent_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"telemonitoring_informed_consent_date",b2$telemonitoring_informed_consent_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"telemonitoring_informed_consent_date",b2$telemonitoring_informed_consent_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,b2[i,1],b2[i,2],b2[i,3],"telemonitoring_informed_consent_date",b2$telemonitoring_informed_consent_date[i],"Incorrect date format\n",sep=","))}
    }

# down to row 352
  } # i
} # function

########################################
# vw interim baseline hrtc 20121018.txt
BH<-function(fname){
bh<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(bh)){
   if(bh$patient_code[i]!=""){
    no<-unlist(strsplit(as.character(bh$patient_code[i]),"-"))[2]
    if(is.na(no)){IssueOut(paste(fname,i,bh[i,1],bh[i,2],bh[i,3],"patient_code",bh$patient_code[i],"Incorrect format: no hyphen\n",sep=","))}
     }
    if(bh$investigator[i]==""){IssueOut(paste(fname,i,bh[i,1],bh[i,2],bh[i,3],"investigator","","No investigator code\n",sep=","))}
    if(bh$cohort[i]==""){IssueOut(paste(fname,i,bh[i,1],bh[i,2],bh[i,3],"cohort","","No cohort code\n",sep=","))}
    if(bh$STATUS[i]!="CLOSED"){IssueOut(paste(fname,i,bh[i,1],bh[i,2],bh[i,3],"STATUS",bh$STATUS[i],"Visit not closed\n",sep=","))}
    if(bh$visit_date[i]!=""){
    day<-unlist(strsplit(as.character(bh$visit_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(bh$visit_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(bh$visit_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,bh[i,1],bh[i,2],bh[i,3],"visit_date",bh$visit_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,bh[i,1],bh[i,2],bh[i,3],"visit_date",bh$visit_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,bh[i,1],bh[i,2],bh[i,3],"visit_date",bh$visit_date[i],"Incorrect date format\n",sep=","))}
    }

 # to row 366
    } # i
} # function


########################################
# vw interim longitudinal hrtc 20121018.txt
LH<-function(fname){
lr<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(lr)){
   if(lr$patient_code[i]!=""){
    no<-unlist(strsplit(as.character(lr$patient_code[i]),"-"))[2]
    if(is.na(no)){IssueOut(paste(fname,i,lr[i,1],lr[i,2],lr[i,3],"patient_code",lr$patient_code[i],"Incorrect format: no hyphen\n",sep=","))}
     }    # row 367
    if(lr$investigator[i]==""){IssueOut(paste(fname,i,lr[i,1],lr[i,2],lr[i,3],"investigator","","No investigator code\n",sep=","))}
    if(lr$cohort[i]==""){IssueOut(paste(fname,i,lr[i,1],lr[i,2],lr[i,3],"cohort","","No cohort code\n",sep=","))}
    if(lr$STATUS[i]!="CLOSED"){IssueOut(paste(fname,i,lr[i,1],lr[i,2],lr[i,3],"STATUS",lr$STATUS[i],"Visit not closed\n",sep=","))}
    if(lr$visit_date[i]!=""){
    day<-unlist(strsplit(as.character(lr$visit_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(lr$visit_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(lr$visit_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,lr[i,1],lr[i,2],lr[i,3],"visit_date",lr$visit_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,lr[i,1],lr[i,2],lr[i,3],"visit_date",lr$visit_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,lr[i,1],lr[i,2],lr[i,3],"visit_date",lr$visit_date[i],"Incorrect date format\n",sep=","))}
    }
    } # i
} # function


##########################################
# vw interim md exacerbations 20121018.txt
ME<-function(fname){
me<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(me)){
    if(me$patient_code[i]==""){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"patient_code","","No patient code\n",sep=","))}    # row 381
    if(me$investigator[i]==""){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"investigator","","No investigator code\n",sep=","))}
    if(me$cohort[i]==""){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"cohort","","No cohort code\n",sep=","))}
    if(me$visit_name[i]=="" && me$location_emergency_room[i]==1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_emergency_room","1","Specified when visit_name is blank\n",sep=","))}
    if(me$visit_name[i]=="" && me$location_hospital_in_general_ward[i]==1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_in_general_ward","1","Specified when visit_name is blank\n",sep=","))}
    if(me$visit_name[i]=="" && me$location_hospital_in_icu[i]==1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_in_icu","1","Specified when visit_name is blank\n",sep=","))}
    if(me$visit_name[i]=="" && me$location_hospital_out_unscheduled[i]==1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_out_unscheduled","1","Specified when visit_name is blank\n",sep=","))}
    if(me$visit_name[i]=="" && me$location_hospital_out_scheduled[i]==1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_out_scheduled","1","Specified when visit_name is blank\n",sep=","))}
    if(me$visit_name[i]=="" && me$location_general_practitioner[i]==1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_general_practitioner","1","Specified when visit_name is blank\n",sep=","))}
    if(me$visit_name[i]=="" && me$location_other[i]==1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_other","1","Specified when visit_name is blank\n",sep=","))}

    if(me$location_emergency_room[i]!=0 && me$location_emergency_room[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_emergency_room",me$location_emergency_room[i],"Value neither zero nor one\n",sep=","))}
    if(me$location_hospital_in_general_ward[i]!=0 && me$location_hospital_in_general_ward[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_in_general_ward",me$location_hospital_in_general_ward[i],"Value neither zero nor one\n",sep=","))}
    if(me$location_hospital_in_icu[i]!=0 && me$location_hospital_in_icu[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_in_icu",me$location_hospital_in_icu[i],"Value neither zero nor one\n",sep=","))}
    if(me$location_hospital_out_unscheduled[i]!=0 && me$location_hospital_out_unscheduled[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_out_unscheduled",me$location_hospital_out_unscheduled[i],"Value neither zero nor one\n",sep=","))}
    if(me$location_hospital_out_scheduled[i]!=0 && me$location_hospital_out_scheduled[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_hospital_out_scheduled",me$location_hospital_out_scheduled[i],"Value neither zero nor one\n",sep=","))}
    if(me$location_general_practitioner[i]!=0 && me$location_general_practitioner[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_general_practitioner",me$location_general_practitioner[i],"Value neither zero nor one\n",sep=","))}
    if(me$location_other[i]!=0 && me$location_other[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"location_other",me$location_other[i],"Value neither zero nor one\n",sep=","))}   # row 391

    if(me$treatment_parenteral_glucocorticoids_lt_3[i]!=0 && me$treatment_parenteral_glucocorticoids_lt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_parenteral_glucocorticoids_lt_3",me$treatment_parenteral_glucocorticoids_lt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_parenteral_glucocorticoids_gt_3[i]!=0 && me$treatment_parenteral_glucocorticoids_gt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_parenteral_glucocorticoids_gt_3",me$treatment_parenteral_glucocorticoids_gt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_doubling_oral_glucocorticoids_lt_3[i]!=0 && me$treatment_doubling_oral_glucocorticoids_lt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_doubling_oral_glucocorticoids_lt_3",me$treatment_doubling_oral_glucocorticoids_lt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_doubling_oral_glucocorticoids_gt_3[i]!=0 && me$treatment_doubling_oral_glucocorticoids_gt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_doubling_oral_glucocorticoids_gt_3",me$treatment_doubling_oral_glucocorticoids_gt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_increase_oral_glucocorticoids_lt_3[i]!=0 && me$treatment_increase_oral_glucocorticoids_lt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_increase_oral_glucocorticoids_lt_3",me$treatment_increase_oral_glucocorticoids_lt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_increase_oral_glucocorticoids_gt_3[i]!=0 && me$treatment_increase_oral_glucocorticoids_gt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_increase_oral_glucocorticoids_gt_3",me$treatment_increase_oral_glucocorticoids_gt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_start_oral_glucocorticoids_lt_3[i]!=0 && me$treatment_start_oral_glucocorticoids_lt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_start_oral_glucocorticoids_lt_3",me$treatment_start_oral_glucocorticoids_lt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_start_oral_glucocorticoids_gt_3[i]!=0 && me$treatment_start_oral_glucocorticoids_gt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_start_oral_glucocorticoids_gt_3",me$treatment_start_oral_glucocorticoids_gt_3[i],"Value neither zero nor one\n",sep=","))}
# row 400

    if(me$treatment_increase_inhaled_glucocorticoids_lt_3[i]!=0 && me$treatment_increase_inhaled_glucocorticoids_lt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_increase_inhaled_glucocorticoids_lt_3",me$treatment_increase_inhaled_glucocorticoids_lt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_increase_inhaled_glucocorticoids_gt_3[i]!=0 && me$treatment_increase_inhaled_glucocorticoids_gt_3[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_increase_inhaled_glucocorticoids_gt_3",me$treatment_increase_inhaled_glucocorticoids_gt_3[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_start_inhaled_glucocorticoids[i]!=0 && me$treatment_start_inhaled_glucocorticoids[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_start_inhaled_glucocorticoids",me$treatment_start_inhaled_glucocorticoids[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_laba_addition[i]!=0 && me$treatment_laba_addition[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_laba_addition",me$treatment_laba_addition[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_short_acting_bronchodilators_addition[i]!=0 && me$treatment_short_acting_bronchodilators_addition[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_short_acting_bronchodilators_addition",me$treatment_short_acting_bronchodilators_addition[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_xanthyne_derivatives_addition[i]!=0 && me$treatment_xanthyne_derivatives_addition[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_xanthyne_derivatives_addition",me$treatment_xanthyne_derivatives_addition[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_leukotriene_antagonists_addition[i]!=0 && me$treatment_leukotriene_antagonists_addition[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_leukotriene_antagonists_addition",me$treatment_leukotriene_antagonists_addition[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_antibiotics_addition[i]!=0 && me$treatment_antibiotics_addition[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_antibiotics_addition",me$treatment_antibiotics_addition[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_intubation[i]!=0 && me$treatment_intubation[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_intubation",me$treatment_intubation[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_assisted_mechanical_ventilation[i]!=0 && me$treatment_assisted_mechanical_ventilation[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_assisted_mechanical_ventilation",me$treatment_assisted_mechanical_ventilation[i],"Value neither zero nor one\n",sep=","))}
    if(me$treatment_other[i]!=0 && me$treatment_other[i]!=1){IssueOut(paste(fname,i,me[i,1],me[i,2],me[i,3],"treatment_other",me$treatment_other[i],"Value neither zero nor one\n",sep=","))}
    # row 411


# to line 415
    }       # i
} # function


##########################################
# vw interim md Ige allergens 20121018.txt
IA<-function(fname){
ia<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(ia)){
   if(ia$patient_code[i]!=""){
    no<-unlist(strsplit(as.character(ia$patient_code[i]),"-"))[2]
    if(is.na(no)){IssueOut(paste(fname,i,ia[i,1],ia[i,2],ia[i,3],"patient_code",ia$patient_code[i],"Incorrect format: no hyphen\n",sep=","))}
     }
     } # i
} # function


##########################################
# vw interim md medications 20121018.txt
MM<-function(fname){
mm<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(mm)){
   if(mm$patient_code[i]!=""){
    no<-unlist(strsplit(as.character(mm$patient_code[i]),"-"))[2]
    if(is.na(no)){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"patient_code",mm$patient_code[i],"Incorrect format: no hyphen\n",sep=","))}
     }

    if(mm$asthma_medication[i]=="yes" && mm$medication[i]==""){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"asthma_medication",mm$asthma_medication[i],"Asthma medication:yes, but medication blank\n",sep=","))}
    if(!is.na(mm$start_day[i]) && mm$start_day[i]==0){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"start_day",mm$start_day[i],"Equals zero\n",sep=","))}  else
      if(!is.na(mm$start_day[i]) && mm$start_day[i]>31){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"start_day",mm$start_day[i],"Greater than 31\n",sep=","))}
    if(!is.na(mm$start_month[i])){
      if(mm$start_month[i]<1){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"start_month",mm$start_month[i],"Less than one\n",sep=","))}  else
      if(mm$start_month[i]>12){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"start_month",mm$start_month[i],"Greater than twelve\n",sep=","))}
    }
    if(!is.na(mm$start_year[i])){
    if(!is.na(mm$end_day[i]) && mm$end_day[i]==0){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"end_day",mm$end_day[i],"Equals zero\n",sep=","))} else
      if(!is.na(mm$end_day[i]) && mm$end_day[i]>31){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"end_day",mm$end_day[i],"Greater than 31\n",sep=","))}
    }
    if(!is.na(mm$end_month[i])){
      if(mm$end_month[i]<1){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"end_month",mm$end_month[i],"Less than one\n",sep=","))}  else
      if(mm$end_month[i]>12){IssueOut(paste(fname,i,mm[i,1],mm[i,2],mm[i,3],"end_month",mm$end_month[i],"Greater than twelve\n",sep=","))}
    }

# to row 439
    } # i
} # function


##########################################
# vw interim md sae 20121018.txt
AE<-function(fname){
ae<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(ae)){
    if(ae$onset_date_event[i]!=""){
    day<-unlist(strsplit(as.character(ae$onset_date_event[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(ae$onset_date_event[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(ae$onset_date_event[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,ae[i,1],ae[i,2],ae[i,3],"onset_date_event",ae$onset_date_event[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,ae[i,1],ae[i,2],ae[i,3],"onset_date_event",ae$onset_date_event[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,ae[i,1],ae[i,2],ae[i,3],"onset_date_event",ae$onset_date_event[i],"Incorrect date format\n",sep=","))}
    }
    if(ae$end_date_event[i]!=""){
    day<-unlist(strsplit(as.character(ae$end_date_event[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(ae$end_date_event[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(ae$end_date_event[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,ae[i,1],ae[i,2],ae[i,3],"end_date_event",ae$end_date_event[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,ae[i,1],ae[i,2],ae[i,3],"end_date_event",ae$end_date_event[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,ae[i,1],ae[i,2],ae[i,3],"end_date_event",ae$end_date_event[i],"Incorrect date format\n",sep=","))}
    }
  } # i
} # function


##########################################
# vw interim md samples 20121018.txt
MS<-function(fname){
ms<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(ms)){
    if(ms$investigator[i]==""){IssueOut(paste(fname,i,ms[i,1],ms[i,2],ms[i,3],"investigator","","No investigator code\n",sep=","))}  # row 459
   if(!is.na(ms$sample_code[i])){
# Query modified according to email from JR on 12th April
      no<-unlist(strsplit(as.character(ms$sample_code[i]),"AB"))[2]
      no2<-unlist(strsplit(as.character(ms$sample_code[i]),"ab"))[2]
      len<-unlist(strsplit(as.character(ms$sample_code[i]),""))
      if(is.na(no) && is.na(no2) && length(len)!=6){IssueOut(paste(fname,i,ms[i,1],ms[i,2],ms[i,3],"sample_code",ms$sample_code[i],"Incorrect format: does not start with AB or ab; not 6 digits\n",sep=","))}
     }
# to row  463
  } # i
} # function


##########################################
# vw interim md skin prick 20121018.txt
SP<-function(fname){
sp<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(sp)){
   if(sp$patient_code[i]!=""){
    no<-unlist(strsplit(as.character(sp$patient_code[i]),"-"))[2]
    if(is.na(no)){IssueOut(paste(fname,i,sp[i,1],sp[i,2],sp[i,3],"patient_code",sp$patient_code[i],"Incorrect format: no hyphen\n",sep=","))}
     }
  } # i
} # function


##########################################
# vw interim repeatability 20121018.txt
IR<-function(fname){
ir<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(ir)){
    if(ir$patient_code[i]==""){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"patient_code","","No patient code\n",sep=","))}    # row 472
    if(ir$investigator[i]==""){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"investigator","","No investigator code\n",sep=","))}
    if(ir$cohort[i]==""){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"cohort","","No cohort code\n",sep=","))}
    if(!is.na(ir$systolic_blood_pressure[i])){
      if(as.numeric(as.character(ir$systolic_blood_pressure[i]))>200){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"systolic_blood_pressure",as.numeric(as.character(ir$systolic_blood_pressure[i])),"Greater than 200\n",sep=","))}else
      if(as.numeric(as.character(ir$systolic_blood_pressure[i]))<80){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"systolic_blood_pressure",as.numeric(as.character(ir$systolic_blood_pressure[i])),"Less than 80\n",sep=","))}
    }
    if(!is.na(ir$diastolic_blood_pressure[i])){
      if(as.numeric(as.character(ir$diastolic_blood_pressure[i]))>120){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"diastolic_blood_pressure",as.numeric(as.character(ir$diastolic_blood_pressure[i])),"Greater than 120\n",sep=","))}else
      if(as.numeric(as.character(ir$diastolic_blood_pressure[i]))<40){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"diastolic_blood_pressure",as.numeric(as.character(ir$diastolic_blood_pressure[i])),"Less than 40\n",sep=","))}
    }
    if(!is.na(ir$heart_rate[i])){
      if(as.numeric(as.character(ir$heart_rate[i]))>120){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"heart_rate",as.numeric(as.character(ir$heart_rate[i])),"Greater than 120\n",sep=","))} else
      if(as.numeric(as.character(ir$heart_rate[i]))<30){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"heart_rate",as.numeric(as.character(ir$heart_rate[i])),"Less than 30\n",sep=","))}
    }
    if(!is.na(ir$respiratory_rate[i])){
      if(as.numeric(as.character(ir$respiratory_rate[i]))>36){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"respiratory_rate",as.numeric(as.character(ir$respiratory_rate[i])),"Greater than 36\n",sep=","))}else
      if(as.numeric(as.character(ir$respiratory_rate[i]))<6){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"respiratory_rate",as.numeric(as.character(ir$respiratory_rate[i])),"Less than 6\n",sep=","))}
    }
    if(ir$valid_spirometry_date[i]!=""){
    day<-unlist(strsplit(as.character(ir$valid_spirometry_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(ir$valid_spirometry_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(ir$valid_spirometry_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"valid_spirometry_date",ir$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"valid_spirometry_date",ir$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"valid_spirometry_date",ir$valid_spirometry_date[i],"Incorrect date format\n",sep=","))}
    }
    if(!is.na(ir$fev1_actual[i])){
      if(as.numeric(as.character(ir$fev1_actual[i]))>6.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_actual",as.numeric(as.character(ir$fev1_actual[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(ir$fev1_actual[i]))<0.2){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_actual",as.numeric(as.character(ir$fev1_actual[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(ir$fev1_predicted[i])){
      if(as.numeric(as.character(ir$fev1_predicted[i]))>6.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_predicted",as.numeric(as.character(ir$fev1_predicted[i])),"Greater than 6.5\n",sep=","))} else
      if(as.numeric(as.character(ir$fev1_predicted[i]))<0.3){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_predicted",as.numeric(as.character(ir$fev1_predicted[i])),"Less than 0.3\n",sep=","))}
    }
    if(!is.na(ir$fev1_percentage[i])){
      if(as.numeric(as.character(ir$fev1_percentage[i]))>130){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_percentage",as.numeric(as.character(ir$fev1_percentage[i])),"Greater than 130%: Check race\n",sep=","))} else
      if(as.numeric(as.character(ir$fev1_percentage[i]))<10){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_percentage",as.numeric(as.character(ir$fev1_percentage[i])),"Less than 10%\n",sep=","))}
    }
    if(!is.na(ir$fvc_actual[i])){
      if(as.numeric(as.character(ir$fvc_actual[i]))>7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_actual",as.numeric(as.character(ir$fvc_actual[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(ir$fvc_actual[i]))<0.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_actual",as.numeric(as.character(ir$fvc_actual[i])),"Less than 0.5\n",sep=","))} # row 510
    }
    if(!is.na(ir$fvc_predicted[i])){
      if(as.numeric(as.character(ir$fvc_predicted[i]))>7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_predicted",as.numeric(as.character(ir$fvc_predicted[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(ir$fvc_predicted[i]))<1){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_predicted",as.numeric(as.character(ir$fvc_predicted[i])),"Less than 1\n",sep=","))}
    }
    if(!is.na(ir$fvc_percentage[i])){
      if(as.numeric(as.character(ir$fvc_percentage[i]))>140){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_percentage",as.numeric(as.character(ir$fvc_percentage[i])),"Greater than 140: Check race\n",sep=","))}else
      if(as.numeric(as.character(ir$fvc_percentage[i]))<30){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_percentage",as.numeric(as.character(ir$fvc_percentage[i])),"Less than 30\n",sep=","))}
    }
    if(!is.na(ir$fef2575_actual[i])){
      if(as.numeric(as.character(ir$fef2575_actual[i]))>6){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_actual",as.numeric(as.character(ir$fef2575_actual[i])),"Greater than 6: Check units\n",sep=","))}else
      if(as.numeric(as.character(ir$fef2575_actual[i]))<0.1){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_actual",as.numeric(as.character(ir$fef2575_actual[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(ir$fef2575_predicted[i])){
      if(as.numeric(as.character(ir$fef2575_predicted[i]))>7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_predicted",as.numeric(as.character(ir$fef2575_predicted[i])),"Greater than 7: Check units\n",sep=","))} else
      if(as.numeric(as.character(ir$fef2575_predicted[i]))<1.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_predicted",as.numeric(as.character(ir$fef2575_predicted[i])),"Less than 1.5: Check units\n",sep=","))}
    }
    if(!is.na(ir$fef2575_percentage[i])){
      if(as.numeric(as.character(ir$fef2575_percentage[i]))>140){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_percentage",as.numeric(as.character(ir$fef2575_percentage[i])),"Greater than 140%\n",sep=","))} else
      if(as.numeric(as.character(ir$fef2575_percentage[i]))<5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_percentage",as.numeric(as.character(ir$fef2575_percentage[i])),"Less than 5%\n",sep=","))}  # row 45
    }
    if(!is.na(ir$pef_actual[i])){
    if(ir$pef_actual_um[i]==""){
    IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_actual",as.numeric(as.character(ir$pef_actual[i])),"Units not specified\n",sep=","))
    } else
    if(ir$pef_actual_um[i]=="l_min"){
      if(as.numeric(as.character(ir$pef_actual[i]))/60>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_actual",as.numeric(as.character(ir$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_actual[i]))/60<0.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_actual",as.numeric(as.character(ir$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }else
    if(ir$pef_actual_um[i]=="l_sec"){
      if(as.numeric(as.character(ir$pef_actual[i]))>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_actual",as.numeric(as.character(ir$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_actual[i]))<0.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_actual",as.numeric(as.character(ir$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }
    }
    if(!is.na(ir$pef_predicted[i])){
    if(ir$pef_predicted_um[i]==""){
    IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_predicted",as.numeric(as.character(ir$pef_predicted[i])),"Units not specified\n",sep=","))
    } else
    if(ir$pef_predicted_um[i]=="l_min"){
        if(as.numeric(as.character(ir$pef_predicted[i]))/60>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_predicted",as.numeric(as.character(ir$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(ir$pef_predicted[i]))/60<0.7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_predicted",as.numeric(as.character(ir$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }else
    if(ir$pef_predicted_um[i]=="l_sec"){
        if(as.numeric(as.character(ir$pef_predicted[i]))>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_predicted",as.numeric(as.character(ir$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(ir$pef_predicted[i]))<0.7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_predicted",as.numeric(as.character(ir$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }
    }
    if(!is.na(ir$pef_percentage[i])){
      if(as.numeric(as.character(ir$pef_percentage[i]))>150){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_percentage",as.numeric(as.character(ir$pef_percentage[i])),"Greater than 150\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_percentage[i]))<10){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_percentage",as.numeric(as.character(ir$pef_percentage[i])),"Less than 10\n",sep=","))}      # row 520
    }
    
    if(ir$reversibility_test_date[i]!=""){
    day<-unlist(strsplit(as.character(ir$reversibility_test_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(ir$reversibility_test_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(ir$reversibility_test_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"reversibility_test_date",ir$reversibility_test_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"reversibility_test_date",ir$reversibility_test_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"reversibility_test_date",ir$reversibility_test_date[i],"Incorrect date format\n",sep=","))}
    }
    if(!is.na(ir$fev1_pre_salbutamol[i])){
      if(as.numeric(as.character(ir$fev1_pre_salbutamol[i]))>6.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_pre_salbutamol",as.numeric(as.character(ir$fev1_pre_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(ir$fev1_pre_salbutamol[i]))<0.2){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_pre_salbutamol",as.numeric(as.character(ir$fev1_pre_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(ir$fev1_post_salbutamol[i])){
      if(as.numeric(as.character(ir$fev1_post_salbutamol[i]))>6.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_post_salbutamol",as.numeric(as.character(ir$fev1_post_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(ir$fev1_post_salbutamol[i]))<0.2){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_post_salbutamol",as.numeric(as.character(ir$fev1_post_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(ir$fev1_change[i])){
      if(as.numeric(as.character(ir$fev1_change[i]))>75){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_change",as.numeric(as.character(ir$fev1_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(ir$fev1_change[i]))+10<0){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fev1_change",as.numeric(as.character(ir$fev1_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(ir$fvc_pre_salbutamol[i])){
      if(as.numeric(as.character(ir$fvc_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_pre_salbutamol",as.numeric(as.character(ir$fvc_pre_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(ir$fvc_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_pre_salbutamol",as.numeric(as.character(ir$fvc_pre_salbutamol[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(ir$fvc_post_salbutamol[i])){
      if(as.numeric(as.character(ir$fvc_post_salbutamol[i]))>7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_post_salbutamol",as.numeric(as.character(ir$fvc_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(ir$fvc_post_salbutamol[i]))<0.6){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_post_salbutamol",as.numeric(as.character(ir$fvc_post_salbutamol[i])),"Less than 0.6: Check units\n",sep=","))}
    }
    if(!is.na(ir$fvc_change[i])){
      if(as.numeric(as.character(ir$fvc_change[i]))>75){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_change",as.numeric(as.character(ir$fvc_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(ir$fvc_change[i]))+10<0){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fvc_change",as.numeric(as.character(ir$fvc_change[i])),"Less than -10\n",sep=","))}    # row  529
   }
   
    if(!is.na(ir$fef2575_pre_salbutamol[i])){
      if(as.numeric(as.character(ir$fef2575_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(ir$fef2575_pre_salbutamol[i])),"Greater than 7: Check units\n",sep=","))}else
      if(as.numeric(as.character(ir$fef2575_pre_salbutamol[i]))<0.1){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(ir$fef2575_pre_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(ir$fef2575_post_salbutamol[i])){
      if(as.numeric(as.character(ir$fef2575_post_salbutamol[i]))>7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_post_salbutamol",as.numeric(as.character(ir$fef2575_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(ir$fef2575_post_salbutamol[i]))<0.1){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_post_salbutamol",as.numeric(as.character(ir$fef2575_post_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(ir$fef2575_change[i])){
      if(as.numeric(as.character(ir$fef2575_change[i]))>100){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_change",as.numeric(as.character(ir$fef2575_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(ir$fef2575_change[i]))+10<0){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"fef2575_change",as.numeric(as.character(ir$fef2575_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(ir$pef_pre_salbutamol[i])){
    if(ir$pef_pre_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_pre_salbutamol",as.numeric(as.character(ir$pef_pre_salbutamol[i])),"Units not specified\n",sep=","))
    } else
    if(ir$pef_pre_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(ir$pef_pre_salbutamol[i]))/60>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_pre_salbutamol",as.numeric(as.character(ir$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_pre_salbutamol[i]))/60<0.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_pre_salbutamol",as.numeric(as.character(ir$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }else
    if(ir$pef_pre_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(ir$pef_pre_salbutamol[i]))>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_pre_salbutamol",as.numeric(as.character(ir$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_pre_salbutamol",as.numeric(as.character(ir$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }
    }
    if(!is.na(ir$pef_post_salbutamol[i])){
    if(ir$pef_post_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_post_salbutamol",as.numeric(as.character(ir$pef_post_salbutamol[i])),"Units not specified\n",sep=","))
    } else
   if(ir$pef_post_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(ir$pef_post_salbutamol[i]))/60>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_post_salbutamol",as.numeric(as.character(ir$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_post_salbutamol[i]))/60<0.7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_post_salbutamol",as.numeric(as.character(ir$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    } else
   if(ir$pef_post_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(ir$pef_post_salbutamol[i]))>14){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_post_salbutamol",as.numeric(as.character(ir$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_post_salbutamol[i]))<0.7){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_post_salbutamol",as.numeric(as.character(ir$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    }
    }
    if(!is.na(ir$pef_change[i])){
      if(as.numeric(as.character(ir$pef_change[i]))>100){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_change",as.numeric(as.character(ir$pef_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(ir$pef_change[i]))+10 <0){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"pef_change",as.numeric(as.character(ir$pef_change[i])),"Less than -10\n",sep=","))}
    }
    if(ir$plethysmography_date[i]!=""){
    day<-unlist(strsplit(as.character(ir$plethysmography_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(ir$plethysmography_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(ir$plethysmography_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_date",ir$plethysmography_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_date",ir$plethysmography_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_date",ir$plethysmography_date[i],"Incorrect date format\n",sep=","))}
    }
    # row 540

    if(!is.na(ir$plethysmography_tlc[i])){
      if(as.numeric(as.character(ir$plethysmography_tlc[i]))>12){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_tlc",as.numeric(as.character(ir$plethysmography_tlc[i])),"Greater than 12\n",sep=","))}else
      if(as.numeric(as.character(ir$plethysmography_tlc[i]))<3){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_tlc",as.numeric(as.character(ir$plethysmography_tlc[i])),"Less than 3\n",sep=","))}
    }
    if(!is.na(ir$plethysmography_rv[i])){
      if(as.numeric(as.character(ir$plethysmography_rv[i]))>8){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_rv",as.numeric(as.character(ir$plethysmography_rv[i])),"Greater than 8\n",sep=","))}else
      if(as.numeric(as.character(ir$plethysmography_rv[i]))<0.5){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_rv",as.numeric(as.character(ir$plethysmography_rv[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(ir$plethysmography_sgaw[i])){
      if(as.numeric(as.character(ir$plethysmography_sgaw[i]))>6){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_sgaw",as.numeric(as.character(ir$plethysmography_sgaw[i])),"Greater than 6\n",sep=","))}else
      if(as.numeric(as.character(ir$plethysmography_sgaw[i]))==0){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"plethysmography_sgaw",as.numeric(as.character(ir$plethysmography_sgaw[i])),"Equals zero\n",sep=","))}
    }

# haematology and biochemistry starting row 563
# See separate function below

   if(ir$sputum_able[i]=="no" && ir$sputum_able_specify[i]==""){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_able_specify","","Sputum unavailable; no reason specified\n",sep=","))}
    if(ir$sputum_sampling_date[i]!=""){
    day<-unlist(strsplit(as.character(ir$sputum_sampling_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(ir$sputum_sampling_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(ir$sputum_sampling_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_sampling_date",ir$sputum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_sampling_date",ir$sputum_sampling_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_sampling_date",ir$sputum_sampling_date[i],"Incorrect date format\n",sep=","))}
    }
    if(ir$sputum_sampling_time[i]!=""){
    hr<-unlist(strsplit(as.character(ir$sputum_sampling_time[i]),"\\."))[1]
    mi<-unlist(strsplit(as.character(ir$sputum_sampling_time[i]),"\\."))[2]
    if(as.numeric(hr)<0 || as.numeric(hr)>24){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_sampling_time",ir$sputum_sampling_time[i],"Incorrect time format\n",sep=","))} else
    if(as.numeric(mi)<0 || as.numeric(mi)>60){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_sampling_time",ir$sputum_sampling_time[i],"Incorrect time format\n",sep=","))}
    }
   if(ir$sputum_able[i]=="yes" && ir$sputum_processing_time[i]==""){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_processing_time","","Sputum available: no processing time specified\n",sep=","))}
   # row 590

   if(ir$sputum_able[i]=="yes" && ir$sputum_consistency[i]==""){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_consistency","","Sputum available: no consistency specified\n",sep=","))}
   if(ir$sputum_able[i]=="yes" && ir$sputum_colour[i]==""){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_colour","","Sputum available: no colour specified\n",sep=","))}
   if(!is.na(ir$sputum_weight[i]) && ir$sputum_weight[i] > 2){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_weight",ir$sputum_weight[i],"Greater than 2: Check units\n",sep=","))}
   if(!is.na(ir$sputum_weight[i]) && is.na(ir$resuspension_volume[i])){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"resuspension_volume","","Sputum weight but no resuspension volume\n",sep=","))}
    if(!is.na(ir$leukocyte_count[i])){
      if(as.numeric(as.character(ir$leukocyte_count[i]))==0){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"leukocyte_count",as.numeric(as.character(ir$leukocyte_count[i])),"Equals zero\n",sep=","))}else
      if(as.numeric(as.character(ir$leukocyte_count[i]))>70){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"leukocyte_count",as.numeric(as.character(ir$leukocyte_count[i])),"Greater than 70\n",sep=","))}
    }
  if(ir$sputum_sample_viability[i]!=""){
    if(ir$sputum_sample_viability[i]=="na"){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_sample_viability",ir$sputum_sample_viability[i],"Missing value\n",sep=","))} else
    if(ir$sputum_sample_viability[i]=="no"){IssueOut(paste(fname,i,ir[i,1],ir[i,2],ir[i,3],"sputum_sample_viability",ir$sputum_sample_viability[i],"Inform Biobank\n",sep=","))}
  }

    # up to row 674

    } # i
} # function



#####################################
# vw interim screening 20121018.txt
SC<-function(fname){
sc<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(sc)){
    if(sc$investigator[i]==""){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"investigator","","No investigator code\n",sep=","))}
    if(sc$cohort[i]==""){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"cohort","","No cohort code\n",sep=","))}
    if(sc$informed_consent[i]!=""){
      if(sc$informed_consent[i]=="no"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"informed_consent",sc$informed_consent[i],"Not given\n",sep=","))}
    }
    if(sc$informed_consent_genetic[i]!=""){
      if(sc$informed_consent_genetic[i]=="no"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"informed_consent_genetic",sc$informed_consent_genetic[i],"Consent not given: Inform Biobank\n",sep=","))}
    }
      if(sc$consent_samples[i]=="no"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"consent_samples",sc$consent_samples[i],"Consent not given: Inform Biobank\n",sep=","))}
    if(!is.na(sc$onset_breathing_problems_age[i])){
      if(sc$onset_breathing_problems_age[i]==0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"onset_breathing_problems_age",sc$onset_breathing_problems_age[i],"Equals zero\n",sep=","))}
      }

    if(!is.na(sc$onset_first_diagnosis_age[i])){
      if(sc$onset_first_diagnosis_age[i]==0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"onset_first_diagnosis_age",sc$onset_first_diagnosis_age[i],"Equals zero\n",sep=","))}
    }
      if(sc$hypertension_age_of_onset[i]=="less_than_2"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"hypertension_age_of_onset",sc$hypertension_age_of_onset[i],"Less than 2\n",sep=","))}else
      if(sc$hypertension_age_of_onset[i]=="2to17"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"hypertension_age_of_onset",sc$hypertension_age_of_onset[i],"Between 2 and 17\n",sep=","))}
      if(sc$coronary_age_of_onset[i]=="less_than_2"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"coronary_age_of_onset",sc$coronary_age_of_onset[i],"Less than 2\n",sep=","))}#else
      if(sc$congestive_age_of_onset[i]=="less_than_2"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"congestive_age_of_onset",sc$congestive_age_of_onset[i],"Less than 2\n",sep=","))}else
      if(sc$congestive_age_of_onset[i]=="2to17"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"congestive_age_of_onset",sc$congestive_age_of_onset[i],"Between 2 and 17\n",sep=","))}
      if(sc$osteoporosis_age_of_onset[i]=="less_than_2"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"osteoporosis_age_of_onset",sc$osteoporosis_age_of_onset[i],"Less than 2\n",sep=","))}else
      if(sc$osteoporosis_age_of_onset[i]=="2to17"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"osteoporosis_age_of_onset",sc$osteoporosis_age_of_onset[i],"Between 2 and 17\n",sep=","))}
      if(sc$diabetes_age_of_onset[i]=="less_than_2"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"diabetes_age_of_onset",sc$diabetes_age_of_onset[i],"Less than 2\n",sep=","))}else
      if(sc$diabetes_age_of_onset[i]=="2to17"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"diabetes_age_of_onset",sc$diabetes_age_of_onset[i],"Between 2 and 17\n",sep=","))}

      if(sc$injectable_corticosteroids[i]=="more_than_once_a_month"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"injectable_corticosteroids",sc$injectable_corticosteroids[i],"More than once a month\n",sep=","))}
    if(!is.na(sc$abdominal_girth[i])){
      if(sc$abdominal_girth[i]==0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"abdominal_girth",sc$abdominal_girth[i],"Equals zero\n",sep=","))}
    }
    if(!is.na(sc$systolic_blood_pressure[i])){
      if(as.numeric(as.character(sc$systolic_blood_pressure[i]))>200){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"systolic_blood_pressure",as.numeric(as.character(sc$systolic_blood_pressure[i])),"Greater than 200\n",sep=","))}else
      if(as.numeric(as.character(sc$systolic_blood_pressure[i]))<80){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"systolic_blood_pressure",as.numeric(as.character(sc$systolic_blood_pressure[i])),"Less than 80\n",sep=","))}
    }
    if(!is.na(sc$diastolic_blood_pressure[i])){
      if(as.numeric(as.character(sc$diastolic_blood_pressure[i]))>120){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"diastolic_blood_pressure",as.numeric(as.character(sc$diastolic_blood_pressure[i])),"Greater than 120\n",sep=","))}else
      if(as.numeric(as.character(sc$diastolic_blood_pressure[i]))<40){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"diastolic_blood_pressure",as.numeric(as.character(sc$diastolic_blood_pressure[i])),"Less than 40\n",sep=","))}
    }
    if(!is.na(sc$heart_rate[i])){
      if(as.numeric(as.character(sc$heart_rate[i]))>120){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"heart_rate",as.numeric(as.character(sc$heart_rate[i])),"Greater than 120\n",sep=","))} else
      if(as.numeric(as.character(sc$heart_rate[i]))<30){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"heart_rate",as.numeric(as.character(sc$heart_rate[i])),"Less than 30\n",sep=","))}
    }
    if(!is.na(sc$respiratory_rate[i])){
      if(as.numeric(as.character(sc$respiratory_rate[i]))>36){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"respiratory_rate",as.numeric(as.character(sc$respiratory_rate[i])),"Greater than 36\n",sep=","))}else
      if(as.numeric(as.character(sc$respiratory_rate[i]))<6){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"respiratory_rate",as.numeric(as.character(sc$respiratory_rate[i])),"Less than 6\n",sep=","))}  # row 1020
    }
    if(sc$valid_spirometry_date[i]!=""){
    day<-unlist(strsplit(as.character(sc$valid_spirometry_date[i]),"/"))[1]
    mo<-unlist(strsplit(as.character(sc$valid_spirometry_date[i]),"/"))[2]
    yr<-unlist(strsplit(as.character(sc$valid_spirometry_date[i]),"/"))[3]
    if(as.numeric(day)<1 || as.numeric(day)>31 ){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"valid_spirometry_date",sc$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(mo)<1 || as.numeric(mo)>12 ){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"valid_spirometry_date",sc$valid_spirometry_date[i],"Incorrect date format\n",sep=","))} else
      if(as.numeric(yr)<2009 || as.numeric(yr)>2014 ){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"valid_spirometry_date",sc$valid_spirometry_date[i],"Incorrect date format\n",sep=","))}
    }
    if(!is.na(sc$fev1_actual[i])){
      if(as.numeric(as.character(sc$fev1_actual[i]))>6.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_actual",as.numeric(as.character(sc$fev1_actual[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(sc$fev1_actual[i]))<0.2){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_actual",as.numeric(as.character(sc$fev1_actual[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(sc$fev1_predicted[i])){
      if(as.numeric(as.character(sc$fev1_predicted[i]))>6.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_predicted",as.numeric(as.character(sc$fev1_predicted[i])),"Greater than 6.5\n",sep=","))} else
      if(as.numeric(as.character(sc$fev1_predicted[i]))<0.3){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_predicted",as.numeric(as.character(sc$fev1_predicted[i])),"Less than 0.3\n",sep=","))}
    }
    if(!is.na(sc$fev1_percentage[i])){
      if(as.numeric(as.character(sc$fev1_percentage[i]))>130){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_percentage",as.numeric(as.character(sc$fev1_percentage[i])),"Greater than 130%: Check race\n",sep=","))} else
      if(as.numeric(as.character(sc$fev1_percentage[i]))<10){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_percentage",as.numeric(as.character(sc$fev1_percentage[i])),"Less than 10%\n",sep=","))}
    }
    if(!is.na(sc$fvc_actual[i])){
      if(as.numeric(as.character(sc$fvc_actual[i]))>7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_actual",as.numeric(as.character(sc$fvc_actual[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(sc$fvc_actual[i]))<0.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_actual",as.numeric(as.character(sc$fvc_actual[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(sc$fvc_predicted[i])){
      if(as.numeric(as.character(sc$fvc_predicted[i]))>7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_predicted",as.numeric(as.character(sc$fvc_predicted[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(sc$fvc_predicted[i]))<1){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_predicted",as.numeric(as.character(sc$fvc_predicted[i])),"Less than 1\n",sep=","))}
    }
    if(!is.na(sc$fvc_percentage[i])){
      if(as.numeric(as.character(sc$fvc_percentage[i]))>140){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_percentage",as.numeric(as.character(sc$fvc_percentage[i])),"Greater than 140: Check race\n",sep=","))}else
      if(as.numeric(as.character(sc$fvc_percentage[i]))<30){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_percentage",as.numeric(as.character(sc$fvc_percentage[i])),"Less than 30\n",sep=","))}   # row 1030
    }
    
    if(!is.na(sc$fef2575_actual[i])){
      if(as.numeric(as.character(sc$fef2575_actual[i]))>6){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_actual",as.numeric(as.character(sc$fef2575_actual[i])),"Greater than 6: Check units\n",sep=","))}else
      if(as.numeric(as.character(sc$fef2575_actual[i]))<0.1){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_actual",as.numeric(as.character(sc$fef2575_actual[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(sc$fef2575_predicted[i])){
      if(as.numeric(as.character(sc$fef2575_predicted[i]))>7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_predicted",as.numeric(as.character(sc$fef2575_predicted[i])),"Greater than 7: Check units\n",sep=","))} else
      if(as.numeric(as.character(sc$fef2575_predicted[i]))<1.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_predicted",as.numeric(as.character(sc$fef2575_predicted[i])),"Less than 1.5: Check units\n",sep=","))}
    }
    if(!is.na(sc$fef2575_percentage[i])){
      if(as.numeric(as.character(sc$fef2575_percentage[i]))>140){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_percentage",as.numeric(as.character(sc$fef2575_percentage[i])),"Greater than 140%\n",sep=","))} else
      if(as.numeric(as.character(sc$fef2575_percentage[i]))<5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_percentage",as.numeric(as.character(sc$fef2575_percentage[i])),"Less than 5%\n",sep=","))}
    }
    if(!is.na(sc$pef_actual[i])){
    if(sc$pef_actual_um[i]==""){
    IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_actual",as.numeric(as.character(sc$pef_actual[i])),"Units not specified\n",sep=","))
    } else
    if(sc$pef_actual_um[i]=="l_min"){
      if(as.numeric(as.character(sc$pef_actual[i]))/60>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_actual",as.numeric(as.character(sc$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_actual[i]))/60<0.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_actual",as.numeric(as.character(sc$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }else
    if(sc$pef_actual_um[i]=="l_sec"){
      if(as.numeric(as.character(sc$pef_actual[i]))>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_actual",as.numeric(as.character(sc$pef_actual[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_actual[i]))<0.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_actual",as.numeric(as.character(sc$pef_actual[i])),"Less than 0.5 lsec\n",sep=","))}
    }
    }
    if(!is.na(sc$pef_predicted[i])){
    if(sc$pef_predicted_um[i]==""){
    IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_predicted",as.numeric(as.character(sc$pef_predicted[i])),"Units not specified\n",sep=","))
    } else
    if(sc$pef_predicted_um[i]=="l_min"){
        if(as.numeric(as.character(sc$pef_predicted[i]))/60>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_predicted",as.numeric(as.character(sc$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(sc$pef_predicted[i]))/60<0.7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_predicted",as.numeric(as.character(sc$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }else
    if(sc$pef_predicted_um[i]=="l_sec"){
        if(as.numeric(as.character(sc$pef_predicted[i]))>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_predicted",as.numeric(as.character(sc$pef_predicted[i])),"Greater than 14 lsec\n",sep=","))}else
        if(as.numeric(as.character(sc$pef_predicted[i]))<0.7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_predicted",as.numeric(as.character(sc$pef_predicted[i])),"Less than 0.7 lsec\n",sep=","))}
    }
    }
    if(!is.na(sc$pef_percentage[i])){
      if(as.numeric(as.character(sc$pef_percentage[i]))>150){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_percentage",as.numeric(as.character(sc$pef_percentage[i])),"Greater than 150\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_percentage[i]))<10){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_percentage",as.numeric(as.character(sc$pef_percentage[i])),"Less than 10\n",sep=","))}
    }
    if(!is.na(sc$fev1_pre_salbutamol[i])){
      if(as.numeric(as.character(sc$fev1_pre_salbutamol[i]))>6.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_pre_salbutamol",as.numeric(as.character(sc$fev1_pre_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(sc$fev1_pre_salbutamol[i]))<0.2){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_pre_salbutamol",as.numeric(as.character(sc$fev1_pre_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
    if(!is.na(sc$fev1_post_salbutamol[i])){
      if(as.numeric(as.character(sc$fev1_post_salbutamol[i]))>6.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_post_salbutamol",as.numeric(as.character(sc$fev1_post_salbutamol[i])),"Greater than 6.5\n",sep=","))}else
      if(as.numeric(as.character(sc$fev1_post_salbutamol[i]))<0.2){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_post_salbutamol",as.numeric(as.character(sc$fev1_post_salbutamol[i])),"Less than 0.2\n",sep=","))}
    }
# row 1050

    if(!is.na(sc$fev1_change[i])){
      if(as.numeric(as.character(sc$fev1_change[i]))>75){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_change",as.numeric(as.character(sc$fev1_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(sc$fev1_change[i]))+10<0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fev1_change",as.numeric(as.character(sc$fev1_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(sc$fvc_pre_salbutamol[i])){
      if(as.numeric(as.character(sc$fvc_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_pre_salbutamol",as.numeric(as.character(sc$fvc_pre_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(sc$fvc_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_pre_salbutamol",as.numeric(as.character(sc$fvc_pre_salbutamol[i])),"Less than 0.5\n",sep=","))}
    }
    if(!is.na(sc$fvc_post_salbutamol[i])){
      if(as.numeric(as.character(sc$fvc_post_salbutamol[i]))>7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_post_salbutamol",as.numeric(as.character(sc$fvc_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(sc$fvc_post_salbutamol[i]))<0.6){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_post_salbutamol",as.numeric(as.character(sc$fvc_post_salbutamol[i])),"Less than 0.6: Check units\n",sep=","))}
    }
    if(!is.na(sc$fvc_change[i])){
      if(as.numeric(as.character(sc$fvc_change[i]))>75){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_change",as.numeric(as.character(sc$fvc_change[i])),"Greater than 75\n",sep=","))}else
      if(as.numeric(as.character(sc$fvc_change[i]))+10<0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fvc_change",as.numeric(as.character(sc$fvc_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(sc$fef2575_pre_salbutamol[i])){
      if(as.numeric(as.character(sc$fef2575_pre_salbutamol[i]))>7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(sc$fef2575_pre_salbutamol[i])),"Greater than 7: Check units\n",sep=","))}else
      if(as.numeric(as.character(sc$fef2575_pre_salbutamol[i]))<0.1){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_pre_salbutamol",as.numeric(as.character(sc$fef2575_pre_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(sc$fef2575_post_salbutamol[i])){
      if(as.numeric(as.character(sc$fef2575_post_salbutamol[i]))>7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_post_salbutamol",as.numeric(as.character(sc$fef2575_post_salbutamol[i])),"Greater than 7\n",sep=","))}else
      if(as.numeric(as.character(sc$fef2575_post_salbutamol[i]))<0.1){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_post_salbutamol",as.numeric(as.character(sc$fef2575_post_salbutamol[i])),"Less than 0.1: Check units\n",sep=","))}
    }
    if(!is.na(sc$fef2575_change[i])){
      if(as.numeric(as.character(sc$fef2575_change[i]))>100){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_change",as.numeric(as.character(sc$fef2575_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(sc$fef2575_change[i]))+10<0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"fef2575_change",as.numeric(as.character(sc$fef2575_change[i])),"Less than -10\n",sep=","))}
    }
    if(!is.na(sc$pef_pre_salbutamol[i])){
    if(sc$pef_pre_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_pre_salbutamol",as.numeric(as.character(sc$pef_pre_salbutamol[i])),"Units not specified\n",sep=","))
    } else
    if(sc$pef_pre_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(sc$pef_pre_salbutamol[i]))/60>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_pre_salbutamol",as.numeric(as.character(sc$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_pre_salbutamol[i]))/60<0.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_pre_salbutamol",as.numeric(as.character(sc$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }else
    if(sc$pef_pre_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(sc$pef_pre_salbutamol[i]))>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_pre_salbutamol",as.numeric(as.character(sc$pef_pre_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_pre_salbutamol[i]))<0.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_pre_salbutamol",as.numeric(as.character(sc$pef_pre_salbutamol[i])),"Less than 0.5 lsec\n",sep=","))}
      }
    }
    if(!is.na(sc$pef_post_salbutamol[i])){

    if(sc$pef_post_salbutamol_um[i]==""){
    IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_post_salbutamol",as.numeric(as.character(sc$pef_post_salbutamol[i])),"Units not specified\n",sep=","))
    } else
   if(sc$pef_post_salbutamol_um[i]=="l_min"){
      if(as.numeric(as.character(sc$pef_post_salbutamol[i]))/60>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_post_salbutamol",as.numeric(as.character(sc$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_post_salbutamol[i]))/60<0.7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_post_salbutamol",as.numeric(as.character(sc$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    } else
   if(sc$pef_post_salbutamol_um[i]=="l_sec"){
      if(as.numeric(as.character(sc$pef_post_salbutamol[i]))>14){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_post_salbutamol",as.numeric(as.character(sc$pef_post_salbutamol[i])),"Greater than 14 lsec\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_post_salbutamol[i]))<0.7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_post_salbutamol",as.numeric(as.character(sc$pef_post_salbutamol[i])),"Less than 0.7 lsec\n",sep=","))}
    }
  }
# row 1060

    if(!is.na(sc$pef_change[i])){
      if(as.numeric(as.character(sc$pef_change[i]))>100){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_change",as.numeric(as.character(sc$pef_change[i])),"Greater than 100\n",sep=","))}else
      if(as.numeric(as.character(sc$pef_change[i]))+10 <0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"pef_change",as.numeric(as.character(sc$pef_change[i])),"Less than -10\n",sep=","))}
    }

    # down to row 1131

  } # i
} # end of function



#####################################
# vw interim study completion 20121018.txt
CO<-function(fname){
co<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(co)){
   if(co$patient_code[i]!=""){
    no<-unlist(strsplit(as.character(co$patient_code[i]),"-"))[2]
    if(is.na(no)){IssueOut(paste(fname,i,co[i,1],co[i,2],co[i,3],"patient_code",co$patient_code[i],"Incorrect format: no hyphen\n",sep=","))}
     }
    if(co$investigator[i]==""){IssueOut(paste(fname,i,co[i,1],co[i,2],co[i,3],"investigator","","No investigator code\n",sep=","))}
    if(co$reason_for_withdrawal[i]=="withdrawal_of_consent"){IssueOut(paste(fname,i,co[i,1],co[i,2],co[i,3],"reason_for_withdrawal","withdrawal_of_consent","Inform Biobank\n",sep=","))}
      if(co$reason_for_withdrawal_specify_other[i]=="DID NOT CONSENT TO BRONCHOSCOPY"){IssueOut(paste(fname,i,co[i,1],co[i,2],co[i,3],"reason_for_withdrawal_specify_other","DID NOT CONSENT TO BRONCHOSCOPY","No consent\n",sep=","))} else
      if(co$reason_for_withdrawal_specify_other[i]=="PATIENT DECIDED NOT TO UNDERGO BRONCHOSCOPY"){IssueOut(paste(fname,i,co[i,1],co[i,2],co[i,3],"reason_for_withdrawal_specify_other","PATIENT DECIDED NOT TO UNDERGO BRONCHOSCOPY","No consent\n",sep=","))}

  } # i
} # end of function



#####################################
# Compare dates between files
dates<-function(long1.name,base1.name,base2.name,sae.name,repe.name,scre.name){ # specify all of the filenames
long1<-data.frame(read.table(long1.name,sep=",",header=T,na.strings=c("NA","","\\N")))
base1<-data.frame(read.table(base1.name,sep=",",header=T,na.strings=c("NA","","\\N")))
base2<-data.frame(read.table(base2.name,sep=",",header=T,na.strings=c("NA","","\\N")))
sae<-data.frame(read.table(sae.name,sep=",",header=T,na.strings=c("NA","","\\N")))
repe<-data.frame(read.table(repe.name,sep=",",header=T,na.strings=c("NA","","\\N")))
scre<-data.frame(read.table(scre.name,sep=",",header=T,na.strings=c("NA","","\\N")))

# 6 loops required
for(i in 1:nrow(long1)){
if(length(which(scre$patient_code==as.character(long1$patient_code[i])))==0){
IssueOut(paste(long1.name,i,long1[i,1],long1[i,2],long1[i,3],"visit_date",long1$visit_date[i],"Patient does not have screening data\n",sep=","))
scr.date=""
}else{
scr.date<-scre$visit_date[which(scre$patient_code==as.character(long1$patient_code[i]))]
scr.date<-as.Date(scr.date,"%d/%m/%Y")
}

if(length(base1$visit_date[which(base1$patient_code==as.character(long1$patient_code[i]))])>0){
bas.date<-base1$visit_date[which(base1$patient_code==as.character(long1$patient_code[i]))]
bas.date<-as.Date(bas.date,"%d/%m/%Y")
} else {bas.date=""}

if(!is.na(long1$visit_date[i]) && as.character(scr.date)!=""){
lon.date<-as.Date(long1$visit_date[i],"%d/%m/%Y")
diff.date<-lon.date-scr.date
if(diff.date<0){IssueOut(paste(long1.name,i,long1[i,1],long1[i,2],long1[i,3],"visit_date",long1$visit_date[i],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(long1$valid_spirometry_date[i]) && as.character(bas.date)!=""){
spi.date<-as.Date(long1$valid_spirometry_date[i],"%d/%m/%Y")
diff.date<-spi.date-bas.date
if(diff.date<0){IssueOut(paste(long1.name,i,long1[i,1],long1[i,2],long1[i,3],"valid_spirometry_date",long1$valid_spirometry_date[i],"Date is prior to baseline date\n",sep=","))}
}

# Historical reversibility checks are allowed - November 2013
#if(!is.na(long1$reversibility_test_date[i]) && as.character(scr.date)!=""){
#rev.date<-as.Date(long1$reversibility_test_date[i],"%d/%m/%Y")
#diff.date<-rev.date-scr.date
#if(diff.date<0){IssueOut(paste(long1.name,i,long1[i,1],long1[i,2],long1[i,3],"reversibility_test_date",long1$reversibility_test_date[i],"Date is prior to screening date\n",sep=","))}
#}

}   # i    long1

for(j in 1:nrow(base1)){
if(length(which(scre$patient_code==as.character(base1$patient_code[j])))==0){
IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"visit_date",base1$visit_date[j],"Patient does not have screening data\n",sep=","))
scr.date=""
}else{
scr.date<-scre$visit_date[which(scre$patient_code==as.character(base1$patient_code[j]))]
scr.date<-as.Date(scr.date,"%d/%m/%Y")
}

if(!is.na(base1$urine_sampling_date[j]) && as.character(scr.date)!=""){
cot.date<-as.Date(base1$urine_sampling_date[j],"%d/%m/%Y")
diff.date<-cot.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"urine_sampling_date",base1$urine_sampling_date[j],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(base1$nicotine_sampling_date[j]) && as.character(scr.date)!=""){
nic.date<-as.Date(base1$nicotine_sampling_date[j],"%d/%m/%Y")
diff.date<-nic.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"nicotine_sampling_date",base1$nicotine_sampling_date[j],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(base1$valid_spirometry_date[j]) && as.character(scr.date)!=""){
spi.date<-as.Date(base1$valid_spirometry_date[j],"%d/%m/%Y")
diff.date<-spi.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"valid_spirometry_date",base1$valid_spirometry_date[j],"Date is prior to screening date\n",sep=","))}
}

# Historical reversibility check allowed - November 2013
#if(!is.na(base1$reversibility_test_date[j]) && as.character(scr.date)!=""){
#rev.date<-as.Date(base1$reversibility_test_date[j],"%d/%m/%Y")
#diff.date<-rev.date-scr.date
#if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"reversibility_test_date",base1$reversibility_test_date[j],"Date is prior to screening date\n",sep=","))}
#}

if(!is.na(base1$plethysmography_date[j]) && as.character(scr.date)!=""){
ple.date<-as.Date(base1$plethysmography_date[j],"%d/%m/%Y")
diff.date<-ple.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"plethysmography_date",base1$plethysmography_date[j],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(base1$fot_date[j]) && as.character(scr.date)!=""){
fot.date<-as.Date(base1$fot_date[j],"%d/%m/%Y")
diff.date<-fot.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"fot_date",base1$fot_date[j],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(base1$lge_sampling_date[j]) && as.character(scr.date)!=""){
lge.date<-as.Date(base1$lge_sampling_date[j],"%d/%m/%Y")
diff.date<-lge.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"lge_sampling_date",base1$lge_sampling_date[j],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(base1$skin_prick_date[j]) && as.character(scr.date)!=""){
ski.date<-as.Date(base1$skin_prick_date[j],"%d/%m/%Y")
diff.date<-ski.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"skin_prick_date",base1$skin_prick_date[j],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(base1$sputum_sampling_date[j]) && as.character(scr.date)!=""){
spu.date<-as.Date(base1$sputum_sampling_date[j],"%d/%m/%Y")
diff.date<-spu.date-scr.date
if(diff.date<0){IssueOut(paste(base1.name,j,base1[j,1],base1[j,2],base1[j,3],"sputum_sampling_date",base1$sputum_sampling_date[j],"Date is prior to screening date\n",sep=","))}
}

} # j base1

for(k in 1:nrow(base2)){
if(length(which(scre$patient_code==as.character(base2$patient_code[k])))==0){
IssueOut(paste(base2.name,k,base2[k,1],base2[k,2],base2[k,3],"visit_date",base2$visit_date[k],"Patient does not have screening data\n",sep=","))
scr.date=""
}else{
scr.date<-scre$visit_date[which(scre$patient_code==as.character(base2$patient_code[k]))]
scr.date<-as.Date(scr.date,"%d/%m/%Y")
}

if(!is.na(base2$sputum_sampling_date[k]) && as.character(scr.date)!=""){
spu.date<-as.Date(base2$sputum_sampling_date[k],"%d/%m/%Y")
diff.date<-spu.date-scr.date
if(diff.date<0){IssueOut(paste(base2.name,k,base2[k,1],base2[k,2],base2[k,3],"urine_sampling_date",base1$urine_sampling_date[k],"Date is prior to screening date\n",sep=","))}
}

} # k base2

for(l in 1:nrow(sae)){
if(length(which(scre$patient_code==as.character(sae$patient_code[l])))==0){
IssueOut(paste(sae.name,l,sae[l,1],sae[l,2],sae[l,3],"visit_date",sae$visit_date[l],"Patient does not have screening data\n",sep=","))
scr.date=""
}else{
scr.date<-scre$visit_date[which(scre$patient_code==as.character(sae$patient_code[l]))]
scr.date<-as.Date(scr.date,"%d/%m/%Y")
}

if(!is.na(sae$onset_date_event[l]) && as.character(scr.date)!=""){
ons.date<-as.Date(sae$onset_date_event[l],"%d/%m/%Y")
diff.date<-ons.date-scr.date
if(diff.date<0){IssueOut(paste(sae.name,l,sae[l,1],sae[l,2],sae[l,3],"onset_date_event",sae$onset_date_event[l],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(sae$end_date_event[l]) && as.character(scr.date)!=""){
end.date<-as.Date(sae$end_date_event[l],"%d/%m/%Y")
diff.date<-end.date-scr.date
if(diff.date<0){IssueOut(paste(sae.name,l,sae[l,1],sae[l,2],sae[l,3],"end_date_event",sae$end_date_event[l],"Date is prior to screening date\n",sep=","))}
diff.date<-end.date-ons.date
if(diff.date<0){IssueOut(paste(sae.name,l,sae[l,1],sae[l,2],sae[l,3],"end_date_event",sae$end_date_event[l],"Date is prior to onset date\n",sep=","))}
}

} # l md sae


if(dim(repe)[1]>0){
  for(m in 1:nrow(repe)){

if(length(which(scre$patient_code==as.character(repe$patient_code[m])))==0){
IssueOut(paste(repe.name,m,repe[m,1],repe[m,2],repe[m,3],"visit_date",repe$visit_date[m],"Patient does not have screening data\n",sep=","))
scr.date=""
}else{
scr.date<-scre$visit_date[which(scre$patient_code==as.character(repe$patient_code[m]))]
scr.date<-as.Date(scr.date,"%d/%m/%Y")
}

if(!is.na(repe$valid_spirometry_date[m]) && as.character(scr.date)!=""){
spi.date<-as.Date(repe$valid_spirometry_date[m],"%d/%m/%Y")
diff.date<-spi.date-scr.date
if(diff.date<0){IssueOut(paste(repe.name,m,repe[m,1],repe[m,2],repe[m,3],"valid_spirometry_date",repe$valid_spirometry_date[m],"Date is prior to screening date\n",sep=","))}
}

if(!is.na(repe$sputum_sampling_date[m]) && as.character(scr.date)!=""){
spu.date<-as.Date(repe$sputum_sampling_date[m],"%d/%m/%Y")
diff.date<-spu.date-scr.date
if(diff.date<0){IssueOut(paste(repe.name,m,repe[m,1],repe[m,2],repe[m,3],"sputum_sampling_date",repe$sputum_sampling_date[m],"Date is prior to screening date\n",sep=","))}
}

  } # m repeatabiliy

} # if file not empty


for(n in 1:nrow(scre)){
if(!is.na(scre$visit_date[n])){
scr.date<-as.Date(scre$visit_date[n],"%d/%m/%Y")
} else {scr.date=""}

if(!is.na(scre$valid_spirometry_date[n]) && as.character(scr.date)!=""){
spi.date<-as.Date(scre$valid_spirometry_date[n],"%d/%m/%Y")
diff.date<-spi.date-scr.date
if(diff.date<0){IssueOut(paste(scre.name,n,scre[n,1],scre[n,2],scre[n,3],"valid_spirometry_date",scre$valid_spirometry_date[n],"Date is prior to screening date\n",sep=","))}
}

# Following query skipped as the value is a time, not a date
#if(!is.na(scre$spirometry_start[n]) && as.character(scr.date)!=""){
#sta.date<-as.Date(scre$spirometry_start[n],"%d/%m/%Y")
#diff.date<-sta.date-scr.date
#if(diff.date<0){IssueOut(paste(scre.name,n,scre[n,1],scre[n,2],scre[n,3],"spirometry_start",scre$spirometry_start[n],"Date is prior to screening date\n",sep=","))}
#}

} # n screening

} # end of function

########################
# 1st December 2013
# BIOCHEMISTRY AND HAEMATOLOGY

# vw interim screening 20121018.txt
HA<-function(fname){
sc<-data.frame(read.table(fname,sep=",",header=T))
  for(i in 1:nrow(sc)){
    if(!is.na(sc$haemoglobin[i])){
#      if(sc$haemoglobin[i]<5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"haemoglobin",sc$haemoglobin[i],"Less than 5: check value\n",sep=","))} else
      if(sc$haemoglobin[i]>100){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"haemoglobin",sc$haemoglobin[i],"Greater than 100: check units are g/dL\n",sep=","))}
      }
    if(!is.na(sc$wbcs[i])){
#      if(sc$wbcs[i]<2){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"wbcs",sc$wbcs[i],"Less than 2: check units\n",sep=","))} else
      if(sc$wbcs[i]>18){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"wbcs",sc$wbcs[i],"Greater than 18: check units are 10^3/microL\n",sep=","))}
      }
    if(!is.na(sc$neutrophils[i])){
#      if(sc$neutrophils[i]<1.6){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"neutrophils",sc$neutrophils[i],"Less than 1.6: check units\n",sep=","))} else
#      if(sc$neutrophils[i]>20){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"neutrophils",sc$neutrophils[i],"Greater than 20: check units\n",sep=","))}  # units
      if(sc$neutrophils[i]>100){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"neutrophils",sc$neutrophils[i],"Greater than 100: check units are percent\n",sep=","))} # percentage
      }
    if(!is.na(sc$lymphocytes[i])){
#      if(sc$lymphocytes[i]<0.7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"lymphocytes",sc$lymphocytes[i],"Less than 0.7: check units\n",sep=","))} else
#      if(sc$lymphocytes[i]>10){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"lymphocytes",sc$lymphocytes[i],"Greater than 10: check units\n",sep=","))}  # units
      if(sc$lymphocytes[i]>100){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"lymphocytes",sc$lymphocytes[i],"Greater than 100: check units are percent\n",sep=","))}  # percentage
      }
    if(!is.na(sc$monocytes[i])){
#      if(sc$monocytes[i]<0.04){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"monocytes",sc$monocytes[i],"Less than 0.04: check units\n",sep=","))} else
#      if(sc$monocytes[i]>2){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"monocytes",sc$monocytes[i],"Greater than 2: check units\n",sep=","))}     # units
      if(sc$monocytes[i]>25){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"monocytes",sc$monocytes[i],"Greater than 25: check units are percent\n",sep=","))}       # percentage
      }
    if(!is.na(sc$basophils[i])){
#      if(sc$basophils[i]<0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"basophils",sc$basophils[i],"Less than 0: check units\n",sep=","))} else
#      if(sc$basophils[i]>0.7){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"basophils",sc$basophils[i],"Greater than 0.7: check units\n",sep=","))}  # units
      if(sc$basophils[i]>25){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"basophils",sc$basophils[i],"Greater than 25: check units are percent\n",sep=","))}   # percentage
      }
    if(!is.na(sc$eosinophils[i])){
#      if(sc$eosinophils[i]<0){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"eosinophils",sc$eosinophils[i],"Less than 0: check units\n",sep=","))} else
#      if(sc$eosinophils[i]>0.9){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"eosinophils",sc$eosinophils[i],"Greater than 0.9: check units\n",sep=","))}  # units
      if(sc$eosinophils[i]>100){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"eosinophils",sc$eosinophils[i],"Greater than 100: check units are percent\n",sep=","))}   # percentage
      }
    if(!is.na(sc$platelets[i])){
      if(sc$platelets[i]<80){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"platelets",sc$platelets[i],"Less than 80: check units are 10^3/microL\n",sep=","))} #else
#      if(sc$platelets[i]>400){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"platelets",sc$platelets[i],"Greater than 400: check units\n",sep=","))}
      }
    if(sc$c_reactive_protein[i]!=""){
      if(sc$c_reactive_protein[i]=="< 5" || sc$c_reactive_protein[i]=="< 5.00" || sc$c_reactive_protein[i]=="<1" || sc$c_reactive_protein[i]=="<1.00"){} else
      if(sc$c_reactive_protein[i]=="<5" || sc$c_reactive_protein[i]=="<5.0" || sc$c_reactive_protein[i]==">1.00"){} else
      if(sc$c_reactive_protein[i]=="0,79"){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"c_reactive_protein",sc$c_reactive_protein[i],"Non-numerical value\n",sep=","))} else
      if(as.numeric(as.character(sc$c_reactive_protein[i]))<1.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"c_reactive_protein",sc$c_reactive_protein[i],"Less than 1.5: check units are mg/L\n",sep=","))} #else
#      if(as.numeric(as.character(sc$c_reactive_protein[i]))>10){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"c_reactive_protein",sc$c_reactive_protein[i],"Greater than 10: check units\n",sep=","))}
      }
    if(!is.na(sc$sodium[i])){
      if(sc$sodium[i]<100){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"sodium",sc$sodium[i],"Less than 100: check units are mmol/L\n",sep=","))} else
      if(sc$sodium[i]>180){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"sodium",sc$sodium[i],"Greater than 180: check units are mmol/L\n",sep=","))}
      }
    if(!is.na(sc$potassium[i])){
      if(sc$potassium[i]<2){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"potassium",sc$potassium[i],"Less than 2: check units are mmol/L\n",sep=","))} else
      if(sc$potassium[i]>8){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"potassium",sc$potassium[i],"Greater than 8: check units are mmol/L\n",sep=","))}
      }
    if(!is.na(sc$creatinine[i])){
      if(sc$creatinine[i]<10){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"creatinine",sc$creatinine[i],"Less than 10: check units  are umol/L\n",sep=","))} #else
#      if(sc$creatinine[i]>133){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"creatinine",sc$creatinine[i],"Greater than 133: check units\n",sep=","))}
      }
    if(!is.na(sc$blood_urea_nitrogen[i])){
      if(sc$blood_urea_nitrogen[i]<8){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"blood_urea_nitrogen",sc$blood_urea_nitrogen[i],"Less than 8: check units are mg/dL\n",sep=","))} #else
#      if(sc$blood_urea_nitrogen[i]>50){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"blood_urea_nitrogen",sc$blood_urea_nitrogen[i],"Greater than 50: check units\n",sep=","))}
      }
    if(!is.na(sc$ast[i])){
      if(sc$ast[i]<1.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"ast",sc$ast[i],"Less than 1.5: check units are U/L\n",sep=","))}# else
#      if(sc$ast[i]>59){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"ast",sc$ast[i],"Greater than 59: check units\n",sep=","))}
      }
    if(!is.na(sc$alt[i])){
      if(sc$alt[i]<1.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"alt",sc$alt[i],"Less than 1.5: check units are U/L\n",sep=","))} #else
#      if(sc$alt[i]>72){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"alt",sc$alt[i],"Greater than 72: check units\n",sep=","))}
      }
    if(!is.na(sc$total_bilirubin[i])){
      if(sc$total_bilirubin[i]<1.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"total_bilirubin",sc$total_bilirubin[i],"Less than 1.5: check units are umol/L\n",sep=","))} #else
#      if(sc$total_bilirubin[i]>26){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"total_bilirubin",sc$total_bilirubin[i],"Greater than 26: check units\n",sep=","))}
      }
    if(!is.na(sc$ldh[i])){
      if(sc$ldh[i]<10){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"ldh",sc$ldh[i],"Less than 10: check units are U/L\n",sep=","))}# else
#      if(sc$ldh[i]>616){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"ldh",sc$ldh[i],"Greater than 616: check units\n",sep=","))}
      }
    if(!is.na(sc$gamma_gt[i])){
      if(sc$gamma_gt[i]<4){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"gamma_gt",sc$gamma_gt[i],"Less than 4: check units are U/L\n",sep=","))} #else
#      if(sc$gamma_gt[i]>115){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"gamma_gt",sc$gamma_gt[i],"Greater than 115: check units\n",sep=","))}
      }
    if(!is.na(sc$alkaline_phosphatase[i])){
      if(sc$alkaline_phosphatase[i]<5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"alkaline_phosphatase",sc$alkaline_phosphatase[i],"Less than 5: check units are U/L\n",sep=","))} #else
#      if(sc$alkaline_phosphatase[i]>400){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"alkaline_phosphatase",sc$alkaline_phosphatase[i],"Greater than 400: check units\n",sep=","))}
      }
    if(!is.na(sc$total_protein[i])){
 #     if(sc$total_protein[i]<6.4){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"total_protein",sc$total_protein[i],"Less than 6.4: check units\n",sep=","))} else
      if(sc$total_protein[i]>20){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"total_protein",sc$total_protein[i],"Greater than 20: check units are g/dL\n",sep=","))}
      }
    if(!is.na(sc$albumin[i])){
#      if(sc$albumin[i]<3.5){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"albumin",sc$albumin[i],"Less than 3.5: check units\n",sep=","))} else
      if(sc$albumin[i]>20){IssueOut(paste(fname,i,sc[i,1],sc[i,2],sc[i,3],"albumin",sc$albumin[i],"Greater than 20: check units are g/dL\n",sep=","))}
      }
    
  } # i
} # END OF FUNCTION 
 