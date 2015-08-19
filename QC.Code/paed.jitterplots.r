# Produce jitter plots with QC threshold overlaid, for clinical review meeting
# August 2015
# Aruna Bansal

setwd("C:/Users/Aruna/Acclarogen/UBIOPRED/2015_03/Paed")


l1.name="dt_long11.txt"
b1.name="dt_base.txt"   
sc.name="dt_screen.txt"

MakePlot<-function(yvar,site.var,thresh,var.name,in.name){
  if(length(thresh)==1){thresh.txt=thresh}else{thresh.txt=paste(thresh[1],thresh[2],sep=",")}
  plot(jitter(site.var),yvar,xaxt="n",ylab=var.name,type="n",xlab="Site",cex.axis=0.8,main=paste(in.name,"/",var.name,"\nThreshold(s) =",thresh.txt),xlim=c(16,22))
  axis(1, at=c(16:22), labels=c(16:22),las=1)
  points(jitter(site.var),yvar,pch=16,col="darkblue")
  abline(h=thresh,lwd=2,col="darkred")
  abline(v=c(16:22),lwd=1,col="grey")

}

########################################
# vw interim longitudinal 1 20121018.txt

l1<-data.frame(read.table(l1.name,sep=",",header=T))

# derive site
l1$site<-rep(NA,nrow(l1))
for(j in 1:nrow(l1)){
l1$site[j]=strsplit(as.character(l1$patient_code[j]),"\\-")[[1]][1]
}

pdf(paste(l1.name,".jitter.pdf",sep=""))
    MakePlot(l1$systolic_blood_pressure,as.numeric(as.character(l1$site)),c(80,200),"systolic_blood_pressure",l1.name)
    MakePlot(l1$diastolic_blood_pressure,as.numeric(as.character(l1$site)),c(40,120),"diastolic_blood_pressure",l1.name)
    MakePlot(l1$heart_rate,as.numeric(as.character(l1$site)),c(30,120),"heart_rate",l1.name)
    MakePlot(l1$respiratory_rate,as.numeric(as.character(l1$site)),c(6,36),"respiratory_rate",l1.name)
    MakePlot(l1$fev1_actual,as.numeric(as.character(l1$site)),c(0.2,6.5),"fev1_actual",l1.name)
    MakePlot(l1$fev1_predicted,as.numeric(as.character(l1$site)),c(0.3,6.5),"fev1_predicted",l1.name)
    MakePlot(l1$fev1_percentage,as.numeric(as.character(l1$site)),c(10,130),"fev1_percentage",l1.name)
    MakePlot(l1$fvc_actual,as.numeric(as.character(l1$site)),c(0.5,7),"fvc_actual",l1.name)
    MakePlot(l1$fvc_predicted,as.numeric(as.character(l1$site)),c(1,7),"fvc_predicted",l1.name)
    MakePlot(l1$fvc_percentage,as.numeric(as.character(l1$site)),c(30,140),"fvc_percentage",l1.name)
    MakePlot(l1$fef2575_actual,as.numeric(as.character(l1$site)),c(0.1,6),"fef2575_actual",l1.name)
    MakePlot(l1$fef2575_predicted,as.numeric(as.character(l1$site)),c(1.5,7),"fef2575_predicted",l1.name)
    MakePlot(l1$fef2575_percentage,as.numeric(as.character(l1$site)),c(5,140),"fef2575_percentage",l1.name)
    MakePlot(l1$pef_actual[which(l1$pef_actual_um=="l_min")]/60,as.numeric(as.character(l1$site))[which(l1$pef_actual_um=="l_min")],c(0.5,14),"pef_actual (l/min)/60",l1.name)
    MakePlot(l1$pef_actual[which(l1$pef_actual_um=="l_sec")],as.numeric(as.character(l1$site[which(l1$pef_actual_um=="l_sec")])),c(0.5,14),"pef_actual (l/sec)",l1.name)
 
    MakePlot(l1$pef_predicted[which(l1$pef_predicted_um=="l_min")]/60,as.numeric(as.character(l1$site))[which(l1$pef_predicted_um=="l_min")],c(0.7,14),"pef_predicted (l/min)/60",l1.name)
    MakePlot(l1$pef_predicted[which(l1$pef_predicted_um=="l_sec")],as.numeric(as.character(l1$site[which(l1$pef_predicted_um=="l_sec")])),c(0.7,14),"pef_predicted (l/sec)",l1.name)

    MakePlot(l1$pef_percentage,as.numeric(as.character(l1$site)),c(10,150),"pef_percentage",l1.name)
    MakePlot(as.numeric(as.character(l1$fev1_pre_salbutamol)),as.numeric(as.character(l1$site)),c(0.2,6.5),"fev1_pre_salbutamol",l1.name)
    MakePlot(as.numeric(as.character(l1$fev1_post_salbutamol)),as.numeric(as.character(l1$site)),c(0.2,6.5),"fev1_post_salbutamol",l1.name)

    MakePlot(l1$fev1_change,as.numeric(as.character(l1$site)),c(-10,75),"fev1_change",l1.name)
    MakePlot(as.numeric(as.character(l1$fvc_pre_salbutamol)),as.numeric(as.character(l1$site)),c(0.5,7),"fvc_pre_salbutamol",l1.name)
    MakePlot(as.numeric(as.character(l1$fvc_post_salbutamol)),as.numeric(as.character(l1$site)),c(0.6,7),"fvc_post_salbutamol",l1.name)
    MakePlot(as.numeric(as.character(l1$fvc_change)),as.numeric(as.character(l1$site)),c(-10,75),"fvc_change",l1.name)
    MakePlot(as.numeric(as.character(l1$fef2575_pre_salbutamol)),as.numeric(as.character(l1$site)),c(0.1,7),"fef2575_pre_salbutamol",l1.name)
    MakePlot(as.numeric(as.character(l1$fef2575_post_salbutamol)),as.numeric(as.character(l1$site)),c(0.1,7),"fef2575_post_salbutamol",l1.name)
    MakePlot(as.numeric(as.character(l1$fef2575_change)),as.numeric(as.character(l1$site)),c(-10,100),"fef2575_change",l1.name)

    MakePlot(l1$pef_pre_salbutamol[which(l1$pef_pre_salbutamol_um=="l_min")]/60,as.numeric(as.character(l1$site))[which(l1$pef_pre_salbutamol_um=="l_min")],c(0.5,14),"pef_pre_salbutamol (l/min)/60",l1.name)
    MakePlot(l1$pef_pre_salbutamol[which(l1$pef_pre_salbutamol_um=="l_sec")],as.numeric(as.character(l1$site[which(l1$pef_pre_salbutamol_um=="l_sec")])),c(0.5,14),"pef_pre_salbutamol (l/sec)",l1.name)

    MakePlot(l1$pef_post_salbutamol[which(l1$pef_post_salbutamol_um=="l_min")]/60,as.numeric(as.character(l1$site))[which(l1$pef_post_salbutamol_um=="l_min")],c(0.7,14),"pef_post_salbutamol (l/min)/60",l1.name)
    MakePlot(l1$pef_post_salbutamol[which(l1$pef_post_salbutamol_um=="l_sec")],as.numeric(as.character(l1$site[which(l1$pef_post_salbutamol_um=="l_sec")])),c(0.7,14),"pef_post_salbutamol (l/sec)",l1.name)

    MakePlot(as.numeric(as.character(l1$pef_change)),as.numeric(as.character(l1$site)),c(-10,100),"pef_change",l1.name)


dev.off()



########################################
# vw interim baseline day 1 20121018.txt
b1<-data.frame(read.table(b1.name,sep=",",header=T))

# derive site
b1$site<-rep(NA,nrow(b1))
for(j in 1:nrow(b1)){
b1$site[j]=strsplit(as.character(b1$patient_code[j]),"\\-")[[1]][1]
}

pdf(paste(b1.name,".jitter.pdf",sep=""))

    MakePlot(b1$systolic_blood_pressure,as.numeric(as.character(b1$site)),c(80,200),"systolic_blood_pressure",b1.name)
    MakePlot(b1$diastolic_blood_pressure,as.numeric(as.character(b1$site)),c(40,120),"diastolic_blood_pressure",b1.name)
    MakePlot(b1$heart_rate,as.numeric(as.character(b1$site)),c(30,120),"heart_rate",b1.name)
    MakePlot(b1$respiratory_rate,as.numeric(as.character(b1$site)),c(6,36),"respiratory_rate",b1.name)
    
    MakePlot(b1$fev1_actual,as.numeric(as.character(b1$site)),c(0.2,6.5),"fev1_actual",b1.name)
    MakePlot(b1$fev1_predicted,as.numeric(as.character(b1$site)),c(0.3,6.5),"fev1_predicted",b1.name)
    MakePlot(b1$fev1_percentage,as.numeric(as.character(b1$site)),c(10,130),"fev1_percentage",b1.name)
    
    MakePlot(b1$fvc_actual,as.numeric(as.character(b1$site)),c(0.5,7),"fvc_actual",b1.name)
    MakePlot(b1$fvc_predicted,as.numeric(as.character(b1$site)),c(1,7),"fvc_predicted",b1.name)
    MakePlot(b1$fvc_percentage,as.numeric(as.character(b1$site)),c(30,140),"fvc_percentage",b1.name)
    
    MakePlot(b1$fef2575_actual,as.numeric(as.character(b1$site)),c(0.1,6),"fef2575_actual",b1.name)
    MakePlot(b1$fef2575_predicted,as.numeric(as.character(b1$site)),c(1.5,7),"fef2575_predicted",b1.name)
    MakePlot(b1$fef2575_percentage,as.numeric(as.character(b1$site)),c(5,140),"fef2575_percentage",b1.name)
    
    MakePlot(b1$pef_actual[which(b1$pef_actual_um=="l_min")]/60,as.numeric(as.character(b1$site))[which(b1$pef_actual_um=="l_min")],c(0.5,14),"pef_actual (l/min)/60",b1.name)
    MakePlot(b1$pef_actual[which(b1$pef_actual_um=="l_sec")],as.numeric(as.character(b1$site[which(b1$pef_actual_um=="l_sec")])),c(0.5,14),"pef_actual (l/sec)",b1.name)
 
    MakePlot(b1$pef_predicted[which(b1$pef_predicted_um=="l_min")]/60,as.numeric(as.character(b1$site))[which(b1$pef_predicted_um=="l_min")],c(0.7,14),"pef_predicted (l/min)/60",b1.name)
    MakePlot(b1$pef_predicted[which(b1$pef_predicted_um=="l_sec")],as.numeric(as.character(b1$site[which(b1$pef_predicted_um=="l_sec")])),c(0.7,14),"pef_predicted (l/sec)",b1.name)

    MakePlot(b1$pef_percentage,as.numeric(as.character(b1$site)),c(10,150),"pef_percentage",b1.name)
    
    # single value of 1000 in fev1 pre salbutamol
    MakePlot(as.numeric(as.character(b1$fev1_pre_salbutamol)),as.numeric(as.character(b1$site)),c(0.2,6.5),"fev1_pre_salbutamol",b1.name)
    MakePlot(as.numeric(as.character(b1$fev1_post_salbutamol)),as.numeric(as.character(b1$site)),c(0.2,6.5),"fev1_post_salbutamol",b1.name)

    MakePlot(b1$fev1_change,as.numeric(as.character(b1$site)),c(-10,75),"fev1_change",b1.name)
    
    MakePlot(as.numeric(as.character(b1$fvc_pre_salbutamol)),as.numeric(as.character(b1$site)),c(0.5,7),"fvc_pre_salbutamol",b1.name)
    MakePlot(as.numeric(as.character(b1$fvc_post_salbutamol)),as.numeric(as.character(b1$site)),c(0.6,7),"fvc_post_salbutamol",b1.name)
    MakePlot(as.numeric(as.character(b1$fvc_change)),as.numeric(as.character(b1$site)),c(-10,75),"fvc_change",b1.name)
    
    MakePlot(as.numeric(as.character(b1$fef2575_pre_salbutamol)),as.numeric(as.character(b1$site)),c(0.1,7),"fef2575_pre_salbutamol",b1.name)
    MakePlot(as.numeric(as.character(b1$fef2575_post_salbutamol)),as.numeric(as.character(b1$site)),c(0.1,7),"fef2575_post_salbutamol",b1.name)
    MakePlot(as.numeric(as.character(b1$fef2575_change)),as.numeric(as.character(b1$site)),c(-10,100),"fef2575_change",b1.name)

    MakePlot(b1$pef_pre_salbutamol[which(b1$pef_pre_salbutamol_um=="l_min")]/60,as.numeric(as.character(b1$site))[which(b1$pef_pre_salbutamol_um=="l_min")],c(0.5,14),"pef_pre_salbutamol (l/min)/60",b1.name)
    MakePlot(b1$pef_pre_salbutamol[which(b1$pef_pre_salbutamol_um=="l_sec")],as.numeric(as.character(b1$site[which(b1$pef_pre_salbutamol_um=="l_sec")])),c(0.5,14),"pef_pre_salbutamol (l/sec)",b1.name)

    MakePlot(b1$pef_post_salbutamol[which(b1$pef_post_salbutamol_um=="l_min")]/60,as.numeric(as.character(b1$site))[which(b1$pef_post_salbutamol_um=="l_min")],c(0.7,14),"pef_post_salbutamol (l/min)/60",b1.name)
    MakePlot(b1$pef_post_salbutamol[which(b1$pef_post_salbutamol_um=="l_sec")],as.numeric(as.character(b1$site[which(b1$pef_post_salbutamol_um=="l_sec")])),c(0.7,14),"pef_post_salbutamol (l/sec)",b1.name)

    MakePlot(as.numeric(as.character(b1$pef_change)),as.numeric(as.character(b1$site)),c(-10,100),"pef_change",b1.name)

    MakePlot(as.numeric(as.character(b1$plethysmography_tlc)),as.numeric(as.character(b1$site)),c(3,12),"plethysmography_tlc",b1.name)
    MakePlot(as.numeric(as.character(b1$plethysmography_rv)),as.numeric(as.character(b1$site)),c(0.5,8),"plethysmography_rv",b1.name)
    MakePlot(as.numeric(as.character(b1$plethysmography_sgaw)),as.numeric(as.character(b1$site)),c(0,6),"plethysmography_sgaw",b1.name)

    MakePlot(as.numeric(as.character(b1$lge_total)),as.numeric(as.character(b1$site)),5000,"lge_total",b1.name)

    MakePlot(as.numeric(as.character(b1$sputum_weight)),as.numeric(as.character(b1$site)),2,"sputum_weight",b1.name)

    MakePlot(as.numeric(as.character(b1$leukocyte_count)),as.numeric(as.character(b1$site)),c(0,70),"leukocyte_count",b1.name)

dev.off()


#####################################
# vw interim screening 20121018.txt
sc<-data.frame(read.table(sc.name,sep=",",header=T))

# derive site
sc$site<-rep(NA,nrow(sc))
for(j in 1:nrow(sc)){
sc$site[j]=strsplit(as.character(sc$patient_code[j]),"\\-")[[1]][1]
}

pdf(paste(sc.name,".jitter.pdf",sep=""))

    MakePlot(sc$systolic_blood_pressure,as.numeric(as.character(sc$site)),c(80,200),"systolic_blood_pressure",sc.name)
    MakePlot(sc$diastolic_blood_pressure,as.numeric(as.character(sc$site)),c(40,120),"diastolic_blood_pressure",sc.name)
    MakePlot(sc$heart_rate,as.numeric(as.character(sc$site)),c(30,120),"heart_rate",sc.name)
    MakePlot(sc$respiratory_rate,as.numeric(as.character(sc$site)),c(6,36),"respiratory_rate",sc.name)
    MakePlot(sc$fev1_actual,as.numeric(as.character(sc$site)),c(0.2,6.5),"fev1_actual",sc.name)
    MakePlot(sc$fev1_predicted,as.numeric(as.character(sc$site)),c(0.3,6.5),"fev1_predicted",sc.name)
    MakePlot(sc$fev1_percentage,as.numeric(as.character(sc$site)),c(10,130),"fev1_percentage",sc.name)
    MakePlot(sc$fvc_actual,as.numeric(as.character(sc$site)),c(0.5,7),"fvc_actual",sc.name)
    MakePlot(sc$fvc_predicted,as.numeric(as.character(sc$site)),c(1,7),"fvc_predicted",sc.name)
    MakePlot(sc$fvc_percentage,as.numeric(as.character(sc$site)),c(30,140),"fvc_percentage",sc.name)
    MakePlot(sc$fef2575_actual,as.numeric(as.character(sc$site)),c(0.1,6),"fef2575_actual",sc.name)
    MakePlot(sc$fef2575_predicted,as.numeric(as.character(sc$site)),c(1.5,7),"fef2575_predicted",sc.name)
    MakePlot(sc$fef2575_percentage,as.numeric(as.character(sc$site)),c(5,140),"fef2575_percentage",sc.name)
    MakePlot(sc$pef_actual[which(sc$pef_actual_um=="l_min")]/60,as.numeric(as.character(sc$site))[which(sc$pef_actual_um=="l_min")],c(0.5,14),"pef_actual (l/min)/60",sc.name)
    MakePlot(sc$pef_actual[which(sc$pef_actual_um=="l_sec")],as.numeric(as.character(sc$site[which(sc$pef_actual_um=="l_sec")])),c(0.5,14),"pef_actual (l/sec)",sc.name)

    MakePlot(sc$pef_predicted[which(sc$pef_predicted_um=="l_min")]/60,as.numeric(as.character(sc$site))[which(sc$pef_predicted_um=="l_min")],c(0.7,14),"pef_predicted (l/min)/60",sc.name)
    MakePlot(sc$pef_predicted[which(sc$pef_predicted_um=="l_sec")],as.numeric(as.character(sc$site[which(sc$pef_predicted_um=="l_sec")])),c(0.7,14),"pef_predicted (l/sec)",sc.name)

    MakePlot(sc$pef_percentage,as.numeric(as.character(sc$site)),c(10,150),"pef_percentage",sc.name)
    MakePlot(as.numeric(as.character(sc$fev1_pre_salbutamol)),as.numeric(as.character(sc$site)),c(0.2,6.5),"fev1_pre_salbutamol",sc.name)
    MakePlot(as.numeric(as.character(sc$fev1_post_salbutamol)),as.numeric(as.character(sc$site)),c(0.2,6.5),"fev1_post_salbutamol",sc.name)

    MakePlot(sc$fev1_change,as.numeric(as.character(sc$site)),c(-10,75),"fev1_change",sc.name)
    MakePlot(as.numeric(as.character(sc$fvc_pre_salbutamol)),as.numeric(as.character(sc$site)),c(0.5,7),"fvc_pre_salbutamol",sc.name)
    MakePlot(as.numeric(as.character(sc$fvc_post_salbutamol)),as.numeric(as.character(sc$site)),c(0.6,7),"fvc_post_salbutamol",sc.name)
    MakePlot(as.numeric(as.character(sc$fvc_change)),as.numeric(as.character(sc$site)),c(-10,75),"fvc_change",sc.name)
    MakePlot(as.numeric(as.character(sc$fef2575_pre_salbutamol)),as.numeric(as.character(sc$site)),c(0.1,7),"fef2575_pre_salbutamol",sc.name)
    MakePlot(as.numeric(as.character(sc$fef2575_post_salbutamol)),as.numeric(as.character(sc$site)),c(0.1,7),"fef2575_post_salbutamol",sc.name)
    MakePlot(as.numeric(as.character(sc$fef2575_change)),as.numeric(as.character(sc$site)),c(-10,100),"fef2575_change",sc.name)

    MakePlot(sc$pef_pre_salbutamol[which(sc$pef_pre_salbutamol_um=="l_min")]/60,as.numeric(as.character(sc$site))[which(sc$pef_pre_salbutamol_um=="l_min")],c(0.5,14),"pef_pre_salbutamol (l/min)/60",sc.name)
    MakePlot(sc$pef_pre_salbutamol[which(sc$pef_pre_salbutamol_um=="l_sec")],as.numeric(as.character(sc$site[which(sc$pef_pre_salbutamol_um=="l_sec")])),c(0.5,14),"pef_pre_salbutamol (l/sec)",sc.name)

    MakePlot(sc$pef_post_salbutamol[which(sc$pef_post_salbutamol_um=="l_min")]/60,as.numeric(as.character(sc$site))[which(sc$pef_post_salbutamol_um=="l_min")],c(0.7,14),"pef_post_salbutamol (l/min)/60",sc.name)
    MakePlot(sc$pef_post_salbutamol[which(sc$pef_post_salbutamol_um=="l_sec")],as.numeric(as.character(sc$site[which(sc$pef_post_salbutamol_um=="l_sec")])),c(0.7,14),"pef_post_salbutamol (l/sec)",sc.name)

    MakePlot(as.numeric(as.character(sc$pef_change)),as.numeric(as.character(sc$site)),c(-10,100),"pef_change",sc.name)

    MakePlot(as.numeric(as.character(sc$haemoglobin)),as.numeric(as.character(sc$site)),100,"haemoglobin (g/dL)",sc.name)
    MakePlot(as.numeric(as.character(sc$wbcs)),as.numeric(as.character(sc$site)),18,"wbcs (10^3/microL)",sc.name)
    MakePlot(as.numeric(as.character(sc$neutrophils)),as.numeric(as.character(sc$site)),100,"neutrophils (%)",sc.name)
    MakePlot(as.numeric(as.character(sc$lymphocytes)),as.numeric(as.character(sc$site)),100,"lymphocytes (%)",sc.name)
    MakePlot(as.numeric(as.character(sc$monocytes)),as.numeric(as.character(sc$site)),100,"monocytes (%)",sc.name)
    MakePlot(as.numeric(as.character(sc$basophils)),as.numeric(as.character(sc$site)),100,"basophils (%)",sc.name)
    MakePlot(as.numeric(as.character(sc$eosinophils)),as.numeric(as.character(sc$site)),100,"eosinophils (%)",sc.name)
    MakePlot(as.numeric(as.character(sc$platelets)),as.numeric(as.character(sc$site)),80,"platelets (10^3/microL)",sc.name)
    MakePlot(sc$c_reactive_protein,as.numeric(as.character(sc$site)),1.5,"c_reactive_protein (mg/L)",sc.name)
    sc$sodium[which(sc$sodium==999.9)]=NA
    MakePlot(as.numeric(as.character(sc$sodium)),as.numeric(as.character(sc$site)),c(100,180),"sodium (mmol/L)",sc.name)
    sc$potassium[which(sc$potassium==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$potassium)),as.numeric(as.character(sc$site)),c(2,8),"potassium (mmol/L)",sc.name)
    sc$creatinine[which(sc$creatinine==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$creatinine)),as.numeric(as.character(sc$site)),10,"creatinine (umol/L)",sc.name)
    sc$blood_urea_nitrogen[which(sc$blood_urea_nitrogen==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$blood_urea_nitrogen)),as.numeric(as.character(sc$site)),8,"blood_urea_nitrogen (mg/dL)",sc.name)
    sc$ast[which(sc$ast==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$ast)),as.numeric(as.character(sc$site)),1.5,"ast (U/L)",sc.name)
    sc$alt[which(sc$alt==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$alt)),as.numeric(as.character(sc$site)),1.5,"alt (U/L)",sc.name)
    sc$total_bilirubin[which(sc$total_bilirubin==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$total_bilirubin)),as.numeric(as.character(sc$site)),1.5,"total_bilirubin (umol/L)",sc.name)
    MakePlot(as.numeric(as.character(sc$ldh)),as.numeric(as.character(sc$site)),10,"ldh (U/L)",sc.name)
    sc$gamma_gt[which(sc$gamma_gt==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$gamma_gt)),as.numeric(as.character(sc$site)),4,"gamma_gt (U/L)",sc.name)
    sc$alkaline_phosphatase[which(sc$alkaline_phosphatase==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$alkaline_phosphatase)),as.numeric(as.character(sc$site)),5,"alkaline_phosphatase (U/L)",sc.name)
    sc$total_protein[which(sc$total_protein==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$total_protein)),as.numeric(as.character(sc$site)),20,"total_protein (g/dL)",sc.name)
    sc$albumin[which(sc$albumin==999.9)]=NA    
    MakePlot(as.numeric(as.character(sc$albumin)),as.numeric(as.character(sc$site)),20,"albumin (g/dL)",sc.name)

dev.off()




 