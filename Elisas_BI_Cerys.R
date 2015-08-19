# BI Elisa Scripts

library(gdata)

matr=read.delim("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Elisas_BI/9999_0001_PDSPT02_P_P40115MX.txt")


longitud= matr[which(matr$CPEVENT=="LONGITUDINAL")  ,]


basel= matr[which(matr$CPEVENT=="BASELINE")  ,]



####BASELINE


baseln= data.frame(
  kit_id = basel$PTNO,
  basel$VALUE[which(basel$ANALYTE=="CHEMOKINE LIGAND 17")],
  basel$VALUE[which(basel$ANALYTE=="CHEMOKINE LIGAND 22")],
  basel$VALUE[which(basel$ANALYTE=="EOTAXIN")],
  basel$VALUE[which(basel$ANALYTE=="EOTAXIN 3" )],
  basel$VALUE[which(basel$ANALYTE=="IFN_GAMMA")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 1_BETA")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 10")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 12_P70")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 13")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 2")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 4")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 6")],
  basel$VALUE[which(basel$ANALYTE=="INTERLEUKIN 8")],
  basel$VALUE[which(basel$ANALYTE=="IP10")],
  basel$VALUE[which(basel$ANALYTE=="MCP-1")],
  basel$VALUE[which(basel$ANALYTE=="MCP-4")],
  basel$VALUE[which(basel$ANALYTE=="MIP1ALPHA")],
  basel$VALUE[which(basel$ANALYTE=="MIP1BETA")],
  basel$VALUE[which(basel$ANALYTE=="TNF_ALPHA")]
)

cols= c("kit_id", levels(basel$ANALYTE))
  colnames(baseln)= cols


for(i in 2:length(baseln)){
  names(baseln)[i]<-paste(names(baseln)[i], "(pg/mL)", sep="")}
names(baseln)= gsub(" ", "_", names(longitudn))



View(baseln)

ubio_id_db = read.xls("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/UBIO_ID_DB.xlsx")

names(baseln)[1]="kit_id"

write.csv(baseln, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Elisas_BI/load/base_BI_plasma_cytok.csv", row.names=F, na=".", quote = F)

#####LONGITUDINAL


longitudn= longitudn= data.frame(
  longitud$PTNO,
  longitud$VALUE[which(longitud$ANALYTE=="CHEMOKINE LIGAND 17")],
  longitud$VALUE[which(longitud$ANALYTE=="CHEMOKINE LIGAND 22")],
  longitud$VALUE[which(longitud$ANALYTE=="EOTAXIN")],
  longitud$VALUE[which(longitud$ANALYTE=="EOTAXIN 3" )],
  longitud$VALUE[which(longitud$ANALYTE=="IFN_GAMMA")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 1_BETA")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 10")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 12_P70")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 13")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 2")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 4")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 6")],
  longitud$VALUE[which(longitud$ANALYTE=="INTERLEUKIN 8")],
  longitud$VALUE[which(longitud$ANALYTE=="IP10")],
  longitud$VALUE[which(longitud$ANALYTE=="MCP-1")],
  longitud$VALUE[which(longitud$ANALYTE=="MCP-4")],
  longitud$VALUE[which(longitud$ANALYTE=="MIP1ALPHA")],
  longitud$VALUE[which(longitud$ANALYTE=="MIP1BETA")],
  longitud$VALUE[which(longitud$ANALYTE=="TNF_ALPHA")]
)

cols2= c("subjid", levels(longitud$ANALYTE))
colnames(longitudn)= cols2

for(i in 2:length(longitudn)){
names(longitudn)[i]<-paste(names(longitudn)[i], "(pg/mL)")}

names(longitudn)= gsub(" ", "_", names(longitudn))


View(longitudn)

write.csv(longitudn, file = "/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Elisas_BI/load/long_BI_plasma_cytok.csv", row.names=F, na=".", quote = F)



##########################
