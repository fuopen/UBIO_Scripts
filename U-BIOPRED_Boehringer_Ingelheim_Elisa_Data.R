# U-BIOPRED Boehringer Ingelheim Elisa Data

elisa_data = read.delim("/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Elisas_BI/9999_0001_PDSPT02_P_P40115MX.txt", header = T)

subset(elisa_data, CPEVENT == "BASELINE", ) -> elisa_data_baseline
subset(elisa_data, CPEVENT == "LONGITUDINAL", ) -> elisa_data_long

levels(elisa_data$ANALYTE) -> analytes

for (i in 1:length(analytes)){
  subset(elisa_data_baseline, ANALYTE == analytes[1], select = c(PTNO,VALUE)) -> data.frame()
}


library(plyr)
library(reshape2)

melt(elisa_data_baseline, id.vars="PTNO", measure.vars="")
