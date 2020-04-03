setwd("E:/R/pancchr/raw")

rawA <- read.csv("20190125variant_summary ES308~312.csv", header = T, stringsAsFactors = TRUE, na.strings = NA)


raw308 <- rawA[rawA$KCLB19ES308_ratio.Alt.Ref.Alt>0,]
raw309 <- rawA[rawA$KCLB19ES309_ratio.Alt.Ref.Alt>0,]
raw310 <- rawA[rawA$KCLB19ES310_ratio.Alt.Ref.Alt>0,]
raw311 <- rawA[rawA$KCLB19ES311_ratio.Alt.Ref.Alt>0,]
raw312 <- rawA[rawA$KCLB19ES312_ratio.Alt.Ref.Alt>0,]

raw257$Gene_Name <- substr(raw257Gene_Name, 2, 100)

write.csv(raw308, file="E:/R/pancchr/raw/SNU2670.csv", row.names=FALSE)
write.csv(raw309, file="E:/R/pancchr/raw/SNU2823.csv", row.names=FALSE)
write.csv(raw310, file="E:/R/pancchr/raw/SNU2729B1.csv", row.names=FALSE)
write.csv(raw311, file="E:/R/pancchr/raw/SNU4223.csv", row.names=FALSE)
write.csv(raw312, file="E:/R/pancchr/raw/SNU4482.csv", row.names=FALSE)
