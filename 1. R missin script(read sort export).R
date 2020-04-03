


setwd("C:/JHR")
getwd()
rawA <- read.csv("SNU2600A.csv", header = TRUE, stringsAsFactors = TRUE)
head(rawA)

rawA.call <- rawA[grep("^chr13$",rawA$chr_name),]

head(rawA.call)

rawA.call

rawA.sort <- as.data.frame(rawA.call)
write.csv(rawA.sort, file="C:/JHR/SNU2600A chr13.csv", row.names=FALSE)
