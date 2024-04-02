library(tidyr)
library(readxl)
library(writexl)

rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.


try( #AUC계산 및 백보드설정
  {cal.AUC <- function(x) {
    AUC = (colSums(x[,1:9], na.rm = T) + colSums(x[2:5,1:9], na.rm = T))*log10(3)*3/(2*sum(x[1:3,10]))
    AUC = t(AUC)
  }
  
  p1 <- read.csv("E:/R/PANC 2nd/Drug/basic/원래 결과 틀 p1.csv", header=F, sep=",")
  p2 <- read.csv("E:/R/PANC 2nd/Drug/basic/원래 결과 틀 p2.csv", header=F, sep=",")
  
  drug_p1 <- as.matrix(p1)
  drug_p2 <- as.matrix(p2)}
)


try({
  assign(paste0("frame1_",1), drug_p1)
  assign(paste0("frame2_",1), drug_p2)
  
  raw_tbl <- read_excel("E:/R/PANC 2nd/Drug/191011 Drug 정리/2729B1 p1 191014.xlsx",
                        sheet=1, range="C12:L17",
                        col_names = F, col_types = "numeric", na="NA")
  
  tbl <- as.matrix(raw_tbl)
  raw.AUC <- as.matrix(cal.AUC(tbl))

  frame1_1[8,2:10] <- raw.AUC
  frame1_1[2:7,2:11] <- tbl
  
  #drug1 line
  for ( j in 0:4) {
    k <- j*11
    frame1_1[(13+k):(18+k),4] <- tbl[1:6,(1+j*2)] # y value
    frame1_1[(13+k),5] <- mean(tbl[1:3,10]) #control value
    frame1_1[(13+k):(18+k),6] <- as.numeric(frame1_1[(13+k):(18+k),4])/mean(tbl[1:3,10])  # normalized y
    for ( i in 0:4) {
      # AUC segment
      subSun <- as.numeric(frame1_1[(14+i+k),3])-as.numeric(frame1_1[(13+i+k),3])
      subMom <- (as.numeric(frame1_1[(14+i+k),6])+as.numeric(frame1_1[(13+i+k),6]))/2
      frame1_1[(13+i+k),7] <- subMom*subSun
    } 
    
    frame1_1[(19+k),7] <- sum(as.numeric(frame1_1[(13+k):(17+k),7])) # AUC
  }
  #drug2 line
  for ( j in 0:3) {
    k <- j*11
    frame1_1[(13+k):(18+k),11] <- tbl[1:6,(2+j*2)] # y value
    frame1_1[(13+k),12] <- mean(tbl[1:3,10]) #control value
    frame1_1[(13+k):(18+k),13] <- as.numeric(frame1_1[(13+k):(18+k),11])/mean(tbl[1:3,10])  # normalized y
    for ( i in 0:4) {
      # AUC segment
      subSun <- as.numeric(frame1_1[(14+i+k),10])-as.numeric(frame1_1[(13+i+k),10]) # x axis length
      subMom <- (as.numeric(frame1_1[(14+i+k),13])+as.numeric(frame1_1[(13+i+k),13]))/2 # normalized y mean value
      frame1_1[(13+i+k),14] <- subMom*subSun
    } 
    
    frame1_1[(19+k),14] <- sum(as.numeric(frame1_1[(13+k):(17+k),14])) # AUC
  }
  
})

drug_p1 <- as.data.frame(drug_p1)
colnames(drug_p1) <- NULL
write_xlsx(drug_p1, "E:/R/PANC 2nd/Drug/basic/test.xlsx", skip = 1)
