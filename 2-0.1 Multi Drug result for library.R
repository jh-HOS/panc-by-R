rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(tidyr)
library(readxl)
library(writexl)

try(
  {src_dir <- c('E:/R/PANC 2nd/Drug/191227 결과정리/')  #불러올 파일들이 있는 위치 설정
  src_file <- list.files(src_dir,pattern = "*.xlsx") 
  src_file_lnc <- length(src_file)
  save_loca <- "E:/R/PANC 2nd/Drug/기존 서식 결과 복붙/191227/"
  
  cal.AUC <- function(x) {
    AUC = (colSums(x[,1:9], na.rm = T) + colSums(x[2:5,1:9], na.rm = T))*log10(3)*3/(2*sum(x[1:3,10]))
    AUC = t(AUC)
  }
  
  p1 <- read.csv("E:/R/PANC 2nd/Drug/basic/원래 결과 틀 p1.csv", header=F, sep=",")
  p2 <- read.csv("E:/R/PANC 2nd/Drug/basic/원래 결과 틀 p2.csv", header=F, sep=",")
  
  drug_p1 <- as.matrix(p1)
  drug_p2 <- as.matrix(p2)}
)

for (t in 1:src_file_lnc){
  print(t)
  print(src_file[t])
  filename <- gsub(".xlsx","",src_file[t])
  
  assign("frame1", drug_p1)
  assign("frame2", drug_p2)
  
  raw_tbl <- read_excel(paste0(src_dir, "/", src_file[t]),sheet=1,range="C12:L17",col_names = F,col_types = "numeric", na="NA")
  
  tbl <- as.matrix(raw_tbl)
  raw.AUC <- as.matrix(cal.AUC(tbl))
  length(grep("p1",src_file[t]))
  
  #if insert 검증
  if (length(grep("p1",src_file[t])) != 0) {
    frame1[8,2:10] <- raw.AUC
    frame1[2:7,2:11] <- tbl
    #drug1 line
    for ( j in 0:4) {
      k <- j*11
      frame1[(13+k):(18+k),4] <- tbl[1:6,(1+j*2)] # y value
      frame1[(13+k),5] <- mean(tbl[1:3,10]) #control value
      frame1[(13+k):(18+k),6] <- as.numeric(frame1[(13+k):(18+k),4])/mean(tbl[1:3,10])  # normalized y
      for ( i in 0:4) {
        # AUC segment
        subSun <- as.numeric(frame1[(14+i+k),3])-as.numeric(frame1[(13+i+k),3])
        subMom <- (as.numeric(frame1[(14+i+k),6])+as.numeric(frame1[(13+i+k),6]))/2
        frame1[(13+i+k),7] <- subMom*subSun
      } 
      
      frame1[(19+k),7] <- sum(as.numeric(frame1[(13+k):(17+k),7])) # AUC
    }
    #drug2 line
    for ( j in 0:3) {
      k <- j*11
      frame1[(13+k):(18+k),11] <- tbl[1:6,(2+j*2)] # y value
      frame1[(13+k),12] <- mean(tbl[1:3,10]) #control value
      frame1[(13+k):(18+k),13] <- as.numeric(frame1[(13+k):(18+k),11])/mean(tbl[1:3,10])  # normalized y
      for ( i in 0:4) {
        # AUC segment
        subSun <- as.numeric(frame1[(14+i+k),10])-as.numeric(frame1[(13+i+k),10]) # x axis length
        subMom <- (as.numeric(frame1[(14+i+k),13])+as.numeric(frame1[(13+i+k),13]))/2 # normalized y mean value
        frame1[(13+i+k),14] <- subMom*subSun
      } 
      
      frame1[(19+k),14] <- sum(as.numeric(frame1[(13+k):(17+k),14])) # AUC
    }
    
    frame1 <- as.data.frame(frame1)
    colnames(frame1) <- NULL
    write_xlsx(frame1, paste0(save_loca,src_file[t]))
#----------------------------------------------------------------------------------------------------    
  } else if (length(grep("p2",src_file[t])) != 0) {
    frame2[8,2:10] <- raw.AUC
    frame2[2:7,2:11] <- tbl
    
    #drug1 line
    for ( j in 0:4) {
      k <- j*11
      frame2[(13+k):(18+k),4] <- tbl[1:6,(1+j*2)] # y value
      frame2[(13+k),5] <- mean(tbl[1:3,10]) #control value
      frame2[(13+k):(18+k),6] <- as.numeric(frame2[(13+k):(18+k),4])/mean(tbl[1:3,10])  # normalized y
      for ( i in 0:4) {
        # AUC segment
        subSun <- as.numeric(frame2[(14+i+k),3])-as.numeric(frame2[(13+i+k),3])
        subMom <- (as.numeric(frame2[(14+i+k),6])+as.numeric(frame2[(13+i+k),6]))/2
        frame2[(13+i+k),7] <- subMom*subSun
      } 
      
      frame2[(19+k),7] <- sum(as.numeric(frame2[(13+k):(17+k),7])) # AUC
    }
    #drug2 line
    for ( j in 0:3) {
      k <- j*11
      frame2[(13+k):(18+k),11] <- tbl[1:6,(2+j*2)] # y value
      frame2[(13+k),12] <- mean(tbl[1:3,10]) #control value
      frame2[(13+k):(18+k),13] <- as.numeric(frame2[(13+k):(18+k),11])/mean(tbl[1:3,10])  # normalized y
      for ( i in 0:4) {
        # AUC segment
        subSun <- as.numeric(frame2[(14+i+k),10])-as.numeric(frame2[(13+i+k),10]) # x axis length
        subMom <- (as.numeric(frame2[(14+i+k),13])+as.numeric(frame2[(13+i+k),13]))/2 # normalized y mean value
        frame2[(13+i+k),14] <- subMom*subSun
      } 
      
      frame2[(19+k),14] <- sum(as.numeric(frame2[(13+k):(17+k),14])) # AUC
    }
    
    frame2 <- as.data.frame(frame2)
    colnames(frame2) <- NULL
    write_xlsx(frame2, paste0(save_loca,src_file[t]))
  } else {
    print("파일이름이 이상한디?")
  }
}
