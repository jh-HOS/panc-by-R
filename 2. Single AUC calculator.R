rm(list=ls()) #���� ����� Data�� value ��� �����ϱ�.

library(tidyr)
library(readxl)

try( #AUC��� �� �麸�弳��
  {cal.AUC <- function(x) {
    AUC = (colSums(x[,1:9], na.rm = T) + colSums(x[2:5,1:9], na.rm = T))*log10(3)*3/(2*sum(x[1:3,10]))
    AUC = t(AUC)
  }
  
  p1 <- read.table("E:/R/PANC 2nd/Drug/�⺻/panc_p1.txt")
  p2 <- read.table("E:/R/PANC 2nd/Drug/�⺻/panc_p2.txt")
  
  drug_p1 <- as.matrix(p1)
  drug_p2 <- as.matrix(p2)}
)

try({
  raw_tbl <- read_excel("E:/R/PANC 2nd/Drug/191227 �������/3923T p1.xlsx",
                        sheet=1, range="C12:L17",
                        col_names = F, col_types = "numeric", na="NA")
  
  tbl <- as.matrix(raw_tbl)
  raw.AUC <- as.matrix(cal.AUC(tbl))
  ctl <- c(mean(tbl[1:3,10]),mean(tbl[4:6,10]))
  ratio <- mean(tbl[4:6,10])/mean(tbl[1:3,10])
  raw.ctl <- as.matrix(t(ctl))
  row.cell <- cbind(raw.AUC,raw.ctl)
  print(row.cell)
  print(ratio)
})

