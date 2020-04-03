rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(stringr)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/sort_1')  #불러올 파일들이 있는 directory 설정
src_file <- list.files(src_dir)     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정

gene <-read.table("E:/R/PANC 2nd/Mutation_clear.txt", header=T, sep="\t")

A <-  read.csv("E:/R/PANC 2nd/sort_1/1_SNU2729B1.csv",stringsAsFactors = T,na.strings = NA, header = T)  #csv로 읽어오기
  
A$Gene_Name <- substr(A$Gene_Name, 2, 100) 
  
B <- subset(A, select = c(Gene_Name,Effect), sort=F)
B <- unique(B)
  
name <- substr("1_SNU2729B1.csv",3 ,100) 
  
colnames(B) <- c("Gene_Name", name)
  
assign(paste0("sort_",i),)
  
  
gene <- merge(x=gene, y=get(paste0("sort_",i)), by="Gene_Name", all = TRUE)
  

write.csv(unique(gene), file="E:/R/PANC 2nd/191129 oncomatrix.csv", row.names=F)

