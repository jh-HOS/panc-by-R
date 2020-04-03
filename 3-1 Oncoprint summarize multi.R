rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.
library(data.table) # 없으면 install
library(arules)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/sort_Reactom/by pathway')  #파일들이 있는 directory 설정
src_file <- list.files(src_dir, pattern = "csv")  #불러올 파일들 리스트화 pattern = ""은 csv 파일만 불러오게 설정
src_file_lnc <- length(src_file)  # 파일들의 길이 설정

throw <- function(x,y) {
  get(paste0(x,y))
}

for (i in 1:src_file_lnc) {
  print(i) #몇번째 파일
  assign(paste0("NGS_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))   #csv로 읽어오기
  
  NGS_df <- select(throw("NGS_",i),Gene_Name, Effect) #Gene_Name Effect 추출
  
  NGS_new <- data.table() #결과를 넣을 빈 table
  
  n <- nrow(NGS_df) #반복을 실행할 횟수 설정 
  
  for (j in 1:n) {
    name_index <- as.character(NGS_df[j, 1])
    
    item_index <- as.character(NGS_df[j, 2])
    
    item_index_split_temp <- data.frame(strsplit(item_index, split = '&'))
    
    mart_temp <- data.frame(cbind(name_index, item_index_split_temp))
    
    names(mart_temp) <- c("Gene_Name", "Effect")
    
    NGS_new <- rbind(NGS_new, mart_temp)
  }
  
  NGS_new <- as(NGS_new, "data.frame")
  
  NGS_tr <- as(split(NGS_new[,"Effect"],NGS_new[,"Gene_Name"]), "transactions")
  
  NGS_summ <- as(NGS_tr, "data.frame")
  
  NGS_summ$items <- gsub("}","",NGS_summ$items, fixed = TRUE, ignore.case = TRUE)
  NGS_summ$items <- gsub("{","",NGS_summ$items, fixed = TRUE, ignore.case = TRUE)
  NGS_summ$items <- gsub(",",";",NGS_summ$items, fixed = TRUE, ignore.case = TRUE)
  
  NGS_summ <- select(NGS_summ, transactionID, items)
  
  colnames(NGS_summ) <- c("Gene_Name", "Effect")
  
  subname <- gsub(".csv","", src_file[i])

  write.csv(NGS_summ, file=paste0("E:/R/PANC 2nd/sort_Reactom/by pathway/","Summarize-",subname,".csv"), row.names=F)
}
