rm(list=ls()) #���� ����� Data�� value ��� �����ϱ�.
library(data.table) # ������ install
library(arules)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/sort_Reactom/by pathway')  #���ϵ��� �ִ� directory ����
src_file <- list.files(src_dir, pattern = "csv")  #�ҷ��� ���ϵ� ����Ʈȭ pattern = ""�� csv ���ϸ� �ҷ����� ����
src_file_lnc <- length(src_file)  # ���ϵ��� ���� ����

throw <- function(x,y) {
  get(paste0(x,y))
}

for (i in 1:src_file_lnc) {
  print(i) #���° ����
  assign(paste0("NGS_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))   #csv�� �о����
  
  NGS_df <- select(throw("NGS_",i),Gene_Name, Effect) #Gene_Name Effect ����
  
  NGS_new <- data.table() #����� ���� �� table
  
  n <- nrow(NGS_df) #�ݺ��� ������ Ƚ�� ���� 
  
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