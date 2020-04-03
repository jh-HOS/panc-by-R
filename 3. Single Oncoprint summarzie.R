rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.
library(data.table)
library(arules)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/raw_No modifier')  #불러올 파일들이 있는 directory 설정
src_file <- list.files(src_dir)     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정

throw <- function(x,y) {
  get(paste0(x,y))
}

NGS <- read.csv("E:/R/PANC 2nd/sort_Reactom/by pathway/Participating Molecules R-HSA-73857 RNApol II-GeneOnly-SNU2729B1.csv", header = T, stringsAsFactors = TRUE, na.strings = NA)
NGS_df <- select(NGS, Gene_Name, Effect)
NGS_new <- data.table()

n <- nrow(NGS_df)

for (i in 1:n){
  
  print(i) # to check progress
  
  name_index <- as.character(NGS_df[i, 1])
  
  item_index <- as.character(NGS_df[i, 2])
  
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


