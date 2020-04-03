rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(tidyr)
library(readxl)
library(org.Hs.eg.db)
library(ReactomePA)
library(clusterProfiler)

try(
  {src_dir <- c('E:/R/PANC 2nd/sort_1')
  src_file <- list.files(src_dir)     #불러올 파일들 리스트화
  src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정
  df_file <- as.data.frame(src_file, stringAsFator = F)
  Entrz <- org.Hs.eg.db
  input_table <- df_file %>%
    separate(src_file, c("no_name", "genes"), "_")}
)

try(
  {for (i in 1:src_file_lnc){
    #sorted gene data 불러오기
    tbl <- assign(paste0("tbl_", i),
                     read.csv(paste0(src_dir, "/", src_file[i]),
                              stringsAsFactors = T,
                              na.strings = NA,header = T))
    #유전자부분만 추출(Column만 추출)
    noeng <- gsub("`", "", tbl$Gene_Name)
    #Enrz ID
    resultdata <- select(Entrz, keys = as.vector(noeng), columns = c("ENTREZID", "SYMBOL"), keytype = "SYMBOL")
    
    colnames(resultdata) <- c("","Gene_Name")
    
    if (i == 1) {
      assign(paste0("resultbl_",i), resultdata)
    } else if (i > 1) {
      assign(paste0("resultbl_",i), rbind(get(paste0("resultbl_",i-1)), resultdata))
    }
    print(i)
    print(src_file[i])
  }
})
