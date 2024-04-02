rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(data.table)

celllines <- c("2729B1","2822","2913","2918","3139","3294","3375","3573","3608","3615","3752","4223","4405","4482","4492")

for ( j in celllines) {
  
  src_dir <- c('E:/R/PANC 2nd/sort_Reactom/1. by pathway')  #불러올 파일들이 있는 directory 설정
  src_file <- list.files(src_dir, pattern = j)     #불러올 파일들 리스트화
  src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정
  
  for (i in 1:src_file_lnc) {
    assign(paste0("NGS_",i),
           fread(paste0(src_dir, "/", src_file[i]),
                 stringsAsFactors = T,
                 na.strings=getOption("datatable.na.strings","NA"),
                 header = T))   #csv로 읽어오기
    
    if (i == 1) {
      assign(paste0("total_",i), NGS_1)
    } else {
      assign(paste0("total_",i), rbind(get(paste0("total_",i-1)), get(paste0("NGS_",i))))
    }
    if (i == src_file_lnc) {
      write.csv(get(paste0("total_",i)), file=paste0("E:/R/PANC 2nd/sort_Reactom/ClinVar library.csv"), row.names=F)
    }
  }
}


