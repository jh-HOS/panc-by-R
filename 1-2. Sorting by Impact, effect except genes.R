rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(data.table)

src_dir <- c('E:/R/PANC 2nd/raw_1')  #불러올 파일들이 있는 directory 설정
src_file <- list.files(src_dir, pattern = ".csv")     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정


for (i in 1:src_file_lnc){
  assign(paste0("NGS_",i),
         fread(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings=getOption("datatable.na.strings","NA"),
                  header = T))   #csv로 읽어오기
  
  assign(paste0("Col_", i),filter(get(paste0("NGS_",i)),
                                  Impact != "MODIFIER", Impact != "LOW"))
  
  Genename <- get(paste0("Col_",i))[!duplicated(get(paste0("Col_",i))$ID),1:19]#중복된 position 제거
  
  Genename$Gene_Name <- gsub("`", "", Genename$Gene_Name)
  
  filename <- gsub("_effect.csv", "",src_file[i])
  
  assign(paste0("name_",i),
         paste0("E:/R/PANC 2nd/raw_No modifier/", filename,"_No modifier.csv" ))
  
  write.csv(Genename, get(paste0("name_",i)), row.names=FALSE) #uniq-i로 명명한 파일을 위의 이름순서대로 csv 저장
  
  print(i)
  print(src_file[i])
}
  
