rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)

src_dir <- c('E:/R/PANC 2nd/ReactomePA _ SORT mutation/Origin')  #불러올 파일들이 있는 directory 설정
src_file <- list.files(src_dir, pattern = ".tsv")     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정

for (i in 1:src_file_lnc){
  assign(paste0("GL_",i),
         read.table(paste0(src_dir, "/", src_file[i]),
                  header=T, sep="\t",stringsAsFactors = T, na.strings = NA))   #csv로 읽어오기
  
  GNs<- as.vector(get(paste0("GL_", i))$MoleculeName)
  
  SP <- strsplit(GNs, " ")
  
  DFSP <- as.data.frame(SP)
  
  DF <- as.data.frame(t(DFSP))
  
  A <- select(DF, V2)
  colnames(A) <- NULL
  row.names(A) <- NULL
  
  assign(paste0("name_", i),paste0("E:/R/PANC 2nd/ReactomePA _ SORT mutation/",gsub(".tsv", "",src_file[i]),"-GeneOnly.tsv"))

  write.table(A, get(paste0("name_",i )), row.names = FALSE)
  
  print(i)
  print(src_file[i])
}
