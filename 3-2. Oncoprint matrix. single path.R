rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(stringr)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/to total/census/total')
src_file <- list.files(src_dir, pattern = "csv")   #불러올 cell line 별 파일 리스트화
src_file_lnc <- length(src_file)

throw <- function(x,y) {
  get(paste0(x,y))
}

Genelsit <- read.table(paste0(src_dir,"/backbone.txt"), header=F, sep="\t")
colnames(Genelsit) <- "Gene_Name"

for (i in 1:src_file_lnc){
  A <- assign(paste0("NGS_",i),
              read.csv(paste0(src_dir, "/", src_file[i]),
                       stringsAsFactors = T,
                       na.strings = NA,
                       header = T))   #csv로 읽어오기
  
  B <- assign(paste0("tbl_",i),subset(A, select = c(Gene_Name, Effect), sort=F)) 

  subname <- gsub("SNU","SNU-",src_file[i])
  subname <- gsub(".csv","",subname)
  #앞에 숫자 제거 밑 제목부여
  
  colnames(B) <- c("Gene_Name", subname)
  B[is.na(B)]<-"" #NA value 공란 치환
  
  if (i == 1) {
    assign(paste0("total_",i), merge(x=Genelsit, y = B , by="Gene_Name", all = T))
  } else {
    assign(paste0("total_",i), merge(x= get(paste0("total_",i-1)), y = B , by="Gene_Name", all = T))
  }
  
  if (i == src_file_lnc) {
    samples <- throw("total_",src_file_lnc)
    write.csv(unique(samples), file=paste0(src_dir,"/Total.csv"), row.names=F)
  }
}
