rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(stringr)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/sort_1')  #불러올 파일들이 있는 directory 설정
src_file <- list.files(src_dir)     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정

Total <-  data.frame(Sample_ID = c(""), Chromosome=c(""), Start_position=c(""), Reference_Allele=c(""), Variant_Allele=c(""),stringsAsFactors=FALSE)

for (i in 1:src_file_lnc){
  A <- assign(paste0("NGS_",i),
              read.csv(paste0(src_dir, "/", src_file[i]),
                       stringsAsFactors = T,
                       na.strings = NA,
                       header = T))   #csv로 읽어오기
  
  name <- assign(paste0("name_", i), substr(src_file[i],3 ,100)) #앞에 숫자 제거 밑 제목부여
  
  A <- transform(A, Sample_ID = name)
  
  B <- assign(paste0("tbl_",i),subset(A, select = c(Sample_ID, CHROM, POS, REF, ALT), sort=F)) #돌연변이 칼럼 2개만 sorting
  
  colnames(B) <- c("Sample_ID", "Chromosome", "Start_position", "Reference_Allele", "Variant_Allele")
  
  assign(paste0("sort_",i),B)
  
  Total <-rbind(Total, get(paste0("sort_", i)))
  
}

write.csv(Total, file="E:/R/PANC 2nd/191202 lollipop.csv", row.names=F)
