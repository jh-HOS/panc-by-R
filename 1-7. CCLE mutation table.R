rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(stringr)

throw <- function(x,y) {
  get(paste0(x,y))
}

src_dir <- c('E:/R/PANC 2nd/to total/census')
src_file <- list.files(src_dir, pattern = "csv")   #불러올 파일들 리스트화
src_file_lnc <- length(src_file)

for ( i in 1:src_file_lnc) {
  #Cell line별 파일 불러오기
  assign(paste0("sort_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))
  
  vec <- throw("sort_",i)

  names(vec) <- c("CHROM",	"POS",	"ID",	"REF",	"ALT",	"DP",	"Gene_Name",	"HGVS.c",	"HGVS.p",	"ClinVar",	"CancerGeneCensus")
  
  sorted <- subset(vec, select = c(Gene_Name,HGVS.c,HGVS.p,ClinVar), sort=F)
  
  sorted$HGVS.p <- gsub("`","",sorted$HGVS.p, fixed = TRUE, ignore.case = TRUE)
  
  NGS_df <- select(sorted,Gene_Name) #Gene_Name 추출
  NGS.C <- select(sorted,HGVS.c)
  NGS.P <- select(sorted,HGVS.p)
  NGS.CV <- select(sorted,ClinVar)

  NGS_V <- as.vector(t(NGS_df))
  NGS_C <- as.vector(t(NGS.C))
  NGS_P <- as.vector(t(NGS.P))
  NGS_CV <- as.vector(t(NGS.CV))
  
  n <- length(NGS_V) #반복을 실행할 횟수 설정 
  
  NGS_new <- data.frame(matrix(nrow=0, ncol=n*3))   #결과를 넣을 빈 table
  NGS_name <- data.frame(matrix(nrow=0, ncol=n*3))
  
  for ( j in 1:n) {
    NGS_name[1,j*3-2] <- NGS_V[j]
    NGS_new[1,j*3-2] <- NGS_C[j]
    NGS_new[1,j*3-1] <- NGS_P[j]
    NGS_new[1,j*3] <- NGS_CV[j]
  }
  #Census 결과 새로운 column에 추가
  newCol <-  t(NGS_new)
  nameCol <- t(NGS_name)

  NEW <- cbind(nameCol, newCol)
  NEW <- as.data.frame(NEW)
  
  if (length(grep("DNALINK",src_file[i]) == 1)) {
    name <- gsub("-DNALINK-CSC.csv","",src_file[i])
  } else {
    name <- gsub("-MACROGEN-CSC.csv","",src_file[i])
  }

  names(NEW) <- c("Gene_Name", name)
  
  write.csv(NEW, file = paste0("E:/R/PANC 2nd/to total/census/CCLE form/",name,"-Mutaton.csv"), row.names = FALSE)
}


