rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(stringr)

throw <- function(x,y) {
  get(paste0(x,y))
}

src_dir <- c('E:/R/PANC 2nd/')
src_file <- list.files(src_dir, pattern = "ClinVar library_20200604.csv")   #불러올 파일들 리스트화
src_file_lnc <- length(src_file)

Census <- read.csv("E:/R/PANC 2nd/Cancer Gene Census _ 20200403.csv", stringsAsFactors = T, na.strings = NA, header = T)

for ( i in 1:src_file_lnc) {
  #Cell line별 파일 불러오기
  assign(paste0("sort_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))
  
  sorted <- subset(throw("sort_",i), select = c(X.CHROM, POS, dbSNP142_ID, REF,ALT, DP, Gene_Name,Effect,Putative_Impact,ClinVar), sort=F)
  
  sorted$ClinVar <- gsub("<U+200B>","",sorted$ClinVar, fixed = TRUE, ignore.case = TRUE)
  
  #benign 제거
  if (length(grep("Benign|benign",sorted$ClinVar)) == 0) {
    #benign 없음
    sorteded <- sorted
  } else {
    #benign 있음
    sorteded <- sorted[-grep("Benign|benign",sorted$ClinVar),] 
  }
  
  NGS_df <- select(sorteded,Gene_Name) #Gene_Name 추출
  NGS_V <- as.vector(t(NGS_df))
  
  n <- length(NGS_V) #반복을 실행할 횟수 설정 
  NGS_new <- data.frame(matrix(nrow=0, ncol=n))   #결과를 넣을 빈 table
  NGS_neew <- data.frame(matrix(nrow=0, ncol=n))   #결과를 넣을 빈 table
  
  if (n == 0) {
    #benign결과만 있는 cell line 분류
    name <- gsub("-clinvar-add.csv","",src_file[i])
    write.csv(NEW, file = paste0("E:/R/PANC 2nd/sort_Reactom/20200514 test/",name,"-Census-added-no-results.csv"), row.names = FALSE)
  } else {
    #유의미한 결과 있는 cell line
    for ( j in 1:n) {
    #
    searchName <- NGS_V[j]
    searchName
    SearchResults <- grep(paste0("^",searchName,"$"), Census$Gene.Symbol)
    if (length(SearchResults) != 0 ) {
      #Census 결과 있는 유전자 
      Answer <- as.vector(Census$Role.in.Cancer[SearchResults])
      NGS_new[1,j] <- Answer
    } else {
      #No Census gene
      Answer <- "No Census"
      NGS_new[1,j] <- Answer
    }
    }
    #Census 결과 새로운 column에 추가
    newCol <-  t(NGS_new)
    colnames(newCol) <- "CancerGeneCensus"
    
    NEW <- cbind(sorteded, newCol)
    name <- gsub("_soreted.csv-clinvar-add.csv","",src_file[i])
    
    write.csv(NEW, file = paste0("E:/R/PANC 2nd/sort_Reactom/20200514 test/",name,"-Census-added.csv"), row.names = FALSE)
  }
}


