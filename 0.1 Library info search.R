rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(data.table)
library(stringr)

throw <- function(x,y) {
  get(paste0(x,y))
}

src_dir <- c('E:/R/PANC 2nd/raw_No modifier/')
src_file <- list.files(src_dir, pattern = "csv")   #불러올 파일들 리스트화
src_file_lnc <- length(src_file)

Census <- read.csv("E:/R/PANC 2nd/Census-added library_20200604.csv", stringsAsFactors = T, na.strings = NA, header = T)

for (i in 1:src_file_lnc) {
    assign(paste0("NGS_",i),
           fread(paste0(src_dir, "/", src_file[i]),
                 stringsAsFactors = T,
                 na.strings=getOption("datatable.na.strings","NA"),
                 header = T))   #csv로 읽어오기
    
    
    sorted <- throw("NGS_",i)
    
    sorted$POS <- gsub(",","",sorted$POS)
    sorted$CHROM <- gsub("chr","",sorted$CHROM)
    
    NGS_df <- select(sorted,POS)
    NGS_V <- as.vector(t(NGS_df))
    
    n <- length(NGS_V)
    
    NGS_CV <- data.frame(matrix(nrow=0, ncol=n))  
    NGS_CGC <- data.frame(matrix(nrow=0, ncol=n))

    for ( k in 1:n) {
      searchName <- NGS_V[k]
      searchName
      SearchResults <- grep(paste0("^",searchName,"$"), Census$POS)
      if (length(SearchResults) == 1 ) {
        NGS_CV[1,k] <- as.vector(Census$ClinVar[SearchResults])
        NGS_CGC[1,k] <- as.vector(Census$CancerGeneCensus[SearchResults])
      } else {
        NGS_CV[1,k] <- "NOPE"
        NGS_CGC[1,k] <- "NOPE"
      }
    }
    
    newCol1 <-  t(NGS_CV)
    colnames(newCol1) <- "ClinVar"
    
    newCol2 <-  t(NGS_CGC)
    colnames(newCol2) <- "CancerGeneCensus"
    
    sorteded <- cbind(sorted, newCol1)
    sorteded <- cbind(sorteded, newCol2)
    
    name <- gsub("_No modifier.csv","",src_file[i])
    
    write.csv(sorteded, file = paste0("E:/R/PANC 2nd/sort_1/",name,"-Indexed.csv"), row.names = FALSE)
}    

