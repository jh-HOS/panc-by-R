rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(data.table)
library(stringr)

throw <- function(x,y) {
  get(paste0(x,y))
}


celllines <- c("2729B1","2822","2913","2918","2982-1","3139","3294","3375","3573","3608","3615","3752","3923T","4208T","4223","4305T","4340T","4354T","4405","4482","4492","4525","4733","4771","4866","5177")

celllines <- c("2822","2913","2918","2982-1","3139","3294","3375","3573","3608","3615","3752","3923T","4208T","4223","4305T","4340T","4354T","4405","4482","4492","4525","4733","4771","4866","5177")


for ( j in celllines) {
  
  SA <- paste0(j,".csv")
  
  src_dir <- c('E:/R/PANC 2nd/sort_3')  #불러올 파일들이 있는 directory 설정
  src_file <- list.files(src_dir, pattern = SA)     #불러올 파일들 리스트화
  src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정
  
  VCF <- fread(paste0("E:/R/PANC 2nd/sort_Reactom/4. by cell line/VCF.csv/SNU-",j," VCF.csv"), stringsAsFactors = T, na.strings=getOption("datatable.na.strings","NA"), header = T)
  
  for (i in 1:src_file_lnc) {
    assign(paste0("NGS_",i),
           fread(paste0(src_dir, "/", src_file[i]),
                 stringsAsFactors = T,
                 na.strings=getOption("datatable.na.strings","NA"),
                 header = T))   #csv로 읽어오기
    
    
    sorted <- subset(throw("NGS_",i), select = c(CHROM, POS, ID, REF, ALT, QUAL), sort=F)
    
    NGS_df <- select(throw("NGS_",i),POS)
    
    NGS_V <- as.vector(t(NGS_df))
    
    n <- length(NGS_V)
    
    NGS_FILTER <- data.frame(matrix(nrow=0, ncol=n))  
    NGS_INFO <- data.frame(matrix(nrow=0, ncol=n))
    NGS_FORMAT <- data.frame(matrix(nrow=0, ncol=n))
    NGS_10data <- data.frame(matrix(nrow=0, ncol=n))
    
    for ( k in 1:n) {
      searchName <- NGS_V[k]
      searchName
      SearchResults <- grep(paste0("^",searchName,"$"), VCF$POS)
      
      length(SearchResults) == 1
      if (length(SearchResults) == 1 ) {
        NGS_FILTER[1,k] <- as.vector(VCF$FILTER[SearchResults])
        NGS_INFO[1,k] <- as.vector(VCF$INFO[SearchResults])
        NGS_FORMAT[1,k] <- as.vector(VCF$FORMAT[SearchResults])
        NGS_10data[1,k] <- as.vector(VCF[SearchResults,10][[1]])
        
      } else if ( length(SearchResults) > 1 ) {
        print(paste0(j," 얘 개많오"))
        SearchResults <- SearchResults[1]
        NGS_FILTER[1,k] <- as.vector(VCF$FILTER[SearchResults])
        NGS_INFO[1,k] <- as.vector(VCF$INFO[SearchResults])
        NGS_FORMAT[1,k] <- as.vector(VCF$FORMAT[SearchResults])
        NGS_10data[1,k] <- as.vector(VCF[SearchResults,10][[1]])
      } else if ( length(SearchResults) == 0 ) {
        print(paste0(j,"VCF 불일치"))
        NGS_FILTER[1,k] <- "NOPE"
        NGS_INFO[1,k] <- "NOPE"
        NGS_FORMAT[1,k] <- "NOPE"
        NGS_10data[1,k] <- "NOPE"
      }
    }
    
    newCol1 <-  t(NGS_FILTER)
    colnames(newCol1) <- "FILTER"
    
    newCol2 <-  t(NGS_INFO)
    colnames(newCol2) <- "INFO"
    
    newCol3 <-  t(NGS_FORMAT)
    colnames(newCol3) <- "FORMAT"
    
    newCol4 <-  t(NGS_10data)
    colnames(newCol4) <- paste0("SNU-",j)
    
    
    sorteded <- cbind(sorted, newCol1)
    sorteded <- cbind(sorteded, newCol2)
    sorteded <- cbind(sorteded, newCol3)
    sorteded <- cbind(sorteded, newCol4)
    names(sorteded)[1] <- c("#CHROM")
    
    name <- gsub(".csv","",src_file[i])
    
    write.csv(sorteded, file = paste0("E:/R/PANC 2nd/sort_5/",name," preVCF.csv"), row.names = FALSE)
  }
}

