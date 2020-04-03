rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(stringr)

throw <- function(x,y) {
  get(paste0(x,y))
}

src_dir <- c('E:/R/PANC 2nd/sort_Reactom/by pathway')
src_file <- list.files(src_dir, pattern = "-clinvar-add")   #불러올 파일들 리스트화
src_file_lnc <- length(src_file)

for ( i in 1:src_file_lnc) {
  assign(paste0("sort_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))
  sorted <- subset(A, select = c(Gene_Name,Effect,ClinVar), sort=F)
  
  name <- gsub("-clinva-add.csv","",src_file[i])
  
  write.csv(NEW, file = paste0("E:/R/PANC 2nd/sort_Reactom/by pathway/Clinvar-added summarize/",name,"-clinvar-summary.csv"), row.names = FALSE)
}