rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(stringr)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/sort_Reactom/by pathway')
src_file_g <- list.files(src_dir, pattern = ".tsv")   #불러올 파일들 리스트화
src_file_lnc_g <- length(src_file_g)

throw <- function(x,y) {
  get(paste0(x,y))
}

for ( j in 1:src_file_lnc_g) {
  lists <- src_file_g[j]
  lists <- gsub(".tsv","",lists)
  assign(paste0("gene_",j),
         read.table(paste0("E:/R/PANC 2nd/sort_Reactom/by pathway/",lists,".tsv"), header=F, sep="\t"))
  listss <- throw("gene_",j)
  colnames(listss) <- "Gene_Name"
  
  src_file <- list.files(src_dir, pattern = paste0("Summarize-",lists,"-SNU"))   #불러올 파일들 리스트화
  src_file_lnc <- length(src_file)
  
  for (i in 1:src_file_lnc){

    A <- assign(paste0("NGS_",i),
                read.csv(paste0(src_dir, "/", src_file[i]),
                         stringsAsFactors = T,
                         na.strings = NA,
                         header = T))   #csv로 읽어오기
    
    B <- assign(paste0("tbl_",i),subset(A, select = c(Gene_Name, Effect), sort=F)) #돌연변이 칼럼 2개만 sorting
    B <- unique(B)
    
    D <- paste0(lists,"-")
    subname <- gsub(D,"",src_file[i])
    subname <- gsub(".csv","", subname)
    subname <- gsub("Summarize-","", subname)
    #앞에 숫자 제거 밑 제목부여
    
    colnames(B) <- c("Gene_Name", subname)
    
    B[is.na(B)]<-""
    
    if (i == 1) {
      assign(paste0("total_",i), merge(x=listss, y = B , by="Gene_Name", all = T))
    } else {
      assign(paste0("total_",i), merge(x= get(paste0("total_",i-1)), y = B , by="Gene_Name", all = T))
    }
    
    if (i == src_file_lnc) {
      samples <- throw("total_",src_file_lnc)
      write.csv(unique(samples), file=paste0("E:/R/PANC 2nd/sort_Reactom/",lists,"-total.csv"), row.names=F)
    }
  }
}

