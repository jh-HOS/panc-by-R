rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

throw <- function(x,y) {
  get(paste0(x,y))
}

library(stringr)
library(dplyr)

src_dir <- c('E:/R/PANC 2nd/sort_2 HER2 oe')
src_file_g <- list.files(src_dir, pattern = ".txt")   #불러올 pathway 별 유전자 리스트
src_file_lnc_g <- length(src_file_g)

for ( j in 1:src_file_lnc_g) {
  lists <- src_file_g[j]
  lists <- gsub(".txt","",lists)
  
  assign(paste0("gene_",j),
         read.table(paste0("E:/R/PANC 2nd/sort_2 KRAS/",lists,".txt"), header=F, sep="\t")) #유전자 리스트
  
  listss <- throw("gene_",j)
  colnames(listss) <- "Gene_Name"
  
  src_file <- list.files(paste0(src_dir,"/2. summarize"), pattern = paste0("summarize-",lists))   #불러올 cell line 별 파일 리스트화
  src_file_lnc <- length(src_file)
  
  for (i in 1:src_file_lnc){
    A <- assign(paste0("NGS_",i),
                read.csv(paste0(src_dir, "/2. summarize/", src_file[i]),
                         stringsAsFactors = T,
                         na.strings = NA,
                         header = T))   #csv로 읽어오기
    
    B <- assign(paste0("tbl_",i),subset(A, select = c(Gene_Name, Effect), sort=F)) #돌연변이 칼럼 2개만 sorting
    B <- unique(B)
    
    D <- lists
    subname <- gsub(D,"",src_file[i])
    subname <- gsub("-.csv","", subname)
    subname <- gsub("summarize-","", subname)
    subname <- gsub("-","",subname)
    #앞에 숫자 제거 밑 제목부여
    
    colnames(B) <- c("Gene_Name", subname)
    B[is.na(B)]<-"" #NA value 공란 치환
    
    if (i == 1) {
      assign(paste0("total_",i), merge(x=listss, y = B , by="Gene_Name", all = T))
    } else {
      assign(paste0("total_",i), merge(x= get(paste0("total_",i-1)), y = B , by="Gene_Name", all = T))
    }
    
    if (i == src_file_lnc) {
      samples <- throw("total_",src_file_lnc)
      write.csv(unique(samples), file=paste0("E:/R/PANC 2nd/sort_2 HER2 oe/3. results/",lists,"-total.csv"), row.names=F)
    }
  }
}

sum(is.na(samples[1,]))
