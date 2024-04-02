rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

src_dir <- c('E:/R/PANC 2nd/sort_2 R PA')  #불러올 파일들이 있는 directory 설정
src_file <- list.files(src_dir, pattern = "-Cuted.csv")     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정

src_dir_g <- c('E:/R/PANC 2nd/sort_2 HER2 oe')  #불러올 파일들이 있는 directory 설정
src_file_g <- list.files(src_dir_g, pattern = ".txt")     #불러올 파일들 리스트화
src_file_lnc_g <- length(src_file_g)

throw <- function(x,y) {
  get(paste0(x,y))
}

for (i in 1:src_file_lnc){
  assign(paste0("NGS_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))   #csv로 읽어오기
  
  name <- gsub("Cuted.csv","",src_file[i])
  
  for (j in 1:src_file_lnc_g) {
    assign(paste0("gene_",j),
           read.table(paste0(src_dir_g,"/",src_file_g[j]),
                      header = F,
                      sep = "\t"))
    
    assign(paste0("trans_",j), t(as.vector(get(paste0("gene_", j)))))
    
    assign(paste0("collap_",j), paste(throw("trans_",j), collapse = "$|^"))
    
    assign(paste0("list_",j),
           grep(paste0("^",throw("collap_",j),"$"),throw("NGS_",i)$Gene_Name))

    assign(paste0("m.list",j),
           sort(as.matrix(get(paste0("list_",j)))))
    
    assign(paste0("Col_",j),
           get(paste0("NGS_", i))[get(paste0("m.list",j)),])
           
    assign(paste0("uniq_",j),
           get(paste0("Col_",j))[!duplicated(get(paste0("Col_",j))$POS),1:19])
    
    savename <- gsub(".txt","",src_file_g[j])
    savename <- gsub("[","",savename, fixed = TRUE, ignore.case = TRUE)
    savename <- gsub("]","",savename, fixed = TRUE, ignore.case = TRUE)
    
    assign(paste0("name_",j),
           paste0("E:/R/PANC 2nd/sort_2 HER2 oe/1. sorted/", savename,"-",name,".csv"))

    write.csv(get(paste0("uniq_",j)), get(paste0("name_",j)), row.names=FALSE)
    
    print(j)
    print(src_file_g[j])
  }
  print(i)
  print(src_file[i])
}
