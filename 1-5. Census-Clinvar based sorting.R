rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(stringr)
library(data.table)

throw <- function(x,y) {
  get(paste0(x,y))
}

src_dir <- c('E:/R/PANC 2nd/sort_1')
src_file <- list.files(src_dir, pattern = "-Indexed")   #불러올 파일들 리스트화
src_file_lnc <- length(src_file)

for ( i in 1:src_file_lnc) {
  #Cell line별 파일 불러오기
  assign(paste0("NGS_",i),
         fread(paste0(src_dir, "/", src_file[i]),
               stringsAsFactors = T,
               na.strings=getOption("datatable.na.strings","NA"),
               header = T))   #csv로 읽어오기
  
  NGS_df <- select(throw("NGS_",i),Gene_Name)
  NGS_V <- as.vector(t(NGS_df))
  
  G_Imp <- select(throw("NGS_",i),Impact)
  G_CV <- select(throw("NGS_",i),ClinVar)
  G_CG <- select(throw("NGS_",i),CancerGeneCensus)
  
  n <- length(NGS_V) #반복을 실행할 횟수 설정 
  
  NGS_neew <- data.frame(matrix(nrow=0, ncol=n))
  
  for ( j in 1:n ) {
    if (G_CV[j,1] == "not reported in ClinVar" & G_CG[j,1] == "No Census") {
      NGS_neew[1,j] <- "DEL"
    } else if ( G_CV[j,1] == "No items found" & G_CG[j,1] == "No Census") {
      NGS_neew[1,j] <- "DEL"
    } else if ( G_CV[j,1] == "NOPE" & G_CG[j,1] == "NOPE" & G_Imp[j,1] != "HIGH") {
      NGS_neew[1,j] <- "DEL"
    } else {
      NGS_neew[1,j] <- "O"
    }
  }

  newCol <-  t(NGS_neew)
  colnames(newCol) <- "Signif"
  NEW <- cbind(throw("NGS_",i),newCol)
  
  Delete.length <- length(-grep("DEL",NEW$Signif))
  
  print(paste0(i,", ",Delete.length,"개 컷!"))
  
  name <- gsub("-Indexed.csv","",src_file[i])
  
  if (Delete.length != 0) {
    NEEW <-NEW[-grep("DEL",NEW$Signif),1:(ncol(NEW)-1)]
    write.csv(NEEW, paste0("E:/R/PANC 2nd/sort_2/",name,"-Cuted.csv"), row.names = FALSE)
  } else {
    NEEW <- NEW[,1:(ncol(NEW)-1)]
    write.csv(NEEW, paste0("E:/R/PANC 2nd/sort_2/",name,"-Cuted.csv"), row.names = FALSE)
  }
}

