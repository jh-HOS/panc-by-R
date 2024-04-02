rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(dplyr)
library(stringr)

throw <- function(x,y) {
  get(paste0(x,y))
}

src_dir <- c('E:/R/PANC 2nd/sort_Reactom/Census-added')
src_file <- list.files(src_dir, pattern = "-Census-added.csv")   #불러올 파일들 리스트화
src_file_lnc <- length(src_file)

for ( i in 1:src_file_lnc) {
  #Cell line별 파일 불러오기
  assign(paste0("NGS_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))
  
  NGS_df <- select(throw("NGS_",i),Gene_Name)
  NGS_V <- as.vector(t(NGS_df))
  
  G_Imp <- select(throw("NGS_",i),Impact)
  G_CV <- select(throw("NGS_",i),ClinVar)
  G_CG <- select(throw("NGS_",i),Cancer.Gene.Census)
  
  n <- length(NGS_V) #반복을 실행할 횟수 설정 
  NGS_new <- data.frame(matrix(nrow=0, ncol=n))
  NGS_neew <- data.frame(matrix(nrow=0, ncol=n))

  for ( j in 1:n ) {
    if ( G_Imp[j,1] == "MODERATE" & G_CV[j,1] == "not reported in ClinVar" & G_CG[j,1] == "No Census") {
      NGS_new[1,j] <- "DEL"
    } else if (G_Imp[j,1] == "MODERATE" & G_CV[j,1] == "No items found" & G_CG[j,1] == "No Census") {
      NGS_new[1,j] <- "DEL"
    } else {
      NGS_new[1,j] <- "O"
    }
  }
  
  name <- gsub("-Census-added.csv","",src_file[i])
  
  newCol <-  t(NGS_neew)
  colnames(newCol) <- "Signif"
  
  NEW <- cbind(throw("NGS_",i),newCol)
  NEEW <-NEW[-grep("DEL",NEW$Signif),1:5]
  
  write.csv(NEEW, file = paste0("E:/R/PANC 2nd/sort_Reactom/Census or reported/Census or reported-",name,".csv"), row.names = FALSE)
  #write.csv(ONlyCensus, file = paste0("E:/R/PANC 2nd/sort_Reactom/Census specific categorized/OnlyCensus-",name,".csv"), row.names = FALSE)
}
