rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(rvest)
library(dplyr)
library(stringr)

throw <- function(x,y) {
  get(paste0(x,y))
}


basic_url <- "https://www.ncbi.nlm.nih.gov/clinvar/?term="
src_dir <- c('E:/R/PANC 2nd/sort_Reactom/by pathway')
src_file <- list.files(src_dir, pattern = ".csv")   #불러올 파일들 리스트화
src_file_lnc <- length(src_file)

for (i in 2:src_file_lnc) {
  print(i) #몇번째 파일
  print(src_file[i])
  
  assign(paste0("NGS_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))   #csv로 읽어오기
  
  NGS_df <- select(throw("NGS_",i),ID) #Gene_Name Effect 추출
  NGS_V <- as.vector(t(NGS_df))
  
  n <- length(NGS_V) #반복을 실행할 횟수 설정 
  NGS_new <- data.frame(matrix(nrow=0, ncol=n))   #결과를 넣을 빈 table
  
  for ( j in 1:n) {
    rsNumb <- NGS_V[j]
    print(j)
    print(rsNumb)
    html <-read_html(paste0(basic_url,rsNumb))
    html_no <- html %>% html_nodes( '.ncbi-docsum') %>% html_nodes('.ncbi-unstyled-list')
    html1 <- html %>% html_nodes("body") %>% html_nodes(".bold")
    htmlm <- html %>% html_nodes("tbody") %>% html_nodes(".rprt")
    
    A <- c(length(html_no),length(html1),length(htmlm))
    print(A)
    if (length(html_no) == 2 & length(html1) == 0 & length(htmlm) == 0) {
      # no_item
      CS <- html_no[1]
      TX <- html_text(CS)
      SX <- str_split(TX, "\n")[[1]]
      CinVar <- SX[1]
      CinVar <- gsub("Clinical significance: ","",CinVar)
      print(CinVar)
      NGS_new[1,j] <- CinVar
      rm(CS,TX,SX,CinVar)
    } else if (length(html_no) == 0 & length(html1) == 1 & length(htmlm) == 0) {
      # one result
      CS <- html1[1]
      TX <- html_text(CS)
      SX <- str_split(TX, "\n")[[1]]
      CinVar <- SX[3]
      print(CinVar)
      NGS_new[1,j] <- CinVar
      rm(CS,TX,SX,CinVar)
    } else if (length(html_no) == 0 & length(html1) == 0 & length(htmlm) == 0) {
      TX <- "No items found"
      CinVar <- TX
      NGS_new[1,j] <- CinVar
      rm(TX, CinVar)
    } else {
      #many results
      CS <- htmlm[1]
      Sample<- html_nodes(CS, "td")
      SamX <- Sample[6]
      TX <- html_text(SamX)
      print(TX)
      NGS_new[1,j] <- TX
      rm(CS,TX,SamX,Sample)
    }
  }
  newCol <-  t(NGS_new)
  colnames(newCol) <- "ClinVar"
  NEW <- cbind(throw("NGS_",i), newCol)
  name <- gsub(".csv","",src_file[i])
  
  write.csv(NEW, file = paste0("E:/R/PANC 2nd/sort_Reactom/by pathway/",name,"-clinvar-add.csv"), row.names = FALSE)
}
