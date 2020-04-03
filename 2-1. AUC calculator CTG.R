rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

library(tidyr)
library(readxl)

try(
  {src_dir <- c('E:/R/PANC 2nd/Drug/totla/')  #불러올 파일들이 있는 위치 설정
  src_file <- list.files(src_dir,pattern = "*.xlsx") 
  src_file_lnc <- length(src_file)
  
  df_file <- as.data.frame(src_file)
  
  input_table <- df_file %>%
    separate(src_file, c("cell", "plate"), " ") #결과 정리 파일
  
  cal.AUC <- function(x) {
    AUC = (colSums(x[,1:9], na.rm = T) + colSums(x[2:5,1:9], na.rm = T))*log10(3)*3/(2*sum(x[1:3,10]))
    AUC = t(AUC)
  }
  
  p1 <- read.table("E:/R/PANC 2nd/Drug/기본/panc_p1.txt")
  p2 <- read.table("E:/R/PANC 2nd/Drug/기본/panc_p2.txt")
  
  drug_p1 <- as.matrix(p1)
  drug_p2 <- as.matrix(p2)}
)

for (i in 1:src_file_lnc){
  print(i)
  print(src_file[i])
  
  raw_tbl <- assign(paste0("result_",i),
                read_excel(paste0(src_dir, "/", src_file[i]),
                           sheet=1,
                           range="C12:L17",
                           col_names = F,
                           col_types = "numeric", na="NA"))   #excel 읽어오기
  
  tbl <- as.matrix(raw_tbl)
  
  raw.plate <- input_table[i,2]
  raw.name <- input_table[i,1]

  MC <- mean(tbl[1:3,10])
  DC <- mean(tbl[4:6,10])

  raw.AUC <- as.matrix(cal.AUC(tbl))

  
  ctl <- c(MC,DC)
  raw.ctl <- as.matrix(t(ctl))
  row.cell <- assign(paste0("row_",i), cbind(raw.name,raw.AUC,raw.ctl))
                
  #if insert 검증
  if (raw.plate == "p1.xlsx"|raw.plate == "p1") {
    drug_p1 <- rbind(drug_p1,row.cell)
            
  } else if (raw.plate == "p2.xlsx"|raw.plate == "p2") {
    drug_p2 <- rbind(drug_p2,row.cell)
  } else {
    print("파일이름이 이상한디?")
  }
}

try({
  d_p1 <- as.matrix(drug_p1, col.names = F)
  d_p2 <- as.matrix(drug_p2, col.names = F)
  
  write.table(d_p1, file="E:/R/PANC 2nd/Drug/totla/Resultes_plate1.txt",col.names = F, row.names=F, sep = "\t")
  write.table(d_p2, file="E:/R/PANC 2nd/Drug/totla/Resultes_plate2.txt",col.names = F, row.names=F, sep = "\t")
})
