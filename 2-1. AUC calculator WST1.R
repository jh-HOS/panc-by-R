# 조건 1 srd_dir 폴더에서 drug 결과만 불러와야 하므로 폴더내에 결과 이외의 파일이 있으면 진행안됨.
# 조건 2 결과 파일 이름 형식은 "Cell이름 p1.txt"으로 해야함. 이름과 플레이트 번호를 띄어쓰기로 구분함.
# 조건 3 p1, p2에 플레이트의 약 순서와 cell 이름, control값이 들어있는 텍스트 파일 넣기.
# 조건 4 WST1 결과 파일만 그대로 읽기 가능, Cell titer glow는 raw를 assign하는 부분의 tbl[]의값을 약간 변형해야함.
# 조건 5 plate 추가시 p3 변수 제작 및 else if 에 조건 추가하기.

rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.
library(tidyr)

src_dir <- c('E:/R/EXAMPLE/')  #불러올 파일들이 있는 위치 설정
src_file <- list.files(src_dir)     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정

df_file <- as.data.frame(src_file)
df_file

input_table <- df_file %>%
  separate(src_file, c("cell", "plate"), " ") # Total 결과 정리할 파일 제작

cal.AUC <- function(x) {
  AUC = (colSums(x[,1:9], na.rm = T) + colSums(x[2:5,1:9], na.rm = T))*log10(3)/sum(x[1:2,10])
  AUC = t(AUC)
} # AUC 계산공식

p1 <- read.table("E:/R/EXAMPLE/191011 Drug 정리/panc_p1.txt")
p2 <- read.table("E:/R/EXAMPLE/191011 Drug 정리/panc_p2.txt")

drug_p1 <- as.matrix(p1)
drug_p2 <- as.matrix(p2) #Drug이름 입력 및 output 형식파일

for (i in 1:src_file_lnc){
  print(i)
  print(src_file[i])
  
  tbl <- assign(paste0("result_",i),
                read.table(paste0(src_dir, "/", src_file[i]),
                           sep = "\t", fill = T,
                           na.strings = NA,
                           header = F))   #table로 읽어오기
  
  raw <- assign(paste0("Data_",i),tbl[4:9,3:12])
  
  MC <- mean(raw[1:2,10])
  DC <- mean(raw[3:4,10])
  EC <- mean(raw[5:6,10])
  ctl <- c(MC,DC,EC)
  
  raw.AUC <- as.matrix(cal.AUC(raw))
  raw.name <- input_table[i,1]
  raw.ctl <- as.matrix(t(ctl))
  raw.plate <- input_table[i,2]
  
  row.cell <- assign(paste0("row_",i), cbind(raw.name,raw.AUC,raw.ctl))
  
  #if insert 검증
  if (raw.plate == "p1.txt"|raw.plate == "p1") {
    drug_p1 <- rbind(drug_p1,row.cell)
    
  } else if (raw.plate == "p2.txt"|raw.plate == "p2") {
    drug_p2 <- rbind(drug_p2,row.cell)
  } else {
    print(i)
    print("파일이름이 이상한디?")
  }
}

d_p1 <- as.matrix(drug_p1, col.names = F)
d_p2 <- as.matrix(drug_p2, col.names = F)

write.table(d_p1, file="E:/R/pancchr/drug_2/Results_plate1.txt",col.names = F, row.names=F, sep = "\t")
write.table(d_p2, file="E:/R/pancchr/drug_2/Results_plate2.txt",col.names = F, row.names=F, sep = "\t")
