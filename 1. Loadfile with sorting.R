rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.

src_dir <- c('E:/R/PANC 2nd/raw_1')  #불러올 파일들이 있는 directory 설정
src_file <- list.files(src_dir, pattern = "csv")     #불러올 파일들 리스트화
src_file_lnc <- length(src_file)  # 불러올 파일들의 길이 설정

gene <-read.table("E:/R/PANC 2nd/Gene_Lists/Well known pdac.txt", header=T, sep="\t")
v.gene <-as.vector(t(gene))   #루프 내에서 모든 변환을 하므로 미리 gene리스트 변형하기

for (i in 1:src_file_lnc){
  assign(paste0("NGS_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))   #csv로 읽어오기
  
  assign(paste0("list_",i),
         grep(paste(v.gene,collapse = "$|^"),get(paste0("NGS_",i))$Gene_Name)) #유전자 파일 collapse하여 매칭되는 column list_로 저장
  
  assign(paste0("m.list",i),
         sort(as.matrix(get(paste0("list_",i))))) #추출되는 리스트 매트릭스로 변환
  
  assign(paste0("Col_",i),
         get(paste0("NGS_",i))[get(paste0("m.list",i)),]) #

  assign(paste0("uniq_",i),
         get(paste0("Col_",i))[!duplicated(get(paste0("Col_",i))$POS),1:29]) #중복된 position 제거
  
  assign(paste0("name_",i),
         paste0("E:/R/PANC 2nd/to total/", src_file[i]))
  
  write.csv(get(paste0("uniq_",i)), get(paste0("name_",i)), row.names=FALSE) #uniq-i로 명명한 파일을 위의 이름순서대로 csv 저장
  
  print(i)
  print(src_file[i])
}

str(Gene_1)
