rm(list=ls()) #���� ����� Data�� value ��� �����ϱ�.

library(dplyr)
library(data.table)

src_dir <- c('E:/R/PANC 2nd/raw_1')  #�ҷ��� ���ϵ��� �ִ� directory ����
src_file <- list.files(src_dir, pattern = ".csv")     #�ҷ��� ���ϵ� ����Ʈȭ
src_file_lnc <- length(src_file)  # �ҷ��� ���ϵ��� ���� ����


for (i in 1:src_file_lnc){
  assign(paste0("NGS_",i),
         fread(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings=getOption("datatable.na.strings","NA"),
                  header = T))   #csv�� �о����
  
  assign(paste0("Col_", i),filter(get(paste0("NGS_",i)),
                                  Impact != "MODIFIER", Impact != "LOW"))
  
  Genename <- get(paste0("Col_",i))[!duplicated(get(paste0("Col_",i))$ID),1:19]#�ߺ��� position ����
  
  Genename$Gene_Name <- gsub("`", "", Genename$Gene_Name)
  
  filename <- gsub("_effect.csv", "",src_file[i])
  
  assign(paste0("name_",i),
         paste0("E:/R/PANC 2nd/raw_No modifier/", filename,"_No modifier.csv" ))
  
  write.csv(Genename, get(paste0("name_",i)), row.names=FALSE) #uniq-i�� ������ ������ ���� �̸�������� csv ����
  
  print(i)
  print(src_file[i])
}
  