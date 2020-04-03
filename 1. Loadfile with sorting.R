rm(list=ls()) #���� ����� Data�� value ��� �����ϱ�.

src_dir <- c('E:/R/PANC 2nd/raw_1')  #�ҷ��� ���ϵ��� �ִ� directory ����
src_file <- list.files(src_dir)     #�ҷ��� ���ϵ� ����Ʈȭ
src_file_lnc <- length(src_file)  # �ҷ��� ���ϵ��� ���� ����

gene <-read.table("E:/R/PANC 2nd/Gene_Lists/Mutation_Basic.txt", header=T, sep="\t")
v.gene <-as.vector(t(gene))   #���� ������ ��� ��ȯ�� �ϹǷ� �̸� gene����Ʈ �����ϱ�

for (i in 1:src_file_lnc){
  assign(paste0("NGS_",i),
         read.csv(paste0(src_dir, "/", src_file[i]),
                  stringsAsFactors = T,
                  na.strings = NA,
                  header = T))   #csv�� �о����
  
  assign(paste0("list_",i),
         grep(paste(v.gene,collapse = "$|^"),get(paste0("NGS_",i))$Gene_Name)) #������ ���� collapse�Ͽ� ��Ī�Ǵ� column list_�� ����
  
  assign(paste0("m.list",i),
         sort(as.matrix(get(paste0("list_",i))))) #����Ǵ� ����Ʈ ��Ʈ������ ��ȯ
  
  assign(paste0("Col_",i),
         get(paste0("NGS_",i))[get(paste0("m.list",i)),]) #

  assign(paste0("coll_",i),
         get(paste0("Col_",i))[(get(paste0("Col_",i))$Impact == 'HIGH'),])
  #Impact�� low, modifier�� ��� �����ϱ�
  
  assign(paste0("uniq_",i),
         get(paste0("coll_",i))[!duplicated(get(paste0("coll_",i))$POS),1:19]) #�ߺ��� position ����
  
  assign(paste0("name_",i),
         paste0("E:/R/PANC 2nd/sort_impact high/", src_file[i]))
  
  write.csv(get(paste0("uniq_",i)), get(paste0("name_",i)), row.names=FALSE) #uniq-i�� ������ ������ ���� �̸�������� csv ����
  
  print(i)
  print(src_file[i])
}

str(Gene_1)