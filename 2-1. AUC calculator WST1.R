# ���� 1 srd_dir �������� drug ����� �ҷ��;� �ϹǷ� �������� ��� �̿��� ������ ������ ����ȵ�.
# ���� 2 ��� ���� �̸� ������ "Cell�̸� p1.txt"���� �ؾ���. �̸��� �÷���Ʈ ��ȣ�� ����� ������.
# ���� 3 p1, p2�� �÷���Ʈ�� �� ������ cell �̸�, control���� ����ִ� �ؽ�Ʈ ���� �ֱ�.
# ���� 4 WST1 ��� ���ϸ� �״�� �б� ����, Cell titer glow�� raw�� assign�ϴ� �κ��� tbl[]�ǰ��� �ణ �����ؾ���.
# ���� 5 plate �߰��� p3 ���� ���� �� else if �� ���� �߰��ϱ�.

rm(list=ls()) #���� ����� Data�� value ��� �����ϱ�.
library(tidyr)

src_dir <- c('E:/R/EXAMPLE/')  #�ҷ��� ���ϵ��� �ִ� ��ġ ����
src_file <- list.files(src_dir)     #�ҷ��� ���ϵ� ����Ʈȭ
src_file_lnc <- length(src_file)  # �ҷ��� ���ϵ��� ���� ����

df_file <- as.data.frame(src_file)
df_file

input_table <- df_file %>%
  separate(src_file, c("cell", "plate"), " ") # Total ��� ������ ���� ����

cal.AUC <- function(x) {
  AUC = (colSums(x[,1:9], na.rm = T) + colSums(x[2:5,1:9], na.rm = T))*log10(3)/sum(x[1:2,10])
  AUC = t(AUC)
} # AUC ������

p1 <- read.table("E:/R/EXAMPLE/191011 Drug ����/panc_p1.txt")
p2 <- read.table("E:/R/EXAMPLE/191011 Drug ����/panc_p2.txt")

drug_p1 <- as.matrix(p1)
drug_p2 <- as.matrix(p2) #Drug�̸� �Է� �� output ��������

for (i in 1:src_file_lnc){
  print(i)
  print(src_file[i])
  
  tbl <- assign(paste0("result_",i),
                read.table(paste0(src_dir, "/", src_file[i]),
                           sep = "\t", fill = T,
                           na.strings = NA,
                           header = F))   #table�� �о����
  
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
  
  #if insert ����
  if (raw.plate == "p1.txt"|raw.plate == "p1") {
    drug_p1 <- rbind(drug_p1,row.cell)
    
  } else if (raw.plate == "p2.txt"|raw.plate == "p2") {
    drug_p2 <- rbind(drug_p2,row.cell)
  } else {
    print(i)
    print("�����̸��� �̻��ѵ�?")
  }
}

d_p1 <- as.matrix(drug_p1, col.names = F)
d_p2 <- as.matrix(drug_p2, col.names = F)

write.table(d_p1, file="E:/R/pancchr/drug_2/Results_plate1.txt",col.names = F, row.names=F, sep = "\t")
write.table(d_p2, file="E:/R/pancchr/drug_2/Results_plate2.txt",col.names = F, row.names=F, sep = "\t")