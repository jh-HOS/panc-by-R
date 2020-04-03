rm(list=ls()) 

source("https://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
library(colorspace)
library(GetoptLong)

raw<-read.csv("E:/R/PANC 2nd/Drug/20200109 total ����.csv", na.strings = "NA", header = T)
row.names(raw)<-raw$Name

raw<-raw[,2:21]
#2:N �ε� N�� ��ü ���� ������ +1 �� ����
raw<-data.matrix(raw)

Heatmap(raw, name="AUC", column_title = "Pancreatic cancer", 
        row_title = "Drugs", 
        column_title_side = "bottom",cluster_rows = FALSE,
        col = colorRamp2(c(0.8, 1.55, 2.2), c("blue", "white", "red")))