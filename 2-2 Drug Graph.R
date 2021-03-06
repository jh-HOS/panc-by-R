rm(list=ls()) 

source("https://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
library(colorspace)
library(GetoptLong)

raw<-read.csv("E:/R/PANC 2nd/Drug/20200109 total 정리.csv", na.strings = "NA", header = T)
row.names(raw)<-raw$Name

raw<-raw[,2:21]
#2:N 인데 N이 전체 샘플 갯수에 +1 한 숫자
raw<-data.matrix(raw)

Heatmap(raw, name="AUC", column_title = "Pancreatic cancer", 
        row_title = "Drugs", 
        column_title_side = "bottom",cluster_rows = FALSE,
        col = colorRamp2(c(0.8, 1.55, 2.2), c("blue", "white", "red")))
