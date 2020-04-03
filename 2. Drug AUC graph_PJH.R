library(ComplexHeatmap)
library(circlize)
library(devtools)

setwd("C:/JHR/pancchr")
auc <- read.csv( "pancdrug result.csv", na.strings = "NA", header = T)
row.names(auc) <- auc$drugs
auc <- auc[,2:10]
auc
auc_matrix <- data.matrix(auc)
Heatmap(auc_matrix,name="AUC",column_title = "Pancreatic cancer Cell lines",
        column_title_side = "bottom",row_title = "Drugs",
        row_title_side = "right", cluster_columns = T, cluster_rows = F,
        col = colorRamp2(c(0, 1.8, 3), c('dodgerblue', 'white', 'indianred1')))
