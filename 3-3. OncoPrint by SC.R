rm(list=ls())

library(ComplexHeatmap)
library(grid)
raw <-  read.csv("E:/R/PANC 2nd/sort_Reactom42/RMTsSDSA.csv", stringsAsFactors = T,na.strings = NA, header = T)  #csv로 읽어오기
row.names(raw) <- raw$Gene_name
raw<-raw[,2:16]
raw = as.matrix(raw)

sampleOrder<-read.table("E:/R/PANC 2nd/Sampleorder.txt", header=F, sep="\t")
f.sampleOrder<-as.character(t(sampleOrder))
v.sampleOrder<-as.vector(f.sampleOrder)

RMT <- oncoPrint(raw, get_type = function(x) strsplit(x, ";")[[1]],
          alter_fun = list( 
            background = function(x, y, w, h) grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#d7d7d7", col = NA)),
            missense_variant = function(x, y, w, h) grid.rect(x, y, w*0.97, h*0.95, gp = gpar(fill = "navy", col = NA)),
            frameshift_variant = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.5, gp = gpar(fill = "red2", col = NA)),
            protein_protein_contact = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.5, gp = gpar(fill = "black", col = NA)),
            stop_gained = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "green2", col = NA)),
            inframe_deletion = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = "slateblue1", col = NA)),
            inframe_insertion = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "salmon", col = NA)),
            disruptive_inframe_deletion = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = "slateblue4", col = NA)),
            disruptive_inframe_insertion = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "salmon4", col = NA))
          ), col = c(missense_variant = "navy", frameshift_variant = "red3", protein_protein_contact = "black", 
                     stop_gained = "green", inframe_deletion = "slateblue1", inframe_insertion = "salmon",
                     disruptive_inframe_deletion = "slateblue4",disruptive_inframe_insertion = "salmon4"),
          show_column_names = TRUE, column_order = v.sampleOrder,
          column_title = "Pancreatic Cancers Mutation Profile", width = unit(25, "cm"),height = unit(7,"cm"), 
          heatmap_legend_param = list(title = "Alternations", at = c("missense_variant", "frameshift_variant", "protein_protein_contact", 
                                                                     "stop_gained", "inframe_deletion","inframe_insertion",
                                                                     "disruptive_inframe_deletion", "disruptive_inframe_insetion"), 
                                      labels = c("missense_variant", "frameshift_variant", "protein_protein_contact", 
                                                 "stop_gained", "inframe_deletion","inframe_insertion",
                                                 "disruptive_inframe_deletion", "disruptive_inframe_insetion")
          ))
