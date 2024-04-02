rm(list=ls())
library(ComplexHeatmap)
library(dendsort)
library(cluster)


col = c(missense_variant = "#377eb8", frameshift_variant = "#ff7f00",stop_lost = "#ffff33", protein_protein_contact = "#8dd3c7", 
        stop_gained = "#e41a1c", start_lost = "#e41a1c", inframe_deletion = "#4daf4a", inframe_insertion = "#b3de69",
        disruptive_inframe_deletion = "#4daf4a",disruptive_inframe_insertion = "#b3de69", sequence_feature = "#FF7DA8",
        splice_region_variant = "#984ea3", intron_variant = "#984ea3", splice_donor_variant = "#984ea3",splice_acceptor_variant = "#984ea3")

alter_fun = list( 
  background = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#d7d7d7", col = NA))
  },
  missense_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "#377eb8", col = NA))
  },
  frameshift_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.6, gp = gpar(fill = "#ff7f00", col = NA))
  },
  stop_lost = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.5, gp = gpar(fill = "#ffff33", col = NA))
  },
  protein_protein_contact = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#8dd3c7", col = NA))
  },
  stop_gained = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.5, gp = gpar(fill = "#e41a1c", col = NA))
  },
  start_lost = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.5, gp = gpar(fill = "#e41a1c", col = NA))
  },
  inframe_deletion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "#4daf4a", col = NA))
  },
  inframe_insertion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "#b3de69", col = NA))
  },
  disruptive_inframe_deletion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "#4daf4a", col = NA))
  },
  disruptive_inframe_insertion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "#b3de69", col = NA))
  },
  sequence_feature = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#FF7DA8", col = NA))
  },
  splice_region_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#984ea3", col = NA))
  },
  intron_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#984ea3", col = NA))
  },
  splice_donor_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#984ea3", col = NA))
  },
  splice_acceptor_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#984ea3", col = NA))
  }
)
heatmap_legend_param = list(title = "Alternations", at = c("missense_variant", "frameshift_variant", "stop_lost", "protein_protein_contact", 
                                                           "stop_gained","start_lost", "inframe_deletion","inframe_insertion",
                                                           "sequence_feature", "splice_region_variant","disruptive_inframe_deletion","disruptive_inframe_insertion",
                                                           "intron_variant","splice_donor_variant","splice_acceptor_variant"), 
                            labels = c("Missense_Mutation", "Frame_Shift", "Stop_Lost","Protein_Protein_Interaction", 
                                       "Nonsense_Mutation","Nonsense_Mutation", "In_Frame_Del","In_Frame_Ins",
                                       "Amino Acids Modification", "Splice_sites","In_Frame_Del","In_Frame_Ins",
                                       "Splice_sites","Splice_sites","Splice_sites"))

raw <- read.csv(paste0("E:/R/PANC 2nd/sort_2 HER2 oe/3. results/WTKRAS related genes-total.csv"), stringsAsFactors = F, header = T)  #csv로 읽어오기
raw[is.na(raw)] = ""
row.names(raw) <- raw$Gene_Name
raw<-raw[,2:8]
raw = as.matrix(raw)

sampleOrder<-read.table("E:/R/PANC 2nd/to total/census/total/roworder.txt", header=F, sep="\t")
f.sampleOrder<-as.character(t(sampleOrder))
v.sampleOrder<-as.vector(f.sampleOrder)

column_title = "PI3K/AKT Signaling in Cancer"
RNAPOL <- oncoPrint(raw, alter_fun = alter_fun, col = col, remove_empty_rows = TRUE,
          show_column_names = T, column_order = NULL , show_row_names = T, row_order = NULL,
          column_title = column_title, heatmap_legend_param = heatmap_legend_param)
RNAPOL 

length(v.sampleOrder)
dim(raw)
