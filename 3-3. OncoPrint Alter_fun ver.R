rm(list=ls())
library(ComplexHeatmap)

col = c(missense_variant = "#377eb8", frameshift_variant = "#33a02c",stop_lost = "#1f78b4", protein_protein_contact = "#ffff33", 
        stop_gained = "black", start_lost = "black", inframe_deletion = "#ff7f00", inframe_insertion = "#fdbf6f",
        disruptive_inframe_deletion = "#ff7f00",disruptive_inframe_insertion = "#fdbf6f", sequence_feature = "#cab2d6",
        splice_region_variant = "#6a3d9a", intron_variant = "#6a3d9a", splice_donor_variant = "#6a3d9a",splice_acceptor_variant = "#6a3d9a")

alter_fun = list( 
  background = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), gp = gpar(fill = "#d7d7d7", col = NA))
  },
  missense_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = "#377eb8", col = NA))
  },
  frameshift_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.6, gp = gpar(fill = "#33a02c", col = NA))
  },
  stop_lost = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.5, gp = gpar(fill = "#1f78b4", col = NA))
  },
  protein_protein_contact = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#ffff33", col = NA))
  },
  stop_gained = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = NA, lwd = 1))
  },
  start_lost = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.9, gp = gpar(fill = NA, lwd = 1))
  },
  inframe_deletion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "#ff7f00", col = NA))
  },
  inframe_insertion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "#fdbf6f", col = NA))
  },
  disruptive_inframe_deletion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.4, gp = gpar(fill = "#ff7f00", col = NA))
  },
  disruptive_inframe_insertion = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.3, gp = gpar(fill = "#fdbf6f", col = NA))
  },
  sequence_feature = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#cab2d6", col = NA))
  },
  splice_region_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#6a3d9a", col = NA))
  },
  intron_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#6a3d9a", col = NA))
  },
  splice_donor_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#6a3d9a", col = NA))
  },
  splice_acceptor_variant = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h*0.2, gp = gpar(fill = "#6a3d9a", col = NA))
  }
)
heatmap_legend_param = list(title = "Alternations", at = c("missense_variant", "frameshift_variant", "stop_lost", "protein_protein_contact", 
                                                           "stop_gained", "inframe_deletion","inframe_insertion",
                                                           "sequence_feature", "splice_region_variant"), 
                            labels = c("Missense_Mutation", "Frame_Shift", "Stop_Lost","Protein_Protein_Interaction", 
                                       "Nonsense_Mutation", "In_Frame_Del","In_Frame_Ins",
                                       "Amino Acids Modification", "Splice_sites"))


path <- "Participating Molecules R-HSA-9671555PDGFR-GeneOnly-total"
raw <- read.csv(paste0("E:/R/PANC 2nd/sort_Reactom/",path,".csv"), stringsAsFactors = F, header = T)  #csv로 읽어오기
raw[is.na(raw)] = ""
row.names(raw) <- raw$Gene_Name
raw<-raw[,2:16]
raw = as.matrix(raw)

sampleOrder<-read.table("E:/R/PANC 2nd/Sampleorder.txt", header=F, sep="\t")
f.sampleOrder<-as.character(t(sampleOrder))
v.sampleOrder<-as.vector(f.sampleOrder)

column_title = "OncoPrint for KCLB Pancreatic Adenocarcinoma, Genes in PDGFR Related Pathway"
PDGFR <- oncoPrint(raw, alter_fun = alter_fun, col = col,remove_empty_rows = TRUE,
          show_column_names = T, column_order = NULL,show_row_names = T,
          column_title = column_title, heatmap_legend_param = heatmap_legend_param)
PDGFR 
