library(ComplexHeatmap)
setwd("C:/JHR/pancchr")

rawA = read.table("Total mutation.csv", row.names=1, header = TRUE, sep = ",", stringsAsFactors = FALSE)

matA <- as.matrix(rawA)
col = c(SNV = "indianred1", INDEL = "dodgerblue")

snv_fun = function(x, y, w, h) {
  grid.rect(x, y, w, h, gp = gpar(fill = col["SNV"], col = NA))
}

indel_fun = function(x, y, w, h) {
  grid.rect(x, y, w, h, gp = gpar(fill = col["INDEL"], col = NA))
}

onco <- oncoPrint(matA, get_type = function(x) strsplit(x, ";")[[1]],
                  column_order = NULL,
                  alter_fun = function(x, y, w, h, v) {
                    n = sum(v)
                    w = convertWidth(w, "cm")*0.9
                    h = convertHeight(h, "cm")*0.9
                    
                    grid.rect(x, y, w, h, gp = gpar(fill = "grey90", col = NA))
                    
                    if(n == 0) return(NULL)
                    if(n == 1) {
                      if(names(which(v)) == "SNV") snv_fun(x, y, w, h)
                      if(names(which(v)) == "INDEL") indel_fun(x, y, w, h)
                    } else if(n == 2) {
                      snv_fun(x, y, w, h)
                      indel_fun(x, y, w, h*0.4)
                    }
                  },column_title = "Pancreatic cancer 2nd Characterization Total mutation",
                   heatmap_legend_param = list(title = "Alternation type", nrow = 1,
                                               title_position = "leftcenter"), col = col)
draw(onco, heatmap_legend_side = "bottom")
