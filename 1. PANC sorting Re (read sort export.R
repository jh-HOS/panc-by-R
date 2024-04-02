
NGS <- read.csv("E:/R/PANC 2nd/raw_No modifier/SNU2729B1_Effect_No modifier.csv", header = T, stringsAsFactors = TRUE, na.strings = NA)

gene <-read.table("E:/R/PANC 2nd/sort_Reactom/by pathway/Participating Molecules R-HSA-73857 RNApol II-GeneOnly.tsv", header=T, sep="\t")
v.gene <-as.vector(t(gene))

list1<-grep(paste(v.gene,collapse="$|^"),NGS$Gene_Name)
m.list1<-as.matrix(list1)


sm.list1<-sort(m.list1)

for (i in 1:length(sm.list1)) {
  assign(paste0("gn_",i), NGS[sm.list1[i],])
  print(get(paste0("gn_",i)))
}
s.NGS <- NGS[sm.list,]

s.NGS <- s.NGS[!duplicated(s.NGS$POS),]

write.csv(s.NGS, file="C:/JHR/pancchr/SNU2822sorted.csv", row.names=FALSE)