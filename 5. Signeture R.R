rm(list=ls())

library("MutationalPatterns")
setwd("F://TOTALVCF/CRCOrganoidVCFTotal/mutationalpattern/TOTAL")

library(MutationalPatterns)
library(BSgenome)

head(available.genomes())
ref_genome <- "BSgenome.Hsapiens.UCSC.hg19"
library(ref_genome, character.only = TRUE)
library(MutationalPatterns)


vcf_dir <- c('F://TOTALVCF/CRCOrganoidVCFTotal/mutationalpattern/TOTAL')
vcf_files <- list.files(vcf_dir, pattern = ".vcf", full.names = TRUE)

vcf_files2 <- list.files(pattern = ".vcf", full.names = TRUE)

write.table(vcf_files2, "totallist.txt", sep="\t")
data1<-read.table("totallist.txt", header=T, sep="\t", na.strings = "NA", fill=T)
v.data1<-as.vector(t(data1))
length(v.data1)

sample_names <- v.data1
vcfs <- read_vcfs_as_granges(vcf_files, sample_names, ref_genome)
summary(vcfs)
tissue <- v.data1


muts = mutations_from_vcf(vcfs[[1]])
head(muts, 12)
types = mut_type(vcfs[[1]])
head(types, 12)
context = mut_context(vcfs[[1]], ref_genome)
head(context, 12)
type_context = type_context(vcfs[[1]], ref_genome)
lapply(type_context, head, 12)
type_occurrences <- mut_type_occurrences(vcfs, ref_genome)
type_occurrences

p1 <- plot_spectrum(type_occurrences)
p2 <- plot_spectrum(type_occurrences, CT = TRUE)
p3 <- plot_spectrum(type_occurrences, CT = TRUE, legend = FALSE)
library("gridExtra")
grid.arrange(p1, p2, p3, ncol=3, widths=c(3,3,1.75))

dev.copy(tiff,"4849mutsig.tiff",width=16, height=8, units="in",res=200) 
dev.off() 


p4 <- plot_spectrum(type_occurrences, by = tissue, CT = TRUE, legend = TRUE)
palette <- c("pink", "orange", "blue", "lightblue", "green", "red", "purple")
p5 <- plot_spectrum(type_occurrences, CT=TRUE, legend=TRUE, colors=palette)
grid.arrange(p4, p5, ncol=2, widths=c(4,2.3))

dev.copy(tiff,"4849mutsig2.tiff",width=16, height=8, units="in",res=200) 
dev.off() 

mut_mat <- mut_matrix(vcf_list = vcfs, ref_genome = ref_genome)
head(mut_mat)
plot_96_profile(mut_mat)

dev.copy(tiff,"4849mutmatrix.tiff",width=10, height=11, units="in",res=200) 
dev.off() 


---------------------------------------------------------------------------------------------------------------------

plot_96_profile(mut_mat[,c(1,7)])
plot_96_profile(mut_mat[,c(1,7)], condensed = TRUE)

mut_mat <- mut_mat + 0.0001
library("NMF")
estimate <- nmf(mut_mat, rank=2:5, method="brunet", nrun=10, seed=123456)
plot(estimate)
nmf_res <- extract_signatures(mut_mat, rank = 2, nrun = 10)
colnames(nmf_res$signatures) <- c("Signature A", "Signature B")
rownames(nmf_res$contribution) <- c("Signature A", "Signature B")
plot_96_profile(nmf_res$signatures, condensed = TRUE)
pc1 <- plot_contribution(nmf_res$contribution, nmf_res$signature, mode = "relative")
pc2 <- plot_contribution(nmf_res$contribution, nmf_res$signature, mode = "absolute")
grid.arrange(pc1, pc2)
plot_contribution(nmf_res$contribution, nmf_res$signature, mode = "absolute", coord_flip = TRUE)


sp_url <- paste("https://cancer.sanger.ac.uk/cancergenome/assets/", "signatures_probabilities.txt", sep = "")
cancer_signatures = read.table(sp_url, sep = "\t", header = TRUE)
new_order = match(row.names(mut_mat), cancer_signatures$Somatic.Mutation.Type)
cancer_signatures = cancer_signatures[as.vector(new_order),]
row.names(cancer_signatures) = cancer_signatures$Somatic.Mutation.Type
cancer_signatures = as.matrix(cancer_signatures[,4:33])
plot_96_profile(cancer_signatures[,1:2], condensed = TRUE, ymax = 0.3)
fit_res <- fit_to_signatures(mut_mat, cancer_signatures)
select <- which(rowSums(fit_res$contribution) > 10)
plot_contribution(fit_res$contribution[select,], cancer_signatures[,select], coord_flip = FALSE,mode = "absolute")

dev.copy(tiff,"totalsig.tiff",width=18, height=10, units="in",res=200) 
dev.off() 






