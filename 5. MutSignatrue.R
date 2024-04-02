rm(list=ls()) #현재 저장된 Data나 value 모두 삭제하기.
library(gridExtra)
library(dplyr)
library(MutationalPatterns)
library(BSgenome)
ref_genome <- "BSgenome.Hsapiens.UCSC.hg19"
library(ref_genome, character.only = TRUE)
library("NMF")

T.celllines <- c("SNU-2729B1","SNU-2822","SNU-2913","SNU-2918","SNU-2982-1","SNU-3139","SNU-3294","SNU-3375","SNU-3573","SNU-3608","SNU-3615","SNU-3752","SNU-3923T","SNU-4208T","SNU-4223","SNU-4305T","SNU-4340T","SNU-4354T","SNU-4405","SNU-4482","SNU-4492","SNU-4525","SNU-4733","SNU-4771","SNU-4866","SNU-5177")

vcf_dir.T <- c('E:/R/PANC 2nd/sort_7 gvcf') #non sorted
vcf_dir.S <- c('E:/R/PANC 2nd/sort_6 vcf')  #sorted

vcf_files.T <- list.files(vcf_dir.T, pattern = ".vcf", full.names = TRUE)
vcf_files.S <- list.files(vcf_dir.S, pattern = ".vcf", full.names = TRUE)

vcfs <- read_vcfs_as_granges(vcf_files.T, T.celllines, ref_genome)
vcfss <- read_vcfs_as_granges(vcf_files.S, T.celllines, ref_genome)

summary(vcfs)

muts = mutations_from_vcf(vcfs[[1]])
types = mut_type(vcfs[[1]])
context = mut_context(vcfs[[1]], ref_genome)
type_context = type_context(vcfs[[1]], ref_genome)
type_occurrences <- mut_type_occurrences(vcfs, ref_genome)
type_occurrencess <- mut_type_occurrences(vcfss, ref_genome)

#96 mutation spectrum total

p1 <- plot_spectrum(type_occurrencess)
p2 <- plot_spectrum(type_occurrencess, CT = TRUE)
p3 <- plot_spectrum(type_occurrencess, CT = TRUE, legend = FALSE)

grid.arrange(p1, p2, p3, ncol=3, widths=c(3,3,1.75))

dev.copy(tiff,"occurrences groupeds.tiff",width=20, height=10, units="in",res=200) 
dev.off() 


#96 mutation type by cell line

mut_mat <- mut_matrix(vcf_list = vcfs, ref_genome = ref_genome)
mut_mats <- mut_matrix(vcf_list = vcfss, ref_genome = ref_genome)

head(mut_mat)
plot_96_profile(mut_mat,condensed = TRUE)
plot_96_profile(mut_mats,condensed = TRUE)


dev.copy(tiff,"96plot total.tiff",width=10, height=40, units="in",res=200) 
dev.off()

mut_mat1 <- mut_mat[,1:6]
mut_mat2 <- mut_mat[,7:13]
mut_mat3 <- mut_mat[,14:19]
mut_mat4 <- mut_mat[,20:26]

mut_mats1 <- mut_mats[,1:6]
mut_mats2 <- mut_mats[,7:13]
mut_mats3 <- mut_mats[,14:19]
mut_mats4 <- mut_mats[,20:26]


plot96_1 <- plot_96_profile(mut_mats1,condensed = TRUE)
plot96_2 <- plot_96_profile(mut_mats2,condensed = TRUE)
plot96_3 <- plot_96_profile(mut_mats3,condensed = TRUE)
plot96_4 <- plot_96_profile(mut_mats4,condensed = TRUE)

plot96_4

dev.copy(tiff,"96plot sorted4.tiff",width=10, height=10, units="in",res=200) 
dev.off() 

mut_mat <- mut_mat + 0.0001

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

#cosmic signature

sp_url <- paste("https://cancer.sanger.ac.uk/cancergenome/assets/", "signatures_probabilities.txt", sep = "")
cancer_signatures = read.table(sp_url, sep = "\t", header = TRUE)

new_order = match(row.names(mut_mat), cancer_signatures$Somatic.Mutation.Type)
new_orders = match(row.names(mut_mats), cancer_signatures$Somatic.Mutation.Type)

cancer_signatures.ns = cancer_signatures[as.vector(new_order),]
cancer_signatures.s = cancer_signatures[as.vector(new_orders),]

row.names(cancer_signatures.ns) = cancer_signatures.ns$Somatic.Mutation.Type
cancer_signatures.ns = as.matrix(cancer_signatures.ns[,4:33])

row.names(cancer_signatures.s) = cancer_signatures.s$Somatic.Mutation.Type
cancer_signatures.s = as.matrix(cancer_signatures.s[,4:33])


#absolute contribution
plot_96_profile(cancer_signatures.ns[,1:2], condensed = TRUE, ymax = 0.3)
fit_res <- fit_to_signatures(mut_mat, cancer_signatures.ns)
select <- which(rowSums(fit_res$contribution) > 10)
plot_contribution(fit_res$contribution[select,], cancer_signatures.ns[,select], coord_flip = FALSE,mode = "absolute")

dev.copy(tiff,"abs contribute non sorted.tiff",width=22, height=8, units="in",res=200) 
dev.off() 

##absolute contribution sorted
plot_96_profile(cancer_signatures.s[,1:2], condensed = TRUE, ymax = 0.3)
fit_ress <- fit_to_signatures(mut_mats, cancer_signatures.s)
selects <- which(rowSums(fit_ress$contribution) > 10)
plot_contribution(fit_ress$contribution[selects,], cancer_signatures.s[,select], coord_flip = FALSE,mode = "absolute")

dev.copy(tiff,"abs contribute sorted.tiff",width=22, height=8, units="in",res=200) 
dev.off() 




#Relative contribution

hclust_cosmic = cluster_signatures(cancer_signatures.ns, method = "average")
cos_sim_samples_signatures = cos_sim_matrix(mut_mat, cancer_signatures.ns)
cosmic_order = colnames(cancer_signatures.ns)[hclust_cosmic$order]

plot_contribution_heatmap(fit_res$contribution,cluster_samples = TRUE,method = "complete")

dev.copy(tiff,"Relative contribute non sorted.tiff",width=10, height=10, units="in",res=200) 
dev.off() 

plot_cosine_heatmap(cos_sim_samples_signatures,col_order = cosmic_order,cluster_rows = TRUE)

dev.copy(tiff,"Cos similarity non sorted.tiff",width=10, height=10, units="in",res=200) 

dev.off() 



##Relative contribution sorted

hclust_cosmics = cluster_signatures(cancer_signatures.s, method = "average")
cos_sim_samples_signaturess = cos_sim_matrix(mut_mats, cancer_signatures.s)
cosmic_orders = colnames(cancer_signatures.s)[hclust_cosmics$order]

plot_contribution_heatmap(fit_ress$contribution,cluster_samples = TRUE,method = "complete")

dev.copy(tiff,"Relative contribute sorted.tiff",width=10, height=10, units="in",res=200) 
dev.off() 

plot_cosine_heatmap(cos_sim_samples_signaturess,col_order = cosmic_orders,cluster_rows = TRUE)

dev.copy(tiff,"Cos similarity sorted.tiff",width=10, height=10, units="in",res=200) 
dev.off() 
