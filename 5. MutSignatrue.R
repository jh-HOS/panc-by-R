install.packages("BiocManager")
BiocManager::install("signeR")
BiocManager::install("BSgenome.Hsapiens.UCSC.hg19")

library(rtracklayer)
library(signeR)
library(VariantAnnotation)
library(BSgenome.Hsapiens.UCSC.hg19)

vcf1 <- readVcf("E:/R/pancchr/raw/VCF/KCLB17ES257.vcf", "hg19")
mut1 <- genCount
