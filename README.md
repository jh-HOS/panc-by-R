# Pancreatic Cancer WES & RNA-seq Analysis Pipeline

## Overview
Analysis pipeline for mutational characterization of 26 
patient-derived pancreatic cancer cell lines established 
from Seoul National University Hospital.

Integrates WES, RNA-seq, and drug sensitivity data to 
identify genomic subtypes and predict anti-cancer drug 
reactivity.

## Key Findings
- KRAS codon 12 mutation: 85% (22/26 cell lines)
- 3 molecular subgroups: KRAS WT / KRAS mut / 
  KRAS mut + HER2 overexpression
- PI3K/AKT and MAPK signaling pathways most disrupted
- Trametinib sensitivity positively correlated with 
  p-ERK1/2 expression in KRAS mutant cell lines

## Workflow
VCF loading → Impact/Effect filtering → 
ClinVar + Cancer Gene Census annotation → 
CCLE comparison → OncoPrint → 
Reactome pathway analysis → Drug AUC correlation → 
Mutational signature analysis

## Data
- 26 patient-derived cell lines (21 tissue, 5 organoid)
- WES: SureSelect V5, HiSeq 2500, 100x depth
- RNA-seq: 51M reads, fusion gene analysis
- Drug sensitivity: 18 anti-cancer agents (CTG/WST1 assay)

## Tools & Databases
R 3.6.3, GATK, BWA, SnpEff  
ClinVar, Cancer Gene Census (COSMIC), CCLE, Reactome

## Publication
Maeng et al. (2025) Cancer Cell International
"Comprehensive molecular analysis of 26 newly established 
human pancreatic ductal adenocarcinoma cell lines"
doi: 10.1186/s12935-025-03671-8

**Co-author (Investigation, Data curation)**
- Established 26 patient-derived PDAC cell lines
- WES and drug sensitivity data generation
