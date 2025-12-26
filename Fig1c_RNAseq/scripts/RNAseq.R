rm(list = ls())

############################################
# Packages
############################################
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("DESeq2")
library(DESeq2)

install.packages("tidyverse")
library(tidyverse)

BiocManager::install("EnhancedVolcano")
library(EnhancedVolcano)

install.packages("dplyr")
library(dplyr)

############################################
# Set Working Directory
############################################
setwd("~/Desktop/Fig1c_RNAseq")

############################################
# Read MouseInfo file and count file
############################################
info <- read.delim(
  "Mouse_Info.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  colClasses = rep("character", 4)
)

colnames(info) <- c(
  "MouseName", "StimType", "OF.15min", "CSI.index",
  "Opt.Stim", "24hrs.Sampling"
)

count <- read.delim("FPKM_count.txt", header = TRUE, stringsAsFactors = FALSE)
gene <- count[, 1]
count <- count[, -1]
colnames(count) <- info$MouseName
row.names(count) <- gene

############################################
# Mouse groups compared
############################################
typ <- "Stimx3.2wks"
typ1 <- info[7:12, ]

typ <- "Stimx10.2wks"
typ1 <- info[13:18, ]

typ <- "Stimx3.24hrs"
typ1 <- info[19:24, ]

typ <- "Stimx10.24hrs"
typ1 <- info[25:30, ]

data <- rbind(info[1:6, ], typ1)
count_filtered <- count[, colnames(count) %in% data$MouseName]

############################################
# Creation of the DESeqDataSet
############################################
dds <- DESeqDataSetFromMatrix(
  countData = count_filtered,
  colData = data,
  design = ~ StimType
)

############################################
# Running the DE analysis
############################################
dds2 <- DESeq(dds)
res <- results(dds2)

resLFC <- lfcShrink(
  dds = dds2,
  res = res,
  type = "normal",
  coef = 2
)

############################################
# Draw Volcano plot
############################################
pdf(paste0("Volcano_", typ, ".pdf"), width = 7, height = 7)

EnhancedVolcano(
  toptable = resLFC,
  x = "log2FoldChange",
  y = "padj",
  lab = rownames(resLFC),
  xlim = c(-1.1, 1.1),
  ylim = c(0, 25),
  pCutoff = 0.05,
  FCcutoff = log(1.2, 2),
  pointSize = 0.5,
  labSize = 10.0,
  selectLab = c("")
)

dev.off()

############################################
# Making Gene List
############################################
diff_all <- res %>%
  as.data.frame() %>%
  rownames_to_column("genes") %>%
  filter(padj < 1) %>%
  arrange(desc(log2FoldChange), desc(padj))

write.csv(
  diff_all,
  paste(c(typ, "_AllGeneList.csv"), collapse = "")
)

############################################
# End of program
############################################
