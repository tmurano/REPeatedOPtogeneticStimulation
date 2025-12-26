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

library(EnhancedVolcano)
library(dplyr)

############################################
# Set Working Directory
############################################
setwd("~/Desktop/")

############################################
# Read MouseInfo file and count file
############################################
info <- read.delim(
  "ATAC_Mouse_Info.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  colClasses = rep("character", 4)
)

colnames(info) <- c(
  "MouseName", "StimType", "OF.15min", "CSI.index",
  "LongTermDemat", "Opt.Stim"
)

count <- read.csv("integr.Peak.csv", header = TRUE)
gene <- count[, 1]
count <- count[, 6:17]
colnames(count) <- info$MouseName
row.names(count) <- gene

############################################
# Mouse groups compared
############################################
typ <- "Stimx3.2wks"
typ1 <- info[5:8, ]

typ <- "Stimx10.2wks"
typ1 <- info[9:12, ]

data <- rbind(info[1:4, ], typ1)
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
# Draw Volcano plot  (PDF)
############################################
pdf(
  paste0("Volcano_", typ, ".pdf"),
  width = 7,
  height = 8.5
)

EnhancedVolcano(
  toptable = res,   
  x = "log2FoldChange",
  y = "padj",
  lab = rownames(res),
  xlim = c(-4, 4),
  ylim = c(0, 30),
  pCutoff = 0.05,
  FCcutoff = log(100, 2),
  pointSize = 0.1,
  selectLab = c(""),
  title = "EnhancedVolcano \n (FC cutoff=0, padj cutoff=0.05)"
)

dev.off()

############################################
# Making Gene List
############################################
diff_all <- res %>%
  as.data.frame() %>%
  rownames_to_column("peak") %>%
  filter(padj < 1) %>%
  arrange(desc(log2FoldChange), desc(padj))

write.csv(
  diff_all,
  paste(c(typ, "_AllPeakList.csv"), collapse = "")
)

############################################
# End of program
############################################
