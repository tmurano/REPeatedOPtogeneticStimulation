rm(list = ls())

############################################
# Packages
############################################
pkgs_cran <- c("openxlsx", "readxl", "ggplot2", "tidyr", "dplyr", "reshape2", "rstatix")

for (pkg in pkgs_cran) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

############################################
# Set Working Directory
############################################
setwd("~/Desktop/")


############################################
# Read data
############################################
data <- read_excel("HomeCage_Results.xlsx")
data <- data[5290:nrow(data), 2:29]  # 2024/03/18 7:00-

############################################
# Summarize data (min -> hour -> day)
############################################
c <- ncol(data)
r <- floor(nrow(data) / 60)

Data <- matrix(NA, nrow = r, ncol = c)
for (i in 1:r) {
  Data[i, ] <- colSums(data[(i * 60 - 59):(i * 60), ])
}
Data <- as.data.frame(Data)

c <- ncol(Data)
r <- floor(nrow(Data) / 24)

Data1 <- matrix(NA, nrow = r, ncol = c)
for (i in 1:r) {
  Data1[i, ] <- colSums(Data[(i * 24 - 23):(i * 24), ])
}
Data1 <- as.data.frame(Data1)

colnames(Data1) <- colnames(data)
labels <- c(sprintf("1.Habit%02d", 1:3), sprintf("2.Stim%02d", 1:10), sprintf("3.Day%02d", 1:(r - 13)))
rownames(Data1) <- labels

############################################
# Clean up days and missing rows
############################################
Data1 <- Data1[-c(1, 2, 3), ]
Data1[c(28, 29, 37, 47, 52), ] <- NA
r <- r - 3

############################################
# Compute mean and SEM
############################################
sem <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

Data_sem <- cbind(
  apply(Data1[, 1:10], 1, sem),
  apply(Data1[, 11:18], 1, sem),
  apply(Data1[, 19:28], 1, sem)
)

Data_mean <- cbind(
  apply(Data1[, 1:10], 1, mean, na.rm = TRUE),
  apply(Data1[, 11:18], 1, mean, na.rm = TRUE),
  apply(Data1[, 19:28], 1, mean, na.rm = TRUE)
)

############################################
# Create plot dataframe
############################################
df <- data.frame(
  Days = 1:r,
  StimType = rep(c("NoStim", "Stimx3+2wks", "Stimx10+2wks"), each = r),
  Mean = melt(Data_mean)[, 3],
  SEM  = melt(Data_sem)[, 3]
)

############################################
# Draw and save plot
############################################
g <- ggplot(df, aes(x = Days, y = Mean, color = StimType, group = StimType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(
    aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = StimType, colour = StimType),
    linetype = "blank", alpha = 0.3
  ) +
  theme_bw(base_size = 30) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text = element_text(colour = "black")
  ) +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  scale_x_continuous(expand = c(0, 0), limits = c(1, 51)) +
  scale_y_continuous(expand = c(0, 0), limits = c(10000, NA))

plot(g)
ggsave("HomeCageDist.png", g, width = 8, height = 5, dpi = 300)

############################################
# Day-wise post hoc tests (main analysis)
############################################
data_df <- Data1[1:51, ]
data_df$days <- 1:51

data_long <- pivot_longer(
  data_df,
  cols = -days,
  names_to = "subject",
  values_to = "Distance"
)

data_long$StimType <- rep(
  c(rep("NoStim", 10), rep("Stimx3+2wks", 8), rep("Stimx10+2wks", 10)),
  51
)

pwc <- data_long %>%
  group_by(days) %>%
  pairwise_t_test(
    Distance ~ StimType,
    paired = FALSE,
    p.adjust.method = "bonferroni"
  )

############################################
# Save results to Excel
############################################
write.xlsx(
  x = list(Daywise_Stats = pwc),
  file = "HomeCage_Stats.xlsx",
  rowNames = FALSE
)

############################################
# End of program
############################################
