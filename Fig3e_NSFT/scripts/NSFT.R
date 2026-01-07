rm(list = ls())

############################################
# Packages
############################################
pkgs_cran <- c(
  "ggplot2",
  "dplyr",
  "readxl",
  "survival",
  "openxlsx",
  "broom",
  "tibble"
)

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
df <- read_excel("NSFT_Results.xlsx", sheet = "NSFT")

df$StimType <- factor(df$StimType, levels = c("NoStim", "Stimx3", "Stimx10"))
color_map <- c("NoStim" = "gray30", "Stimx3" = "blue", "Stimx10" = "red")

############################################
# Kaplan–Meier plot
############################################
surv_obj <- Surv(time = df$OF_Latency_sec, event = 1 - df$Censoring)
fit <- survfit(surv_obj ~ StimType, data = df)

df_km <- broom::tidy(fit) %>%
  mutate(StimType = gsub("StimType=", "", strata))

df_km0 <- df_km %>%
  group_by(StimType) %>%
  slice(1) %>%
  mutate(time = 0, estimate = 1, n.event = 0, n.censor = 0)

df_km <- bind_rows(df_km0, df_km) %>%
  arrange(StimType, time)

p2 <- ggplot(df_km, aes(x = time, y = estimate, color = StimType)) +
  geom_step(linewidth = 0.7) +
  geom_point(
    data = subset(df_km, n.event > 0 | n.censor > 0),
    aes(x = time, y = estimate, color = StimType),
    size = 3
  ) +
  scale_color_manual(values = color_map) +
  scale_x_continuous(
    breaks = seq(0, 620, 100),
    minor_breaks = seq(0, 620, 20),
    limits = c(-20, 620),
    expand = c(0, 0)
  ) +
  scale_y_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) +
  labs(x = "Time (s)", y = "Cum. Survival") +
  theme(
    text = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    legend.position = "none"
  )

ggsave("NSFT_KaplanMeier.png", p2, width = 4, height = 3, dpi = 300)

############################################
# Log-rank tests
############################################
fit_all <- survdiff(surv_obj ~ StimType, data = df)

logrank_all <- data.frame(
  Comparison = "All groups",
  ChiSq = fit_all$chisq,
  df = length(fit_all$n) - 1,
  p_value = 1 - pchisq(fit_all$chisq, length(fit_all$n) - 1)
)

pairs <- list(
  c("NoStim", "Stimx3"),
  c("NoStim", "Stimx10"),
  c("Stimx3", "Stimx10")
)

logrank_pairs <- do.call(rbind, lapply(pairs, function(pair) {
  df_sub <- subset(df, StimType %in% pair)
  surv_obj_sub <- Surv(time = df_sub$OF_Latency_sec, event = 1 - df_sub$Censoring)
  fit_pair <- survdiff(surv_obj_sub ~ StimType, data = df_sub)
  data.frame(
    Comparison = paste(pair, collapse = " vs "),
    ChiSq = fit_pair$chisq,
    df = length(fit_pair$n) - 1,
    p_value = 1 - pchisq(fit_pair$chisq, length(fit_pair$n) - 1)
  )
})) 

logrank_df <- rbind(logrank_all, logrank_pairs)

############################################
# ANCOVA
############################################
ancova_model <- aov(OF_Latency_sec ~ StimType + OF_Distance_cm, data = df)
ancova_df <- summary(ancova_model)[[1]] %>%
  as.data.frame() %>%
  rownames_to_column("Factor") %>%
  select(Factor, everything())

############################################
# Export to Excel
############################################
wb <- createWorkbook()
addWorksheet(wb, "Logrank")
addWorksheet(wb, "ANCOVA")
writeData(wb, "Logrank", logrank_df)
writeData(wb, "ANCOVA", ancova_df)
saveWorkbook(wb, "NSFT_Stats.xlsx", overwrite = TRUE)

############################################
# End of program
############################################
