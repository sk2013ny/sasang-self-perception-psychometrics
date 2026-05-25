# Step 8: Supplementary UMAP and Random Forest
load("backup_NEW/step1_output.RData")
if (!requireNamespace("umap", quietly = TRUE)) {
  install.packages("umap", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest", repos = "https://cloud.r-project.org")
}
library(umap)
library(caret)
library(randomForest)
library(dplyr)
library(tidyr)
library(ggplot2)

cat("========================================================================\n")
cat("Step 8: Supplementary UMAP and Random Forest\n")
cat("========================================================================\n")

# 1. Prepare One-Hot Encoded 40-D Fingerprint
q_cols <- paste0("Q", 1:10)
fingerprint_list <- lapply(q_cols, function(q) {
  out <- lapply(sasang_levels, function(lv) {
    setNames(
      data.frame(as.integer(df_wide[[q]] == lv)),
      paste0(q, "_", lv)
    )
  })
  do.call(cbind, out)
})
df_fp <- do.call(cbind, fingerprint_list)

# 2. Run UMAP
cat("\nRunning UMAP dimensionality reduction...")
set.seed(42)
umap_config <- umap.defaults
umap_config$n_neighbors <- 15
umap_config$min_dist <- 0.1
umap_config$metric <- "euclidean"
umap_config$random_state <- 42

umap_res <- umap(df_fp, config = umap_config)
df_umap <- data.frame(
  UMAP1 = umap_res$layout[, 1],
  UMAP2 = umap_res$layout[, 2],
  EI = df_wide$EI,
  soma_group = ifelse(df_wide$actual_sasang == "소음인", "SoEum", "NonSoEum")
)

# Figure S1: UMAP Plot (Supplementary)
p_s1 <- ggplot(df_umap, aes(x = UMAP1, y = UMAP2)) +
  geom_point(aes(color = EI, shape = soma_group), alpha = 0.6, size = 1.8) +
  scale_color_manual(values = c("E" = "#E74C3C", "I" = "#3498DB")) +
  scale_shape_manual(values = c("NonSoEum" = 16, "SoEum" = 17)) +
  labs(
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "MBTI E/I",
    shape = "Soma Group",
    caption = "Supplementary visualization only; not used as diagnostic validation."
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    plot.caption = element_text(size = 9, face = "italic", color = "gray30", hjust = 0.5)
  )

ggsave("FigS1_UMAP_Overlay.png", p_s1, width = 7, height = 5, dpi = 300)
cat("\nFigure S1 저장 완료: FigS1_UMAP_Overlay.png\n")

# 3. Run Random Forest Classification (10-fold repeated CV)
cat("\nTraining Random Forest Models...")
df_ml <- data.frame(df_fp)
df_ml$EI     <- as.factor(df_wide$EI)
df_ml$sasang <- as.factor(df_wide$actual_sasang)
df_ml$soma   <- as.factor(ifelse(df_wide$actual_sasang == "소음인", "SoEum", "NonSoEum"))

# Parse other MBTI dimensions
df_ml$SN     <- as.factor(ifelse(grepl(".S..", df_wide$mbti), "S", "N"))
df_ml$TF     <- as.factor(ifelse(grepl("..T.", df_wide$mbti), "T", "F"))
df_ml$JP     <- as.factor(ifelse(grepl("...J", df_wide$mbti), "J", "P"))

# 10-fold repeated CV configuration
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  savePredictions = "final"
)

run_rf <- function(target, label) {
  cat("\nFitting RF for:", label, "...")
  form <- as.formula(paste(target, "~ ."))
  feat_cols <- names(df_fp)
  train_df <- df_ml[, c(feat_cols, target)]
  
  fit <- train(
    form,
    data = train_df,
    method = "rf",
    trControl = ctrl,
    importance = TRUE
  )
  fit
}

set.seed(42)
rf_EI     <- run_rf("EI", "E/I")
rf_SN     <- run_rf("SN", "S/N")
rf_TF     <- run_rf("TF", "T/F")
rf_JP     <- run_rf("JP", "J/P")
rf_sasang <- run_rf("sasang", "Sasang")
rf_soma   <- run_rf("soma", "Soma")

# Compile performance statistics
rf_results <- data.frame(
  Model = c("E/I", "S/N", "T/F", "J/P", "Sasang", "Soma"),
  Accuracy = round(c(
    mean(rf_EI$resample$Accuracy),
    mean(rf_SN$resample$Accuracy),
    mean(rf_TF$resample$Accuracy),
    mean(rf_JP$resample$Accuracy),
    mean(rf_sasang$resample$Accuracy),
    mean(rf_soma$resample$Accuracy)
  ) * 100, 1),
  Kappa = round(c(
    mean(rf_EI$resample$Kappa),
    mean(rf_SN$resample$Kappa),
    mean(rf_TF$resample$Kappa),
    mean(rf_JP$resample$Kappa),
    mean(rf_sasang$resample$Kappa),
    mean(rf_soma$resample$Kappa)
  ), 3)
)

print(rf_results)

# Figure S2: Random Forest Performance Bar Plot (Supplementary)
df_fig_rf <- rf_results %>%
  pivot_longer(cols = c(Accuracy, Kappa), names_to = "Metric", values_to = "Value")

p_s2 <- ggplot(df_fig_rf, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black", linewidth = 0.5) +
  facet_wrap(~Metric, scales = "free_y") +
  scale_fill_manual(values = c("Accuracy" = "#34495E", "Kappa" = "#16A085")) +
  labs(
    x = "Target Variable",
    y = "Model Score",
    caption = "Supplementary visualization only; not used as diagnostic validation."
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    plot.caption = element_text(size = 9, face = "italic", color = "gray30", hjust = 0.5)
  )

ggsave("FigS2_RF_Performance.png", p_s2, width = 7, height = 4, dpi = 300)
cat("\nFigure S2 저장 완료: FigS2_RF_Performance.png\n")

# Save outputs
save(umap_res, df_umap, rf_results, file = "backup_NEW/step8_output.RData")
cat("\nStep 8 완료: backup_NEW/step8_output.RData 저장 완료\n")
cat("========================================================================\n")
