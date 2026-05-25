# Step 3: Bootstrap Cramer's V and Delta V
load("backup_NEW/step1_output.RData")
library(dplyr)
library(tidyr)
library(ggplot2)

cat("========================================================================\n")
cat("Step 3: Bootstrap Cramer's V and Delta V\n")
cat("========================================================================\n")

# Setup pairs
psych_pairs <- combn(psych_questions, 2, simplify = FALSE)
soma_pairs  <- combn(soma_questions, 2, simplify = FALSE)
cross_pairs <- expand.grid(psych_questions, soma_questions, stringsAsFactors = FALSE)

# Helper function to compute average Cramer's V
compute_avg_v <- function(data, pairs, is_cross = FALSE) {
  v_vals <- numeric(length(pairs))
  if (is_cross) {
    for (i in 1:nrow(pairs)) {
      tab <- table(data[[pairs[i, 1]]], data[[pairs[i, 2]]])
      n <- sum(tab)
      r <- nrow(tab)
      c <- ncol(tab)
      chi <- suppressWarnings(chisq.test(tab))$statistic
      v_vals[i] <- sqrt(chi / (n * min(r - 1, c - 1)))
    }
  } else {
    for (i in 1:length(pairs)) {
      tab <- table(data[[pairs[[i]][1]]], data[[pairs[[i]][2]]])
      n <- sum(tab)
      r <- nrow(tab)
      c <- ncol(tab)
      chi <- suppressWarnings(chisq.test(tab))$statistic
      v_vals[i] <- sqrt(chi / (n * min(r - 1, c - 1)))
    }
  }
  mean(v_vals, na.rm = TRUE)
}

# Run Bootstrap
B <- 2000
N <- nrow(df_wide)

boot_psych <- numeric(B)
boot_soma  <- numeric(B)
boot_cross <- numeric(B)
boot_diff1 <- numeric(B) # Psych-Psych minus Psych-Soma
boot_diff2 <- numeric(B) # Soma-Soma minus Psych-Soma

set.seed(42)
for (b in 1:B) {
  # Sample with replacement
  boot_idx <- sample(N, N, replace = TRUE)
  boot_df <- df_wide[boot_idx, ]
  
  # Compute averages
  v_p <- compute_avg_v(boot_df, psych_pairs, is_cross = FALSE)
  v_s <- compute_avg_v(boot_df, soma_pairs, is_cross = FALSE)
  v_c <- compute_avg_v(boot_df, cross_pairs, is_cross = TRUE)
  
  boot_psych[b] <- v_p
  boot_soma[b]  <- v_s
  boot_cross[b] <- v_c
  boot_diff1[b] <- v_p - v_c
  boot_diff2[b] <- v_s - v_c
}

# Calculate 95% Confidence Intervals
get_stats <- function(boot_dist) {
  ci <- quantile(boot_dist, probs = c(0.025, 0.975))
  data.frame(
    Mean = mean(boot_dist),
    Lower = ci[1],
    Upper = ci[2]
  )
}

stats_psych <- get_stats(boot_psych)
stats_soma  <- get_stats(boot_soma)
stats_cross <- get_stats(boot_cross)
stats_diff1 <- get_stats(boot_diff1)
stats_diff2 <- get_stats(boot_diff2)

cat("\n--- 부트스트랩 평균 및 95% 신뢰구간 (B = 2000) ---\n")
cat("Psych-Psych (영역 내 심리):", round(stats_psych$Mean, 4), "[", round(stats_psych$Lower, 4), ",", round(stats_psych$Upper, 4), "]\n")
cat("Soma-Soma (영역 내 신체):  ", round(stats_soma$Mean, 4), "[", round(stats_soma$Lower, 4), ",", round(stats_soma$Upper, 4), "]\n")
cat("Psych-Soma (영역 간 교차):  ", round(stats_cross$Mean, 4), "[", round(stats_cross$Lower, 4), ",", round(stats_cross$Upper, 4), "]\n")

cat("\n--- 두 영역의 차이 분석 (Delta V) ---\n")
cat("Delta 1 (Psych-Psych - Psych-Soma):", round(stats_diff1$Mean, 4), "[", round(stats_diff1$Lower, 4), ",", round(stats_diff1$Upper, 4), "]\n")
cat("Delta 2 (Soma-Soma - Psych-Soma):  ", round(stats_diff2$Mean, 4), "[", round(stats_diff2$Lower, 4), ",", round(stats_diff2$Upper, 4), "]\n")

# 1. Figure 1B: Point-Range Plot of Mean Cramer's V (Styled)
df_fig1b <- data.frame(
  Association = factor(c("Psych-Psych", "Soma-Soma", "Psych-Soma"), 
                       levels = c("Psych-Psych", "Soma-Soma", "Psych-Soma")),
  Mean = c(stats_psych$Mean, stats_soma$Mean, stats_cross$Mean),
  Lower = c(stats_psych$Lower, stats_soma$Lower, stats_cross$Lower),
  Upper = c(stats_psych$Upper, stats_soma$Upper, stats_cross$Upper)
)

p_1b <- ggplot(df_fig1b, aes(x = Association, y = Mean, color = Association)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, linewidth = 1.0) +
  geom_point(aes(fill = Association), size = 5.0, shape = 21, color = "white", stroke = 1.5) +
  scale_color_manual(values = c("Psych-Psych" = "#E74C3C", "Soma-Soma" = "#3498DB", "Psych-Soma" = "#95A5A6")) +
  scale_fill_manual(values = c("Psych-Psych" = "#E74C3C", "Soma-Soma" = "#3498DB", "Psych-Soma" = "#95A5A6")) +
  labs(
    x = "Association Type",
    y = "Mean Cramer's V"
  ) +
  scale_y_continuous(limits = c(0.10, 0.22), breaks = seq(0.10, 0.22, by = 0.02)) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank()
  )

ggsave("Fig1B_MeanCramersV.png", p_1b, width = 6, height = 5, dpi = 300)
cat("\nFigure 1B 저장 완료: Fig1B_MeanCramersV.png\n")

# 2. Figure 1C: Forest Plot of Bootstrap Delta V (Contrast Plot)
df_fig1c <- data.frame(
  Contrast = factor(c("Psych-Psych - Psych-Soma", "Soma-Soma - Psych-Soma"),
                    levels = c("Soma-Soma - Psych-Soma", "Psych-Psych - Psych-Soma")),
  Mean = c(stats_diff1$Mean, stats_diff2$Mean),
  Lower = c(stats_diff1$Lower, stats_diff2$Lower),
  Upper = c(stats_diff1$Upper, stats_diff2$Upper)
)

p_1c <- ggplot(df_fig1c, aes(x = Mean, y = Contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.12, linewidth = 1.0, color = "#2C3E50") +
  geom_point(size = 4.5, color = "#2C3E50", fill = "white", shape = 21, stroke = 1.5) +
  labs(
    x = expression(paste(Delta, " Cramer's V (Bootstrap 95% CI)")),
    y = "Contrast"
  ) +
  scale_x_continuous(limits = c(-0.005, 0.075), breaks = seq(0.0, 0.07, by = 0.01)) +
  theme_bw(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    panel.grid.minor = element_blank()
  )

ggsave("Fig1C_BootstrapDeltaV.png", p_1c, width = 6, height = 4, dpi = 300)
cat("\nFigure 1C 저장 완료: Fig1C_BootstrapDeltaV.png\n")

# Save output
save(stats_psych, stats_soma, stats_cross, stats_diff1, stats_diff2, file = "backup_NEW/step3_output.RData")
cat("\nStep 3 완료: backup_NEW/step3_output.RData 저장 완료\n")
cat("========================================================================\n")
