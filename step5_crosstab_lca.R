# Step 5: Cross-Tabulation of Psych and Soma Classes
load("backup_NEW/step1_output.RData")
load("backup_NEW/step4_output.RData")
library(dplyr)
library(tidyr)
library(ggplot2)

cat("========================================================================\n")
cat("Step 5: Cross-Tabulation of Psych and Soma Classes (Neutral Labeling)\n")
cat("========================================================================\n")

# 1. Assign modal classes
df_wide$psych_class <- psych_lca_res$best_model$predclass
df_wide$soma_class  <- soma_lca_res$best_model$predclass

# 2. Build contingency table
cross_tab <- table(df_wide$psych_class, df_wide$soma_class)
rownames(cross_tab) <- c("Psych_Class1", "Psych_Class2")
colnames(cross_tab) <- c("Soma_Class1", "Soma_Class2")

cat("\n--- 모달 클래스 배정 빈도 교차표 (Neutral Labels) ---\n")
print(cross_tab)

# Proportions
pct_tab <- prop.table(cross_tab)
pct_tab_print <- round(pct_tab * 100, 1)
cat("\n--- 모달 클래스 배정 백분율 교차표 (%) ---\n")
print(pct_tab_print)

# 3. Calculate observed vs expected mixed/concordant profiles
p_p1 <- sum(df_wide$psych_class == 1) / nrow(df_wide)
p_p2 <- sum(df_wide$psych_class == 2) / nrow(df_wide)
p_s1 <- sum(df_wide$soma_class == 1) / nrow(df_wide)
p_s2 <- sum(df_wide$soma_class == 2) / nrow(df_wide)

# Observed
obs_concordant <- pct_tab[1, 1] + pct_tab[2, 2]
obs_mixed      <- pct_tab[1, 2] + pct_tab[2, 1]

# Expected under independence
exp_concordant <- (p_p1 * p_s1) + (p_p2 * p_s2)
exp_mixed      <- (p_p1 * p_s2) + (p_p2 * p_s1)

cat("\n--- 결합 양상 요약 (Observed vs Expected) ---\n")
cat("Observed Concordant (정합형 관찰치):  ", round(obs_concordant * 100, 1), "%\n")
cat("Observed Mixed (혼합형 관찰치):       ", round(obs_mixed * 100, 1), "%\n")
cat("Expected Mixed under Independence:   ", round(exp_mixed * 100, 1), "%\n")

# 4. Run Chi-Square and Odds Ratio
chi_test <- chisq.test(cross_tab)
phi <- as.numeric(sqrt(chi_test$statistic / sum(cross_tab)))
odds_ratio <- (cross_tab[1, 1] * cross_tab[2, 2]) / (cross_tab[1, 2] * cross_tab[2, 1])

cat("\n--- 통계적 연관성 검정 ---\n")
cat("Chi-Square statistic:", round(chi_test$statistic, 3), "p-value:", round(chi_test$p.value, 4), "\n")
cat("Phi Coefficient (Cramer's V):", round(phi, 4), "\n")
cat("Odds Ratio (OR):", round(odds_ratio, 4), "\n")

# 5. Figure 1D: 2x2 Heatmap of observed proportions (Revised with Interpretations)
df_fig1d <- as.data.frame(pct_tab) %>%
  rename(Psych = Var1, Soma = Var2, Percent = Freq) %>%
  mutate(
    Percent_Val = Percent * 100,
    # Map to clear, neutral class labels with academic interpretations as requested
    Psych = factor(ifelse(Psych == "Psych_Class1", "Psych Class 1\n(Active/outward)", "Psych Class 2\n(Passive/inward)")),
    Soma = factor(ifelse(Soma == "Soma_Class1", "Soma Class 1\n(Non-sensitive)", "Soma Class 2\n(SoEum-like)"))
  )

# Add diagonal/off-diagonal labels
df_fig1d <- df_fig1d %>%
  mutate(
    Type = ifelse((Psych == "Psych Class 1\n(Active/outward)" & Soma == "Soma Class 1\n(Non-sensitive)") |
                  (Psych == "Psych Class 2\n(Passive/inward)" & Soma == "Soma Class 2\n(SoEum-like)"),
                  "Concordant", "Mixed"),
    Cell_Label = sprintf("%.1f%%\n[%s]", Percent_Val, Type)
  )

p_1d <- ggplot(df_fig1d, aes(x = Soma, y = Psych, fill = Percent_Val)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = Cell_Label), color = "black", fontface = "bold", size = 4.8) +
  scale_fill_gradient(low = "#F4F7F9", high = "#34495E", limits = c(0, 50)) +
  labs(
    x = "Somatic Latent Classes (Body Domain)",
    y = "Psychological Latent Classes (Mind Domain)",
    fill = "Observed %",
    title = "Cross-Tabulation of Psych and Soma Latent Classes",
    subtitle = "Observed Mixed Profiles = 55.0%  (Expected under Independence = 49.6%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12.5, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 10.5, color = "#2C3E50", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11.5),
    axis.text = element_text(face = "bold", color = "black", size = 10),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave("Fig1D_LCA_Crosstab.png", p_1d, width = 6.5, height = 5.5, dpi = 300)
cat("\nFigure 1D 저장 완료: Fig1D_LCA_Crosstab.png\n")

# Save output and run post-hoc bootstrap analyses
cat("\n========================================================================\n")
cat("Step 5 Post-Hoc: Bootstrap CIs of Mixed Profiles (B = 2000)\n")
cat("========================================================================\n")

# Load step 4 results to get posteriors for weighted analysis
load("backup_NEW/step4_output.RData")

# Extract posteriors for weighted analysis
p_psych <- psych_lca_res$best_model$posterior  # N x 2
p_soma  <- soma_lca_res$best_model$posterior   # N x 2
mixed_prob <- p_psych[, 1] * p_soma[, 2] + p_psych[, 2] * p_soma[, 1]

# Set up bootstrap parameters
B <- 2000
N <- nrow(df_wide)

boot_observed <- numeric(B)
boot_expected <- numeric(B)
boot_excess   <- numeric(B)
boot_weighted <- numeric(B)

set.seed(42)
for (b in 1:B) {
  # Sample with replacement
  boot_idx <- sample(N, N, replace = TRUE)
  boot_df <- df_wide[boot_idx, ]
  
  # 1. Observed Mixed (off-diagonal)
  obs_mix <- mean(boot_df$psych_class != boot_df$soma_class)
  
  # 2. Expected Mixed under independence
  p_p1 <- mean(boot_df$psych_class == 1)
  p_p2 <- mean(boot_df$psych_class == 2)
  p_s1 <- mean(boot_df$soma_class == 1)
  p_s2 <- mean(boot_df$soma_class == 2)
  
  exp_mix <- (p_p1 * p_s2) + (p_p2 * p_s1)
  
  # 3. Excess Mixed
  excess <- obs_mix - exp_mix
  
  # 4. Posterior-Weighted Mixed
  obs_weight <- mean(mixed_prob[boot_idx])
  
  boot_observed[b] <- obs_mix
  boot_expected[b] <- exp_mix
  boot_excess[b]   <- excess
  boot_weighted[b] <- obs_weight
}

# Calculate 95% CIs
get_stats <- function(dist) {
  ci <- quantile(dist, probs = c(0.025, 0.975))
  data.frame(
    Mean = mean(dist),
    Lower = ci[1],
    Upper = ci[2],
    row.names = NULL
  )
}

stats_obs  <- get_stats(boot_observed)
stats_exp  <- get_stats(boot_expected)
stats_ex   <- get_stats(boot_excess)
stats_obs_weighted <- get_stats(boot_weighted)

cat("\n--- Bootstrap LCA Mixed Profile Results (B = 2000) ---\n")
cat(sprintf("Observed Mixed Proportion: %6.2f%% [95%% CI: %6.2f%%, %6.2f%%]\n", 
            stats_obs$Mean * 100, stats_obs$Lower * 100, stats_obs$Upper * 100))
cat(sprintf("Expected Mixed Proportion: %6.2f%% [95%% CI: %6.2f%%, %6.2f%%]\n", 
            stats_exp$Mean * 100, stats_exp$Lower * 100, stats_exp$Upper * 100))
cat(sprintf("Excess Mixed Proportion:   %6.2f%% [95%% CI: %6.2f%%, %6.2f%%]\n", 
            stats_ex$Mean * 100, stats_ex$Lower * 100, stats_ex$Upper * 100))
cat(sprintf("Posterior-Weighted Mixed:  %6.2f%% [95%% CI: %6.2f%%, %6.2f%%]\n", 
            stats_obs_weighted$Mean * 100, stats_obs_weighted$Lower * 100, stats_obs_weighted$Upper * 100))

# Save output including bootstrap stats
save(df_wide, cross_tab, pct_tab, obs_concordant, obs_mixed, exp_mixed, chi_test, phi, odds_ratio,
     stats_obs, stats_exp, stats_ex, stats_obs_weighted, file = "backup_NEW/step5_output.RData")
cat("\nStep 5 완료 (Post-Hoc Bootstrap 포함): backup_NEW/step5_output.RData 저장 완료\n")
cat("========================================================================\n")
