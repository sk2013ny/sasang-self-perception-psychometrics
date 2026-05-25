# Step 2: Pairwise Association Matrix
load("backup_NEW/step1_output.RData")
library(dplyr)
library(tidyr)
library(ggplot2)

cat("========================================================================\n")
cat("Step 2: Pairwise Association Matrix & 10x10 CSV Export\n")
cat("========================================================================\n")

# Helper function to compute Cramer's V and Chi-Square p-value
calc_cramers_v <- function(q1, q2) {
  tab <- table(df_wide[[q1]], df_wide[[q2]])
  # Chi-Square test with Monte Carlo simulation for robustness
  chi_test <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 2000))
  n <- sum(tab)
  r <- nrow(tab)
  c <- ncol(tab)
  v <- as.numeric(sqrt(chi_test$statistic / (n * min(r - 1, c - 1))))
  list(v = v, p = chi_test$p.value)
}

# 1. Compute for all 45 unique pairs
all_pairs <- combn(all_questions, 2, simplify = FALSE)
pairwise_results <- list()

for (i in 1:length(all_pairs)) {
  q1 <- all_pairs[[i]][1]
  q2 <- all_pairs[[i]][2]
  res <- calc_cramers_v(q1, q2)
  
  # Determine type
  is_p1 <- q1 %in% psych_questions
  is_p2 <- q2 %in% psych_questions
  
  type <- if (is_p1 && is_p2) {
    "psych-psych"
  } else if (!is_p1 && !is_p2) {
    "soma-soma"
  } else {
    "psych-soma"
  }
  
  pairwise_results[[i]] <- data.frame(
    q1 = q1,
    q2 = q2,
    Cramers_V = res$v,
    p_value = res$p,
    Type = type,
    stringsAsFactors = FALSE
  )
}

df_all_pairs <- do.call(rbind, pairwise_results)

# Apply FDR correction across all 45 pairwise tests
df_all_pairs$p_adjusted <- p.adjust(df_all_pairs$p_value, method = "BH")
df_all_pairs$Significant <- ifelse(df_all_pairs$p_adjusted < 0.05, "*", "")

# 2. Construct symmetric 10x10 matrices
cramers_v_matrix <- matrix(1.0, nrow = 10, ncol = 10, dimnames = list(all_questions, all_questions))
p_adj_matrix <- matrix(NA_real_, nrow = 10, ncol = 10, dimnames = list(all_questions, all_questions))

for (i in 1:nrow(df_all_pairs)) {
  q1 <- df_all_pairs$q1[i]
  q2 <- df_all_pairs$q2[i]
  v  <- df_all_pairs$Cramers_V[i]
  p  <- df_all_pairs$p_adjusted[i]
  
  cramers_v_matrix[q1, q2] <- v
  cramers_v_matrix[q2, q1] <- v
  
  p_adj_matrix[q1, q2] <- p
  p_adj_matrix[q2, q1] <- p
}

# Save matrices as CSV files
write.csv(cramers_v_matrix, "pairwise_cramers_v_10x10.csv", row.names = TRUE)
write.csv(p_adj_matrix, "pairwise_p_adj_10x10.csv", row.names = TRUE)

cat("10x10 Cramer's V Matrix 저장 완료: pairwise_cramers_v_10x10.csv\n")
cat("10x10 FDR-adjusted P-value Matrix 저장 완료: pairwise_p_adj_10x10.csv\n")

# Save grouping table
grouping_table <- df_all_pairs %>%
  select(Question_1 = q1, Question_2 = q2, Category = Type, Cramers_V, P_Value = p_value, FDR_Adj_P = p_adjusted)
write.csv(grouping_table, "cramers_v_groupings.csv", row.names = FALSE)
cat("Grouping table 저장 완료: cramers_v_groupings.csv\n")

# Extract Cross-Domain (Psych-Soma) for plotting and FDR analysis
df_cross <- df_all_pairs %>%
  filter(Type == "psych-soma") %>%
  mutate(
    Psych = ifelse(q1 %in% psych_questions, q1, q2),
    Soma = ifelse(q1 %in% soma_questions, q1, q2)
  )

# Enforce factor levels for custom ordering on axes
df_cross <- df_cross %>%
  mutate(
    Psych = factor(Psych, levels = c("Q9", "Q7", "Q6", "Q3", "Q1")),
    Soma = factor(Soma, levels = c("Q2", "Q4", "Q5", "Q8", "Q10"))
  )

# Print top 5 associations
cat("\n--- 상위 5개 심리-신체 교차 상관 문항 쌍 ---\n")
df_cross %>%
  arrange(desc(Cramers_V)) %>%
  head(5) %>%
  select(Psych, Soma, Cramers_V, p_value, p_adjusted) %>%
  print()

# 3. Figure 2A: Cross-Domain Heatmap (Refined)
p_heat <- ggplot(df_cross, aes(x = Soma, y = Psych, fill = Cramers_V)) +
  geom_tile(color = "white", linewidth = 0.6) +
  # Draw a bold red border around top 3 associations
  geom_tile(data = subset(df_cross, (Psych == "Q1" & Soma == "Q4") | 
                                    (Psych == "Q9" & Soma == "Q4") | 
                                    (Psych == "Q1" & Soma == "Q2")),
            color = "#E74C3C", fill = NA, linewidth = 1.3) +
  geom_text(aes(label = sprintf("%.3f%s", Cramers_V, Significant)), 
            color = "black", fontface = "bold", size = 4.5) +
  scale_fill_gradient(low = "#F4F6F7", high = "#3498DB", limits = c(0, 0.3)) +
  labs(
    x = "Somatic items",
    y = "Psychological items",
    fill = "Cramer's V",
    caption = "* FDR-adjusted p < 0.05"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold", color = "black", size = 11),
    plot.caption = element_text(size = 9, face = "italic", color = "gray30", hjust = 1.0),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave("Fig2A_PairwiseHeatmap.png", p_heat, width = 7, height = 6, dpi = 300)
cat("Figure 2A 저장 완료: Fig2A_PairwiseHeatmap.png\n")

# Save outputs for next steps
save(df_all_pairs, df_cross, file = "backup_NEW/step2_output.RData")
cat("Step 2 완료: backup_NEW/step2_output.RData 저장 완료\n")
cat("========================================================================\n")
