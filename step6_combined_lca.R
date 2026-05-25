# Step 6: Combined 10-item LCA
load("backup_NEW/step1_output.RData")
library(poLCA)
library(dplyr)
library(tidyr)

cat("========================================================================\n")
cat("Step 6: Combined 10-item LCA & Class Quality Metrics\n")
cat("========================================================================\n")

# poLCA formula for all 10 items
f_all <- as.formula(paste("cbind(", paste(all_questions, collapse = ","), ") ~ 1"))

# Run combined LCA models for K = 1 to 8
cat("\n--- Running Combined LCA for K = 1 to 8 ---\n")
lca_summary <- list()
best_bic <- Inf
best_model <- NULL

for (k in 1:8) {
  set.seed(42)
  # nrep=10 for global optimization
  fit <- tryCatch({
    poLCA(f_all, data = df_lca, nclass = k, maxiter = 3000, graphs = FALSE, verbose = FALSE, nrep = 10)
  }, error = function(e) {
    NULL
  })
  
  if (is.null(fit)) next
  
  # Calculate Entropy
  p_mat <- fit$posterior
  entropy <- if (k > 1) {
    1 - (sum(-p_mat * log(p_mat + 1e-12)) / (nrow(p_mat) * log(k)))
  } else {
    NA_real_
  }
  
  # Calculate Mean Posterior Probability
  mean_posterior <- if (k > 1) {
    mean(apply(p_mat, 1, max))
  } else {
    1.0
  }
  
  # Store metrics
  lca_summary[[k]] <- data.frame(
    K = k,
    AIC = fit$aic,
    BIC = fit$bic,
    Entropy = entropy,
    Smallest_Class_Prop = min(fit$P),
    Mean_Posterior_Prob = mean_posterior,
    stringsAsFactors = FALSE
  )
  
  # Track best model by BIC
  if (fit$bic < best_bic) {
    best_bic <- fit$bic
    best_model <- fit
  }
}

summary_df <- do.call(rbind, lca_summary)
print(round(summary_df, 4))

# Find class sizes for optimal K = 3
cat("\n--- 최적 결합 잠재모델 클래스 비율 (K =", length(best_model$P), ") ---\n")
for (i in 1:length(best_model$P)) {
  cat(sprintf("Combined Class %d: %.1f%%\n", i, best_model$P[i] * 100))
}

# Save output
save(summary_df, best_model, file = "backup_NEW/step6_output.RData")
cat("\nStep 6 완료: backup_NEW/step6_output.RData 저장 완료\n")
cat("========================================================================\n")
