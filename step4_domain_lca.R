# Step 4: Domain-Specific LCA
load("backup_NEW/step1_output.RData")
library(poLCA)
library(dplyr)
library(tidyr)

cat("========================================================================\n")
cat("Step 4: Domain-Specific LCA & Class Quality Metrics\n")
cat("========================================================================\n")

# poLCA formulas
f_psych <- as.formula(paste("cbind(", paste(psych_questions, collapse = ","), ") ~ 1"))
f_soma  <- as.formula(paste("cbind(", paste(soma_questions, collapse = ","), ") ~ 1"))

# Function to run poLCA for multiple K and return optimal model and summary
run_domain_lca <- function(formula, data, label, max_k = 6) {
  cat("\n--- Running LCA for:", label, "---\n")
  lca_summary <- list()
  best_bic <- Inf
  best_model <- NULL
  
  for (k in 1:max_k) {
    set.seed(42)
    # nrep=10 to avoid local maxima
    fit <- tryCatch({
      poLCA(formula, data = data, nclass = k, maxiter = 3000, graphs = FALSE, verbose = FALSE, nrep = 10)
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
  
  list(summary = summary_df, best_model = best_model, best_k = length(best_model$P))
}

# 1. Run Psychological LCA
psych_lca_res <- run_domain_lca(f_psych, df_lca, "Psychological Domain")

# 2. Run Somatic LCA
soma_lca_res <- run_domain_lca(f_soma, df_lca, "Somatic Domain")

# Print best class proportions (Model-Estimated Class Prevalence)
cat("\n--- 최적 심리 잠재모델 클래스 비율 (K =", psych_lca_res$best_k, ") ---\n")
for (i in 1:psych_lca_res$best_k) {
  cat(sprintf("Psych Class %d: %.1f%%\n", i, psych_lca_res$best_model$P[i] * 100))
}

cat("\n--- 최적 신체 잠재모델 클래스 비율 (K =", soma_lca_res$best_k, ") ---\n")
for (i in 1:soma_lca_res$best_k) {
  cat(sprintf("Soma Class %d: %.1f%%\n", i, soma_lca_res$best_model$P[i] * 100))
}

# Save output
save(psych_lca_res, soma_lca_res, file = "backup_NEW/step4_output.RData")
cat("\nStep 4 완료: backup_NEW/step4_output.RData 저장 완료\n")
cat("========================================================================\n")
