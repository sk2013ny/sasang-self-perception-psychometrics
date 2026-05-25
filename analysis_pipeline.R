# Master Re-analysis Pipeline
options(width = 120)

if (!dir.exists("backup_NEW")) {
  dir.create("backup_NEW")
}

cat("========================================================================\n")
cat("Executing Comprehensive Re-analysis Pipeline for 983 Participant Dataset\n")
cat("========================================================================\n")

# Run Steps sequentially
source("step1_data_cleaning.R")
source("step1_schematic.R")
source("step2_pairwise_association.R")
source("step3_bootstrap_ci.R")
source("step4_domain_lca.R")
source("step5_crosstab_lca.R")
source("step6_combined_lca.R")
source("step7_categorical_mgm.R")
source("step8_supplementary_umap_rf.R")

# Load final results to print Table 1
load("backup_NEW/step3_output.RData")
load("backup_NEW/step4_output.RData")
load("backup_NEW/step5_output.RData")
load("backup_NEW/step6_output.RData")
load("backup_NEW/step7_output.RData")
load("backup_NEW/step8_output.RData")

cat("\n========================================================================\n")
cat("Table 1. 핵심 통계 요약표 (Core Statistical Summary Table)\n")
cat("========================================================================\n")

cat(sprintf("%-25s | %-50s | %-45s\n", "Analysis (분석명)", "Result (수치 결과)", "Interpretation (학술적 해석)"))
cat(paste(rep("-", 125), collapse = ""), "\n")

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "Sample Size", 
            "N = 983", 
            "Final analytic sample"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "Mean Cramer's V", 
            sprintf("Psych-Psych %.4f; Soma-Soma %.4f; Psych-Soma %.4f", stats_psych$Mean, stats_soma$Mean, stats_cross$Mean), 
            "Cross-domain coupling was weakest"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "Bootstrap Delta V (1)", 
            sprintf("Psych-Psych > Psych-Soma: Delta=%.4f, 95%% CI [%.4f, %.4f]", stats_diff1$Mean, stats_diff1$Lower, stats_diff1$Upper), 
            "Stable difference"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "Bootstrap Delta V (2)", 
            sprintf("Soma-Soma > Psych-Soma: Delta=%.4f, 95%% CI [%.4f, %.4f]", stats_diff2$Mean, stats_diff2$Lower, stats_diff2$Upper), 
            "Stable but small difference"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "Psych LCA Optimal class", 
            sprintf("K = %d (BIC = %.2f)", psych_lca_res$best_k, psych_lca_res$best_model$bic), 
            "Psych Class 1 vs Class 2"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "Soma LCA Optimal class", 
            sprintf("K = %d (BIC = %.2f)", soma_lca_res$best_k, soma_lca_res$best_model$bic), 
            "Soma Class 1 vs Class 2"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "LCA Mixed Profiles", 
            sprintf("Obs: %.1f%% [%.1f%%, %.1f%%]; Exp: %.1f%% [%.1f%%, %.1f%%]", 
                    stats_obs$Mean*100, stats_obs$Lower*100, stats_obs$Upper*100,
                    stats_exp$Mean*100, stats_exp$Lower*100, stats_exp$Upper*100), 
            sprintf("Excess: %.1f%% [%.1f%%, %.1f%%]", 
                    stats_ex$Mean*100, stats_ex$Lower*100, stats_ex$Upper*100)))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "Combined LCA", 
            sprintf("K = %d primary (BIC = %.2f)", length(best_model$P), best_model$bic), 
            "Not reducible to a single binary structure"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "MGM Bridge Centrality", 
            sprintf("Q4 strongest (%.3f); Q2 second (%.3f)", df_bridge$Bridge_Strength[df_bridge$Node == "Q4"], df_bridge$Bridge_Strength[df_bridge$Node == "Q2"]), 
            "Body-shape and postprandial as bridge items"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "MGM Isolated Node", 
            sprintf("Q8 strength: %.3f", df_bridge$Bridge_Strength[df_bridge$Node == "Q8"]), 
            "Symptom-vulnerability conditionally isolated"))

cat(sprintf("%-25s | %-50s | %-45s\n", 
            "RF Classification", 
            sprintf("Sasang %.1f%%; Soma %.1f%%; E/I %.1f%%", rf_results$Accuracy[rf_results$Model == "Sasang"], rf_results$Accuracy[rf_results$Model == "Soma"], rf_results$Accuracy[rf_results$Model == "E/I"]), 
            "Exploratory classification baseline"))

cat(paste(rep("-", 125), collapse = ""), "\n")

cat("\n========================================================================\n")
cat("Re-analysis Complete! All Figures and Table 1 are successfully generated.\n")
cat("========================================================================\n")
