# Step 7: Categorical MGM Network & EBIC Gamma Sensitivity Analysis
load("backup_NEW/step1_output.RData")
if (!requireNamespace("mgm", quietly = TRUE)) {
  install.packages("mgm", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("igraph", quietly = TRUE)) {
  install.packages("igraph", repos = "https://cloud.r-project.org")
}
library(mgm)
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)

cat("========================================================================\n")
cat("Step 7: Categorical MGM Network & EBIC Gamma Sensitivity\n")
cat("========================================================================\n")

# Prepare 0-indexed matrix for mgm
X_mgm <- as.matrix(df_mgm[, paste0("Q", 1:10)])

# Define domains and indices
psych_idx <- c(1, 3, 6, 7, 9)
soma_idx  <- c(2, 4, 5, 8, 10)

# Helper function to compute Bridge Strength for any wadj matrix
calc_bridge_strength <- function(wadj, p_idx, s_idx) {
  strengths <- numeric(10)
  for (i in 1:10) {
    if (i %in% p_idx) {
      # Psychological node: sum connections to somatic nodes
      strengths[i] <- sum(wadj[i, s_idx])
    } else {
      # Somatic node: sum connections to psychological nodes
      strengths[i] <- sum(wadj[i, p_idx])
    }
  }
  strengths
}

# Helper function to get top bridge nodes
get_top_bridges <- function(bs) {
  df <- data.frame(Node = paste0("Q", 1:10), BS = bs, stringsAsFactors = FALSE) %>%
    arrange(desc(BS))
  paste(df$Node[1:3], collapse = ", ")
}

# 1. Fit models for gamma = 0, 0.25, 0.5
cat("\nFitting MGM with EBIC selection for gamma = 0 (dense) ...\n")
set.seed(42)
fit_gam0 <- mgm(
  data = X_mgm,
  type = rep("c", 10),
  level = rep(4, 10),
  lambdaSel = "EBIC",
  lambdaGam = 0,
  ruleReg = "AND",
  pbar = FALSE
)

cat("Fitting MGM with EBIC selection for gamma = 0.25 (primary) ...\n")
set.seed(42)
fit_mgm <- mgm(
  data = X_mgm,
  type = rep("c", 10),
  level = rep(4, 10),
  lambdaSel = "EBIC",
  lambdaGam = 0.25,
  ruleReg = "AND",
  pbar = FALSE
)

cat("Fitting MGM with EBIC selection for gamma = 0.5 (sparse) ...\n")
set.seed(42)
fit_gam5 <- mgm(
  data = X_mgm,
  type = rep("c", 10),
  level = rep(4, 10),
  lambdaSel = "EBIC",
  lambdaGam = 0.5,
  ruleReg = "AND",
  pbar = FALSE
)

# Extract regularized conditional adjacency matrices
wadj_0  <- fit_gam0$pairwise$wadj
wadj_25 <- fit_mgm$pairwise$wadj
wadj_50 <- fit_gam5$pairwise$wadj

colnames(wadj_25) <- paste0("Q", 1:10)
rownames(wadj_25) <- paste0("Q", 1:10)

cat("\n--- MGM 조건부 연관성 인접 행렬 (gamma = 0.25) ---\n")
print(round(wadj_25, 4))

# 2. Compute Edge Counts and Bridge Strengths
edges_0  <- sum(wadj_0[upper.tri(wadj_0)] > 0)
edges_25 <- sum(wadj_25[upper.tri(wadj_25)] > 0)
edges_50 <- sum(wadj_50[upper.tri(wadj_50)] > 0)

bs_0  <- calc_bridge_strength(wadj_0, psych_idx, soma_idx)
bs_25 <- calc_bridge_strength(wadj_25, psych_idx, soma_idx)
bs_50 <- calc_bridge_strength(wadj_50, psych_idx, soma_idx)

# Print Summary Table
cat("\n========================================================================\n")
cat("MGM Regularization Sensitivity Analysis Summary\n")
cat("========================================================================\n")
cat(sprintf("gamma = 0:    %2d active edges. Top Bridges: %s\n", edges_0, get_top_bridges(bs_0)))
cat(sprintf("gamma = 0.25: %2d active edges. Top Bridges: %s\n", edges_25, get_top_bridges(bs_25)))
cat(sprintf("gamma = 0.5:  %2d active edges. Top Bridges: %s\n", edges_50, get_top_bridges(bs_50)))

# Specifically check if Q4, Q2, Q1 remain stable
check_stability <- function(bs, gamma_val) {
  df <- data.frame(Node = paste0("Q", 1:10), BS = bs, stringsAsFactors = FALSE) %>%
    mutate(Rank = rank(-BS, ties.method = "min"))
  
  q4_rank <- df$Rank[df$Node == "Q4"]
  q2_rank <- df$Rank[df$Node == "Q2"]
  q1_rank <- df$Rank[df$Node == "Q1"]
  
  q4_bs <- df$BS[df$Node == "Q4"]
  q2_bs <- df$BS[df$Node == "Q2"]
  q1_bs <- df$BS[df$Node == "Q1"]
  
  cat(sprintf("  At gamma = %-4s: Q4 BS = %.3f (Rank %d), Q2 BS = %.3f (Rank %d), Q1 BS = %.3f (Rank %d)\n",
              gamma_val, q4_bs, q4_rank, q2_bs, q2_rank, q1_bs, q1_rank))
}

cat("\n--- Stability of Key Bridge Nodes (Q4, Q2, Q1) ---\n")
check_stability(bs_0, "0")
check_stability(bs_25, "0.25")
check_stability(bs_50, "0.5")
cat("========================================================================\n")

# Prepare bridge dataframe for primary model (gamma = 0.25)
df_bridge <- data.frame(
  Node = factor(paste0("Q", 1:10), levels = paste0("Q", 1:10)),
  Domain = ifelse(1:10 %in% psych_idx, "Psychological", "Somatic"),
  Bridge_Strength = bs_25,
  stringsAsFactors = FALSE
) %>% arrange(desc(Bridge_Strength))

# 3. Figure 2B: MGM Network Visualization (gamma = 0.25)
cat("\nGenerating Figure 2B (MGM Network plot)...")
g <- graph_from_adjacency_matrix(wadj_25, mode = "undirected", weighted = TRUE, diag = FALSE)

# Fixed circle layout for clean domain comparison
layout_circle <- layout_in_circle(g)

# Nodes: Red for psychological, Blue for somatic
V(g)$color <- ifelse(1:10 %in% psych_idx, "#E74C3C", "#3498DB")

# Emphasize Q4, Q2, Q1 nodes
# Strongest bridge Q4 gets a thick black border and largest size (28)
# Q1 and Q2 get black borders and medium size (25)
# Others get thin white borders and smaller size (22)
V(g)$frame.color <- ifelse(V(g)$name %in% c("Q1", "Q2", "Q4"), "black", "white")
V(g)$size <- ifelse(V(g)$name == "Q4", 28, ifelse(V(g)$name %in% c("Q1", "Q2"), 25, 22))

V(g)$label.color <- "black"
V(g)$label.font <- ifelse(V(g)$name %in% c("Q1", "Q2", "Q4"), 4, 2) # 4 is bold-italic, 2 is bold
V(g)$label.size <- 1.1

# Edges: scale width by weight, emphasize cross-domain links
E(g)$width <- E(g)$weight * 13
edge_list <- as_edgelist(g)
is_cross_edge <- sapply(1:ecount(g), function(idx) {
  n1 <- as.integer(sub("Q", "", edge_list[idx, 1]))
  n2 <- as.integer(sub("Q", "", edge_list[idx, 2]))
  (n1 %in% psych_idx && n2 %in% soma_idx) || (n1 %in% soma_idx && n2 %in% psych_idx)
})

# Cross-domain edges get dark slate-blue (#2C3E50), within-domain get very faint light gray
E(g)$color <- ifelse(is_cross_edge, "#2C3E50", "#E5E7E9")

png("Fig2B_MGMNetwork.png", width = 800, height = 800, res = 135)
plot(g, 
     layout = layout_circle, 
     vertex.label = V(g)$name,
     vertex.label.dist = 0,
     main = "Conditional Item Network")
dev.off()
cat("\nFigure 2B 저장 완료: Fig2B_MGMNetwork.png\n")

# 4. Figure 2C: Bridge Strength Bar Plot (gamma = 0.25)
p_2c <- ggplot(df_bridge, aes(x = reorder(Node, Bridge_Strength), y = Bridge_Strength, fill = Domain)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", linewidth = 0.5, alpha = 0.85) +
  # Add exact bridge strength value labels on top of the bars
  geom_text(aes(label = sprintf("%.3f", Bridge_Strength)), 
            hjust = -0.15, fontface = "bold", size = 3.5, color = "black") +
  scale_fill_manual(values = c("Psychological" = "#E74C3C", "Somatic" = "#3498DB")) +
  coord_flip() +
  labs(
    x = "Question Node",
    y = "Bridge Strength (Cross-Domain)",
    fill = "Domain",
    caption = "* Q8 had no selected cross-domain edges under regularized MGM"
  ) +
  scale_y_continuous(limits = c(0, 1.6), breaks = seq(0, 1.6, by = 0.2)) +
  theme_bw(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    # Move legend inside the bottom-right empty space to save space
    legend.position = c(0.78, 0.22),
    legend.background = element_rect(color = "gray80", fill = "white", linewidth = 0.4),
    plot.caption = element_text(size = 8.5, face = "italic", color = "gray40", hjust = 1.0)
  )

ggsave("Fig2C_BridgeStrengths.png", p_2c, width = 6, height = 4.5, dpi = 300)
cat("Figure 2C 저장 완료: Fig2C_BridgeStrengths.png\n")

# 5. Figure 2D: Local Edge Weights of the Primary Bridge Node (Q4)
cat("\nGenerating Figure 2D (Bridge Node Local Edges)...")
q4_edges <- wadj_25["Q4", ]
q4_edges <- q4_edges[names(q4_edges) != "Q4"]  # exclude self

df_q4_edges <- data.frame(
  Node = names(q4_edges),
  Weight = as.numeric(q4_edges),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Domain = ifelse(as.integer(sub("Q", "", Node)) %in% psych_idx, "Psychological", "Somatic"),
    Label = case_when(
      Node == "Q1"  ~ "Q1: Energy/Activity",
      Node == "Q3"  ~ "Q3: Social Style",
      Node == "Q6"  ~ "Q6: Personality/Work",
      Node == "Q7"  ~ "Q7: Conversation",
      Node == "Q9"  ~ "Q9: Locomotor/Walking",
      Node == "Q2"  ~ "Q2: Postprandial/Digestion",
      Node == "Q5"  ~ "Q5: Sweat",
      Node == "Q8"  ~ "Q8: Illness-Symptom Response",
      Node == "Q10" ~ "Q10: Weather/Season",
      TRUE ~ Node
    )
  ) %>%
  arrange(desc(Weight))

p_2d <- ggplot(df_q4_edges, aes(x = reorder(Label, Weight), y = Weight, fill = Domain)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", linewidth = 0.5, alpha = 0.85) +
  geom_text(aes(label = sprintf("%.3f", Weight)), 
            hjust = -0.15, fontface = "bold", size = 3.5, color = "black") +
  scale_fill_manual(values = c("Psychological" = "#E74C3C", "Somatic" = "#3498DB")) +
  coord_flip() +
  labs(
    title = "Conditional Association Profile of the Primary Bridge Node (Q4)",
    subtitle = "Q4 (Body Shape) connects robustly across both Psychological and Somatic domains",
    x = "Target Question Node",
    y = "Conditional Association Weight (MGM Edge Weight)",
    fill = "Domain"
  ) +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.4, by = 0.1)) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 9, face = "italic", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    legend.position = c(0.78, 0.22),
    legend.background = element_rect(color = "gray80", fill = "white", linewidth = 0.4)
  )

ggsave("Fig2D_BridgeLocalEdges.png", p_2d, width = 6.5, height = 4.5, dpi = 300)
cat("Figure 2D 저장 완료: Fig2D_BridgeLocalEdges.png\n")

# 6. Figure S3: Bridge Strength Sensitivity across EBIC Gamma Tuning Parameters
df_sens <- data.frame(
  Node = rep(paste0("Q", 1:10), 3),
  Gamma = factor(rep(c("gamma = 0 (Dense MRF)", "gamma = 0.25 (Primary MRF)", "gamma = 0.5 (Sparse MRF)"), each = 10),
                 levels = c("gamma = 0 (Dense MRF)", "gamma = 0.25 (Primary MRF)", "gamma = 0.5 (Sparse MRF)")),
  Bridge_Strength = c(bs_0, bs_25, bs_50),
  Domain = rep(ifelse(1:10 %in% psych_idx, "Psychological", "Somatic"), 3),
  stringsAsFactors = FALSE
)

p_sens <- ggplot(df_sens, aes(x = Node, y = Bridge_Strength, fill = Domain)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 0.4, alpha = 0.85) +
  scale_fill_manual(values = c("Psychological" = "#E74C3C", "Somatic" = "#3498DB")) +
  facet_wrap(~Gamma, ncol = 1, scales = "free_y") +
  labs(
    title = "Bridge Strength Sensitivity across EBIC Gamma Tuning Parameters",
    subtitle = "Highlighting Q4 (Body Shape), Q2 (Postprandial), and Q1 (Energy) as stable cross-domain bridge nodes",
    x = "Question Node",
    y = "Bridge Strength (Cross-Domain Sum of Edge Weights)",
    caption = "Supplementary visualization only; not used as diagnostic validation."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    plot.caption = element_text(size = 9, face = "italic", color = "gray30", hjust = 0.5)
  )

ggsave("FigS3_MGM_Sensitivity.png", p_sens, width = 7, height = 8, dpi = 300)
cat("Figure S3 (Sensitivity Analysis) 저장 완료: FigS3_MGM_Sensitivity.png\n")

# Save output
save(fit_mgm, wadj_25, df_bridge, fit_gam0, fit_gam5, bs_0, bs_25, bs_50, file = "backup_NEW/step7_output.RData")
cat("\nStep 7 완료: backup_NEW/step7_output.RData 저장 완료\n")
cat("========================================================================\n")
