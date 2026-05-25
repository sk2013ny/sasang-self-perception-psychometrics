# Step 1: Schematic of Questionnaire Domain Architecture (Figure 1A)
library(ggplot2)

cat("========================================================================\n")
cat("Step 1: Generating Figure 1A (Questionnaire Domain Schematic)\n")
cat("========================================================================\n")

# Coordinates for domains
nodes <- data.frame(
  x = c(rep(1, 5), rep(2, 5)),
  y = c(5:1, 5:1),
  q_name = c("Q1", "Q3", "Q6", "Q7", "Q9", "Q2", "Q4", "Q5", "Q8", "Q10"),
  label = c(
    "Q1: Energy/Activity",
    "Q3: Social Style",
    "Q6: Personality/Work",
    "Q7: Conversation",
    "Q9: Locomotor/Walking",
    "Q2: Postprandial/Digestion",
    "Q4: Body Shape/Appearance",
    "Q5: Sweat",
    "Q8: Illness-Symptom Response",
    "Q10: Weather/Season"
  ),
  domain = c(rep("Psychological", 5), rep("Somatic", 5))
)

# Connective lines: all 25 cross-domain pairs
links <- expand.grid(
  q1 = c("Q1", "Q3", "Q6", "Q7", "Q9"),
  q2 = c("Q2", "Q4", "Q5", "Q8", "Q10"),
  stringsAsFactors = FALSE
)

# Add coordinates
links$x1 <- 1
links$x2 <- 2
links$y1 <- sapply(links$q1, function(q) nodes$y[nodes$q_name == q])
links$y2 <- sapply(links$q2, function(q) nodes$y[nodes$q_name == q])

# Set primary bridges to highlight:
# Q4 is the strongest bridge: connects to Q1 (Energy) and Q9 (Locomotor)
# Q2 is the second strongest bridge: connects to Q1 (Energy)
links$is_bridge <- FALSE
links$is_bridge[links$q1 == "Q1" & links$q2 == "Q4"] <- TRUE
links$is_bridge[links$q1 == "Q9" & links$q2 == "Q4"] <- TRUE
links$is_bridge[links$q1 == "Q1" & links$q2 == "Q2"] <- TRUE

# Build plot
p_1a <- ggplot() +
  # Draw background frames for the domains
  geom_rect(aes(xmin = 0.5, xmax = 1.45, ymin = 0.3, ymax = 5.75), fill = "#FDEDEC", color = NA, alpha = 0.55) +
  geom_rect(aes(xmin = 1.55, xmax = 2.5, ymin = 0.3, ymax = 5.75), fill = "#EBF5FB", color = NA, alpha = 0.55) +
  
  # Draw potential cross-domain links (weak coupling) - extremely faint dotted lines
  geom_segment(data = subset(links, !is_bridge), aes(x = x1, y = y1, xend = x2, yend = y2), 
               color = "gray88", linetype = "dotted", linewidth = 0.3, alpha = 0.5) +
  # Draw bridge links - clean solid line
  geom_segment(data = subset(links, is_bridge), aes(x = x1, y = y1, xend = x2, yend = y2), 
               color = "#2C3E50", linetype = "solid", linewidth = 1.3, alpha = 0.9) +
               
  # Domain Titles and Subtitles
  annotate("text", x = 0.975, y = 5.55, label = "Psychological Domain", 
           fontface = "bold", size = 4.8, color = "#C0392B", hjust = 0.5) +
  annotate("text", x = 0.975, y = 5.40, label = "Mind & Behavior", 
           fontface = "italic", size = 3.5, color = "#7F8C8D", hjust = 0.5) +
           
  annotate("text", x = 2.025, y = 5.55, label = "Somatic Domain", 
           fontface = "bold", size = 4.8, color = "#2980B9", hjust = 0.5) +
  annotate("text", x = 2.025, y = 5.40, label = "Physical & Sensory", 
           fontface = "italic", size = 3.5, color = "#7F8C8D", hjust = 0.5) +
           
  # Draw Nodes
  geom_label(data = nodes, aes(x = x, y = y, label = label, fill = domain), 
             color = "white", fontface = "bold", size = 4.0, linewidth = 0.4, 
             label.padding = unit(0.5, "lines"), label.r = unit(0.2, "lines"),
             show.legend = FALSE) +
  scale_fill_manual(values = c("Psychological" = "#E74C3C", "Somatic" = "#3498DB")) +
  
  xlim(0.4, 2.6) +
  ylim(0.1, 5.8) +
  theme_void() +
  theme(
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("Fig1A_DomainSchematic.png", p_1a, width = 7, height = 6, dpi = 300)
cat("\nFigure 1A 저장 완료: Fig1A_DomainSchematic.png\n")
cat("========================================================================\n")
