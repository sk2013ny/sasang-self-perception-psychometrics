# Step 1: Data Cleaning and Coding
if (!dir.exists("backup_NEW")) {
  dir.create("backup_NEW")
}
library(readr)
library(dplyr)
library(tidyr)

cat("========================================================================\n")
cat("Step 1: Data Cleaning and Coding\n")
cat("========================================================================\n")

# 1. Load active raw dataset
raw_df <- read_csv("analysis_983.csv", show_col_types = FALSE)

# 2. Basic diagnostic checks
n_total_rows <- nrow(raw_df)
n_participants <- n_distinct(raw_df$submission_id)

cat("총 데이터 행 수:", n_total_rows, "\n")
cat("총 고유 참가자 수:", n_participants, "\n")

# Check missing rates
missing_counts <- colSums(is.na(raw_df))
cat("\n--- 변수별 결측치 현황 ---\n")
print(missing_counts)

# 3. Pivot to wide format
df_wide <- raw_df %>%
  distinct(submission_id, age_group, gender, mbti, actual_sasang) %>%
  left_join(
    raw_df %>%
      select(submission_id, question_number, selected_answer) %>%
      pivot_wider(
        names_from  = question_number,
        values_from = selected_answer,
        names_prefix = "Q"
      ),
    by = "submission_id"
  )

# Verify wide format size
cat("\nWide format 변환 완료: ", nrow(df_wide), "행 (참가자) × ", ncol(df_wide), "열\n")

# 4. Map categories to factor levels (태양인=1, 태음인=2, 소양인=3, 소음인=4)
sasang_levels <- c("태양인", "태음인", "소양인", "소음인")

# Create df_lca: 1-based integers for poLCA (1 to 4)
df_lca <- df_wide %>%
  mutate(across(starts_with("Q"), ~ as.integer(factor(.x, levels = sasang_levels))))

# Create df_mgm: 0-based integers for mgm (0 to 3)
df_mgm <- df_wide %>%
  mutate(across(starts_with("Q"), ~ as.integer(factor(.x, levels = sasang_levels)) - 1))

# Define variables
psych_questions <- paste0("Q", c(1, 3, 6, 7, 9))
soma_questions  <- paste0("Q", c(2, 4, 5, 8, 10))
all_questions   <- paste0("Q", 1:10)

# Display demographic summary
cat("\n--- 성별 분포 ---\n")
print(table(df_wide$gender))

cat("\n--- 연령대 분포 ---\n")
print(table(df_wide$age_group))

cat("\n--- MBTI E/I 분포 ---\n")
df_wide <- df_wide %>% mutate(EI = ifelse(grepl("^E", mbti), "E", "I"))
print(table(df_wide$EI))

# 5. Export clean datasets for subsequent steps
save(df_wide, df_lca, df_mgm, psych_questions, soma_questions, all_questions, sasang_levels, file = "backup_NEW/step1_output.RData")
cat("\nStep 1 완료: backup_NEW/step1_output.RData 저장 완료\n")
cat("========================================================================\n")
