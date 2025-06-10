# Mobile App Data Analysis and Visualization
# Author: Owais Raza (took help with Git's copilot)
# Date: June 03 2025
# Listening to: The HU - Wolf Totem 
# Description: summary tables 

required_packages <- c("dplyr", "tidyr", "scales", "openxlsx", "stringr")
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
lapply(required_packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE)))

df <- readRDS("cleaned_appdata_with_haddon.rds")

# 1. Overall App Count by Unified Category
table_category <- df %>%
  count(UnifiedCategory, sort = TRUE) %>%
  mutate(percent = scales::percent(n / sum(n)))

# 2. Overall App Count by Haddon Top Factor
table_haddon <- df %>%
  count(Haddon_top_factor, sort = TRUE) %>%
  mutate(percent = scales::percent(n / sum(n)))

# 3. App Count by Stage
table_stage <- df %>%
  count(Final_Stage, sort = TRUE) %>%
  mutate(percent = scales::percent(n / sum(n)))

# 4. Unified Category × Haddon Top Factor
table_cat_haddon <- df %>%
  count(UnifiedCategory, Haddon_top_factor) %>%
  tidyr::pivot_wider(names_from = Haddon_top_factor, values_from = n, values_fill = 0)

# 5. Stage × Haddon Top Factor
table_stage_haddon <- df %>%
  count(Final_Stage, Haddon_top_factor) %>%
  tidyr::pivot_wider(names_from = Haddon_top_factor, values_from = n, values_fill = 0)

# 6. Unified Category × Stage
table_cat_stage <- df %>%
  count(UnifiedCategory, Final_Stage) %>%
  tidyr::pivot_wider(names_from = Final_Stage, values_from = n, values_fill = 0)

# 7. Average Keyword Scores by Unified Category
table_avg_scores <- df %>%
  group_by(UnifiedCategory) %>%
  summarise(
    avg_human = mean(score_human),
    avg_vehicle = mean(score_vehicle),
    avg_physical = mean(score_physical_environment),
    avg_social = mean(score_social_environment)
  )

# 8. Number of Apps with Multiple Top Factors (Ties)
table_topfactor_ties <- df %>%
  mutate(num_factors = stringr::str_count(Haddon_top_factor, "&") + 1) %>%
  count(num_factors)

# 9. Three-way: Unified Category × Stage × Haddon Top Factor
table_cat_stage_haddon <- df %>%
  count(UnifiedCategory, Final_Stage, Haddon_top_factor) %>%
  arrange(desc(n))

# 10. Top Apps per Factor
table_topapps <- df %>%
  group_by(Haddon_top_factor) %>%
  slice_max(Haddon_top_score, n = 3, with_ties = FALSE) %>%
  select(`App name`, UnifiedCategory, Haddon_top_score, Haddon_top_factor)

# ---- Save All Tables to Excel ----
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Category_Summary")
openxlsx::addWorksheet(wb, "Haddon_Summary")
openxlsx::addWorksheet(wb, "Stage_Summary")
openxlsx::addWorksheet(wb, "Cat_x_Haddon")
openxlsx::addWorksheet(wb, "Stage_x_Haddon")
openxlsx::addWorksheet(wb, "Cat_x_Stage")
openxlsx::writeData(wb, "Category_Summary", table_category)
openxlsx::writeData(wb, "Haddon_Summary", table_haddon)
openxlsx::writeData(wb, "Stage_Summary", table_stage)
openxlsx::writeData(wb, "Cat_x_Haddon", table_cat_haddon)
openxlsx::writeData(wb, "Stage_x_Haddon", table_stage_haddon)
openxlsx::writeData(wb, "Cat_x_Stage", table_cat_stage)
openxlsx::saveWorkbook(wb, "MobileApp_Haddon_Summaries.xlsx", overwrite = TRUE)
cat("All summary tables saved to 'MobileApp_Haddon_Summaries.xlsx'\n")