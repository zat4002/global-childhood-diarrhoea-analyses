# Load required libraries
library(readxl)
library(meta)
library(zoo)
library(dplyr)
library(purrr)
library(writexl)
library(metasens)
library(psych)
library(robvis)
library(ggplot2)
library(svglite)
library(patchwork)
library(cowplot)
library(ggpubr)
library(tidyr)
#install.packages("devtools")
#devtools::install_github("mcguinlu/robvis")

# Set working directory
setwd('C:/Users/zat4002/Box/Meta analysis paper/Paper/First review round/Codes for Github')

# Load data
raw_data <- read_excel("Extraction sheet.xlsx", sheet = "data")

# Create 5-year and 10-year time category variables
raw_data$Year_Category5 <- cut(raw_data$Year_calculated, 
                               breaks = seq(1985, 2025, by = 5), 
                               labels = paste(seq(1985, 2020, by = 5), seq(1989, 2024, by = 5), sep = "-"),
                               right = FALSE)

raw_data$Year_Category10 <- cut(raw_data$Year_calculated, 
                                breaks = seq(1985, 2025, by = 10), 
                                labels = paste(seq(1985, 2020, by = 10), seq(1994, 2029, by = 10), sep = "-"),
                                right = FALSE)

# -------------------------------
# Impute missing values
# -------------------------------

# Convert selected columns to numeric
cleaned_data <- raw_data %>%
  mutate(across(c(14, 15, 16, 18:30), ~ as.numeric(as.character(.)))) %>% 
  
  # Fill missing values within countries using LOCF (last observation carried forward)
  group_by(Country) %>%
  arrange(desc(Year_calculated), .by_group = TRUE) %>%
  mutate(across(18:30, ~ na.locf(na.locf(.x, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE))) %>%
  ungroup() %>%
  
  # Round case counts and impute remaining NA values by WHO Region & decade
  mutate(`Total Number of Diarrhea Cases` = round(`Total Number of Diarrhea Cases`, 0)) %>%
  group_by(`WHO Region`, Year_Category10) %>%
  mutate(across(18:30, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup() %>%
  
  # Impute missing sample sizes by country-level medians
  group_by(Country) %>%
  mutate(across(`Total Sample Size (n)`, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  
  # Estimate missing number of cases using prevalence × sample size
  mutate(across(`Total Number of Diarrhea Cases`, ~ ifelse(
    is.na(.), `Total Sample Size (n)` * (`Diarrhea prevalence in Children Under 5 (%)` / 100), .))) %>%
  mutate(`Total Number of Diarrhea Cases` = round(`Total Number of Diarrhea Cases`, 0)) %>%
  ungroup() %>%
  
  # Round values and scale some variables
  mutate(
    across(c(14, 18:30, -20, -22, -23), ~ round(., 1)),
    across(c(20, 22), ~ round(., 2) / 2),
    across(23, ~ round(., 3)),
    across(c(14, 18, 19, 25:29), ~ ./100)
  )

# Save cleaned dataset
write_xlsx(cleaned_data, path = "dataimputed.xlsx")

# -------------------------------------
# Meta-analysis: Global (Under 5)
# -------------------------------------

cleaned_data = cleaned_data%>% mutate(type2 = ifelse(Type == "MICS","MICS","DHS"),name = paste(Country,Year,type2))

# Sort dataset by most recent year
under5_data <- cleaned_data %>% arrange(desc(Year_calculated))

# Run meta-analysis with 5-year subgroups
global_meta <- metaprop(
  event = under5_data$`Total Number of Diarrhea Cases`,
  n = under5_data$`Total Sample Size (n)`,
  method = "Inverse",
  sm = "PFT",
  title = "Meta analysis per Year category",
  subgroup = under5_data$Year_Category5,
  prediction.subgroup = TRUE,
  prediction = TRUE,
  overall = TRUE
)

# Meta-analysis summary stats
global_meta_summary <- data.frame(
  k = global_meta$k.all.w,
  n = global_meta$n.w,
  events = global_meta$event.w,
  I2 = round(global_meta$I2 * 100, 1),
  I2lower = round(global_meta$lower.I2 * 100, 1),
  I2upper = round(global_meta$upper.I2 * 100, 1),
  I2sub = round(global_meta$I2.w * 100, 1),
  I2lowersub = round(global_meta$lower.I2.w * 100, 1),
  I2uppersub = round(global_meta$upper.I2.w * 100, 1),
  Q = round(global_meta$Q, 1),
  Qsub = round(global_meta$Q.w, 1),
  Qpval = round(global_meta$pval.Q.w, 4),
  k_total = global_meta$k.all
) %>%
  mutate(I = paste(I2sub, " (", I2lowersub, "-", I2uppersub, ")", sep = ""))

# Summary statistics: Global by year
global_summary_stats_by_year <- under5_data %>%
  group_by(Year_Category5) %>%
  summarise(
    Median = round(median(`Diarrhea prevalence in Children Under 5 (%)`) * 100, 1),
    Q1 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.25),
    Q3 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.75),
    .groups = "drop"
  ) %>%
  mutate(q1q3 = paste(round(Q1, 1), round(Q3, 1), sep = "-"))

# Summary statistics: Overall global
global_summary_stats_total <- under5_data %>%
  summarise(
    Median = round(median(`Diarrhea prevalence in Children Under 5 (%)`) * 100, 1),
    Q1 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.25),
    Q3 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.75)
  ) %>%
  mutate(q1q3 = paste(round(Q1, 1), round(Q3, 1), sep = "-"))

# -------------------------------------
# Meta-analysis by WHO Region
# -------------------------------------

# Split data by WHO Region and apply meta-analysis
regional_meta <- under5_data %>%
  group_by(`WHO Region`) %>%
  group_split() %>%
  map(~ metaprop(
    event = .x$`Total Number of Diarrhea Cases`,
    n = .x$`Total Sample Size (n)`,
    method = "Inverse",
    sm = "PFT",
    subgroup = .x$Year_Category5,
    prediction.subgroup = TRUE,
    prediction = TRUE,
    overall = TRUE,
    title = paste("Meta-analysis for", .x$`WHO Region`[1])
  ))

print(regional_meta[[1]])
print(regional_meta[[2]])
print(regional_meta[[3]])
print(regional_meta[[4]])
print(regional_meta[[5]])
print(regional_meta[[6]])

# Combine results
regional_meta_summary <- map_dfr(regional_meta, function(res) {
  data.frame(
    Region = res$title,
    k = res$k.all.w,
    n = res$n.w,
    events = res$event.w,
    I2 = round(res$I2 * 100, 1),
    I2lower = round(res$lower.I2 * 100, 1),
    I2upper = round(res$upper.I2 * 100, 1),
    I2sub = round(res$I2.w * 100, 1),
    I2lowersub = round(res$lower.I2.w * 100, 1),
    I2uppersub = round(res$upper.I2.w * 100, 1),
    Q = round(res$Q, 1),
    Qsub = round(res$Q.w, 1),
    Qpval = round(res$pval.Q.w, 4),
    k_total = res$k.all
  )
}) %>%
  mutate(I = paste(I2sub, " (", I2lowersub, "-", I2uppersub, ")", sep = ""))

# Summary statistics by WHO Region and year
regional_summary_stats_by_year <- under5_data %>%
  group_by(Year_Category5, `WHO Region`) %>%
  summarise(
    Median = round(median(`Diarrhea prevalence in Children Under 5 (%)`) * 100, 1),
    Q1 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.25),
    Q3 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.75),
    .groups = "drop"
  ) %>%
  mutate(q1q3 = paste(round(Q1, 1), round(Q3, 1), sep = "-"))

# Summary statistics overall by WHO Region
regional_summary_stats_total <- under5_data %>%
  group_by(`WHO Region`) %>%
  summarise(
    Median = round(median(`Diarrhea prevalence in Children Under 5 (%)`) * 100, 1),
    Q1 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.25),
    Q3 = quantile(`Diarrhea prevalence in Children Under 5 (%)` * 100, 0.75),
    .groups = "drop"
  ) %>%
  mutate(q1q3 = paste(round(Q1, 1), round(Q3, 1), sep = "-"))

# Save summary outputs
write_xlsx(global_summary_stats_by_year, path = "upsummary_statsglobalunder5.xlsx")
write_xlsx(regional_summary_stats_by_year, path = "upsummary_statswhounder5.xlsx")
write_xlsx(global_meta_summary, path = "upmetaanalysis5results.xlsx")
write_xlsx(regional_meta_summary, path = "upmetaanalysis5resultswho.xlsx")

# ----------------------------------------
# Forest Plot: Global Meta-analysis
# ----------------------------------------

# Meta-analysis of global diarrhea prevalence with study labels
meta_global <- metaprop(
  event = under5_data$`Total Number of Diarrhea Cases`,
  n = under5_data$`Total Sample Size (n)`,
  method = "Inverse",
  sm = "PFT",
  title = "Meta analysis per Year category",
  prediction.subgroup = TRUE,
  prediction = TRUE,
  overall = TRUE,
  studlab = under5_data$name
)

# DOI Plot for global analysis
svg(file = 'doiplotmeta.svg', width = 7, height = 7)
doiplot(meta_global, xlim = c(0, 1), ylim = c(3.5, 0), main = 'All regions', adj = 0.5, cex.main = 2)
dev.off()

# Forest Plot for global analysis
svg(file = 'forestplot.svg', width = 12, height = 121.5)
forestplot_global <- forest(
  meta_global,
  comb.fixed = TRUE,
  comb.random = TRUE,
  overall = TRUE,
  pooled.events = TRUE,
  prediction = TRUE,
  print.subgroup.labels = TRUE,
  bylab = under5_data$name,
  col.diamond = "gray",
  col.diamond.lines = "black",
  col.predict.lines = "black",
  col.by = "black",
  xlab = "Childhood diarrhea prevalence (95% CI)",
  xlim = c(0, 100),
  xlab.pos = 50,
  col.inside.square = "black",
  leftcols = c("studlab", "event", "n"),
  leftlabs = c("Study", "Number of cases", "Sample size"),
  rightcols = c("w.random", "effect", "ci"),
  rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
  col.predict = "black",
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.Q = TRUE,
  print.pval.Q = TRUE,
  print.prediction = TRUE,
  digits = 1,
  pscale = 100
)
dev.off()

# ----------------------------------------
# Forest Plots: Meta-analysis by WHO Region
# ----------------------------------------

# Run meta-analysis for each WHO Region separately
meta_by_region  = under5_data %>%
  group_by(`WHO Region`) %>%
  group_split() %>%
  map(~ metaprop(
    event = .x$`Total Number of Diarrhea Cases`,
    n = .x$`Total Sample Size (n)`,
    method = "Inverse",
    studlab = .x$name,
    sm = "PFT",
    prediction.subgroup = TRUE,
    prediction = TRUE,
    overall = TRUE,
    title = paste("Meta-analysis for", .x$`WHO Region`[1])
  ))

# -------------------------
# Forest Plot: African Region
# -------------------------

svg(file = 'forestplotAFR.svg', width = 12, height = 60)
forest(meta_by_region[[1]], 
       comb.fixed = TRUE, comb.random = TRUE, overall = TRUE,
       pooled.events = TRUE,
       prediction = TRUE,
       print.subgroup.labels = TRUE,
       bylab = under5_data$name,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict.lines = "black",
       col.by = "black",
       xlab = "Childhood diarrhea prevalence (95% CI)", xlim = c(0, 100), xlab.pos = 50,
       col.inside.square = "black",
       leftcols = c("studlab", "event", "n"),
       leftlabs = c("Study", "Number of cases", "Sample size"),
       rightcols = c("w.random", "effect", "ci"),
       rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
       col.predict = "black",
       print.tau2 = FALSE,
       print.I2 = TRUE,
       print.Q = TRUE,
       print.pval.Q = TRUE,
       print.prediction = TRUE,
       digits = 1,
       pscale = 100)
dev.off()

# --------------------------------------------------
# Forest Plot for Region of the Americas (AMR)
# --------------------------------------------------

svg(file = 'forestplotAMR.svg', width = 12, height = 21)
forestplot_amr <- forest(
  meta_by_region[[2]],
  comb.fixed = TRUE, comb.random = TRUE, overall = TRUE,
  pooled.events = TRUE,
  prediction = TRUE,
  print.subgroup.labels = TRUE,
  bylab = under5_data$name,
  col.diamond = "gray",
  col.diamond.lines = "black",
  col.predict.lines = "black",
  col.by = "black",
  xlab = "Childhood diarrhea prevalence (95% CI)",
  xlim = c(0, 100), xlab.pos = 50,
  col.inside.square = "black",
  leftcols = c("studlab", "event", "n"),
  leftlabs = c("Study", "Number of cases", "Sample size"),
  rightcols = c("w.random", "effect", "ci"),
  rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
  col.predict = "black",
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.Q = TRUE,
  print.pval.Q = TRUE,
  print.prediction = TRUE,
  digits = 1,
  pscale = 100
)
dev.off()

# --------------------------------------------------
# Forest Plot for Eastern Mediterranean Region (EMR)
# --------------------------------------------------

svg(file = 'forestplotEMR.svg', width = 12, height = 16)
forestplot_emr <- forest(
  meta_by_region[[3]],
  comb.fixed = TRUE, comb.random = TRUE, overall = TRUE,
  pooled.events = TRUE,
  prediction = TRUE,
  print.subgroup.labels = TRUE,
  bylab = under5_data$name,
  col.diamond = "gray",
  col.diamond.lines = "black",
  col.predict.lines = "black",
  col.by = "black",
  xlab = "Childhood diarrhea prevalence (95% CI)",
  xlim = c(0, 100), xlab.pos = 50,
  col.inside.square = "black",
  leftcols = c("studlab", "event", "n"),
  leftlabs = c("Study", "Number of cases", "Sample size"),
  rightcols = c("w.random", "effect", "ci"),
  rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
  col.predict = "black",
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.Q = TRUE,
  print.pval.Q = TRUE,
  print.prediction = TRUE,
  digits = 1,
  pscale = 100
)
dev.off()

# --------------------------------------------------
# Forest Plot for European Region (EUR)
# --------------------------------------------------

svg(file = 'forestplotEUR.svg', width = 12, height = 14)
forestplot_eur <- forest(
  meta_by_region[[4]],
  comb.fixed = TRUE, comb.random = TRUE, overall = TRUE,
  pooled.events = TRUE,
  prediction = TRUE,
  print.subgroup.labels = TRUE,
  bylab = under5_data$name,
  col.diamond = "gray",
  col.diamond.lines = "black",
  col.predict.lines = "black",
  col.by = "black",
  xlab = "Childhood diarrhea prevalence (95% CI)",
  xlim = c(0, 100), xlab.pos = 50,
  col.inside.square = "black",
  leftcols = c("studlab", "event", "n"),
  leftlabs = c("Study", "Number of cases", "Sample size"),
  rightcols = c("w.random", "effect", "ci"),
  rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
  col.predict = "black",
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.Q = TRUE,
  print.pval.Q = TRUE,
  print.prediction = TRUE,
  digits = 1,
  pscale = 100
)
dev.off()

# --------------------------------------------------
# Forest Plot for South-East Asia Region (SEAR)
# --------------------------------------------------

svg(file = 'forestplotSEAR.svg', width = 12, height = 14)
forestplot_sear <- forest(
  meta_by_region[[5]],
  comb.fixed = TRUE, comb.random = TRUE, overall = TRUE,
  pooled.events = TRUE,
  prediction = TRUE,
  print.subgroup.labels = TRUE,
  bylab = under5_data$name,
  col.diamond = "gray",
  col.diamond.lines = "black",
  col.predict.lines = "black",
  col.by = "black",
  xlab = "Childhood diarrhea prevalence (95% CI)",
  xlim = c(0, 100), xlab.pos = 50,
  col.inside.square = "black",
  leftcols = c("studlab", "event", "n"),
  leftlabs = c("Study", "Number of cases", "Sample size"),
  rightcols = c("w.random", "effect", "ci"),
  rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
  col.predict = "black",
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.Q = TRUE,
  print.pval.Q = TRUE,
  print.prediction = TRUE,
  digits = 1,
  pscale = 100
)
dev.off()

# --------------------------------------------------
# Forest Plot for Western Pacific Region (WPR)
# --------------------------------------------------

svg(file = 'forestplotWPR.svg', width = 12, height = 11)
forestplot_wpr <- forest(
  meta_by_region[[6]],
  comb.fixed = TRUE, comb.random = TRUE, overall = TRUE,
  pooled.events = TRUE,
  prediction = TRUE,
  print.subgroup.labels = TRUE,
  bylab = under5_data$name,
  col.diamond = "gray",
  col.diamond.lines = "black",
  col.predict.lines = "black",
  col.by = "black",
  xlab = "Childhood diarrhea prevalence (95% CI)",
  xlim = c(0, 100), xlab.pos = 50,
  col.inside.square = "black",
  leftcols = c("studlab", "event", "n"),
  leftlabs = c("Study", "Number of cases", "Sample size"),
  rightcols = c("w.random", "effect", "ci"),
  rightlabs = c("W(Random)", "Prev(%)", "95%CI"),
  col.predict = "black",
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.Q = TRUE,
  print.pval.Q = TRUE,
  print.prediction = TRUE,
  digits = 1,
  pscale = 100
)
dev.off()

# -------------------------
# DOI Plots: By WHO Region
# -------------------------

# African Region
svg(file = 'doiplotAFR.svg', width = 7, height = 7)
doiplot(meta_by_region[[1]], xlim = c(0, 1), ylim = c(3.5, 0), main = 'A. African Region', adj = 0.5, cex.main = 2)
dev.off()

# Region of the Americas
svg(file = 'doiplotAMR.svg', width = 7, height = 7)
doiplot(meta_by_region[[2]], xlim = c(0, 1), ylim = c(3.5, 0), main = 'B. Region of the Americas', adj = 0.5, cex.main = 2)
dev.off()

# Eastern Mediterranean Region
svg(file = 'doiplotEMR.svg', width = 7, height = 7)
doiplot(meta_by_region[[3]], xlim = c(0, 1), ylim = c(3.5, 0), main = 'C. Eastern Mediterranean Region', adj = 0.5, cex.main = 2)
dev.off()

# European Region
svg(file = 'doiplotEUR.svg', width = 7, height = 7)
doiplot(meta_by_region[[4]], xlim = c(0, 1), ylim = c(3.5, 0), main = 'D. European Region', adj = 0.5, cex.main = 2)
dev.off()

# South-East Asia Region
svg(file = 'doiplotSEAR.svg', width = 7, height = 7)
doiplot(meta_by_region[[5]], xlim = c(0, 1), ylim = c(3.5, 0), main = 'E. South-East Asia Region', adj = 0.5, cex.main = 2)
dev.off()

# Western Pacific Region
svg(file = 'doiplotWPR.svg', width = 7, height = 7)
doiplot(meta_by_region[[6]], xlim = c(0, 1), ylim = c(3.5, 0), main = 'F. Western Pacific Region', adj = 0.5, cex.main = 2)
dev.off()

####################################################################################################################################################################

#Multivariable meta-regression


# --------------------------------------------------
# Rename relevant variables for clarity
# --------------------------------------------------

names(cleaned_data)[c(14,17,18:30,40)] <- c(
  "DiarrPrev",                
  "WHORegion",                
  "ImprovedWaterAccess",      
  "ImprovedSanitationAccess", 
  "WaterAccessIndex",         
  "AvgHouseholdSize",         
  "MaternalEduIndex",         
  "HDI",                      
  "PopDensity",               
  "UrbanHouseholdsProp",      
  "UrbanChildrenProp",        
  "UnderweightPrev",          
  "StuntingPrev",             
  "WastingPrev",              
  "GDPpercap",                
  "Under3"                    
)

# --------------------------------------------------
# Run overall meta-analysis
# --------------------------------------------------

meta_model_overall <- metaprop(
  event = cleaned_data$`Total Number of Diarrhea Cases`,
  n = cleaned_data$`Total Sample Size (n)`,
  method = "Inverse",
  sm = "PLN",
  data = cleaned_data
)

# --------------------------------------------------
# Construct socio-environmental index using PCA
# --------------------------------------------------

# Select variables for PCA
socio_vars <- cleaned_data[, c(
  "UrbanHouseholdsProp", "MaternalEduIndex", "AvgHouseholdSize",
  "PopDensity", "HDI", "GDPpercap",
  "ImprovedWaterAccess", "ImprovedSanitationAccess", "WaterAccessIndex",
  "UnderweightPrev", "StuntingPrev", "WastingPrev"
)]

# Convert to matrix and check suitability
socio_matrix <- as.matrix(socio_vars)
KMO(socio_matrix)

# Run Principal Component Analysis (PCA) with 4 factors
pca_result <- principal(
  r = socio_matrix,
  nfactors = 4,
  rotate = "none",
  method = "regression"
)

# Print factor results
print(pca_result)
pca_result$weights
pca_result$values

# Extract PCA scores
pca_scores <- factor.scores(socio_matrix, pca_result)$scores
pca_scores <- as.data.frame(pca_scores)

# --------------------------------------------------
# Create quintiles for PC1, PC2, and PC3
# --------------------------------------------------

# TC1 = Socioindex based on PC1
cleaned_data$TC1 <- factor(
  ntile(pca_scores$PC1, 5),
  labels = c("Poorest", "Second", "Middle", "Fourth", "Richest")
)
cat("Distribution of TC1 (PC1) by WHO Region:\n")
print(table(cleaned_data$TC1, cleaned_data$WHORegion))

# TC2 = Socioindex based on PC2
cleaned_data$TC2 <- factor(
  ntile(pca_scores$PC2, 5),
  labels = c("Poorest", "Second", "Middle", "Fourth", "Richest")
)
cat("Distribution of TC2 (PC2) by WHO Region:\n")
print(table(cleaned_data$TC2, cleaned_data$WHORegion))

# TC3 = Socioindex based on PC3
cleaned_data$TC3 <- factor(
  ntile(pca_scores$PC3, 5),
  labels = c("Poorest", "Second", "Middle", "Fourth", "Richest")
)
cat("Distribution of TC3 (PC3) by WHO Region:\n")
print(table(cleaned_data$TC3, cleaned_data$WHORegion))

# --------------------------------------------------
# Re-run meta-analysis after factor creation
# --------------------------------------------------

meta_model_overall <- metaprop(
  event = cleaned_data$`Total Number of Diarrhea Cases`,
  n = cleaned_data$`Total Sample Size (n)`,
  method = "Inverse",
  sm = "PLN",
  data = cleaned_data
)

# --------------------------------------------------
# Summary of sample sizes by TC1 quintiles
# --------------------------------------------------

socioindex_summary <- cleaned_data %>%
  group_by(TC1) %>%
  summarise(
    `Total Sample Size (n)` = sum(`Total Sample Size (n)`, na.rm = TRUE),
    `Number of Observations` = n(),
    .groups = "drop"
  )

# --------------------------------------------------
# Meta-regression: Univariable on TC1
# --------------------------------------------------

meta_reg_TC1 <- metareg(
  x = meta_model_overall,
  formula = TC1,
  hakn = FALSE
)

results_TC1 <- data.frame(
  estimate = exp(meta_reg_TC1$b),
  lower.CI = exp(meta_reg_TC1$ci.lb),
  upper.CI = exp(meta_reg_TC1$ci.ub),
  pval = meta_reg_TC1$pval
)

# --------------------------------------------------
# Meta-regression: Univariable on TC2
# --------------------------------------------------

meta_reg_TC2 <- metareg(
  x = meta_model_overall,
  formula = TC2,
  hakn = FALSE
)

# --------------------------------------------------
# Meta-regression: Univariable on TC3
# --------------------------------------------------

meta_reg_TC3 <- metareg(
  x = meta_model_overall,
  formula = TC3,
  hakn = FALSE
)

# --------------------------------------------------
# Meta-regression: Multivariable with TC1 + WHORegion + Under3 + Year
# --------------------------------------------------

meta_reg_TC1_full <- metareg(
  x = meta_model_overall,
  formula = Under3 + WHORegion + TC1 + Year_calculated,
  hakn = FALSE,
  method.tau = "REML"
)

results_TC1_full <- data.frame(
  estimate = exp(meta_reg_TC1_full$b),
  lower.CI = exp(meta_reg_TC1_full$ci.lb),
  upper.CI = exp(meta_reg_TC1_full$ci.ub),
  pval = meta_reg_TC1_full$pval
)

meta_reg_TC1_full$fit.stats

# --------------------------------------------------
# Model comparisons with multiple TCs
# --------------------------------------------------

# TC1 + TC2
meta_reg_TC12 <- metareg(
  x = meta_model_overall,
  formula = Under3 + WHORegion + TC1 + TC2 + Year_calculated,
  hakn = FALSE,
  method.tau = "REML"
)
meta_reg_TC12$fit.stats

# TC1 + TC3
meta_reg_TC13 <- metareg(
  x = meta_model_overall,
  formula = Under3 + WHORegion + TC1 + TC3 + Year_calculated,
  hakn = FALSE,
  method.tau = "REML"
)
meta_reg_TC13$fit.stats

# TC1 + TC2 + TC3
meta_reg_TC123 <- metareg(
  x = meta_model_overall,
  formula = Under3 + WHORegion + TC1 + TC2 + TC3 + Year_calculated,
  hakn = FALSE,
  method.tau = "REML"
)
meta_reg_TC123$fit.stats


####################################################################################################################################################################

#Traffic plots


# Load data
rob_data_raw <- read_excel("Extraction sheet.xlsx", sheet = "data")

# Create helper columns for study name and type
rob_data_raw <- rob_data_raw %>%
  mutate(
    survey_type = ifelse(Type == "MICS", "MICS", "DHS"),
    study_id = paste(Country, Year, survey_type)
  ) %>%
  arrange(desc(Year_calculated))

# Select and rename core RoB columns
rob_data <- rob_data_raw %>%
  select(study_id, D1:D9, Overall, `WHO Region`) %>%
  rename(Study = study_id) %>%
  mutate(across(c(D1:D9, Overall), ~as.factor(.)))

# Create summary table of RoB judgments
rob_summary_table <- rob_data %>%
  pivot_longer(cols = D1:Overall, names_to = "Domain", values_to = "Rating") %>%
  group_by(Domain, Rating) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Domain) %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 1)) %>%
  arrange(Domain, Rating)

# Recode values for robvis plotting
rob_data <- rob_data %>%
  mutate(across(c(D1:D9),
                ~case_when(
                  . == "Yes" ~ "Low",
                  . == "Unclear" ~ "Unclear",
                  . == "No" ~ "High"
                )),
         Overall = case_when(
           Overall == "LOW" ~ "High",
           Overall == "MEDIUM" ~ "Unclear", 
           Overall == "HIGH" ~ "Low"
         ))

# Rename domains to descriptive names
rob_data <- rob_data %>%
  rename(
    "Overall" = Overall,
    "Appropriateness of the sampling frame" = D1,
    "Appropriateness of the sampling method" = D2,
    "Adequacy of sample size" = D3,
    "Clarity in the description of study subjects and settings" = D4,
    "Robustness of data analysis" = D5,
    "Validity of methods for condition identification" = D6,
    "Reliability of measurement of the condition" = D7,
    "Appropriateness of statistical analysis" = D8,
    "Adequacy and proper handling of response rates" = D9
  )


rob_global = rob_data %>% select(-`WHO Region`)
rob_overall = robvis::rob_summary(rob_global, tool = "Generic",overall = TRUE)
rob_overall

# Split by WHO Region
rob_afr <- rob_data %>% filter(`WHO Region` == 'AFR') %>% select(-`WHO Region`)
rob_amr <- rob_data %>% filter(`WHO Region` == 'AMR') %>% select(-`WHO Region`)
rob_emr <- rob_data %>% filter(`WHO Region` == 'EMR') %>% select(-`WHO Region`)
rob_eur <- rob_data %>% filter(`WHO Region` == 'EUR') %>% select(-`WHO Region`)
rob_sear <- rob_data %>% filter(`WHO Region` == 'SEA') %>% select(-`WHO Region`)
rob_wpr <- rob_data %>% filter(`WHO Region` == 'WPR') %>% select(-`WHO Region`)

# --------------------
# AFR region batches
# --------------------

afr_1 <- rob_afr[c(1:40), ]
afr_2 <- rob_afr[c(41:80), ]
afr_3 <- rob_afr[c(81:120), ]
afr_4 <- rob_afr[c(121:160), ]
afr_5 <- rob_afr[c(161:200), ]
afr_6 <- rob_afr[c(201:240), ]
afr_7 <- rob_afr[c(241:280), ]

plot_afr_1 <- rob_traffic_light(afr_1, tool = "Generic")
plot_afr_2 <- rob_traffic_light(afr_2, tool = "Generic")
plot_afr_3 <- rob_traffic_light(afr_3, tool = "Generic")
plot_afr_4 <- rob_traffic_light(afr_4, tool = "Generic")
plot_afr_5 <- rob_traffic_light(afr_5, tool = "Generic")
plot_afr_6 <- rob_traffic_light(afr_6, tool = "Generic")
plot_afr_7 <- rob_traffic_light(afr_7, tool = "Generic")

ggsave("plot1AFR.svg", plot_afr_1, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot2AFR.svg", plot_afr_2, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot3AFR.svg", plot_afr_3, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot4AFR.svg", plot_afr_4, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot5AFR.svg", plot_afr_5, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot6AFR.svg", plot_afr_6, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot7AFR.svg", plot_afr_7, width = 30, height = 40, units = "cm", dpi = 300)

# --------------------
# AMR region
# --------------------

amr_1 <- rob_amr[c(1:40), ]
amr_2 <- rob_amr[c(41:80), ]
amr_3 <- rob_amr[c(81:91), ]

plot_amr_1 <- rob_traffic_light(amr_1, tool = "Generic")
plot_amr_2 <- rob_traffic_light(amr_2, tool = "Generic")
plot_amr_3 <- rob_traffic_light(amr_3, tool = "Generic")

ggsave("plot1AMR.svg", plot_amr_1, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot2AMR.svg", plot_amr_2, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot3AMR.svg", plot_amr_3, width = 30, height = 15, units = "cm", dpi = 300)

# --------------------
# EMR region
# --------------------

emr_1 <- rob_emr[c(1:40), ]
emr_2 <- rob_emr[c(41:68), ]

plot_emr_1 <- rob_traffic_light(emr_1, tool = "Generic")
plot_emr_2 <- rob_traffic_light(emr_2, tool = "Generic")

ggsave("plot1EMR.svg", plot_emr_1, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot2EMR.svg", plot_emr_2, width = 30, height = 29, units = "cm", dpi = 300)

# --------------------
# EUR region
# --------------------

eur_1 <- rob_eur[c(1:40), ]
eur_2 <- rob_eur[c(41:57), ]

plot_eur_1 <- rob_traffic_light(eur_1, tool = "Generic")
plot_eur_2 <- rob_traffic_light(eur_2, tool = "Generic")

ggsave("plot1EUR.svg", plot_eur_1, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot2EUR.svg", plot_eur_2, width = 30, height = 22, units = "cm", dpi = 300)

# --------------------
# SEAR region
# --------------------

sear_1 <- rob_sear[c(1:40), ]
sear_2 <- rob_sear[c(41:56), ]

plot_sear_1 <- rob_traffic_light(sear_1, tool = "Generic")
plot_sear_2 <- rob_traffic_light(sear_2, tool = "Generic")

ggsave("plot1SEAR.svg", plot_sear_1, width = 30, height = 40, units = "cm", dpi = 300)
ggsave("plot2SEAR.svg", plot_sear_2, width = 30, height = 21, units = "cm", dpi = 300)

# --------------------
# WPR region
# --------------------

plot_wpr <- rob_traffic_light(rob_wpr, tool = "Generic")
ggsave("plot1WPR.svg", plot_wpr, width = 30, height = 41, units = "cm", dpi = 300)


