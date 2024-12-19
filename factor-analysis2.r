# Install and load necessary libraries
# install.packages("lavaan", repos = "https://cloud.r-project.org/")
# install.packages("semPlot", repos = "https://cloud.r-project.org/")
# install.packages("psych", repos = "https://cloud.r-project.org/")
library(psych)
library(lavaan)
library(jsonlite)
library(semPlot)

# Load your data
data <- fromJSON("data2.json")

# Convert into a data frame
df <- as.data.frame(data)

# Rename columns to match the model (adjust based on your actual column names)
colnames(df) <- c(
   "WordCloze1", "WordCloze2", "WordCloze3", "WordCloze4", "WordCloze5", "WordCloze6", "WordCloze7", "WordCloze8", 
   "RavenMatrix1", "RavenMatrix2", "RavenMatrix3", "RavenMatrix4",
   "PuzzlePieces1", "PuzzlePieces2", "PuzzlePieces3", "PuzzlePieces4", 
   "SymbolSpan1", "SymbolSpan2", "SymbolSpan3", "SymbolSpan4"
)

# Ensure data is numeric
df[] <- lapply(df, as.numeric)

# Remove rows with missing values
df <- na.omit(df)

# Check the structure of the data
str(df)

# Specify the bifactor model
model <- '
  # General factor
  g =~ WordCloze1 + WordCloze2 + WordCloze3 + WordCloze4 + WordCloze5 + WordCloze6 + WordCloze7 + WordCloze8 + RavenMatrix1 + RavenMatrix2 + RavenMatrix3 + RavenMatrix4 + PuzzlePieces1 + PuzzlePieces2 + PuzzlePieces3 + PuzzlePieces4 + SymbolSpan1 + SymbolSpan2 + SymbolSpan3 + SymbolSpan4

  # Group factors
  Verbal =~ WordCloze1 + WordCloze2 + WordCloze3 + WordCloze4 + WordCloze5 + WordCloze6 + WordCloze7 + WordCloze8
  Logic =~ RavenMatrix1 + RavenMatrix2 + RavenMatrix3 + RavenMatrix4
  Visual =~ PuzzlePieces1 + PuzzlePieces2 + PuzzlePieces3 + PuzzlePieces4
  Memory =~ SymbolSpan1 + SymbolSpan2 + SymbolSpan3 + SymbolSpan4

  # Orthogonal constraints between general and group factors
  g ~~ 0*Logic
  g ~~ 0*Visual
  g ~~ 0*Memory

  # Orthogonal constraints between group factors
  Verbal ~~ 0*Logic
  Verbal ~~ 0*Visual
  Verbal ~~ 0*Memory
  Logic ~~ 0*Visual
  Logic ~~ 0*Memory
  Visual ~~ 0*Memory
'
# Fit the model
fit <- cfa(model, data = df, estimator = "ML", orthogonal = TRUE)

# Display summary results
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Extract parameter estimates
params <- parameterEstimates(fit)
library(dplyr)

# Separate general and specific factors
loadings_G <- params %>%
  filter(op == "=~" & lhs == "g") %>%
  pull(est)  # Unstandardized loadings for general factor

loadings_Verbal <- params %>%
  filter(op == "=~" & lhs == "Verbal") %>%
  pull(est)  # Unstandardized loadings for Verbal factor

loadings_Logic <- params %>%
  filter(op == "=~" & lhs == "Logic") %>%
  pull(est)  # Unstandardized loadings for Logic factor

loadings_Visual <- params %>%
  filter(op == "=~" & lhs == "Visual") %>%
  pull(est)  # Unstandardized loadings for Visual factor

loadings_Memory <- params %>%
  filter(op == "=~" & lhs == "Memory") %>%
  pull(est)  # Unstandardized loadings for Memory factor

# Extract error variances
error_vars <- params %>%
  filter(op == "~~" & lhs == rhs) %>%  # Residual variances (diagonal elements)
  pull(est)

# Compute squared sums
squared_sum_G <- sum(loadings_G^2)
squared_sum_Verbal <- sum(loadings_Verbal^2)
squared_sum_Logic <- sum(loadings_Logic^2)
squared_sum_Visual <- sum(loadings_Visual^2)
squared_sum_Memory <- sum(loadings_Memory^2)
total_error_variance <- sum(error_vars)

# Compute omega hierarchical
omega_hierarchical <- squared_sum_G / 
  (squared_sum_G + squared_sum_Verbal + squared_sum_Logic + squared_sum_Visual + squared_sum_Memory + total_error_variance)

cat("Omega Hierarchical = ", omega_hierarchical, "\n")

# General factor loading (g) calculation
g_loading <- sqrt(omega_hierarchical)
cat("G-loading (uncorrected) = ", g_loading, "\n")

# --- Spearman's Law of Diminishing Returns (SLODR) Calculation ---
# Calculate the SLODR adjusted g-loading
sample_mean_iq <- 120
g_slodr_adjusted <- g_loading^exp(-(sample_mean_iq - 100) / (2 * 15)) # Apply the SLODR correction
cat("G-loading (corrected for SLODR) = ", g_slodr_adjusted, "\n")

# Extract goodness-of-fit metrics
fit_measures <- lavaan::fitMeasures(fit, c("pvalue", "gfi", "agfi", "nfi", "nnfi", "tli", 
                                           "cfi", "rmsea", "rmr", "srmr", "rfi", "ifi", "pnfi"))

# Define a function to generate the report
generate_gof_report <- function(fit_measures) {
  # Initialize output string
  output <- "Goodness-of-fit metrics:\n\n"
  
  # P(χ²) - Chi-square p-value
  pvalue <- round(fit_measures["pvalue"], 3)
  if (!is.na(pvalue)) {
    status <- if (pvalue > 0.05) "[✔]" else "[ ]"
    output <- paste0(output, status, " P(χ²): ", pvalue, "\n")
  }
  
# GFI - Goodness of Fit
gfi <- round(fit_measures["gfi"], 3)
status_gfi <- if (gfi >= 0.95) "[✔]" else "[ ]"
output <- paste0(output, status_gfi, " GFI: ", gfi, "\n")

# AGFI - Adjusted Goodness of Fit
agfi <- round(fit_measures["agfi"], 3)
status_agfi <- if (agfi >= 0.9) "[✔]" else "[ ]"
output <- paste0(output, status_agfi, " AGFI: ", agfi, "\n")
  
  # NFI - Normed Fit Index
nfi <- round(fit_measures["nfi"], 3)
status_nfi <- if (nfi >= 0.9) "[✔]" else "[ ]"
output <- paste0(output, status_nfi, " NFI: ", nfi, "\n")

# NNFI - Non-Normed Fit Index
nnfi <- round(fit_measures["nnfi"], 3)
status_nnfi <- if (nnfi >= 0.95) "[✔]" else "[ ]"
output <- paste0(output, status_nnfi, " NNFI/TLI: ", nnfi, "\n")
  
  # CFI - Comparative Fit Index
  cfi <- fit_measures["cfi"]
  status <- if (!is.na(cfi) && cfi >= 0.9) "[✔]" else "[ ]"
  output <- paste0(output, status, " CFI: ", ifelse(!is.na(cfi), round(cfi, 3), "N/A"), "\n")
  
  # RMSEA - Root Mean Square Error of Approximation
  rmsea <- fit_measures["rmsea"]
  status <- if (!is.na(rmsea) && rmsea < 0.08) "[✔]" else "[ ]"
  output <- paste0(output, status, " RMSEA: ", ifelse(!is.na(rmsea), round(rmsea, 3), "N/A"), "\n")
  
# RMR - Root Mean Square Residual
rmr <- round(fit_measures["rmr"], 3)
status_rmr <- if (rmr < 0.08) "[✔]" else "[ ]"
output <- paste0(output, status_rmr, " RMR: ", rmr, "\n")

# SRMR - Standardized Root Mean Square Residual
srmr <- round(fit_measures["srmr"], 3)
status_srmr <- if (srmr < 0.08) "[✔]" else "[ ]"
output <- paste0(output, status_srmr, " SRMR: ", srmr, "\n")

  
  # RFI - Relative Fit Index
  rfi <- fit_measures["rfi"]
  status <- if (!is.na(rfi) && rfi >= 0.9) "[✔]" else "[ ]"
  output <- paste0(output, status, " RFI: ", ifelse(!is.na(rfi), round(rfi, 3), "N/A"), "\n")
  
  # IFI - Incremental Fit Index
  ifi <- fit_measures["ifi"]
  status <- if (!is.na(ifi) && ifi >= 0.9) "[✔]" else "[ ]"
  output <- paste0(output, status, " IFI: ", ifelse(!is.na(ifi), round(ifi, 3), "N/A"), "\n")
  
  # PNFI - Parsimony Normed Fit Index
  pnfi <- fit_measures["pnfi"]
  status <- if (pnfi > 0.5) "[✔]" else "[ ]"
  output <- paste0(output, status, " PNFI: ", round(pnfi, 3), "\n")
  
  # Print the final output
  cat(output)
}

# Call the function with fit_measures
generate_gof_report(fit_measures)

# Plot the bifactor model
semPaths(
  fit,
  whatLabels = "std",         # Display standardized estimates
  layout = "tree2",           # Bifactor structure layout
  style = "lisrel",           # LISREL style for clarity
  sizeMan = 4,                # Size of manifest (observed) variables
  sizeLat = 10,               # Size of latent variables
  edge.label.cex = 0.8,       # Size of edge labels
  optimizeLatRes = TRUE,      # Optimize layout of latent residuals
  residuals = FALSE,          # Hide residual arrows
  rotation = 2,               # Rotate for better alignment
  intercepts = FALSE,         # Hide intercept arrows
  bifactor = "g",
  exoCov = FALSE
)

# Rscript factor-analysis2.r