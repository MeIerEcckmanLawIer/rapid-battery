library(jsonlite)
library(psych)

data_matrix <- fromJSON("data.json")
data_matrix <- as.data.frame(data_matrix)
colnames(data_matrix) <- c( "Raven's Matrix #1", "Raven's Matrix #2", "Raven's Matrix #3", "Raven's Matrix #4", 'Visual Puzzle #1', 'Visual Puzzle #2', 'Visual Puzzle #3', 'Visual Puzzle #4', 'Symbol Span #1', 'Symbol Span #2', 'Symbol Span #3', 'Symbol Span #4')

cor_matrix <- cor(data_matrix)
eigenvalues <- eigen(cor_matrix)$values

plot(eigenvalues, type = "b", main = "Scree Plot for Factor Analysis", col = "red", xlab = "Factor Number", ylab = "Eigenvalue", pch = 19, cex = 1.5)

omega_result <- omega(data_matrix, digits = 3, fm="pa", sl = FALSE, n.obs = 129, nfactors = 3,plot = TRUE, )
round(sqrt(omega_result$omega_h), 2)

# Rscript factor-analysis.r