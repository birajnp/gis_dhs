# Calculate VIF for continuous variables
library(car)

# Create model matrix 
X <- model.matrix(~ systolic_bp + diastolic_bp + new_bmi + hv105, 
                  data = svy_dataset$variables)

# Remove intercept column
X <- X[,-1]

# Convert to data frame
X_df <- as.data.frame(X)

# Calculate VIF
vif_values <- vif(lm(systolic_bp ~ ., data = X_df))
print("Variance Inflation Factors:")
print(round(vif_values, 2))

# Correlation matrix with significance levels
cor_matrix <- cor(X_df, use = "complete.obs")
print("\nCorrelation Matrix:")
print(round(cor_matrix, 3))

# General rule of thumb:
# VIF > 5-10: Indicates problematic collinearity
# Correlation > 0.7: Strong correlation that might indicate collinearity


# Create contingency tables to run chi-square tests
cat_vars <- c("hv104", "age_cat", "marital_status", "edu_cat", 
              "wealth_cat", "blood_pressure_cat_new", "bmi_category", "hv025", "shecoreg")

# Create empty matrix for Cramer's V
n_cat <- length(cat_vars)
cramer_matrix <- matrix(NA, n_cat, n_cat)
rownames(cramer_matrix) <- cat_vars
colnames(cramer_matrix) <- cat_vars

for(i in 1:n_cat) {
  for(j in 1:n_cat) {
    if(i != j) {
      # Create contingency table
      cont_table <- table(svy_dataset$variables[[cat_vars[i]]], 
                          svy_dataset$variables[[cat_vars[j]]])
      
      # Calculate chi-square
      chi_sq <- chisq.test(cont_table)
      
      # Calculate Cramer's V
      n <- sum(cont_table)
      min_dim <- min(dim(cont_table)) - 1
      cramer_v <- sqrt(chi_sq$statistic / (n * min_dim))
      
      cramer_matrix[i,j] <- round(cramer_v, 3)
    } else {
      cramer_matrix[i,j] <- 1
    }
  }
}

print(cramer_matrix)
