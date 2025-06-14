# Covariance Estimator Analysis Example
# Demonstrates advanced covariance estimation across different portfolio types

# Load required libraries and classes
source("R/SharadarData.R")
source("R/CovarianceEstimator.R")
source("config/database_config.R")

# Initialize data connection
cat("Initializing analysis framework...\n")
sharadar <- SharadarData$new(
  DATABASE_CONFIG$server,
  DATABASE_CONFIG$database,
  DATABASE_CONFIG$username,
  DATABASE_CONFIG$password
)

# =============================================================================
# PORTFOLIO CONSTRUCTION COMPARISON
# =============================================================================

cat("\n=== PORTFOLIO CONSTRUCTION COMPARISON ===\n")

# Test different portfolio construction approaches
portfolio_configs <- list(
  
  # High liquidity approach (traditional)
  "high_liquidity" = list(
    method = "liquidity",
    n_stocks = 50,
    description = "Top 50 most liquid S&P 500 stocks"
  ),
  
  # Sector diversified approach
  "sector_diversified" = list(
    method = "sector", 
    sectors = c("Technology", "Financial Services", "Healthcare", 
               "Consumer Cyclical", "Industrials", "Energy"),
    stocks_per_sector = 8,
    description = "Sector-diversified portfolio (8 stocks per sector)"
  ),
  
  # Value-focused approach
  "value_focused" = list(
    method = "sector",
    sectors = c("Financial Services", "Utilities", "Energy", 
               "Consumer Defensive", "Real Estate"),
    stocks_per_sector = 6,
    description = "Value-oriented sectors (6 stocks per sector)"
  )
)

# Store results for comparison
portfolio_results <- list()

for (config_name in names(portfolio_configs)) {
  config <- portfolio_configs[[config_name]]
  
  cat(sprintf("\n--- Analyzing %s ---\n", config$description))
  
  # Get portfolio data
  if (config$method == "liquidity") {
    portfolio_data <- sharadar$getReturns(
      "2014-01-01", "2016-12-31",
      selection_method = "liquidity",
      n_stocks = config$n_stocks
    )
  } else {
    portfolio_data <- sharadar$getReturns(
      "2014-01-01", "2016-12-31", 
      selection_method = "sector",
      sectors = config$sectors,
      stocks_per_sector = config$stocks_per_sector
    )
  }
  
  # Run covariance analysis
  estimator <- CovarianceEstimator$new(
    portfolio_data$returns,
    portfolio_data$dates,
    portfolio_data$tickers
  )
  
  results <- estimator$runComparison()
  
  # Export results
  export_files <- estimator$exportResults(
    filename = paste0("covariance_", config_name),
    output_dir = "exports"
  )
  
  # Store for comparison
  portfolio_results[[config_name]] <- list(
    config = config,
    results = results,
    n_assets = ncol(portfolio_data$returns),
    mean_correlation = mean(cor(portfolio_data$returns)),
    export_files = export_files
  )
  
  cat(sprintf("Results exported to: %s\n", dirname(export_files$summary)))
}

# =============================================================================
# CROSS-PORTFOLIO COMPARISON
# =============================================================================

cat("\n=== CROSS-PORTFOLIO COMPARISON ===\n")

# Create comparison summary
comparison_summary <- data.frame(
  portfolio = character(),
  n_assets = numeric(),
  mean_correlation = numeric(),
  best_method = character(),
  best_risk = numeric(),
  scm_risk = numeric(),
  improvement = numeric(),
  stringsAsFactors = FALSE
)

for (config_name in names(portfolio_results)) {
  result <- portfolio_results[[config_name]]
  
  # Find best performing method
  best_idx <- which.min(result$results$avg_risks)
  best_method <- result$results$config$methods[best_idx]
  best_risk <- result$results$avg_risks[best_idx]
  scm_risk <- result$results$avg_risks[1]  # SCM is always first
  
  comparison_summary <- rbind(comparison_summary, data.frame(
    portfolio = result$config$description,
    n_assets = result$n_assets,
    mean_correlation = round(result$mean_correlation, 4),
    best_method = best_method,
    best_risk = round(best_risk, 2),
    scm_risk = round(scm_risk, 2),
    improvement = round(scm_risk - best_risk, 3),
    stringsAsFactors = FALSE
  ))
}

cat("Portfolio comparison summary:\n")
print(comparison_summary)

# =============================================================================
# METHOD PERFORMANCE ANALYSIS
# =============================================================================

cat("\n=== METHOD PERFORMANCE ANALYSIS ===\n")

# Analyze which methods work best across different portfolio types
method_performance <- data.frame(
  method = character(),
  portfolio_type = character(), 
  risk = numeric(),
  improvement_vs_scm = numeric(),
  stringsAsFactors = FALSE
)

for (config_name in names(portfolio_results)) {
  result <- portfolio_results[[config_name]]
  
  for (i in seq_along(result$results$config$methods)) {
    method_name <- result$results$config$methods[i]
    risk <- result$results$avg_risks[i]
    improvement <- result$results$improvements[i]
    
    method_performance <- rbind(method_performance, data.frame(
      method = method_name,
      portfolio_type = config_name,
      risk = risk,
      improvement_vs_scm = improvement,
      stringsAsFactors = FALSE
    ))
  }
}

# Find best method for each portfolio type
best_methods <- method_performance %>%
  group_by(portfolio_type) %>%
  filter(risk == min(risk)) %>%
  select(portfolio_type, method, risk, improvement_vs_scm)

cat("Best method for each portfolio type:\n")
print(as.data.frame(best_methods))

# =============================================================================
# CORRELATION STRUCTURE ANALYSIS
# =============================================================================

cat("\n=== CORRELATION STRUCTURE ANALYSIS ===\n")

# Analyze how portfolio correlation affects optimal method selection
correlation_analysis <- comparison_summary %>%
  select(portfolio, mean_correlation, best_method, improvement) %>%
  arrange(mean_correlation)

cat("Correlation vs. optimal method:\n")
print(correlation_analysis)

# =============================================================================
# SHRINKAGE INTENSITY ANALYSIS
# =============================================================================

cat("\n=== SHRINKAGE INTENSITY ANALYSIS ===\n")

# Compare shrinkage intensities across portfolio types
shrinkage_comparison <- data.frame(
  portfolio = character(),
  ledoit_wolf = numeric(),
  identity = numeric(), 
  diagonal = numeric(),
  stringsAsFactors = FALSE
)

for (config_name in names(portfolio_results)) {
  result <- portfolio_results[[config_name]]
  
  if (!is.null(result$results$shrinkage_intensities)) {
    shrinkage_means <- colMeans(result$results$shrinkage_intensities, na.rm = TRUE)
    
    shrinkage_comparison <- rbind(shrinkage_comparison, data.frame(
      portfolio = config_name,
      ledoit_wolf = round(shrinkage_means[1], 3),
      identity = round(shrinkage_means[2], 3),
      diagonal = round(shrinkage_means[3], 3),
      stringsAsFactors = FALSE
    ))
  }
}

cat("Average shrinkage intensities by portfolio type:\n")
print(shrinkage_comparison)

# =============================================================================
# EXPORT COMPREHENSIVE COMPARISON
# =============================================================================

cat("\n=== EXPORTING COMPREHENSIVE COMPARISON ===\n")

# Create output directory
comparison_dir <- file.path("exports", "covariance_comparison")
if (!dir.exists(comparison_dir)) {
  dir.create(comparison_dir, recursive = TRUE)
}

# Export comparison tables
write.csv(comparison_summary, 
          file.path(comparison_dir, "portfolio_comparison.csv"),
          row.names = FALSE)

write.csv(method_performance,
          file.path(comparison_dir, "method_performance.csv"),
          row.names = FALSE)

write.csv(shrinkage_comparison,
          file.path(comparison_dir, "shrinkage_analysis.csv"),
          row.names = FALSE)

# Create executive summary
executive_summary <- data.frame(
  finding = c(
    "Total portfolios analyzed",
    "Most effective method overall", 
    "Portfolio with lowest risk",
    "Highest correlation portfolio",
    "Lowest correlation portfolio",
    "Average improvement vs SCM"
  ),
  result = c(
    length(portfolio_results),
    names(sort(table(best_methods$method), decreasing = TRUE))[1],
    comparison_summary$portfolio[which.min(comparison_summary$best_risk)],
    comparison_summary$portfolio[which.max(comparison_summary$mean_correlation)],
    comparison_summary$portfolio[which.min(comparison_summary$mean_correlation)],
    paste0(round(mean(comparison_summary$improvement), 3), "%")
  )
)

write.csv(executive_summary,
          file.path(comparison_dir, "executive_summary.csv"),
          row.names = FALSE)

cat("Comprehensive comparison exported to:", comparison_dir, "\n")

# =============================================================================
# KEY INSIGHTS SUMMARY
# =============================================================================

cat("\n=== KEY INSIGHTS ===\n")

cat("1. Portfolio Construction Impact:\n")
for (i in 1:nrow(comparison_summary)) {
  row <- comparison_summary[i, ]
  cat(sprintf("   %s: %.2f%% risk (%s method)\n", 
             row$portfolio, row$best_risk, row$best_method))
}

cat("\n2. Method Effectiveness:\n")
method_wins <- table(best_methods$method)
for (method in names(method_wins)) {
  cat(sprintf("   %s: Best for %d portfolio type(s)\n", method, method_wins[method]))
}

cat("\n3. Correlation Structure:\n")
cat(sprintf("   Range: %.4f to %.4f\n", 
           min(comparison_summary$mean_correlation),
           max(comparison_summary$mean_correlation)))
cat(sprintf("   Lower correlation generally favors: %s\n", 
           correlation_analysis$best_method[1]))

cat("\nAnalysis complete! Check 'exports/covariance_comparison/' for detailed results.\n")


