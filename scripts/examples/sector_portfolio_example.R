# Sector-Based Portfolio Optimization Example
# Demonstrates building and optimizing a sector-diversified portfolio

# Load required libraries
library(Rcpp)
library(RcppArmadillo)
source("R/SharadarData.R")
source("R/CovarianceEstimator.R")
source("config/database_config.R")

# Compile Rcpp functions
Rcpp::sourceCpp("src/rcpp_deoptim.cpp")

cat("=== SECTOR-BASED PORTFOLIO OPTIMIZATION EXAMPLE ===\n\n")

# Configuration
SECTORS <- c(
  "Technology",           # Tech giants
  "Financial Services",   # Banks, insurance
  "Healthcare",          # Pharma, biotech
  "Consumer Cyclical",   # Retail, auto
  "Communication Services", # Media, telecom
  "Industrials",         # Manufacturing
  "Consumer Defensive",  # Food, beverages
  "Energy",             # Oil & gas
  "Basic Materials",    # Chemicals, mining
  "Utilities",          # Electric, gas utilities
  "Real Estate"         # REITs
)

# Initialize data connection
sharadar <- SharadarData$new(
  DATABASE_CONFIG$server,
  DATABASE_CONFIG$database,
  DATABASE_CONFIG$username,
  DATABASE_CONFIG$password
)

# Step 1: Get sector-diversified portfolio
cat("1. Building sector-diversified portfolio\n")
cat(sprintf("   Selecting 2 stocks from each of %d sectors\n\n", length(SECTORS)))

portfolio_data <- sharadar$getReturns(
  start_date = "2023-01-01",
  end_date = "2024-12-31",
  selection_method = "sector",
  sectors = SECTORS,
  stocks_per_sector = 2
)

cat(sprintf("✓ Selected %d stocks across %d sectors\n", 
           ncol(portfolio_data$returns), length(SECTORS)))
cat(sprintf("✓ Data period: %s to %s (%d days)\n\n", 
           portfolio_data$dates[1], 
           tail(portfolio_data$dates, 1), 
           nrow(portfolio_data$returns)))

# Display selected stocks
cat("Selected stocks by ticker:\n")
cat(paste(portfolio_data$tickers, collapse = ", "), "\n\n")

# Step 2: Optimize portfolio weights
cat("2. Optimizing portfolio weights\n")

# Initialize covariance estimator
covariance_estimator <- CovarianceEstimator$new(
  portfolio_data$returns,
  portfolio_data$dates,
  portfolio_data$tickers
)

# Use Ledoit-Wolf estimation
covariance_estimator$setParameters(
  methods = c("Ledoit-Wolf"),
  training_days = nrow(portfolio_data$returns) - 50
)

# Estimate covariance
all_covariances <- covariance_estimator$estimateAllCovariances(portfolio_data$returns)
cov_matrix <- all_covariances$LedoitWolf

# Ensure positive definite
min_eig <- min(eigen(cov_matrix, only.values = TRUE)$values)
if (min_eig <= 1e-8) {
  cov_matrix <- cov_matrix + (1e-6 - min_eig) * diag(ncol(portfolio_data$returns))
}

# Optimize using Sharpe ratio maximization
cat("   Running differential evolution optimization...\n")
optimal_weights <- deoptim_portfolio_rcpp(
  returns = portfolio_data$returns,
  cov_matrix = cov_matrix,
  min_weight = 0.02,      # 2% minimum
  max_weight = 0.10,      # 10% maximum
  pop_size = 100,
  max_iter = 200,
  F = 0.8,
  CR = 0.9,
  ncores = 4,
  DEBUG = FALSE,
  objective = "max_sharpe",
  risk_free_rate = 0.04   # 4% annual
)

cat("✓ Optimization complete\n\n")

# Step 3: Compare with equal weight portfolio
equal_weights <- rep(1/ncol(portfolio_data$returns), ncol(portfolio_data$returns))

# Calculate portfolio returns
optimal_returns <- portfolio_data$returns %*% optimal_weights
equal_returns <- portfolio_data$returns %*% equal_weights

# Calculate performance metrics
calculate_metrics <- function(returns, name) {
  annual_return <- mean(returns) * 252
  annual_vol <- sd(returns) * sqrt(252)
  sharpe <- (annual_return - 0.04) / annual_vol
  
  list(
    name = name,
    annual_return = annual_return * 100,
    annual_vol = annual_vol * 100,
    sharpe = sharpe
  )
}

opt_metrics <- calculate_metrics(optimal_returns, "Optimized")
eq_metrics <- calculate_metrics(equal_returns, "Equal Weight")

# Display results
cat("3. Performance Comparison\n")
cat(sprintf("%-15s %10s %10s %10s\n", "Portfolio", "Return %", "Vol %", "Sharpe"))
cat(paste(rep("-", 50), collapse = ""), "\n")
cat(sprintf("%-15s %10.1f %10.1f %10.3f\n", 
           opt_metrics$name, opt_metrics$annual_return, 
           opt_metrics$annual_vol, opt_metrics$sharpe))
cat(sprintf("%-15s %10.1f %10.1f %10.3f\n", 
           eq_metrics$name, eq_metrics$annual_return, 
           eq_metrics$annual_vol, eq_metrics$sharpe))

# Show top holdings
cat("\n4. Top 10 Holdings (Optimized Portfolio)\n")
top_indices <- order(optimal_weights, decreasing = TRUE)[1:10]
for (i in top_indices) {
  cat(sprintf("   %-6s: %5.1f%%\n", 
             portfolio_data$tickers[i], 
             optimal_weights[i] * 100))
}

# Export results
output_dir <- "exports"
if (!dir.exists(output_dir)) dir.create(output_dir)

weights_df <- data.frame(
  ticker = portfolio_data$tickers,
  optimal_weight = optimal_weights * 100,
  equal_weight = equal_weights * 100,
  stringsAsFactors = FALSE
)

output_file <- paste0(output_dir, "/sector_portfolio_weights_", 
                     format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
write.csv(weights_df, output_file, row.names = FALSE)

cat(sprintf("\n✓ Portfolio weights exported to: %s\n", output_file))
cat("\n✓ Sector portfolio optimization example complete!\n")