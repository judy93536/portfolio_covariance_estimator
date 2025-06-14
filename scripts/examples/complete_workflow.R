# Complete Portfolio Analysis Workflow
source("R/SharadarData.R")
source("R/CovarianceEstimator.R")
source("config/database_config.R")  # Your credentials

# Initialize data connection
sharadar <- SharadarData$new(
  DATABASE_CONFIG$server,
  DATABASE_CONFIG$database, 
  DATABASE_CONFIG$username,
  DATABASE_CONFIG$password
)

# Screen for value stocks
value_stocks <- sharadar$screenValueStocks(
  min_dividend_yield = 2.0,
  max_pe = 15.0,
  sp500_only = TRUE
)

# Get sector-based portfolio
portfolio_data <- sharadar$getReturns(
  "2014-01-01", "2016-12-31",
  selection_method = "sector",
  sectors = c("Technology", "Financial Services", "Healthcare"),
  stocks_per_sector = 10
)

# Run covariance analysis
estimator <- CovarianceEstimator$new(
  portfolio_data$returns, 
  portfolio_data$dates, 
  portfolio_data$tickers
)
results <- estimator$runComparison()

# Export everything
export_result <- sharadar$exportCompleteAnalysis(
  value_stocks = value_stocks,
  output_dir = "exports",
  project_name = "complete_analysis"
)

