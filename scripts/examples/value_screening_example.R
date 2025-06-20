# Value Stock Screening Example
# Demonstrates how to find undervalued S&P 500 stocks using fundamental criteria

# Load required libraries and source files
source("R/SharadarData.R")
source("config/database_config.R")

cat("=== VALUE STOCK SCREENING EXAMPLE ===\n\n")

# Initialize data connection
sharadar <- SharadarData$new(
  DATABASE_CONFIG$server,
  DATABASE_CONFIG$database,
  DATABASE_CONFIG$username,
  DATABASE_CONFIG$password
)

# Example 1: Basic value screening with default parameters
cat("1. Basic Value Screening\n")
cat("   Criteria: P/E <= 15, P/B <= 3, Dividend Yield >= 2%\n\n")

value_stocks <- sharadar$screenValueStocks(
  min_dividend_yield = 2.0,
  max_pe = 15.0,
  max_pb = 3.0,
  min_market_cap = 5000,  # $5B minimum
  sp500_only = TRUE
)

if (nrow(value_stocks) > 0) {
  cat(sprintf("Found %d value stocks meeting criteria\n\n", nrow(value_stocks)))
  
  # Display top 10 by dividend yield
  cat("Top 10 by dividend yield:\n")
  top_stocks <- head(value_stocks[order(-value_stocks$dividend_yield_pct), ], 10)
  print(top_stocks[, c("ticker", "sector", "pe", "pb", "dividend_yield_pct", "market_cap_millions")])
  
  # Summary statistics
  cat("\nPortfolio Summary:\n")
  cat(sprintf("Average P/E: %.1f\n", mean(value_stocks$pe)))
  cat(sprintf("Average P/B: %.2f\n", mean(value_stocks$pb)))
  cat(sprintf("Average Dividend Yield: %.1f%%\n", mean(value_stocks$dividend_yield_pct)))
  cat(sprintf("Total Market Cap: $%.0fB\n", sum(value_stocks$market_cap_millions) / 1000))
} else {
  cat("No stocks found meeting criteria. Try relaxing the filters.\n")
}

# Example 2: Sector-specific value screening
cat("\n\n2. Sector-Specific Value Screening\n")
cat("   Looking for value in Financial Services sector\n\n")

financial_value <- sharadar$screenValueStocks(
  sector = "Financial Services",
  min_dividend_yield = 2.5,
  max_pe = 12.0,
  max_pb = 1.5,
  sp500_only = TRUE
)

if (nrow(financial_value) > 0) {
  cat(sprintf("Found %d financial sector value stocks\n", nrow(financial_value)))
  print(financial_value[, c("ticker", "industry", "pe", "pb", "dividend_yield_pct")])
}

# Example 3: Finding 15-20 stocks for a portfolio
cat("\n\n3. Building a 20-Stock Value Portfolio\n")
cat("   Using relaxed criteria to get ~20 stocks\n\n")

portfolio_stocks <- sharadar$screenValueStocks(
  min_market_cap = 5000,      # $5B+ for liquidity
  min_dividend_yield = 1.5,   # Decent yield
  max_pe = 20.0,              # Reasonable P/E
  max_pb = 3.5,               # Slightly relaxed P/B
  sp500_only = TRUE
)

# Take top 20 by dividend yield
if (nrow(portfolio_stocks) >= 20) {
  portfolio_20 <- head(portfolio_stocks[order(-portfolio_stocks$dividend_yield_pct), ], 20)
  
  cat(sprintf("Selected %d stocks for portfolio\n\n", nrow(portfolio_20)))
  
  # Sector breakdown
  cat("Sector Diversification:\n")
  sector_counts <- table(portfolio_20$sector)
  for (sector in names(sort(sector_counts, decreasing = TRUE))) {
    cat(sprintf("  %s: %d stocks (%.0f%%)\n", 
               sector, sector_counts[sector], 
               sector_counts[sector] / sum(sector_counts) * 100))
  }
  
  # Export results
  output_file <- paste0("exports/value_portfolio_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  if (!dir.exists("exports")) dir.create("exports")
  write.csv(portfolio_20, output_file, row.names = FALSE)
  cat(sprintf("\nPortfolio exported to: %s\n", output_file))
}

cat("\n✓ Value screening example complete!\n")