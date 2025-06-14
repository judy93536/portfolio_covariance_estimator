# Value Stock Screening Example
# Demonstrates comprehensive value investing analysis using Sharadar data

# Load required libraries and classes
source("R/SharadarData.R")
source("config/database_config.R")

# Initialize data connection
cat("Initializing Sharadar data connection...\n")
sharadar <- SharadarData$new(
  DATABASE_CONFIG$server,
  DATABASE_CONFIG$database,
  DATABASE_CONFIG$username,
  DATABASE_CONFIG$password
)

# =============================================================================
# BASIC VALUE SCREENING
# =============================================================================

cat("\n=== BASIC VALUE STOCK SCREENING ===\n")

# Screen for S&P 500 value stocks with conservative criteria
value_stocks <- sharadar$screenValueStocks(
  min_dividend_yield = 2.5,    # At least 2.5% yield
  max_pe = 15.0,               # P/E ratio under 15
  max_pb = 3.0,                # P/B ratio under 3
  min_market_cap = 10000,      # $10B+ market cap (large caps only)
  sp500_only = TRUE            # S&P 500 constituents only
)

cat(sprintf("Found %d value stocks meeting criteria\n", nrow(value_stocks)))
cat("Top 10 value opportunities:\n")
print(head(value_stocks, 10))

# =============================================================================
# SECTOR-SPECIFIC VALUE ANALYSIS
# =============================================================================

cat("\n=== SECTOR-SPECIFIC VALUE ANALYSIS ===\n")

# Analyze value opportunities by sector
value_sectors <- c("Financial Services", "Utilities", "Energy", 
                  "Consumer Defensive", "Communication Services")

for (sector in value_sectors) {
  sector_value <- sharadar$screenValueStocks(
    sector = sector,
    min_dividend_yield = 2.0,
    max_pe = 15.0,
    max_pb = 3.0,
    min_market_cap = 5000,
    sp500_only = TRUE
  )
  
  if (nrow(sector_value) > 0) {
    cat(sprintf("\n%s: %d stocks found\n", sector, nrow(sector_value)))
    cat(sprintf("  Top pick: %s (%.1f%% yield, %.1f P/E)\n", 
               sector_value$ticker[1], 
               sector_value$dividend_yield_pct[1],
               sector_value$pe[1]))
  } else {
    cat(sprintf("\n%s: No stocks meeting criteria\n", sector))
  }
}

# =============================================================================
# DIVIDEND ANALYSIS
# =============================================================================

cat("\n=== DIVIDEND SUSTAINABILITY ANALYSIS ===\n")

# Analyze dividend history for top 10 value stocks
top_value_tickers <- head(value_stocks$ticker, 10)

dividend_analysis <- sharadar$getDividendAnalysis(
  top_value_tickers, 
  years_back = 10
)

# Summarize dividend consistency
dividend_summary <- dividend_analysis %>%
  group_by(ticker) %>%
  summarise(
    years_of_data = n(),
    avg_annual_dividend = mean(annual_dividend, na.rm = TRUE),
    dividend_cuts = sum(no_cut_flag == 0, na.rm = TRUE),
    dividend_consistency = sum(no_cut_flag == 1, na.rm = TRUE) / sum(!is.na(no_cut_flag)) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(dividend_consistency))

cat("Dividend consistency analysis (top 5):\n")
print(head(dividend_summary, 5))

# =============================================================================
# PRICE POSITIONING ANALYSIS  
# =============================================================================

cat("\n=== CURRENT PRICE POSITIONING ===\n")

# Get current price metrics for all value stocks
price_metrics <- sharadar$getPriceMetrics(value_stocks$ticker)

# Merge with fundamental data
complete_analysis <- merge(value_stocks, price_metrics, by = "ticker", all.x = TRUE)

# Identify deep value opportunities (stocks near 52-week lows)
deep_value <- complete_analysis[
  !is.na(complete_analysis$pct_of_52w_range) & 
  complete_analysis$pct_of_52w_range < 40, 
]

if (nrow(deep_value) > 0) {
  cat(sprintf("Found %d stocks trading in bottom 40%% of 52-week range:\n", nrow(deep_value)))
  deep_value_summary <- deep_value[, c("ticker", "dividend_yield_pct", "pe", 
                                      "current_price", "pct_of_52w_range", "pct_off_high")]
  print(head(deep_value_summary, 5))
} else {
  cat("No stocks currently trading in bottom 40% of 52-week range\n")
}

# =============================================================================
# HIGH-YIELD OPPORTUNITIES
# =============================================================================

cat("\n=== HIGH-YIELD OPPORTUNITIES ===\n")

# Focus on highest yielding stocks with reasonable valuations
high_yield <- value_stocks[
  value_stocks$dividend_yield_pct >= 4.0 & 
  value_stocks$pe <= 12.0,
]

if (nrow(high_yield) > 0) {
  cat(sprintf("Found %d high-yield stocks (4%+ yield, P/E <= 12):\n", nrow(high_yield)))
  high_yield_summary <- high_yield[, c("ticker", "sector", "dividend_yield_pct", 
                                      "pe", "pb", "market_cap_millions")]
  print(head(high_yield_summary, 5))
} else {
  cat("No stocks meeting high-yield criteria\n")
}

# =============================================================================
# EXPORT RESULTS
# =============================================================================

cat("\n=== EXPORTING ANALYSIS RESULTS ===\n")

# Export comprehensive analysis
export_result <- sharadar$exportCompleteAnalysis(
  value_stocks = complete_analysis,
  dividend_analysis = dividend_analysis,
  price_metrics = price_metrics,
  output_dir = "exports",
  project_name = "value_stock_analysis"
)

cat("Analysis exported to:", export_result$output_directory, "\n")

# Create summary report
summary_stats <- data.frame(
  metric = c("total_value_stocks", "avg_dividend_yield", "avg_pe_ratio", 
            "avg_market_cap_billions", "stocks_with_price_data"),
  value = c(
    nrow(value_stocks),
    round(mean(value_stocks$dividend_yield_pct), 2),
    round(mean(value_stocks$pe), 2),
    round(mean(value_stocks$market_cap_millions) / 1000, 1),
    nrow(price_metrics)
  )
)

write.csv(summary_stats, 
          file.path(export_result$output_directory, "summary_statistics.csv"),
          row.names = FALSE)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Key findings:\n")
cat(sprintf("- Found %d S&P 500 value stocks\n", nrow(value_stocks)))
cat(sprintf("- Average dividend yield: %.2f%%\n", mean(value_stocks$dividend_yield_pct)))
cat(sprintf("- Average P/E ratio: %.1f\n", mean(value_stocks$pe)))
cat(sprintf("- %d stocks have complete price data\n", nrow(price_metrics)))

if (nrow(deep_value) > 0) {
  cat(sprintf("- %d stocks trading near 52-week lows (potential opportunities)\n", nrow(deep_value)))
}

cat("\nFiles exported to:", export_result$output_directory, "\n")


