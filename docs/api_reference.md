# API Reference

Complete documentation for the Portfolio Covariance Estimator framework.

## SharadarData Class

High-level interface for financial data access, value stock screening, and analysis.

### Constructor

```r
SharadarData$new(server, database, username, password)
```

**Parameters:**
- `server` (string): PostgreSQL server hostname or IP
- `database` (string): Database name containing Sharadar data
- `username` (string): Database username
- `password` (string): Database password

**Returns:** SharadarData object instance

---

### Data Retrieval Methods

#### `getReturns()`

Retrieve historical return data with flexible selection methods.

```r
getReturns(start_date, end_date, n_stocks = NULL, 
          selection_method = "liquidity", sectors = NULL, 
          stocks_per_sector = NULL)
```

**Parameters:**
- `start_date` (string): Start date in "YYYY-MM-DD" format
- `end_date` (string): End date in "YYYY-MM-DD" format  
- `n_stocks` (integer): Number of stocks for liquidity-based selection
- `selection_method` (string): "liquidity" or "sector"
- `sectors` (vector): Sector names for sector-based selection
- `stocks_per_sector` (integer): Number of stocks per sector

**Returns:** List containing:
- `returns`: Matrix of daily log-returns
- `dates`: Vector of corresponding dates
- `tickers`: Vector of stock symbols

**Example:**
```r
# Liquidity-based selection
data <- sharadar$getReturns("2020-01-01", "2022-12-31", n_stocks = 50)

# Sector-based selection  
data <- sharadar$getReturns(
  "2020-01-01", "2022-12-31",
  selection_method = "sector",
  sectors = c("Technology", "Healthcare"),
  stocks_per_sector = 10
)
```

#### `getMostLiquid()`

Select most liquid stocks by dollar volume.

```r
getMostLiquid(start_date, end_date, n_stocks)
```

**Returns:** Vector of ticker symbols

#### `getMostLiquidBySector()`

Select most liquid stocks within specified sectors.

```r
getMostLiquidBySector(start_date, end_date, sectors = NULL, stocks_per_sector = 10)
```

**Returns:** Vector of ticker symbols

---

### Value Investing Methods

#### `screenValueStocks()`

Advanced fundamental screening for value opportunities.

```r
screenValueStocks(sector = NULL, min_market_cap = 1000, max_market_cap = NULL,
                 min_dividend_yield = 2.0, max_pe = 15.0, max_pb = 3.0,
                 sp500_only = TRUE)
```

**Parameters:**
- `sector` (string): Filter to specific sector (optional)
- `min_market_cap` (numeric): Minimum market cap in millions
- `max_market_cap` (numeric): Maximum market cap in millions (optional)
- `min_dividend_yield` (numeric): Minimum dividend yield percentage
- `max_pe` (numeric): Maximum P/E ratio
- `max_pb` (numeric): Maximum P/B ratio
- `sp500_only` (boolean): Filter to S&P 500 constituents only

**Returns:** Data frame with columns:
- `ticker`: Stock symbol
- `sector`: Business sector
- `industry`: Industry classification
- `market_cap_millions`: Market capitalization
- `pe`: Price-to-earnings ratio
- `pb`: Price-to-book ratio
- `dividend_yield_pct`: Annual dividend yield percentage
- `data_date`: Date of fundamental data

**Example:**
```r
# Conservative value screening
value_stocks <- sharadar$screenValueStocks(
  min_dividend_yield = 3.0,
  max_pe = 12.0,
  max_pb = 2.5,
  min_market_cap = 5000,
  sp500_only = TRUE
)

# Sector-specific screening
utility_value <- sharadar$screenValueStocks(
  sector = "Utilities",
  min_dividend_yield = 4.0,
  max_pe = 15.0
)
```

#### `getDividendAnalysis()`

Analyze historical dividend payments and growth patterns.

```r
getDividendAnalysis(tickers, years_back = 5)
```

**Parameters:**
- `tickers` (vector): Stock symbols to analyze
- `years_back` (integer): Number of years of history

**Returns:** Data frame with columns:
- `ticker`: Stock symbol
- `div_year`: Year of dividend payment
- `annual_dividend`: Total annual dividend amount
- `payments_per_year`: Number of dividend payments
- `prev_year_dividend`: Previous year's dividend
- `no_cut_flag`: 1 if dividend maintained/increased, 0 if cut
- `yoy_growth_pct`: Year-over-year growth percentage

**Example:**
```r
dividend_history <- sharadar$getDividendAnalysis(
  c("KO", "JNJ", "PG"), 
  years_back = 10
)
```

#### `getPriceMetrics()`

Current price positioning and volume analysis.

```r
getPriceMetrics(tickers)
```

**Parameters:**
- `tickers` (vector): Stock symbols to analyze

**Returns:** Data frame with columns:
- `ticker`: Stock symbol
- `current_price`: Latest closing price
- `price_date`: Date of latest price
- `high_52w`: 52-week high price
- `low_52w`: 52-week low price
- `avg_52w`: 52-week average price
- `avg_daily_volume_millions`: Average daily dollar volume
- `pct_of_52w_range`: Current price as % of 52-week range
- `pct_off_high`: Percentage below 52-week high

---

### Utility Methods

#### `getAvailableSectors()`

Get list of available sectors in the database.

```r
getAvailableSectors()
```

**Returns:** Data frame with sector names and stock counts

#### `getSectorInfo()`

Get sector and industry information for specific tickers.

```r
getSectorInfo(tickers)
```

**Returns:** Data frame with ticker, sector, industry, and market cap scale

---

### Export Methods

#### `exportValueStocks()`

Export value stock screening results to CSV.

```r
exportValueStocks(value_stocks_df, filename = NULL)
```

#### `exportDividendAnalysis()`

Export dividend analysis results to CSV.

```r
exportDividendAnalysis(dividend_df, filename = NULL)
```

#### `exportCompleteAnalysis()`

Export comprehensive analysis with multiple datasets.

```r
exportCompleteAnalysis(value_stocks, dividend_analysis = NULL, 
                      price_metrics = NULL, output_dir = ".", 
                      project_name = NULL)
```

**Parameters:**
- `value_stocks`: Value screening results data frame
- `dividend_analysis`: Dividend analysis results (optional)
- `price_metrics`: Price metrics results (optional)
- `output_dir`: Output directory path
- `project_name`: Project name for directory creation

**Returns:** List with output directory and created file paths

---

## CovarianceEstimator Class

Advanced covariance matrix estimation and portfolio risk analysis.

### Constructor

```r
CovarianceEstimator$new(returns, dates, tickers)
```

**Parameters:**
- `returns`: Matrix of daily returns (days × assets)
- `dates`: Vector of corresponding dates
- `tickers`: Vector of asset symbols

**Returns:** CovarianceEstimator object instance

---

### Analysis Methods

#### `runComparison()`

Execute comprehensive covariance estimator comparison.

```r
runComparison()
```

**Process:**
1. Rolling window validation (200-day training, 30-day testing)
2. Portfolio optimization for each method
3. Out-of-sample risk measurement
4. Statistical analysis across all periods

**Returns:** Results object containing:
- `avg_risks`: Average portfolio risk by method
- `std_risks`: Standard deviation of risks
- `improvements`: Risk improvement vs. Sample Covariance Matrix
- `period_risks`: Time series of portfolio risks
- `shrinkage_intensities`: Shrinkage parameters over time
- `eigenvalue_stats`: Random Matrix Theory statistics

#### `setParameters()`

Configure analysis parameters.

```r
setParameters(training_days = 200, update_frequency = 30, 
             min_return_daily = 0.0004, methods = NULL)
```

**Parameters:**
- `training_days` (integer): Size of rolling training window
- `update_frequency` (integer): Days between portfolio rebalancing
- `min_return_daily` (numeric): Minimum daily return constraint
- `methods` (vector): List of methods to test

---

### Covariance Estimation Methods

The framework implements six distinct approaches:

#### 1. Sample Covariance Matrix (SCM)
Traditional unbiased maximum likelihood estimator.

#### 2. Ledoit-Wolf Official
Shrinkage toward constant-correlation target using original methodology.
- **Target**: Preserves individual variances, shrinks correlations toward average
- **Intensity**: Optimally estimated to minimize expected loss

#### 3. Identity Shrinkage
Shrinkage toward zero-correlation (identity) matrix.
- **Target**: Diagonal matrix with sample variances
- **Use case**: Effective when genuine correlations are low

#### 4. Diagonal Shrinkage  
Shrinkage toward equal-variance diagonal matrix.
- **Target**: Scalar multiple of identity matrix
- **Use case**: Assumes equal risk across assets

#### 5. RMT-Standard
Random Matrix Theory eigenvalue cleaning with arithmetic mean replacement.
- **Process**: Clips eigenvalues below Marchenko-Pastur bounds
- **Replacement**: Arithmetic mean of clipped eigenvalues

#### 6. RMT-Conservative
Random Matrix Theory eigenvalue cleaning with geometric mean replacement.
- **Process**: Clips eigenvalues below Marchenko-Pastur bounds  
- **Replacement**: Geometric mean of clipped eigenvalues

---

### Visualization Methods

#### `plotResults()`

Generate comprehensive visualization of results.

```r
plotResults()
```

**Creates:**
- Risk comparison bar chart
- Risk improvement vs. SCM
- Time series of portfolio risks
- Shrinkage intensity distributions
- Eigenvalue statistics over time
- Method ranking heatmap

#### `displayResults()`

Print formatted summary of analysis results.

```r
displayResults()
```

**Outputs:**
- Average portfolio risks by method
- Risk improvements vs. Sample Covariance Matrix
- Best performing method identification
- Shrinkage intensity statistics
- Random Matrix Theory analysis summary

---

### Export Methods

#### `exportResults()`

Export detailed analysis results to CSV files.

```r
exportResults(filename = NULL, output_dir = ".")
```

**Creates:**
- Summary results with method rankings
- Period-by-period risk measurements
- Shrinkage intensity time series
- Eigenvalue statistics
- Configuration parameters

#### `exportPortfolioBacktest()`

Export portfolio backtesting results.

```r
exportPortfolioBacktest(filename = NULL, output_dir = ".")
```

**Creates:**
- Portfolio performance comparison
- Risk-adjusted metrics
- Win rates vs. benchmark methods

---

## Configuration Options

### Database Configuration

```r
# config/database_config.R
DATABASE_CONFIG <- list(
  server = "hostname_or_ip",
  database = "database_name",
  username = "username", 
  password = "password",
  port = 5432
)
```

### Analysis Parameters

```r
# Default covariance estimator configuration
config <- list(
  training_days = 200,        # Rolling window size
  update_frequency = 30,      # Rebalancing frequency  
  min_return_daily = 0.0004,  # 10% annualized minimum return
  methods = c(                # Methods to compare
    'SCM', 'SCM-Raw', 'Ledoit-Wolf-Official', 
    'Identity', 'Diagonal', 'RMT-Standard', 'RMT-Conservative'
  )
)
```

---

## Error Handling

### Common Issues

#### Database Connection
```r
# Error: Connection failed
# Solution: Check database credentials and network connectivity
tryCatch({
  sharadar <- SharadarData$new(server, database, username, password)
}, error = function(e) {
  cat("Database connection failed:", e$message, "\n")
  cat("Check credentials and network connectivity\n")
})
```

#### Insufficient Data
```r
# Error: Too few tickers remain after cleaning
# Solution: Adjust date range or selection criteria
data <- sharadar$getReturns("2020-01-01", "2022-12-31", n_stocks = 100)
if (ncol(data$returns) < 20) {
  warning("Limited diversification with fewer than 20 stocks")
}
```

#### Portfolio Optimization Failures
```r
# Error: Portfolio optimization failed
# Solution: Check return constraints and covariance matrix conditioning
estimator$setParameters(min_return_daily = 0.0001)  # Lower constraint
```

---

## Performance Considerations

### Memory Usage
- Large return matrices (>1000 stocks) may require substantial RAM
- Consider processing in batches for very large universes
- Export intermediate results to prevent data loss

### Computation Time
- Rolling window analysis scales with number of periods
- Covariance estimation complexity: O(n²) for n assets
- Consider parallel processing for multiple portfolio comparisons

### Database Optimization
- Index ticker and date columns for faster queries
- Consider materialized views for frequently accessed aggregations
- Use connection pooling for multiple analysis runs

---

## Best Practices

### Data Quality
1. Always validate data completeness before analysis
2. Check for corporate action adjustments
3. Verify date ranges align with intended analysis period

### Method Selection  
1. Test multiple methods on your specific data
2. Consider portfolio correlation structure when interpreting results
3. Validate out-of-sample performance before implementation

### Result Interpretation
1. Focus on statistical significance of performance differences
2. Consider transaction costs in practical implementation
3. Validate results across different market regimes


