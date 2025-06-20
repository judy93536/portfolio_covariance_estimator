# Portfolio Covariance Estimator

A comprehensive R framework for advanced portfolio covariance estimation and value stock screening using high-quality financial data. This package implements multiple covariance estimation methods and demonstrates their performance across different data sources and portfolio construction approaches.

## 🎯 Key Features

- **Advanced Covariance Estimation**: Six different methods including Ledoit-Wolf shrinkage, Random Matrix Theory, and identity-based approaches
- **Value Stock Screening**: Sophisticated fundamental analysis with dividend history and price metrics
- **Sector-Based Portfolio Construction**: Automated sector diversification with liquidity filtering
- **Portfolio Optimization**: Rcpp-powered differential evolution optimizer with Sharpe ratio maximization
- **Data Source Comparison**: Framework for comparing different financial data providers
- **Export Capabilities**: Complete CSV export functionality for reproducible research

## 📊 Research Findings

Our empirical analysis reveals several important insights:

### Covariance Estimator Performance
- **Identity shrinkage consistently outperforms** traditional methods on high-quality, sector-diversified data
- **Random Matrix Theory methods** can underperform on clean institutional data
- **Method selection depends critically on data quality characteristics** rather than theoretical considerations alone

### Data Source Analysis
- **Retail data providers converge** when proper corporate actions are applied
- **Raw exchange data provides minimal advantage** over professional retail sources for portfolio optimization
- **Corporate action handling is more critical** than data source choice

## 🛠 Installation

### Prerequisites
```r
# Required R packages
install.packages(c(
  "R6", "DBI", "RPostgres", "tidyr", "dplyr", 
  "xts", "quadprog", "Matrix", "Rcpp", "RcppArmadillo"
))
```

### Database Setup
This framework requires access to Sharadar financial data via PostgreSQL. Set up your database configuration:

```r
# Copy config/database_config_template.R to config/database_config.R
# Fill in your credentials (DO NOT commit this file)
DATABASE_CONFIG <- list(
  server = "your_server",
  database = "your_database", 
  username = "your_username",
  password = "your_password"
)
```

## 🚀 Quick Start

### Basic Value Stock Screening
```r
source("R/SharadarData.R")
source("config/database_config.R")

# Initialize data connection
sharadar <- SharadarData$new(
  DATABASE_CONFIG$server,
  DATABASE_CONFIG$database,
  DATABASE_CONFIG$username, 
  DATABASE_CONFIG$password
)

# Screen for S&P 500 value stocks
value_stocks <- sharadar$screenValueStocks(
  min_dividend_yield = 2.0,
  max_pe = 15.0,
  max_pb = 3.0,
  min_market_cap = 5000,
  sp500_only = TRUE
)

print(value_stocks)
```

### Covariance Estimation Analysis
```r
source("R/CovarianceEstimator.R")

# Get sector-diversified portfolio data
portfolio_data <- sharadar$getReturns(
  "2014-01-01", "2016-12-31",
  selection_method = "sector",
  sectors = c("Technology", "Financial Services", "Healthcare"),
  stocks_per_sector = 10
)

# Run covariance estimator comparison
estimator <- CovarianceEstimator$new(
  portfolio_data$returns,
  portfolio_data$dates, 
  portfolio_data$tickers
)

results <- estimator$runComparison()
estimator$displayResults()
```

### Complete Workflow
```r
# Run the complete analysis pipeline
source("scripts/examples/complete_workflow.R")
```

## 📚 Documentation

### Core Classes

#### SharadarData
High-level interface for financial data access and analysis.

**Key Methods:**
- `getReturns()` - Retrieve return data with liquidity or sector-based selection
- `screenValueStocks()` - Advanced fundamental screening with S&P 500 filtering
- `getDividendAnalysis()` - Historical dividend growth and consistency analysis
- `getPriceMetrics()` - Current price positioning and volume metrics
- `exportCompleteAnalysis()` - Comprehensive CSV export functionality

#### CovarianceEstimator  
Advanced covariance matrix estimation and portfolio risk analysis.

**Implemented Methods:**
- **Sample Covariance Matrix (SCM)**: Traditional unbiased estimator
- **Ledoit-Wolf Official**: Constant-correlation shrinkage using original methodology
- **Identity Shrinkage**: Shrinkage toward zero-correlation matrix
- **Diagonal Shrinkage**: Shrinkage toward equal-variance diagonal matrix  
- **RMT-Standard**: Marchenko-Pastur eigenvalue clipping with mean replacement
- **RMT-Conservative**: MP eigenvalue clipping with geometric mean replacement

**Key Methods:**
- `runComparison()` - Rolling window validation across all methods
- `displayResults()` - Comprehensive results summary
- `plotResults()` - Visualization of method performance
- `exportResults()` - Detailed CSV export for further analysis

## 🎯 Use Cases

### 1. Value Investing
```r
# Find undervalued S&P 500 stocks
value_opportunities <- sharadar$screenValueStocks(
  min_dividend_yield = 3.0,
  max_pe = 12.0,
  max_pb = 2.0,
  sp500_only = TRUE
)

# Analyze dividend consistency  
dividend_history <- sharadar$getDividendAnalysis(
  value_opportunities$ticker,
  years_back = 10
)
```

### 2. Portfolio Risk Management
```r
# Compare estimator performance on your portfolio
my_portfolio <- c("AAPL", "MSFT", "JPM", "JNJ", "XOM")
portfolio_data <- sharadar$getReturns(
  "2020-01-01", "2024-12-31",
  selection_method = "liquidity",
  n_stocks = length(my_portfolio)
)

risk_analysis <- CovarianceEstimator$new(
  portfolio_data$returns,
  portfolio_data$dates,
  portfolio_data$tickers
)
risk_results <- risk_analysis$runComparison()
```

### 3. Sector Analysis
```r
# Analyze sector-specific covariance patterns
tech_data <- sharadar$getReturns(
  "2014-01-01", "2016-12-31", 
  selection_method = "sector",
  sectors = "Technology",
  stocks_per_sector = 20
)

# Compare with diversified portfolio
diversified_data <- sharadar$getReturns(
  "2014-01-01", "2016-12-31",
  selection_method = "sector", 
  sectors = c("Technology", "Healthcare", "Financials"),
  stocks_per_sector = 7
)
```

## 📈 Example Results

### Typical Performance (S&P 500, 2014-2016)
```
=== FINAL RESULTS ===
Average annualized portfolio risks:
  SCM                 : 16.71% (±6.98%) [+0.000% vs SCM]
  Identity            : 16.21% (±6.73%) [+0.503% vs SCM] ← Best
  Ledoit-Wolf-Official: 16.69% (±7.02%) [+0.015% vs SCM]
  RMT-Standard        : 17.72% (±7.08%) [-1.009% vs SCM]
  
Best performer: Identity (16.21% risk)
```

### Value Stock Screening Results
- **Typical finding**: 60-80 S&P 500 stocks meeting value criteria
- **Top opportunities**: Often include utilities, telecom, financials
- **Yield range**: 2-8% with sustainable dividend histories
- **Valuation metrics**: P/E < 15, P/B < 3, strong balance sheets

## 🔬 Research Methodology

### Rolling Window Validation
- **Training period**: 200 days for covariance estimation
- **Test period**: 30-day out-of-sample validation
- **Rebalancing**: Every 30 days with portfolio risk measurement
- **Performance metric**: Annualized portfolio standard deviation

### Data Quality Assessment
- **Corporate action handling**: Critical for meaningful comparison
- **Eigenvalue spectrum analysis**: Detects data processing artifacts
- **Correlation structure**: Validates fundamental market relationships

## 📊 Data Sources

This framework is designed for use with:
- **Sharadar**: Professional-grade fundamental and price data
- **S&P 500 constituents**: Current and historical membership
- **PostgreSQL**: High-performance database backend

## 🤝 Contributing

We welcome contributions! Please see our contributing guidelines for:
- Code style and standards
- Testing requirements  
- Documentation expectations
- Research methodology validation

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## ⚠️ Disclaimer

This software is for educational and research purposes only. It is not intended as investment advice. All investment decisions should be made with proper due diligence and professional consultation.

## 🙏 Acknowledgments

- **Sharadar/Nasdaq**: High-quality financial data
- **Olivier Ledoit & Michael Wolf**: Shrinkage estimator methodology
- **Random Matrix Theory**: Eigenvalue cleaning approaches
- **IEX Exchange**: Raw market data for validation studies

## 📞 Support

For questions, issues, or contributions:
- Open an issue on GitHub
- Review the documentation in `docs/`
- Check example scripts in `scripts/examples/`

---

*Built with ❤️ for quantitative finance research and practical portfolio management.*


