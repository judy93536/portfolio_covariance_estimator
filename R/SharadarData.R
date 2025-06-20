library(R6)
library(DBI)
library(RPostgres)
library(tidyr)
library(dplyr)

#' SharadarData Class
#' 
#' Handles data retrieval from Sharadar PostgreSQL database.
#' Focuses purely on data access without analysis concerns.
SharadarData <- R6::R6Class("SharadarData",
  private = list(
    connection = NULL,
    config = NULL,
    
    .validate_date_range = function(start_date, end_date) {
      start_date <- as.Date(start_date)
      end_date <- as.Date(end_date)
      
      if (start_date >= end_date) {
        stop("Start date must be before end date")
      }
      
      if (end_date > Sys.Date()) {
        warning("End date is in the future")
      }
      
      return(list(start = start_date, end = end_date))
    }
  ),

  public = list(
    #' Initialize database connection configuration
    #' @param server Database server hostname
    #' @param database Database name
    #' @param username Database username  
    #' @param password Database password
    initialize = function(server, database, username, password) {
      private$config <- list(
        server = server,
        database = database,
        username = username,
        password = password,
        port = 5432
      )
      message(sprintf("SharadarData configured for %s@%s:%s", username, server, database))
    },

    #' Get returns data for portfolio construction
    #' @param start_date Start date (YYYY-MM-DD)
    #' @param end_date End date (YYYY-MM-DD)
    #' @param n_stocks Number of stocks (for liquidity method)
    #' @param selection_method Either "liquidity" or "sector"
    #' @param sectors Vector of sector names (for sector method)
    #' @param stocks_per_sector Number of stocks per sector
    #' @return List with returns matrix, dates, and tickers
    getReturns = function(start_date, end_date, n_stocks = NULL, 
                         selection_method = "liquidity", 
                         sectors = NULL, 
                         stocks_per_sector = NULL) {
      
      # Validate inputs
      dates <- private$.validate_date_range(start_date, end_date)
      
      if (selection_method == "liquidity" && is.null(n_stocks)) {
        stop("n_stocks required for liquidity selection method")
      }
      
      if (selection_method == "sector" && (is.null(sectors) || is.null(stocks_per_sector))) {
        stop("sectors and stocks_per_sector required for sector selection method")
      }
      
      # Log what we're doing
      if (selection_method == "liquidity") {
        message(sprintf("Loading returns data: %s to %s (%d stocks)", 
                       dates$start, dates$end, n_stocks))
      } else {
        message(sprintf("Loading returns data: %s to %s (sector-based)", 
                       dates$start, dates$end))
      }
      
      tryCatch({
        self$connect()
        
        # Get tickers based on selection method
        if (selection_method == "liquidity") {
          tickers <- self$getMostLiquid(dates$start, dates$end, n_stocks)
        } else {
          tickers <- self$getMostLiquidBySector(dates$start, dates$end, sectors, stocks_per_sector)
        }
        
        message(sprintf("Found %d tickers", length(tickers)))

        # Get price data and convert to returns
        price_data <- self$queryPrices(tickers, dates$start, dates$end)
        message(sprintf("Retrieved %d price records", nrow(price_data)))

        matrix_data <- self$pivotToMatrix(price_data)
        prices <- matrix_data$prices
        dates_vec <- matrix_data$dates
        tickers_final <- matrix_data$tickers

        # Calculate log returns
        returns <- diff(log(prices))
        dates_vec <- dates_vec[-1]  # align with returns

        message(sprintf("Final dataset: %d days × %d stocks", nrow(returns), ncol(returns)))
        message(sprintf("Date range: %s to %s", dates_vec[1], tail(dates_vec, 1)))
        message(sprintf("Mean correlation: %.4f", mean(cor(returns), na.rm = TRUE)))

        list(returns = returns, dates = dates_vec, tickers = tickers_final)
        
      }, error = function(e) {
        message("Error loading data: ", e$message)
        stop(e)
      }, finally = {
        self$disconnect()
      })
    },

    #' Get most liquid stocks by dollar volume
    #' @param start_date Start date
    #' @param end_date End date  
    #' @param n_stocks Number of stocks to return
    #' @return Vector of ticker symbols
    getMostLiquid = function(start_date, end_date, n_stocks) {
      query <- sprintf("
        SELECT ticker FROM (
          SELECT ticker, AVG(volume * closeadj) as avg_liquidity, COUNT(*) as days
          FROM sp500.daily_prices
          WHERE date >= '%s' AND date <= '%s'
            AND closeadj IS NOT NULL AND closeadj > 0
            AND volume IS NOT NULL AND volume > 0
          GROUP BY ticker
          HAVING COUNT(*) >= 500
          ORDER BY avg_liquidity DESC
          LIMIT %d
        ) liquid_stocks", start_date, end_date, n_stocks)

      result <- DBI::dbGetQuery(private$connection, query)
      tickers <- result$ticker
      
      if (length(tickers) == 0) {
        stop("No liquid tickers found for the specified period")
      }
      
      message("Selected tickers: ", paste(head(tickers, 5), collapse = ", "))
      return(tickers)
    },

    #' Get most liquid stocks by sector
    #' @param start_date Start date
    #' @param end_date End date
    #' @param sectors Vector of sector names
    #' @param stocks_per_sector Number of stocks per sector
    #' @return Vector of ticker symbols
    getMostLiquidBySector = function(start_date, end_date, sectors, stocks_per_sector) {
      sectors_sql <- paste0("'", sectors, "'", collapse = ", ")
      
      query <- sprintf("
        WITH distinct_tickers AS (
          SELECT DISTINCT ticker, sector
          FROM sharadar_src.tickers
          WHERE sector IN (%s)
        ),
        sector_liquidity AS (
          SELECT 
            t.ticker,
            t.sector,
            AVG(p.volume * p.closeadj) as avg_liquidity,
            COUNT(*) as days,
            ROW_NUMBER() OVER (PARTITION BY t.sector ORDER BY AVG(p.volume * p.closeadj) DESC) as sector_rank
          FROM distinct_tickers t
          INNER JOIN sp500.daily_prices p ON t.ticker = p.ticker
          WHERE p.date >= '%s' AND p.date <= '%s'
            AND p.closeadj IS NOT NULL AND p.closeadj > 0
            AND p.volume IS NOT NULL AND p.volume > 0
          GROUP BY t.ticker, t.sector
          HAVING COUNT(*) >= 500
        )
        SELECT ticker, sector, avg_liquidity
        FROM sector_liquidity
        WHERE sector_rank <= %d
        ORDER BY sector, avg_liquidity DESC", 
        sectors_sql, start_date, end_date, stocks_per_sector)

      result <- DBI::dbGetQuery(private$connection, query)
      
      if (nrow(result) == 0) {
        stop("No liquid tickers found for the specified sectors and period")
      }
      
      # Display sector breakdown
      sector_counts <- table(result$sector)
      message("Sector breakdown:")
      for (sector in names(sector_counts)) {
        sector_tickers <- result$ticker[result$sector == sector]
        message(sprintf("  %s: %d stocks (%s)", 
                       sector, sector_counts[sector], 
                       paste(head(sector_tickers, 3), collapse = ", ")))
      }
      
      return(result$ticker)
    },

    #' Get benchmark data (SPY or other ETF)
    #' @param ticker Benchmark ticker (default: SPY)
    #' @param start_date Start date
    #' @param end_date End date
    #' @return List with returns vector and dates
    getBenchmarkReturns = function(ticker = "SPY", start_date, end_date) {
      dates <- private$.validate_date_range(start_date, end_date)
      
      message(sprintf("Loading benchmark data: %s (%s to %s)", ticker, dates$start, dates$end))
      
      tryCatch({
        self$connect()
        
        # Try different data sources for benchmark data
        price_data <- self$queryBenchmarkPrices(ticker, dates$start, dates$end)
        
        if (nrow(price_data) == 0) {
          stop(sprintf("No price data found for benchmark ticker: %s", ticker))
        }
        
        # Convert to time series
        prices <- price_data$closeadj
        dates_vec <- price_data$date
        
        # Calculate returns
        returns <- diff(log(prices))
        dates_vec <- dates_vec[-1]
        
        message(sprintf("Benchmark data: %d observations", length(returns)))
        
        list(returns = returns, dates = dates_vec, ticker = ticker)
        
      }, error = function(e) {
        message("Error loading benchmark data: ", e$message)
        stop(e)
      }, finally = {
        self$disconnect()
      })
    },

    #' Connect to database
    connect = function() {
      if (!is.null(private$connection) && DBI::dbIsValid(private$connection)) {
        return()
      }

      private$connection <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = private$config$server,
        port = private$config$port,
        dbname = private$config$database,
        user = private$config$username,
        password = private$config$password
      )
      message("Connected to PostgreSQL database")
    },

    #' Disconnect from database
    disconnect = function() {
      if (!is.null(private$connection) && DBI::dbIsValid(private$connection)) {
        DBI::dbDisconnect(private$connection)
        private$connection <- NULL
        message("Disconnected from database")
      }
    },

    #' Query price data for given tickers
    #' @param tickers Vector of ticker symbols
    #' @param start_date Start date
    #' @param end_date End date
    #' @return Data frame with date, ticker, closeadj columns
    queryPrices = function(tickers, start_date, end_date) {
      quoted <- paste0("'", tickers, "'", collapse = ", ")
      query <- sprintf("
        SELECT date, ticker, closeadj
        FROM sp500.daily_prices
        WHERE ticker IN (%s)
          AND date >= '%s' AND date <= '%s'
          AND closeadj IS NOT NULL AND closeadj > 0
        ORDER BY date, ticker", quoted, start_date, end_date)

      data <- DBI::dbGetQuery(private$connection, query)
      data$date <- as.Date(data$date)
      
      if (nrow(data) == 0) {
        stop("No price data found for the specified criteria")
      }
      
      return(data)
    },

    #' Query benchmark price data (handles different table structures)
    #' @param ticker Single ticker symbol
    #' @param start_date Start date
    #' @param end_date End date
    #' @return Data frame with date, ticker, closeadj columns
    queryBenchmarkPrices = function(ticker, start_date, end_date) {
      # First try the standard sp500.daily_prices table
      standard_query <- sprintf("
        SELECT date, ticker, closeadj
        FROM sp500.daily_prices
        WHERE ticker = '%s'
          AND date >= '%s' AND date <= '%s'
          AND closeadj IS NOT NULL AND closeadj > 0
        ORDER BY date", ticker, start_date, end_date)
      
      data <- DBI::dbGetQuery(private$connection, standard_query)
      
      if (nrow(data) > 0) {
        data$date <- as.Date(data$date)
        message(sprintf("Found %d records in sp500.daily_prices", nrow(data)))
        return(data)
      }
      
      # If not found, try the sharadar_src.sfp table (correct column names)
      message("Trying sharadar_src.sfp table...")
      sharadar_query <- sprintf("
        SELECT date, ticker, closeadj
        FROM sharadar_src.sfp
        WHERE ticker = '%s'
          AND date >= '%s' AND date <= '%s'
          AND closeadj IS NOT NULL AND closeadj > 0
        ORDER BY date", ticker, start_date, end_date)
      
      data <- DBI::dbGetQuery(private$connection, sharadar_query)
      
      if (nrow(data) > 0) {
        data$date <- as.Date(data$date)
        message(sprintf("Found %d records in sharadar_src.sfp", nrow(data)))
        return(data)
      }
      
      # Fallback: try using 'close' column if 'closeadj' has issues
      message("Trying sharadar_src.sfp with 'close' column...")
      fallback_query <- sprintf("
        SELECT date, ticker, close as closeadj
        FROM sharadar_src.sfp
        WHERE ticker = '%s'
          AND date >= '%s' AND date <= '%s'
          AND close IS NOT NULL AND close > 0
        ORDER BY date", ticker, start_date, end_date)
      
      data <- DBI::dbGetQuery(private$connection, fallback_query)
      
      if (nrow(data) > 0) {
        data$date <- as.Date(data$date)
        message(sprintf("Found %d records in sharadar_src.sfp (using close)", nrow(data)))
        return(data)
      }
      
      # If nothing found, return empty data frame with correct structure
      message(sprintf("No data found for %s in any table", ticker))
      return(data.frame(date = as.Date(character(0)), 
                       ticker = character(0), 
                       closeadj = numeric(0)))
    },

    #' Convert long-form price data to matrix format
    #' @param data Data frame with date, ticker, closeadj columns
    #' @return List with prices matrix, dates vector, and tickers vector
    pivotToMatrix = function(data) {
      wide_data <- tidyr::pivot_wider(data, names_from = ticker, values_from = closeadj)
      dates <- wide_data$date
      prices <- as.matrix(wide_data[, -1])
      tickers <- colnames(prices)

      # Remove columns with any missing data
      complete_cols <- colSums(is.na(prices)) == 0
      if (any(!complete_cols)) {
        removed_count <- sum(!complete_cols)
        message(sprintf("Removing %d tickers with missing data", removed_count))
        prices <- prices[, complete_cols, drop = FALSE]
        tickers <- tickers[complete_cols]
      }

      if (ncol(prices) < 10) {
        stop("Too few tickers remain after cleaning (need at least 10)")
      }

      # Check for remaining missing values
      missing_pct <- sum(is.na(prices)) / length(prices) * 100
      if (missing_pct > 1) {
        warning(sprintf("%.2f%% of price data is missing", missing_pct))
      }

      return(list(prices = prices, dates = dates, tickers = tickers))
    },

    #' Screen for value stocks based on fundamental criteria
    #' @param sector Optional sector filter
    #' @param min_market_cap Minimum market cap in millions (default: 1000)
    #' @param max_market_cap Maximum market cap in millions (optional)
    #' @param min_dividend_yield Minimum dividend yield percentage (default: 2.0)
    #' @param max_pe Maximum P/E ratio (default: 15.0)
    #' @param max_pb Maximum P/B ratio (default: 3.0)
    #' @param sp500_only Filter to S&P 500 constituents only (default: TRUE)
    #' @return Data frame with screened stocks and their metrics
    screenValueStocks = function(sector = NULL, min_market_cap = 1000, max_market_cap = NULL,
                                min_dividend_yield = 2.0, max_pe = 15.0, max_pb = 3.0,
                                sp500_only = TRUE) {
      message("Screening for value stocks...")
      message(sprintf("Criteria: Dividend yield >= %.1f%%, P/E <= %.1f, P/B <= %.1f", 
                     min_dividend_yield, max_pe, max_pb))
      if (sp500_only) message("Filtering to S&P 500 constituents only")
      
      tryCatch({
        self$connect()
        
        # Build filter conditions
        sector_filter <- ""
        if (!is.null(sector)) {
          sector_filter <- sprintf("AND t.sector = '%s'", sector)
        }
        
        market_cap_filter <- sprintf("AND f.marketcap / 1000000 >= %d", min_market_cap)
        if (!is.null(max_market_cap)) {
          market_cap_filter <- paste(market_cap_filter, 
                                   sprintf("AND f.marketcap / 1000000 <= %d", max_market_cap))
        }
        
        # Convert dividend yield percentage to decimal for comparison
        min_yield_decimal <- min_dividend_yield / 100
        
        # Query to get latest fundamentals with value metrics
        query <- sprintf("
          WITH latest_fundamentals AS (
            SELECT 
              f.ticker,
              t.sector,
              t.industry,
              f.marketcap / 1000000 as market_cap_millions,
              f.pe,
              f.pb,
              CASE 
                WHEN f.divyield ~ '^[0-9]+\\.?[0-9]*$' THEN f.divyield::numeric
                ELSE NULL 
              END as divyield,
              f.datekey,
              ROW_NUMBER() OVER (PARTITION BY f.ticker ORDER BY f.datekey DESC) as rn
            FROM sharadar_src.sf1 f
            INNER JOIN sharadar_src.tickers t ON f.ticker = t.ticker
            WHERE f.dimension = 'MRY'  -- Most Recent Year
              AND f.datekey >= CURRENT_DATE - INTERVAL '1 year'
              AND f.pe IS NOT NULL AND f.pe > 0 AND f.pe <= %f
              AND f.pb IS NOT NULL AND f.pb > 0 AND f.pb <= %f
              %s  -- sector filter
              %s  -- market cap filter
          )
          SELECT 
            ticker,
            sector,
            industry,
            market_cap_millions,
            pe,
            pb,
            divyield * 100 as dividend_yield_pct,
            datekey as data_date
          FROM latest_fundamentals
          WHERE rn = 1
            AND divyield IS NOT NULL 
            AND divyield >= %f
          ORDER BY divyield DESC, pe ASC
          LIMIT 100",
          max_pe, max_pb, sector_filter, market_cap_filter, min_yield_decimal)
        
        result <- DBI::dbGetQuery(private$connection, query)
        
        if (nrow(result) == 0) {
          warning("No stocks found meeting the value criteria. Try relaxing the filters.")
          return(data.frame())
        }
        
        message(sprintf("Found %d stocks meeting value criteria", nrow(result)))
        
        # If S&P 500 only, filter to current constituents
        if (sp500_only && nrow(result) > 0) {
          sp500_query <- "SELECT DISTINCT ticker FROM sp500.constituents WHERE is_current = TRUE"
          sp500_tickers <- DBI::dbGetQuery(private$connection, sp500_query)$ticker
          
          result <- result[result$ticker %in% sp500_tickers, ]
          message(sprintf("After S&P 500 filter: %d stocks remain", nrow(result)))
        }
        
        return(result)
        
      }, error = function(e) {
        message("Error in value screening: ", e$message)
        stop(e)
      }, finally = {
        self$disconnect()
      })
    },
    
    #' Explore table structure for debugging
    #' @param table_name Name of table to explore
    #' @param ticker Optional ticker to check data availability
    #' @return Information about table structure and data
    exploreTable = function(table_name = "sharadar_src.sfp", ticker = "SPY") {
      tryCatch({
        self$connect()
        
        # Get column information
        if (grepl("\\.", table_name)) {
          parts <- strsplit(table_name, "\\.")[[1]]
          schema <- parts[1]
          table <- parts[2]
          
          col_query <- sprintf("
            SELECT column_name, data_type 
            FROM information_schema.columns 
            WHERE table_schema = '%s' AND table_name = '%s'
            ORDER BY ordinal_position", schema, table)
        } else {
          col_query <- sprintf("
            SELECT column_name, data_type 
            FROM information_schema.columns 
            WHERE table_name = '%s'
            ORDER BY ordinal_position", table_name)
        }
        
        columns <- DBI::dbGetQuery(private$connection, col_query)
        cat("Table structure for", table_name, ":\n")
        print(columns)
        
        # Sample some data if ticker provided
        if (!is.null(ticker)) {
          sample_query <- sprintf("
            SELECT * FROM %s 
            WHERE ticker = '%s' 
            ORDER BY date DESC 
            LIMIT 5", table_name, ticker)
          
          sample_data <- DBI::dbGetQuery(private$connection, sample_query)
          
          if (nrow(sample_data) > 0) {
            cat("\nSample data for", ticker, ":\n")
            print(sample_data)
            
            # Check date range
            date_query <- sprintf("
              SELECT MIN(date) as min_date, MAX(date) as max_date, COUNT(*) as record_count
              FROM %s 
              WHERE ticker = '%s'", table_name, ticker)
            
            date_info <- DBI::dbGetQuery(private$connection, date_query)
            cat("\nDate range for", ticker, ":\n")
            print(date_info)
          } else {
            cat("\nNo data found for ticker:", ticker, "\n")
          }
        }
        
      }, error = function(e) {
        message("Error exploring table: ", e$message)
      }, finally = {
        self$disconnect()
      })
    }
  )
)
