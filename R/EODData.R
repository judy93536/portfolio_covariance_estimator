library(R6)
library(DBI)
library(RPostgres)
library(tidyr)
library(dplyr)
library(xts)


EODData <- R6::R6Class("EODData",
  private = list(
    connection = NULL,
    config = NULL
  ),

  public = list(
    initialize = function(server, database, username, password) {
      private$config <- list(
        server = server,
        database = database,
        username = username,
        password = password,
        port = 5432
      )
      message(sprintf("EODData configured for %s@%s:%s", username, server, database))
    },

    getReturns = function(start_date, end_date, n_stocks) {
      message(sprintf("Loading EOD returns data: %s to %s (%d stocks)", start_date, end_date, n_stocks))
      tryCatch({
        self$connect()
        liquid_tickers <- self$getMostLiquid(start_date, end_date, n_stocks)
        message(sprintf("Found %d liquid tickers", length(liquid_tickers)))

        price_data <- self$queryPrices(liquid_tickers, start_date, end_date)
        message(sprintf("Retrieved %d price records", nrow(price_data)))

        matrix_data <- self$pivotToMatrix(price_data)
        prices <- matrix_data$prices
        dates <- matrix_data$dates
        tickers <- matrix_data$tickers

        returns <- diff(log(prices))
        dates <- dates[-1]

        message(sprintf("Final dataset: %d days × %d stocks", nrow(returns), ncol(returns)))
        message(sprintf("Date range: %s to %s", dates[1], tail(dates, 1)))
        message(sprintf("Mean correlation: %.4f", mean(cor(returns), na.rm = TRUE)))
        message(sprintf("Return statistics:\n  Mean daily return: %.6f (%.2f%% annualized)",
                        mean(returns, na.rm = TRUE),
                        mean(returns, na.rm = TRUE) * 252 * 100))
        message(sprintf("  Daily volatility: %.6f (%.2f%% annualized)",
                        sd(returns, na.rm = TRUE),
                        sd(returns, na.rm = TRUE) * sqrt(252) * 100))

        list(returns = returns, dates = dates, tickers = tickers)
      }, error = function(e) {
        message("Error loading EOD data: ", e$message)
        stop(e)
      }, finally = {
        self$disconnect()
      })
    },

    getMostLiquid = function(start_date, end_date, n_stocks) {
      query <- sprintf("
        SELECT code FROM (
          SELECT code, AVG(volume * adjusted_close) as avg_liquidity, COUNT(*) as days
          FROM sp500.sp500_eod_data
          WHERE date >= '%s' AND date <= '%s'
            AND adjusted_close IS NOT NULL AND adjusted_close > 0
            AND volume IS NOT NULL AND volume > 0
            AND code IS NOT NULL
          GROUP BY code
          HAVING COUNT(*) >= 400
          ORDER BY avg_liquidity DESC
          LIMIT %d
        ) liquid_stocks", start_date, end_date, n_stocks)

      result <- DBI::dbGetQuery(private$connection, query)
      tickers <- result$code
      if (length(tickers) == 0) stop("No liquid tickers found for the specified period")
      message("Selected tickers: ", paste(head(tickers, 5), collapse = ", "))
      return(tickers)
    },

    getDatasetInfo = function(start_date, end_date) {
      self$connect()
      info <- tryCatch({
        query <- sprintf("
          SELECT COUNT(*) as total_records,
                 COUNT(DISTINCT code) as unique_tickers,
                 COUNT(DISTINCT date) as unique_dates
          FROM sp500.sp500_eod_data
          WHERE date >= '%s' AND date <= '%s'", start_date, end_date)

        result <- DBI::dbGetQuery(private$connection, query)

        sample_query <- "
          SELECT DISTINCT code FROM sp500.sp500_eod_data
          WHERE code IS NOT NULL LIMIT 10"
        sample <- DBI::dbGetQuery(private$connection, sample_query)

        message(sprintf("EOD Dataset Info (%s to %s):", start_date, end_date))
        message(sprintf("  Total records: %d", result$total_records))
        message(sprintf("  Unique tickers: %d", result$unique_tickers))
        message(sprintf("  Unique dates: %d", result$unique_dates))
        message("  Sample tickers: ", paste(sample$code, collapse = ", "))

        return(result)
      }, error = function(e) {
        message("Error getting dataset info: ", e$message)
        return(NULL)
      }, finally = {
        self$disconnect()
      })
    },

    connect = function() {
      if (!is.null(private$connection) && DBI::dbIsValid(private$connection)) return()

      private$connection <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = private$config$server,
        port = private$config$port,
        dbname = private$config$database,
        user = private$config$username,
        password = private$config$password
      )
      message("Connected to EOD PostgreSQL database")
    },

    disconnect = function() {
      if (!is.null(private$connection) && DBI::dbIsValid(private$connection)) {
        DBI::dbDisconnect(private$connection)
        private$connection <- NULL
        message("Disconnected from EOD database")
      }
    },

    queryPrices = function(tickers, start_date, end_date) {
      quoted <- paste0("'", tickers, "'", collapse = ", ")
      query <- sprintf("
        SELECT date, code, adjusted_close
        FROM sp500.sp500_eod_data
        WHERE code IN (%s)
          AND date >= '%s' AND date <= '%s'
          AND adjusted_close IS NOT NULL AND adjusted_close > 0
        ORDER BY date, code", quoted, start_date, end_date)

      data <- DBI::dbGetQuery(private$connection, query)
      data$date <- as.Date(data$date)
      if (nrow(data) == 0) stop("No price data found for the specified criteria")
      return(data)
    },

    pivotToMatrix = function(data) {
      wide_data <- tidyr::pivot_wider(data, names_from = code, values_from = adjusted_close)
      dates <- wide_data$date
      prices <- as.matrix(wide_data[ , -1])  # exclude date column
      tickers <- colnames(prices)

      complete_cols <- colSums(is.na(prices)) == 0
      if (any(!complete_cols)) {
        message(sprintf("Removing %d tickers with missing data", sum(!complete_cols)))
        prices <- prices[, complete_cols, drop = FALSE]
        tickers <- tickers[complete_cols]
      }

      if (ncol(prices) < 10) stop("Too few tickers remain after cleaning")

      missing_pct <- sum(is.na(prices)) / length(prices) * 100
      if (missing_pct > 1) warning(sprintf("%.2f%% of price data is missing", missing_pct))

      if (nrow(prices) > 1) {
        daily_changes <- abs(diff(log(prices)))
        extreme_moves <- daily_changes > 0.2
        extreme_pct <- sum(extreme_moves, na.rm = TRUE) / length(daily_changes) * 100
        if (extreme_pct > 1) {
          message(sprintf("Warning: %.2f%% extreme price movements detected", extreme_pct))
        }
      }

      return(list(prices = prices, dates = dates, tickers = tickers))
    }
  )
)
