library(R6)
library(quadprog)
library(Matrix)

#' CovarianceEstimator Class
#' 
#' Implements multiple covariance estimation methods and compares their
#' performance in portfolio optimization context.
CovarianceEstimator <- R6::R6Class("CovarianceEstimator",
  private = list(
    method_map = list(
      "SCM" = "SCM",
      "SCM-Raw" = "SCMRaw", 
      "Ledoit-Wolf" = "LedoitWolf",
      "Identity" = "Identity",
      "Diagonal" = "Diagonal",
      "RMT-Standard" = "RMTStandard",
      "RMT-Conservative" = "RMTConservative",
      "Gerber-MAD" = "GerberMAD"
    ),
    
    .validate_returns = function(returns) {
      if (!is.matrix(returns)) {
        stop("Returns must be a matrix")
      }
      
      if (any(is.na(returns))) {
        stop("Returns matrix cannot contain missing values")
      }
      
      if (nrow(returns) < 50) {
        warning("Very short time series (< 50 observations)")
      }
      
      if (ncol(returns) < 5) {
        warning("Very few assets (< 5)")
      }
    }
  ),
  
  public = list(
    # Data
    returns = NULL,
    dates = NULL,
    tickers = NULL,
    
    # Configuration and Results
    config = NULL,
    results = NULL,
    
    #' Initialize the covariance estimator
    #' @param returns Matrix of returns (rows = time, cols = assets)
    #' @param dates Vector of dates corresponding to return rows
    #' @param tickers Vector of ticker symbols corresponding to return columns
    initialize = function(returns, dates, tickers) {
      private$.validate_returns(returns)
      
      self$returns <- returns
      self$dates <- dates  
      self$tickers <- tickers
      
      # Default configuration
      self$config <- list(
        training_days = 200,
        update_frequency = 30,
        min_return_daily = 0.0004,
        methods = c('SCM', 'Ledoit-Wolf', 'Identity', 'Diagonal', 
                   'RMT-Standard', 'RMT-Conservative', 'Gerber-MAD'),
        gerber_threshold = 0.4,
        gerber_mad_multiplier = 1.4826
      )
      
      self$results <- list()
      
      N <- nrow(returns)
      M <- ncol(returns)
      cat(sprintf("CovarianceEstimator initialized: %d days × %d assets\n", N, M))
      cat(sprintf("Period: %s to %s\n", as.character(dates[1]), as.character(tail(dates, 1))))
      cat(sprintf("Mean correlation: %.4f\n", mean(cor(returns), na.rm = TRUE)))
      cat("Methods available:", paste(self$config$methods, collapse = ", "), "\n")
    },
    
    #' Update configuration parameters
    #' @param ... Named parameters to update
    setParameters = function(...) {
      params <- list(...)
      for (param in names(params)) {
        if (param %in% names(self$config)) {
          self$config[[param]] <- params[[param]]
          cat(sprintf("Set %s = %s\n", param, toString(params[[param]])))
        } else {
          warning(sprintf("Unknown parameter: %s", param))
        }
      }
    },
    
    #' Run rolling window comparison of covariance estimation methods
    #' @return List containing comparison results
    runComparison = function() {
      cat("\n=== RUNNING COVARIANCE ESTIMATION COMPARISON ===\n")
      cat(sprintf("Training window: %d days\n", self$config$training_days))
      cat(sprintf("Update frequency: %d days\n", self$config$update_frequency))
      cat(sprintf("Methods: %s\n", paste(self$config$methods, collapse = ", ")))
      
      # Setup rolling windows
      N <- nrow(self$returns)
      min_required <- self$config$training_days + self$config$update_frequency
      
      if (N < min_required) {
        stop(sprintf("Need at least %d observations, have %d", min_required, N))
      }
      
      window_starts <- seq(1, N - min_required + 1, by = self$config$update_frequency)
      n_periods <- length(window_starts)
      n_methods <- length(self$config$methods)
      
      cat(sprintf("Number of rebalancing periods: %d\n", n_periods))
      
      # Storage for results
      period_risks <- matrix(NA, nrow = n_periods, ncol = n_methods)
      colnames(period_risks) <- self$config$methods
      
      shrinkage_intensities <- matrix(NA, nrow = n_periods, ncol = 3)
      colnames(shrinkage_intensities) <- c("Ledoit-Wolf", "Identity", "Diagonal")
      
      eigenvalue_stats <- matrix(NA, nrow = n_periods, ncol = 3)
      colnames(eigenvalue_stats) <- c("Above_MP_Bounds", "Condition_Before", "Condition_After")
      
      gerber_stats <- matrix(NA, nrow = n_periods, ncol = 3)
      colnames(gerber_stats) <- c("Threshold_Used", "Sparsity_Ratio", "Condition_Number")
      
      # Rolling window analysis
      cat("\nRunning rolling window analysis...\n")
      for (i in 1:n_periods) {
        if (i %% 5 == 1 || i == n_periods) {
          cat(sprintf("Period %d/%d\n", i, n_periods))
        }
        
        result <- tryCatch({
          self$analyzePeriod(window_starts[i])
        }, error = function(e) {
          cat(sprintf("Error in period %d: %s\n", i, e$message))
          list(
            risks = rep(NA, n_methods), 
            shrink_rhos = rep(NA, 3), 
            eig_stats = rep(NA, 3),
            gerber_stats = rep(NA, 3)
          )
        })
        
        period_risks[i, ] <- result$risks
        shrinkage_intensities[i, ] <- result$shrink_rhos
        eigenvalue_stats[i, ] <- result$eig_stats
        gerber_stats[i, ] <- result$gerber_stats
      }
      
      # Summarize results
      self$results <- self$summarizeResults(period_risks, shrinkage_intensities, 
                                           eigenvalue_stats, gerber_stats)
      
      # Display summary
      self$displayResults()
      
      return(self$results)
    },
    
    #' Analyze a single period (estimate covariances and test portfolios)
    #' @param start_idx Starting index for training window
    #' @return List with portfolio risks and statistics
    analyzePeriod = function(start_idx) {
      end_idx <- start_idx + self$config$training_days - 1
      test_start <- end_idx + 1
      test_end <- min(test_start + self$config$update_frequency - 1, nrow(self$returns))
      
      # Extract data
      train_returns <- self$returns[start_idx:end_idx, , drop = FALSE]
      test_returns <- self$returns[test_start:test_end, , drop = FALSE]
      mu <- colMeans(train_returns)
      
      # Estimate all covariance matrices
      covmats <- self$estimateAllCovariances(train_returns)
      
      # Test portfolio performance
      risks <- numeric(length(self$config$methods))
      names(risks) <- self$config$methods
      
      for (i in seq_along(self$config$methods)) {
        method_name <- self$config$methods[i]
        field_name <- private$method_map[[method_name]]
        
        if (is.null(covmats[[field_name]])) {
          risks[i] <- NA
          next
        }
        
        portfolio <- self$solvePortfolio(covmats[[field_name]], mu)
        port_returns <- test_returns %*% portfolio
        risks[i] <- sd(port_returns) * sqrt(252) * 100  # Annualized percentage
      }
      
      return(list(
        risks = risks,
        shrink_rhos = covmats$shrinkage_intensities,
        eig_stats = covmats$eigenvalue_stats,
        gerber_stats = covmats$gerber_statistics
      ))
    },
    
    #' Estimate covariance matrices using all configured methods
    #' @param train_returns Training period returns matrix
    #' @return List of covariance matrices and statistics
    estimateAllCovariances = function(train_returns) {
      n <- nrow(train_returns)
      p <- ncol(train_returns)
      
      covmats <- list()
      
      # 1. Sample Covariance Matrix
      covmats$SCM <- cov(train_returns)
      
      # 2. Ledoit-Wolf shrinkage (constant correlation target)
      lw_result <- self$covCor(train_returns)
      covmats$LedoitWolf <- lw_result$sigmahat
      rho_lw <- lw_result$shrinkage
      
      # 3-4. Other shrinkage estimators
      S <- covmats$SCM
      sample_vars <- diag(S)
      
      # Identity target (keep sample variances on diagonal)
      F_id <- diag(sample_vars)
      rho_id <- self$estimateShrinkageIntensity(S, F_id, p/n)
      covmats$Identity <- rho_id * F_id + (1 - rho_id) * S
      
      # Diagonal target (equal variances)
      F_diag <- diag(mean(sample_vars), p)
      rho_diag <- self$estimateShrinkageIntensity(S, F_diag, p/n)
      covmats$Diagonal <- rho_diag * F_diag + (1 - rho_diag) * S
      
      # Store shrinkage intensities
      covmats$shrinkage_intensities <- c(rho_lw, rho_id, rho_diag)
      
      # 5-6. RMT Methods
      rmt_std <- self$applyRMTCleaning(S, p/n, 'standard')
      rmt_cons <- self$applyRMTCleaning(S, p/n, 'conservative')
      
      covmats$RMTStandard <- rmt_std$cleaned
      covmats$RMTConservative <- rmt_cons$cleaned
      
      covmats$eigenvalue_stats <- c(
        rmt_std$num_above_bounds, 
        rmt_std$condition_before, 
        rmt_std$condition_after
      )
      
      # 7. Gerber MAD Covariance
      if ("Gerber-MAD" %in% self$config$methods) {
        gerber_result <- self$estimateGerberMAD(train_returns)
        covmats$GerberMAD <- gerber_result$covariance
        covmats$gerber_statistics <- gerber_result$statistics
      } else {
        covmats$gerber_statistics <- c(NA, NA, NA)
      }
      
      return(covmats)
    },
    
    #' Ledoit-Wolf shrinkage estimator (official implementation)
    #' @param Y Returns matrix (centered)
    #' @return List with shrinkage estimator and shrinkage intensity
    covCor = function(Y) {
      N <- nrow(Y)
      p <- ncol(Y)
      
      # Demean the data
      Y <- Y - matrix(colMeans(Y), N, p, byrow = TRUE)
      n <- N - 1
      
      # Sample covariance matrix
      sample <- (t(Y) %*% Y) / n
      
      # Shrinkage target (constant correlation)
      samplevar <- diag(sample)
      sqrtvar <- sqrt(samplevar)
      rBar <- (sum(sample / (sqrtvar %*% t(sqrtvar))) - p) / (p * (p - 1))
      target <- rBar * sqrtvar %*% t(sqrtvar)
      diag(target) <- samplevar
      
      # Estimate optimal shrinkage intensity
      Y2 <- Y^2
      sample2 <- (t(Y2) %*% Y2) / n
      piMat <- sample2 - sample^2
      pihat <- sum(piMat)
      
      gammahat <- norm(sample - target, "F")^2
      
      rho_diag <- sum(diag(piMat))
      
      term1 <- (t(Y^3) %*% Y) / n
      term2 <- matrix(samplevar, p, p, byrow = FALSE) * sample
      thetaMat <- term1 - term2
      diag(thetaMat) <- 0
      rho_off <- rBar * sum(((1/sqrtvar) %*% t(sqrtvar)) * thetaMat)
      
      rhohat <- rho_diag + rho_off
      kappahat <- (pihat - rhohat) / gammahat
      shrinkage <- max(0, min(1, kappahat / n))
      
      sigmahat <- shrinkage * target + (1 - shrinkage) * sample
      
      return(list(sigmahat = sigmahat, shrinkage = shrinkage))
    },
    
    #' Estimate shrinkage intensity for simple targets
    #' @param S Sample covariance matrix
    #' @param F Target matrix
    #' @param c Dimensionality ratio (p/n)
    #' @return Optimal shrinkage intensity
    estimateShrinkageIntensity = function(S, F, c) {
      numerator <- norm(S - F, "F")^2
      denominator <- norm(S, "F")^2
      
      base_shrinkage <- min(0.8, c / (1 + c))
      
      if (denominator > 0) {
        distance_factor <- numerator / denominator
        adjustment <- -0.3 * min(1, distance_factor)
      } else {
        adjustment <- 0
      }
      
      rho <- max(0.01, min(0.95, base_shrinkage + adjustment))
      return(rho)
    },
    
    #' Apply Random Matrix Theory eigenvalue cleaning
    #' @param S Sample covariance matrix
    #' @param c Dimensionality ratio (p/n)
    #' @param mode Either 'standard' or 'conservative'
    #' @return List with cleaned matrix and statistics
    applyRMTCleaning = function(S, c, mode) {
      eigen_decomp <- eigen(S)
      eigenvals <- eigen_decomp$values
      V <- eigen_decomp$vectors
      
      # Sort in descending order
      idx <- order(eigenvals, decreasing = TRUE)
      eigenvals <- eigenvals[idx]
      V <- V[, idx]
      
      # Marchenko-Pastur bounds
      sigma2 <- sum(eigenvals) / length(eigenvals)
      lambda_plus <- sigma2 * (1 + sqrt(c))^2
      
      # Identify signal eigenvalues
      above_bounds <- sum(eigenvals > lambda_plus)
      
      # Apply cleaning
      noise_eigs <- eigenvals[eigenvals <= lambda_plus]
      if (length(noise_eigs) > 0) {
        if (mode == 'conservative') {
          replacement <- exp(mean(log(noise_eigs)))  # geometric mean
        } else {
          replacement <- mean(noise_eigs)  # arithmetic mean
        }
      } else {
        replacement <- lambda_plus * 0.1
      }
      
      clean_eigs <- eigenvals
      clean_eigs[eigenvals <= lambda_plus] <- max(replacement, lambda_plus * 0.1)
      
      # Reconstruct matrix
      S_clean <- V %*% diag(clean_eigs) %*% t(V)
      
      # Ensure positive definite
      min_eig <- min(clean_eigs)
      if (min_eig <= 1e-8) {
        S_clean <- S_clean + 1e-6 * diag(nrow(S))
      }
      
      condition_before <- max(eigenvals) / min(eigenvals)
      condition_after <- max(clean_eigs) / min(clean_eigs)
      
      return(list(
        cleaned = S_clean,
        num_above_bounds = above_bounds,
        condition_before = condition_before,
        condition_after = condition_after
      ))
    },
    
    #' Gerber MAD covariance estimator
    #' @param returns Returns matrix
    #' @param threshold_param Threshold parameter (default from config)
    #' @return List with covariance matrix and statistics
    estimateGerberMAD = function(returns, threshold_param = NULL) {
      if (is.null(threshold_param)) {
        threshold_param <- self$config$gerber_threshold
      }
      
      n <- nrow(returns)
      p <- ncol(returns)
      
      # Calculate MAD-based thresholds
      mad_values <- apply(returns, 2, function(x) {
        med_x <- median(x, na.rm = TRUE)
        mad_x <- median(abs(x - med_x), na.rm = TRUE)
        return(max(mad_x, 1e-6))
      })
      
      std_equiv <- self$config$gerber_mad_multiplier * mad_values
      thresholds <- threshold_param * std_equiv
      
      # Calculate concordant/discordant pairs
      concordant_matrix <- matrix(0, p, p)
      discordant_matrix <- matrix(0, p, p)
      total_significant <- matrix(0, p, p)
      
      for (i in 1:p) {
        for (j in i:p) {
          if (i == j) {
            concordant_matrix[i, j] <- n
            total_significant[i, j] <- n
            next
          }
          
          # Identify significant movements
          up_i <- returns[, i] >= thresholds[i]
          down_i <- returns[, i] <= -thresholds[i]
          up_j <- returns[, j] >= thresholds[j]
          down_j <- returns[, j] <= -thresholds[j]
          
          # Count pairs
          concordant <- sum((up_i & up_j) | (down_i & down_j))
          discordant <- sum((up_i & down_j) | (down_i & up_j))
          
          concordant_matrix[i, j] <- concordant_matrix[j, i] <- concordant
          discordant_matrix[i, j] <- discordant_matrix[j, i] <- discordant
          total_significant[i, j] <- total_significant[j, i] <- concordant + discordant
        }
      }
      
      # Calculate Gerber correlation matrix
      gerber_corr <- matrix(0, p, p)
      sample_corr <- cor(returns)
      
      for (i in 1:p) {
        for (j in 1:p) {
          if (i == j) {
            gerber_corr[i, j] <- 1
          } else if (total_significant[i, j] >= 5) {
            gerber_corr[i, j] <- (concordant_matrix[i, j] - discordant_matrix[i, j]) / 
                                total_significant[i, j]
          } else {
            gerber_corr[i, j] <- sample_corr[i, j] * 0.5
          }
        }
      }
      
      # Convert to covariance
      sample_std <- apply(returns, 2, sd)
      gerber_cov <- gerber_corr * (sample_std %*% t(sample_std))
      
      # Ensure positive definite
      gerber_cov_adj <- self$ensurePositiveDefinite(gerber_cov)
      
      # Calculate statistics
      avg_threshold_used <- mean(thresholds / std_equiv)
      sparsity_ratio <- sum(abs(gerber_corr) < 0.1) / (p^2 - p)
      eigenvals <- eigen(gerber_cov_adj, only.values = TRUE)$values
      condition_number <- max(eigenvals) / min(eigenvals)
      
      statistics <- c(avg_threshold_used, sparsity_ratio, condition_number)
      
      return(list(
        covariance = gerber_cov_adj,
        correlation = gerber_corr,
        statistics = statistics
      ))
    },
    
    #' Ensure matrix is positive definite
    #' @param sigma_matrix Input covariance matrix
    #' @return Positive definite covariance matrix
    ensurePositiveDefinite = function(sigma_matrix) {
      eigenvals <- eigen(sigma_matrix, only.values = TRUE)$values
      min_eig <- min(eigenvals)
      
      if (min_eig > 1e-6) {
        return(sigma_matrix)
      }
      
      p <- nrow(sigma_matrix)
      eigen_decomp <- eigen(sigma_matrix)
      eigenvals_adj <- pmax(eigen_decomp$values, 1e-6)
      
      # Control condition number
      max_eig <- max(eigenvals_adj)
      if (max_eig / min(eigenvals_adj) > 1000) {
        target_min <- max_eig / 500
        eigenvals_adj <- pmax(eigenvals_adj, target_min)
      }
      
      sigma_adj <- eigen_decomp$vectors %*% diag(eigenvals_adj) %*% t(eigen_decomp$vectors)
      return(sigma_adj)
    },
    
    #' Solve minimum variance portfolio optimization
    #' @param Sigma Covariance matrix
    #' @param mu Expected returns vector
    #' @return Portfolio weights vector
    solvePortfolio = function(Sigma, mu) {
      M <- length(mu)
      
      # Ensure positive definite
      min_eig <- min(eigen(Sigma, only.values = TRUE)$values)
      if (min_eig < 1e-8) {
        Sigma <- Sigma + 1e-6 * diag(M)
      }
      
      tryCatch({
        # Quadratic programming formulation
        Dmat <- 2 * Sigma
        dvec <- rep(0, M)
        
        # Constraints: return target, budget, non-negativity
        Amat <- cbind(mu, rep(1, M), diag(M))
        bvec <- c(self$config$min_return_daily, 1, rep(0, M))
        
        result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
        portfolio <- result$solution
        
        # Normalize and ensure non-negative
        portfolio <- pmax(portfolio, 0)
        portfolio <- portfolio / sum(portfolio)
        
        return(portfolio)
        
      }, error = function(e) {
        # Fallback to equal weights
        return(rep(1/M, M))
      })
    },
    
    #' Summarize comparison results
    #' @param period_risks Matrix of portfolio risks by period and method
    #' @param shrinkage_intensities Matrix of shrinkage intensities
    #' @param eigenvalue_stats Matrix of eigenvalue statistics
    #' @param gerber_stats Matrix of Gerber statistics
    #' @return Summary results list
    summarizeResults = function(period_risks, shrinkage_intensities, eigenvalue_stats, gerber_stats) {
      results <- list()
      results$period_risks <- period_risks
      results$shrinkage_intensities <- shrinkage_intensities
      results$eigenvalue_stats <- eigenvalue_stats
      results$gerber_stats <- gerber_stats
      
      # Calculate averages
      results$avg_risks <- colMeans(period_risks, na.rm = TRUE)
      results$std_risks <- apply(period_risks, 2, sd, na.rm = TRUE)
      
      # Find best method
      best_idx <- which.min(results$avg_risks)
      results$best_risk <- results$avg_risks[best_idx]
      results$best_method <- names(results$avg_risks)[best_idx]
      
      # Calculate improvements vs first method (typically SCM)
      baseline_risk <- results$avg_risks[1]
      results$improvements <- baseline_risk - results$avg_risks
      
      return(results)
    },
    
    #' Display comparison results
    displayResults = function() {
      if (is.null(self$results)) {
        cat("No results to display. Run runComparison() first.\n")
        return()
      }
      
      results <- self$results
      
      cat("\n=== COVARIANCE ESTIMATION COMPARISON RESULTS ===\n")
      cat("Average annualized portfolio risks:\n")
      
      for (i in seq_along(results$avg_risks)) {
        method_name <- names(results$avg_risks)[i]
        improvement <- results$improvements[i]
        cat(sprintf("  %-20s: %.2f%% (±%.2f%%) [%+.3f%%]\n",
                   method_name, 
                   results$avg_risks[i], 
                   results$std_risks[i], 
                   improvement))
      }
      
      cat(sprintf("\nBest performer: %s (%.2f%% risk)\n", 
                 results$best_method, results$best_risk))
      
      # Additional statistics
      if (!all(is.na(results$shrinkage_intensities))) {
        cat("\nShrinkage intensity averages:\n")
        shrink_means <- colMeans(results$shrinkage_intensities, na.rm = TRUE)
        for (i in seq_along(shrink_means)) {
          cat(sprintf("  %-12s: %.3f\n", 
                     names(shrink_means)[i], shrink_means[i]))
        }
      }
      
      if (!all(is.na(results$eigenvalue_stats))) {
        cat("\nRMT Analysis:\n")
        eig_means <- colMeans(results$eigenvalue_stats, na.rm = TRUE)
        cat(sprintf("  Avg eigenvalues above MP bounds: %.1f/%d\n", 
                   eig_means[1], ncol(self$returns)))
        cat(sprintf("  Avg condition improvement: %.1fx\n", 
                   eig_means[2] / eig_means[3]))
      }
    },
    
    #' Export results to files
    #' @param filename Base filename (without extension)
    #' @param output_dir Output directory
    #' @return List of exported file paths
    exportResults = function(filename = "covariance_results", output_dir = "exports") {
      if (is.null(self$results)) {
        stop("No results to export. Run runComparison() first.")
      }
      
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      base_path <- file.path(output_dir, paste0(filename, "_", timestamp))
      
      exported_files <- list()
      
      # Summary results
      summary_df <- data.frame(
        Method = names(self$results$avg_risks),
        Avg_Risk = self$results$avg_risks,
        Std_Risk = self$results$std_risks,
        Improvement = self$results$improvements,
        stringsAsFactors = FALSE
      )
      
      summary_file <- paste0(base_path, "_summary.csv")
      write.csv(summary_df, summary_file, row.names = FALSE)
      exported_files$summary <- summary_file
      
      # Detailed period risks
      risks_file <- paste0(base_path, "_period_risks.csv")
      write.csv(self$results$period_risks, risks_file, row.names = TRUE)
      exported_files$period_risks <- risks_file
      
      cat(sprintf("Results exported to: %s\n", output_dir))
      return(exported_files)
    }
  )
)



# library(R6)
# library(quadprog)
# library(Matrix)

# CovarianceEstimator <- R6::R6Class("CovarianceEstimator",
#   private = list(
#     method_map = list(
#       "SCM" = "SCM",
#       "SCM-Raw" = "SCMRaw", 
#       "Ledoit-Wolf-Official" = "LedoitWolfOfficial",
#       "Identity" = "Identity",
#       "Diagonal" = "Diagonal",
#       "RMT-Standard" = "RMTStandard",
#       "RMT-Conservative" = "RMTConservative",
#       "Gerber-MAD" = "GerberMAD"
#     )
#   ),
  
#   public = list(
#     # Data
#     returns = NULL,
#     dates = NULL,
#     tickers = NULL,
    
#     # Configuration and Results
#     config = NULL,
#     results = NULL,
#     cache = NULL,
    
#     initialize = function(returns, dates, tickers) {
#       self$returns <- returns
#       self$dates <- dates  
#       self$tickers <- tickers
      
#       # Default configuration matching MATLAB + Gerber MAD
#       self$config <- list(
#         training_days = 200,
#         update_frequency = 30,
#         min_return_daily = 0.0004,
#         methods = c('SCM', 'SCM-Raw', 'Ledoit-Wolf-Official', 'Identity', 'Diagonal', 
#                    'RMT-Standard', 'RMT-Conservative', 'Gerber-MAD'),
#         gerber_threshold = 0.4,  # Optimal from Zhou paper
#         gerber_mad_multiplier = 1.4826  # Normal distribution adjustment
#       )
      
#       self$results <- list()
#       self$cache <- list()
      
#       N <- nrow(returns)
#       M <- ncol(returns)
#       cat(sprintf("CovarianceEstimator initialized: %d days × %d assets\n", N, M))
#       cat(sprintf("Period: %s to %s\n", as.character(dates[1]), as.character(tail(dates, 1))))
#       cat(sprintf("Mean correlation: %.4f\n", mean(cor(returns), na.rm = TRUE)))
#       cat("Methods available:", paste(self$config$methods, collapse = ", "), "\n")
#     },
    
#     setParameters = function(...) {
#       params <- list(...)
#       for (param in names(params)) {
#         if (param %in% names(self$config)) {
#           self$config[[param]] <- params[[param]]
#           cat(sprintf("Set %s = %s\n", param, toString(params[[param]])))
#         } else {
#           warning(sprintf("Unknown parameter: %s", param))
#         }
#       }
#     },
    
#     runComparison = function() {
#       cat("\n=== RUNNING COVARIANCE ESTIMATION COMPARISON ===\n")
#       cat(sprintf("Training window: %d days\n", self$config$training_days))
#       cat(sprintf("Update frequency: %d days\n", self$config$update_frequency))
#       cat(sprintf("Methods: %s\n", paste(self$config$methods, collapse = ", ")))
#       cat(sprintf("Gerber MAD threshold: %.2f\n", self$config$gerber_threshold))
      
#       # Setup rolling windows
#       N <- nrow(self$returns)
#       window_starts <- seq(1, N - self$config$training_days - self$config$update_frequency, 
#                           by = self$config$update_frequency)
#       n_periods <- length(window_starts)
#       n_methods <- length(self$config$methods)
      
#       cat(sprintf("Number of rebalancing periods: %d\n", n_periods))
      
#       # Storage for results
#       period_risks <- matrix(NA, nrow = n_periods, ncol = n_methods)
#       shrinkage_intensities <- matrix(NA, nrow = n_periods, ncol = 3)
#       eigenvalue_stats <- matrix(NA, nrow = n_periods, ncol = 3)
#       gerber_stats <- matrix(NA, nrow = n_periods, ncol = 3)  # New for Gerber analysis
      
#       # Rolling window analysis
#       cat("\nRunning rolling window analysis...\n")
#       for (i in 1:n_periods) {
#         if (i %% 5 == 1) {
#           cat(sprintf("Period %d/%d\n", i, n_periods))
#         }
        
#         result <- tryCatch({
#           self$analyzePeriod(window_starts[i])
#         }, error = function(e) {
#           cat(sprintf("Error in period %d: %s\n", i, e$message))
#           list(risks = rep(NA, n_methods), 
#                shrink_rhos = rep(NA, 3), 
#                eig_stats = rep(NA, 3),
#                gerber_stats = rep(NA, 3))
#         })
        
#         period_risks[i, ] <- result$risks
#         shrinkage_intensities[i, ] <- result$shrink_rhos
#         eigenvalue_stats[i, ] <- result$eig_stats
#         gerber_stats[i, ] <- result$gerber_stats
#       }
      
#       # Summarize results
#       self$results <- self$summarizeResults(period_risks, shrinkage_intensities, 
#                                            eigenvalue_stats, gerber_stats)
      
#       # Display summary
#       self$displayResults()
      
#       return(self$results)
#     },
    
#     analyzePeriod = function(start_idx) {
#       end_idx <- start_idx + self$config$training_days - 1
#       test_start <- end_idx + 1
#       test_end <- min(test_start + self$config$update_frequency - 1, nrow(self$returns))
      
#       # Extract data
#       train_returns <- self$returns[start_idx:end_idx, , drop = FALSE]
#       test_returns <- self$returns[test_start:test_end, , drop = FALSE]
#       mu <- colMeans(train_returns)
      
#       # Estimate all covariance matrices
#       covmats <- self$estimateAllCovariances(train_returns)
      
#       # Test portfolio performance
#       risks <- numeric(length(self$config$methods))
#       for (i in seq_along(self$config$methods)) {
#         method_name <- self$config$methods[i]
#         field_name <- private$method_map[[method_name]]
#         portfolio <- self$solvePortfolio(covmats[[field_name]], mu)
#         port_returns <- test_returns %*% portfolio
#         risks[i] <- sd(port_returns) * sqrt(252) * 100  # Annualized
#       }
      
#       return(list(
#         risks = risks,
#         shrink_rhos = covmats$shrinkage_intensities,
#         eig_stats = covmats$eigenvalue_stats,
#         gerber_stats = covmats$gerber_statistics
#       ))
#     },
    
#     estimateAllCovariances = function(train_returns) {
#       n <- nrow(train_returns)
#       p <- ncol(train_returns)
      
#       covmats <- list()
      
#       # 1. Sample Covariance Matrix
#       covmats$SCM <- cov(train_returns)
      
#       # 1b. Raw Sample Covariance Matrix (from scratch)
#       covmats$SCMRaw <- self$calculateRawCovariance(train_returns)
      
#       # 2-4. Shrinkage Estimators
#       S <- covmats$SCM
#       sample_vars <- diag(S)
      
#       # Official Ledoit-Wolf shrinkage (constant correlation target)
#       lw_result <- self$covCor(train_returns)
#       covmats$LedoitWolfOfficial <- lw_result$sigmahat
#       rho_lw <- lw_result$shrinkage
      
#       # Identity target - should use SAMPLE variances, not unit diagonal
#       F_id <- diag(sample_vars)  # Keep sample variances on diagonal
#       rho_id <- self$estimateShrinkageIntensity(S, F_id, p/n)
#       covmats$Identity <- rho_id * F_id + (1 - rho_id) * S
      
#       # Diagonal target  
#       F_diag <- diag(mean(sample_vars), p)
#       rho_diag <- self$estimateShrinkageIntensity(S, F_diag, p/n)
#       covmats$Diagonal <- rho_diag * F_diag + (1 - rho_diag) * S
      
#       # Store shrinkage intensities
#       covmats$shrinkage_intensities <- c(rho_lw, rho_id, rho_diag)
      
#       # 5-6. RMT Methods (use raw covariance for consistency)
#       rmt_std <- self$applyRMTCleaning(covmats$SCMRaw, p/n, 'standard')
#       rmt_cons <- self$applyRMTCleaning(covmats$SCMRaw, p/n, 'conservative')
      
#       covmats$RMTStandard <- rmt_std$cleaned
#       covmats$RMTConservative <- rmt_cons$cleaned
      
#       covmats$eigenvalue_stats <- c(rmt_std$num_above_bounds, 
#                                    rmt_std$condition_before, 
#                                    rmt_std$condition_after)
      
#       # 7. NEW: Gerber MAD Covariance
#       gerber_result <- self$estimateGerberMAD(train_returns)
#       covmats$GerberMAD <- gerber_result$covariance
#       covmats$gerber_statistics <- gerber_result$statistics
      
#       return(covmats)
#     },
    
#     # NEW METHOD: Gerber MAD Covariance Implementation
#     estimateGerberMAD = function(returns, threshold_param = NULL) {
#       if (is.null(threshold_param)) {
#         threshold_param <- self$config$gerber_threshold
#       }
      
#       cat(sprintf("Computing Gerber MAD covariance (threshold=%.2f)...\n", threshold_param))
      
#       n <- nrow(returns)
#       p <- ncol(returns)
      
#       # Calculate MAD-based thresholds for each asset (more robust)
#       mad_values <- apply(returns, 2, function(x) {
#         med_x <- median(x, na.rm = TRUE)
#         mad_x <- median(abs(x - med_x), na.rm = TRUE)
#         # Ensure MAD is not zero (use small positive value if needed)
#         return(max(mad_x, 1e-6))
#       })
      
#       # Convert MAD to standard deviation equivalent
#       std_equiv <- self$config$gerber_mad_multiplier * mad_values
#       thresholds <- threshold_param * std_equiv
      
#       # Add small random perturbation to make threshold vary slightly
#       threshold_noise <- rnorm(p, 0, 0.001 * std_equiv)
#       thresholds <- thresholds + threshold_noise
      
#       # Initialize matrices for concordant/discordant pairs
#       concordant_matrix <- matrix(0, p, p)
#       discordant_matrix <- matrix(0, p, p)
#       total_significant <- matrix(0, p, p)
      
#       # Calculate Gerber statistics for each pair
#       for (i in 1:p) {
#         for (j in i:p) {
#           if (i == j) {
#             concordant_matrix[i, j] <- n  # Asset with itself
#             total_significant[i, j] <- n
#             next
#           }
          
#           # Identify significant movements (more lenient criteria)
#           up_i <- returns[, i] >= thresholds[i]
#           down_i <- returns[, i] <= -thresholds[i]
#           up_j <- returns[, j] >= thresholds[j]
#           down_j <- returns[, j] <= -thresholds[j]
          
#           # Count concordant pairs (same direction)
#           concordant <- sum((up_i & up_j) | (down_i & down_j))
          
#           # Count discordant pairs (opposite direction)
#           discordant <- sum((up_i & down_j) | (down_i & up_j))
          
#           # Store results (symmetric)
#           concordant_matrix[i, j] <- concordant_matrix[j, i] <- concordant
#           discordant_matrix[i, j] <- discordant_matrix[j, i] <- discordant
#           total_significant[i, j] <- total_significant[j, i] <- concordant + discordant
#         }
#       }
      
#       # Calculate Gerber correlation matrix with fallback
#       gerber_corr <- matrix(0, p, p)
#       sample_corr <- cor(returns)  # Fallback correlation
      
#       for (i in 1:p) {
#         for (j in 1:p) {
#           if (i == j) {
#             gerber_corr[i, j] <- 1
#           } else if (total_significant[i, j] >= 5) {  # Need minimum observations
#             gerber_corr[i, j] <- (concordant_matrix[i, j] - discordant_matrix[i, j]) / 
#                                 total_significant[i, j]
#           } else {
#             # Fallback to sample correlation if insufficient data
#             gerber_corr[i, j] <- sample_corr[i, j] * 0.5  # Shrink towards zero
#           }
#         }
#       }
      
#       # Convert to covariance using sample standard deviations
#       sample_std <- apply(returns, 2, sd)
#       gerber_cov <- gerber_corr * (sample_std %*% t(sample_std))
      
#       # Apply positive definiteness optimization
#       gerber_cov_adj <- self$ensurePositiveDefinite(gerber_cov)
      
#       # Calculate more realistic statistics
#       avg_threshold_used <- mean(thresholds / std_equiv)  # Should vary around threshold_param
#       sparsity_ratio <- sum(abs(gerber_corr) < 0.1) / (p^2 - p)  # Exclude diagonal
#       eigenvals <- eigen(gerber_cov_adj, only.values = TRUE)$values
#       condition_number <- max(eigenvals) / min(eigenvals)
      
#       statistics <- c(avg_threshold_used, sparsity_ratio, condition_number)
      
#       cat(sprintf("  Gerber MAD complete: sparsity=%.3f, condition=%.1f\n", 
#                  sparsity_ratio, condition_number))
      
#       return(list(
#         covariance = gerber_cov_adj,
#         correlation = gerber_corr,
#         statistics = statistics,
#         thresholds = thresholds,
#         concordant = concordant_matrix,
#         discordant = discordant_matrix
#       ))
#     },
    
#     # Positive definiteness optimization - more robust version
#     ensurePositiveDefinite = function(sigma_matrix) {
#       # Check if already positive definite
#       eigenvals <- eigen(sigma_matrix, only.values = TRUE)$values
#       min_eig <- min(eigenvals)
      
#       if (min_eig > 1e-6) {
#         return(sigma_matrix)  # Already positive definite
#       }
      
#       cat(sprintf("  Applying positive definiteness optimization (min eig: %.2e)\n", min_eig))
      
#       p <- nrow(sigma_matrix)
      
#       # Method 1: Eigenvalue adjustment (similar to RMT cleaning)
#       eigen_decomp <- eigen(sigma_matrix)
#       eigenvals_adj <- pmax(eigen_decomp$values, 1e-6)  # Floor small eigenvalues
      
#       # Reconstruct matrix
#       sigma_adj <- eigen_decomp$vectors %*% diag(eigenvals_adj) %*% t(eigen_decomp$vectors)
      
#       # Ensure condition number is reasonable (< 1000)
#       max_eig <- max(eigenvals_adj)
#       min_eig_adj <- min(eigenvals_adj)
      
#       if (max_eig / min_eig_adj > 1000) {
#         target_min <- max_eig / 500  # Target condition number of 500
#         eigenvals_adj <- pmax(eigenvals_adj, target_min)
#         sigma_adj <- eigen_decomp$vectors %*% diag(eigenvals_adj) %*% t(eigen_decomp$vectors)
#       }
      
#       return(sigma_adj)
#     },
    
#     # ... (keep all your existing methods: covCor, calculateRawCovariance, etc.)
    
#     covCor = function(Y) {
#       # Official Ledoit-Wolf shrinkage estimator (constant correlation target)
#       # Direct translation from Olivier Ledoit's covCor.m
      
#       N <- nrow(Y)
#       p <- ncol(Y)
      
#       # Demean the data
#       Y <- Y - matrix(colMeans(Y), N, p, byrow = TRUE)
#       n <- N - 1  # Adjust effective sample size
      
#       # Compute sample covariance matrix
#       sample <- (t(Y) %*% Y) / n
      
#       # Compute shrinkage target (constant correlation)
#       samplevar <- diag(sample)
#       sqrtvar <- sqrt(samplevar)
#       rBar <- (sum(sample / (sqrtvar %*% t(sqrtvar))) - p) / (p * (p - 1))  # mean correlation
#       target <- rBar * sqrtvar %*% t(sqrtvar)
#       diag(target) <- samplevar
      
#       # Estimate pi parameter
#       Y2 <- Y^2
#       sample2 <- (t(Y2) %*% Y2) / n  # sample covariance matrix of squared returns
#       piMat <- sample2 - sample^2
#       pihat <- sum(piMat)
      
#       # Estimate gamma parameter
#       gammahat <- norm(sample - target, "F")^2
      
#       # Diagonal part of rho parameter
#       rho_diag <- sum(diag(piMat))
      
#       # Off-diagonal part of rho parameter
#       term1 <- (t(Y^3) %*% Y) / n
#       term2 <- matrix(samplevar, p, p, byrow = FALSE) * sample
#       thetaMat <- term1 - term2
#       diag(thetaMat) <- 0
#       rho_off <- rBar * sum(((1/sqrtvar) %*% t(sqrtvar)) * thetaMat)
      
#       # Compute shrinkage intensity
#       rhohat <- rho_diag + rho_off
#       kappahat <- (pihat - rhohat) / gammahat
#       shrinkage <- max(0, min(1, kappahat / n))
      
#       # Compute shrinkage estimator
#       sigmahat <- shrinkage * target + (1 - shrinkage) * sample
      
#       return(list(sigmahat = sigmahat, shrinkage = shrinkage))
#     },
    
#     calculateRawCovariance = function(returns) {
#       # Calculate covariance matrix from scratch without using cov()
#       N <- nrow(returns)
#       p <- ncol(returns)
      
#       # Center the data (subtract mean)
#       X_centered <- returns - matrix(colMeans(returns), N, p, byrow = TRUE)
      
#       # Test both versions
#       S_mle <- (t(X_centered) %*% X_centered) / N        # MLE version
#       S_unbiased <- (t(X_centered) %*% X_centered) / (N - 1)  # Unbiased version
#       S_matlab <- cov(returns)
      
#       # Compare both
#       diff_mle <- norm(S_mle - S_matlab, "F")
#       diff_unbiased <- norm(S_unbiased - S_matlab, "F")
      
#       # Use the one that matches R's cov() (should be unbiased)
#       if (diff_unbiased < diff_mle) {
#         S_raw <- S_unbiased
#         if (diff_unbiased > 1e-12) {
#           cat(sprintf("  Raw covariance (unbiased) vs R diff: %.2e\n", diff_unbiased))
#         }
#       } else {
#         S_raw <- S_mle
#         cat(sprintf("  Using MLE version, diff from R: %.2e\n", diff_mle))
#       }
      
#       # Check positive definiteness
#       min_eig <- min(eigen(S_raw, only.values = TRUE)$values)
#       if (min_eig <= 1e-12) {
#         cat(sprintf("  Raw covariance regularized (min eig: %.2e)\n", min_eig))
#         S_raw <- S_raw + 1e-8 * diag(p)
#       }
      
#       return(S_raw)
#     },
    
#     estimateShrinkageIntensity = function(S, F, c) {
#       # More realistic shrinkage intensity estimation
#       # Based on the relative Frobenius norm difference
#       numerator <- norm(S - F, "F")^2
#       denominator <- norm(S, "F")^2
      
#       # Basic shrinkage that adapts to dimensionality ratio
#       base_shrinkage <- min(0.8, c / (1 + c))  # Higher c -> more shrinkage needed
      
#       # Adjust based on how different S and F are
#       if (denominator > 0) {
#         distance_factor <- numerator / denominator
#         # More different matrices need less shrinkage to preserve signal
#         adjustment <- -0.3 * min(1, distance_factor)
#       } else {
#         adjustment <- 0
#       }
      
#       rho <- max(0.01, min(0.95, base_shrinkage + adjustment))
#       return(rho)
#     },
    
#     applyRMTCleaning = function(S, c, mode) {
#       # Apply Random Matrix Theory eigenvalue cleaning
#       eigen_decomp <- eigen(S)
#       eigenvals <- eigen_decomp$values
#       V <- eigen_decomp$vectors
      
#       # Sort in descending order
#       idx <- order(eigenvals, decreasing = TRUE)
#       eigenvals <- eigenvals[idx]
#       V <- V[, idx]
      
#       # Marchenko-Pastur bounds
#       sigma2 <- sum(eigenvals) / length(eigenvals)  # trace(S) / p
#       lambda_plus <- sigma2 * (1 + sqrt(c))^2
      
#       # Identify signal eigenvalues
#       above_bounds <- sum(eigenvals > lambda_plus)
      
#       # Apply cleaning
#       if (mode == 'conservative') {
#         replacement_vals <- eigenvals[eigenvals <= lambda_plus]
#         if (length(replacement_vals) > 0) {
#           replacement <- exp(mean(log(replacement_vals)))  # geometric mean
#         } else {
#           replacement <- lambda_plus * 0.1
#         }
#       } else {
#         replacement_vals <- eigenvals[eigenvals <= lambda_plus]
#         if (length(replacement_vals) > 0) {
#           replacement <- mean(replacement_vals)
#         } else {
#           replacement <- lambda_plus * 0.1
#         }
#       }
      
#       clean_eigs <- eigenvals
#       clean_eigs[eigenvals <= lambda_plus] <- max(replacement, lambda_plus * 0.1)
      
#       # Reconstruct
#       S_clean <- V %*% diag(clean_eigs) %*% t(V)
      
#       # Ensure positive definite
#       if (min(clean_eigs) <= 1e-8) {
#         S_clean <- S_clean + 1e-6 * diag(nrow(S))
#       }
      
#       # Statistics
#       condition_before <- max(eigenvals) / min(eigenvals)
#       condition_after <- max(clean_eigs) / min(clean_eigs)
      
#       return(list(
#         cleaned = S_clean,
#         num_above_bounds = above_bounds,
#         condition_before = condition_before,
#         condition_after = condition_after
#       ))
#     },
    
#     solvePortfolio = function(Sigma, mu) {
#       # Solve minimum variance portfolio with return constraint
#       M <- length(mu)
      
#       # Ensure positive definite
#       min_eig <- min(eigen(Sigma, only.values = TRUE)$values)
#       if (min_eig < 1e-8) {
#         Sigma <- Sigma + 1e-6 * diag(M)
#       }
      
#       # Quadratic programming setup
#       # min 0.5 * w' * Sigma * w
#       # subject to: mu' * w >= min_return_daily, sum(w) = 1, w >= 0
      
#       tryCatch({
#         Dmat <- 2 * Sigma
#         dvec <- rep(0, M)
        
#         # Inequality constraints: mu' * w >= min_return_daily, w >= 0
#         Amat <- cbind(mu, diag(M))
#         bvec <- c(self$config$min_return_daily, rep(0, M))
        
#         # Equality constraint: sum(w) = 1
#         Aeq <- matrix(1, 1, M)
#         beq <- 1
        
#         # Solve QP
#         result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
#         portfolio <- result$solution
        
#         # Ensure non-negative and normalized
#         portfolio <- pmax(portfolio, 0)
#         portfolio <- portfolio / sum(portfolio)
        
#         return(portfolio)
        
#       }, error = function(e) {
#         # Fallback to equal weights
#         return(rep(1/M, M))
#       })
#     },
    
#     summarizeResults = function(period_risks, shrinkage_intensities, eigenvalue_stats, gerber_stats) {
#       # Summarize analysis results
#       results <- list()
#       results$period_risks <- period_risks
#       results$shrinkage_intensities <- shrinkage_intensities
#       results$eigenvalue_stats <- eigenvalue_stats
#       results$gerber_stats <- gerber_stats
      
#       # Calculate averages
#       results$avg_risks <- colMeans(period_risks, na.rm = TRUE)
#       results$std_risks <- apply(period_risks, 2, sd, na.rm = TRUE)
      
#       # Find best method
#       best_idx <- which.min(results$avg_risks)
#       results$best_risk <- results$avg_risks[best_idx]
#       results$best_method <- self$config$methods[best_idx]
      
#       # Calculate improvements vs SCM
#       results$improvements <- results$avg_risks[1] - results$avg_risks
      
#       return(results)
#     },
    
#     displayResults = function() {
#       # Display summary of results
#       results <- self$results
      
#       cat("\n=== FINAL RESULTS ===\n")
#       cat("Average annualized portfolio risks:\n")
      
#       for (i in seq_along(self$config$methods)) {
#         improvement <- results$improvements[i]
#         cat(sprintf("  %-20s: %.2f%% (±%.2f%%) [%+.3f%% vs SCM]\n",
#                    self$config$methods[i], 
#                    results$avg_risks[i], 
#                    results$std_risks[i], 
#                    improvement))
#       }
      
#       cat(sprintf("\nBest performer: %s (%.2f%% risk)\n", 
#                  results$best_method, results$best_risk))
      
#       # Additional statistics
#       cat("\nShrinkage intensity averages:\n")
#       shrink_names <- c("Ledoit-Wolf", "Identity", "Diagonal")
#       for (i in 1:3) {
#         shrink_mean <- mean(results$shrinkage_intensities[, i], na.rm = TRUE)
#         shrink_sd <- sd(results$shrinkage_intensities[, i], na.rm = TRUE)
#         cat(sprintf("  %-12s: %.3f ± %.3f\n", shrink_names[i], shrink_mean, shrink_sd))
#       }
      
#       cat("\nRMT Analysis:\n")
#       eig_mean_above <- mean(results$eigenvalue_stats[, 1], na.rm = TRUE)
#       cond_improvement <- mean(results$eigenvalue_stats[, 2] / results$eigenvalue_stats[, 3], na.rm = TRUE)
      
#       cat(sprintf("  Avg eigenvalues above MP bounds: %.1f/%d\n", 
#                  eig_mean_above, ncol(self$returns)))
#       cat(sprintf("  Avg condition number improvement: %.1fx\n", cond_improvement))
      
#       # NEW: Gerber MAD statistics
#       if (!is.null(results$gerber_stats)) {
#         cat("\nGerber MAD Analysis:\n")
#         gerber_mean_threshold <- mean(results$gerber_stats[, 1], na.rm = TRUE)
#         gerber_mean_sparsity <- mean(results$gerber_stats[, 2], na.rm = TRUE)
#         gerber_mean_condition <- mean(results$gerber_stats[, 3], na.rm = TRUE)
        
#         cat(sprintf("  Avg threshold used: %.3f\n", gerber_mean_threshold))
#         cat(sprintf("  Avg sparsity ratio: %.3f\n", gerber_mean_sparsity))
#         cat(sprintf("  Avg condition number: %.1f\n", gerber_mean_condition))
#       }
#     },
    
#     plotResults = function() {
#       # Create visualization of results
#       if (is.null(self$results)) {
#         cat("No results to plot. Run runComparison() first.\n")
#         return()
#       }
      
#       # Set up plotting area
#       par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))
      
#       # Plot 1: Risk comparison
#       barplot(self$results$avg_risks, 
#               names.arg = self$config$methods, 
#               las = 2, 
#               main = "Average Portfolio Risk by Method",
#               ylab = "Average Risk (%)",
#               col = "lightblue",
#               cex.names = 0.8)
      
#       # Plot 2: Risk improvement vs SCM
#       improvements <- self$results$improvements
#       colors <- ifelse(improvements > 0, "green", "red")
#       barplot(improvements, 
#               names.arg = self$config$methods, 
#               las = 2,
#               main = "Risk Improvement Over SCM",
#               ylab = "Risk Improvement vs SCM (%)",
#               col = colors,
#               cex.names = 0.8)
#       abline(h = 0, col = "black", lwd = 2)
      
#       # Plot 3: Time series of risks
#       matplot(self$results$period_risks, 
#               type = "l", 
#               lwd = 1.5,
#               main = "Risk Over Time",
#               xlab = "Rebalancing Period",
#               ylab = "Portfolio Risk (%)",
#               col = rainbow(length(self$config$methods)))
#       legend("topright", legend = self$config$methods, 
#              col = rainbow(length(self$config$methods)), 
#              lty = 1, cex = 0.5)
      
#       # Plot 4: Shrinkage intensities
#       boxplot(self$results$shrinkage_intensities, 
#               names = c("Ledoit-Wolf", "Identity", "Diagonal"),
#               main = "Shrinkage Intensity Distribution",
#               ylab = "Shrinkage Intensity")
      
#       # Plot 5: Eigenvalue statistics
#       plot(self$results$eigenvalue_stats[, 1], 
#            type = "b", 
#            col = "blue", 
#            lwd = 1.5,
#            main = "RMT Analysis: Signal Eigenvalues",
#            xlab = "Period",
#            ylab = "Eigenvalues Above MP Bounds")
      
#       # Plot 6: Method ranking over time
#       rankings <- t(apply(self$results$period_risks, 1, rank))
#       image(1:nrow(rankings), 1:ncol(rankings), rankings,
#             main = "Method Ranking Over Time (1=Best)",
#             xlab = "Rebalancing Period",
#             ylab = "Method",
#             col = heat.colors(length(self$config$methods)))
      
#       # NEW Plot 7: Gerber MAD sparsity over time
#       if (!is.null(self$results$gerber_stats)) {
#         plot(self$results$gerber_stats[, 2], 
#              type = "b", 
#              col = "purple", 
#              lwd = 1.5,
#              main = "Gerber MAD Sparsity Over Time",
#              xlab = "Period",
#              ylab = "Sparsity Ratio")
#       }
      
#       # NEW Plot 8: Risk comparison focused on best performers
#       top_methods <- order(self$results$avg_risks)[1:4]
#       barplot(self$results$avg_risks[top_methods], 
#               names.arg = self$config$methods[top_methods], 
#               las = 2,
#               main = "Top 4 Performers",
#               ylab = "Average Risk (%)",
#               col = c("gold", "silver", "#CD7F32", "lightblue"),
#               cex.names = 0.8)
      
#       # Reset plotting parameters
#       par(mfrow = c(1, 1))
#     },
    
#     # NEW: Test Gerber MAD on specific data
#     testGerberMAD = function(subset_period = NULL, threshold_range = seq(0.2, 0.8, 0.1)) {
#       cat("\n=== TESTING GERBER MAD ESTIMATOR ===\n")
      
#       # Use subset of data if specified
#       if (is.null(subset_period)) {
#         test_returns <- self$returns[1:min(200, nrow(self$returns)), ]
#       } else {
#         test_returns <- self$returns[subset_period, ]
#       }
      
#       cat(sprintf("Testing on %d observations, %d assets\n", nrow(test_returns), ncol(test_returns)))
      
#       # Test different threshold parameters
#       results_df <- data.frame(
#         threshold = numeric(),
#         condition_number = numeric(),
#         sparsity = numeric(),
#         pos_def = logical(),
#         stringsAsFactors = FALSE
#       )
      
#       for (thresh in threshold_range) {
#         cat(sprintf("Testing threshold %.1f...\n", thresh))
        
#         gerber_result <- self$estimateGerberMAD(test_returns, thresh)
        
#         results_df <- rbind(results_df, data.frame(
#           threshold = thresh,
#           condition_number = gerber_result$statistics[3],
#           sparsity = gerber_result$statistics[2],
#           pos_def = min(eigen(gerber_result$covariance, only.values = TRUE)$values) > 1e-8
#         ))
#       }
      
#       cat("\nThreshold sensitivity analysis:\n")
#       print(results_df)
      
#       # Find optimal threshold (balance of condition number and sparsity)
#       optimal_idx <- which.min(results_df$condition_number * (1 - results_df$sparsity))
#       optimal_threshold <- results_df$threshold[optimal_idx]
      
#       cat(sprintf("\nRecommended threshold: %.1f\n", optimal_threshold))
#       cat("(Balances condition number and sparsity)\n")
      
#       return(list(
#         results = results_df,
#         optimal_threshold = optimal_threshold,
#         gerber_estimate = self$estimateGerberMAD(test_returns, optimal_threshold)
#       ))
#     }
#   )
# )
