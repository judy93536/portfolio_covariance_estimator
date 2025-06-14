library(R6)
library(quadprog)
library(Matrix)

CovarianceEstimator <- R6::R6Class("CovarianceEstimator",
  private = list(
    method_map = list(
      "SCM" = "SCM",
      "SCM-Raw" = "SCMRaw", 
      "Ledoit-Wolf-Official" = "LedoitWolfOfficial",
      "Identity" = "Identity",
      "Diagonal" = "Diagonal",
      "RMT-Standard" = "RMTStandard",
      "RMT-Conservative" = "RMTConservative"
    )
  ),
  
  public = list(
    # Data
    returns = NULL,
    dates = NULL,
    tickers = NULL,
    
    # Configuration and Results
    config = NULL,
    results = NULL,
    cache = NULL,
    
    initialize = function(returns, dates, tickers) {
      self$returns <- returns
      self$dates <- dates  
      self$tickers <- tickers
      
      # Default configuration matching MATLAB
      self$config <- list(
        training_days = 200,
        update_frequency = 30,
        min_return_daily = 0.0004,
        methods = c('SCM', 'SCM-Raw', 'Ledoit-Wolf-Official', 'Identity', 'Diagonal', 'RMT-Standard', 'RMT-Conservative')
      )
      
      self$results <- list()
      self$cache <- list()
      
      N <- nrow(returns)
      M <- ncol(returns)
      cat(sprintf("CovarianceEstimator initialized: %d days × %d assets\n", N, M))
      cat(sprintf("Period: %s to %s\n", as.character(dates[1]), as.character(tail(dates, 1))))
      cat(sprintf("Mean correlation: %.4f\n", mean(cor(returns), na.rm = TRUE)))
    },
    
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
    
    runComparison = function() {
      cat("\n=== RUNNING COVARIANCE ESTIMATION COMPARISON ===\n")
      cat(sprintf("Training window: %d days\n", self$config$training_days))
      cat(sprintf("Update frequency: %d days\n", self$config$update_frequency))
      cat(sprintf("Methods: %s\n", paste(self$config$methods, collapse = ", ")))
      
      # Setup rolling windows
      N <- nrow(self$returns)
      window_starts <- seq(1, N - self$config$training_days - self$config$update_frequency, 
                          by = self$config$update_frequency)
      n_periods <- length(window_starts)
      n_methods <- length(self$config$methods)
      
      cat(sprintf("Number of rebalancing periods: %d\n", n_periods))
      
      # Storage for results
      period_risks <- matrix(NA, nrow = n_periods, ncol = n_methods)
      shrinkage_intensities <- matrix(NA, nrow = n_periods, ncol = 3)
      eigenvalue_stats <- matrix(NA, nrow = n_periods, ncol = 3)
      
      # Rolling window analysis
      cat("\nRunning rolling window analysis...\n")
      for (i in 1:n_periods) {
        if (i %% 5 == 1) {
          cat(sprintf("Period %d/%d\n", i, n_periods))
        }
        
        result <- tryCatch({
          self$analyzePeriod(window_starts[i])
        }, error = function(e) {
          cat(sprintf("Error in period %d: %s\n", i, e$message))
          list(risks = rep(NA, n_methods), 
               shrink_rhos = rep(NA, 3), 
               eig_stats = rep(NA, 3))
        })
        
        period_risks[i, ] <- result$risks
        shrinkage_intensities[i, ] <- result$shrink_rhos
        eigenvalue_stats[i, ] <- result$eig_stats
      }
      
      # Summarize results
      self$results <- self$summarizeResults(period_risks, shrinkage_intensities, eigenvalue_stats)
      
      # Display summary
      self$displayResults()
      
      return(self$results)
    },
    
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
      for (i in seq_along(self$config$methods)) {
        method_name <- self$config$methods[i]
        field_name <- private$method_map[[method_name]]
        portfolio <- self$solvePortfolio(covmats[[field_name]], mu)
        port_returns <- test_returns %*% portfolio
        risks[i] <- sd(port_returns) * sqrt(252) * 100  # Annualized
      }
      
      return(list(
        risks = risks,
        shrink_rhos = covmats$shrinkage_intensities,
        eig_stats = covmats$eigenvalue_stats
      ))
    },
    
    estimateAllCovariances = function(train_returns) {
      n <- nrow(train_returns)
      p <- ncol(train_returns)
      
      covmats <- list()
      
      # 1. Sample Covariance Matrix
      covmats$SCM <- cov(train_returns)
      
      # 1b. Raw Sample Covariance Matrix (from scratch)
      covmats$SCMRaw <- self$calculateRawCovariance(train_returns)
      
      # 2-4. Shrinkage Estimators
      S <- covmats$SCM
      sample_vars <- diag(S)
      
      # Official Ledoit-Wolf shrinkage (constant correlation target)
      lw_result <- self$covCor(train_returns)
      covmats$LedoitWolfOfficial <- lw_result$sigmahat
      rho_lw <- lw_result$shrinkage
      
      # Identity target
      F_id <- diag(sample_vars)
      rho_id <- self$estimateShrinkageIntensity(S, F_id, p/n)
      covmats$Identity <- rho_id * F_id + (1 - rho_id) * S
      
      # Diagonal target  
      F_diag <- diag(mean(sample_vars), p)
      rho_diag <- self$estimateShrinkageIntensity(S, F_diag, p/n)
      covmats$Diagonal <- rho_diag * F_diag + (1 - rho_diag) * S
      
      # Store shrinkage intensities
      covmats$shrinkage_intensities <- c(rho_lw, rho_id, rho_diag)
      
      # 5-6. RMT Methods (use raw covariance for consistency)
      rmt_std <- self$applyRMTCleaning(covmats$SCMRaw, p/n, 'standard')
      rmt_cons <- self$applyRMTCleaning(covmats$SCMRaw, p/n, 'conservative')
      
      covmats$RMTStandard <- rmt_std$cleaned
      covmats$RMTConservative <- rmt_cons$cleaned
      
      covmats$eigenvalue_stats <- c(rmt_std$num_above_bounds, 
                                   rmt_std$condition_before, 
                                   rmt_std$condition_after)
      
      return(covmats)
    },
    
    covCor = function(Y) {
      # Official Ledoit-Wolf shrinkage estimator (constant correlation target)
      # Direct translation from Olivier Ledoit's covCor.m
      
      N <- nrow(Y)
      p <- ncol(Y)
      
      # Demean the data
      Y <- Y - matrix(colMeans(Y), N, p, byrow = TRUE)
      n <- N - 1  # Adjust effective sample size
      
      # Compute sample covariance matrix
      sample <- (t(Y) %*% Y) / n
      
      # Compute shrinkage target (constant correlation)
      samplevar <- diag(sample)
      sqrtvar <- sqrt(samplevar)
      rBar <- (sum(sample / (sqrtvar %*% t(sqrtvar))) - p) / (p * (p - 1))  # mean correlation
      target <- rBar * sqrtvar %*% t(sqrtvar)
      diag(target) <- samplevar
      
      # Estimate pi parameter
      Y2 <- Y^2
      sample2 <- (t(Y2) %*% Y2) / n  # sample covariance matrix of squared returns
      piMat <- sample2 - sample^2
      pihat <- sum(piMat)
      
      # Estimate gamma parameter
      gammahat <- norm(sample - target, "F")^2
      
      # Diagonal part of rho parameter
      rho_diag <- sum(diag(piMat))
      
      # Off-diagonal part of rho parameter
      term1 <- (t(Y^3) %*% Y) / n
      term2 <- matrix(samplevar, p, p, byrow = FALSE) * sample
      thetaMat <- term1 - term2
      diag(thetaMat) <- 0
      rho_off <- rBar * sum(((1/sqrtvar) %*% t(sqrtvar)) * thetaMat)
      
      # Compute shrinkage intensity
      rhohat <- rho_diag + rho_off
      kappahat <- (pihat - rhohat) / gammahat
      shrinkage <- max(0, min(1, kappahat / n))
      
      # Compute shrinkage estimator
      sigmahat <- shrinkage * target + (1 - shrinkage) * sample
      
      return(list(sigmahat = sigmahat, shrinkage = shrinkage))
    },
    
    calculateRawCovariance = function(returns) {
      # Calculate covariance matrix from scratch without using cov()
      N <- nrow(returns)
      p <- ncol(returns)
      
      # Center the data (subtract mean)
      X_centered <- returns - matrix(colMeans(returns), N, p, byrow = TRUE)
      
      # Test both versions
      S_mle <- (t(X_centered) %*% X_centered) / N        # MLE version
      S_unbiased <- (t(X_centered) %*% X_centered) / (N - 1)  # Unbiased version
      S_matlab <- cov(returns)
      
      # Compare both
      diff_mle <- norm(S_mle - S_matlab, "F")
      diff_unbiased <- norm(S_unbiased - S_matlab, "F")
      
      # Use the one that matches R's cov() (should be unbiased)
      if (diff_unbiased < diff_mle) {
        S_raw <- S_unbiased
        if (diff_unbiased > 1e-12) {
          cat(sprintf("  Raw covariance (unbiased) vs R diff: %.2e\n", diff_unbiased))
        }
      } else {
        S_raw <- S_mle
        cat(sprintf("  Using MLE version, diff from R: %.2e\n", diff_mle))
      }
      
      # Check positive definiteness
      min_eig <- min(eigen(S_raw, only.values = TRUE)$values)
      if (min_eig <= 1e-12) {
        cat(sprintf("  Raw covariance regularized (min eig: %.2e)\n", min_eig))
        S_raw <- S_raw + 1e-8 * diag(p)
      }
      
      return(S_raw)
    },
    
    estimateShrinkageIntensity = function(S, F, c) {
      # Estimate optimal shrinkage intensity using heuristic from MATLAB
      distance <- norm(S - F, "F") / norm(S, "F")
      base_shrinkage <- 0.05 + 0.4 * c
      adjustment <- 0.2 * distance
      rho <- min(0.7, base_shrinkage + adjustment)
      return(rho)
    },
    
    applyRMTCleaning = function(S, c, mode) {
      # Apply Random Matrix Theory eigenvalue cleaning
      eigen_decomp <- eigen(S)
      eigenvals <- eigen_decomp$values
      V <- eigen_decomp$vectors
      
      # Sort in descending order
      idx <- order(eigenvals, decreasing = TRUE)
      eigenvals <- eigenvals[idx]
      V <- V[, idx]
      
      # Marchenko-Pastur bounds
      sigma2 <- sum(eigenvals) / length(eigenvals)  # trace(S) / p
      lambda_plus <- sigma2 * (1 + sqrt(c))^2
      
      # Identify signal eigenvalues
      above_bounds <- sum(eigenvals > lambda_plus)
      
      # Apply cleaning
      if (mode == 'conservative') {
        replacement_vals <- eigenvals[eigenvals <= lambda_plus]
        if (length(replacement_vals) > 0) {
          replacement <- exp(mean(log(replacement_vals)))  # geometric mean
        } else {
          replacement <- lambda_plus * 0.1
        }
      } else {
        replacement_vals <- eigenvals[eigenvals <= lambda_plus]
        if (length(replacement_vals) > 0) {
          replacement <- mean(replacement_vals)
        } else {
          replacement <- lambda_plus * 0.1
        }
      }
      
      clean_eigs <- eigenvals
      clean_eigs[eigenvals <= lambda_plus] <- max(replacement, lambda_plus * 0.1)
      
      # Reconstruct
      S_clean <- V %*% diag(clean_eigs) %*% t(V)
      
      # Ensure positive definite
      if (min(clean_eigs) <= 1e-8) {
        S_clean <- S_clean + 1e-6 * diag(nrow(S))
      }
      
      # Statistics
      condition_before <- max(eigenvals) / min(eigenvals)
      condition_after <- max(clean_eigs) / min(clean_eigs)
      
      return(list(
        cleaned = S_clean,
        num_above_bounds = above_bounds,
        condition_before = condition_before,
        condition_after = condition_after
      ))
    },
    
    solvePortfolio = function(Sigma, mu) {
      # Solve minimum variance portfolio with return constraint
      M <- length(mu)
      
      # Ensure positive definite
      min_eig <- min(eigen(Sigma, only.values = TRUE)$values)
      if (min_eig < 1e-8) {
        Sigma <- Sigma + 1e-6 * diag(M)
      }
      
      # Quadratic programming setup
      # min 0.5 * w' * Sigma * w
      # subject to: mu' * w >= min_return_daily, sum(w) = 1, w >= 0
      
      tryCatch({
        Dmat <- 2 * Sigma
        dvec <- rep(0, M)
        
        # Inequality constraints: mu' * w >= min_return_daily, w >= 0
        Amat <- cbind(mu, diag(M))
        bvec <- c(self$config$min_return_daily, rep(0, M))
        
        # Equality constraint: sum(w) = 1
        Aeq <- matrix(1, 1, M)
        beq <- 1
        
        # Solve QP
        result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
        portfolio <- result$solution
        
        # Ensure non-negative and normalized
        portfolio <- pmax(portfolio, 0)
        portfolio <- portfolio / sum(portfolio)
        
        return(portfolio)
        
      }, error = function(e) {
        # Fallback to equal weights
        return(rep(1/M, M))
      })
    },
    
    summarizeResults = function(period_risks, shrinkage_intensities, eigenvalue_stats) {
      # Summarize analysis results
      results <- list()
      results$period_risks <- period_risks
      results$shrinkage_intensities <- shrinkage_intensities
      results$eigenvalue_stats <- eigenvalue_stats
      
      # Calculate averages
      results$avg_risks <- colMeans(period_risks, na.rm = TRUE)
      results$std_risks <- apply(period_risks, 2, sd, na.rm = TRUE)
      
      # Find best method
      best_idx <- which.min(results$avg_risks)
      results$best_risk <- results$avg_risks[best_idx]
      results$best_method <- self$config$methods[best_idx]
      
      # Calculate improvements vs SCM
      results$improvements <- results$avg_risks[1] - results$avg_risks
      
      return(results)
    },
    
    displayResults = function() {
      # Display summary of results
      results <- self$results
      
      cat("\n=== FINAL RESULTS ===\n")
      cat("Average annualized portfolio risks:\n")
      
      for (i in seq_along(self$config$methods)) {
        improvement <- results$improvements[i]
        cat(sprintf("  %-20s: %.2f%% (±%.2f%%) [%+.3f%% vs SCM]\n",
                   self$config$methods[i], 
                   results$avg_risks[i], 
                   results$std_risks[i], 
                   improvement))
      }
      
      cat(sprintf("\nBest performer: %s (%.2f%% risk)\n", 
                 results$best_method, results$best_risk))
      
      # Additional statistics
      cat("\nShrinkage intensity averages:\n")
      shrink_names <- c("Ledoit-Wolf", "Identity", "Diagonal")
      for (i in 1:3) {
        shrink_mean <- mean(results$shrinkage_intensities[, i], na.rm = TRUE)
        shrink_sd <- sd(results$shrinkage_intensities[, i], na.rm = TRUE)
        cat(sprintf("  %-12s: %.3f ± %.3f\n", shrink_names[i], shrink_mean, shrink_sd))
      }
      
      cat("\nRMT Analysis:\n")
      eig_mean_above <- mean(results$eigenvalue_stats[, 1], na.rm = TRUE)
      cond_improvement <- mean(results$eigenvalue_stats[, 2] / results$eigenvalue_stats[, 3], na.rm = TRUE)
      
      cat(sprintf("  Avg eigenvalues above MP bounds: %.1f/%d\n", 
                 eig_mean_above, ncol(self$returns)))
      cat(sprintf("  Avg condition number improvement: %.1fx\n", cond_improvement))
    },
    
    plotResults = function() {
      # Create visualization of results
      if (is.null(self$results)) {
        cat("No results to plot. Run runComparison() first.\n")
        return()
      }
      
      # Set up plotting area
      par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
      
      # Plot 1: Risk comparison
      barplot(self$results$avg_risks, 
              names.arg = self$config$methods, 
              las = 2, 
              main = "Average Portfolio Risk by Method",
              ylab = "Average Risk (%)",
              col = "lightblue")
      
      # Plot 2: Risk improvement vs SCM
      improvements <- self$results$improvements
      colors <- ifelse(improvements > 0, "green", "red")
      barplot(improvements, 
              names.arg = self$config$methods, 
              las = 2,
              main = "Risk Improvement Over SCM",
              ylab = "Risk Improvement vs SCM (%)",
              col = colors)
      abline(h = 0, col = "black", lwd = 2)
      
      # Plot 3: Time series of risks
      matplot(self$results$period_risks, 
              type = "l", 
              lwd = 1.5,
              main = "Risk Over Time",
              xlab = "Rebalancing Period",
              ylab = "Portfolio Risk (%)",
              col = rainbow(length(self$config$methods)))
      legend("topright", legend = self$config$methods, 
             col = rainbow(length(self$config$methods)), 
             lty = 1, cex = 0.6)
      
      # Plot 4: Shrinkage intensities
      boxplot(self$results$shrinkage_intensities, 
              names = c("Ledoit-Wolf", "Identity", "Diagonal"),
              main = "Shrinkage Intensity Distribution",
              ylab = "Shrinkage Intensity")
      
      # Plot 5: Eigenvalue statistics
      plot(self$results$eigenvalue_stats[, 1], 
           type = "b", 
           col = "blue", 
           lwd = 1.5,
           main = "RMT Analysis: Signal Eigenvalues",
           xlab = "Period",
           ylab = "Eigenvalues Above MP Bounds")
      
      # Plot 6: Method ranking over time
      rankings <- t(apply(self$results$period_risks, 1, rank))
      image(1:nrow(rankings), 1:ncol(rankings), rankings,
            main = "Method Ranking Over Time (1=Best)",
            xlab = "Rebalancing Period",
            ylab = "Method",
            col = heat.colors(length(self$config$methods)))
      
      # Reset plotting parameters
      par(mfrow = c(1, 1))
    }
  )
)