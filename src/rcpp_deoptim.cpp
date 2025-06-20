// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <omp.h>

//' Portfolio Objective Function
//'
//' Calculates the objective function for portfolio optimization.
//' The objective is to maximize mean return while minimizing risk.
//'
//' @param weights Vector of portfolio weights
//' @param returns Matrix of asset returns (rows = time, cols = assets)
//' @param cov_matrix Covariance matrix of asset returns
//' @param risk_aversion Risk aversion parameter (default: 0.5)
//' @return Objective function value (negative for maximization)
//'
// [[Rcpp::export]]
double portfolio_objective(
    const arma::vec& weights,
    const arma::mat& returns,
    const arma::mat& cov_matrix,
    double risk_aversion = 0.5,
    const std::string& objective = "mean_variance",
    double risk_free_rate = 0.0) {
  
  // Calculate mean return
  double mean_return = arma::mean(returns * weights);
  
  // Calculate portfolio variance
  double variance = arma::as_scalar(weights.t() * cov_matrix * weights);
  double std_dev = sqrt(variance);
  
  // Different objectives
  if (objective == "max_sharpe") {
    // Sharpe ratio: (return - risk_free) / std_dev
    // We minimize negative Sharpe ratio
    if (std_dev < 1e-8) return 1e10;  // Penalize zero variance
    double excess_return = mean_return - risk_free_rate / 252;  // Daily risk-free
    return -(excess_return / std_dev);  // Negative for minimization
  } else if (objective == "min_variance") {
    // Minimize variance only
    return variance;
  } else {
    // Default: mean-variance optimization
    return -mean_return + risk_aversion * std_dev;
  }
}

//' Differential Evolution Portfolio Optimization
//'
//' Optimizes portfolio weights using Differential Evolution algorithm
//' with OpenMP parallelization for improved performance.
//'
//' @param returns Matrix of asset returns (rows = time, cols = assets)
//' @param cov_matrix Covariance matrix of asset returns
//' @param min_weight Minimum weight per asset (default: 0.01)
//' @param max_weight Maximum weight per asset (default: 0.15)
//' @param pop_size Population size for DE algorithm (default: 50)
//' @param max_iter Maximum number of iterations (default: 200)
//' @param F Differential weight [0,2] (default: 0.8)
//' @param CR Crossover probability [0,1] (default: 0.9)
//' @param ncores Number of cores for parallel processing (default: 1)
//' @param DEBUG Whether to print debug information (default: false)
//' @return Vector of optimized portfolio weights
//'
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::export]]
arma::vec deoptim_portfolio_rcpp(
    const arma::mat& returns,
    const arma::mat& cov_matrix,
    double min_weight = 0.01,
    double max_weight = 0.15,
    int pop_size = 50,
    int max_iter = 200,
    double F = 0.8,
    double CR = 0.9,
    int ncores = 1,
    bool DEBUG = false,
    std::string objective = "mean_variance",
    double risk_aversion = 0.5,
    double risk_free_rate = 0.0) {

  // Validate inputs
  int n_assets = returns.n_cols;
  if (n_assets == 0 || returns.n_rows == 0) {
    Rcpp::stop("Error: Returns matrix is empty!");
  }

  // Validate weight constraints
  if (min_weight < 0 || max_weight > 1 || min_weight >= max_weight) {
    Rcpp::stop("Invalid weight constraints: min_weight must be >= 0, max_weight <= 1, and min_weight < max_weight");
  }
  if (min_weight * n_assets > 1.0) {
    Rcpp::stop("min_weight too high: min_weight * n_assets must be <= 1");
  }

  // Initialize population and fitness
  arma::mat population(pop_size, n_assets);
  arma::vec fitness(pop_size, arma::fill::zeros);

  // Initialize population with random weights that sum to 1
  #pragma omp parallel for num_threads(ncores)
  for (int i = 0; i < pop_size; i++) {
    // Generate random weights
    arma::vec weights = arma::randu(n_assets);
    
    // Normalize to sum to 1
    weights /= std::max(sum(weights), 1e-8);  // Avoid division by zero
    
    // Apply bounds
    weights = arma::clamp(weights, min_weight, max_weight);
    
    // Re-normalize after clamping
    weights /= std::max(sum(weights), 1e-8);
    
    // Store in population
    population.row(i) = weights.t();
    
    // Calculate fitness
    fitness(i) = portfolio_objective(weights, returns, cov_matrix, risk_aversion, objective, risk_free_rate);
  }

  // Main DE loop
  for (int gen = 0; gen < max_iter; gen++) {
    // Process each individual in the population
    #pragma omp parallel for num_threads(ncores)
    for (int i = 0; i < pop_size; i++) {
      // Select three unique random individuals different from i
      int r1, r2, r3;
      do { r1 = arma::randi<arma::uvec>(1, arma::distr_param(0, pop_size-1))[0]; } while (r1 == i);
      do { r2 = arma::randi<arma::uvec>(1, arma::distr_param(0, pop_size-1))[0]; } while (r2 == r1 || r2 == i);
      do { r3 = arma::randi<arma::uvec>(1, arma::distr_param(0, pop_size-1))[0]; } while (r3 == r1 || r3 == r2 || r3 == i);

      // Mutation: Create trial vector
      arma::vec trial = population.row(r1).t() + F * (population.row(r2).t() - population.row(r3).t());

      // Crossover: Mix trial vector with current individual
      arma::uvec crossover_indices = arma::find(arma::randu(trial.n_elem) < CR);
      arma::vec parent = population.row(i).t();
      
      // For selected indices, keep parent values
      trial.elem(crossover_indices) = parent.elem(crossover_indices);

      // Ensure constraints
      // First normalize
      trial = trial / std::max(sum(trial), 1e-8);
      
      // Apply min/max weight bounds
      trial = arma::clamp(trial, min_weight, max_weight);
      
      // Re-normalize after clamping
      trial = trial / std::max(sum(trial), 1e-8);

      // Selection: Keep better solution
      double trial_fitness = portfolio_objective(trial, returns, cov_matrix, risk_aversion, objective, risk_free_rate);

      // Critical section to avoid parallel write conflicts
      #pragma omp critical
      {
        if (trial_fitness < fitness(i)) {
          population.row(i) = trial.t();
          fitness(i) = trial_fitness;
        }
      }
    }

    // Print progress if in debug mode
    if (DEBUG && (gen % 50 == 0)) {
      #pragma omp critical
      {
        Rcpp::Rcout << "Iteration: " << gen
                    << " Best Value: " << fitness.min()
                    << " Best Weight: " << population.row(fitness.index_min())
                    << std::endl;
      }
    }
  }

  // Return best solution (weights that minimize the objective function)
  return population.row(fitness.index_min()).t();
}

//' Create Equal-Weight Portfolio
//'
//' Creates an equal-weight portfolio with weights summing to 1.
//'
//' @param n_assets Number of assets in the portfolio
//' @return Vector of equal weights
//'
// [[Rcpp::export]]
arma::vec equal_weight_portfolio(int n_assets) {
  return arma::ones(n_assets) / n_assets;
}

//' Calculate Portfolio Returns
//'
//' Calculates the returns of a portfolio given weights and asset returns.
//'
//' @param returns Matrix of asset returns (rows = time, cols = assets)
//' @param weights Vector of portfolio weights
//' @return Vector of portfolio returns
//'
// [[Rcpp::export]]
arma::vec portfolio_returns(const arma::mat& returns, const arma::vec& weights) {
  return returns * weights;
}

//' Calculate Sharpe Ratio
//'
//' Calculates the Sharpe ratio of a portfolio.
//'
//' @param returns Vector of portfolio returns
//' @param risk_free Risk-free rate (default: 0)
//' @return Sharpe ratio
//'
// [[Rcpp::export]]
double sharpe_ratio(const arma::vec& returns, double risk_free = 0.0) {
  double mean_excess_return = arma::mean(returns) - risk_free;
  double std_dev = arma::stddev(returns);
  
  // Avoid division by zero
  if (std_dev < 1e-10) {
    return 0.0;
  }
  
  return mean_excess_return / std_dev;
}


