#' Display All Possible Parameter Combinations for Cronbach's Alpha
#'
#' This function computes and displays all possible combinations of Cronbach's alpha
#' bounds for various parameter settings, including combinations of optimization
#' methods, rough approximation, random sampling, and enumeration.
#'
#' @param scores_mat A matrix where rows represent persons and columns represent tests (or items),
#'   providing the performance of a person on a test. NA should be used for missing values.
#' @param score_max An integer indicating the largest possible score of the test.
#' @param tol A numeric value representing the desired accuracy in computing the lower and upper bound of Cronbach's alpha.
#' @param num_random An integer specifying the number of random samples used in estimating the lower and upper bound. Default is 1000.
#' @param num_opt An integer specifying the number of times to run the optimization algorithm. Default is 1.
#' @param methods A character vector specifying the optimization methods to be used (e.g., 'GA', 'DEoptim', 'nloptr'). Default is c('GA').
#' @param enum_all Logical, whether to include enumeration in the parameter combinations. Default is FALSE.
#' @param rough Logical, whether to include rough approximation in the parameter combinations. Default is TRUE.
#'
#' @return A list where each element is a result of the `cronbachs_alpha` function for a unique parameter combination, including computation time.
#'
#' @export
display_all <- function(
    scores_mat,
    score_max,
    tol = 1e-3,
    num_random = 1000,
    num_opt = 1,
    methods = c("GA", "DEoptim", "nloptr"),
    enum_all = FALSE,
    rough = TRUE
) {
  results <- list()

  # Check enumeration (if requested)
  if (enum_all) {
    start_time <- Sys.time()
    result_enum <- cronbachs_alpha(
      scores_mat,
      score_max = score_max,
      tol = tol,
      enum_all = TRUE
    )
    end_time <- Sys.time()
    results[["Enumeration_Method"]] <- list(
      Alpha_Min = result_enum$alpha_min_enum,
      Alpha_Max = result_enum$alpha_max_enum,
      Runtime = as.numeric(difftime(end_time, start_time, units = "secs"))
    )
  }

  # Check rough approximation (if requested)
  if (rough) {
    # Integer-only sampling
    start_time <- Sys.time()
    result_rough_int <- cronbachs_alpha(
      scores_mat,
      score_max = score_max,
      tol = tol,
      rough = TRUE,
      num_random = num_random,
      int_only = TRUE
    )
    end_time <- Sys.time()
    results[["Rough_Integer_Method"]] <- list(
      Alpha_Min = result_rough_int$alpha_min_rough,
      Alpha_Max = result_rough_int$alpha_max_rough,
      Runtime = as.numeric(difftime(end_time, start_time, units = "secs"))
    )

    # Float sampling
    start_time <- Sys.time()
    result_rough_float <- cronbachs_alpha(
      scores_mat,
      score_max = score_max,
      tol = tol,
      rough = TRUE,
      num_random = num_random,
      int_only = FALSE
    )
    end_time <- Sys.time()
    results[["Rough_Float_Method"]] <- list(
      Alpha_Min = result_rough_float$alpha_min_rough,
      Alpha_Max = result_rough_float$alpha_max_rough,
      Runtime = as.numeric(difftime(end_time, start_time, units = "secs"))
    )
  }

  # Loop through each optimization method
  for (method in methods) {
    start_time <- Sys.time()
    result_opt <- cronbachs_alpha(
      scores_mat,
      score_max = score_max,
      tol = tol,
      num_opt = num_opt,
      method = method
    )
    end_time <- Sys.time()
    results[[paste0("Optimization_Method_", method)]] <- list(
      Alpha_Min = result_opt$alpha_min_opt,
      Alpha_Max = result_opt$alpha_max_opt,
      Runtime = as.numeric(difftime(end_time, start_time, units = "secs"))
    )
  }
  class(results) <- "allResult"
  return(results)
}

#' @noRd
#' @method summary allResult
#' @export
summary.allResult <- function(object, ...) {
  for (name in names(object)) {
    result <- object[[name]]
    cat(sprintf("%s:\n", name))
    cat(sprintf("Alpha Min: %f\n", result$Alpha_Min))
    cat(sprintf("Alpha Max: %f\n", result$Alpha_Max))
    cat(sprintf("Runtime: %f seconds\n\n", result$Runtime))
  }
}
summary <- function(object, ...) UseMethod("summary")
