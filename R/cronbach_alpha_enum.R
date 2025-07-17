#' Compute Exact Bounds of Cronbach's Alpha via Enumeration
#'
#' This function computes the minimum and maximum possible values of Cronbach's alpha
#' by enumerating all possible values for unknown entries in the score matrix.
#'
#' @param scores_mat A matrix where rows represent individuals and columns represent test items.
#'   It contains the performance of individuals on different test items, with NA for missing values.
#' @param score_max An integer specifying the largest possible score for any test item.
#'
#' @details
#' This function works by enumerating all possible combinations of values for missing entries (represented by NA) in the \code{scores_mat}.
#' It systematically explores all combinations of missing values from \code{0} to \code{score_max} using the \code{expand.grid} function.
#' For each combination, it calculates Cronbach's alpha using the \code{compute_cronbach_alpha} function and keeps track of the minimum and maximum alpha values encountered.
#'
#' The enumeration ensures that the function finds the exact minimum and maximum possible values of Cronbach's alpha given the possible missing score combinations.
#' However, due to the exhaustive nature of enumeration, this function may become computationally expensive for large datasets or a high number of missing values.
#'
#' @return A numeric vector of length 2, where the first element is the minimum Cronbach's alpha and the second element is the maximum Cronbach's alpha.
#'
#' @seealso \code{\link{cronbach_alpha_rough}}
#' @export

cronbach_alpha_enum <- function(
    scores_mat,
    score_max
) {
  tryCatch({
    unknown_info <- unknown_loc(scores_mat)
    unknown_count <- unknown_info[[1]]

    alpha_min <- 100.0
    alpha_max <- -100.0

    # Function to generate all combinations manually
    generate_combinations <- function(n, max_val) {
      if (n == 0) {
        return(matrix(numeric(0), nrow = 1))
      }
      combinations <- matrix(0:max_val, ncol = 1)
      for (i in 2:n) {
        new_combinations <- c()
        for (row in 1:nrow(combinations)) {
          for (val in 0:max_val) {
            new_combinations <- rbind(new_combinations, c(combinations[row, ], val))
          }
        }
        combinations <- new_combinations
      }
      return(combinations)
    }

    # Replace expand.grid with the manual implementation
    possible_scores <- tryCatch({
      generate_combinations(unknown_count, score_max)
    }, error = function(e) {
      warning("Fallback to manual combination generation failed.")
      stop("Error generating score combinations.")
    })

    for (i in 1:nrow(possible_scores)) {
      scores <- as.numeric(possible_scores[i, ])
      alpha <- compute_cronbach_alpha(scores_mat, unknown_info, scores)
      if (alpha < alpha_min) {
        alpha_min <- alpha
      }
      if (alpha > alpha_max) {
        alpha_max <- alpha
      }
    }
    return(c(alpha_min, alpha_max))
  }, error = function(e) {
    return(c(NA, NA))
  })
}
