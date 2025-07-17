#' Generate Bernoulli Distributed Scores Matrix with Missing Values
#'
#' This function generates a matrix of scores for a set of people and items,
#' where the scores are generated using a Bernoulli distribution with person-specific probabilities.
#' It also allows for some scores to be missing (represented by \code{NA}).
#'
#' @param n_person An integer representing the number of people (rows in the matrix).
#' @param n_item An integer representing the number of items (columns in the matrix).
#' @param n_missing An integer representing the number of missing scores (set to \code{NA} in the matrix).
#' @param score_max An integer representing the largest possible score for any item. Default is 1.
#'
#' @details
#' The function generates a score matrix where each person's score for each item is drawn
#' from a Bernoulli distribution with a person-specific probability. A number of scores are set to \code{NA}
#' to simulate missing values.
#'
#' The probability of each person scoring on the items is determined by randomly generating a
#' probability for each person using \code{runif}. The Bernoulli distribution is then used
#' (via \code{rbinom}) to generate the scores, and \code{NA} values are assigned to randomly selected positions
#' in the matrix based on \code{n_missing}.
#'
#' @return A matrix of size \code{n_person} by \code{n_item} containing generated scores (0 or \code{score_max})
#' with some values replaced by \code{NA} to simulate missing data.
#'
#' @examples
#' # Generate a 10x5 score matrix with 10 missing values and maximum score of 1
#' scores_mat <- generate_scores_mat_bernoulli(10, 5, 10, score_max = 1)
#' print(scores_mat)
#'
#' @import stats
#' @export


generate_scores_mat_bernoulli <- function(
    n_person,  # The number of all people
    n_item,    # The number of all items
    n_missing, # The number of missing results (or scores)
    score_max = 1  # The largest possible score
) {

  scores_mat <- matrix(0, nrow = n_person, ncol = n_item)

  scores_person <- runif(n_person)

  missing_count <- sample(n_person * n_item, n_missing, replace = FALSE)

  count <- 0
  for (i in 1:n_person) {
    for (j in 1:n_item) {
      count <- count + 1
      if (count %in% missing_count) {
        scores_mat[i, j] <- NA
      } else {
        scores_mat[i, j] <- rbinom(1, size = score_max, prob = scores_person[i])
      }
    }
  }

  return(scores_mat)
}
