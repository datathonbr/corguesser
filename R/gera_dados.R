#' generate_one_set
#'
#' @param corr theorical correlation
#' @param n number of points
#' @param n_outliers number of outliers (extra noise)
#'
#' @export
#'
#' @example
#' library(tidyverse)

generate_one_set <- function(corr, n = 30, n_outliers = rpois(1, 6)){

  x_y <- mvtnorm::rmvnorm(n,
                          mean = c(0, 0),
                          sigma = matrix(c(1, corr, corr, 1), 2, 2))

  x_y <- (x_y - min(x_y))/(max(x_y) - min(x_y))

  outliers <- matrix(runif(2*n_outliers), ncol = 2)

  data <- rbind(x_y[seq_len(n - n_outliers), ], outliers)


  return(list(data = data,
              corr = cor(data[,1], data[,2])))
}


#' generate_several_sets
#'
#' @param n_sets
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
generate_n_sets <- function(n_sets, f = generate_one_set) {
  hashid_setting <- hashids::hashid_settings(as.character(runif(1)), min_length = 5 + sample(5, 1))
  db <- tibble(id = map_chr(1:n_sets, hashids::encode, settings = hashid_setting)) %>%
    mutate(corr_teorica = 2*runif(n()) - 1,
           data = map(corr_teorica, f),
           corr_observada = map_dbl(data, "corr"),
           data = map(data, "data"))
  return(db)
}

