#' @import purrr
#' @import future
#' @import furrr
#' @import stats
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))

#' main function to run linear regression with mini bag bootstrap
#'
#' @param formula an object with class "formula"
#' @param data a data frame or a vector of filenames
#' @param m how many times we split the data
#' @param B number of bootstrap performed
#' @param cluster_size number of clusters
#'
#' @return blblm object
#'
#' @export
#'
blblm <- function(formula, data, m = 10, B = 5000, cluster_size = 1) {
  if (is.data.frame(data)) {
      data_list <- split_data(data, m)
      if (cluster_size == 1) {
          estimates <- map(data_list, ~ lm_each_subsample(
                                                           formula = formula,
                                                           data = .,
                                                           n = nrow(data),
                                                           B = B
          ))
    } else{
      suppressWarnings(plan(multiprocess, workers = cluster_size))
      estimates <- future_map(data_list,
                              ~ lm_each_subsample(
                                formula = formula,
                                data = .,
                                n = nrow(data),
                                B = B
                              ))
    }
  } else{
    if (cluster_size == 1) {
          num_rows = data %>% map( ~ {
                                         df <- read.csv(., )
                                         nrow(df)
                          }) %>% reduce(`+`)
            estimates = data %>% map( ~ {
                                          df <- read.csv(., )
                                          lm_each_subsample(
                                            formula = formula,
                                            data = df,
                                            n = num_rows,
                                            B = B
                                          )
      })
    } else{
      suppressWarnings(plan(multiprocess, workers = cluster_size))
      num_rows = data %>% map(~ {
                                    df <- read.csv(.,)
                                    nrow(df)
                             }) %>% reduce(`+`)
      estimates = data %>% future_map(~ {
                                            df <- read.csv(.,)
                                            lm_each_subsample(
                                              formula = formula,
                                              data = df,
                                              n = num_rows,
                                              B = B
                                            )
      })
    }
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' function to split data to m almost equal size data
#'
#' @param data a data frame
#' @param m number of times to split data
#'
#' @export
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}

#' linear regression on each subsample
#'
#' @param formula a formula class object
#' @param data a data frame
#' @param n sample size
#' @param B number of bootstrap
#' @export
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}

#' linear regression for each bootstrap
#'
#' @param formula a formula class
#' @param data a data frame
#' @param n sample size
#'
#' @export
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}

#' run regression model
#'
#' @param formula a formula class
#' @param data a data frame
#' @param freqs number of times each rows are reselected in bootstrap
#'
#' @export
lm1 <- function(formula, data, freqs) {
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

#' obtain coefficients
#'
#' @param fit fitted regression
#'
#' @export
blbcoef <- function(fit) {
  coef(fit)
}

#' obtain blblm sigma
#'
#' @param fit fitted regression
#'
#' @export
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' print for blblm class
#'
#' @param x blblm object
#' @param ... more arguments
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

##################

#' obtain sigma
#'
#' @param object blblm class
#' @param confidence need confidence interval or not
#' @param level 1-alpha, confidence level
#' @param ... more arguments
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' obtain coefficients
#'
#' @param object blblm object
#' @param ... more argument
#'
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

#' obtain confidence interval
#'
#' @param object blblm object
#' @param parm parms
#' @param level 1-alpha, confidence level
#' @param ... more argument
#'
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' prediciton for blbm class
#'
#' @param object blblm object
#' @param new_data a new data frame
#' @param confidence need confidence interval or not
#' @param level 1-alpha, confidence level
#' @param ... more argument
#'
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}

mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
