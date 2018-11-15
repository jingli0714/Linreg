#' Linear regression
#'
#' Runs an OLS regression not unlike \code{\link{lm}}
#'
#' @param x covariate matrix (p x n) with no intercept
#' @param y response vector (1 x n)
#'
#'
#' @return A list with 4 elements: coefficients, vcov, sigma, df
#'
#' @examples
#' data(mtcars)
#' X <- as.matrix(mtcars[, c("cyl", "disp", "hp")])
#' y <- mtcars[, "mpg"]
#' linmodEst(y, X)
#'
#' @export
#'

# # If not already run when creating package with RStudio:
# # git init
# git add *
# git commit -m "First commit"
# # Replace cchoirat with your GitHub username
# git remote add origin https://github.com/cchoirat/Linreg
# git push -u origin master

linmodEst <- function(x, y) {
  coef <- solve(t(x) %*% x) %*% t(x) %*% y
  ## degrees of freedom and standard deviation of residuals
  df <- nrow(x) - ncol(x)
  sigma2 <- sum((y - x %*% coef) ^ 2) / df
  ## compute sigma^2 * (x’x)^-1
  vcov <- sigma2 * solve(t(x) %*% x)
  colnames(vcov) <- rownames(vcov) <- colnames(x)
  list(
    coefficients = coef,
    vcov = vcov,
    sigma = sqrt(sigma2),
    df = df
  )
}
