#' Internal function used to do symbolic matrix multiplication using the Ryacas
#' R package
#'
#' @importFrom Ryacas %*%
#' @name %*%
#' @rdname SymbolicMultiplication
#' @keywords internal
#' @param x1 A Ryacas object.
#' @param x2 A Ryacas object.
#' @examples
#' \dontrun{
#' blueprint <- matrix( c("ARx", "CLxy", "ARy", "0"), nrow = 2, ncol = 2 )
#'
#' SymbolicCovMat <- matrix( c("VarX", "CovXY", "CovXY", "VarY"), ncol = 2, nrow = 2)
#'
#' B <- Ryacas::ysym(blueprint)
#' Cov1 <- Ryacas::ysym(SymbolicCovMat)
#'
#' SymbMultiplication(x1 = t(B), x2= Cov1)
#' }
SymbMultiplication <- function(x1, x2) {
  x1 %*% x2
}
