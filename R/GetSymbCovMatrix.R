#' Create a symbolic covariance matrix
#'
#' @param blueprint A character matrix which specifies which effects to estimate and
#' which effects to constrain to a non-zero value
#'
#' @return A character matrix
#' @export
#'
#' @examples
#' blueprint <- matrix( c("ARx", "CLxy", "ARy", "0"), nrow = 2, ncol = 2 )
#' colnames(blueprint) <- c("X", "Y")
#' rownames(blueprint) <- c("X_0", "Y_0")
#' GetSymbCovMatrix(blueprint)

GetSymbCovMatrix <- function(blueprint){

  varNames <- colnames(blueprint)

  StCovMat <-  matrix(rep(0, length(varNames)^2),
                      nrow = length(varNames),
                      ncol = length(varNames))

  colnames(StCovMat) <- rownames(StCovMat) <- varNames


  for(i in 1:ncol(StCovMat)){

    for(j in 1:nrow(StCovMat)){

      if(colnames(StCovMat)[i] == rownames(StCovMat)[j]){
        StCovMat[j, i] <- paste0("Var", colnames(StCovMat)[i])
      } else {

        StCovMat[j, i] <- paste0("Cov", colnames(StCovMat)[i], rownames(StCovMat)[j])

      }

    }}

  StCovMat[upper.tri(StCovMat)] <- t(StCovMat)[upper.tri(StCovMat)]


  return(SymbCov = StCovMat)
}
