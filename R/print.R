print.SIModel <- function(out, digits = 2){


  printMatrix <- out$ResultMatrix
  printMatrix[, 2:6] <- round(printMatrix[,2:6],  digits = digits )
  if( any(out$NoWarnings == FALSE) ){
    warning("One or more of the lavaan models produced an error or warning")
  }

  cat('Stability-Informed Results for', nrow(printMatrix), 'Models \n')
  print(printMatrix)

}
