CreateResultMatrix <- function(modelFit, effects, stability){

  q <- sum(effects$estimate == "Yes")

  ResultMatrix <- as.data.frame(matrix( 0, nrow = 1, ncol = q + length(stability) * 2))

  for(i in 1:length(modelFit)){

    stabilityIndex <- stability[i, ]


    lavaanLambda <- lavaan::inspect(modelFit[[i]], what = "std")$lambda

    AReffects <- diag(lavaanLambda)

    CLeffects <- rep(0, q)
    CL_name <- rep(0, q)

    for(j in 1:nrow(effects)) {

      if( effects$estimate[j] == "Yes" ){

        lavaanTable <- lavaan::parameterestimates(modelFit[[i]])

        CLeffects[j] <- lavaanTable[which(lavaanTable$label == effects$name[j]), "est"]
        CL_name[j] <- effects$name[j]
      }
    }

    ResultMatrix[i, ] <- unlist(c(stabilityIndex, AReffects, CLeffects))
  }

  colnames(ResultMatrix) <- c(paste0("Stability", colnames(stability)),
                              paste0("AR", rownames(lavaanLambda)),
                              CL_name)

  ResultMatrix <- data.frame(Model = paste0("Model ", 1:nrow(ResultMatrix)), ResultMatrix)

  return(ResultMatrix)
}
