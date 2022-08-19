#' Estimate a Stability Informed Model
#'
#' @param data A dataframe with the measured variables. Not needed if S is provided
#' @param S A covariance matrix for the measured variables. Not needed if data is provided.
#' @param n Number of observations. Not needed if data is provided.
#' @param model An object with the cross-sectional model description in lavaan syntax
#' @param stability A data frame that contains stability information for each
#'                  variable in the model. If unnamed SIM will assume the stability
#'                  values are in the same order as the provided data set/
#'                  covariance matrix.
#'
#' @return An object of class SIM
#' @export
#'
#' @examples
#'
#' model <- 'Y~X'
#' stability <- data.frame(X = .3, Y = .3)
#' dat <- data.frame(Y = rnorm(500, 0, 1), X = rnorm(500, 0, 1), Z = rnorm(500, 0, 1))
#'
#' SIM(data = dat, model = model, stability = stability)


SIM <- function(data = NULL, S = NULL, n = NULL,
                    model, stability){

  # To dos...
  #############################
  # Allow residuals to correlate


  ####################
  ##  Check inputs  ##
  ####################


  # Checks for data/covariance input

  if( !is.null(S) & is.null(n) ) stop("Sample size must be provided if a covariance matrix is used as the data input.")

  if( is.null(S) & is.null(data) ) stop("Input needed for either `dat` or `S` argument.")


  # Checks for data input
  if( !is.null(data)){

    stopifnot("`data` must be a dataframe " = is.data.frame(data))

    n <- nrow(data)
    S <- stats::cov(data)

  }

  # Checks for S input
  if( !is.null(S)){

    stopifnot("`S` must be a matrix" = is.matrix(S))

    stopifnot("`S` must be symmetric" = isSymmetric(S))

  }

  # Checks for n input
  if( !is.null(n)){

    stopifnot("`n` must be numeric" = is.numeric(n))

  }

  # Checks for model input
  stopifnot("`model` must be a character element " = is.character(model))

  effects <- CreateEffectTable(model)


  # Create list of variables to use
  use <- unique(c(effects$predictor, effects$outcome))
  use <- use[order(match(use, colnames(S)))]

  if( any(is.na(match(use, colnames(S)))) == TRUE ) {

    stop("Variable names in the effects object do not match the variable
          names in the dataset/covariance matrix")

  }


  # Checks for stability input
  stopifnot("`stability` must be a dataframe " = is.data.frame(stability))

  if( any(is.na(match(colnames(stability), use))) == TRUE ) {

    stop("The stability object does not have a stability value for each
          variable")

  }

  ################################
  ##  Check Degrees of Freedom  ##
  ################################

  p <- length(use)
  df <- (p * (p-1)) /2

  q <- suppressWarnings(sum(is.na(as.numeric(effects$name))))

  if (q > df ) stop("The number of specified parameters to estimate are greater than the degrees of freedom.")

  #################
  ##  Run Model  ##
  #################

  effects$predictor <- paste0(effects$predictor, "_0")

  ModelResults <- list()

  blueprint <- CreateBlueprint(effects, use)

  for(i in 1: nrow(stability)){

  stabilityIndex <- stability[i, ]

  ArEquations <- GetModelImpEquations(S, blueprint, stabilityIndex)

  LavaanSyntax <- GetLavaanEquations(blueprint, S)
  LavaanSyntax <- c(LavaanSyntax, ArEquations)

  if( nrow(stability) == 1 ){

    ModelResults <- lavaan::sem(LavaanSyntax, sample.cov = S, sample.nobs= n,
                                std.lv = TRUE)

  } else {

  ModelResults[[i]] <- lavaan::sem(LavaanSyntax, sample.cov = S, sample.nobs= n,
                          std.lv = TRUE)
  }

  }

  # Things to return in object, Lavaan syntax, Lavaan object,
  #                             results df, plot, any lavaan warnings...





  return(ModelResults)


}
