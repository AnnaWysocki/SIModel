#' Estimate a Stability Informed Model
#'
#' @param data A dataframe with the measured variables. Not needed if S is provided
#' @param S A covariance matrix for the measured variables. Not needed if data is provided.
#' @param n Number of observations. Not needed if data is provided.
#' @param effects A data frame that contains information on which effects to
#'                estimate or constrain to a value other than zero. Each row
#'                represents one effect. The `effects` object  must have three
#'                columns: column 1 has the variable names for the predictors,
#'                column 2 has the variable names for the outcomes, and column
#'                3 has the names of the effects (if the effect should be estimated)
#'                or the numeric value the effect should be set at (if the effect
#'                is not to be estimated but rather to be set to a value other
#'                than zero).
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
#' effects <- data.frame(predictor = "X", outcome = "Y", name = "CLxy")
#' stability <- data.frame(X = .3, Y = .3)
#' dat <- data.frame(Y = rnorm(500, 0, 1), X = rnorm(500, 0, 1), Z = rnorm(500, 0, 1))
#'
#' SIM(data = dat, effects = effects, stability = stability)


SIM <- function(data = NULL, S = NULL, n = NULL,
                    effects, stability){

  # To dos...
  #############################
  # want to be able to set non-estimated parameters to something other than 0
  # want to add the ability to run a sensitivity check by having multiple
  # potential stabilities
  # Check if number of parameters specified to estimate fits with df
  # Allow residuals to correlate


  ####################
  ##  Check inputs  ##
  ####################

  # What needs to be true for each of these inputs

  # dat:       needs to be a dataframe...
  #            needs to have the same labels as the stability and effect labels

  # S:         needs to be a matrix
  #            must be symmetric
  #            must have n specified
  #            needs to have the same labels as the stability and effect labels

  # n:         must be specified if data is not provided
  #            must be numeric

  # effects:   needs to have three columns
  #            needs to have specific columns names (or change)
  #            must be a dataframe
  #            needs to have the same labels as the data/cov labels

  # stability: must be a dataframe
  #            must have stability for each variable
  #            must be named
  #            must be numeric values


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

  # Checks for effects input
  stopifnot("`effects` must be a dataframe " = is.data.frame(effects))

  if( ncol(effects) != 3) stop("The `effects` object needs 3 columns: predictor, outcome, and name")

  if(!setequal(colnames(effects), c("predictor", "outcome", "name"))){

    colnames(effects) <- c("predictor", "outcome", "name")

    warning("Column names for effects object were renamed.
             Effect input should have the column names 'predictor', 'outcome', and 'name'.")
  }


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

  p <- length(use)

  effects$predictor <- paste0(effects$predictor, "_0")



  ################################################
  ##         Create a character matrix          ##
  ##  that specifies which effects to estimate  ##
  ##          and which to constrain            ##
  ################################################

  blueprint <- CreateBlueprint(effects, use)

  ArEquations <- GetModelImpEquations(S, blueprint, stability)

  LavaanSyntax <- GetLavaanEquations(blueprint, S)
  LavaanSyntax <- c(LavaanSyntax, ArEquations)


  modelFit <- lavaan::sem(LavaanSyntax, sample.cov = S, sample.nobs= n,
                          std.lv = TRUE)

  # Things to return in object, Lavaan syntax, Lavaan object,
  #                             results df, plot, any lavaan warnings...



  return(modelFit)


}
