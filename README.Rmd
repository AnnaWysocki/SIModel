---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```

# SIModel

The **SIModel** package provides users with the ability to estimate stability-informed models (SIM). These models can be used when a researcher is interested in a longtiudinal estimate but only has cross-sectional data available to them. 
Stability-informed models allow for the integration of existing longitudinal information with cross-sectional estimates. 
The Stability Informed Model is a estimated in a SEM framework, and the **SIModel** package uses the **lavaan** package. Here, previous versions of the measured variables are 
specified as latent (or phantom) variables that have paths to the set of measured variables. We then set a series of constraints which allows cross-lagged effects to be estimated.  

## Installation

You can install the development version of **SIModels** from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/AnnaWysocki/SIModel")
```

## Example

Cross-validating the Holzingerswineford1939 dataset

Load package and read in data from the **lavaan** package:
```{r example}
library(SIModel)
example_data <- lavaan::HolzingerSwineford1939
```

Add column names
```{r}
colnames(example_data) <- c("id", "sex", "ageyr", "agemo", "school", "grade",
                            "visualPerception", "cubes", "lozenges", "comprehension",
                            "sentenceCompletion", "wordMeaning", "speededAddition",
                            "speededCounting", "speededDiscrimination")
```

## Define Models

We want to estimate the longitudinal effect of sentenceCompletion and wordMeaning on comprehension. 
Let's imagine that we got 1 week-lag stability values (i.e., corrleation between a variable and the future version of itself) for each of the variables we want to include in our model. 

```{r}
stability <- data.frame(sentenceCompletion = .3, wordMeaning = .4, comprehension = .5)
```

We can define the cross-sectional model using `lavaan` notation:
```{r}
model1 <- 'comprehension ~ sentenceCompletion + wordMeaning' # if we want to estimate two cross-lagged effects
model2 <- 'comprehension ~ sentenceCompletion + .3 * wordMeaning' # estimate one cross-lagged effect and constrain the other to .3

model3 <- 'comprehension ~ sentenceCompletion + wordMeaning 
           sentenceCompletion ~~  wordMeaning'  # estimate two cross-lagged effects and allow the residual between sentenceCompletion and wordMeaning to covary
```

## Estimate the stability-informed model
Define number of folds `k` and call `cvsem` function. 
Here we use `k=10` folds. CV is based on the discrepancy between test sample covariance matrix and the model implied matrix from the training data. The discrepancy among sample and implied matrix is defined in `discrepancyMetric`.
Currently three discrepancy metrics are available: `KL-Divergence`, Generalized Least Squares `GLS`, and Frobenius Distance `FD`.
Here we use `KL-Divergence`. 
```{r}

# two ways to use this function
# 1. with data input
fit1 <- SIM(data = example_data, model = model1, stability = stability)

#2. with covariance matrix and sample size input
fit1 <- SIM(S = cov(example_data), n = nrow(example_data) , model = model1, stability = stability)
```

## Show Results

```{r}
fit1$ResultsMatrix
```

