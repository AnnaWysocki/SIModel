effects1 <- data.frame(predictor = "X_0", outcome = "Y", name = "CLxy")
use1 <- c("X", "Y")
bp1 <- matrix(c("ARX", "0", "CLxy", "ARY"), ncol = 2)
colnames(bp1) <- c("X", "Y")
rownames(bp1) <- c("X_0", "Y_0")


effects2 <- data.frame(predictor = "X_0", outcome = "Y", name = "CLxy")
use2 <- c("Y", "X")
bp2 <- matrix(c("ARY", "CLxy", "0", "ARX"), ncol = 2)
colnames(bp2) <- c("Y", "X")
rownames(bp2) <- c("Y_0", "X_0")


effects3 <- data.frame(predictor = c("X_0", "Y_0"), outcome = c("Y", "Z"),
                       name = c("CLxy", "CLyz"))
use3 <- c("X", "Y", "Z")
bp3 <- matrix(c("ARX", "0", "0", "CLxy", "ARY", "0",
                "0", "CLyz", "ARZ"), ncol = 3)
colnames(bp3) <- c("X", "Y", "Z")
rownames(bp3) <- c("X_0", "Y_0", "Z_0")

test_that("CreateBlueprint function exports a matrix", {
  expect_equal(is.matrix(CreateBlueprint(effects = effects1, use = use1)), TRUE)
  expect_equal(is.matrix(CreateBlueprint(effects = effects3, use = use3)), TRUE)
})

test_that("CreateBlueprint has right number of variables", {
  expect_equal(ncol(CreateBlueprint(effects = effects1, use = use1)), length(use1))
  expect_equal(ncol(CreateBlueprint(effects = effects3, use = use3)), length(use3))
})

test_that("The order of the variables in the input change the CreateBlueprint matrix", {
  expect_equal(CreateBlueprint(effects = effects1, use = use1), bp1)
  expect_equal(CreateBlueprint(effects = effects2, use = use2), bp2)
})

test_that("CreateBluerprint function works with 3 variables", {
  expect_equal(CreateBlueprint(effects = effects3, use = use3), bp3)
})
