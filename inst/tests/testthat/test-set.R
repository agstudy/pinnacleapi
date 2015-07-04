context("Test set data in pinnaclesports")

test_that("PlaceBet  works fine",{

  res <- PlaceBet ()
  expect_is(res,"data.frame")

})
