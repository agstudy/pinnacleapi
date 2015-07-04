context("Test set data in pinnaclesports")

test_that("PlaceBet  works fine",{

  res <- PlaceBet (acceptBetterLine=TRUE,oddsFormat="AMERICAN", stake=10,
               winRiskStake="WIN",sportId=29,eventId=307962592,
              periodNumber=0,betType="MONEYLINE",lineId=103648474)
  expect_is(res,"list")
  expect_true(res$ACCEPTED)

})
