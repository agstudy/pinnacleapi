context("Test retrieves data from pinnaclesports")


test_that("Test GetSports returns the good type",{

  res <- GetSports()
  expect_is(res,"data.frame")
  target.cols <- c("Sport ID", "Feed Contents" ,"Sport Name")
  expect_equal(names(res),target.cols)

})




test_that("Test GetCurrencies returns the good type",{

  res <- GetCurrencies()
  expect_is(res,"data.frame")
  target.cols <- c("Code", "Rate" ,"Name")
  expect_equal(names(res),target.cols)

})



test_that("Test GetLeaguesByID returns the good type",{

  res <- pinnacleAPI:::GetLeaguesByID(3)
  expect_is(res,"data.frame")
  target.cols <- c("League ID", "Feed Contents",
                   "HomeTeamType","AllowRoundRobin" ,"League Name")
  expect_equal(names(res),target.cols)

})

test_that("Test GetLeagues works fine with scalar",{

  res <- GetLeagues(c("Basketball"))
  expect_is(res,"data.frame")
  target.cols <- c("League ID", "Feed Contents",
                   "HomeTeamType","AllowRoundRobin" ,"League Name")
  expect_equal(names(res),target.cols)

})


test_that("Test GetLeagues returns the good type",{

  res <- GetLeagues(c("Basketball","Soccer"))
  res.basket <- GetLeagues("Basketball")
  res.socc <- GetLeagues("Soccer")
  expect_equal(nrow(res),nrow(res.basket)+nrow(res.socc))

})


test_that("Test GetLeagues works fine with regex",{

  target <- GetLeagues(c("Basketball","Soccer"))

  res <- GetLeagues(c("basket","socc"),regex=TRUE)


  res <- merge(target,res)

  expect_equivalent( res[order(res[,1]),],
                     target[order(target[,1]),])

})
test_that("GetFixtures works fine",{

  res <- GetFixtures(sportname="Soccer", leagueid=c(11,45),
                     since=26142345,isLive=0)
  expect_is(res,"data.frame")
  target.cols <-  c("SportID","Last",
                    "LeagueID","EventID","StartTime",
                    "HomeTeamName","AwayTeamName","Rotation Number",
                    "Live Status","Status" ,"Parlay Status")

  expect_equal(colnames(res),target.cols)

})

test_that("GetOdds  works fine",{

  res <- GetOdds (sportname="Soccer", leagueid=c(11,45),
                  since=26142345,isLive=0)
  expect_is(res,"data.frame")


})


test_that("GetClientBalance  works fine",{

  res <- GetClientBalance ()
  expect_is(res,"list")
  expect_equal(c("availableBalance", "outstandingTransactions", "givenCredit",
                 "currency"),
               names(res))

})



