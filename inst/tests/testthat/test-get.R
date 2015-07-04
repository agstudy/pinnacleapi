context("Test retrieves data from pinnaclesports")


test_that("Test GetSports returns the good type",{

  res <- GetSports()
  expect_is(res,"data.frame")
  target.cols <- c("Sport ID", "Feed Contents" ,"Sport Name")
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

  res <- GetLeagues(c("basket","socc"),regex=TRUE)
  target <- GetLeagues(c("Basketball","Soccer"))
  target <- res[order(target[,1]),]
  mm <- merge(res.target,res)
  result <- mm[order(mm[,1]),]

  expect_equivalent(result,target)

})
test_that("GetFixtures works fine",{

  res <- GetFixtures(portid=29, leagueid=c(11,45),
                     since=26142345,isLive=0)
  expect_is(res,"data.frame")
  target.cols <-  c("SportID","Last","League",
                    "LeagueID","EventID","StartTime",
                    "HomeTeamName","AwayTeamName","Rotation Number",
                    "Live Status","Status" ,"Parlay Status")

  expect_equal(colnames(res),target.cols)

})

test_that("GetOdds  works fine",{

  res <- GetOdds (sportid=29, leagueid=c(11,45),
                  since=26142345,isLive=0)
  expect_is(res,"data.frame")


})

test_that("GetCurrencies  works fine",{

  res <- GetCurrencies()
  expect_is(res,"data.frame")


})

test_that("GetClientBalance  works fine",{

  res <- GetClientBalance ()
  expect_is(res,"data.frame")

})



