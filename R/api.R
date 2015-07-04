


authorization <-
  function (user = get_user(),pwd =get_pwd()){
    credentials = paste(user,pwd,sep=":")
    credentials.r = charToRaw(enc2utf8(credentials))
    paste0("Basic ", base64Encode(credentials.r, "character"))

  }

#' Get Sports
#'
#' Returns all sports with the status whether they currently have lines or not
#'
#' @param force boolean if false use cached data.
#' @return  a data frame having columns:
#' \itemize{
#' \item Sport ID
#' \item  Feed Contents
#' \item Sport Name
#' }
#'
#' @import httr
#' @import XML
#' @importFrom RCurl base64Encode
#' @export
#'
#' @examples
#' GetSports()
GetSports <-
  function(force=FALSE){
    if(length(.settings$sports)==0 || force){
      r <- GET(paste0(.settings$url ,"/v1/sports"),
               add_headers("Authorization"= authorization())
      )
      dc <- xmlParse(content(r, "text"))
      xml_path <- "/rsp/sports/sport"
      .settings$sports <-
        data.frame("Sport ID"=xpathSApply(dc,xml_path,xmlGetAttr,"id"),
                   "Feed Contents"=xpathSApply(dc,xml_path,xmlGetAttr,"feedContents"),
                   "Sport Name" = xpathSApply(dc,xml_path,xmlValue),
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    }

    .settings$sports
  }



#' Get the list of supported currencies
#'
#' @param force boolean if false use cached data.
#' @return  a data frame having columns:
#' \itemize{
#' \item Code
#' \item Rate
#' \item Name
#' }
#'
#' @export
#'
#' @examples
#' GetCurrencies()
GetCurrencies <-
  function(force=FALSE){
    if(length(.settings$currencies)==0 || force){
      r <- GET(paste0(.settings$url ,"/v1/currencies"),
               add_headers("Authorization"= authorization())
      )
      dc <- xmlParse(content(r, "text"))
      xml_path <- "/rsp/currencies/currency"
      .settings$currencies <-
        data.frame("Code"=xpathSApply(dc,xml_path,xmlGetAttr,"code"),
                   "Rate"=xpathSApply(dc,xml_path,xmlGetAttr,"rate"),
                   "Name" = xpathSApply(dc,xml_path,xmlValue),
                   check.names = FALSE,
                   stringsAsFactors = FALSE)
    }

    .settings$currencies
  }


#' Get leagues for a sportid
#'
#' retrieves all sports leagues with the status whether they currently have lines or not.
#'
#' @param sportid integer the id of the sport
#'
#' @return  a data frame having columns:
#' \itemize{
#' \item League ID
#' \item  Feed Contents
#' \item HomeTeamType
#' \item AllowRoundRobin
#' \item League Name
#' }
#'
GetLeaguesByID <-
  function(sportid){
    r <- GET(paste0(.settings$url ,"/v1/leagues"),
             add_headers("Authorization"= authorization()),
             query = list(sportid=sportid)
    )
    content(r, "text")
    dc <- xmlParse(content(r, "text"))
    xml_path <- "/rsp/leagues/league"
    data.frame("League ID"= xpathSApply(dc,xml_path,xmlGetAttr,"id"),
               "Feed Contents"   = xpathSApply(dc,xml_path,xmlGetAttr,"feedContents"),
               "HomeTeamType"= xpathSApply(dc,xml_path,xmlGetAttr,"homeTeamType"),
               "AllowRoundRobin"   = xpathSApply(dc,xml_path,xmlGetAttr,"allowRoundRobins"),
               "League Name" = xpathSApply(dc,xml_path,xmlValue),
               check.names = FALSE,
               stringsAsFactors = FALSE)
  }

#' Get leagues for a sportnames
#'
#' retrieves all sports leagues with the status whether they currently have lines or not.
#'
#' @param sports scalar or vector of characters of sports names.
#' @param force boolean if true retrieves all sports. Default use cache settings.
#' @param regex boolean if TRUE , retreives sports id using regular expression on names
#'
#' @return a data frame having columns:
#' \itemize{
#' \item League ID
#' \item Feed Contents
#' \item HomeTeamType
#' \item AllowRoundRobin
#' \item League Name
#' }
#'@export
#'@examples
#'
#'GetLeagues("Soccer")
#'GetLeagues(c("Soccer","Boxing"))
#'GetLeagues(c("soccer","box"),regex=TRUE)
#'
#'
GetLeagues <-
  function(sports,force=FALSE,regex=FALSE){
    ## this is called once
    sports.all <- GetSports(force)
    ids <- sports.all[,"Sport ID"]
    ids.serach <- if(!regex)
      match(sports,sports.all[,"Sport Name"])
    else {
      patt <- paste(tolower(sports),collapse='|')
      ids[grepl(patt,tolower(sports.all[,"Sport Name"]))]
    }
    do.call(rbind,
            lapply(ids.serach,GetLeaguesByID))

  }



#' Get Fixtures
#'
#' @param sportname The sport name for which to retrieve the fixutres
#' @param leagueids vector of characters.
#' @param since numeric This is used to receive incremental updates.
#' Use the value of last from previous fixtures response.
#' @param isLive boolean if TRUE retrieves ONLY live events
#'
#' @return returns a data frame with columns:
#' \itemize{
#' \item SportID
#' \item Last
#' \item League
#' \item LeagueID
#' \item EventID
#' \item StartTime
#' \item HomeTeamName
#' \item AwayTeamName
#' \item Rotation Number
#' \item Live Status
#' \item Status
#' \item Parlay Status
#' }
#'
#' @import jsonlite
#' @export
#'
#' @examples
#'  GetFixtures(sportname="Soccer", leagueid=c(11,45),since=26142345,isLive=TRUE)
#'

GetFixtures <-
  function(sportname,leagueids,since,isLive){

    ## retrieve sportid
    if (missing(sportname))
      stop("You should prove a sport name as a character")
    ## TODO : serach by sport name as a regex
    sportid <- GetSports(F)[,"Sport ID"][sportname== GetSports(F)[,"Sport Name"]]
    ##
    if(missing(leagueids))
      leagueids <- GetLeaguesByID(sportid)

    r <- GET(paste0(.settings$url ,"/v1/fixtures"),
             add_headers(Authorization= authorization(),
                         "Content-Type" = "application/json"),
             query = list(sportid=sportid,
                          leagueid = paste(leagueids,collapse=','),
                          since=since,
                          isLive=isLive*1))
    res <-  fromJSON(content(r,type="text"))

    out <- cbind(res$sportId,
                 res$last,
                  do.call(rbind,
                          Map(function(id,events)data.frame(id,events) ,
                              res$league$id,res$league$events)))

    colnames(out) <- c("SportID","Last","LeagueID","EventID",
                       "StartTime","HomeTeamName","AwayTeamName",
                   "Rotation Number","Live Status","Status","Parlay Status")
   out

  }


#' Get Client Balance Response
#'
#' @param force boolean if TRUE relaod the data from the site otherwise use the cache
#'
#' @return anmed vector client balance parameter
#' @export
#'
#' @examples
#' GetClientBalance()
GetClientBalance <- function(force=FALSE){

  if(length(.settings$ClientBalance)==0 || force){
    r <- GET(paste0(.settings$url ,"/v1/client/balance"),
             add_headers("Authorization"= authorization(),
                         "Content-Type" = "application/json")
    )
    .settings$ClientBalance <- fromJSON(content(r, "text"))
  }
  .settings$ClientBalance
}


#' Get Odds
#'
#' @param sportname The sport name for which to retrieve the fixutres
#' @param leagueids vector of characters.
#' @param since numeric This is used to receive incremental updates.
#' Use the value of last from previous fixtures response.
#' @param isLive boolean if TRUE retrieves ONLY live events
#'
#' @return list of lists
#' @export
#'
#' @examples
#' GetOdds (sportname="Soccer", leagueid=c(11,45),since=26142345,isLive=0)
#'

GetOdds <-
  function(sportname,leagueids,since,isLive){
    ## retrieve sportid
    if (missing(sportname))
      stop("You should prove a sport name as a character")
    ## TODO : serach by sport name as a regex
    sportid <- GetSports(F)[,"Sport ID"][sportname== GetSports(F)[,"Sport Name"]]
    ##
    if(missing(leagueids))
      leagueids <- GetLeaguesByID(sportid)

    r <- GET(paste0(.settings$url ,"/v1/odds"),
             add_headers(Authorization= authorization(),
                         "Content-Type" = "application/json"),
             query = list(sportid=sportid,
                          leagueid = paste(leagueids,collapse=','),
                          since=since,
                          isLive=isLive*1))
    fromJSON(content(r,type="text"))
#
#     res <- unlist(res,recursive = FALSE)
#     events = unlist(res$leagues.events,recursive = FALSE)
#
#     data.frame (sportId= res$sportId,
#                 last= res$last,
#                 leagueids = res$leagues.id
#     )

  }





#' Place Bet
#'
#' Place bet in the system
#'
#' @param acceptBetterLine boolean Whether or not to accept a bet
#'  when there is a line change in favor of the client
#' @param oddsFormat character Bet is processed with this odds format
#' it takes values in :
#' \itemize{
#' \item{AMERICAN}
#' \item{DECIMAL}
#' \item{HONGKONG}
#' \item{INDONESIAN}
#' \item{MALAY}
#' }
#' @param stake numeric Wagered amount in Client’s currency
#' @param winRiskStake WIN_RISK_TYPE Whether the stake amount is risk or win amount
#' \itemize{
#' \item{WIN}
#' \item{RISK}
#' }
#' @param sportId numeric the sport id
#' @param eventId numeric the vent id
#' @param periodNumber This represents the period of the match. For example
#' For soccer : #' 0-->Game 1-->1st Half 2-->2nd Half
#' @param betType BET_TYPE
#' \itemize{
#' \item{SPREAD	}
#' \item{MONEYLINE}
#' \item{TOTAL_POINTS}
#' \item{TEAM_TOTAL_POINTS}
#' }
#' @param lineId numeric Line identification
#'
#' @return list containing :
#' \itemize{
#'   \item{status}  If Status is PROCESSED_WITH_ERROR errorCode will be in the response
#'   \item{errorCode}
#' }
#' @export
#'
#' @examples
#' PlaceBet (acceptBetterLine=TRUE,oddsFormat="AMERICAN", stake=10,
#'           winRiskStake="WIN",sportId=29,eventId=307962592,
#'            periodNumber=0,betType="MONEYLINE",lineId=103648474)

PlaceBet <-
  function(acceptBetterLine,
           oddsFormat,
           stake,
           winRiskStake,
           sportId,
           eventId,
           periodNumber,
           betType,
           lineId){

    r <- POST(paste0(.settings$url ,"/v1/odds"),
             add_headers(Authorization= authorization(),
                         "Content-Type" = "application/json"),
             query = list(uniqueRequestId=UUIDgenerate(),
                          acceptBetterLine=acceptBetterLine,
                          oddsFormat=oddsFormat,
                          stake=stake,
                          winRiskStake=winRiskStake,
                          sportId=sportId,
                          eventId=eventId,
                          periodNumber=periodNumber,
                          betType=betType,
                          lineId=lineId)
    )
    fromJSON(content(r,type="text"))

  }

