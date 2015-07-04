


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

    r <- GET(paste0(.settings$url ,"/v1/leagues"),
             add_headers(Authorization= authorization(),
                         "Content-Type" = "application/json"),
             query = list(sportid=sportid,
                          leagueid = paste(leagueids,collapse=','),
                          since=since,
                          isLive=isLive*1))

    fromJSON(content(r))

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
