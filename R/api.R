
base_url <- "https://api.pinnaclesports.com"

authorization <-
  function (user = get_user(),pwd =get_pwd()){
    credentials = paste(user,pwd,sep=":")
    credentials.r = charToRaw(enc2utf8(x))
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
  function(force){
    if(length(.settings$sports)==0 || force){
      r <- GET(paste0(base_url,"/v1/sports"),
               add_headers("Authorization"= authorization())
      )
      content(r, "text")
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
    r <- GET(paste0(base_url,"/v1/leagues"),
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

