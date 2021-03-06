% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/api.R
\name{GetFixtures}
\alias{GetFixtures}
\title{Get Fixtures}
\usage{
GetFixtures(sportname, leagueids, since, isLive)
}
\arguments{
\item{sportname}{The sport name for which to retrieve the fixutres}

\item{leagueids}{vector of characters.}

\item{since}{numeric This is used to receive incremental updates.
Use the value of last from previous fixtures response.}

\item{isLive}{boolean if TRUE retrieves ONLY live events}
}
\value{
returns a data frame with columns:
\itemize{
\item SportID
\item Last
\item League
\item LeagueID
\item EventID
\item StartTime
\item HomeTeamName
\item AwayTeamName
\item Rotation Number
\item Live Status
\item Status
\item Parlay Status
}
}
\description{
Get Fixtures
}
\examples{
GetFixtures(sportname="Soccer", leagueid=c(11,45),since=26142345,isLive=TRUE)
}

