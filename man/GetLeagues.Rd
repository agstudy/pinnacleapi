% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/api.R
\name{GetLeagues}
\alias{GetLeagues}
\title{Get leagues for a sportnames}
\usage{
GetLeagues(sports, force = FALSE, regex = FALSE)
}
\arguments{
\item{sports}{scalar or vector of characters of sports names.}

\item{force}{boolean if true retrieves all sports. Default use cache settings.}

\item{regex}{boolean if TRUE , retreives sports id using regular expression on names}
}
\value{
a data frame having columns:
\itemize{
\item League ID
\item Feed Contents
\item HomeTeamType
\item AllowRoundRobin
\item League Name
}
}
\description{
retrieves all sports leagues with the status whether they currently have lines or not.
}
\examples{
GetLeagues("Soccer")
GetLeagues(c("Soccer","Boxing"))
GetLeagues(c("soccer","box"),regex=TRUE)
}

