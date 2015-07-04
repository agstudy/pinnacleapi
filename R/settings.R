

.settings <- new.env()
.settings$url <- "https://api.pinnaclesports.com"

.settings$credentials <- list(
  user = "TESTAPI",
  pwd ="test123!"
)


#' Gets API user
#'
#' @return charcater api user
#' @export
get_user <- function() .settings$credentials$user

#' Sets API user
#' @param the new user value
#' @return nothing
#' @export
set_user <- function(value) .settings$credentials$user <- value

#' Gets API pwd
#'
#' @return character api pwd
#' @export
get_pwd  <- function() .settings$credentials$pwd

#' Sets API pwd
#'
#' @return Nothing
#' @export
set_pwd  <- function(value) .settings$credentials$pwd <- value
