# pinnacleapi

 Get and Post data via the [pinnaclesports.com API](http://www.pinnaclesports.com/fr/api/manual) . This R package makes it easy to interact with this API.
 
 
      library(pinnacleAPI)
      ## set user/pwd this should be called only at initialisation
      set_ser("user1")
      set_pwd("pwd") 
      ## get all sports 
      res <- GetSports()
  
  
# Installation

The package is not already in the CRAN but 
you can install the development version from Github:


      if (!require('devtools')) install.packages('devtools')
      devtools::install_github('agstudy/pinnacleAPI')
      
      
# Documentation

In addiotional to the API documenation all the function in the package are documented. 

You can see inst/tests for examples of use.


# Tests

To run tests 

     library(testthat)
     test_package("pinnacleAPI")
