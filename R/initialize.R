pkg.env <- new.env()
pkg.env$yf.baseurl <- 'https://query2.finance.yahoo.com/'

#' This function tests the API connection.
#'
#' @param apiKey This is the key provided by Yahoo Finance API
#'
#' @return Returns the status code of the test API call.
#'
#'
yfinTestConnection <- function() {

  ## This tests to see that a query works successfully.
  status <- httr::GET(url=paste0(pkg.env$yf.baseurl, 'v8/finance/spark?symbols=AAPL'),
                      httr::add_headers(
                        c("Content-Type"="application/json")
                      ),
                      query = list(parameter = list(symbols = "AAPL")),
                      encode = "json")$status

  if(status == 200) {
    message("Successfully connected to Yahoo Finance API.")
    return(status)
  } else {
    message("Failed to connect to Yahoo Finance API. Please check your connection and try again.")
    return(status)
  }

}



