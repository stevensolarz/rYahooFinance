pkg.env <- new.env()
pkg.env$yf.baseurl <- 'https://yfapi.net/'

#' This function initializes the package with the users API key.
#'
#' @param apiKey This is the key provided by Yahoo Finance API
#'
#' @return Returns the status code of the test API call.
#'
#'
yfinConnect <- function(apiKey) {

  if(is.character(apiKey)) {
    pkg.env$yf.apiKey <- apiKey
  } else {
    stop("Please check the API Key provided and try again.")
  }

  ## This tests to see that a query works successfully.
  status <- httr::GET(url=paste0(pkg.env$yf.baseurl, 'v8/finance/spark?symbols=AAPL'),
                      httr::add_headers(
                        c("Content-Type"="application/json"),
                        "x-api-key"=pkg.env$yf.apiKey
                      ),
                      query = list(parameter = list(symbols = "AAPL")),
                      encode = "json")$status

  if(status == 200) {
    message("Successfully connected to Yahoo Finance API.")
    return(status)
  } else {
    message("Failed to connect to Yahoo Finance API. Please check the API Key provided and try again.")
    yfinDisconnect()
    return(status)
  }

}

yfinDisconnect <- function() {
  pkg.env$yf.apiKey <- NULL
  message("Cleared Yahoo Finance API settings.")
}






