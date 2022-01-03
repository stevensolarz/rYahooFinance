#' Run a query against the API. This function is used as the base query for all
#' other functions in this package, or you can input your own custom path
#'
#' @param path The path of the query.
#'
#' @return Result of the query
#' @export
#'
yfinQuery <- function(path) {

  tryCatch({
    url <- httr::modify_url(
      url=pkg.env$yf.baseurl,
      path=path
    )
  },error=function(e){
    message("Please check your path and try again.")
  })

  data <- httr::GET(url,
                    httr::add_headers(
                      c("Content-Type"="application/json")
                    ),
                    encode = "json")
  if(data$status_code == 403) {
    message("Access forbidden. Please check your path and try again.")
    return(NULL)
    
  } else if(data$status_code == 404) {
    message("Query not found. Please check your path and try again.")
    return(NULL)
  }else {
    return(data)
  }

}
