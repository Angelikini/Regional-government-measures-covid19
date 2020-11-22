
#install.packages(c("httr", "jsonlite"))

library(httr)
library(jsonlite)

## Post credentials to get an authentication token
credentials <- list(username="an@acaps.org", password="Beukstraat149")
auth_token_response <- httr::POST("https://api.acaps.org/api/v1/token-auth/", body=credentials)
auth_token <- httr::content(auth_token_response)$token

## Pull data from ACAPS API and loop through the pages
df <- data.frame()
request_url <- "https://api.acaps.org/api/v1/government-measures/" # Replace with the URL of the dataset you want to access
last_request_time <- Sys.time()
while (TRUE) {
  
  ## Wait to avoid throttling
  while (as.numeric(Sys.time()-last_request_time) < 1) {
    Sys.sleep(0.1)
  }
  
  ## Make the request
  response <- httr::GET(request_url, add_headers(Authorization=paste("Token", auth_token, sep=" ")))
  last_request_time <- Sys.time()
  
  ## Append to the dataframe
  df_results <- jsonlite::fromJSON(content(response, "text"))$results
  df <- rbind(df, df_results)
  
  ## Loop to the next page; if we are on the last page, break the loop
  if (("next" %in% names(content(response))) && (typeof(content(response)[["next"]]) != "NULL")) {
    request_url <- content(response)[["next"]]
  }
  else {
    break
  }
}

rm(list=setdiff(ls(), "df"))