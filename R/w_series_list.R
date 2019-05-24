#' List the available series from a dataset.
#'
#' @section Identification:
#' You should declare your Webstat client ID in a global "webstat_client_ID" variable. Alternatively, you can enter your client ID as a parameter or enter it when prompted.
#' 
#' @param dataset_name Mandatory. String (Must be between quotes.) The datasets codes can be determined with the w_datasets() function.
#' @param language Optional. String. Defaults to "fr" (French). The only other available option is "en" (English). Determines the language of the metadata. Your Webstat "App" must be subscribed to the API in this language (or both) or you'll get a 501 http error.
#' @param client_ID Optional. String. If you do not specify it when calling the function, it will check if a global variable called ".GlobalEnv$webstat_client_ID exists and use it. If not, you will be prompted. The easiest way is to save the client ID as a string in ".GlobalEnv$webstat_client_ID".
#'
#' @examples
#' \donttest{
#' ## Request the list of all series from the BPM6 dataset
#' w_series_list("BPM6")
#' 
#' ## Request the list of all series from the CPP dataset, with English metadata
#' w_series_list("CPP", language = "en")
#' 
#' \dontrun{
#' ## Your client ID can be entered as a parameter as follows or saved
#' ## in a global variable named "webstat_client_ID" in order to reuse it.
#' w_series_list("CPP", client_ID = "1234abcd-12ab-12ab-12ab-123456abcdef") 
#' }
#' }
#' 
#' @return A data frame listing all the series from the requested dataset with their codes, titles and dimensions.
#' 
#' @import dplyr
#' @import httr
#' @import getPass
#' @import readr
#' @import utils
#' 
#' @export
w_series_list <- function(dataset_name,language = "fr", client_ID)
{
  # check client_ID
  if(missing(client_ID)) {
    if(exists("webstat_client_ID")) {
      client_ID <-  webstat_client_ID
    } else {
      client_ID <- set_client_id()
    }
    webstat_client_ID <-  NULL
  }
  
  # check language and dataset
  if (language !="en" & language !="fr") { stop("language must be either 'fr' or 'en'")}
  if (missing(dataset_name)) { stop("Dataset is missing")}
  
  # Build API URL "w_url" for the request
  api_base_url <- paste("https://api.webstat.banque-france.fr/webstat-",language,"/v1/",sep="")
  api_fun <- "catalogue/"
  format <- "?format=csv"
  w_url <- paste0(api_base_url,
                  api_fun,dataset_name,format)
  
  # Call the API
  req_csv <- GET(w_url, add_headers("x-ibm-client-id" = client_ID), content_type("text/csv"))
  
  # Check for http error
  if(req_csv$status_code != 200) {
    switch(as.character(req_csv$status_code),
           "502" = {stop(paste(url,"\nTime Out : please request a smaller set"))},
           "429" = {stop(paste(content(req_csv,encoding = "UTF-8")$httpMessage,"\n",content(req_csv,encoding = "UTF-8")$moreInformation))},
           "501" = {stop(paste(url,"\nFormat not yet implemented."))},
           "500" = {stop(paste(url,"\nInternal error. Please try again."))},
           "400" = {stop(paste(url,"\nIncorrect format value."))},
           "401" = {stop(paste(url,"\nInvalid client_ID. Please check that the string contained in the client_ID or webstat_client_ID variable is correct (format: ''123456ab-ab12-12cd-12cd-123456789abc'') and that your account is registered to the Webstat API you are trying to access."))},
           "404" = {stop(paste(url,"\nData not found."))},
           {stop(content(req_csv,encoding = "UTF-8")$message)}
    )
  }
  
  # Get content from request
  cont_csv <- content(req_csv, as = "text", encoding = "UTF-8")
    
  # Read in table format  ------
  if (language == "en") { sep_lg = ","} else { sep_lg = ";"}
  
  series <- read.csv(text = cont_csv, sep = sep_lg, header = F,stringsAsFactors = F)
  # Clean colnames and remove the last NA-filled column
  names(series) <- series[1,]
  series <- series[-1,-dim(series)[2]]
  return(series)
}
