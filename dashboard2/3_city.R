install.packages("ggmap")
install.packages("tmaptools")
install.packages("RCurl")
install.packages("jsonlite")
install.packages("tidyverse")
install.packages("leaflet")

# load packages
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leaflet)

key="AIzaSyB9yfhosjbjh7sN4WNGrvaxW9pLruFcO4o"

#Función para buscar y reemplazar texto

url_google_place_search <- function(search_query_url, key_url) {
  # load libraries
  library(RCurl)
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  # google places api url
  url_places_api <- "https://maps.googleapis.com/maps/api/place/"
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  # construct search request for place id
  url_place_search_call <- paste0(url_places_api, "findplacefromtext/",
                                  "json", "?input=", search_query_url,
                                  "&inputtype=textquery","&fields=formatted_address",
                                  "&key=", key_url)
  return(url_place_search_call)
}

get_place_formatted_address_from_json_google <- function(place_json) {
  # load library
  library(jsonlite)
  # convert json output into r object
  place_search <- lapply(place_json, fromJSON,simplifyVector = FALSE)
  # extract place id
  place_address <- list()
  for (i in 1:length(place_search)) {
    if (place_search[[i]]$status=="OK") {
      place_address[[i]] <- place_search[[i]]$candidates[[1]]$formatted_address
    } else {
      place_address[[i]] <- NA
    }
  }
  return(place_address)
}



geocode_google <- function(search_query, key) {
  library(RCurl)
  
  # /// get place_id from Place Search API ///
  # construct url for place search
  url_place_search <- url_google_place_search(search_query, key)
  place_json <- getURL(url_place_search)
  address <- get_place_formatted_address_from_json_google(place_json)
  if (!exists("geodata_df")) {
      geodata_df <- as.data.frame(sapply(search_query, as.character),
                                stringsAsFactors = FALSE)
      names(geodata_df) <- "search query"
      rownames(geodata_df) <- NULL
  }
  
  df <- as.data.frame(do.call(rbind, address))
  geodata_df[, c("address")] <- df

  return(geodata_df)

}

