#' Retrieve data in Open Context by country
#'
#' Given a character vector of one or more countries, or a data frame of
#' countries returned from \code{\link{oc_browse}}, this function retrieves data
#' related to those countries. The function can return projects, locations, or
#' descriptions.
#'
#' @param country A character vectory of country names or a data frame
#'   returned from \code{\link{oc_browse}}.
#' @param type The type of data to return: \code{"projects"},
#'   \code{"locations"}, \code{"descriptions"}. The default is
#'   \code{"projects"}.
#' @return A data frame with the additional class \code{oc_dataframe}.
#' @examples
#' oc_get_countries("germany", type= "projects")
#'
#' library(dplyr)
#' oc_browse(type = "countries") %>%
#'   filter(label == "Turkey") %>%
#'   oc_get_countries(type = "locations")
#'
#' @export
oc_get_countries <- function(country, type = c("projects", "locations",
                                               "descriptions")) {
  type <- match.arg(type)
  oc_get_records(country, type, category = "countries")
}

#' Retrieve the names of projects in a given Country and Location
#'
#' @param country A country name
#' @param locations A character vector of locations in that country
#' @param type The type of records to return.
#' @examples
#' oc_get_locations("Turkey", "Ulucak")
#' @export
oc_get_locations <- function(country, locations, type = c("projects", "descriptions")){
  type <- match.arg(type)
  oc_get_countries(country, type = "locations") %>%
  filter_(~label %in% locations) %>%
  oc_get_records(type)
}


#' Retrieve data given an Open Context project name
#'
#' @param project A character vector of project names
#' @return A data frame of resources associated with the projects.
#' @examples
#' oc_get_projects("Kenan Tepe")
#' @export
oc_get_projects <- function(project) {
  oc_get_records(project, type = "projects", category = "projects")
}




oc_get_records <- function(field, type, category) {
  UseMethod("oc_get_records")
}

oc_get_records.oc_dataframe <- function(field, type, category) {

  result <- field %>%
              rowwise() %>%
              do(get_row(., type = type)) %>%
              ungroup()

  oc_dataframe(result)

}

oc_get_records.character <- function(field, type, category) {

  oc_browse(type = category) %>%
    filter_(~tolower(label) %in% tolower(field)) %>%
    oc_get_records(type = type)

}

get_row <- function(row, type) {
  message("Getting data for ", row$label)

  req <- httr::GET(row$id, httr::accept_json())
  httr::warn_for_status(req)
  response <- httr::content(req, as = "text")
  if (identical(response, "")) stop("")
  result <- jsonlite::fromJSON(response)

  result <- switch(type,
         "locations" = result$`oc-api:has-facets`$`oc-api:has-id-options`[[1]],
         "projects"  = result$`oc-api:has-facets`$`oc-api:has-id-options`[[2]],
         "descriptions"  = result$`oc-api:has-facets`$`oc-api:has-id-options`[[3]],
         "four"  = result$`oc-api:has-facets`$`oc-api:has-id-options`[[4]]
  )

  result
}


# ---- Pipeline Annotation ----

#' @title Get Locations
#' 
#' Retrieves all possible locations within a country.
#'
#' @param country A country name
#' @examples
#' oc_get_locations("United States")
#' @export
oc_list_locations <- function(country = "") {
  url <- paste0(base_url(), "sets/")
  url <- paste0(url, gsub(" ", "+", country), ".json")
  
  req <- httr::GET(url, query = list(), httr::accept_json())
  httr::warn_for_status(req)
  
  response <- httr::content(req, as = "text")
  
  if (identical(response, "")) {
    stop("")
  }
  locations <- jsonlite::fromJSON(response)
  locations_oc <- locations$`oc-api:has-facets`$`oc-api:has-id-options`[[1]]
  
  return(locations_oc)
}

#' @title Get County Names
#' 
#' Retrieves data from location specific queries.
#'
#' @param location A location name
#' @param country A country name
#' @examples
#' oc_local_info("United States", "Utah")
#' oc_local_info("Cyprus/Turkey", "Boğazköy")
#' @export
oc_county_info <- function(country = "", locations = "") {
  url <- paste0(base_url(), "sets/")
  url <- paste0(url, gsub(" ", "+", country), "/")
  url <- paste0(url, gsub(" ", "+", locations), ".json")
  
  req <- httr::GET(url, query = list(), httr::accept_json())
  httr::warn_for_status(req)
  
  response <- httr::content(req, as = "text")
  
  if (identical(response, "")) {
    stop("")
  }
  local_info <- jsonlite::fromJSON(response)
  local_info_oc <- local_info$`oc-api:has-facets`$`oc-api:has-id-options`[[1]]
  
  return(local_info_oc)
}

