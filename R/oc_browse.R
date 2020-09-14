#' Browse the Open Context archeological database
#'
#' This function returns a data frame of certain types of top level data from
#' Open Context. You can get either a data frame of countries for which Open
#' Context has data, project names that have data on Open Context, or a list of
#' descriptions (Common Standards) of data attributes that are widely used in
#' Open Context datasets.
#'
#' @param type The kind of to be returned. You can chose either
#'   \code{'countries'} to get a data frame of names of countries that have Open
#'   Context datasets, or \code{'projects'} to get a data frame project names,
#'   or \code{'descriptions'} to get a data frame of data attributes that are
#'   widely used in Open Context data sets.
#' @param print_url Whether or not to display a message with the URL of the
#'   query. You can navigate to this URL to see the web interface's version of
#'   the data returned by the API.
#' @param ... Additional arguments passed to \code{\link[httr]{GET}}.
#' @param show_raw Whether or not to show the counts and other raw data or just the 
#' associated descriptions i.e counties, categories, etc.
#' @return A data frame with additional class \code{oc_dataframe}.
#' @examples
#' oc_browse("countries")
#' oc_browse("projects")
#' @export
oc_browse <- function(type = c("countries", "projects", "descriptions"),
                      print_url = FALSE, show_raw = FALSE, ...) {

  type <- match.arg(type)

  url <- paste0(base_url(), "sets/")
  if (print_url) {
    message(url)
  }

  req <- httr::GET(url, query = list(), httr::accept_json(), ...)
  httr::warn_for_status(req)

  response <- httr::content(req, as = "text")
  
  if (identical(response, "")) {
    stop("")
  }
  result <- jsonlite::fromJSON(response)
  
  if (show_raw) {
    result <- switch(type,
            "countries" = result$`oc-api:has-facets`$`oc-api:has-id-options`[[1]]["label"],
            "projects"  = result$`oc-api:has-facets`$`oc-api:has-id-options`[[2]]["label"],
            "descriptions"  = result$`oc-api:has-facets`$`oc-api:has-id-options`[[3]]["label"],
    )
  } else {
    result <- switch(type,
                     "countries" = result$`oc-api:has-facets`$`oc-api:has-id-options`[[1]],
                     "projects"  = result$`oc-api:has-facets`$`oc-api:has-id-options`[[2]],
                     "descriptions"  = result$`oc-api:has-facets`$`oc-api:has-id-options`[[3]],
    )
  }

  oc_dataframe(result)
}


oc_pipeline <- function() {
  url <- paste0(base_url(), "sets/")
  
  countries <- oc_browse("countries", show_raw = TRUE)
  countries$"url" <- countries$"label"
  countries[["url"]] <- paste0(url, gsub(" ", "+", countries[["url"]]), ".json")
  
  print(paste0("Fetching Subregion Information"))
  pb = txtProgressBar(min = 0, max = nrow(countries), initial = 0) 
  locations_results <- c()
  for(i in 1:nrow(countries)) {
    locations <- oc_list_locations(countries[i,1])
    locations$"country" <- replicate(nrow(locations), countries[i,1])
    locations_results <- rbind(locations_results, locations)
    
    setTxtProgressBar(pb,i)
  }
  
  print(paste0("Fetching County Information"))
  pb = txtProgressBar(min = 0, max = nrow(locations_results), initial = 0) 
  county_result <- c()
  for(i in 1:nrow(locations_results)) {
    req <- httr::GET(locations_results[i,2], query = list(), httr::accept_json())
    httr::warn_for_status(req)
     
    response <- httr::content(req, as = "text")
    if (identical(response, "")) {
      stop("")
    }

    county_info <- jsonlite::fromJSON(response)
    counties <- county_info$`oc-api:has-facets`$`oc-api:has-id-options`[[1]]
    counties$"sub_region" <- replicate(nrow(counties), locations_results[i,4])
     
    county_result <- rbind(county_result, counties)
    
    setTxtProgressBar(pb,i)
  }
  colnames(locations_results)[4] <- "sub_region"
  county_result <- merge(locations_results[,c(4,7)], county_result, 
                         by  = "sub_region", all.x = TRUE)
  
  print(paste0("Fetching Local Information"))
  pb = txtProgressBar(min = 0, max = nrow(county_result), initial = 0)
  local_result <- c()
  for(i in 1:nrow(county_result)) {
    req <- httr::GET(county_result[i,2], query = list(), httr::accept_json())
    httr::warn_for_status(req)

    response <- httr::content(req, as = "text")
    if (identical(response, "")) {
      stop("")
    }

    local_result <- jsonlite::fromJSON(response)
    local <- local_result$`oc-api:has-facets`$`oc-api:has-id-options`[[1]]
    local$"sub_region" <- replicate(nrow(local), county_result[i,4])

    local_result <- rbind(local_result, local)

    setTxtProgressBar(pb,i)
  }
  colnames(county_result)[6] <- "county"
  county_result <- merge(county_result[,c(1,2,6)], local_result, 
                         by  = "county", all.x = TRUE)
  View(local_result)
}

