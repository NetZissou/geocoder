#' Geocoding data using United States Census
#'
#' @description documentation https://geocoding.geo.census.gov/geocoder/
#' @param address address is free-form but it is best form your address like {street}, {city}, {county}, {state}, {postalcode}
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding_census <- function(address) {

  url_base <- "https://geocoding.geo.census.gov/geocoder/{returntype}/{searchtype}"

  url_geo <- glue::glue(url_base, returntype = "geographies", searchtype = "onelineaddress")

  res <- httr::GET(
    url_geo,
    query = list(
      benchmark = "Public_AR_Current",
      vintage= "Current_Current",
      address = address,
      format = "json"
    )
  )

  res_content <- httr::content(res)

  result <-
    tibble::tibble(
      address = address,
      lat = NA_real_,
      lng = NA_real_,
      city = NA_character_,
      county = NA_character_,
      zip_geocode = NA_character_,
      status_geocode = FALSE,
      importance = NA_real_,
      service = "US Census"
    )

  request_valid <- (httr::status_code(res) == 200)
  has_records <- request_valid && (length(res_content$result$addressMatches) != 0)
  status_geocode_valid <- (request_valid && has_records)

  if (!status_geocode_valid) {
    return(result)
  }

  # ==================== #
  # ---- Store Data ----
  # ==================== #
  best_match <- res_content$result$addressMatches[[1]]
  address_attr <- names(best_match$addressComponents)

  # > Geocoding Score ----
  result$importance <- NA_real_ # US Census service has no importance score
  # > Geocoding Status ----
  result$status_geocode <- status_geocode_valid
  # > Address: Lat & Lng ----
  result$lat <- as.double(best_match$coordinates$y)
  result$lng <- as.double(best_match$coordinates$x)

  # > Address: City ----
  if ("city" %in% address_attr) {
    result$city <- as.character(best_match$addressComponents$city)
  }

  # > Address: County ----
  result$county <- as.character(best_match$geographies$Counties[[1]]$BASENAME)

  # > Address: Zipcode ----
  if ("zip" %in% address_attr) {
    result$zip_geocode <- as.character((best_match$addressComponents$zip))
  }

  # ============== #
  # ---- Tidy ----
  # ============== #
  result <-
    result %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~dplyr::na_if(., "")
      )
    )


  return(result)
}


#' Reverse geocoding with United States Census
#'
#' @param lat lat
#' @param lng lng
#'
#' @return raw request content
#' @export
geocoding_census_reverse <- function(lat, lng) {

  url_base <- "https://geocoding.geo.census.gov/geocoder/{returntype}/{searchtype}"

  url_geo <- glue::glue(url_base, returntype = "geographies", searchtype = "coordinates")

  res <- httr::GET(
    url_geo,
    query = list(
      benchmark = "Public_AR_Current",
      vintage= "Current_Current",
      x = lng,
      y = lat,
      format = "json"
    )
  )

  res_content <- httr::content(res)

  return(res_content)
}
