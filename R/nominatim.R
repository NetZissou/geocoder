#' Geocoding data using Nominatim
#'
#' @param address address is free-form but it is best form your address like {street}, {city}, {county}, {state}, {postalcode}
#' @param threshold importance threshold to identify invalid geocoded observation
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding_nominatim <- function(address, threshold = 0.5) {

  tryCatch(
    expr = {
      # ============================ #
      # ---- Format Request URL ----
      # ============================ #
      url <- "https://nominatim.osc.edu/"
      # ================================= #
      # ---- Get data from Nominatim ----
      # ================================= #

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
          service = "nominatim"
        )

      res <- httr::GET(
        url,
        query = list(
          q = address,
          addressdetails = 1,
          format = "json",
          limit = 1
        )
      )
      res_content <- httr::content(res)

      # ======================== #
      # ---- Error Handling ----
      # ======================== #
      contain_cross_street <- stringr::str_detect(address, "&|/")
      contain_seperators <- stringr::str_detect(address, ",")

      has_records <- (length(res_content) != 0)
      above_threshold <- (has_records && res_content[[1]]$importance >= threshold)
      status_geocode_valid <- has_records && above_threshold

      if (!status_geocode_valid & contain_cross_street) {

        # > Split Cross Street ---- #
        return(
          geocoding_nominatim_safely_cross_street(address = address, threshold = threshold)
        )
      } else if (!status_geocode_valid & contain_seperators) {

        # > Remove Seperators ---- #
        return(
          geocoding_nominatim_safely_remove_seperator(address = address, threshold = threshold)
        )
      } else if (!status_geocode_valid) {

        # > No geocoding result ---- #
        return(result)
      }


      # ==================== #
      # ---- Store Data ----
      # ==================== #
      best_match <- res_content[[1]]
      address_attr <- names(best_match$address)

      # > Geocoding Score ----
      result$importance <- best_match$importance
      # > Geocoding Status ----
      result$status_geocode <- status_geocode_valid
      # > Address: Lat & Lng ----
      result$lat <- as.double(best_match$lat)
      result$lng <- as.double(best_match$lon)

      # > Address: City ----
      if ("city" %in% address_attr) {
        result$city <- as.character(best_match$address$city)
      } else if ("town" %in% address_attr) {
        result$city <- as.character(best_match$address$town)
      }

      # > Address: County ----
      if ("county" %in% address_attr) {
        result$county <- as.character(best_match$address$county)
      }

      # > Address: Zipcode ----
      if ("postcode" %in% address_attr) {
        result$zip_geocode <- as.character(best_match$address$postcode)
      }
    },

    error = function(error) {
      cat(crayon::red(
        glue::glue("Failed geocoding address: {address}", address = address)
      ), sep = "\n")
      print(error)
    }
  )



  return(result)
}


#' Support function for `geocoding_nominatim` when there address failed to geocode but contains cross road
#'
#' @inheritParams geocoding_nominatim
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
geocoding_nominatim_safely_cross_street <- function(address, threshold) {

  # ============================================= #
  # ---- Split Cross Street into Two Address ----
  # ============================================= #
  address_split <-
    stringr::str_split(address, pattern = "&|/")[[1]] %>%
    stringr::str_trim(side = "both")

  street_1 <- address_split[1]
  street_2 <- address_split[2]


  address_street_1 <- stringr::str_c(c(street_1, address_split[-c(1,2)]), collapse  = " ")
  address_street_2 <- stringr::str_c(c(street_2, address_split[-c(1,2)]), collapse  = " ")

  #result_street_1 <- geocoding_nominatim(address_street_1)
  result_street_2 <- geocoding_nominatim(address_street_2)


  return(geocoding_nominatim(address = address, threshold = threshold))
  # result <-
  #   dplyr::bind_rows(
  #     result_street_1,
  #     result_street_2
  #   ) %>%
  #   dplyr::mutate(
  #     address = address
  #   )
  #
  # if (all(result$status_geocode)) {
  #   # Obtained result from both street
  #   if (result_street_1$importance > result_street_2$importance) {
  #     return(result[1,])
  #   } else {
  #     return(result[2,])
  #   }
  # } else if (!all(result$status_geocode)) {
  #   # Obtained no result from either street
  #   result <-
  #     tibble::tibble(
  #       address = address,
  #       lat = NA_real_,
  #       lng = NA_real_,
  #       city = NA_character_,
  #       county = NA_character_,
  #       zip_geocode = NA_character_,
  #       status_geocode = FALSE,
  #       importance = NA_real_,
  #       service = "nominatim"
  #     )
  #   return(result)
  # } else  {
  #   # One street has result
  #   return(
  #     result %>% dplyr::filter(.data$status_geocode)
  #   )
  # }
}


#' Support function for `geocoding_nominatim` when there address failed to geocode because of the seperators between attributes
#'
#' @inheritParams geocoding_nominatim
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
geocoding_nominatim_safely_remove_seperator <- function(address, threshold) {

  address_clean <- stringr::str_replace_all(address, ", ", " ")
  return(
    geocoding_nominatim(address = address_clean, threshold = threshold)
  )
}


#' Reverse geocoding with Nominatim
#'
#' @param lat lat
#' @param lng lng
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding_nominatim_reverse <- function(lat, lng) {

  # ============================ #
  # ---- Format Request URL ----
  # ============================ #
  url <- "https://nominatim.osc.edu/reverse"

  # ================================= #
  # ---- Get data from Nominatim ----
  # ================================= #

  result <-
    tibble::tibble(
      address = NA_character_,
      lat = lat,
      lng = lng,
      city = NA_character_,
      county = NA_character_,
      zip_geocode = NA_character_,
      status_geocode = FALSE,
      importance = NA_real_,
      service = "nominatim"
    )

  res <- httr::GET(
    url,
    query = list(
      lat = lat,
      lon = lng,
      addressdetails = 1,
      format = "json",
      limit = 1
    )
  )

  res_content <- httr::content(res)

  has_records <- (length(res_content) != 0)

  # ==================== #
  # ---- Store Data ----
  # ==================== #
  if (has_records) {

    address_attr <- names(res_content$address)

    # > Geocoding Score ----
    result$importance <- 1
    # > Geocoding Status
    result$status_geocode <- TRUE
    # > Address: Full Address
    result$address <- res_content$display_name

    # > Address: City
    if ("city" %in% address_attr) {
      result$city <- as.character(res_content$address$city)
    } else if ("town" %in% address_attr) {
      result$city <- as.character(res_content$address$town)
    } else if ("municipality" %in% address_attr) {
      result$city <- as.character(res_content$address$municipality)
    }
    # > Address: County ----
    if ("county" %in% address_attr) {
      result$county <- as.character(res_content$address$county)
    }
    # > Address: Zipcode ----
    if ("postcode" %in% address_attr) {
      result$zip_geocode <- as.character(res_content$address$postcode)
    }
  }

  return(result)
}
