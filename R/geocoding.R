#' Geocoding a list of address using Nominatim, US Census.
#'
#' @param address_list a list of address that should be geocoded
#' @param threshold importance threshold to identify invalid geocoded observation
#' @param max_attempt max attempt to fetch the data from US census API
#'
#' @return address, lat, lng, city, county, zipcode, status, importance
#' @export
geocoding <- function(address_list, threshold = 0.5, max_attempt = 3) {

  # =================== #
  # ---- Nominatim ----
  # =================== #
  geocoding_result <-
    purrr::map_dfr(
      .x = address_list,
      .f = geocoding_nominatim,
      threshold = threshold
    ) %>%
    dplyr::mutate(
      index = dplyr::row_number(),
      # If the zip in the address does not align with
      # the zip from the Nominatim geocoding result
      zip_match = purrr::pmap_lgl(
        .l = list(.data$address, .data$zip_geocode),
        .f = function(address, zip_geocode) {

          zip_address <- stringr::str_extract(address, "\\d{5}")
          has_zip_geocode <- !is.na(zip_geocode)
          has_zip_address <- !is.na(zip_address)

          if (has_zip_geocode && has_zip_address) {
            return(zip_address == zip_geocode)
          } else {
            return(FALSE)
          }
        }
      ),
      status_geocode = ifelse(
        .data$zip_match,
        .data$status_geocode,
        FALSE
      )
    ) %>%
    dplyr::select(-.data$zip_match,) %>%
    dplyr::select(.data$index, dplyr::everything())

  status_geocode_all_valid <- all(geocoding_result$status_geocode)

  # ==================== #
  # ---- US Census -----
  # ==================== #
  MAX_ATTEMPT <- max_attempt
  census_API_attempt <- 0
  while (!status_geocode_all_valid && (census_API_attempt < MAX_ATTEMPT)) {

    geocoding_result_invalid <-
      geocoding_result %>%
      dplyr::filter(!.data$status_geocode)

    geocoding_result_rework <-
      geocoding_result_invalid %>%
      dplyr::select(.data$index) %>%
      dplyr::bind_cols(
        purrr::map_dfr(
          .x = geocoding_result_invalid$address,
          .f = geocoding_census
        )
      )

    geocoding_result <-
      geocoding_result %>%
      dplyr::filter(.data$status_geocode) %>%
      dplyr::bind_rows(geocoding_result_rework) %>%
      dplyr::arrange(.data$index)

    status_geocode_all_valid <- all(geocoding_result$status_geocode)
    census_API_attempt <- census_API_attempt + 1
  }

  # ============== #
  # ---- Tidy ----
  # ============== #

  geocoding_result <-
    geocoding_result %>%
    dplyr::arrange(.data$index) %>%
    dplyr::select(-.data$index) %>%
    dplyr::mutate(
      city = stringr::str_to_title(.data$city),
      county = stringr::str_to_title(stringr::str_remove(.data$city, " County")),
      contains_na = is.na(.data$lat) | is.na(.data$lng) |is.na(.data$city) | is.na(.data$county) | is.na(.data$zip_geocode),
      status_geocode = ifelse(.data$contains_na, FALSE, .data$status_geocode)
    ) %>%
    dplyr::select(-.data$contains_na)

  # ============= #
  # ---- I/O ----
  # ============= #
  geocoding_result_all_valid <- all(geocoding_result$status_geocode)

  if (!geocoding_result_all_valid) {
    geocoding_result_invalid <-
      geocoding_result %>%
      dplyr::filter(!.data$status_geocode)
    n_invalid <- nrow(geocoding_result_invalid)
    cat(crayon::red(
      glue::glue(
        "Failed to geolocate {n_invalid} addresses",
        n_invalid = n_invalid
      )
    ),sep = "\n")
  }

  return(geocoding_result)
}
