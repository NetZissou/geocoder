#' Geolocate a point among the provided sf objects
#'
#' @param lat lat
#' @param lng lng
#' @param sf a sf tibble that contains many sf objects
#' @param id the identifier column of the sf object
#' @param parallel TRUE to execute in parallel
#'
#' @return the name of the object that the point falls into; return NA if none
#' @export
geocoding_sf <- function(lat, lng, sf, id = "name", parallel = FALSE) {

  point <- sf::st_point(x = c(lng, lat), dim = "XYZ")

  map_function <- purrr::pmap_lgl
  if (parallel) {
    map_function <- furrr::future_pmap_lgl
  }

  result <-
    sf %>%
    dplyr::mutate(
      result = map_function(
        .l = list(.data$geometry),
        .f = function(geom, point) {
          result <-
            sf::st_intersects(
              geom,
              point
            )[[1]]

          if (length(result) == 0) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        },
        point = point
      )
    ) %>%
    dplyr::filter(.data$result) %>%
    utils::head(1) %>% # Only keep one if there are multiple result
    dplyr::pull(.data[[id]])

  if (length(result) == 0) {
    result <- NA_character_
  }
  return(result)
}
