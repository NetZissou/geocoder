
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geocoder

<!-- badges: start -->

<!-- badges: end -->

The goal of `geocoder` is to provide geocoding service for R users.

## Installation

To install this package

``` r
devtools::install_github("NetZissou/geocoder")
```

## Geocoding

User could use `geocoding` function to obtain the spatial information
from a given address. User may pass in one or multiple address all at
once.

The data structure returned from the `geocoding` function is specified
as follows:

  - `address`: the address specified by the user
  - `lat`: latitude
  - `lng`: longitude
  - `county`: county name
  - `zip_geocode`: zip code from the best-matched geocoding result
  - `status_geocode`: `TRUE` if a valid result is founded, otherwise
    `FALSE`
  - `importance`: the accuracy score from the nominatim geocoding
    service
  - `service`: [nominatim](https://nominatim.osc.edu/ui/search.html) is
    prioritize and [US census
    API](https://geocoding.geo.census.gov/geocoder/) comes second

<!-- end list -->

``` r
library(geocoder)

address <- "Robinwood Avenue, Whitehall, Franklin County, Ohio, 43213, United States"
result <- geocoding(address)
result
#> # A tibble: 1 × 9
#>   address   lat   lng city  county zip_geocode status_geocode importance service
#>   <chr>   <dbl> <dbl> <chr> <chr>  <chr>       <lgl>               <dbl> <chr>  
#> 1 Robinw…  40.0 -82.9 Whit… White… 43213       TRUE                 1.03 nomina…
dplyr::glimpse(result)
#> Rows: 1
#> Columns: 9
#> $ address        <chr> "Robinwood Avenue, Whitehall, Franklin County, Ohio, 43…
#> $ lat            <dbl> 39.96893
#> $ lng            <dbl> -82.8964
#> $ city           <chr> "Whitehall"
#> $ county         <chr> "Whitehall"
#> $ zip_geocode    <chr> "43213"
#> $ status_geocode <lgl> TRUE
#> $ importance     <dbl> 1.03
#> $ service        <chr> "nominatim"
```

``` r
address_list <- 
  c(
     "Robinwood Avenue, Whitehall, Franklin County, Ohio, 43213, United States",
     "Hamilton Road, East Hamilton Plaza, Whitehall, Franklin County, Ohio, 43125, United States",
     "Target, 3955, East Broad Street, Whitehall, Franklin County, Ohio, 43213, United States",
     "Josephus Lane, Whitehall, Franklin County, Ohio, 43227, United States",
     "Chandler Drive, Whitehall, Franklin County, Ohio, 43213, United States",
     "Enterprise, East Broad Street, East Broad Plaza Apartments, Whitehall, Franklin County, Ohio, 43004, United States"
  )

geocoding(address_list)
#> # A tibble: 6 × 9
#>   address   lat   lng city  county zip_geocode status_geocode importance service
#>   <chr>   <dbl> <dbl> <chr> <chr>  <chr>       <lgl>               <dbl> <chr>  
#> 1 Robinw…  40.0 -82.9 Whit… White… 43213       TRUE                 1.03 nomina…
#> 2 Hamilt…  40.0 -82.9 Whit… White… 43125       TRUE                 1.33 nomina…
#> 3 Target…  40.0 -82.9 Whit… White… 43213       TRUE                 1.25 nomina…
#> 4 Joseph…  40.0 -82.9 Whit… White… 43227       TRUE                 1.03 nomina…
#> 5 Chandl…  40.0 -82.9 Whit… White… 43213       TRUE                 1.03 nomina…
#> 6 Enterp…  40.0 -82.9 Whit… White… 43004       TRUE                 1.54 nomina…
```

To specify the service to use, user could use `geocoding_nominatim` or
`geocoding_census`.

``` r
# Use US Census API
geocoding_census(address = address)
#> # A tibble: 1 × 9
#>   address   lat   lng city  county zip_geocode status_geocode importance service
#>   <chr>   <dbl> <dbl> <chr> <chr>  <chr>       <lgl>               <dbl> <chr>  
#> 1 Robinw…    NA    NA <NA>  <NA>   <NA>        FALSE                  NA US Cen…

# Use Nominatim
geocoding_nominatim(address = address, threshold = 0.5)
#> # A tibble: 1 × 9
#>   address   lat   lng city  county zip_geocode status_geocode importance service
#>   <chr>   <dbl> <dbl> <chr> <chr>  <chr>       <lgl>               <dbl> <chr>  
#> 1 Robinw…  40.0 -82.9 Whit… Frank… 43213       TRUE                 1.03 nomina…
```

Note that these two functios only works for single address input. To
geocode multiple addresses with specified service, wrap the function
with a for-loop or use the `map` family functions.

``` r
purrr::map_dfr(
  .x = address_list,
  .f = geocoding_nominatim
)
#> # A tibble: 6 × 9
#>   address   lat   lng city  county zip_geocode status_geocode importance service
#>   <chr>   <dbl> <dbl> <chr> <chr>  <chr>       <lgl>               <dbl> <chr>  
#> 1 Robinw…  40.0 -82.9 Whit… Frank… 43213       TRUE                 1.03 nomina…
#> 2 Hamilt…  40.0 -82.9 Whit… Frank… 43125       TRUE                 1.33 nomina…
#> 3 Target…  40.0 -82.9 Whit… Frank… 43213       TRUE                 1.25 nomina…
#> 4 Joseph…  40.0 -82.9 Whit… Frank… 43227       TRUE                 1.03 nomina…
#> 5 Chandl…  40.0 -82.9 Whit… Frank… 43213       TRUE                 1.03 nomina…
#> 6 Enterp…  40.0 -82.9 Whit… Frank… 43004       TRUE                 1.54 nomina…
```

## Reverse Geocoding

Sometimes only lat and lng is provided but you would like to have other
spatial information such as zipcode, county, city and etc. You could use
`geocoding_nominatim_reverse` to reverse geocoding.

``` r
geocoding_nominatim_reverse(
  lat = 39.96893,
  lng = -82.8964
)
#> # A tibble: 1 × 9
#>   address   lat   lng city  county zip_geocode status_geocode importance service
#>   <chr>   <dbl> <dbl> <chr> <chr>  <chr>       <lgl>               <dbl> <chr>  
#> 1 Robinw…  40.0 -82.9 Whit… Frank… 43213       TRUE                    1 nomina…
```

## Geocoding with Spatial Shapefiles

`geocoding_sf` is designed when you have a shapefile that contains
serveral spatial regions and you wold like to know which region your
data falls into.

``` r
geocoding_sf(
  lat = lat,
  lng = lng,
  sf = sf,     # spatial object
  id = "name", # the identifier column of the sf object
  parallel = FALSE # parallel execution
)
```
