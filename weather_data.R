# FarmTech Solutions - Weather Data Retrieval
#
# This script retrieves current weather data for a specified city using the wttr.in API.
# It doesn't require an API key and uses only base R functions for HTTP requests and JSON parsing.
#
# Author: Gabriel Mule <gabemule@gmail.com>
# Date: 2024

#' Make an HTTP GET request
#'
#' @param url The URL to fetch data from
#' @return The response body as a character string
#' @throws An error if the request fails
http_get <- function(url) {
  tryCatch({
    con <- url(url, "rb")
    on.exit(close(con))
    rawToChar(readBin(con, "raw", n = 1e6))
  }, error = function(e) {
    stop(paste("Error fetching data:", e$message))
  })
}

#' Extract a value from a JSON string using regex
#'
#' @param json_string The JSON string to parse
#' @param key The key to extract the value for
#' @return The extracted value, or NA if not found
extract_value <- function(json_string, key) {
  if (key == "weatherDesc") {
    pattern <- '"weatherDesc":\\s*\\[\\s*\\{\\s*"value":\\s*"([^"]+)"'
    match <- regexec(pattern, json_string)
    if (match[[1]][1] != -1) {
      return(substr(json_string, match[[1]][2], match[[1]][2] + attr(match[[1]], "match.length")[2] - 1))
    }
  } else {
    pattern <- sprintf('"%s"\\s*:\\s*"?([^",}]+)"?', key)
    match <- regexec(pattern, json_string)
    if (match[[1]][1] != -1) {
      return(substr(json_string, match[[1]][2], match[[1]][2] + attr(match[[1]], "match.length")[2] - 1))
    }
  }
  return(NA)
}

#' Fetch and display weather data for a given city
#'
#' @param city The name of the city to get weather data for
get_weather_data <- function(city) {
  # wttr.in API URL
  url <- sprintf("https://wttr.in/%s?format=j1", URLencode(city))
  
  tryCatch({
    # Make API request
    response <- http_get(url)
    
    # Extract relevant information
    temp <- extract_value(response, "temp_C")
    humidity <- extract_value(response, "humidity")
    wind_speed <- extract_value(response, "windspeedKmph")
    description <- extract_value(response, "weatherDesc")
    
    # Display weather information
    cat("Current Weather in", city, "\n")
    cat("Temperature:", temp, "°C\n")
    cat("Humidity:", humidity, "%\n")
    cat("Wind Speed:", wind_speed, "km/h\n")
    cat("Description:", description, "\n")
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    cat("Please check your internet connection or try again later.\n")
  })
}

#' Main function to run the weather data retrieval
main <- function() {
  # Get city from user input
  cat("Enter city name: ")
  city <- readLines("stdin", n=1)
  
  # Get and display weather data
  get_weather_data(city)
}

# Run the main function
main()