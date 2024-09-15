# FarmTech Solutions - Weather Data Retrieval (OpenWeatherMap)
#
# This script retrieves current weather data for a specified city using the OpenWeatherMap API.
# It reads the API key from a .env file.
#
# Author: FarmTech Solutions Team
# Date: 2024

# Function to read .env file
read_env <- function(file = ".env") {
  if (!file.exists(file)) {
    warning("The .env file does not exist. Please create it and add your API key.")
    return(list())
  }
  lines <- readLines(file)
  env_vars <- list()
  for (line in lines) {
    parts <- strsplit(line, "=")[[1]]
    if (length(parts) == 2) {
      # Remove any surrounding quotation marks and whitespace
      env_vars[[trimws(parts[1])]] <- gsub("^\\s*[\"']?|[\"']?\\s*$", "", trimws(parts[2]))
    }
  }
  return(env_vars)
}

# Load environment variables
env <- read_env()
api_key <- env[["OPENWEATHERMAP_API_KEY"]]

if (is.null(api_key) || api_key == "YOUR_API_KEY_HERE") {
  stop("Please set your OpenWeatherMap API key in the .env file.")
}

# Function to make HTTP GET request
http_get <- function(url) {
  con <- url(url, "rb")
  on.exit(close(con))
  rawToChar(readBin(con, "raw", n = 1e6))
}

# Function to parse JSON string
parse_json <- function(json_string) {
  # Remove whitespace
  json_string <- gsub("\\s", "", json_string)
  
  parse_value <- function(str) {
    if (substr(str, 1, 1) == "{") {
      parse_object(str)
    } else if (substr(str, 1, 1) == "[") {
      parse_array(str)
    } else if (substr(str, 1, 1) == "\"") {
      gsub("^\"|\"$", "", str)
    } else if (grepl("^-?[0-9.]+$", str)) {
      as.numeric(str)
    } else if (str == "true") {
      TRUE
    } else if (str == "false") {
      FALSE
    } else if (str == "null") {
      NULL
    } else {
      str
    }
  }
  
  parse_object <- function(str) {
    str <- substr(str, 2, nchar(str) - 1)
    pairs <- strsplit(str, ",")[[1]]
    result <- list()
    for (pair in pairs) {
      kv <- strsplit(pair, ":", fixed = TRUE)[[1]]
      key <- gsub("^\"|\"$", "", kv[1])
      value <- parse_value(paste(kv[-1], collapse = ":"))
      result[[key]] <- value
    }
    result
  }
  
  parse_array <- function(str) {
    str <- substr(str, 2, nchar(str) - 1)
    elements <- strsplit(str, ",")[[1]]
    lapply(elements, parse_value)
  }
  
  parse_value(json_string)
}

# Function to get weather data
get_weather_data <- function(city, api_key) {
  # OpenWeatherMap API URL
  url <- sprintf("http://api.openweathermap.org/data/2.5/weather?q=%s&appid=%s&units=metric", URLencode(city), api_key)
  
  tryCatch({
    # Make API request
    response <- http_get(url)
    
    # Parse JSON response
    weather_data <- parse_json(response)
    
    # Display all weather information
    cat("Current Weather in", city, "\n\n")
    
    print_nested <- function(data, indent = "") {
      for (name in names(data)) {
        value <- data[[name]]
        if (is.list(value)) {
          cat(indent, name, ":\n", sep = "")
          print_nested(value, paste0(indent, "  "))
        } else {
          cat(indent, name, ": ", value, "\n", sep = "")
        }
      }
    }
    
    print_nested(weather_data)
    
  }, error = function(e) {
    cat("Error fetching weather data:", conditionMessage(e), "\n")
    cat("Please check your internet connection, API key, or try again later.\n")
  })
}

# Main execution
main <- function() {
  # Get city from user input
  cat("Enter city name: ")
  city <- readLines("stdin", n=1)
  
  # Get and display weather data
  get_weather_data(city, api_key)
}

# Run the main function
main()