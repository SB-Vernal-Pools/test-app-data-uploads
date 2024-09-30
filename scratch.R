library(lubridate)
library(here)

source(here("water-year-fun.R"))


# create empty df (only once to avoid overwriting)
if (!exists("hydro_new")) {
  hydro_new <- data.frame(
    object_id = as.numeric(),
    global_id = as.character(),
    date = as.Date(character()),
    location = as.character(),
    unlisted_location = as.character(),
    monitor_name = as.character(),
    pool_id = as.character(),
    water_level_in = as.numeric(),
    optional_comments = as.character(),
    creation_date = as.Date(character()),
    creator = as.character(),
    edit_date = as.Date(character()),
    editor = as.character(),
    x = as.numeric(),
    y = as.numeric(),
    water_year = as.numeric(),
    location_pool_id = as.character(),
    stringsAsFactors = FALSE
  )
}


# list/input key data
hydro_date <- as.Date("2024-09-15")
hydro_location <- "NCOS"
hydro_monitor_name <- "Bri"
hydro_pool_id <- "4"
water_level_in <- 12
optional_comments <- NA

# find water year & location_pool_id
location_pool_id <- paste(hydro_location, hydro_pool_id, sep = " - ")
water_year <- water_year(hydro_date)


# add new data to full row with additional default values
new_row <- data.frame(
  object_id = NA, 
  global_id = NA,
  date = hydro_date,
  location = hydro_location,
  unlisted_location = NA,
  monitor_name = hydro_monitor_name,
  pool_id = hydro_pool_id,
  water_level_in = as.numeric(water_level_in),
  optional_comments = optional_comments,
  creation_date = NA,
  creator = NA,
  edit_date = NA,
  editor = NA,
  x = NA,
  y = NA,
  water_year = water_year,
  location_pool_id = location_pool_id,
  stringsAsFactors = FALSE
)

# append inputted data to hydro_new df
data_to_append <- rbind(hydro_new, new_row)

# overwrite original hydro_new to incldue all entries
hydro_new <- data_to_append
