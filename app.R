# ==================================================
#                     Global                    ----
# ==================================================
library(tidyverse)
library(shiny)
library(lubridate)
library(here)
library(DT)
source(here("water-year-fun.R"))

percent_cover_data <- read_csv(here("data","percent_cover_data.csv"))
hydro_data <- read_csv(here("data","hydro_data.csv"))

spp_df <- read_csv(here("data","percent_cover_data.csv")) %>% 
  distinct(type, species, spp_cal_flora)

# Create empty df structures
empty_hydro_df <- data.frame(
  object_id = character(),
  global_id = character(),
  date = as.Date(character()),
  location = character(),
  unlisted_location = character(),
  monitor_name = character(),
  pool_id = character(),
  water_level_in = numeric(),
  optional_comments = character(),
  creation_date = as.Date(character()),
  creator = character(),
  edit_date = as.Date(character()),
  editor = character(),
  x = numeric(),
  y = numeric(),
  water_year = numeric(),
  location_pool_id = character(),
  stringsAsFactors = FALSE
)

empty_pc_df <- data.frame(
  object_id = character(),
  global_id = character(),
  parent_global_id = character(),
  monitoring_date = as.Date(character()),
  location = character(),
  pool_id = character(),
  transect_axis = character(),
  total_transect_length = numeric(),
  transect_notes = character(),
  transect_distance_of_quadrat = numeric(),
  side_of_transect_left_or_right = character(),
  vegetation_zone = character(),
  quadrat_notes = character(),
  percent_bare_ground = numeric(),
  percent_thatch = numeric(),
  count_of_native_species_automatically_calculated = numeric(),
  sum_of_native_cover_automatically_calculated = numeric(),
  count_of_non_native_species_automatically_calculated = numeric(),
  sum_of_non_native_cover_automatically_calculated = numeric(),
  count_of_unknown_species_automatically_calculated = numeric(),
  sum_of_unknown_cover_automatically_calculated = numeric(),
  sum_of_all_cover_automatically_calculated = numeric(),
  type = character(),
  species = character(),
  unlisted_species = character(),
  other_cover_notes = character(),
  percent_cover = numeric(),
  location_pool_id = character(),
  transect_axia = character(),
  spp_cal_flora = character(),
  stringsAsFactors = FALSE
)

# ==================================================
#                  User Interface               ----
# ==================================================
ui <- fluidPage(
  titlePanel("Add Data"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "df_input",
        label = "Select Data",
        choices = c("Hydrology", "Percent Cover")
      ),
      
      # input for hydrology df
      conditionalPanel(
        condition = "input.df_input == 'Hydrology'",
        textInput(inputId = "hydro_date", "Date (YYYY-MM-DD)"),
        textInput("hydro_location", "Location"),
        textInput("hydro_monitor_name", "Monitor Name"),
        textInput("hydro_pool_id", "Pool ID"),
        numericInput("water_level_in", "Water Level (in)", value = NA),
        textInput("optional_comments", "Optional Comments")
      ),
      
      # input for percent cover df
      conditionalPanel(
        condition = "input.df_input == 'Percent Cover'",
        textInput(inputId = "pc_date", "Date (YYYY-MM-DD)"),
        textInput("pc_location", "Location"),
        textInput("pc_pool_id", "Pool ID"),
        selectizeInput("pc_axis", "Transect Axis",
                       choices = c("major", "minor")),
        numericInput("pc_length", "Total Transect Length", value = NA, min = 0),
        textInput("pc_notes", "Transect Notes"),
        numericInput("pc_distance", "Transect Distance of Quadrat", value = NA, min = 0),
        selectizeInput("pc_transect_side", "Side of Transect",
                       choices = c("left", "right")),
        selectizeInput("pc_veg_zone", "Vegetation Zone",
                       choices = c("bottom", "edge", "transition")),
        numericInput("pc_bare_ground", "Percent Bare Ground", value = NA, min = 0),
        numericInput("pc_thatch", "Percent Thatch", value = NA, min = 0),
        numericInput("pc_cnt_native", "Count of Native Species", value = NA, min = 0),
        numericInput("pc_sum_native", "Sum of Native Cover", value = NA, min = 0),
        numericInput("pc_cnt_non", "Count of Non-Native Species", value = NA, min = 0),
        numericInput("pc_sum_non", "Sum of Non-Native Cover", value = NA, min = 0),
        numericInput("pc_cnt_unk", "Count of Unknown Species", value = NA, min = 0),
        numericInput("pc_sum_unk", "Sum of Unknown Cover", value = NA, min = 0),
        numericInput("pc_sum_all", "Sum of All Cover", value = NA, min = 0),
        selectizeInput("pc_type", "Type",
                       choices = c("Non-Native", "Native", "Unknown")),
        selectizeInput("pc_species", "Species",
                       choices = sort(unique(spp_df$species))),
        numericInput("pc_cover", "Percent Cover", value = NA)
      ),
      
      actionButton(inputId = "add_new_row", "Add Row"),
      actionButton(inputId = "delete_last_row", "Delete Row"),
      actionButton(inputId = "append_data", "Append Data")
    ),
    mainPanel(
      DTOutput("data_table")
    )
  )
)

# ==================================================
#                       Server                  ----
# ==================================================
server <- function(input, output, session) {
  
  # Create reactive values to store the data
  hydro_data <- reactiveVal(empty_hydro_df)
  pc_data <- reactiveVal(empty_pc_df)
  
  observeEvent(input$add_new_row, {
    
    # ....................... INPUT - Hydrology Data ...........................
    if (input$df_input == "Hydrology") {
      current_data <- hydro_data()
      new_row <- data.frame(
        object_id = NA, 
        global_id = NA,
        date = as.Date(input$hydro_date),
        location = input$hydro_location,
        unlisted_location = NA,
        monitor_name = input$hydro_monitor_name,
        pool_id = input$hydro_pool_id,
        water_level_in = as.numeric(input$water_level_in),
        optional_comments = input$optional_comments,
        creation_date = NA,
        creator = NA,
        edit_date = NA,
        editor = NA,
        x = NA,
        y = NA,
        water_year = water_year(as.Date(input$hydro_date)),
        location_pool_id = paste(input$hydro_location, input$hydro_pool_id, sep = " - "),
        stringsAsFactors = FALSE
      )
      updated_data <- rbind(current_data, new_row)
      hydro_data(updated_data)
      
      
      # ..................... INPUT - Percent Cover Data .........................
    } else if (input$df_input == "Percent Cover") {
      
      current_data <- pc_data()
      
      new_row <- data.frame(
        object_id = NA,
        global_id = NA,
        parent_global_id = NA,
        monitoring_date = as.Date(input$pc_date),
        location = input$pc_location,
        pool_id = input$pc_pool_id,
        transect_axis = input$pc_axis,
        total_transect_length = input$pc_length,
        transect_notes = input$pc_notes,
        transect_distance_of_quadrat = input$pc_distance,
        side_of_transect_left_or_right = input$pc_transect_side,
        vegetation_zone = input$pc_veg_zone,
        quadrat_notes = NA,
        percent_bare_ground = input$pc_bare_ground,
        percent_thatch = input$pc_thatch,
        count_of_native_species_automatically_calculated = input$pc_cnt_native,
        sum_of_native_cover_automatically_calculated = input$pc_sum_native,
        count_of_non_native_species_automatically_calculated = input$pc_cnt_non,
        sum_of_non_native_cover_automatically_calculated = input$pc_sum_non,
        count_of_unknown_species_automatically_calculated = input$pc_cnt_unk ,
        sum_of_unknown_cover_automatically_calculated = input$pc_sum_unk,
        sum_of_all_cover_automatically_calculated = input$pc_sum_all,
        type = input$pc_type,
        species = input$pc_species,
        unlisted_species = NA,
        other_cover_notes = NA,
        percent_cover = input$pc_cover,
        location_pool_id = paste(input$pc_location, input$pc_pool_id, sep = " - "),
        transect_axia = NA,
        spp_cal_flora = NA,
        stringsAsFactors = FALSE)
      
      updated_data <- rbind(current_data, new_row)
      pc_data(updated_data)
      
    }
  }) # END observeEvent for add_new_row
  
  # ..................... INPUT - Delete Last Row .........................
  
  observeEvent(input$delete_last_row, {
    
    if (input$df_input == "Hydrology") {
      current_data <- hydro_data()
      if (nrow(current_data) > 0) {
        updated_data <- current_data[-nrow(current_data), ]
        hydro_data(updated_data)
      }
    } else if (input$df_input == "Percent Cover") {
      current_data <- pc_data()
      if (nrow(current_data) > 0) {
        updated_data <- current_data[-nrow(current_data), ]
        pc_data(updated_data)
      }
    }
    
  }) # END observeEvent for delete_last_row
  
  
  # ..................... INPUT - Append to DF .........................
  
  observeEvent(input$append_data, {
    
    # Define current data & file path
    if (input$df_input == "Hydrology") {
      current_data <- hydro_data()
      file_path <- here("data", "hydro_data.csv")
      empty_df <- empty_hydro_df
      
    } else if (input$df_input == "Percent Cover") {
      current_data <- pc_data()
      file_path <- here("data", "percent_cover_data.csv")
      empty_df <- empty_pc_df
      
      # Error if no data type selected
    } else {
      showNotification("Invalid input type", type = "error")
      return()
    }
    
    if (nrow(current_data) > 0) {
      existing_data <- if (file.exists(file_path)) read_csv(file_path) else empty_df
      updated_data <- bind_rows(existing_data, current_data)
      
      tryCatch({
        write_csv(updated_data, file_path)
        if (input$df_input == "Hydrology") {
          hydro_data(empty_hydro_df)
        } else {
          pc_data(empty_pc_df)
        }
        showNotification(paste("Data appended to", basename(file_path), "Rows added:", nrow(current_data)), type = "message")
        print(paste(input$df_input, "data appended. Rows added:", nrow(current_data)))
      }, error = function(e) {
        showNotification("Error saving data to file.", type = "error")
        print(paste("Error writing to file:", e$message))
      })
    } else {
      showNotification("No new data to append", type = "warning")
    }
  })
  
  
  output$data_table <- renderDT({
    
    # ....................... OUTPUT - Hydrology Data ...........................
    if (input$df_input == "Hydrology") {
      
      hydro_data() %>% 
        select(date, location, monitor_name, 
               pool_id, water_level_in, optional_comments,
               water_year, location_pool_id)
      
      # ..................... OUTPUT - Percent Cover Data .........................
    } else if (input$df_input == "Percent Cover") {
      pc_data() %>% 
        select(monitoring_date, location, pool_id,
               transect_axis, total_transect_length, transect_notes,
               transect_distance_of_quadrat, side_of_transect_left_or_right,
               vegetation_zone, percent_bare_ground, percent_thatch,
               count_of_native_species_automatically_calculated,
               sum_of_native_cover_automatically_calculated,
               count_of_non_native_species_automatically_calculated,
               sum_of_non_native_cover_automatically_calculated,
               count_of_unknown_species_automatically_calculated,
               sum_of_unknown_cover_automatically_calculated,
               sum_of_all_cover_automatically_calculated,
               type, species, percent_cover, location_pool_id, spp_cal_flora)
    }
  }) # END renderDT
}

shinyApp(ui = ui, server = server)
