# Set working directory to the script's location (if running in RStudio)
tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    if (nchar(script_dir) > 0) {
      setwd(script_dir)
    }
  }
}, error = function(e) {
  # If not in RStudio, assume current working directory is correct
  cat("Note: Working directory not changed. Current:", getwd(), "\n")
})

# MEMORY OPTIMIZATION: Increase R memory limits for large TIFF files
# Check current memory limits
current_vsize <- memory.limit()
current_nsize <- memory.limit(size = NA)

cat("Current memory limits - VSize:", current_vsize, "MB, NSize:", current_nsize, "MB\n")

# Try to increase memory limits (Windows only)
if (.Platform$OS.type == "windows") {
  tryCatch({
    # Increase vector memory to 4GB if possible
    new_limit <- min(memory.limit(size = 4096), 8192)  # Try 4GB, max 8GB
    cat("Increased memory limit to:", new_limit, "MB\n")
  }, error = function(e) {
    cat("Could not increase memory limit:", e$message, "\n")
    cat("Consider restarting R and running: memory.limit(size = 4096)\n")
  })
} else {
  cat("Memory limit adjustment only available on Windows\n")
  cat("For large TIFF files, consider increasing R memory using ulimit or system settings\n")
}

# Force garbage collection to free any existing memory
gc(verbose = TRUE)

library(shiny)
library(png)
library(jpeg)
library(tiff) # For TIFF file support
library(abind) # For array binding operations
library(ggplot2)
library(grid) # For the rasterGrob function
library(Cardinal)

# Source the overlay plotting functions
# Try relative path first, then absolute path as fallback
if (file.exists("plot_Card_overlay_NEW (1).R")) {
  source("plot_Card_overlay_NEW (1).R")
} else if (file.exists("g:/r script (registration)/plot_Card_overlay_NEW (1).R")) {
  source("g:/r script (registration)/plot_Card_overlay_NEW (1).R")
} else {
  stop("Cannot find plot_Card_overlay_NEW (1).R file. Please ensure it's in the same directory as this script.")
}
#options(shiny.maxRequestSize = 50*1024^2)  # Set to 50MB or adjust as needed

# Define the maximum file size for uploads (100MB)
options(shiny.maxRequestSize=500*1024^2)

ui <- fluidPage(
  # Add JavaScript for arrow key functionality and CSS for button styling
  tags$head(
    tags$style(HTML("
      .transform-btn {
        margin: 1px;
        font-weight: bold;
        border-radius: 4px;
      }
      .arrow-btn {
        font-size: 16px;
        background-color: #337ab7;
        color: white;
        border: none;
      }
      .arrow-btn:hover {
        background-color: #286090;
      }
      .rotate-btn {
        background-color: #5cb85c;
        color: white;
        border: none;
      }
      .rotate-btn:hover {
        background-color: #449d44;
      }
      .scale-btn {
        background-color: #f0ad4e;
        color: white;
        border: none;
      }
      .scale-btn:hover {
        background-color: #ec971f;
      }
      .reset-btn {
        background-color: #5bc0de;
        color: white;
        border: none;
      }
      .reset-btn:hover {
        background-color: #31b0d5;
      }
      .center-btn {
        background-color: #777;
        color: white;
        border: none;
      }
      .center-btn:hover {
        background-color: #555;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function(){
        var step_size = 0.5; // Translation step size
        var rot_step = 1; // Rotation step size
        var scale_step = 0.01; // Scale step size
        
        $(document).keydown(function(e) {
          // Only capture keys when not in input fields
          if (!$(e.target).is('input, textarea, select')) {
            switch(e.which) {
              case 37: // left arrow
                if (e.shiftKey) {
                  // Shift + Left: Rotate counter-clockwise
                  var current = $('#rotate').val();
                  $('#rotate').val(parseFloat(current) - rot_step).trigger('change');
                } else if (e.ctrlKey) {
                  // Ctrl + Left: Scale X down
                  var current = $('#scalex').val();
                  $('#scalex').val(Math.max(0.1, parseFloat(current) - scale_step)).trigger('change');
                } else {
                  // Left: Move left
                  var current = $('#translate_x').val();
                  $('#translate_x').val(parseFloat(current) - step_size).trigger('change');
                }
                e.preventDefault();
                break;
              case 38: // up arrow
                if (e.shiftKey) {
                  // Shift + Up: Scale Y up
                  var current = $('#scaley').val();
                  $('#scaley').val(Math.min(5, parseFloat(current) + scale_step)).trigger('change');
                } else if (e.ctrlKey) {
                  // Ctrl + Up: Scale both up
                  var currentX = $('#scalex').val();
                  var currentY = $('#scaley').val();
                  $('#scalex').val(Math.min(5, parseFloat(currentX) + scale_step)).trigger('change');
                  $('#scaley').val(Math.min(5, parseFloat(currentY) + scale_step)).trigger('change');
                } else {
                  // Up: Move up
                  var current = $('#translate_y').val();
                  $('#translate_y').val(parseFloat(current) + step_size).trigger('change');
                }
                e.preventDefault();
                break;
              case 39: // right arrow
                if (e.shiftKey) {
                  // Shift + Right: Rotate clockwise
                  var current = $('#rotate').val();
                  $('#rotate').val(parseFloat(current) + rot_step).trigger('change');
                } else if (e.ctrlKey) {
                  // Ctrl + Right: Scale X up
                  var current = $('#scalex').val();
                  $('#scalex').val(Math.min(5, parseFloat(current) + scale_step)).trigger('change');
                } else {
                  // Right: Move right
                  var current = $('#translate_x').val();
                  $('#translate_x').val(parseFloat(current) + step_size).trigger('change');
                }
                e.preventDefault();
                break;
              case 40: // down arrow
                if (e.shiftKey) {
                  // Shift + Down: Scale Y down
                  var current = $('#scaley').val();
                  $('#scaley').val(Math.max(0.1, parseFloat(current) - scale_step)).trigger('change');
                } else if (e.ctrlKey) {
                  // Ctrl + Down: Scale both down
                  var currentX = $('#scalex').val();
                  var currentY = $('#scaley').val();
                  $('#scalex').val(Math.max(0.1, parseFloat(currentX) - scale_step)).trigger('change');
                  $('#scaley').val(Math.max(0.1, parseFloat(currentY) - scale_step)).trigger('change');
                } else {
                  // Down: Move down
                  var current = $('#translate_y').val();
                  $('#translate_y').val(parseFloat(current) - step_size).trigger('change');
                }
                e.preventDefault();
                break;
            }
          }
        });
      });
    "))
  ),
  # Use a sidebar layout with a panel for inputs and a main panel for the image output
  sidebarLayout(
    sidebarPanel(
      # File inputs
      fileInput("histology_upload", "Upload Histology Image", accept = c('image/png', 'image/jpeg', 'image/tiff', 'image/tif')),
      p("Note: For best results, use PNG format. If using TIFF, ensure it's not too large to avoid memory issues.", 
        style = "font-size: 12px; color: #666; margin-top: -10px;"),
      fileInput("msi_upload", "Upload msi dataset (.imzML file, and .ibd if separate)", accept = c('.imzML', '.ibd'), multiple = TRUE),
      p("Note: Balanced resolution (200 DPI) for good MSI quality. Histology overlay defaults to 60% scale for full image visibility. Standard size proportions (4:3 ratio).", 
        style = "font-size: 12px; color: #666; margin-top: -10px;"),
      sliderInput("scale_box", "Multiplier for images", min=0.1, max=10, value=1.0, step=0.1),
        # Add buttons for saving and loading settings
      downloadButton("save_button", "Save Settings"),
      fileInput("load_button", "Load Settings", accept = c(".rds")),
      
      
        # actionButton("save_button", "Save Settings"),
       actionButton("restore_settings", "Restore Settings"),
        # 
      
      
      
      # Sliders for image transformation - default to smaller scale for full visibility
      sliderInput("scalex", "Scale Image in X", min = 0.1, max = 5, value = 0.6, step=0.001),
      sliderInput("scaley", "Scale Image in Y", min = 0.1, max = 5, value = 0.6, step=0.001),
      p("Note: Default scale is 0.6 to show the entire histology image. Increase for larger overlay.", 
        style = "font-size: 11px; color: #666; margin-top: -5px; margin-bottom: 10px;"),
      sliderInput("rotate", "Rotate Image (degrees)", min = -180, max = 180, value = 0, step = 0.1),
      sliderInput("translate_x", "Translate Image X", min = -100, max = 100, value = 0, step=0.1),
      sliderInput("translate_y", "Translate Image Y", min = -100, max = 100, value = 0,step=0.1),
      
      # Physical transformation buttons
      hr(),
      h5("Quick Transform Buttons:"),
      fluidRow(
        column(6,
          h6("Translation:"),
          fluidRow(
            column(4),
            column(4, actionButton("move_up", "↑", width = "100%", 
                                 class = "transform-btn arrow-btn", 
                                 style = "margin-bottom: 2px;")),
            column(4)
          ),
          fluidRow(
            column(4, actionButton("move_left", "←", width = "100%", 
                                 class = "transform-btn arrow-btn")),
            column(4, actionButton("center_image", "⌂", width = "100%", 
                                 class = "transform-btn center-btn",
                                 title = "Center")),
            column(4, actionButton("move_right", "→", width = "100%", 
                                 class = "transform-btn arrow-btn"))
          ),
          fluidRow(
            column(4),
            column(4, actionButton("move_down", "↓", width = "100%", 
                                 class = "transform-btn arrow-btn",
                                 style = "margin-top: 2px;")),
            column(4)
          )
        ),
        column(6,
          h6("Rotation:"),
          fluidRow(
            column(6, actionButton("rotate_ccw", "↶ -5°", width = "100%", 
                                 class = "transform-btn rotate-btn")),
            column(6, actionButton("rotate_cw", "↷ +5°", width = "100%", 
                                 class = "transform-btn rotate-btn"))
          ),
          fluidRow(
            column(6, actionButton("rotate_ccw_fine", "↶ -1°", width = "100%", 
                                 class = "transform-btn rotate-btn",
                                 style = "margin-top: 2px;")),
            column(6, actionButton("rotate_cw_fine", "↷ +1°", width = "100%", 
                                 class = "transform-btn rotate-btn",
                                 style = "margin-top: 2px;"))
          ),
          fluidRow(
            column(12, actionButton("reset_rotation", "Reset Rotation", width = "100%", 
                                  class = "transform-btn reset-btn",
                                  style = "margin-top: 2px;"))
          )
        )
      ),
      fluidRow(
        column(6,
          h6("Scaling:"),
          fluidRow(
            column(6, actionButton("scale_up", "Scale + 0.1", width = "100%", 
                                 class = "transform-btn scale-btn")),
            column(6, actionButton("scale_down", "Scale - 0.1", width = "100%", 
                                 class = "transform-btn scale-btn"))
          ),
          fluidRow(
            column(6, actionButton("scale_up_fine", "Scale + 0.01", width = "100%", 
                                 class = "transform-btn scale-btn",
                                 style = "margin-top: 2px;")),
            column(6, actionButton("scale_down_fine", "Scale - 0.01", width = "100%", 
                                 class = "transform-btn scale-btn",
                                 style = "margin-top: 2px;"))
          ),
          fluidRow(
            column(12, actionButton("reset_scale", "Reset Scale", width = "100%", 
                                  class = "transform-btn reset-btn",
                                  style = "margin-top: 2px;"))
          )
        ),
        column(6,
          h6("Reset All:"),
          fluidRow(
            column(12, actionButton("reset_all_transforms", "Reset All Transforms", 
                                  width = "100%", 
                                  style = "background-color: #d9534f; color: white; margin-top: 10px; font-weight: bold;"))
          )
        )
      ),
      
      checkboxInput("spatial", "Spatial plot only", value=T),
      checkboxInput("debug", "Debug plot?", value=F),
      
      # Arrow key instructions
      hr(),
      h5("Keyboard Shortcuts:"),
      tags$div(
        style = "font-size: 12px; color: #666;",
        tags$ul(
          tags$li("Arrow Keys: Move image"),
          tags$li("Shift + Arrow Keys: Rotate (left/right) or Scale Y (up/down)"),
          tags$li("Ctrl + Arrow Keys: Scale X (left/right) or Scale both (up/down)")
        )
      )
      # # Adding inputs for zoom area in your sidebarPanel
      # sliderInput("zoom_xmin", "Zoom X min", min = 0, max = 100, value = 0),
      # sliderInput("zoom_xmax", "Zoom X max", min = 0, max = 100, value = 100),
      # sliderInput("zoom_ymin", "Zoom Y min", min = 0, max = 100, value = 0),
      # sliderInput("zoom_ymax", "Zoom Y max", min = 0, max = 100, value = 100)
      
    ),
    
    mainPanel(
      # Output the overlay plot
      #imageOutput("overlayImage"),
      plot_card_UI("hist_plot_card"),
      sliderInput("alpha", "Adjust Image Transparency", min = 0, max = 1, value = 0.5)
    )
  )
)
server <- function(input, output, session) {
  
  # Create a reactiveValues object
  allInputs <- reactiveValues()
  
  # Track when MSI data is successfully loaded
  msi_data_ready <- reactiveVal(FALSE)
  
  # Use an observe function to update the reactiveValues object whenever inputs change
  observe({
    # This loop copies all input values to the reactiveValues object
    inputList <- reactiveValuesToList(input)
    for (name in names(inputList)) {
      allInputs[[name]] <- inputList[[name]]
    }
  })
  
  observe({
    req(input$histology_upload)
    req(input$msi_upload)
    
    # Handle imzML file upload with improved error handling
    uploaded_files <- input$msi_upload
    
    # Debug info
    if (input$debug) {
      cat("Uploaded files:\n")
      print(uploaded_files)
    }
    
    # Find the .imzML file
    imzml_file <- uploaded_files[grepl("\\.imzML$", uploaded_files$name, ignore.case = TRUE), ]
    
    if (nrow(imzml_file) == 0) {
      showNotification("Please upload an .imzML file", type = "error")
      return()
    }
    
    # Check if there's also an .ibd file uploaded
    ibd_file <- uploaded_files[grepl("\\.ibd$", uploaded_files$name, ignore.case = TRUE), ]
    
    imzml_path <- imzml_file$datapath[1]
    
    if (input$debug) {
      cat("imzML path:", imzml_path, "\n")
      cat("File exists:", file.exists(imzml_path), "\n")
    }
    
    # If .ibd file is uploaded, copy it to the expected location
    if (nrow(ibd_file) > 0) {
      ibd_path <- ibd_file$datapath[1]
      expected_ibd_path <- sub("\\.imzML$", ".ibd", imzml_path, ignore.case = TRUE)
      
      if (input$debug) {
        cat("ibd_path:", ibd_path, "\n")
        cat("expected_ibd_path:", expected_ibd_path, "\n")
      }
      
      success <- file.copy(ibd_path, expected_ibd_path, overwrite = TRUE)
      if (!success) {
        showNotification("Failed to process .ibd file", type = "error")
        return()
      }
    }
    
    # Try to read the MSI data with better error handling
    tryCatch({
      dat_in <- readMSIData(imzml_path)
      
      # Only call plot_card_server if MSI data was successfully loaded
      if (!is.null(dat_in)) {
        plot_card_server("hist_plot_card", dat_in, spatialOnly=input$spatial, allInputs=allInputs)
        msi_data_ready(TRUE)  # Set flag to indicate MSI data is ready
        showNotification("MSI data loaded successfully!", type = "message")
      }
    }, error = function(e) {
      error_msg <- e$message
      if (input$debug) {
        cat("MSI reading error:", error_msg, "\n")
      }
      
      if (grepl("cannot find the file|file does not exist", error_msg, ignore.case = TRUE)) {
        showNotification("Missing .ibd file - please upload both .imzML and .ibd files together", type = "error")
      } else if (grepl("not supported|unsupported", error_msg, ignore.case = TRUE)) {
        showNotification("Unsupported MSI file format", type = "error")
      } else {
        showNotification(paste("Error reading MSI data:", error_msg), type = "error")
      }
    })
  })
  
  # To save the settings to disk
  output$save_button <- downloadHandler(
    filename = function() {
      paste("settings-", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      settings_to_save <- list(
        scalex = input$scalex,
        scaley = input$scaley,
        rotate = input$rotate,
        translate_x = input$translate_x,
        translate_y = input$translate_y,
        alpha = input$alpha
      )
      saveRDS(settings_to_save, file)
    }
  )
  
  observeEvent(input$load_button, {
    req(input$load_button) # require that a file is input
    
    settings_to_load <- readRDS(input$load_button$datapath)
    
    # Update the UI inputs with loaded settings
    updateSliderInput(session, "scalex", value = settings_to_load$scalex)
    updateSliderInput(session, "scaley", value = settings_to_load$scaley)
    updateSliderInput(session, "rotate", value = settings_to_load$rotate)
    updateSliderInput(session, "translate_x", value = settings_to_load$translate_x)
    updateSliderInput(session, "translate_y", value = settings_to_load$translate_y)
    updateSliderInput(session, "alpha", value = settings_to_load$alpha)
    
    showNotification("Settings loaded successfully!", type = "message")
  })
  
  observeEvent(input$restore_settings, {
    # Reset to default values - use 0.6 scale for full histology visibility
    updateSliderInput(session, "scalex", value = 0.6)
    updateSliderInput(session, "scaley", value = 0.6)
    updateSliderInput(session, "rotate", value = 0)
    updateSliderInput(session, "translate_x", value = 0)
    updateSliderInput(session, "translate_y", value = 0)
    updateSliderInput(session, "alpha", value = 0.5)
    
    showNotification("Settings restored to defaults!", type = "message")
  })
  
  # Button event handlers for rigid transformations
  
  # Translation buttons
  observeEvent(input$move_up, {
    updateSliderInput(session, "translate_y", value = input$translate_y + 1)
  })
  
  observeEvent(input$move_down, {
    updateSliderInput(session, "translate_y", value = input$translate_y - 1)
  })
  
  observeEvent(input$move_left, {
    updateSliderInput(session, "translate_x", value = input$translate_x - 1)
  })
  
  observeEvent(input$move_right, {
    updateSliderInput(session, "translate_x", value = input$translate_x + 1)
  })
  
  observeEvent(input$center_image, {
    updateSliderInput(session, "translate_x", value = 0)
    updateSliderInput(session, "translate_y", value = 0)
  })
  
  # Rotation buttons
  observeEvent(input$rotate_cw, {
    new_value <- input$rotate + 5
    if (new_value > 180) new_value <- new_value - 360
    updateSliderInput(session, "rotate", value = new_value)
  })
  
  observeEvent(input$rotate_ccw, {
    new_value <- input$rotate - 5
    if (new_value < -180) new_value <- new_value + 360
    updateSliderInput(session, "rotate", value = new_value)
  })
  
  observeEvent(input$rotate_cw_fine, {
    new_value <- input$rotate + 1
    if (new_value > 180) new_value <- new_value - 360
    updateSliderInput(session, "rotate", value = new_value)
  })
  
  observeEvent(input$rotate_ccw_fine, {
    new_value <- input$rotate - 1
    if (new_value < -180) new_value <- new_value + 360
    updateSliderInput(session, "rotate", value = new_value)
  })
  
  observeEvent(input$reset_rotation, {
    updateSliderInput(session, "rotate", value = 0)
  })
  
  # Scaling buttons
  observeEvent(input$scale_up, {
    updateSliderInput(session, "scalex", value = min(5, input$scalex + 0.1))
    updateSliderInput(session, "scaley", value = min(5, input$scaley + 0.1))
  })
  
  observeEvent(input$scale_down, {
    updateSliderInput(session, "scalex", value = max(0.1, input$scalex - 0.1))
    updateSliderInput(session, "scaley", value = max(0.1, input$scaley - 0.1))
  })
  
  observeEvent(input$scale_up_fine, {
    updateSliderInput(session, "scalex", value = min(5, input$scalex + 0.01))
    updateSliderInput(session, "scaley", value = min(5, input$scaley + 0.01))
  })
  
  observeEvent(input$scale_down_fine, {
    updateSliderInput(session, "scalex", value = max(0.1, input$scalex - 0.01))
    updateSliderInput(session, "scaley", value = max(0.1, input$scaley - 0.01))
  })
  
  observeEvent(input$reset_scale, {
    updateSliderInput(session, "scalex", value = 0.6)
    updateSliderInput(session, "scaley", value = 0.6)
  })
  
  # Reset all transformations
  observeEvent(input$reset_all_transforms, {
    updateSliderInput(session, "scalex", value = 0.6)
    updateSliderInput(session, "scaley", value = 0.6)
    updateSliderInput(session, "rotate", value = 0)
    updateSliderInput(session, "translate_x", value = 0)
    updateSliderInput(session, "translate_y", value = 0)
    showNotification("All transformations reset!", type = "message")
  })
  
  # Diagnostic code for testing file processing (uncomment to use):
  # # Test histology image reading
  # test_histology <- function(file_path) {
  #   cat("Testing histology file:", file_path, "\n")
  #   cat("File exists:", file.exists(file_path), "\n")
  #   file_type <- tolower(tools::file_ext(file_path))
  #   cat("File type detected:", file_type, "\n")
  #   
  #   if (file_type %in% c("png", "jpg", "jpeg", "tiff", "tif")) {
  #     tryCatch({
  #       img <- switch(file_type,
  #                     "png" = png::readPNG(file_path),
  #                     "jpg" = jpeg::readJPEG(file_path),
  #                     "jpeg" = jpeg::readJPEG(file_path),
  #                     "tiff" = tiff::readTIFF(file_path),
  #                     "tif" = tiff::readTIFF(file_path))
  #       cat("Successfully read image. Dimensions:", dim(img), "\n")
  #       return(TRUE)
  #     }, error = function(e) {
  #       cat("Error reading image:", e$message, "\n")
  #       return(FALSE)
  #     })
  #   } else {
  #     cat("Unsupported file type\n")
  #     return(FALSE)
  #   }
  # }
  # 
  # # Test MSI data reading
  # test_msi <- function(imzml_path, ibd_path = NULL) {
  #   cat("Testing MSI files:", imzml_path, "\n")
  #   if (!is.null(ibd_path)) {
  #     cat("IBD file:", ibd_path, "\n")
  #   }
  #   
  #   tryCatch({
  #     dat <- readMSIData(imzml_path)
  #     cat("Successfully read MSI data. Dimensions:", dim(dat), "\n")
  #     return(TRUE)
  #   }, error = function(e) {
  #     cat("Error reading MSI data:", e$message, "\n")
  #     return(FALSE)
  #   })
  # }
  # Commented out unused overlayImage - the actual overlay is handled by plot_card_server
  # output$overlayImage <- renderImage({
  #   # This code was causing conflicts with the main overlay functionality
  #   # The overlay is now handled entirely by plot_card_server in plot_Card_overlay_NEW (1).R
  # }, deleteFile = TRUE) 
}

# Run the application
shinyApp(ui, server)
