library(shiny)
library(Cardinal)
library(sf)
library(stringr)
library(ggplot2)
#library(shinyWidgets)
library(colourpicker)  # Load colourpicker for colourInput

library(jsonlite)
library(dplyr)


options(shiny.maxRequestSize = 50*1024^2)  # Set to 50MB or adjust as needed

# plot function, mainly for debugging to plot points and polygons together
plot_overlay<-function(moving, static) {
  #get max and min x and y from both sf files
  bbox_moving<-st_bbox(moving)
  bbox_static<-st_bbox(static)
  
  #find overall xmin / xmax
  xmin<-min(c(bbox_moving$xmin, bbox_static$xmin))
  xmax<-max(c(bbox_moving$xmax, bbox_static$xmax))
  
  #find overall ymin / ymax
  ymin<-min(c(bbox_moving$ymin, bbox_static$ymin))
  ymax<-max(c(bbox_moving$ymax, bbox_static$ymax))
  
  # Define color with transparency using rgb() function
  # The fourth argument (alpha) is transparency, where 1 is fully opaque and 0 is fully transparent.
  transparent_red <- rgb(1, 0, 0, alpha = 0.03)
  
  plot(sf::st_coordinates(moving), pch=".", xlim=c(xmin, xmax), ylim=c(ymax, ymin))
  par(new=TRUE)
  plot(sf::st_coordinates(static), col=transparent_red, pch=20, xlim=c(xmin, xmax), ylim=c(ymax, ymin))
}


#for Cardinal 3.6+ the combine function doesn't work the same way
#in many case can use do.call(cbind) to combine from a list, but doesn't work with some datasets
#this function is a workaround to combine datasets

combine_card <- function(x) {
  
  if (length(x) == 1) {
    return(x[[1]])
  }
  if (length(x) == 0) {
    return(NULL)
  }
  if (length(x) > 1) {
    
    #get run order to preserve order of runs
    run_order <- unlist(lapply(x, runNames))
    
    #reorder list based on ncol
    x <- x[order(unlist(lapply(x, ncol)), decreasing = TRUE)]
    
    tmp <- cbind(x[[1]])
    for (i in 2:length(x)) {
      tmp <- cbind(tmp, x[[i]])
    }
    
    #reorder pixels based on orginal order
    pData(tmp)$run <- factor(run(tmp), levels = run_order)
    
    return(tmp)
  }
}


server <- function(input, output, session) {
  
  #setwd("~/Box Sync/DESI (Aalim Weljie)/Qupath_viz/")
  #setwd("/Users/aalim/Desktop/DESI_Data_Copy/Brains_Dan/HI-res_images")
  #setwd("/Users/aalim/Desktop/DESI_Data_Copy/Brains_Dan/10um_brain_test")
  setwd('/Users/danboehm/Desktop/HCC Cells High Res/02_2um_Pixels_20raster_06kVvoltage')
  
  
  has.new.files <- function() {
    unique(list.files(getwd()))
  }
  get.files <- function() {
    list.files(getwd(), recursive = T)
  }
  
  # store as a reactive instead of output
  my_files <- reactivePoll(10, session=NULL, checkFunc=has.new.files, valueFunc=get.files)
  
  # any time the reactive changes, update the selectInput
  observeEvent(my_files(),ignoreInit = T,ignoreNULL = T, {
    print(grep("rds|RData|imzML", my_files(), ignore.case =T, value=T  ))
    updateSelectInput( inputId = 'peakPickfile',choices = grep("rds|RData|.imzML", my_files(), ignore.case =T, value=T))
  })
  
  
  x0<-reactiveValues(
    data_set_storage=NULL,
    region_table=NULL,
    transform_values=NULL
    
  )
  
  multipolygon_data <- reactive({
    if (is.null(input$multipolygon_file)) {
      return(NULL)
    }
    #browser()
    tryCatch({
      # Use sf to read the GeoJSON
      geojson <- sf::st_read(input$multipolygon_file$datapath)
      return(geojson)
    }, error = function(e) {
      warning("Error reading GeoJSON file: ", e$message)
      return(NULL)
    })
  })
  
  multipoint_data_files <- reactive({
    if (is.null(input$peakPickfile)) {
      return(NULL)
    }
    
    tryCatch({
      #check for rds in filename
      if(grepl("rds", input$peakPickfile, ignore.case = T)){
        
        #browser()
        rds_data <- readRDS(input$peakPickfile)
        if(class(rds_data)!="list"){
          rds_data<-list(rds_data)
          names(rds_data)<-input$peakPickfile
        }
        
        x0$data_set_storage <- rds_data
        return(rds_data)
      } else if(grepl("imzML", input$peakPickfile, ignore.case = T)){
        
        #browser()
        imzml_data <- readImzML(input$peakPickfile)
        if(class(imzml_data)!="list"){
          imzml_data<-list(imzml_data)
          names(imzml_data)<-input$peakPickfile
        }
        
        x0$data_set_storage <- imzml_data
        return(imzml_data)
      }
    }, error = function(e) {
      warning("Error reading RDS file: ", e$message)
      return(NULL)
    })
  })
  
  # Use observeEvent to load the dataset on pressing 'load_data'
  observeEvent(input$load_data, {
    req(input$peakPickfile)
    # Check and load the selected dataset
    multipoint_data_files() 
  })
  
  output$mz <- renderUI({
    req(multipoint_data_files())
    req(input$coordinates)
    
    
    available_mz <- Cardinal::mz(multipoint_data_files()[[input$coordinates]])
    if (is.null(available_mz) || length(available_mz) == 0) {
      return(NULL) # No data available, don't render the UI element
    }
    print(head(available_mz))
    selectInput("mz", "m/z value to display", choices = round(available_mz,4), selected = 303.2324)
  })
  
  output$pk_file <- renderUI({
    req(my_files())
    
    available_files <- grep("rds|RData|.imzML", my_files(), ignore.case = T, value = T)
    if (length(available_files) == 0) {
      return(NULL) # No files available, so don't render the UI element
    }
    
    selectInput(inputId = "peakPickfile", label = "Imageset with picked peaks", choices = available_files)
  })
  
  output$coordinates <- renderUI({
    req(multipoint_data_files())
    available_coordinates <- names(multipoint_data_files())
    if (is.null(available_coordinates) || length(available_coordinates) == 0) {
      return(NULL) # No data available, don't render the UI element
    }
    
    selectInput("coordinates", "Imageset with coordinates", choices = available_coordinates)
  })
  
  multipoint_data <- reactive({
    if (is.null(multipoint_data_files())) {
      return(NULL)
    }
    req(input$coordinates)
    
    # Extract the data as plain coordinates
    data <- as.data.frame(Cardinal::coord(multipoint_data_files()[[input$coordinates]]))
    
    # Skip any CRS assignment and return directly
    return(data)
  })
  
  transformed_data <- reactive({
    # Ensure that both datasets are available
    req(multipolygon_data(), multipoint_data())
    
    # Get the original multipolygon data (should already be an sf object)
    mp_data <- multipolygon_data()
    
    # Get the original multipoint data and ensure it's an sf object
    mpp_data <- multipoint_data()
    if (!inherits(mpp_data, "sf")) {
      mpp_data <- sf::st_as_sf(mpp_data, coords = c("x", "y"), crs = st_crs(mp_data))
    }
    
    # Ensure both datasets share the same CRS
    st_crs(mpp_data) <- st_crs(mp_data)
    
    # If ignore_orig_coords is checked, reset coordinates to center
    if (isTRUE(input$ignore_orig_coords)) {
      # Calculate centers for both mp_data and mpp_data
      bbox_mp <- sf::st_bbox(mp_data)
      center_mp <- c((bbox_mp$xmin + bbox_mp$xmax) / 2, (bbox_mp$ymin + bbox_mp$ymax) / 2)
      
      #bbox_mpp <- sf::st_bbox(mpp_data)
      #center_mpp <- c((bbox_mpp$xmin + bbox_mpp$xmax) / 2, (bbox_mpp$ymin + bbox_mpp$ymax) / 2)
      
      # Shift both datasets to center
      mp_data <- sf::st_set_geometry(mp_data, sf::st_geometry(mp_data) - center_mp)
      #mpp_data <- sf::st_set_geometry(mpp_data, sf::st_geometry(mpp_data) - center_mpp)
    }
    
    # Check if mp_data has rows to iterate over
    if (nrow(mp_data) == 0) {
      warning("mp_data has no rows after centering.")
      return(NULL)
    }
    
    # Extract and transform coordinates polygon by polygon
    transformed_geometry <- lapply(seq_len(nrow(mp_data)), function(i) {
      # Extract coordinates for the ith row based on L1
      row_coords <- sf::st_coordinates(mp_data[i, ])
      
      # If no coordinates are found, skip this row
      if (nrow(row_coords) == 0) {
        return(NULL)
      }
      
      # Apply transformations to each polygon separately
      rot = function(a) matrix(c(cos(a), -sin(a), sin(a), cos(a)), 2, 2)
      rot_angle = input$rotation * pi / 180  # Convert degrees to radians
      rotation_matrix = rot(rot_angle)
      
      # Rotate, scale, and translate coordinates
      # Note: The order of operations is important
      # Rotate after scaling to preserve registration
      row_coords[, 1] <- row_coords[, 1] * input$x_scaling + input$x_translation  # Scale & Translate X
      row_coords[, 2] <- row_coords[, 2] * input$y_scaling + input$y_translation  # Scale & Translate Y
      row_coords[, 1:2] <- t(rotation_matrix %*% t(row_coords[, 1:2]))  # Rotate X and Y
      
      # Check if L3 is present
      if ("L3" %in% colnames(row_coords)) {
        # Group by L3 to handle multiple sub-polygons
        polygons <- lapply(unique(row_coords[, "L3"]), function(L3_value) {
          sub_polygon_coords <- row_coords[row_coords[, "L3"] == L3_value, ]
          
          # Group by L2 to handle outer/inner rings
          rings <- lapply(unique(sub_polygon_coords[, "L2"]), function(L2_value) {
            ring_coords <- sub_polygon_coords[sub_polygon_coords[, "L2"] == L2_value, 1:2]
            
            # Ensure the ring is closed
            if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
              ring_coords <- rbind(ring_coords, ring_coords[1, ])  # Close the ring
            }
            as.matrix(ring_coords)
          })
          sf::st_polygon(rings)
        })
      } else {
        # No L3, so only group by L2 to create a single polygon
        rings <- lapply(unique(row_coords[, "L2"]), function(L2_value) {
          ring_coords <- row_coords[row_coords[, "L2"] == L2_value, 1:2]
          
          # Ensure the ring is closed
          if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
            ring_coords <- rbind(ring_coords, ring_coords[1, ])  # Close the ring
          }
          as.matrix(ring_coords)
        })
        
        # Create a single polygon from the rings
        polygons <- list(sf::st_polygon(rings))
      }
      
      # Return either a single polygon or a multipolygon
      if (length(polygons) == 1) {
        polygons[[1]]
      } else {
        sf::st_multipolygon(polygons)
      }
    })
    
    # Combine all transformed geometries into a single sfc object
    final_geometry <- sf::st_sfc(transformed_geometry, crs = st_crs(mp_data))
    
    # Update the geometry in the sf object
    mp_data_transformed <- sf::st_set_geometry(mp_data, final_geometry)
    
    # Return the transformed data (both multipolygon and multipoint)
    return(list(mp_data = mp_data_transformed, mpp_data = mpp_data))
  })
  
  
  observeEvent(input$apply_estimate, {
    req(transformed_data(), multipoint_data())
    
    # Get the already transformed data from the reactive value
    transformed_sf <- transformed_data()$mp_data
    mpp_data <- multipoint_data()
    
    
    
    
    # Calculate bounding box dimensions for both the transformed dataset and the points
    bbox_mp <- sf::st_bbox(transformed_sf)
    bbox_pts <- sf::st_bbox(sf::st_as_sf(mpp_data, coords = c("x", "y")))
    
    # Calculate the center of each bounding box
    center_mp_x <- (bbox_mp$xmin + bbox_mp$xmax) / 2
    center_mp_y <- (bbox_mp$ymin + bbox_mp$ymax) / 2
    center_pts_x <- (bbox_pts$xmin + bbox_pts$xmax) / 2
    center_pts_y <- (bbox_pts$ymin + bbox_pts$ymax) / 2
    
    # Calculate the translation to align the centers of the bounding boxes
    translation_x <- center_pts_x - center_mp_x
    translation_y <- center_pts_y - center_mp_y
    
    # Estimate scaling factors based on the bounding box sizes if requested
    if(input$scale_est){
      scale_x <- ifelse(bbox_mp$xmax != bbox_mp$xmin, 
                        (bbox_pts$xmax - bbox_pts$xmin) / (bbox_mp$xmax - bbox_mp$xmin), 
                        1)
      scale_y <- ifelse(bbox_mp$ymax != bbox_mp$ymin, 
                        (bbox_pts$ymax - bbox_pts$ymin) / (bbox_mp$ymax - bbox_mp$ymin), 
                        1)
    } else {
      scale_x <- input$x_scaling
      scale_y <- input$y_scaling
    }
    
    # Set transformation values to the inputs to avoid toggling
    updateNumericInput(session, "x_translation", value = as.numeric(translation_x))
    updateNumericInput(session, "y_translation", value = as.numeric(translation_y))
    updateNumericInput(session, "x_scaling", value = as.numeric(scale_x))
    updateNumericInput(session, "y_scaling", value = as.numeric(scale_y))
  })
  
  
  output$plot <- renderImage({
    req(multipolygon_data(), multipoint_data())
    
    
    
    # If the data is still NULL, do not proceed
    if (is.null(multipolygon_data()) || is.null(multipoint_data())) {
      return(NULL)
    }
    
    
    # Create a temporary file to save the plot
    temp_file <- tempfile(fileext = ".png")
    
    # Proceed with plotting if all data is available
    mpp_data <- multipoint_data()
    bbox_pts <- sf::st_bbox(sf::st_as_sf(mpp_data, coords = c("x", "y")))
    
    # Calculate the width and height based on the bounding box, adding 10% margin
    width <- as.numeric((bbox_pts$xmax - bbox_pts$xmin) * 10.1)
    height <- as.numeric((bbox_pts$ymax - bbox_pts$ymin) * 10.1)
    aspect_ratio <- width / height
    
    
    # Set the dimensions and resolution dynamically for the PNG file
    #png(filename = temp_file, width = width, height = height, res = 100)  # Adjust resolution as needed
    
    
    #print width and height to console
    # print(width)
    # print(height)
    
    
    # Use static coordinates from `aa` as the reference
    dat_show <- multipoint_data_files()
    dat_show <- dat_show[[input$coordinates]]
    req(dat_show)
    mz(dat_show) <- round(mz(dat_show), 4)
    req(input$mz)
    
    #browser()
    # Static coordinates for plotting
    test_image <- Cardinal::image(dat_show, mz = as.numeric(input$mz), enhance = "histogram", smooth="guided")
    test_pixels <- test_image$plots[[1]]$marks$pixels$encoding
    static_coords <- data.frame(x = test_pixels$x, y = test_pixels$y, color = test_pixels$color)
    
    #browser()
    
    # Extract transformed coordinates
    transformed_sf <- transformed_data()$mp_data
    
    # Check if a geometry is empty
    is_empty <- sf::st_is_empty(transformed_sf)
    
    # Filter out empty geometries
    non_empty_geometries <- transformed_sf[!is_empty, ]
    
    # Now ensure that all remaining geometries are polygons or multipolygons
    is_polygon <- sf::st_is(non_empty_geometries, c("POLYGON", "MULTIPOLYGON"))
    
    # Filter to keep only polygons
    polygon_geometries <- non_empty_geometries[is_polygon, ]
    
    # Cast all geometries to MULTIPOLYGON
    polygon_geometries <- sf::st_cast(polygon_geometries, "MULTIPOLYGON")
    
    # Extract coordinates from the polygon geometries
    transformed_coords_matrix <- sf::st_coordinates(polygon_geometries)
    # Convert transformed coordinates to a data frame for plotting
    transformed_coords <- data.frame(x = transformed_coords_matrix[, 1], y = transformed_coords_matrix[, 2])
    
    #browser()
    # Plot using ggplot2
    
    
    if(input$plot_type=="area"){
      #browser()
      
      #look for x0$data_set_storage[[input$coordinates]] first
      if(!is.null(x0$data_set_storage[[input$coordinates]])){
        static_coords$color<-pData(x0$data_set_storage[[input$coordinates]])[input$region_select][,1]
      } else {
        static_coords$color<- pData(dat_show)[input$region_select][,1]
      }
      
      p1<-ggplot() +
        # Original points (static coordinates)
        geom_tile(data = static_coords, aes(x = x, y = y, color = (color)), linewidth = 1, alpha = 1-input$alpha) +
        # Transformed points overlay
        geom_point(data = transformed_coords, aes(x = x, y = y), 
                   color = input$overlay_color, size = input$overlay_size, shape = 16, alpha = input$alpha) +
        #scale_color_gradient(low = "blue", high = "red") +
        #scale_color_viridis_c()+
        theme_minimal() +
        ggtitle("Overlay of Static and Transformed Coordinates") +
        xlab("X Coordinate") +
        ylab("Y Coordinate") +
        theme(legend.position = "bottom")+
        scale_y_reverse()
    } else {
      
      #browser()
      
      
      if(input$median_filter) {
        #browser()
        static_coords_orig<-static_coords
        # Ensure x and y are numeric
        static_coords$x <- as.numeric(as.character(static_coords$x))
        static_coords$y <- as.numeric(as.character(static_coords$y))
        # Step 1: Convert your data to a matrix
        # Normalize the 'color' values to range [0, 1]
        static_coords$color <- (static_coords_orig$color - min(static_coords_orig$color)) / (max(static_coords_orig$color) - min(static_coords_orig$color))
        
        # Assuming your data is in a data frame called 'image_data'
        # Create a matrix with rows as `y` and columns as `x`
        image_matrix <- with(static_coords, tapply(color, list(y, x), identity))
        
        #replace NAs with 0
        image_matrix[is.na(image_matrix)] <- 0  # Replace with 0
        
        
        # Step 2: Apply the median filter
        # Define the filter size (e.g., 3x3 neighborhood)
        filtered_image <- EBImage::medianFilter(image_matrix, size = input$filter_size)
        
        if(diff(range(filtered_image))==0){
          showNotification("filtering not succesful, reverting to original image")
          static_coords$color<-static_coords_orig$color
        } else {
          
          # Step 3: Visualize the result
          # Display the original and filtered images
          #EBImage::display(normalize(image_matrix), title = "Original Image")
          #EBImage::display(normalize(filtered_image), title = "Filtered Image")
          
          # Step 4: (Optional) Convert the filtered image back to a data frame if needed
          static_coords <- as.data.frame(as.table(filtered_image))
          colnames(static_coords) <- c("y", "x", "color")
          # Ensure x and y are numeric
          static_coords$x <- as.numeric(as.character(static_coords$x))
          static_coords$y <- as.numeric(as.character(static_coords$y))
          
        }
        
        
        
        
      }
      
      # Assuming `aa$color` contains the color intensity values to normalize
      color_values <- static_coords$color
      
      
      if(sum(color_values)==0) {
        showNotification("No color values detected in the static coordinates, please choose anotehr m/z value", type = "warning")
        message("No color values detected in the static coordinates")
        message("Please choose another m/z value")
        return(NULL)
      }
      
      # Step 1: Compute the histogram
      num_bins <- 256  # Choose the number of bins for the histogram
      hist_result <- hist(color_values, breaks = num_bins, plot = FALSE)
      
      # Step 2: Compute the cumulative distribution function (CDF)
      cdf <- cumsum(hist_result$counts) / sum(hist_result$counts)
      
      #browser()
      # Step 3: Map the original values to the normalized values
      # Find the indices in the original values that correspond to each CDF value
      normalized_values <- approx(hist_result$mids, cdf, color_values, rule = 2)$y
      
      # Now `normalized_values` contains the histogram normalized values of `aa$color`
      # You may want to scale these values to a desired range, for example, 0 to 1 or 0 to 255
      normalized_values_scaled <- normalized_values * 255  # Scale to 0-255 for image intensity
      
      # Replace `aa$color` with the normalized values if needed
      static_coords$color_normalized <- normalized_values_scaled
      
      # browser()
      # Plot with ggplot2 using user-selected color and shape
      p1 <- ggplot() +
        geom_tile(data = static_coords, aes(x = x, y = y, fill = color_normalized), linewidth = 1, alpha = 1-input$alpha) +
        geom_point(data = transformed_coords, aes(x = x, y = y), 
                   color = input$overlay_color, size = input$overlay_size, shape = 16, alpha = input$alpha) +
        theme_minimal() +
        ggtitle("Overlay of Static and Transformed Coordinates") +
        xlab("X Coordinate") +
        ylab("Y Coordinate") +
        scale_y_reverse() +
        scale_fill_viridis_c(option = input$msi_color)+ #, limits = c(-1, 5)) +
        theme(legend.position = "none")
      
    }
    
    #print(p1)
    # Save the plot with ggsave
    ggsave(filename = temp_file, plot = p1, width = 2000, height = 2000, dpi = 300, units="px")
    
    #dev.off()
    
    # Return a list containing the filename, width, height, and content type of the image
    list(
      src = temp_file,
      contentType = 'image/png',
      width = 2000,
      height = 2000,
      alt = "This is a tile plot with enhanced contrast"
    )
  }, deleteFile = TRUE)
  
  
  # # Plot the transformed data
  # output$plotxx <- renderImage({
  #   # Create a temporary file to save the plot
  #   temp_file <- tempfile(fileext = ".png")
  #   
  #   # Set the dimensions and resolution for the PNG file
  #   png(filename = temp_file, width = 800, height = 600, res = 100)  # Adjust dimensions and resolution
  #   
  #   
  #   
  #   req(transformed_data())
  #   req(input$coordinates)
  #   
  #     data <- transformed_data()
  #    
  #     dat_show<-multipoint_data_files()
  #     dat_show<-dat_show[[input$coordinates]]
  #     req(dat_show)
  #     mz(dat_show)<-round(mz(dat_show), 4)
  #     
  #     browser()
  #     a<-Cardinal::image(dat_show, mz=303.2324, contrast.enhance="histogram")
  #     
  #     print(matter::as_facets(a))
  #     print(plot(data$mp_data, col = scales::alpha("blue", input$alpha), layout=F))
  #     
  # 
  #     
  #   
  #   
  #   # Ensure the device is closed after plotting
  #     dev.off()
  #     
  #     
  #     # Return a list containing the filename
  #     list(src = temp_file,
  #          contentType = 'image/png',
  #          width = 800,
  #          height = 600,
  #          alt = "This is alternate text")
  # }, deleteFile = TRUE)
  #   
  
  
  
  observeEvent(input$store, {
    
    req(input$peakPickfile)
    req(multipoint_data_files()[[input$coordinates]])
    
    # Store the dataset with the current selection
    x0$data_set_storage[[input$coordinates]] <- multipoint_data_files()[[input$coordinates]]
    
    
    sf_use_s2(FALSE)  # Disable s2 geometry checks
    
    
    
    # Extract transformed coordinates
    transformed_sf <- transformed_data()$mp_data
    
    # Check if a geometry is empty
    is_empty <- sf::st_is_empty(transformed_sf)
    
    # Filter out empty geometries
    non_empty_geometries <- transformed_sf[!is_empty, ]
    
    # Now ensure that all remaining geometries are polygons or multipolygons
    is_polygon <- sf::st_is(non_empty_geometries, c("POLYGON", "MULTIPOLYGON"))
    
    # Filter to keep only polygons
    polygon_geometries <- non_empty_geometries[is_polygon, ]
    
    # Cast all geometries to MULTIPOLYGON
    polygon_geometries <- sf::st_cast(polygon_geometries, "MULTIPOLYGON")
    
    poly <- polygon_geometries
    mpp_data<-transformed_data()$mpp_data
    
    # # Remove existing CRS
    st_crs(poly) <- NA
    st_crs(mpp_data) <- NA
    # 
    # 
    poly <- st_make_valid(poly)
    mpp_data <- st_make_valid(mpp_data)
    # 
    # Ensure CRS is consistent
    st_crs(mpp_data) <- st_crs(poly)
    
    
    
    #debugging to make sure polygons correctly aligned with data
    #plot_overlay(poly, mpp_data)
    
    
    # Extract region names from the 'classification' field
    regions <- str_extract(poly$classification, ":\\ .+\\,") %>%
      str_replace_all(":|\\ |\\,|\"", "")
    
    if(sum(grep("\\[", regions))>0) {
      
      regions <- sapply(1:length(poly$classification), function(i) {
        jsonlite::fromJSON(poly$classification[i])$name
      })
    }
    
    
    if(length(regions)>0) {
      poly$poly_name<-regions
      n_cells<-NULL
    }
    
    
    
    #if classifications doesn't exist (originally from ABBA),  look for cells or else generic polygon assignments
    if(length(unique(regions))==0){
      message("No regions detected in the polygon file")
      message("will attempt to extract cells from the root polygon")
      
      #check for cell in objectType, and if so proceed extracting cell information
      if("cell" %in% poly$objectType){
        #extract measurements from the root polygon
        message("Cells detected in the polygon file")
        measurements<-poly$measurements
        
        #how many polygons?
        n_poly<-dim(poly)[1]
        n_annotations<-which(poly$objectType%in%"annotation")
        n_cells<-which(poly$objectType%in%"cell")
        
        #create regions file by pasting cell and n_cells
        regions<-paste0("cell_", (n_cells))
        
        poly$poly_name[n_cells]<-regions
        
      } else {
        message("No cells or regions detected in the polygon file")
        message("will maps regions to generic polygons")
        
        regions<-paste0("region_", 1:length(poly$objectType))
        poly$poly_name<-regions
        
        n_cells<-NULL
        
      }
    }
    
    message("Attempting to extract measurements from cells")
    
    #extract measurements from the root polygon
    measurements<-poly$measurements
    
    if(!is.null(n_cells)){
      message("cells detected, extracting only measurement from cell polygons")
      measurement_cells<-measurements[n_cells]
      
      # Convert each character string to a list and then bind them into a dataframe
      df <- measurement_cells %>%
        lapply(fromJSON) %>%  # Convert each JSON-like string to a list
        bind_rows() %>%          # Bind each list as a row in a dataframe
        as.data.frame()
      rownames(df)<-regions
      
      
    } else {
      message("no cells detected, no measurements will be extracted")
      # Convert each character string to a list and then bind them into a dataframe
      df <- NULL
    }
    
    
    #store table in reactive available for download
    x0$region_table<-df
    
    
    #look at the root polygon and overlay plot for testing
    #n_poly<-which(regions%in%"root")
    #plot_overlay(poly[n_poly,], mpp_data)
    
    
    
    # Get the list of intersections between points and polygons
    int_list <- st_intersects(mpp_data, poly)
    
    #browser()
    
    # Get overlapping areas between mpp_data and mp_data
    message("Calculating overlapping regions, can take a while for large datasets")
    test_overlap <- st_intersection(mpp_data, poly)
    
    #showNotification("FIX THIS!!!", type = "warning")
    #save.image(file = "test_overlap.RData") # Save the data for debugging
    #load("test_overlap.RData")
    
    #debugging
    #Extract the polygon representing the hippocampus
    # hippocampus <-poly[2,]
    # plot_overlay(hippocampus, mpp_data)
    # hipp_overlay <- test_overlap[grep("HIP",test_overlap$classification),]
    # plot_overlay(hipp_overlay, mpp_data)
    # 
    
    
    
    overlap_regions<-test_overlap$poly_name
    
    # Step 4: Add the region information from the polygon to the points
    # Here, we'll assume test_overlap already has the points data and region information
    # You can now map the regions onto the points
    points_with_regions <- cbind(st_coordinates(test_overlap), overlap_regions)
    
    #debugging
    #hippocampus <- test_overlap[grep("HIP",test_overlap$classification),]
    
    regions_detected <- unique(overlap_regions)
    # Calculate region sizes and filter small regions based on threshold
    region_sizes <- sapply(regions_detected, function(x) {
      sum(overlap_regions == x, na.rm=TRUE)
    })
    
    #browser()  # For debugging, remove if not needed
    #####need to work from here....
    points_with_regions<-as.data.frame(points_with_regions)
    points_with_regions <- points_with_regions[points_with_regions$overlap_regions %in% 
                                                 names(region_sizes)[region_sizes>input$size_thresh],]
    
    # Function to map region to pdata
    map_region <- function(pdat, coord_dat, draw = FALSE) {
      # Ensure columns are named consistently beforehand
      colnames(coord_dat)[1:2] <- c("x", "y")
      coord_dat <- coord_dat %>% dplyr::mutate(x = as.character(x), y = as.character(y))
      pdat <- pdat %>% as.data.frame() %>% dplyr::mutate(x = as.character(x), y = as.character(y))
      
      
      # Perform a semi-join to find matching coordinates in pdat
      select_vec <- pdat %>% 
        dplyr::select(x, y) %>%
        dplyr::semi_join(coord_dat, by = c("x", "y")) %>%
        dplyr::mutate(select = TRUE)
      
      # Initialize select vector with FALSE for unmatched rows
      select_vec_full <- rep(FALSE, nrow(pdat))
      
      # Mark matching rows as TRUE
      matching_rows <- match(paste(select_vec$x, select_vec$y), paste(pdat$x, pdat$y))
      select_vec_full[matching_rows] <- TRUE
      
      # Optional plot for debugging
      if (draw) {
        print("Not yet...")
      }
      
      return(select_vec_full)
    }
    
    
    # Create pdata with new pixel assignments
    pdat <- pData(multipoint_data_files()[[input$coordinates]])
    
    #test for duplicate x,y coordinates in points_with_regions
    df<-points_with_regions
    duplicates <- df[duplicated(df[, c("X", "Y")]) | duplicated(df[, c("X", "Y")], fromLast = TRUE), ]
    
    if(nrow(duplicates)==0){
      message("No duplicate x,y coordinates detected")
      
      # Ensure the columns in `df` are numeric for proper matching
      df <- df %>%
        mutate(X = as.numeric(X), Y = as.numeric(Y))
      
      
      # Perform the join to add overlap_regions to pdat based on (x, y) == (X, Y)
      pdat2 <- pdat %>% as.data.frame() %>%
        left_join(df, by = c("x" = "X", "y" = "Y"))
      
      pdat$poly_name <- pdat2$overlap_regions #mased regions
      
      pdat$poly_unmasked <- is.na(pdat2$overlap_regions) #unmasked regions
      
      
      
    } else {
      message("Duplicate x,y coordinates detected")
      
      
      #browser()
      # Get the list of pixels for each region
      pixel_list <- lapply(unique(points_with_regions$overlap_regions), function(region) {
        select_vec <- points_with_regions$overlap_regions == region
        coord_dat <- points_with_regions[select_vec, 1:2]
        map_region(pdat, coord_dat)
      } )
      
      names(pixel_list) <- unique(points_with_regions$overlap_regions)
      
      pixel_assignment <- as.data.frame(do.call(cbind, pixel_list))
      
      # Assign new pixel info to pdata
      pdat[colnames(pixel_assignment)] <- pixel_assignment
      # Fill missing region columns with FALSE
      pdat[regions[!regions %in% colnames(pdat)]] <- FALSE
      
    }
    
    dat <- multipoint_data_files()[[input$coordinates]]
    
    #browser()
    # Update pdata in the dataset
    Cardinal::pData(dat) <- pdat
    x0$data_set_storage[[input$coordinates]] <- dat
    
    #save txt file with affine parameters used for the transformation
    # with the filename coming from the geojson input filename
    #get transform values
    transform_values <- list(
      scale_x = input$x_scaling,
      scale_y = input$y_scaling,
      translation_x = input$x_translation,
      translation_y = input$y_translation,
      rotation = input$rotation,
      alpha = input$alpha
    )
    x0$transform_values <- transform_values
    
    message("Done storing data")
  } 
  )
  
  
  output$save <- downloadHandler(
    filename = function() {
      paste0("newlist", Sys.Date(), ".rds")
    },
    content = function(file) {
      #pk_img <- x0$overview_peaks
      
      list_dat <- x0$data_set_storage
      list_names<-names(list_dat)
      
      
      
      #create list of unique variables across all datasets in the list
      reg_list<-unique(unlist(lapply(list_dat, function(x) colnames(pData(x)))))
      
      #create new list with complete variables in pdat for consistency across datasets
      new_list_dat<-list(NULL)
      fill_pdat<-function(x) {
        dat<-list_dat[[x]]
        pdat<-pData(dat)
        pdat[reg_list[!reg_list %in% colnames(pdat)]]<-FALSE #fill in missing columns with FALSE
        pData(dat)<-pdat
        new_list_dat[[x]]<-dat
      }
      
      #apply to all datasets in list
      pk_img<-lapply(1:length(list_dat), function(x) fill_pdat(x))
      #this is to combine and create single rds file
      #dat_comb<-combine(lapply(1:length(list_dat), function(x) fill_pdat(x)))
      
      names(pk_img)<-list_names
      
      
      #save MSI.EAGLE appropriate file
      #remove those brains where input$region is all null
      message("saving RDS file for MSI.EAGLE for runs where selected region to view is present")
      message("Will overwrite any file named 'MSI.EAGLE_combined_file.rds'")
      message("selecting data which exists in currently selected region")
      
      #WARNING may need this for overlapping region, but fails with others
      
      
      #which list elements have the selected region
      dat_select <- unlist(lapply(pk_img, function(x) {
        selected_data<-as.data.frame(pData(x))[input$region_select]
        if(isTRUEorFALSE(selected_data)){
          return(sum(selected_data))
        } else {
          message("selected region is not boolean, assuming it is present in the data")
          return(1)
        }
        
      }))>0
      eagle_tmp<-Cardinal::combine(pk_img[dat_select])
      
      #eagle_tmp<-combine_card(pk_img)
      #eagle_tmp<-Cardinal::combine(pk_img)
      
      #fData(eagle_tmp)<-fData(eagle_tmp)["ID"]
      
      #remove pdata columns that are all False
      
      pdat_select<-sapply(pData(eagle_tmp), function(x) all(sapply(x, isFALSE)))
      
      #remove all false columns
      pdat<-pData(eagle_tmp)
      pdat_data<-pdat[!pdat_select]
      pdat_data<-pdat_data[,!colnames(pdat_data) %in% c("x", "y", "run")]
      
      
      pData(eagle_tmp)<-PositionDataFrame(coord(pdat), run(pdat), pdat_data)
      
      #browser()
      write.csv(x0$transform_values, file=paste0(gsub(".geojson", "", input$multipolygon_file$name), "_transform_values.csv"))
      
      saveRDS(eagle_tmp, file="MSI.EAGLE_combined_file.rds") # save combined file
      writeImzML(eagle_tmp, file="MSI.EAGLE_combined_file")
      #pData(pk_img)<-x3$pdata
      saveRDS(pk_img, file)
      
      write.csv(x0$region_table, file=paste0(gsub(".rds", "", input$multipolygon_file$name), "_region_table.csv"))
      message("Done saving data")
    }
  )
  
  output$region_select<-renderUI({
    req(x0$data_set_storage)
    req(input$coordinates)
    #browser()
    col_choices<- colnames(pData(x0$data_set_storage[[input$coordinates]]))
    col_choices<-col_choices[!col_choices %in% c("x", "y")]
    selectInput("region_select", "region to plot", col_choices, selected = "HY")
    
    
  })
  
  output$plot2 <- renderImage({
    #browser()
    # Ensure required inputs are available
    req(input$region_select)
    req(input$coordinates)
    req(input$multipolygon_file)
    
    # Generate a temporary file path
    outfile <- tempfile(fileext = '.png')
    
    # Create the plot and save it to the temporary file
    png(outfile, width = 800, height = 600)
    
    if(input$plot_overlay){
      
      # Access the data and handle errors
      temp_var <- try(as.data.frame(pData(x0$data_set_storage[[input$coordinates]])[input$region_select]))
      if (inherits(temp_var, "try-error")) {
        return(NULL)
      }
      
      
      
      
      
      if(colnames(temp_var) %in% c("x", "y")){
        message("x and y not valid")
        return(NULL)
      }
      # Generate the plot using Cardinal's image function
      print(Cardinal::image(
        x0$data_set_storage[[input$coordinates]],
        colnames(temp_var)
      ))
      
      # If you need to overlay additional data, include it here
      # For example:
      # plot(data$mp_data, col = scales::alpha("blue", 0.05), add = TRUE)
      # plot(data$mpp_data, col = scales::alpha("red", 0.4), pch = 20, add = TRUE)
    }
    # Finish plotting
    dev.off()
    
    # Return a list containing the filename and other parameters
    list(
      src = outfile,
      contentType = 'image/png',
      width = 800,
      height = 600,
      alt = "Plot of data"
    )
    
  }, deleteFile = TRUE)  # deleteFile ensures the temporary file is removed after it's sent
  
  
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Input fields for selecting the input files
      fileInput("multipolygon_file", "Choose a multipolygon file", accept = ".geojson"),
      #shinyFiles::shinyFilesButton("rds_file", "File select", "Please select a processed rds file", multiple = FALSE, viewtype = "detail"),
      #fileInput("rds_file", "Choose a processed rds file"),
      uiOutput("pk_file"),
      # Add a "Load Data" button to the UI
      actionButton("load_data", "Load Selected Dataset"),
      #numericInput("scale_factor", "Scale factor for image", value = 1, min = 0.1, max = 10, step = 0.1),
      selectInput("msi_color", "Choose MSI color palette", c("magma", "inferno","plasma", "viridis", "cividis", "rocket", "mako", "turbo"), selected = "rocket"),
      uiOutput("coordinates", label="Choose a specific tissue matching the polygon file"),
      checkboxInput("scale_est", "Estimate scaling?", value = FALSE),
      checkboxInput("ignore_orig_coords", "Ignore original coordinates?", value = FALSE),
      actionButton("apply_estimate", "Apply Estimated Transform"),
      # Input fields for translation, scaling, and rotation
      
      # #For 5um Tristan N1. Scaling ~ 3, ie exported image res/MSI res - using original coord space
      # numericInput("rotation", "Rotation (degrees)", -90),
      # numericInput("alpha", "Alpha value for region overlay", 0.3, min=0, max=1, step=0.005),
      # numericInput("x_translation", "X Translation", -260),
      # numericInput("y_translation", "Y Translation", -1200),
      # numericInput("x_scaling", "X Scaling Factor", 0.28, step = 0.1),
      # numericInput("y_scaling", "Y Scaling Factor", 0.28, step = 0.1),
      
      #For 2um kidney N1. Scaling = 0.1138/2, ie exported image res/MSI res - ignore original coords
      numericInput("rotation", "Rotation (degrees)", 0),
      numericInput("alpha", "Alpha value for region overlay", 0.3, min=0, max=1, step=0.005),
      numericInput("x_translation", "X Translation", 240),
      numericInput("y_translation", "Y Translation", 510),
      numericInput("x_scaling", "X Scaling Factor", 0.0569, step = 0.1),
      numericInput("y_scaling", "Y Scaling Factor", 0.0569, step = 0.1),
      
      colourInput("overlay_color", "Choose Overlay Color", value = "#73FFFF"),
      numericInput("overlay_size", "Overlay Point Size", value = 0.1, min = 0.01, max = 5, step = 0.05),
      checkboxInput("median_filter", "Apply median filter to image?", value = TRUE),
      numericInput("filter_size", "Filter size for median filter", value = 1, min = 0, max = 10, step = 1),
      
      numericInput("size_thresh", "Exclude areas with Area below this threshold", 1),
      uiOutput("mz"),
      uiOutput("region_select"),
      selectInput("plot_type", "Choose plot type for region overlay", c("Ion overlay"="ion", "Phenotype"="area"), multiple=FALSE),
      
      # "Run" button to perform the specified operations
      actionButton("store", "Store data"),
      downloadButton("save", label="Save registered data"),
      bookmarkButton(),
      checkboxInput("plot_overlay", "Show secondary spatial image?", value = FALSE)
    ),
    
    mainPanel(
      
      # Main plot area for displaying the results
      #imageOutput("plot", height = "1200px", width = "800px"),
      imageOutput("plot"),
      imageOutput("plot2")
    )
  )
)

shinyApp(ui = ui, server = server, enableBookmarking = "url")


