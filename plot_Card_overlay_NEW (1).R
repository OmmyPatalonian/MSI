
  plot_card_UI<-function(id) {
    
    ns <- NS(id)
    
    col_choices<-c(hcl.pals())
    initial_cols<-c("Spectral", "Cividis", "Viridis", "Inferno", "Plasma", 
                    "Zissou 1", "Purple-Green", "Berlin", "PiYG", "Grays", 
                    "Batlow", "turku", "YlOrRd", "Terrain", 
                    "PrGn", "Green-Brown", "Hawaii", "Cork", "Rocket", "RdYlBu")
    
    col_choices<-c(initial_cols, setdiff(col_choices, initial_cols))
  
    tagList(  
      
        fluidRow(
          tags$head(
            #tags$style("label{font-family: BentonSans Book;}")
            #tags$style("label{font-size: 11px;} ")
          ),
          
          column(3, offset = 0,
                 radioButtons(ns("ion_viz3"), "Image visualization ions",
                                 c("First ion" = "viz_first", 
                                   "All ions (TIC)"="viz_all", 
                                    "Custom single / multiple"="custom")),
                 #numericInput(ns("mz_viz3"), "mz value for visualization",255.2),
                 uiOutput(ns("mz_viz3a")),
                 
                 fluidRow(
                   column(6, selectInput(ns("contrast3"), "Contrast enhancement", c( "none", "histogram",  "adaptive"))),
                   column(6, selectInput(ns("color3"), "Colorscale", col_choices, selected="Spectral"))
                 ),
                 fluidRow(
                   column(6, selectInput(ns("smooth3"), "Smoothing options", c("none", "mean", "gaussian", "bilateral", "adaptive", "diffusion", "guided"), selected = "gaussian")),
                   column(6, checkboxInput(ns("normalize3"), "Scale multiple images?", value = TRUE))
                   ),
                 
                 fluidRow(
                   column(6, checkboxInput(ns("colorkey3"), "Draw color key?", value = TRUE)),
                   column(6, checkboxInput(ns("dark_bg"), "Dark background?", value = FALSE))
                   ),
                   
                 fluidRow(
                   column(6, checkboxInput(ns("high_quality"), "High quality rendering?", value = TRUE)),
                   column(6, selectInput(ns("interpolation"), "Image interpolation", c("bilinear", "bicubic", "none"), selected = "bicubic"))
                   ),
                   
                 
                 
                 fluidRow(
                   column(6, numericInput(ns("width_im"), "MSI plot width (px)", value = 1200, step = 50)),
                   column(6, numericInput(ns("height_im"), "MSI plot height (px)", value=900, step = 50))
                 ),
                 fluidRow(
                   column(3, actionButton(ns("size_small"), "Small (800x600)", class = "btn btn-sm")),
                   column(3, actionButton(ns("size_medium"), "Medium (1200x900)", class = "btn btn-sm")),
                   column(3, actionButton(ns("size_large"), "Large (1600x1200)", class = "btn btn-sm")),
                   column(3, actionButton(ns("size_xlarge"), "X-Large (2000x1500)", class = "btn btn-sm"))
                 ),
                 p("Note: Larger sizes improve overlay alignment but may slow rendering"),
                 p("Quality Settings: 'High quality rendering' uses optimized DPI (200) for sharp MSI images. 'Bicubic interpolation' smooths pixelated appearance. Histology overlay shows 85% of image for full visibility.", 
                   style = "font-size: 12px; color: #666;"),
                 checkboxInput(ns("plot_pdata"), "Plot Phenotype data?", value=FALSE),
                 uiOutput(ns("plotpdata")),
                 checkboxInput(ns("expand_fonts"), "Extended font options?", value=FALSE),
                 uiOutput(ns("fonts")),                 
                 checkboxInput(ns("expand_runs"), "Select individual runs for plotting only?", value=FALSE),
                 uiOutput(ns("select_runs")),
                 p("___________________________________________________________________")
                 

                 
          ),
          column(9, uiOutput(ns("plot.window"))
          )  
        ),
        
        uiOutput(ns("spectrum"))
    )
          
  }
  

  plot_card_server <- function(id, overview_peaks_sel, spatialOnly=FALSE, allInputs=NULL) {

    moduleServer(id, function(input, output, session){
      
      #for dynamic UI
      ns = session$ns
      
      graphics.off()

      #create new overview_peaks_sel object with mean values
      if(is.null(fData(overview_peaks_sel)$mean)) {
        overview_peaks_sel<-Cardinal::summarizeFeatures(overview_peaks_sel, verbose=F)
        if(class(overview_peaks_sel)=="try-error") {
          showNotification("No data available, please check your parameters or dataset", type="error")
          return()
        }
      }
      # browser()
      # #create new overview_peaks_sel object with media values
      # if(is.null(fData(overview_peaks_sel)$non_zero)) {
      #   overview_peaks_sel$non_zero<-Cardinal::summarizeFeatures(overview_peaks_sel, "nnzero")
      #   if(class(overview_peaks_sel)=="try-error") {
      #     showNotification("No data available, please check your parameters or dataset", type="error")
      #     return()
      #   }
      # }
      # 
 
      observe({
        
        output$plot.window <- renderUI({
          
          if(input$ion_viz3=="custom") {
            fluidRow(
              column(12, imageOutput(ns("plot3_pk"))),
              fluidRow(
                column(12, br()),  # Adding space between the image and the table
                column(12, DT::dataTableOutput(ns('tbl')))
              )
            )
          } else {
            fluidRow(
              column(12, imageOutput(ns("plot3_pk")))
            )
          }
          
        })
        
      })
      
      

      
         output$mz_viz3a <- renderUI ({
           req(overview_peaks_sel)
           
           if(input$ion_viz3=="custom") {
             
            
             mz_list<- mz(overview_peaks_sel)
             tbl<-as.data.frame(fData(overview_peaks_sel))
             if(!is.null(tbl$freq)) {
               tbl$freq<-round(tbl$freq, 2)
             }
             if(!is.null(tbl$mean)) {
               tbl$mean<-round(tbl$mean, 1)
             }
             tbl$mz<-round(tbl$mz, 4)
             
             
             updateNumericInput(session, ("width_im"), value=1200, step=50)
             updateNumericInput(session, ("height_im"), value=900, step=50)
             
        
             
             #add something here at some point to only select mz, freq, count, and mean columns
             
             
             output$tbl <-DT::renderDataTable({
               tbl
               
             }, selection = "multiple")
  
             list(
 
               checkboxInput(ns("superpose"), "Superpose images?", value=FALSE),
               selectInput(ns("display_mode"), "Ion math?", c("none", "sum", "ratio", "subtract", "min", "max", "mean", "sd", "var",  "multiply")),
               numericInput(ns("plusminus_viz3"), "+/- m/z for visualization",0.05)
               
             )
           }
         
          })
         
        
    
      
      mz_viz3 <- reactive({
        req(overview_peaks_sel)
        req(input$tbl_rows_selected)
        tbl<-as.data.frame(fData(overview_peaks_sel))
        return(tbl[input$tbl_rows_selected, "mz"])
        
      })

      

      observe({
        
        output$select_runs <- renderUI ({
          req(overview_peaks_sel)
          
          if(input$expand_runs) {
            
            run_list<- unique(run((overview_peaks_sel)))
            
            list(
              selectInput(ns("select_runs"),
                          label= "run selection (plot only)",
                          multiple=TRUE,
                          choices = run_list,
                          selected = run_list)
            )
          }
        })
        
        
      })
      
      observe({
        output$fonts <- renderUI ({
          
          if(input$expand_fonts){
            list(
              numericInput(ns("axis_size"), label = "Axis font scaling (%)", min = 0, value=100),
              numericInput(ns("title_size"), label = "Title font scaling (%)", min = 0, value=100),
              numericInput(ns("label_size"), label = "Label font scaling (%)", min = 0, value=100),
              numericInput(ns("subtitle_size"), label = "Subtitle font scaling (%)", min = 0, value=100)
            )
          }
          
          
        })
      })
      
      observe({
        output$plotpdata <- renderUI ({
          
          if(input$plot_pdata){
            list(
              selectInput(ns("pdata_var_plot"), label="pData variable to plot", 
                          choices = colnames(as.data.frame(pData(overview_peaks_sel)))[-c(1:2)]
                          )
            )
          }
          
          
        })
      })

       #add spectrum or not
      output$spectrum<-renderUI({
        
        if(input$ion_viz3=="custom") {
          DT::dataTableOutput(ns('tbl'))
        }

        
        if(spatialOnly==FALSE){
          tagList(
            fluidRow(
              column(6,uiOutput(ns("plot_ranges"))),
              column(6,uiOutput(ns("plot_ranges2"))),
            ),
           
            fluidRow(
              
              column(2,  uiOutput(ns("x_target"))),
              column(2,  numericInput(ns("x_tol"), "+/- m/z window", value=5)),
              column(2,  uiOutput(ns("slider_y_max"))),
              column(3,numericInput(ns("width_ms"), "Plot width (px)", value = 800)),
              column(3,numericInput(ns("height_ms"), "Plot height (px)", value=300))
            ),
            fluidRow(
              column(10, imageOutput(ns("plot4")), style = "margin-bottom: 0px; padding-bottom: 0px;")
              ),
            fluidRow(
              column(3, checkboxInput(ns("calc_ppm"), label = "Show ppm error?", width = '100%')),
              column(3, checkboxInput(ns("show_int"), label = "Show intensity?", width = '100%')),
              column(3, numericInput(ns("show_mz"), label = "# mz values to show?", width = '100%', value=0))
            ),
            checkboxInput(ns("spectrum_expand_fonts"), "Extended font options?", value=FALSE),
            uiOutput(ns("spectrum_fonts")),
          )
        } 
      })
      
      observe({
        output$spectrum_fonts <- renderUI ({
          
          if(input$spectrum_expand_fonts) {
            fluidRow(
              column(
                3,
                numericInput(
                  ns("spectrum_axis_size"),
                  label = "Axis font scaling (%)",
                  min = 0,
                  value = 100
                )
              ),
              # column(
              #   2,
              #   numericInput(
              #     ns("spectrum_title_size"),
              #     label = "Title font scaling (%)",
              #     min = 0,
              #     value = 100
              #   )
              # ),
              column(
                3,
                numericInput(
                  ns("spectrum_label_size"),
                  label = "Label font scaling (%)",
                  min = 0,
                  value = 100
                )
              ),
              # column(
              #   2,
              #   numericInput(
              #     ns("spectrum_subtitle_size"),
              #     label = "Subtitle font scaling (%)",
              #     min = 0,
              #     value = 100
              #   )
              # ),
              column(
                3,
                numericInput(
                  ns("linewidth"),
                  label="Linewidth",
                  min=0,
                  value=100
                )
              )
            )
            
          }
          
          
        })
      })
      
      # Size preset button handlers
      observeEvent(input$size_small, {
        updateNumericInput(session, "width_im", value = 800)
        updateNumericInput(session, "height_im", value = 600)
      })
      
      observeEvent(input$size_medium, {
        updateNumericInput(session, "width_im", value = 1200)
        updateNumericInput(session, "height_im", value = 900)
      })
      
      observeEvent(input$size_large, {
        updateNumericInput(session, "width_im", value = 1600)
        updateNumericInput(session, "height_im", value = 1200)
      })
      
      observeEvent(input$size_xlarge, {
        updateNumericInput(session, "width_im", value = 2000)
        updateNumericInput(session, "height_im", value = 1500)
      })
          
       observe({
        output$plot3_pk <- renderImage( {  #plot image in overview after peakpicking / reading file
          
          #req(overview_peaks_sel)
          
          # A temp file to save the output.
          # This file will be removed later by renderImage
          outfile <- tempfile(fileext = '.png')
          
          # Balanced PNG rendering for good quality without excessive size
          dpi_setting <- if(!is.null(input$high_quality) && input$high_quality) 200 else 150
          
          # Use balanced resolution and DPI for optimal viewing
          png(outfile, 
              width = input$width_im, 
              height = input$height_im, 
              res = dpi_setting,  # Balanced DPI for good quality and reasonable file size
              units = "px",
              bg = "white",
              type = "cairo-png",  # Better rendering engine
              antialias = "default")  # Anti-aliasing for smoother edges
          
          #ion <- switch(input$mode,
          #              "p"=786,
          #             "n"=255.2)
          
          
          if(!is.null(input$select_runs)) {
            overview_peaks_sel <- subsetPixels(overview_peaks_sel, run %in% input$select_runs)
          }
          
          vp_orig<-vizi_par()
          
          #set sizes
          if(input$expand_fonts) {
            req(input$axis_size)
            
            vizi_par(
              cex.axis=input$axis_size/100,
              cex.lab=input$label_size/100,
              cex.main=input$title_size/100,
              cex.sub=input$subtitle_size/100
            )
            #get margins
            # cur_mar<-par()$mar
            # 
            # new_mar<-c(cur_mar[1]+cex.labp/8, cur_mar[2]+cex.labp/2, cur_mar[3]+cex.mainp/2, cur_mar[4])
            # 
            # #if drawing colorkey, add a little on the left
            # if(input$colorkey3) {
            #   new_mar[4]=new_mar[4]+cex.axisp
            # }
            # 
            # cur_mgp<-par()$mgp
            # 
            # #new_mgp<-c(cur_mgp[1]+cex.labp/8, cur_mgp[2], 0)
            # new_mgp<-c(3+max(new_mar[1:2])/20, 1,0)
            # 
            
            
            
            
           } else {
              vizi_par(
                cex.axis=1,
                cex.lab=1,
                cex.main=1,
                cex.sub=1
              )

           }
           
          if(input$plot_pdata){
            req(input$pdata_var_plot)
            
            #create list of arguments for image
            arg_list<-list(overview_peaks_sel, 
                       input$pdata_var_plot,
                        key=(input$colorkey3),
                        col=pals::alphabet())
                        
            if(input$dark_bg) {
              arg_list$style <- "dark"
            }
            
            plt_tmp<-do.call(Cardinal::image, arg_list)
            
            
            # Cardinal::image(overview_peaks_sel,
            #                         input$pdata_var_plot,
            #                         key=(input$colorkey3),
            #                         #superpose=input$superpose,
            #                         col=pals::alphabet())
            print(plt_tmp,
                                  #cex.axis=req(cex.axisp),
                                  #cex.lab=cex.labp,
                                  #cex.main=cex.mainp,
                                  #cex.sub=cex.subp,
                                  #mar=new_mar,
                                  #mgp=new_mgp
                  )
            
            vizi_par(vp_orig)
          } else if (input$ion_viz3=="viz_all") {
            
            
            mz_range=range(mz(overview_peaks_sel))
            
            #find closest mz value to middle of range
            test_value <- mean(mz_range)
            
            
            # Calculate the absolute differences
            differences <- abs(mz(overview_peaks_sel) - test_value)
            
            # Find the index of the minimum difference
            closest_index <- which.min(differences)
            
            mz_set=mz(overview_peaks_sel)[closest_index]
            tol=max(differences) + differences[closest_index]+1
            
            plusminus=tol
            
            #old way-- may be more memory efficient?
            # smoothing_option <- if (input$smooth3 != "none") paste0(", smooth ='", input$smooth3,"'") else ""
            # enhance_option <- if (input$contrast3 != "none") paste0(", enhance ='", input$contrast3,"'") else ""
            # 
            # image_command <- paste("Cardinal::image(overview_peaks_sel, 
            #                       mz=mz_set,
            #                       tolerance=round(plusminus,3), 
            #                       units='mz',
            #                       col=cpal(input$color3)",
            #                       enhance_option,
            #                       smoothing_option,",
            #                       scale=input$normalize3,
            #                       #superpose=input$superpose,
            #                       key=(input$colorkey3),
            #                       #cex.axis=req(cex.axisp),
            #                       #cex.lab=cex.labp,
            #                       #cex.main=cex.mainp,
            #                       #cex.sub=cex.subp,
            #                       #mar=new_mar,
            #                       #mgp=new_mgp
            # )")
            # 
            
            #print(eval(parse(text = image_command)))
            
            
            
            smoothing_option <- if (input$smooth3 != "none")  input$smooth3 else NULL
            enhance_option <- if (input$contrast3 != "none")  input$contrast3 else NULL
            
            # Enhanced Cardinal plotting parameters for better quality
            arg_list<-list(overview_peaks_sel, 
                           mz=mz_set,
                           tolerance=round(plusminus,3), 
                           units='mz',
                           col=cpal(input$color3),
                            enhance=enhance_option,
                           smooth=smoothing_option,
                           scale=input$normalize3,
                           #superpose=input$superpose,
                           key=(input$colorkey3))
            
            # Add interpolation for smoother rendering
            if(!is.null(input$interpolation) && input$interpolation != "none") {
              if(input$interpolation == "bicubic") {
                arg_list$interpolate <- "cubic"
              } else if(input$interpolation == "bilinear") {
                arg_list$interpolate <- "linear"
              }
            }
            
            if(input$dark_bg) {
              arg_list$style <- "dark"
            }
            
            print(do.call(Cardinal::image, arg_list))
            
            
            
            

            vizi_par(vp_orig)
            
            
          } else if (input$ion_viz3=="custom"){
            
            if(is.null(mz_viz3())){
              ion=mz(overview_peaks_sel[1,])
            } else {
              
              # observeEvent(input$mz_viz3,{
                ion=as.numeric(mz_viz3())
              # })
            }
            
            
            if(!is.null(input$display_mode) && input$display_mode!="none"){
              if(input$display_mode%in%c("min", "max", "min", "mean", "sum", "sd", "var")) { 
          
                
                
                select_vec<-as.character(mz(overview_peaks_sel)) %in% as.character(ion)
                #test to make sure there are 2 or more elements
                if(sum(select_vec)<2){
                  showNotification("At least two ions required for this calculation", type="error")
                  message("At least two ions required for this calculation")
                  return()
                }
                
                sm<-summarizePixels(overview_peaks_sel[select_vec,], stat=c(xic=input$display_mode), as="DataFrame")
                pData(overview_peaks_sel)$xic<-sm$xic
                
                label_txt=paste(input$display_mode, "mz(s)=", paste(ion, collapse=", "))
            
              }else if(input$display_mode=="ratio"){
                if(length(ion)!=2){
                  showNotification("Exactly two ions required for ratio (mz1/mz2)", type="error")
                  message("Exactly two ions required for ratio (mz1/mz2)")
                  return()
                } else {
                  mz1 <- spectra(subsetFeatures(overview_peaks_sel, mz=ion[1]))[1,]
                  mz2 <- spectra(subsetFeatures(overview_peaks_sel, mz=ion[2]))[1,]
                  overview_peaks_sel$xic <- (1 + mz1) / (1 + mz2)
                  
                  ion=round(ion, 4)
                  label_txt=paste("ratio of",ion[1],"/",ion[2])
                }
                
              } else if(input$display_mode=="subtract") {
                if(length(ion)!=2){
                  showNotification("Exactly two ions required for subtraction (mz1-mz2)", type="error")
                  message("Exactly two ions required for subtraction (mz1-mz2)")
                  return()
                } else {
                  mz1 <- spectra(subsetFeatures(overview_peaks_sel, mz=ion[1]))[1,]
                  mz2 <- spectra(subsetFeatures(overview_peaks_sel, mz=ion[2]))[1,]
                  overview_peaks_sel$xic <- (mz1) - (mz2)
                  ion=round(ion, 4)
                  label_txt=paste("difference of",ion[1],"-",ion[2])
                  
                }
                
                
              }else if(input$display_mode=="multiply"){
                nelements=length(ion)
                xic <- 1+spectra(subsetFeatures(overview_peaks_sel, mz=ion[1]))[1,]
                
                for(i in 2:nelements){
                  mz2= 1+spectra(subsetFeatures(overview_peaks_sel, mz=ion[i]))[1,]
                  overview_peaks_sel$xic=(xic) * (mz2)
                  ion=round(ion, 4)
                  label_txt=paste(ion[1],"*",ion[2])
                  }
                }
            
  
              
              
              if(sum(is.na(pData(overview_peaks_sel)$xic))==length(overview_peaks_sel)) {
                showNotification("This calculation does not work!")
                return()
              }
              
              
              
              # smoothing_option <- if (input$smooth3 != "none") paste0(", smooth ='", input$smooth3,"'") else ""
              # enhance_option <- if (input$contrast3 != "none") paste0(", enhance ='", input$contrast3,"'") else ""
              # 
              plusminus=input$plusminus_viz3
              
            #   image_command <- paste("Cardinal::image(overview_peaks_sel, 'xic',
            #                       tolerance=round(plusminus,3), 
            #                       units='mz',
            #                       col=cpal(input$color3)",
            #                          enhance_option,
            #                          smoothing_option,",
            #                       scale=input$normalize3,
            #                       #superpose=input$superpose,
            #                       key=(input$colorkey3),
            #                       #cex.axis=req(cex.axisp),
            #                       #cex.lab=cex.labp,
            #                       #cex.main=cex.mainp,
            #                       #cex.sub=cex.subp,
            #                       #mar=new_mar,
            #                       #mgp=new_mgp
            # )")
            #   
              smoothing_option <- if (input$smooth3 != "none")  input$smooth3 else NULL
              enhance_option <- if (input$contrast3 != "none")  input$contrast3 else NULL
              
              
              arg_list<-list(overview_peaks_sel,
                             'xic',
                             tolerance=round(plusminus,3), 
                             units='mz',
                             col=cpal(input$color3),
                             enhance=enhance_option,
                             smooth=smoothing_option,
                             scale=input$normalize3,
                             #superpose=input$superpose,
                             key=(input$colorkey3))
              
              if(input$dark_bg) {
                arg_list$style <- "dark"
              }
              
              print(matter::as_facets(do.call(Cardinal::image, arg_list), labels=label_txt))
              
              vizi_par(vp_orig)

            } else {
              
              
              
              mz_set=ion
              
              mz_range=range(mz(overview_peaks_sel))
              
              
              # smoothing_option <- if (input$smooth3 != "none") paste0(", smoothing ='", input$smooth3,"'") else ""
              # enhance_option <- if (input$contrast3 != "none") paste0(", enhance ='", input$contrast3,"'") else ""
              # 
              plusminus=input$plusminus_viz3
              
            #   image_command <- paste("Cardinal::image(overview_peaks_sel, 
            #                       mz=mz_set,
            #                       tolerance=round(plusminus,3), 
            #                       units='mz',
            #                       col=cpal(input$color3)",
            #                          enhance_option,
            #                          smoothing_option,",
            #                       scale=input$normalize3,
            #                       superpose=input$superpose,
            #                       key=(input$colorkey3),
            #                       #cex.axis=req(cex.axisp),
            #                       #cex.lab=cex.labp,
            #                       #cex.main=cex.mainp,
            #                       #cex.sub=cex.subp,
            #                       #mar=new_mar,
            #                       #mgp=new_mgp
            # )")
            #   
              
              
              smoothing_option <- if (input$smooth3 != "none")  input$smooth3 else NULL
              enhance_option <- if (input$contrast3 != "none")  input$contrast3 else NULL
              
              # Enhanced Cardinal plotting parameters for better quality
              arg_list<-list(overview_peaks_sel,
                             #'xic',
                             mz=mz_set,
                             tolerance=round(plusminus,3), 
                             units='mz',
                             col=cpal(input$color3),
                             enhance=enhance_option,
                             smooth=smoothing_option,
                             scale=input$normalize3,
                             superpose=input$superpose,
                             key=(input$colorkey3))
              
              # Add interpolation for smoother rendering
              if(!is.null(input$interpolation) && input$interpolation != "none") {
                if(input$interpolation == "bicubic") {
                  arg_list$interpolate <- "cubic"
                } else if(input$interpolation == "bilinear") {
                  arg_list$interpolate <- "linear"
                }
              }
              
              if(input$dark_bg) {
                arg_list$style <- "dark"
              }
              
              
              
              
              print(do.call(Cardinal::image, arg_list))
              
              vizi_par(vp_orig)

              
            }
          } else if (input$ion_viz3=="viz_first") {
            
            
            tol=0.05
            #browser()
            
            # Enhanced Cardinal plotting parameters for better quality
            smoothing_option <- if (input$smooth3 != "none")  input$smooth3 else NULL
            enhance_option <- if (input$contrast3 != "none")  input$contrast3 else NULL
            
            arg_list <- list(overview_peaks_sel, 
                            col=cpal(input$color3),
                            enhance=enhance_option,
                            smooth=smoothing_option,
                            scale=input$normalize3,
                            key=(input$colorkey3))
            
            # Add interpolation for smoother rendering
            if(!is.null(input$interpolation) && input$interpolation != "none") {
              if(input$interpolation == "bicubic") {
                arg_list$interpolate <- "cubic"
              } else if(input$interpolation == "bilinear") {
                arg_list$interpolate <- "linear"
              }
            }
            
            if(input$dark_bg) {
              arg_list$style <- "dark"
            }
            
            image_command <- do.call(Cardinal::image, arg_list)
            
            print(image_command)
            
            vizi_par(vp_orig)
            
          }
          
          
          #browser()
          # MSI data rendering is now complete - add histology overlay ONLY if both uploads exist
          if(!is.null(allInputs$histology_upload) && !is.null(allInputs$msi_upload)) {
            
            # Additional validation to prevent premature execution
            if (is.null(overview_peaks_sel)) {
              # No overlay needed, MSI plot is complete
              dev.off()
              return(list(src = outfile,
                         contentType = 'image/png',
                         width = input$width_im,
                         height = input$height_im,
                         alt = "MSI plot without overlay"))
            }
            
            # Validate that uploads have the expected structure
            if (is.null(allInputs$histology_upload$datapath)) {
              stop("Invalid histology upload - missing datapath")
            }
            
            if (is.null(allInputs$msi_upload$datapath)) {
              stop("Invalid MSI upload - missing datapath")
            }
            
            # Debug information
            if(allInputs$debug==T){
              cat("Adding histology overlay to MSI data...\n")
              cat("Histology upload info:\n")
              print(allInputs$histology_upload)
              cat("MSI upload info:\n")
              print(allInputs$msi_upload)
            }
          
          
          ensure_3d <- function(image) {
            if(allInputs$debug==T) {
              cat("ensure_3d input dimensions:", dim(image), "\n")
              cat("ensure_3d input class:", class(image), "\n")
              cat("ensure_3d input memory:", object.size(image), "bytes\n")
            }
            
            # Handle different image structures with memory optimization
            if (is.null(dim(image))) {
              stop("Image has no dimensions - invalid image data")
            }
            
            # Check if image is grayscale (2 dimensions)
            if (length(dim(image)) == 2) {
              # Convert grayscale to RGB by replicating the matrix across 3 layers
              # More memory efficient: create once, replicate references
              gray_layer <- image
              image <- array(dim = c(dim(gray_layer)[1], dim(gray_layer)[2], 3))
              image[,,1] <- gray_layer
              image[,,2] <- gray_layer
              image[,,3] <- gray_layer
              rm(gray_layer)  # Free original
              gc(verbose = FALSE)
              if(allInputs$debug==T) cat("Converted 2D to 3D RGB (memory optimized)\n")
            }
            # Check if image has more than 3 dimensions and reduce if necessary
            else if (length(dim(image)) > 3) {
              # For 4D images (e.g., RGBA), take first 3 channels
              if (dim(image)[3] >= 3) {
                temp_image <- image[,,1:3,drop=FALSE]
                rm(image)  # Free original large image
                image <- temp_image
                rm(temp_image)
                gc(verbose = FALSE)
                if(allInputs$debug==T) cat("Reduced 4D+ to 3D by taking first 3 channels (memory optimized)\n")
              } else {
                temp_image <- image[,,,1]  # Take the first layer if multiple layers
                rm(image)
                image <- temp_image
                rm(temp_image)
                gc(verbose = FALSE)
                if(allInputs$debug==T) cat("Reduced 4D+ to 3D by taking first layer (memory optimized)\n")
              }
            }
            # Check if we have exactly 3 dimensions but wrong channel count
            else if (length(dim(image)) == 3) {
              if (dim(image)[3] == 1) {
                # Single channel image, replicate to RGB (memory efficient)
                single_channel <- image[,,1]
                image <- array(dim = c(dim(single_channel)[1], dim(single_channel)[2], 3))
                image[,,1] <- single_channel
                image[,,2] <- single_channel
                image[,,3] <- single_channel
                rm(single_channel)
                gc(verbose = FALSE)
                if(allInputs$debug==T) cat("Converted single channel to RGB (memory optimized)\n")
              } else if (dim(image)[3] == 2) {
                # Two channel image, add a third channel (reuse first)
                temp_image <- array(dim = c(dim(image)[1], dim(image)[2], 3))
                temp_image[,,1:2] <- image[,,1:2]
                temp_image[,,3] <- image[,,1]  # Reuse first channel
                rm(image)
                image <- temp_image
                rm(temp_image)
                gc(verbose = FALSE)
                if(allInputs$debug==T) cat("Extended 2-channel to 3-channel (memory optimized)\n")
              } else if (dim(image)[3] > 4) {
                # More than 4 channels, take first 3
                temp_image <- image[,,1:3,drop=FALSE]
                rm(image)
                image <- temp_image
                rm(temp_image)
                gc(verbose = FALSE)
                if(allInputs$debug==T) cat("Reduced multi-channel to RGB (memory optimized)\n")
              }
            }
            
            # Ensure values are in [0,1] range (in-place to save memory)
            max_val <- max(image, na.rm = TRUE)
            if (max_val > 1) {
              if (max_val <= 255) {
                image <- image / 255
                if(allInputs$debug==T) cat("Normalized from 0-255 to 0-1 range\n")
              } else {
                min_val <- min(image, na.rm = TRUE)
                image <- (image - min_val) / (max_val - min_val)
                if(allInputs$debug==T) cat("Normalized to 0-1 range\n")
              }
            }
            
            if(allInputs$debug==T) {
              cat("ensure_3d output dimensions:", dim(image), "\n")
              cat("ensure_3d output range:", range(image, na.rm = TRUE), "\n")
              cat("ensure_3d output memory:", object.size(image), "bytes\n")
            }
            
            return(image)
          }
          
          #browser()            # Load the uploaded histology image with TIFF-to-PNG conversion (MEMORY OPTIMIZED)
            histology_path <- allInputs$histology_upload$datapath
            
            # Memory check before processing
            if(allInputs$debug==T){
              cat("Memory status before image processing:\n")
              print(gc())
              cat("Original histology_path:\n")
              print(histology_path)
              cat("Length of histology_path:", length(histology_path), "\n")
            }
            
            # Ensure we have a single file path (not a vector)
            if (length(histology_path) > 1) {
              histology_path <- histology_path[1]
              if(allInputs$debug==T){
                cat("Using first histology_path:", histology_path, "\n")
              }
            }
            
            # Get file extension and convert to lowercase for consistency
            file_type <- tolower(tools::file_ext(histology_path))
            
            if(allInputs$debug==T){
              cat("Detected file_type:", file_type, "\n")
              cat("File exists:", file.exists(histology_path), "\n")
            }
            
            # Validate that the file exists and is a supported image type
            if (!file.exists(histology_path)) {
              stop("Histology image file not found: ", histology_path)
            }
            
            if (!file_type %in% c("png", "jpg", "jpeg", "tiff", "tif")) {
              stop("Unsupported histology image file type: ", file_type, ". Supported types: PNG, JPG, JPEG, TIFF, TIF")
            }
            
            # SOLUTION: Convert TIFF to PNG to avoid grey lines issue (MEMORY OPTIMIZED)
            if (file_type %in% c("tiff", "tif")) {
              if(allInputs$debug==T) {
                cat("TIFF detected - converting to PNG with memory optimization...\n")
              }
              
              # Memory-optimized TIFF reading and conversion
              tiff_image <- tryCatch({
                # Enhanced TIFF reading with memory management
                img <- NULL
                
                # Method 1: Try reading with all=TRUE but only first page to save memory
                tryCatch({
                  img_all <- tiff::readTIFF(histology_path, all = TRUE)
                  if(allInputs$debug==T) {
                    cat("TIFF pages found:", length(img_all), "\n")
                  }
                  
                  # Use only the first page and immediately free memory
                  if (length(img_all) > 0) {
                    img <- img_all[[1]]
                    # Free memory from unused pages
                    rm(img_all)
                    gc(verbose = FALSE)  # Force garbage collection
                  }
                }, error = function(e) {
                  if(allInputs$debug==T) cat("Method 1 failed:", e$message, "\n")
                })
                
                # Method 2: Try standard reading if Method 1 failed
                if (is.null(img)) {
                  tryCatch({
                    img <- tiff::readTIFF(histology_path, native = FALSE)
                    if(allInputs$debug==T) {
                      cat("Standard TIFF read successful. Dimensions:", dim(img), "\n")
                    }
                  }, error = function(e) {
                    if(allInputs$debug==T) cat("Method 2 failed:", e$message, "\n")
                  })
                }
                
                # Method 3: Try with native=TRUE if previous methods failed
                if (is.null(img)) {
                  tryCatch({
                    img <- tiff::readTIFF(histology_path, native = TRUE)
                    if(allInputs$debug==T) {
                      cat("Native TIFF read successful. Dimensions:", dim(img), "\n")
                    }
                    # Convert native format to standard if needed
                    if (is.raw(img) || !is.array(img)) {
                      img <- tiff::readTIFF(histology_path, native = FALSE)
                    }
                  }, error = function(e) {
                    if(allInputs$debug==T) cat("Method 3 failed:", e$message, "\n")
                  })
                }
                
                if (is.null(img)) {
                  stop("Failed to read TIFF file with all available methods")
                }
                
                img
              }, error = function(e) {
                if (grepl("vector memory exhausted|memory limit|cannot allocate", e$message, ignore.case = TRUE)) {
                  stop("Memory limit reached while reading TIFF file. This TIFF file is too large for available memory. Please:\n",
                       "1. Convert TIFF to PNG using external software (ImageJ, GIMP)\n",
                       "2. Reduce TIFF image resolution\n", 
                       "3. Restart R and try: memory.limit(size = 8192)\n",
                       "4. Close other applications to free memory\n",
                       "Original error: ", e$message)
                }
                stop("Failed to read TIFF image: ", e$message)
              })
              
              # Memory-efficient processing: ensure_3d and immediate conversion
              if(allInputs$debug==T) {
                cat("Processing TIFF with memory optimization...\n")
                cat("Original image size:", object.size(tiff_image), "bytes\n")
              }
              
              # Process image efficiently
              processed_tiff <- ensure_3d(tiff_image)
              
              # Free original image memory immediately
              rm(tiff_image)
              gc(verbose = FALSE)
              
              # Convert TIFF to PNG in a temporary file with compression
              png_temp_path <- tempfile(fileext = ".png")
              
              # Write PNG with compression to reduce file size
              png::writePNG(processed_tiff, png_temp_path)
              
              # Free processed image memory immediately
              rm(processed_tiff)
              gc(verbose = FALSE)
              
              if(allInputs$debug==T) {
                cat("TIFF converted to PNG at:", png_temp_path, "\n")
                cat("PNG file size:", file.size(png_temp_path), "bytes\n")
                cat("Memory freed after conversion\n")
              }
              
              # Update the path to use PNG version
              histology_path <- png_temp_path
              file_type <- "png"
              
            }
            
            if(allInputs$debug==T){
              cat("Final file type for processing:", file_type, "\n")
              cat("Final histology file path:", histology_path, "\n")
              cat("File size:", file.size(histology_path), "bytes\n")
            }            # Read the histology image (now PNG format after conversion if needed)
            histology_image <- tryCatch({
              # All TIFF files have been converted to PNG at this point
              switch(file_type,
                     "png" = png::readPNG(histology_path),
                     "jpg" = jpeg::readJPEG(histology_path),
                     "jpeg" = jpeg::readJPEG(histology_path),
                     stop("Unexpected file type after preprocessing: ", file_type))
            }, error = function(e) {
              stop("Failed to read histology image: ", e$message)
            })
            
            histology_image <- ensure_3d(histology_image)
            
            # Create overlay with transparency blending (memory optimized approach)
            if(allInputs$debug==T) {
              cat("Image dimensions after ensure_3d:", dim(histology_image), "\n")
              cat("Applying transparency and creating overlay (memory optimized)...\n")
              cat("Histology image memory:", object.size(histology_image), "bytes\n")
            }
            
            # Memory-efficient RGBA image creation for overlay
            final_image <- NULL
            
            if (length(dim(histology_image)) == 3) {
              if (dim(histology_image)[3] >= 3) {
                # Use RGB channels, add alpha (memory efficient)
                img_dims <- dim(histology_image)
                final_image <- array(dim = c(img_dims[1], img_dims[2], 4))
                
                # Copy RGB channels efficiently
                final_image[,,1:3] <- histology_image[,,1:3]
                
                # Add alpha channel
                final_image[,,4] <- allInputs$alpha
                
                # Free original image memory
                rm(histology_image)
                gc(verbose = FALSE)
                
                if(allInputs$debug==T) cat("Created RGBA from RGB image (memory optimized)\n")
                
              } else if (dim(histology_image)[3] == 1) {
                # Grayscale - replicate to RGB then add alpha (memory efficient)
                img_dims <- dim(histology_image)
                final_image <- array(dim = c(img_dims[1], img_dims[2], 4))
                
                # Replicate grayscale to RGB
                gray_channel <- histology_image[,,1]
                final_image[,,1] <- gray_channel
                final_image[,,2] <- gray_channel
                final_image[,,3] <- gray_channel
                final_image[,,4] <- allInputs$alpha
                
                # Free memory
                rm(histology_image, gray_channel)
                gc(verbose = FALSE)
                
                if(allInputs$debug==T) cat("Created RGBA from grayscale (memory optimized)\n")
              }
            }
            
            if(allInputs$debug==T) {
              cat("Final overlay image dimensions:", dim(final_image), "\n")
              cat("Final overlay image range:", range(final_image, na.rm = TRUE), "\n")
              cat("Final overlay memory:", object.size(final_image), "bytes\n")
            }
            
            # Create the histology overlay using standard rasterGrob
            # Since we converted TIFF to PNG, this should not have grey lines
            histology_grob <- rasterGrob(
              final_image, 
              interpolate = TRUE,
              x = unit(0.5, "npc"),
              y = unit(0.5, "npc"),
              width = unit(1, "npc"),
              height = unit(1, "npc"),
              just = c("center", "center")
            )
            
            # FIXED: Apply transformations with proper scaling for full image visibility
            # Problem: Previous code used MSI plot pixel dimensions (1200x900) in "points" units, 
            #          making histology overlay enormous and cut off
            # Solution: Use normalized parent coordinates (npc) for overlay sizing with a base scale factor
            #          that fits the histology image properly within the plot area
            base_scale <- 0.8  # Base scale to fit histology within plot area (80% of plot area)
            
            transformed_grob <- editGrob(
              histology_grob,
              vp = viewport(
                x = unit(0.5, "npc") + unit(allInputs$translate_x, "mm"),
                y = unit(0.5, "npc") + unit(allInputs$translate_y, "mm"),
                angle = allInputs$rotate,
                width = unit(base_scale * allInputs$scalex, "npc"),
                height = unit(base_scale * allInputs$scaley, "npc"),
                just = c("center", "center")
              )
            )
            
            # Add histology overlay to the existing MSI plot
            if(allInputs$debug==T) cat("Adding histology overlay to MSI plot\n")
            grid.draw(transformed_grob)
            
          } else {
            # No histology overlay - just MSI data
            if(allInputs$debug==T) cat("No histology overlay - displaying MSI data only\n")
          }
          
          # Close the graphics device
          dev.off()
          
          
          # Return a list containing the filename
          list(src = outfile,
               contentType = 'image/png',
               width = input$width_im,
               height = input$height_im,
               alt = "This is alternate text")
        }, deleteFile = TRUE)
       }) 
       
       
       if(spatialOnly==FALSE) {
         observe({  
          output$plot_ranges<- renderUI( {
            
            req(overview_peaks_sel)
            
            a<-Cardinal::plot(overview_peaks_sel)
            
  
                  sliderInput(ns("mass_range_plot"), 
                              label = p("m/z range for MS plot (X)"), 
                              min = round(a$channels$x$limits[1]),
                              max = round(a$channels$x$limits[2]), 
                              value = round(a$channels$x$limits), 
                              step = NULL,
                              round = TRUE)
  
            
          })
         })
         
         observe({
          output$plot_ranges2<- renderUI( {
            req(overview_peaks_sel)
            #browser()
            #overview_peaks_sel<-Cardinal::summarizeFeatures(overview_peaks_sel)
            a<-Cardinal::plot(overview_peaks_sel, "mean")
            
            
           
            
            sliderInput (ns("int_range_plot"), 
                        label = p("intensity range for MS plot (Y)"), 
                        min = round(a$channels$y$limits[1],0),
                        max = round(a$channels$y$limits[2],0)*1.05, 
                        value = req(input$param_numeric), #round(a$par$ylim,0), 
                        step = a$channels$y$limits[2]/20,
                        round = TRUE
            )
            
                        
                        
            
          })
         })
         
         
         #set center of observed spectrum if using custom ion visualization
         observe({
           output$x_target <- renderUI({
             
             if(input$ion_viz3!="custom") {
               numericInput(ns("x_target"), "Center m/z value", value = NULL)
             } else {
               numericInput(ns("x_target"), "Center m/z value", value = mz_viz3())
             }
             
           })
         })
         
        
         
         
         
         observe( {
           
           output$slider_y_max <- renderUI({
             
             req(overview_peaks_sel)
            
             
             a<-Cardinal::plot(overview_peaks_sel, "mean")
             
             
             numericInput(ns("param_numeric"),
                          "Manual y-axis intensity max value",
                          min = round(a$channels$y$limits[1],0),
                          max = round(a$channels$y$limits[2],0),
                          value = round(a$channels$y$limits[2],0)
             )
           })
         })
               
          
         if(!is.null(overview_peaks_sel)) {
           
           updateSliderInput(session, ns("int_range_plot"), value = c(round(Cardinal::plot(req(overview_peaks_sel))$channels$y$limits,0) 
                                                                      ))
           updateNumericInput(session, ns("param_numeric"), value = round(Cardinal::plot(req(overview_peaks_sel))$channels$y$limits[2],0))
          }
        
        
        
         
         observe({
           output$plot4 <- renderImage( {
             
             
             req(overview_peaks_sel)
             req(input$mass_range_plot)
             
             # A temp file to save the output.
             # This file will be removed later by renderImage
             outfile <- tempfile(fileext = '.png')
             
             png(outfile, width = input$width_ms, height = input$height_ms)
             
            
             
             if(length(input$int_range_plot)==1) {
               ylim=c(0, input$int_range_plot)
             } else {
               ylim = input$int_range_plot
             }
             
             #change xlimits based on custom ion or not
             if(is.null(input$x_target) || is.na(input$x_target)){
               xlim=input$mass_range_plot
               
               overview_peaks_sel_plot<-overview_peaks_sel
               
             } else {
               xlim=c(input$x_target-input$x_tol, input$x_target+input$x_tol)
               
               #subsetFeatures to only include mz values within range
               overview_peaks_sel_plot<-subsetFeatures(overview_peaks_sel, mz > xlim[1] & mz < xlim[2])
               
             }
             
             if(!is.finite(xlim[1])){
               xlim=input$mass_range_plot
             }
             
             vp_orig<-vizi_par()
             if(input$spectrum_expand_fonts) {
               req(input$spectrum_axis_size)
               browser()
               vizi_par(
                 cex.axis=req(input$spectrum_axis_size)/100,
                 cex.lab=input$spectrum_label_size/100,
                 cex.main=input$spectrum_label_size/100,
                 #cex.subp=input$spectrum_subtitle_size/100
                 lwd=input$linewidth/100,
                 mar = c(0, 0, 1, 1)
               )
               
               
               #get margins
               #cur_mar<-par()$mar
               
               #new_mar<-c(cur_mar[1]+cex.labp/8, cur_mar[2]+cex.labp/2, cur_mar[3], cur_mar[4])
               
               #cur_mgp<-par()$mgp
               
               #new_mgp<-c(cur_mgp[1]+cex.labp/8, cur_mgp[2], 0)
               #new_mgp<-c(3+max(new_mar[1:2])/20, 1,0)
               
               #lwd=lwdp
                          
               
               
               
               
             } else {
               vizi_par(
                 cex.axis=1,
                 cex.lab=1,
                 cex.main=1,
                 cex.sub=1
               )
             }
             
             #browser()
             #overview_peaks_sel<-Cardinal::summarizeFeatures(overview_peaks_sel)
             
             p1<-Cardinal::plot(overview_peaks_sel_plot,
                                xlim=xlim,
                                ylim =ylim,
                                #cex.axis=req(cex.axisp),
                                #cex.lab=cex.labp,
                                #cex.main=cex.mainp,
                                #cex.sub=cex.subp,
                                #lwd=lwdp,
                                # mar=new_mar, 
                                # mgp=new_mgp, 
                                "mean",
                                annPeaks=input$show_mz,
                                free="y")
             print(p1)
             vizi_par(vp_orig)
             
             #check for ppm calc
             if(input$calc_ppm) {
               
               dat=overview_peaks_sel_plot
               x=mz(dat)
               targ_mz<-req(input$x_target)
               x_sel<-subset(x, x>=xlim[1] & x<= xlim[2])
               
               ppm_error<- round(1e6*(x_sel-targ_mz)/targ_mz, 2)
               
               p1_coord<-p1[[1]][[1]]$marks$peaks$encoding
               
               y_labs<-p1_coord$y[p1_coord$x %in% x_sel]
               
               if(length(ppm_error)==0){
                 showNotification("No ppm error calculated, are there any peaks?", type="warning")
                 return()
               } else {
                print(text(x=x_sel, y=y_labs+ylim[2]*.25, req(ppm_error)))
               }
               
             }
             
             
             
             if(input$show_int) {
              
               
               ###
               p1_coord<-p1[[1]][[1]]$marks$peaks$encoding
               
               
               
               dat=overview_peaks_sel_plot
               x=mz(dat)
               
               x_sel<-subset(x, x>=xlim[1] & x<= xlim[2])
               
               
               
               y_labs<-p1_coord$y[p1_coord$x %in% x_sel]
               
               if(length(y_labs)==0){
                 showNotification("No intensities found, are there any peaks?", type="warning")
                 return()
               } else {
                 print(text(x=x_sel, y=y_labs+ylim[2]*.15, req(round(y_labs, 0))))
               }
               
             }
             
             

             
             dev.off()
             
             # Return a list containing the filename
             list(src = outfile,
                  contentType = 'image/png',
                  width = input$width,
                  height = input$height,
                  alt = "This is alternate text")
           }, deleteFile = TRUE)
           
         })
       }
  })
        
  
}

