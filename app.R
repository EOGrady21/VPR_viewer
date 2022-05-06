# Library -----
suppressWarnings(librarian::shelf(shiny, vprr, magick, ggplot2, metR, dplyr, DT, base64enc, shinyFiles, bslib, thematic,
                 shinyWidgets, shinyhelper, spelling, exiftoolr, oce, gridExtra, waiter, quiet = TRUE))

# ExifTool -----
# install exiftool to check image metadata
# required when run on new machine
is_exiftool_available <- function() {
  !is.null(tryCatch(exif_version(), error = function(e) NULL))
}
if(is_exiftool_available() == FALSE){install_exiftool()}

# Theme -----
thematic_shiny() #theme plots to match bs_theme() argument
light <- bs_theme(bootswatch = 'zephyr', version = '5')
dark <-  bs_theme(bootswatch = 'superhero', version = '5')

# Cache ----
# shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "vprv-cache")))
             
# UI -------------------------------------------------------------------------------------------
{ui <- fluidPage(
  waiter::use_waiter(),

# * Debugging ----
  # console command : $('#browser').show();
  actionButton("browser", "browser"),
  tags$script("$('#browser').hide();"),
 
# * Theme & Formatting ----
    theme = light,
    switchInput(inputId = "dark_mode", label = "Dark mode", value = FALSE, size = 'mini'),  
    
  # plot formatting (fix from https://stackoverflow.com/questions/45642283/how-to-save-png-image-of-shiny-plot-so-it-matches-the-dimensions-on-my-screen
    tags$script(
        "$(document).on('shiny:connected', function(event) {
  var myWidth = $(window).width();
  Shiny.onInputChange('shiny_width',myWidth)
  });"
    ),
    
    tags$script(
        "$(document).on('shiny:connected', function(event) {
  var myHeight = $(window).height();
  Shiny.onInputChange('shiny_height',myHeight)
  });"
    ), 

  titlePanel("VPR Viewer"),
# * Side bar layout ----
    sidebarLayout(
        sidebarPanel(
            helpText("Input Metadata."),
            fluidRow(
                column(3, offset = 5,
                       actionButton('update', label = 'Update', icon(name = 'refresh', lib = 'glyphicon')) %>%
                         helper(content = 'update')
                )
                ),
          
            fluidRow(column(9,
              fileInput('ctd_files', label = 'CTD Files', multiple = FALSE, accept = '.dat') %>%
              helper(content = 'ctd_files')
              ),
            column(3, 
                   dropdownButton(inputId = 'ctd_column_drop',
                           label = 'VPR CTD version',
                           circle = TRUE,
                           pickerInput(inputId = 'vpr_version', 
                                       choices = list('DAVPR 27 (BIO)', 'DAVPR 14 (GW)'),
                                       selected = 'DAVPR 27 (BIO)')) %>%
              helper(content = 'ctd_col_drop')
            )),
            textInput('cruise', 'Cruise ID', placeholder = 'eg. IML2018051', value = 'IML2018051') %>% 
              helper(content = 'cruise'),
            
            textInput('tow', label = 'VPR Tow ID', placeholder = 'eg. 1', value = '0') %>%
              helper(content = 'tow'),
            
            textInput('day', 'Day', placeholder = 'eg. 283', value = '286') %>%
              helper(content = 'day'),
            
            textInput('hour', 'Hour', placeholder = 'eg. 06', value = '22') %>%
              helper(content = 'hour'),
            
            # checkboxInput("multiple", "Multiple Hours"),
            # conditionalPanel(
            #   condition = "input.multiple == true",
            #   textInput('hour2', 'Hour', placeholder = 'eg. 06'),
            #   textInput('hour3', 'Hour', placeholder = 'eg. 06')
            # ),
            
            textInput('station', label = 'Station ID', placeholder = 'eg. CAP1-2') %>%
              helper(content = 'station'),

            textInput('event', label = 'Event ID', placeholder = 'eg. 001') %>%
              helper(content = 'event'),

            numericInput('imageVolume', label = 'Image Volume ', value = 108155 ) %>%
              helper(content = 'imageVolume'),

            numericInput('binSize', label = 'Bin Size', value = 5) %>%
              helper(content = 'binSize'),

            textInput('basepath', label = 'Base Path', placeholder = 'E:/VP_data', value = 'C:/data') %>%
              helper(content = 'basepath'), # 'C:/data'

            ##OPTIONAL QC PARAMETERS##
            
            actionBttn('reset', label = 'Reset Ranges', style = 'jelly', icon(name = 'remove', lib = 'glyphicon')),
            
            numericRangeInput("sal_range", label = h3("Salinity Range"), value = c(28, 35)),
            
            numericRangeInput("temp_range", label = h3("Temperature Range"), value = c(0, 15)),
            
            numericRangeInput("pres_range", label = h3("Pressure Range"), value = c(0, 500)),
            
            numericRangeInput("hr_range", label = h3("Time Range (hr)"), value = c(0, 24), step = 0.1)
        ),
        
# * Main panel layout ----        
        mainPanel(
            # Output: Tabset w/ plots, summary, and table 
            tabsetPanel(type = "tabs",
                        tabPanel("CTD Plot", 
                                 fluidRow(
                                   # ctd path plot
                                   column(3, offset = 9,
                                          downloadButton('ctdsave1', label = 'Save') %>%
                                            helper(content = 'save')),
                                   column(12, plotOutput("ctdplot"))
                                 ),
                                 fluidRow(
                                   # ctd profiles (two distinct plots)
                                   column(3, offset = 9,
                                          downloadButton('ctdsave2_3', label = 'Save')), # they save together
                                   column(6, plotOutput("ctdplot2")),
                                   column(6, plotOutput("ctdplot3"))
                                 ),
                                 fluidRow(
                                   # temperature contour
                                   column(3, offset = 9,
                                          downloadButton('ctdsave4', label = 'Save')),
                                   column(12, plotOutput("ctdplot4"))
                                 ),
                                 fluidRow(
                                   #salinity contour
                                   column(3, offset = 9,
                                          downloadButton('ctdsave5', label = 'Save')),
                                   column(12, plotOutput("ctdplot5"))
                                 ),
                                 fluidRow(
                                   # TS plot
                                   column(3, offset = 9,
                                          downloadButton('ctdsave6', label = 'Save')),
                                   column(6, offset = 3, plotOutput("ctdplot6", width = 800, height = 800))
                                 )
                                 ),
                        
                        tabPanel("VPR Plot", 
                                 # profile plots
                                 fluidRow(
                                   column( 3, offset = 9,
                                   downloadButton('save1', label = 'Save') %>%
                                     helper(content = 'save'))),
                                 fluidRow(
                                   column(12, plotOutput("plot"))
                                 ),
                                 # concentration interpolation
                                   fixedRow(column(3, offset = 9,
                                          downloadButton('save2', label = 'Save'))),
                                 fixedRow(
                                   column(12, plotOutput('plot2'))),
                                   # temperature interpolation
                                   fixedRow(column(3, offset = 9,
                                          downloadButton('save3', label = 'Save'))),
                                   fixedRow(column(12, plotOutput('plot3'))),
                                   # salinity interpolation
                                   fixedRow(column(3, offset = 9,
                                          downloadButton('save4', label = 'Save'))),
                                   fixedRow(column(12, plotOutput('plot4'))),
                                   # TS plot
                                   column(3, offset = 9,
                                          downloadButton('save5', label = 'Save')),
                                   column(12, plotOutput('plot5')),
                                   # binned cast
                                   column(3, offset = 9,
                                          downloadButton('save6', label = 'Save')),
                                   column(12, plotOutput('plot6')) %>%
                                   helper(content = 'cast_plot')

                                 ), 
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Table", DT::dataTableOutput("ctdroi")),
                        tabPanel("Images",
                                 wellPanel(
                                 shinyDirButton("dir", "Choose ROI directory", "Upload") %>%
                                   helper(content = 'roi_dir'),
                                 fluidRow( 
                                   column(6,
                                   numericInput('num', 'Number of images shown', value = 10, step = 2)
                                   ),
                                   column(6,
                                   pickerInput(inputId = 'sorting', label = 'Image Sorting',
                                          choices = list('Time (default)', 'Small -> Large', 'Large -> Small', 'Custom'),
                                          selected = 'Time (default)') %>% # set default
                                   helper(content = 'image_sorting')
                                   )
                                   )
                                 ),
                                 conditionalPanel(condition = "input.sorting == 'Custom'",
                                                  wellPanel(
                                                    fluidRow(
                                                    column(6,
                                                           numericRangeInput(inputId = 'size_range', 
                                                                    label = 'Size Range (px)', 
                                                                    value = c(0, 500),
                                                                    min = 0
                                                                    ) %>%
                                                    helper(content = 'custom_size')
                                                    ),
                                                    column(6,
                                                  pickerInput(inputId = 'sorting_custom',
                                                              label = 'Image Sorting [custom]',
                                                              choices = list('Time (default)', 'Small -> Large', 'Large -> Small'),
                                                              selected = 'Time (default)'
                                                              ))
                                                  )
                                                  )
                                                  ),
                                 
                                 fluidRow(
                                     column(6, 
                                             imageOutput("image")
                                     ),
                                     
                                     column(6,  
                                            imageOutput("image2")
                                     )
                                 )
                        )
            )
        )
    )
)}

# Server ------------------------------------------------------------------------------------------
server <- function(input, output, session) {
# * Admin info ----
    #debugging
    observeEvent(input$browser,{
    browser()
  })
    observe(session$setCurrentTheme(
        if (isTRUE(input$dark_mode)) dark else light
    ))
    
    observe_helpers()
    # helper function
    ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
    
    
    
# * Load CTD data ----    
      ctd_dat <- reactive({
       # get CTD columns
        if(input$vpr_version == 'DAVPR 27 (BIO)'){
          ctd_col <- c("time_ms", "conductivity", "temperature", "pressure", "salinity", "NA", "fluorescence_mv",
                        "turbidity_mv", "oxygen_mv", "pitch_deg", "roll_deg", "image_num")
        } else if(input$vpr_version == 'DAVPR 14 (GW)') {
        ctd_col <- c("time_ms", "conductivity", "temperature", "pressure", "salinity", "fluor_ref", "fluorescence_mv",
                     "turbidity_ref", "turbidity_mv", "altitude_NA")
      }
        req(input$ctd_files)
        
        ctd_fns <- input$ctd_files
        
        ctd_dat <- vpr_ctd_read(ctd_fns$datapath, 
                                station_of_interest = input$station, 
                                day = input$day, 
                                hour = input$hour, 
                                col_list = ctd_col)
        
        ctd_dat$avg_hr <- ctd_dat$time_ms / 3.6e+06
        
        # check metadata 
        # ONLY CHECKS HOUR, DEV REQ
        # TO DO: Check day, tow and cruise
        validate( c(
           need(vpr_hour(input$ctd_files)$name == paste0('h',input$hour), "Please correct metadata! (Incorrect hour)")
           )
        )
        
        return(ctd_dat)
      })
    
    observeEvent(input$update, {
      # add loading icons
      waiter::Waiter$new(id = "ctdplot", color = 'blue')$show()
      waiter::Waiter$new(id = "ctdplot2", color = 'blue')$show()
      waiter::Waiter$new(id = "ctdplot3", color = 'blue')$show()
      waiter::Waiter$new(id = "ctdplot4", color = 'blue')$show()
      waiter::Waiter$new(id = "ctdplot5", color = 'blue')$show()
      waiter::Waiter$new(id = "ctdplot6", color = 'blue')$show()
    })
# * Set Q ranges ----
    observeEvent(input$ctd_files,{
      # reset ranges between files (default ranges - avoid cutting off data based on previous set)
      updateNumericRangeInput(session, 
                              inputId = 'sal_range',
                              value = c(0, 50))
      updateNumericRangeInput(session, 
                              inputId = 'temp_range',
                              value = c(0, 50))
      updateNumericRangeInput(session, 
                              inputId = 'pres_range',
                              value = c(0, 500))
      updateNumericRangeInput(session, 
                              inputId = 'hr_range',
                              value = c(0, 24))
    })
    observeEvent(input$reset,{
      #reset ranges to full data (reset button)
      updateNumericRangeInput(session, 
                              inputId = 'sal_range',
                              value = c(floor(min(ctd_dat()$salinity, na.rm = TRUE)),
                                        ceiling(max(ctd_dat()$salinity, na.rm = TRUE))))
      updateNumericRangeInput(session,
                              inputId = 'temp_range',
                              value = c(floor(min(ctd_dat()$temperature, na.rm = TRUE)),
                                        ceiling(max(ctd_dat()$temperature, na.rm = TRUE))))
      updateNumericRangeInput(session,
                              inputId = 'pres_range',
                              value = c(floor(min(ctd_dat()$pressure, na.rm = TRUE)),
                                        ceiling(max(ctd_dat()$pressure, na.rm = TRUE))))
      
      mval <- max(ctd_dat()$avg_hr) - min(ctd_dat()$avg_hr)
      updateNumericRangeInput(session,
                              inputId = 'hr_range',
                              value = c(0,
                                        ceiling_dec(mval, 1)))
    })
    # update ranges with plot updates, perpetuate user input
    observeEvent(input$update, {
      # salinity
      updateNumericRangeInput(session,
                              inputId = 'sal_range',
                              value = c(floor(min(dat_qc()$salinity, na.rm = TRUE)),
                                        ceiling(max(dat_qc()$salinity, na.rm = TRUE))) 
                              )
      # temperature
      updateNumericRangeInput(session,
                              inputId = 'temp_range',
                              value = c(floor(min(dat_qc()$temperature, na.rm = TRUE)),
                                        ceiling(max(dat_qc()$temperature, na.rm = TRUE))) 
      )
      #pressure
      updateNumericRangeInput(session,
                              inputId = 'pres_range',
                              value = c(floor(min(dat_qc()$pressure, na.rm = TRUE)),
                                        ceiling(max(dat_qc()$pressure, na.rm = TRUE))) 
      )
      # hr range
      mval <- max(dat_qc()$avg_hr) - min(dat_qc()$avg_hr)
      updateNumericRangeInput(session,
                              inputId = 'hr_range',
                              value = c(0,
                                        ceiling_dec(mval, 1))
      )
      })
   
# * CTD plotting functions ----       
      ctd_TS_profile_plot <- function(){
        isolate({
          p <- ggplot(ctd_dat()) +
            geom_point(aes(x = temperature, y = depth), col = 'red') +
            scale_y_reverse(name = 'Pressure [db]', limits = c(max(ctd_dat()$pressure)+2, 0))
          
          p_TS <- p + geom_point(aes(x = (salinity -25), y = depth), col = 'blue') +
            scale_x_continuous(name = expression(paste("Temperature [",degree,"C]")),sec.axis = sec_axis(~ . +25, name = 'Salinity [PSU]')) +
            theme(axis.line.x.bottom = element_line(colour = 'red'),
                  axis.ticks.x.bottom = element_line(colour = 'red'),
                  panel.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.line.y = element_line(linetype = 'solid'),
                  axis.line.x.top = element_line(colour = 'blue'),
                  axis.ticks.x.top = element_line(colour = 'blue'),
                  axis.title = element_text(size = 24), 
                  plot.title = element_text(size = 28),
                  axis.text = element_text(size = 20),
                  strip.text = element_text(size = 20)
            ) +
            ggtitle('CTD temperature and salinity') 
          
          return(p_TS)
        })
      }
      
      ctd_FD_profile_plot <- function(){
        isolate({
          p <- ggplot(ctd_dat()) +
            geom_point(aes(x = fluorescence_mv, y = depth), col = 'green') +
            scale_y_reverse(name = 'Pressure [db]', limits = c(max(ctd_dat()$pressure)+2, 0))
          
          p_FD <- p + geom_point(aes(x = (sigmaT  -20) * 20, y = depth)) +
            scale_x_continuous(name = 'Fluorescence [mv]',sec.axis = sec_axis(~. /20   +20, name = 'Density')) +
            theme(axis.line.x.bottom = element_line(colour = 'green'),
                  axis.ticks.x.bottom = element_line(colour = 'green'),
                  panel.background = element_blank(),
                  panel.grid = element_blank(),
                  axis.line.y = element_line(linetype = 'solid'),
                  axis.line.x.top = element_line(colour = 'black'),
                  axis.title = element_text(size = 24), 
                  plot.title = element_text(size = 28),
                  axis.text = element_text(size = 20), 
                  strip.text = element_text(size = 20)
            ) +
            ggtitle('CTD fluorescence and density')
          
          return(p_FD)
          
        })
      }
        
      ctd_path_plot <- function(){
          isolate({
            ctd_dat <- ctd_dat() %>%
              dplyr::mutate(., avg_hr = avg_hr - min(avg_hr, na.rm = TRUE))
            ggplot(data = ctd_dat ) +
              geom_point(aes(avg_hr, pressure)) +
              scale_x_continuous(name = 'Time [hr]') +
              scale_y_reverse(name = 'Pressure [db]') + 
              ggtitle(label = 'CTD/ VPR Path') + 
              theme(
                panel.background = element_blank(),
                panel.grid = element_blank(),
                plot.title = element_text(size = 28))
          })
        
        }
        
      ctd_t_contour <- function(){
          isolate({
            d <- ctd_dat()
            ctd_int <- interp::interp(x = d$avg_hr, y = d$depth, z = d$temperature, duplicate= 'strip')

            ctd_int$x <- ctd_int$x - min(ctd_int$x)
            d$avg_hr <- d$avg_hr - min(d$avg_hr)
            
            #set consistent x and y limits
            y_limits <- rev(range(ctd_int$y))
            x_limits <- range(ctd_int$x)
          
            if(max(x_limits) > max(d$avg_hr)){
               x_limits[2] <- max(d$avg_hr)
            }
          
            if(min(x_limits) < min(d$avg_hr)){
              x_limits[1] <- min(d$avg_hr)
            }
          
          cmpalf <- cmocean::cmocean('thermal')
          #make contour plot
          filled.contour(ctd_int$x, ctd_int$y, ctd_int$z, nlevels = 50,
                         color.palette = cmpalf,
                         ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Interpolated Temperature',
                         #add anotations
                         plot.axes = {
                           #add vpr path
                           points(d$avg_hr, d$depth, type = 'l')
                           #add axes
                           axis(1)
                           axis(2)
                           #add contour lines
                           contour(ctd_int$x, ctd_int$y, ctd_int$z, nlevels=10, add = T)
                         }) 
          })
        }
        
      ctd_s_contour <- function(){
          isolate({
            d <- ctd_dat()
            #interpolate data
            ctd_int <- interp::interp(x = d$avg_hr, y = d$depth, z = d$salinity, duplicate= 'strip')
            
            ctd_int$x <- ctd_int$x - min(ctd_int$x)
            d$avg_hr <- d$avg_hr - min(d$avg_hr)
            
            #set consistent x and y limits
            y_limits <- rev(range(ctd_int$y))
            x_limits <- range(ctd_int$x)
            
            if(max(x_limits) > max(d$avg_hr)){
              x_limits[2] <- max(d$avg_hr)
            }
            
            if(min(x_limits) < min(d$avg_hr)){
              x_limits[1] <- min(d$avg_hr)
            }
            cmpalf <- cmocean::cmocean('haline')
            #make contour plot
            filled.contour(ctd_int$x, ctd_int$y, ctd_int$z, nlevels = 50,
                           color.palette = cmpalf,
                           ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Interpolated Salinity',
                           #add annotations
                           plot.axes = {
                             #add vpr path
                             points(d$avg_hr , d$depth, type = 'l')
                             #add axes
                             axis(1)
                             axis(2)
                             #add contour lines
                             contour(ctd_int$x, ctd_int$y, ctd_int$z, nlevels=10, add = T)
                           })
          })
        }
        
      ctd_TS <- function() {
          isolate({
            ctd_oce <- vpr_oce_create(ctd_dat())
            plot(ctd_oce) # oce default plot is TS
          })
        }
        
# * Output CTD plots ----
      
      # CTD path plot
      output$ctdplot <- renderPlot({
        input$update
        ctd_path_plot()
      })
      
      output$ctdsave1 <- downloadHandler(
        filename = paste0(input$cruise, "_CTD", input$tow, "_d", input$day, "_h", input$hour, "_path_plot.png"), 
        content = function(file) {
          png(file,
              width = input$shiny_width,
              height = input$shiny_height)
          ctd_path_plot()
          dev.off()
        }
      )
      
      # CTD TS profile plot
      output$ctdplot2 <- renderPlot({
        input$update
        ctd_TS_profile_plot()
      })
      
      # CTD FD profile plot
      output$ctdplot3 <- renderPlot({
        input$update
        
        ctd_FD_profile_plot()
      })
      
      output$ctdsave2_3 <- downloadHandler(
        filename = paste0(input$cruise, "_CTD", input$tow, "_d", input$day, "_h", input$hour, "_profile_plot.png"), 
        content = function(file) {
          png(file,
              width = input$shiny_width,
              height = input$shiny_height)
          gridExtra::grid.arrange(ctd_TS_profile_plot(), ctd_FD_profile_plot(), nrow = 1)
          dev.off()
        }
      )
      
      # CTD Temp Contour plot
      output$ctdplot4 <- renderPlot({
        input$update
        ctd_t_contour()
      })
      
      output$ctdsave4 <- downloadHandler(
        filename = paste0(input$cruise, "_CTD", input$tow, "_d", input$day, "_h", input$hour, "_temp_contour_plot.png"), 
        content = function(file) {
          png(file,
              width = input$shiny_width,
              height = input$shiny_height)
          ctd_t_contour()
          dev.off()
        }
      )
      
      # CTD Salinity Contour plot
      output$ctdplot5 <- renderPlot({
        input$update
        ctd_s_contour()
      })
      
      output$ctdsave5 <- downloadHandler(
        filename = paste0(input$cruise, "_CTD", input$tow, "_d", input$day, "_h", input$hour, "_sal_contour_plot.png"), 
        content = function(file) {
          png(file,
              width = input$shiny_width,
              height = input$shiny_height)
          ctd_s_contour()
          dev.off()
        }
      )
      
      # CTD TS plot
      output$ctdplot6 <- renderPlot({
        input$update
        ctd_TS()
      })
      
      output$ctdsave6 <- downloadHandler(
        filename = paste0(input$cruise, "_CTD", input$tow, "_d", input$day, "_h", input$hour, "_TS_plot.png"), 
        content = function(file) {
          png(file,
              width = input$shiny_width,
              height = input$shiny_height)
          ctd_TS()
          dev.off()
        }
      )

# * Load VPR data ----   
    datasetInput <- reactive({
        # get CTD data
        ctd_dat <- ctd_dat() 

        # interpret working directory notation in basepath
        if(length(grep(input$basepath, pattern = '~')) != 0){
          bp <- file.path(gsub(pattern = '~', replacement = getwd(), x = input$basepath))
        }else {bp <- input$basepath}
        
        # check basepath 
        validate( c(
          need(file.exists(bp) == TRUE, "Invalid basepath!")
        ))
        
        # get ROI data
        roi_files <- list.files(file.path(bp, input$cruise, 'rois', paste0('vpr', input$tow),paste0('d', input$day), paste0('h', input$hour) ))
        roi_num <- substr(roi_files, 5, nchar(roi_files) - 4)

        #get roi number that will match ctd time_ms
        rois <- as.numeric(substr(roi_num, 1, 8))
        #format as table to get frequency
        roi_table <- as.data.frame(table(rois))
        
        #subset ctd and roi data where time/roi identifier match
        ctd_sub <- which(ctd_dat$time_ms %in% rois)
        roi_sub <- which(rois %in% ctd_dat$time_ms)
        #subset data individually
        ctd_dat_sub <- ctd_dat[ctd_sub,]
        
        # check that there are valid ROIs
        validate(
          need(length(ctd_dat_sub[[1]]) != 0, 'No ROIs found for CTD data! Please verify metadata!')
        )
        
        #EC & KS fix 2019/08/08 due to error producing NA roi numbers
        roi_dat_sub <- rois[!duplicated(rois)]
        #roi_dat_sub <- rois[ctd_sub] # overwriting bug fix? EOG 3/3/22
        
        #combine roi and ctd data
        all_dat <- ctd_dat_sub %>%
            dplyr::mutate(., roi = roi_dat_sub) %>%
            dplyr::mutate(., n_roi = roi_table$Freq) #add n_roi (count of rois per second)
        
        #add  time(hr) to combined data frame
        all_dat <- all_dat %>%
            dplyr::mutate(., avg_hr = time_ms/3.6e+06) #%>%
             #dplyr::mutate(., avg_hr = avg_hr - min(avg_hr))
          
        
        all_dat <- all_dat %>% #filter data based on parameter ranges 
            dplyr::filter(., salinity > min(input$sal_range)) %>%
            dplyr::filter(., salinity < max(input$sal_range)) %>%
            dplyr::filter(., temperature > min(input$temp_range)) %>%
            dplyr::filter(., temperature < max(input$temp_range)) %>%
            dplyr::filter(., pressure > min(input$pres_range)) %>%
            dplyr::filter(., pressure < max(input$pres_range)) 
        
        validate(
          need(length(all_dat$time_ms) > 10, 'Too few valid data points available in your desired QC range! Please expand!')
        )
        
        return(all_dat)
    })
    
    # Bin VPR and CTD data
    binnedData <- reactive({
      # add loading icons
      waiter::Waiter$new(id = "plot", color = 'blue')$show()
      waiter::Waiter$new(id = "plot2", color = 'blue')$show()
      waiter::Waiter$new(id = "plot3", color = 'blue')$show()
      waiter::Waiter$new(id = "plot4", color = 'blue')$show()
      waiter::Waiter$new(id = "plot5", color = 'blue')$show()
      waiter::Waiter$new(id = "plot6", color = 'blue')$show()

        all_dat <- datasetInput()
        
        ctd_roi_oce <- vpr_oce_create(all_dat)
        
        # vpr_depth_bin <- bin_cast(ctd_roi_oce = ctd_roi_oce , imageVolume = input$imageVolume, binSize = input$binSize, rev = TRUE)
        #seperate into up and down casts before binning data
        #find upcasts
        upcast <- ctd_cast(data = ctd_roi_oce, cast_direction = 'ascending', data_type = 'df')
        upcast2 <- lapply(X = upcast, FUN = bin_calculate, binSize = input$binSize, imageVolume = input$imageVolume, rev = TRUE)
        upcast_df <- do.call(rbind, upcast2)
        #find downcasts
        downcast <- ctd_cast(ctd_roi_oce, "descending", "df")
        downcast2 <- lapply(X = downcast, FUN = bin_calculate, binSize = input$binSize, imageVolume = input$imageVolume, rev = TRUE)
        downcast_df <- do.call(rbind, downcast2)
        #combine_data into bins
        vpr_depth_bin <- rbind(upcast_df, downcast_df)
        vpr_depth_bin <- data.frame(vpr_depth_bin)
        
        #zero time values for neat plot axes 
        vpr_depth_bin <- vpr_depth_bin %>%
          dplyr::mutate(., avg_hr = avg_hr - min(all_dat$avg_hr)) # EOG: fixes issue where data was not aligning between all data and binned data
    })
    
    # filter data by time
    dat_qc <- reactive({
      
        all_dat <- datasetInput()
        
        all_dat <- all_dat %>%
          dplyr::mutate(., avg_hr = avg_hr - min(avg_hr))
        
        all_dat_q <- all_dat %>%
            dplyr::filter(., avg_hr < max(input$hr_range)) %>%
            dplyr::filter(., avg_hr > min(input$hr_range))

        validate(
          need(length(all_dat_q$time_ms) > 10, 'Too few valid data points available in your desired time range! Please expand!')
        )
        return(all_dat_q)
    })

# * Output data table ----
    output$ctdroi <- renderDataTable({
        input$update
        isolate({
          return(dat_qc())
        })
    })
  
# * Output Summary tab ----
    output$summary <- renderPrint({
        input$update
        isolate({
            all_dat <- datasetInput()
            
            cat(paste('Data Summary Report ', '\n'));
            cat(paste('Report processed:', as.character(Sys.time())), '\n');
            cat(paste('Cast: ', input$tow, '   Day: ', input$day, '   Hour: ', input$hour), '\n');
            
            cat(paste(' >>>>  Time ', '\n'));
            cat(paste('Data points: ', length(all_dat$time_ms), '\n'));
            cat(paste('Range: ', min(all_dat$time_ms),' - ', max(all_dat$time_ms), ' (ms) ', '\n'));
            cat(paste('Range: ', min(all_dat$avg_hr),' - ', max(all_dat$avg_hr), ' (hr) ', '\n'))

            cat(paste(' >>>>  Conductivity ', '\n'))
            cat(paste('Data points: ', length(all_dat$conductivity), '\n'))
            cat(paste('Range: ', min(all_dat$conductivity),' - ', max(all_dat$conductivity), '\n'))
            
            cat(paste(' >>>>  Temperature ', '\n'))
            cat(paste('Data points: ', length(all_dat$temperature), '\n'))
            cat(paste('Range: ', min(all_dat$temperature),' - ', max(all_dat$temperature), ' (c) ', '\n'))
            
            cat(paste(' >>>>  Pressure ', '\n'))
            cat(paste('Data points: ', length(all_dat$pressure), '\n'))
            cat(paste('Range: ', min(all_dat$pressure),' - ', max(all_dat$pressure), ' (db) ', '\n'))
            
            cat(paste(' >>>>  Salinity ', '\n'))
            cat(paste('Data points: ', length(all_dat$salinity), '\n'))
            cat(paste('Range: ', min(all_dat$salinity),' - ', max(all_dat$salinity), ' (PSU) ', '\n'))

            cat(paste(' >>>>  Fluorescence ', '\n'))
            cat(paste('Data points: ', length(all_dat$fluorescence_mv), '\n'))
            cat(paste('Range: ', min(all_dat$fluorescence_mv),' - ', max(all_dat$fluorescence_mv), ' (mv)', '\n'))

            cat(paste(' >>>>  Turbidity ', '\n'))
            cat(paste('Data points: ', length(all_dat$turbidity_mv), '\n'))
            cat(paste('Range: ', min(all_dat$turbidity_mv),' - ', max(all_dat$turbidity_mv), ' (mv) ', '\n'))

            cat(paste(' >>>>  ROI count ', '\n'))
            cat(paste('Data points: ', length(all_dat$n_roi), '\n'))
            cat(paste('Range: ', min(all_dat$n_roi),' - ', max(all_dat$n_roi), ' (counts) ', '\n'))
            
            cat(paste(' >>>>  Sigma T ', '\n'))
            cat(paste('Data points: ', length(all_dat$sigmaT), '\n'))
            cat(paste('Range: ', min(all_dat$sigmaT),' - ', max(all_dat$sigmaT), '\n'))
        })
        
    })
    
# * VPR plotting functions ----
    
    plot_profile <- function(){
        
        isolate({
            vpr_depth_bin <- binnedData()
            
            vpr_sel <- vpr_depth_bin %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))
            
            validate(
              need(length(vpr_sel$avg_hr) > 10, "Too few valid data points in selected QC range! Please expand!")
            )

            ctd_plots <- vpr_plot_profile(taxa_conc_n = vpr_sel, taxa_to_plot = NULL, plot_conc = FALSE)
            
            # plot concentration
            pp <- ggplot(vpr_sel) +
              geom_point(aes(x = depth, y = conc_m3)) + 
              stat_summary_bin(aes(x = depth, y = conc_m3), fun = 'mean', col = 'red', geom = 'line', size = 3)  +
              scale_x_reverse(name = 'Depth [m]') +
              scale_y_continuous(name = expression('ROI Concentration / m'^'3')) +
              # ggtitle('Concentrations') +
              theme_classic() +
              theme(panel.background = element_blank(),
                    panel.grid = element_blank(),
                    strip.text = element_text(size = 18),
                    plot.title = element_text(size = 25),
                    axis.title = element_text(size = 20))+
              coord_flip()
            
            p <- gridExtra::grid.arrange(ctd_plots, pp, nrow = 1)
            
            return(p)
            
        })    
    }
    
    plot_conc <- function(){
      isolate({
        
        vpr_depth_bin <- binnedData()
        all_dat <- datasetInput()
        
        vpr_sel_bin <- vpr_depth_bin %>%
          # dplyr::mutate(., avg_hr = avg_hr - min(avg_hr)) %>%
           dplyr::filter(., avg_hr <= max(input$hr_range)) %>%
           dplyr::filter(., avg_hr >= min(input$hr_range))
        
        sel_dat <- all_dat %>%
           dplyr::mutate(., avg_hr = avg_hr - min(avg_hr)) %>%
           dplyr::filter(., avg_hr <= max(input$hr_range)) %>%
           dplyr::filter(., avg_hr >= min(input$hr_range))
        
        validate(
          need(length(vpr_sel_bin$avg_hr) > 10, "Too few valid data points in selected QC range! Please expand!")
        )
        
        #interpolate data
        vpr_int <- akima::interp(x = vpr_sel_bin$avg_hr, y = vpr_sel_bin$depth, z = vpr_sel_bin$conc_m3, duplicate= 'strip')
        
        #set consistent x and y limits
        y_limits <- rev(range(vpr_int$y))
        x_limits <- range(input$hr_range)
        
        if(max(x_limits) > max(vpr_sel_bin$avg_hr)){
          x_limits[2] <- max(vpr_sel_bin$avg_hr)
        }
        cmpalf <- cmocean::cmocean('matter')
        cmo_data <- cmpalf(100)
        

        taxa_dat_zero <- vpr_sel_bin %>%
          dplyr::filter(., conc_m3 == 0)
        
        taxa_dat <- vpr_sel_bin %>%
          dplyr::filter(., conc_m3 > 0)
        
       
        df <- akima::interp2xyz(vpr_int, data.frame = TRUE)
        
        
        
        #make contour plot
        p <- ggplot(df) +
           geom_tile(aes(x = x, y = y, fill = z)) +
          labs(fill = 'Concentration [m -3]') +
          scale_y_reverse(name = "Depth [m]") +
          scale_x_continuous(name = "Time [h]") +
          theme_classic() +
          geom_contour(aes(x = x, y = y, z = z), col = "black") +
          geom_text_contour(aes(x = x, y = y, z = z), col = 'white', check_overlap = TRUE, size = 8)+
          scale_fill_gradientn(colours = cmo_data, na.value = 'gray')+
           geom_line(data = sel_dat, aes(x = avg_hr, y = depth), col = 'snow4', inherit.aes = FALSE) +
           geom_point(data = taxa_dat, aes(x = avg_hr, y = depth, size = conc_m3), inherit.aes = FALSE, pch = 21, alpha = 0.5, fill = 'black', colour = 'white')+
           geom_point(data = taxa_dat_zero, aes(x = avg_hr, y = depth), pch = 7, colour = 'gray', alpha = 0.7) +
          ggtitle("Concentration" ) +
          labs(size = expression("Concentration /m" ^3), fill = expression("Concentration /m" ^3))+
          scale_size_continuous(range = c(0, 20)) +
          # facet_wrap(~taxa, ncol = 1, scales = 'free') +
          theme(legend.key.size = unit(0.8, 'cm'),
                axis.title = element_text(size = 20),
                strip.text = element_text(size = 20),
                plot.title = element_text(size = 32),
                axis.ticks = element_line(size = 1, lineend = 'square'),
                axis.text = element_text(size = 30),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 15)
          )
         return(p)
        # filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
        #                color.palette = cmpalf, key.title = title("Conc/m3"),
        #                ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration',
        #                #add anotations
        #                plot.axes = {
        #                  #add bubbles
        #                  points(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, pch = 0)
        #                  #add vpr path
        #                  points(sel_dat$avg_hr - min(sel_dat$avg_hr), sel_dat$depth, type = 'l')
        #                  #add axes
        #                  axis(1)
        #                  axis(2)
        #                  #add contour lines
        #                  contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = TRUE, col = 'white', labcex = 1.5)
        #                  #enlarge bubble size based on concentration
        #                  symbols(vpr_sel_bin$avg_hr[vpr_sel_bin$conc_m3 != 0], 
        #                          vpr_sel_bin$depth[vpr_sel_bin$conc_m3 != 0],
        #                          circles = vpr_sel_bin$conc_m3,
        #                          fg = "darkgrey", bg = "black", 
        #                          inches = 0.3, add = TRUE)
        #                })
      })
    }
    
    plot_conctemp <- function(){
      isolate({
        vpr_depth_bin <- binnedData()
        all_dat <- datasetInput()
        
        vpr_sel_bin <- vpr_depth_bin %>%
          dplyr::filter(., avg_hr < max(input$hr_range)) %>%
          dplyr::filter(., avg_hr > min(input$hr_range))
        
        sel_dat <- all_dat %>%
          dplyr::mutate(., avg_hr = avg_hr - min(avg_hr)) %>%
          dplyr::filter(., avg_hr < max(input$hr_range)) %>%
          dplyr::filter(., avg_hr > min(input$hr_range))
        
        validate(
          need(length(vpr_sel_bin$avg_hr) > 10, "Too few valid data points in selected QC range! Please expand!")
        )
        
        #interpolate data
        vpr_int <- akima::interp(x = vpr_sel_bin$avg_hr, y = vpr_sel_bin$depth, z = vpr_sel_bin$temperature, duplicate= 'strip')
        
        #set consistent x and y limits
        y_limits <- rev(range(vpr_int$y))
        x_limits <- range(input$hr_range)
        
        if(max(x_limits) > max(vpr_sel_bin$avg_hr)){
          x_limits[2] <- max(vpr_sel_bin$avg_hr)
        }
        cmpalf <- cmocean::cmocean('thermal')
        cmo_data <- cmpalf(100)
        
        
        taxa_dat_zero <- vpr_sel_bin %>%
          dplyr::filter(., conc_m3 == 0)
        
        taxa_dat <- vpr_sel_bin %>%
          dplyr::filter(., conc_m3 > 0)
        
        
        df <- akima::interp2xyz(vpr_int, data.frame = TRUE)
        
        #make contour plot
        p <- ggplot(df) +
          geom_tile(aes(x = x, y = y, fill = z)) +
          labs(fill = 'Temperature [deg C]') +
          scale_y_reverse(name = "Depth [m]") +
          scale_x_continuous(name = "Time [h]") +
          theme_classic() +
          geom_contour(aes(x = x, y = y, z = z), col = "black") +
          geom_text_contour(aes(x = x, y = y, z = z), col = 'white', check_overlap = TRUE, size = 8)+
          scale_fill_gradientn(colours = cmo_data, na.value = 'gray')+
          geom_line(data = sel_dat, aes(x = avg_hr , y = depth), col = 'snow4', inherit.aes = FALSE) +
          geom_point(data = taxa_dat, aes(x = avg_hr, y = (min_depth+max_depth)/2, size = conc_m3), pch = 21, alpha = 0.5, fill = 'black', colour = 'white')+
          geom_point(data = taxa_dat_zero, aes(x = avg_hr, y = min_depth), pch = 7, colour = 'gray', alpha = 0.7) +
          ggtitle("Concentration over Temperature" ) +
          labs(size = expression("Concentration /m" ^3), fill = expression(paste(degree,"C")))+
          scale_size_continuous(range = c(0, 20)) +
          # facet_wrap(~taxa, ncol = 1, scales = 'free') +
          theme(legend.key.size = unit(0.8, 'cm'),
                axis.title = element_text(size = 20),
                strip.text = element_text(size = 20),
                plot.title = element_text(size = 32),
                axis.ticks = element_line(size = 1, lineend = 'square'),
                axis.text = element_text(size = 30),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 15)
          )
        return(p)
        # cmpalf <- cmocean::cmocean('thermal')
        # #make contour plot
        # filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
        #                color.palette = cmpalf,
        #                ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Temperature',
        #                #add anotations
        #                plot.axes = {
        #                  #add bubbles
        #                  points(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, pch = ".")
        #                  #add vpr path
        #                  points(sel_dat$avg_hr - min(sel_dat$avg_hr), sel_dat$depth, type = 'l')
        #                  #add axes
        #                  axis(1)
        #                  axis(2)
        #                  #add contour lines
        #                  contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
        #                  #enlarge bubble size based on concentration
        #                  symbols(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, circles = vpr_sel_bin$conc_m3, 
        #                          fg = "darkgrey", bg = "black", inches = 0.3, add = T)
        #                }) 
      })
      
    }
    
    plot_concsal <- function(){
      
      isolate({
        vpr_depth_bin <- binnedData()
        all_dat <- datasetInput()
        
        vpr_sel_bin <- vpr_depth_bin %>%
          dplyr::filter(., avg_hr < max(input$hr_range)) %>%
          dplyr::filter(., avg_hr > min(input$hr_range))
        
        sel_dat <- all_dat %>%
          dplyr::mutate(., avg_hr = avg_hr - min(avg_hr)) %>%
          dplyr::filter(., avg_hr < max(input$hr_range)) %>%
          dplyr::filter(., avg_hr > min(input$hr_range))
        
        validate(
          need(length(vpr_sel_bin$avg_hr) > 10, "Too few valid data points in selected QC range! Please expand!")
        )
        
        vpr_int <- akima::interp(x = vpr_sel_bin$avg_hr, y = vpr_sel_bin$depth, z = vpr_sel_bin$salinity, duplicate = 'strip')
        
        #set consistent x and y limits
        y_limits <- rev(range(vpr_int$y))
        x_limits <- range(input$hr_range)
        
        if(max(x_limits) > max(vpr_sel_bin$avg_hr)){
          x_limits[2] <- max(vpr_sel_bin$avg_hr)
        }
        
        cmpalf <- cmocean::cmocean('haline')
        cmo_data <- cmpalf(100)
        
        
        taxa_dat_zero <- vpr_sel_bin %>%
          dplyr::filter(., conc_m3 == 0)
        
        taxa_dat <- vpr_sel_bin %>%
          dplyr::filter(., conc_m3 > 0)
        
        
        df <- akima::interp2xyz(vpr_int, data.frame = TRUE)
        
        #make contour plot
        p <- ggplot(df) +
          geom_tile(aes(x = x, y = y, fill = z)) +
          labs(fill = 'Salinity [PSU]') +
          scale_y_reverse(name = "Depth [m]") +
          scale_x_continuous(name = "Time [h]") +
          theme_classic() +
          geom_contour(aes(x = x, y = y, z = z), col = "black") +
          geom_text_contour(aes(x = x, y = y, z = z), col = 'white', check_overlap = TRUE, size = 8)+
          scale_fill_gradientn(colours = cmo_data, na.value = 'gray')+
          geom_line(data = sel_dat, aes(x = avg_hr, y = depth), col = 'snow4', inherit.aes = FALSE) +
          geom_point(data = taxa_dat, aes(x = avg_hr, y = (min_depth+max_depth)/2, size = conc_m3), pch = 21, alpha = 0.5, fill = 'black', colour = 'white')+
          geom_point(data = taxa_dat_zero, aes(x = avg_hr, y = min_depth), pch = 7, colour = 'gray', alpha = 0.7) +
          ggtitle("Concentration over Salinity" ) +
          labs(size = expression("Concentration /m" ^3), fill = "PSU")+
          scale_size_continuous(range = c(0, 20)) +
          # facet_wrap(~taxa, ncol = 1, scales = 'free') +
          theme(legend.key.size = unit(0.8, 'cm'),
                axis.title = element_text(size = 20),
                strip.text = element_text(size = 20),
                plot.title = element_text(size = 32),
                axis.ticks = element_line(size = 1, lineend = 'square'),
                axis.text = element_text(size = 30),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 15)
          )
        return(p)
        # cmpalf <- cmocean::cmocean('haline')
        # #make contour plot
        # filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
        #                color.palette = cmpalf,
        #                ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Salinity',
        #                #add annotations
        #                plot.axes = {
        #                  #add bubbles
        #                  points(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, pch = ".")
        #                  #add vpr path
        #                  points(sel_dat$avg_hr - min(sel_dat$avg_hr), sel_dat$depth, type = 'l')
        #                  #add axes
        #                  axis(1)
        #                  axis(2)
        #                  #add contour lines
        #                  contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
        #                  #enlarge bubbles based on concentration
        #                  symbols(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, circles = vpr_sel_bin$conc_m3, 
        #                          fg = "darkgrey", bg = "black", inches = 0.3, add = T)
        #                }) 
      })
      
    }
    
    roi_ts_plot <- function(){
      isolate({
        dat <- binnedData()
        dat_f <- dat %>%
          dplyr::filter(., avg_hr < max(input$hr_range)) %>%
          dplyr::filter(., avg_hr > min(input$hr_range))
        vpr_plot_TS(x = dat_f, var = "conc_m3") +
          theme(legend.key.size = unit(0.8, 'cm'),
                axis.title = element_text(size = 20),
                strip.text = element_text(size = 20),
                plot.title = element_text(size = 32),
                axis.ticks = element_line(size = 1, lineend = 'square'),
                axis.text = element_text(size = 30),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 15)
          )
      })
    }
    
    cast_plot <- function(){
      isolate({
        dat <- binnedData()
        dat_f <- dat %>%
          dplyr::filter(., avg_hr < max(input$hr_range)) %>%
          dplyr::filter(., avg_hr > min(input$hr_range))
        ggplot(dat_f) +
          geom_point(aes(x = avg_hr, y = depth, col = towyo), size = 4) +
          scale_y_reverse(name = 'Depth [m]') +
          scale_x_continuous(name = 'Time [hr]')+
          scale_color_discrete(name = 'Cast Identifier') +
          ggtitle('Binned Casts')+
          theme(panel.background = element_blank(),
                panel.grid = element_blank(),
                legend.key.size = unit(0.8, 'cm'),
                axis.title = element_text(size = 20),
                strip.text = element_text(size = 20),
                plot.title = element_text(size = 32),
                axis.ticks = element_line(size = 1, lineend = 'square'),
                axis.text = element_text(size = 30),
                legend.text = element_text(size = 10),
                legend.title = element_text(size = 15)
        )
      })
    }

# * Output VPR plots ----
    # profile plots
    output$plot <- renderPlot({
        input$update
        plot_profile()
    })
    
    output$save1 <- downloadHandler(
        filename = paste0(input$cruise, "_VPR", input$tow, "_d", input$day, "_h", input$hour, "profile_plot.png"), 
        content = function(file) {
            png(file,
                width = input$shiny_width,
                height = input$shiny_height)
            plot_profile()
            dev.off()
        }
    )
 
    # concentration contour plot
    output$plot2 <- renderPlot({
        input$update
        plot_conc()
    })

    output$save2 <- downloadHandler(
        filename = paste0(input$cruise, "_VPR", input$tow, "_d", input$day, "_h", input$hour, "_conc_plot.png"), 
        content = function(file) {
            png(file,
                width = input$shiny_width,
                height = input$shiny_height)
            plot_conc()
            dev.off()
        }
    )

    # temperature contour plot
    output$plot3 <- renderPlot({
        input$update
        plot_conctemp()
    })
    
    output$save3 <- downloadHandler(
        filename = paste0(input$cruise, "_VPR", input$tow, "_d", input$day, "_h", input$hour, "_tempconc_plot.png"), 
        content = function(file) {
            png(file,
                width = input$shiny_width,
                height = input$shiny_height)
            plot_conctemp()
            dev.off()
        }
    )

    # salinity contour plot
    output$plot4 <- renderPlot({
        input$update
        plot_concsal()
    })
    
    output$save4 <- downloadHandler(
        filename = paste0(input$cruise, "_VPR", input$tow, "_d", input$day, "_h", input$hour, "_salconc_plot.png"), 
        content = function(file) {
            png(file,
                width = input$shiny_width,
                height = input$shiny_height)
            plot_concsal()
            dev.off()
        }
    )
    
   # ROI TS plot
    output$plot5 <- renderPlot({
      input$update
      roi_ts_plot()
    })
    
    output$save5 <- downloadHandler(
      filename = paste0(input$cruise, "_VPR", input$tow, "_d", input$day, "_h", input$hour, "_TSroi_plot.png"), 
      content = function(file) {
        png(file,
            width = input$shiny_width,
            height = input$shiny_height)
        roi_ts_plot()
        dev.off()
      }
    )

    # cast identifier plot
    output$plot6 <- renderPlot({
      input$update
      cast_plot()
    })
    
    output$save6 <- downloadHandler(
      filename = paste0(input$cruise, "_VPR", input$tow, "_d", input$day, "_h", input$hour, "_cast_plot.png"),
      content = function(file) {
        png(file,
            width = input$shiny_width,
            height = input$shiny_height)
        cast_plot()
        dev.off()
      }
    )
# * Image data loading ----
    shinyDirChoose(input, 'dir',roots = c('C:/' = 'C:/', 'D:/' = 'D:/', 'E:/' = 'E:/', 'F:/' = 'F:/'  )  )
    dir <- reactive(input$dir)
    output$dir <- renderPrint(dir())
    
    # get valid path
    path <- reactive({
        home <- input$dir$root
        file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    })
    
    # find ROI files
    # output$files <- renderPrint(list.files(path())) # not needed? EOG 3/10/22

    # check for user directory input
    values <- reactiveValues(
        upload_state = NULL
    )
    observeEvent(input$dir, {
        values$upload_state <- 'uploaded'
    })

    # get image path
     imgs_path <- reactive({
        if (is.null(values$upload_state)) {
            imgs_path <-  paste0(input$basepath, '/', input$cruise, "/rois/vpr", input$tow,"/d", input$day, "/h", input$hour, "/")
        } else if (values$upload_state == 'uploaded') {
            imgs_path <- path()
        }
     })
    
     
    # get utility values for splitting images into columns
    ipgmax <- reactive(input$num)
    num_col <- reactive({
      num_col <- ipgmax()/2
      if(input$sorting == 'Custom'){
        if(length(custom_index()) < input$num){
         num_col <- round(length(custom_index())/ 2, 0)
         validate({
           need(num_col != 0, 'No valid images detected in custom range!')
         })
        }
      }
      return(num_col)
      })
    
    # in case number of images requested is greater than available images in custom index
    
    # get all image files
    imgss <- reactive({
      imgss <- list.files(imgs_path(), pattern = '.tif', full.names = TRUE)
    })
    
    # check which images are valid ROIs with associated CTD data
    validroi <- reactive ({
      all_dat <- dat_qc()
      imgdat <- imgss()
      roi_ids <- vpr_roi(imgdat)
      roi_ids <- as.numeric(substr(roi_ids, 1, 8))
      validroi_ind <- which(roi_ids %in% all_dat$roi)
      validroi <- roi_ids[roi_ids %in% all_dat$roi]
      
      # error if there are no valid rois
      validate(
        need(length(validroi) != 0, "No valid ROI images found")
      )
      return(validroi)
    })
    
    # subset image files based on CTD data
    img_valid <- reactive({
        all_dat <- dat_qc()
        roi_ids <- vpr_roi(imgss())
        roi_ids <- as.numeric(substr(roi_ids, 1, 8))
        validroi_ind <- which(roi_ids %in% all_dat$roi)
        imgdat <- imgss()
        img_valid <- imgdat[validroi_ind]
    })
    
    # get image metadata
    d <- reactive({
      isolate({
        withProgress(message ='Sorting images...', { # adds progress bar
          d <- exif_read(img_valid())
          }) # check all images -- v slow
      })
    }) # %>%
   # bindCache(d()) # TODO - work on caching solution
    
    # sort valid images based on user selection
    sorted_index <- reactive({
      input$update
      isolate({
          dimdat <- (d()$ImageHeight + d()$ImageWidth) /2 # calulate avg dimension
          names(dimdat) <- d()$FileName
        # get an ordered index 
      if(isolate(input$sorting) == 'Small -> Large'){
        sorted_index <- order(dimdat, decreasing = FALSE)
      }else if(isolate(input$sorting) == 'Large -> Small'){
        sorted_index <- order(dimdat, decreasing = TRUE)
      } 
      })
    })
    
    # get custom index
    custom_index <- reactive({
      input$update
      isolate({
        dimdat <- (d()$ImageHeight + d()$ImageWidth) /2 # calulate avg dimension
        names(dimdat) <- d()$FileName
        
        dimdat_c <- dimdat[dimdat > min(input$size_range) & dimdat < max(input$size_range)]
        
        
        if(isolate(input$sorting_custom) == 'Small -> Large'){
          dimdat_c_o <- dimdat_c[order(dimdat_c, decreasing = FALSE)]
          imgvalid_c <- paste0(imgs_path(), names(dimdat_c_o))
          custom_index <- list()
          for( i in 1:length(dimdat_c_o)){ # looping is not an ideal solution 
            custom_index[[i]] <- which(img_valid() == imgvalid_c[i])
          }
          custom_index <- unlist(custom_index)
         
        }else if(isolate(input$sorting_custom) == 'Large -> Small'){
          dimdat_c_o <- dimdat_c[order(dimdat_c, decreasing = TRUE)]
          imgvalid_c <- paste0(imgs_path(), names(dimdat_c_o))
          custom_index <- list()
          for( i in 1:length(dimdat_c_o)){ # looping is not an ideal solution 
            custom_index[[i]] <- which(img_valid() == imgvalid_c[i])
          }
          custom_index <- unlist(custom_index)

        } else if (isolate(input$sorting_custom) == 'Time (default)'){ # TODO: check that this is properly ordered by time
          custom_index <- which(dimdat %in% dimdat_c) # check this line with an example where size subset should be small
        }
         return(custom_index)
      })
    })
    
   
    
    # set colum 1 index based on sorted values or default
    index <- reactive({
      if(input$sorting == 'Time (default)'){
        index <- seq(1, num_col())
      } else if (input$sorting %in% c('Small -> Large', 'Large -> Small')){ 
        index <- sorted_index()[1:num_col()]
      } else if (input$sorting == 'Custom'){
       index <- custom_index()[1:num_col()]
      }
    })

    # set index for column 2
    index2 <- reactive({
      if(input$sorting == 'Time (default)'){
        index2 <- seq(num_col()+1, num_col()*2)
      } else if (input$sorting %in% c('Small -> Large', 'Large -> Small')){
        index2 <- sorted_index()[num_col()+1: num_col()*2]
      } else if (input$sorting == 'Custom'){
        index2 <- custom_index()[num_col()+1: num_col()*2]
      }
    })
    
    # read in first column of images
    imgobj <- reactive ({
      # add loading icon
      waiter::Waiter$new(id = "image", color = 'blue')$show()
      
       img1 <- img_valid()[index()]
       roi_id_string <- stringr::str_c(vpr_roi(img1), sep = ',') 
       image <- image_read(na.omit(img1))
       image <- image_annotate(image, roi_id_string, color = 'red', size = 15)
       image <- image_append(image_border(image, color = 'white', geometry = '10x8'), stack = TRUE)
    })
    
    # read in second column of images
    imgobj2 <- reactive ({
      # add loading icon
      waiter::Waiter$new(id = "image2", color = 'blue')$show()
      
       img2 <- img_valid()[index2()]
       roi_id_string <- stringr::str_c(vpr_roi(img2), sep = ',') 
       image <- image_read(na.omit(img2))
       image <- image_annotate(image, roi_id_string, color = 'red', size = 15)
       image <- image_append(image_border(image, color = 'white'), stack = TRUE)
    })
   
# * Output Image Gallery ----
    # output first column of images
    output$image <- renderImage({
      input$update 
      isolate({
        tmpfile <- imgobj() %>%
            image_write(tempfile(fileext='png'), format = 'png')
        list(src = tmpfile, contentType = "image/png")
      })
    }, deleteFile = FALSE)

    # output second column of images
    output$image2 <- renderImage({
      input$update
        isolate({
          tmpfile <- imgobj2() %>%
            image_write(tempfile(fileext='png'), format = 'png')
          list(src = tmpfile, contentType = "image/png")
        })
    }, deleteFile = FALSE)
}

# Run App ------------------------------------------------------------------------------------------
shinyApp(ui, server)
