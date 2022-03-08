suppressWarnings(librarian::shelf(shiny, vprr, magick, ggplot2, metR, dplyr, DT, base64enc, shinyFiles, bslib, thematic,
                 shinyWidgets, shinyhelper, spelling, exiftoolr, quiet = TRUE))

# install exiftool to check image metadata
# required when run on new machine
is_exiftool_available <- function() {
  !is.null(tryCatch(exif_version(), error = function(e) NULL))
}
if(is_exiftool_available() == FALSE){install_exiftool()}

thematic_shiny() #theme plots to match bs_theme() argument
light <- bs_theme(bootswatch = 'zephyr')
dark <-  bs_theme(bootswatch = 'superhero')

ui <- fluidPage(
  
 #debugging
  actionButton("browser", "browser"),
  tags$script("$('#browser').hide();"),
 
    
    theme = light,
    switchInput(inputId = "dark_mode", label = "Dark mode", value = FALSE, size = 'mini'),  
    # plot formatting (fix from https://stackoverflow.com/questions/45642283/how-to-save-png-image-of-shiny-plot-so-it-matches-the-dimensions-on-my-screen)
    
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
    
    titlePanel("Plankton Plotter"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Input Metadata."),
            fluidRow(
                column(3, offset = 5,
                       actionButton('update', label = 'Update', icon(name = 'refresh', lib = 'glyphicon')) %>%
                         helper(content = 'update'),
                )),
            
            fileInput('ctd_files', label = 'CTD Files', multiple = FALSE, accept = '.dat') %>%
              helper(content = 'ctd_files'),
            
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
            #min and max values of each parameter
            
            sliderInput("sal_range", label = h3("Salinity Range"), min = 15, 
                        max = 45, value = c(28, 35)),
            
            sliderInput("temp_range", label = h3("Temperature Range"), min = 0, 
                        max = 30, value = c(0, 15)),
            
            sliderInput("pres_range", label = h3("Pressure Range"), min = 0, 
                        max = 500, value = c(0, 500)),
            
            sliderInput("hr_range", label = h3("Time Range (hr)"), min = 0, 
                        max = 24, value = c(0, 24), step = 0.1)
        ),
        
        
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", 
                                 fluidRow(
                                     column(3, offset = 9,
                                            downloadButton('save1', label = 'Save') %>%
                                              helper(content = 'save'))
                                 ),
                                 fluidRow(
                                     column(12, plotOutput("plot")),
                                     column(3, offset = 9,
                                            downloadButton('save2', label = 'Save')
                                     ),
                                     column(12, plotOutput('plot2')),
                                     column(3, offset = 9,
                                            downloadButton('save3', label = 'Save')
                                     ),
                                     column(12, plotOutput('plot3')),
                                     column(3, offset = 9,
                                            downloadButton('save4', label = 'Save')
                                     ),
                                     column(12, plotOutput('plot4')),
                                 )),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Table", DT::dataTableOutput("ctdroi")),
                        tabPanel("Images",
                                 shinyDirButton("dir", "Choose ROI directory", "Upload") %>%
                                   helper(content = 'roi_dir'),
                                 numericInput('num', 'Number of images shown', value = 10, step = 2),
                                 
                                 fluidRow(
                                     column( 1, 
                                             imageOutput("image")
                                     ),
                                     
                                     column(4, offset = 5, 
                                            imageOutput("image2")
                                     )
                                 )
                        )
            )
        )
    )
)


server <- function(input, output, session) {
    #debugging
    observeEvent(input$browser,{
    browser()
  })
    observe(session$setCurrentTheme(
        if (isTRUE(input$dark_mode)) dark else light
    ))
    
    observe_helpers()
    
    datasetInput <- reactive({
        
        req(input$ctd_files)
        
        ctd_fns <- input$ctd_files
        
        ctd_dat <- vpr_ctd_read(ctd_fns$datapath, station_of_interest = input$station, day = input$day, hour = input$hour)
        
        # check metadata 
        # ONLY CHECKS HOUR, DEV REQ
        # TO DO: Check day, tow and cruise
        validate( c(
           need(vpr_hour(input$ctd_files)$name == paste0('h',input$hour), "Please correct metadata! (Incorrect hour)")
           )
        )
        
        #vpr_oce_create(ctd_dat)
        #get ROI data
        
        if(length(grep(input$basepath, pattern = '~')) != 0){
          bp <- file.path(gsub(pattern = '~', replacement = getwd(), x = input$basepath))
        }else {bp <- input$basepath}
        
        # check basepath 
        validate( c(
          need(file.exists(bp) == TRUE, "Invalid basepath!")
        ))
        
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
            dplyr::mutate(., avg_hr = time_ms/3.6e+06)
        
        # all_dat_o <- all_dat #save original data as seperate object for comparison
        all_dat <- all_dat %>% #filter data based on parameter ranges set in step 1
            dplyr::filter(., salinity > min(input$sal_range)) %>%
            dplyr::filter(., salinity < max(input$sal_range)) %>%
            dplyr::filter(., temperature > min(input$temp_range)) %>%
            dplyr::filter(., temperature < max(input$temp_range)) %>%
            dplyr::filter(., pressure > min(input$pres_range)) %>%
            dplyr::filter(., pressure < max(input$pres_range)) #%>%
        #dplyr::filter(., avg_hr > min(input$hr_range)) %>%
        # dplyr::filter(., avg_hr < max(input$hr_range))
    })
    
    
    binnedData <- reactive({

        all_dat <- datasetInput()
        
        ctd_roi_oce <- vpr_oce_create(all_dat)
        
        vpr_depth_bin <- bin_cast(ctd_roi_oce = ctd_roi_oce , imageVolume = input$imageVolume, binSize = input$binSize, rev = TRUE)
    })
    
    
    dat_qc <- reactive({
    #input$update
        all_dat <- datasetInput()
        
        all_dat_q <- all_dat %>%
            dplyr::mutate(., avg_hr = avg_hr - min(avg_hr))%>%
            dplyr::filter(., avg_hr < max(input$hr_range)) %>%
            dplyr::filter(., avg_hr > min(input$hr_range))

        return(all_dat_q)
    })
    
    output$ctdroi <- renderDataTable({
        
        input$update
        
        isolate({
            
        return(dat_qc())
        })
        
    })
    
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
    
    plot_profile <- function(){
        
        isolate({

            vpr_depth_bin <- binnedData()
            
            vpr_sel <- vpr_depth_bin %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))

            vpr_plot_profile(taxa_conc_n = vpr_sel, taxa_to_plot = NULL, plot_conc = TRUE)
            
        })    
    }
    
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
    
    
    plot_conc <- function(){
        isolate({
            
            vpr_depth_bin <- binnedData()
            all_dat <- datasetInput()
            
            vpr_sel_bin <- vpr_depth_bin %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))
            
            sel_dat <- all_dat %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))
            
            #temperature
            #interpolate data
            vpr_int <- akima::interp(x = vpr_depth_bin$avg_hr, y = vpr_depth_bin$depth, z = vpr_depth_bin$conc_m3, duplicate= 'strip')

            #plot
            #set consistent x and y limits
            y_limits <- rev(range(vpr_int$y))
            x_limits <- range(input$hr_range)
            
            if(max(x_limits) > max(vpr_depth_bin$avg_hr)){
                x_limits[2] <- max(vpr_depth_bin$avg_hr)
            }
            
            cmpalf <- cmocean::cmocean('matter')
            #make contour plot
            filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
                           color.palette = cmpalf,
                           #color.palette = colorRampPalette(c( "blue", 'red')),
                           ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration',
                           #add anotations
                           plot.axes = {
                               #add bubbles
                               points(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, pch = ".")
                               #add vpr path
                               points(sel_dat$avg_hr - min(sel_dat$avg_hr), sel_dat$depth, type = 'l')
                               #add axes
                               axis(1)
                               axis(2)
                               #add contour lines
                               contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                               #enlarge bubble size based on concentration
                               symbols(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, circles = vpr_sel_bin$conc_m3, 
                                       fg = "darkgrey", bg = "grey", inches = 0.3, add = T)
                           }) 
        })
    }
    
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
    
    plot_conctemp <- function(){
        
        isolate({
            vpr_depth_bin <- binnedData()
            all_dat <- datasetInput()
            
            vpr_sel_bin <- vpr_depth_bin %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))
            
            sel_dat <- all_dat %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))
            
            #temperature
            #interpolate data
            vpr_int <- akima::interp(x = vpr_depth_bin$avg_hr, y = vpr_depth_bin$depth, z = vpr_depth_bin$temperature, duplicate= 'strip')

            #plot
            #set consistent x and y limits
            y_limits <- rev(range(vpr_int$y))
            x_limits <- range(input$hr_range)
            
            if(max(x_limits) > max(vpr_depth_bin$avg_hr)){
                x_limits[2] <- max(vpr_depth_bin$avg_hr)
            }
            
            cmpalf <- cmocean::cmocean('thermal')
            #make contour plot
            filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
                           color.palette = cmpalf,
                           #color.palette = colorRampPalette(c( "blue", 'red')),
                           ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Temperature',
                           #add anotations
                           plot.axes = {
                               #add bubbles
                               points(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, pch = ".")
                               #add vpr path
                               points(sel_dat$avg_hr - min(sel_dat$avg_hr), sel_dat$depth, type = 'l')
                               #add axes
                               axis(1)
                               axis(2)
                               #add contour lines
                               contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                               #enlarge bubble size based on concentration
                               symbols(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, circles = vpr_sel_bin$conc_m3, 
                                       fg = "darkgrey", bg = "grey", inches = 0.3, add = T)
                           }) 
        })
        
    }
    
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
    
    plot_concsal <- function(){
        
        isolate({
            vpr_depth_bin <- binnedData()
            all_dat <- datasetInput()
            
            vpr_sel_bin <- vpr_depth_bin %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))
            
            sel_dat <- all_dat %>%
                dplyr::filter(., avg_hr < max(input$hr_range)) %>%
                dplyr::filter(., avg_hr > min(input$hr_range))

            vpr_int <- akima::interp(x = vpr_depth_bin$avg_hr, y = vpr_depth_bin$depth, z = vpr_depth_bin$salinity, duplicate = 'strip')

            #set consistent x and y limits
            y_limits <- rev(range(vpr_int$y))
            x_limits <- range(input$hr_range)
            
            if(max(x_limits) > max(vpr_depth_bin$avg_hr)){
                x_limits[2] <- max(vpr_depth_bin$avg_hr)
            }
            
            cmpalf <- cmocean::cmocean('haline')
            #make contour plot
            filled.contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels = 50,
                           color.palette = cmpalf,
                           #color.palette = colorRampPalette(c( "blue", 'red')),
                           ylim = y_limits, xlim = x_limits, xlab = "Time (h)", ylab = "Depth (m)", main = 'Concentration over Salinity',
                           #add annotations
                           plot.axes = {
                               #add bubbles
                               points(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, pch = ".")
                               #add vpr path
                               points(sel_dat$avg_hr - min(sel_dat$avg_hr), sel_dat$depth, type = 'l')
                               #add axes
                               axis(1)
                               axis(2)
                               #add contour lines
                               contour(vpr_int$x, vpr_int$y, vpr_int$z, nlevels=10, add = T)
                               #enlarge bubbles based on concentration
                               symbols(vpr_sel_bin$avg_hr, vpr_sel_bin$depth, circles = vpr_sel_bin$conc_m3, 
                                       fg = "darkgrey", bg = "grey", inches = 0.3, add = T)
                           }) 
        })
        
    }
    
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
    
    ##ROI images

    shinyDirChoose(input, 'dir',roots = c('C:/' = 'C:/', 'D:/' = 'D:/', 'E:/' = 'E:/', 'F:/' = 'F:/'  )  )
    dir <- reactive(input$dir)
    output$dir <- renderPrint(dir())
    
    # path
    path <- reactive({
        # parseDirPath(roots = c('C:/' = 'C:/', 'D:/' = 'D:/', 'E:/' = 'E:/', 'F:/' = 'F:/'),
        #              selection = input$dir) # try something new, not working
        home <- input$dir$root
        file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    })
    
    # files
   output$files <- renderPrint(list.files(path()))

    values <- reactiveValues(
        upload_state = NULL
    )
    
    observeEvent(input$dir, {
        values$upload_state <- 'uploaded'
    })
    # browser()
     imgs_path <- reactive({
      
        if (is.null(values$upload_state)) {

            imgs_path <-  paste0(input$basepath, '/', input$cruise, "/rois/vpr", input$tow,"/d", input$day, "/h", input$hour, "/")

        } else if (values$upload_state == 'uploaded') {
            imgs_path <- path()

        }
     })
    
    
    ipgmax <- reactive(input$num)
    num_col <- reactive(ipgmax()/2)
    index <- reactive(seq(1,num_col()))
    index2 <- reactive(seq(num_col()+1, num_col()*2))
    
    
    imgobj <- reactive ({
     # isolate({
      
      all_dat <- dat_qc()
      imgss <- list.files(imgs_path(), pattern = '.tif', full.names = TRUE)
      
      roi_ids <- vpr_roi(imgss)
      roi_ids <- as.numeric(substr(roi_ids, 1, 8))
      validroi_ind <- which(roi_ids %in% all_dat$roi)
      validroi <- roi_ids[roi_ids %in% all_dat$roi]
      
      validate(
        need(length(validroi) != 0, "No valid ROI images found")
      )
      
      imgss <- imgss[validroi_ind]
      
      imgss <- imgss[index()]
      
      roi_id_string <- stringr::str_c(validroi, sep = ',')
      
      image <- image_read(na.omit(imgss))
      
      image <- image_annotate(image, roi_id_string, color = 'red', size = 15)
      
      image <- image_append(image_border(image, color = 'white', geometry = '10x8'), stack = TRUE)
      
    })
    #})
   
    
    output$image <- renderImage({
      input$update
      isolate({
      tmpfile <- imgobj() %>%
            image_write(tempfile(fileext='png'), format = 'png')
        
        list(src = tmpfile, contentType = "image/png")
      })
    }, deleteFile = FALSE)
   
    
    
    imgobj2 <- reactive ({
     # isolate ({
      all_dat <- dat_qc()
      imgss <- list.files(imgs_path(), pattern = '.tif', full.names = TRUE)
      
      roi_ids <- vpr_roi(imgss)
      roi_id_string <- stringr::str_c(roi_ids, sep = ',')
      
      roi_ids <- as.numeric(substr(roi_ids, 1, 8))
      validroi_ind <- which(roi_ids %in% all_dat$roi)
      validroi <- roi_ids[roi_ids %in% all_dat$roi]
      
      validate(
        need(length(validroi) != 0, "No valid ROI images found")
      )
      
      imgss <- imgss[validroi_ind]
      imgss <- imgss[index2()]
      
      image <- image_read(na.omit(imgss))
      
      image <- image_annotate(image, roi_id_string, color = 'red', size = 15)
      
      
      image <- image_append(image_border(image, color = 'white'), stack = TRUE)
      
    })
  #  })
    
    output$image2 <- renderImage({
      input$update
        
        isolate ({
        tmpfile <- imgobj2() %>%
            image_write(tempfile(fileext='png'), format = 'png')
        
        list(src = tmpfile, contentType = "image/png")
        })
    }, deleteFile = FALSE)
    
    
    
    
}




shinyApp(ui, server)
