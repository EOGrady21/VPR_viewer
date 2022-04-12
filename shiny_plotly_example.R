library(base64enc)
library(shiny)
library(shinydashboard)
library(plotly)
library(magick)
library(vprr)
library(plyr)

# this is a minimal working example of how we could get an interactive plotly/shiny 
# app to display ROI images associated with CTD data

#  notes:
# - images are displayed on hover (this could be adjusted to click or double click)
# - the images need to be read in, converted to PNG, then converted again to base64enc 
#          (this is the slowest part of the backend)
# - this example is currently tested on a 1000 image dataset (typical dataset would be x10)
# - takes about 3 minutes to load all data, once data is loaded, plot is quite quick to react
# - this example is hard coded with data on EOG's computer, if this was brouught to VPRv,
#       would need to readjust some sections to create reactivity


ui <- dashboardPage(
  dashboardHeader(title = "ROI test"),
  dashboardSidebar(
    #fileInput('ctd_files', label = 'CTD Files', multiple = FALSE, accept = '.dat') ,
    #textInput('imgs_path', 'Image Path', value = 'c:/data/cruise_COR2019002/rois/vpr33/d241/h11/')
  ),
  dashboardBody(tags$head(tags$style(
    HTML("img.small-img {
          max-width: 75px;
          }")
  )),
  plotlyOutput("hoverplot"))
)

server <- function(input, output, session) {
  
  
  # read in VPR data
   ctd_fn <- 'c:/data/cruise_COR2019002/rois/vpr33/d241/h11ctd.dat'
  #ctd_dat <- reactive({
    #req(input$ctd_files)
    
   # ctd_fn <- input$ctd_files
    
   ctd_dat <-  vpr_ctd_read(ctd_fn, station_of_interest = NA)
  #})
  
  # get associated ROI images
   imgs_path <- 'c:/data/cruise_COR2019002/rois/vpr33/d241/h11/'
  
  # imgs_path <- input$imgs_path
  all_img_path <- list.files(imgs_path, full.names = TRUE)
  roi_ids <- vpr_roi(all_img_path)
  roi_df <- data.frame(roi_id = unlist(roi_ids)) %>%
    dplyr::mutate(., img_path = all_img_path) %>%
    dplyr::mutate(., time_ms = substr(roi_id, 1, 8))
  
  
  # combine data
  all_dat <- join(ctd_dat, roi_df, by = 'time_ms')
  
  # subset only data points with ROI images
  roi_all_dat <- all_dat[!is.na(all_dat$roi_id),]
  
  # get image data into PNG format
  
  imgs <- roi_all_dat$img_path
  imgobj <- image_read(imgs)
  tmpfile <- list()
  withProgress(message = 'Writing PNGs...', {
    for(i in 1:length(imgobj)){
      incProgress(1/length(imgobj))
      tmpfile[[i]] <- imgobj[i] %>%
      image_write(tempfile(fileext='.png'), format = 'png')
  }
  })

  # load PNGs into base64
   withProgress(message = 'Encoding images...', {
     img_png <- vapply(tmpfile, function(x){base64enc::dataURI(file = x)}, FUN.VALUE = character(1L))
   })
 
   # test on a small data subset
   # test_dat <- roi_all_dat[1:10,]
   plot_dat <- roi_all_dat %>%
     dplyr::mutate(., pngpath = img_png) %>%
     dplyr::mutate(., avg_hr = time_ms/ 3.6e+06)

   # plotly
  output$hoverplot <- renderPlotly({
    plot_ly(
      plot_dat,
      x         = ~ avg_hr,
      y         = ~ depth,
      type      = 'scatter',
      mode      = 'markers',
      hoverinfo = 'none',
      source = "hoverplotsource",
      customdata = ~ pngpath
    ) %>%
      event_register('plotly_hover') %>%
      event_register('plotly_unhover') %>%
      layout(yaxis = list(autorange = "reversed"))
  })
  
  # hover mechanics
  hover_event <- reactive({
    event_data(event = "plotly_hover", source = "hoverplotsource")
  })
  
  unhover_event <- reactive({
    event_data(event = "plotly_unhover", source = "hoverplotsource")
  })
  
  hoverplotlyProxy <- plotlyProxy("hoverplot", session)
  
  observeEvent(unhover_event(), {
    hoverplotlyProxy %>%
      plotlyProxyInvoke("relayout", list(images = list(NULL)))
  })
  
 # load images on hover
  observeEvent(hover_event(), {
    hoverplotlyProxy %>%
      plotlyProxyInvoke("relayout", list(images = list(
        list(
          source = hover_event()$customdata,
          xref = "x",
          yref = "y",
          x = hover_event()$x,
          y = hover_event()$y,
          sizex = 20,
          sizey = 20,
          opacity = 1
        )
      )))
  })
}

# run app
shinyApp(ui = ui, server = server)
