ctd_bin_calculate <- function(data, binSize = 1, imageVolume, rev = FALSE){
  
  # browser()
  cast_id <-unique(data$cast_id)
  
  cast_id <-unique(data$cast_id)
  max_cast_depth <- max(data$depth) # ADDED BY KS TO IDENTIFY EACH TOWYO CHUNK
  
  p <- data$depth
  max_depth <- max(p, na.rm = TRUE)
  min_depth <- min(p, na.rm = TRUE)
  x_breaks <- seq(from = floor(min_depth), to = ceiling(max_depth), by = binSize)
  if (rev == TRUE){
    x_breaks <- seq(from = ceiling(max_depth), to = floor(min_depth), by = - binSize) #reversed by KS
  }
  
  # error when cast is too small
  if(max_depth - min_depth < binSize){
    warning(paste('Cast', cast_id, 'is too small to calculate information for bins of size', binSize))
    data.frame(NULL)
  }else{
    
    
    # Get variables of interest using oce bin functions
    
    min_time_s <- oce::binApply1D(p, data$time/1000, xbreaks = x_breaks, min)$result
    max_time_s <- oce::binApply1D(p, data$time/1000, xbreaks = x_breaks, max)$result
    min_depth <- oce::binApply1D(p, data$depth, xbreaks = x_breaks, min)$result
    max_depth <- oce::binApply1D(p, data$depth, xbreaks = x_breaks, max)$result
    # n_roi_bin <- oce::binApply1D(p, data$n_roi, xbreaks = x_breaks, sum)$result
    temperature <- oce::binApply1D(p, data$temperature, xbreaks = x_breaks, mean)$result
    salinity <- oce::binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$result
    density <- oce::binApply1D(p, data$sigmaT, xbreaks = x_breaks, mean)$result
    fluorescence <- oce::binApply1D(p, data$fluorescence_mv, xbreaks = x_breaks, mean)$result
    turbidity <- oce::binApply1D(p, data$turbidity_mv, xbreaks = x_breaks, mean)$result
    avg_hr <- oce::binApply1D(p, data$time/(1000*3600), xbreaks = x_breaks, mean)$result
    if (rev == TRUE){
      
      depth <- rev(oce::binApply1D(p, data$depth, xbreaks = x_breaks, mean)$xmids)
      
    }else{ # simplify?
      
      depth <- oce::binApply1D(p, data$salinity, xbreaks = x_breaks, mean)$xmids
      
    }
    # calculates number of frames captured per depth bin by counting number of pressure observations per bin
    n_frames <- oce::binApply1D(p, data$depth, xbreaks = x_breaks, length)$result # KS edit 10/9/19
    
    # WARNING
    # binApply1D does not calculate NAs, if there is binned depth range that does
    # not contain any data, the binApply function will not create an empty or NA
    # placeholder bin in that case the result length will be different than the
    # length of midpoints since the variable "pressure" is a mid point calculation it is used to
    # test for non existent empty bins. If there are non existant empty bins,
    # binMean1D will calculate them as NA, this loop finds where the bins would
    # have been located and removes those indexes from the pressure vector so the
    # length of variables is all identical
    
    if (!(length(depth) == length(salinity))) {
      
      salinity_mean <- binMean1D(p, data$salinity, xbreaks = x_breaks)$result
      
      idx_rm <- which(is.na(salinity_mean))
      
      # informs user where bins were removed due to NAs
      # note if a bin is 'NA' typically because there is no valid data in that depth range,
      # if you have a lot of NA bins, think about increasing your binSize
      message(paste('Removed bins at', depth[idx_rm]))
      
      lp <- length(depth)
      depth <- depth[-idx_rm]
      if (length(n_frames) == lp){
        n_frames <- n_frames[-idx_rm]
      }
      
    }
    # make sure n_frames matches the length of other data frame rows
    if (length(n_frames) > length(depth)){
      n_frames <- n_frames[-length(n_frames)]
    }
    if( length(n_frames) < length(depth)){
      n_frames <- c(n_frames, 0)
    }
    if (length(n_frames) != length(depth)){
      length(n_frames) <- length(depth)
    }
    # Get derived variables
    
    time_diff_s <- max_time_s - min_time_s
    
    # calculate concentration based on opticalSetting
    
    # "Old way" of calculating concentration assuming constant frame rate of 15 fps
    # conc_m3 <- n_roi_bin/((imageVolume/1e09)*(15)*(time_diff_s)) #
    
    # "New way" of calculating concentration by summing volume associated with frames over depth bin
    vol_sampled_bin_m3 <- (imageVolume/1e09)*n_frames
    #conc_m3 <- n_roi_bin/(vol_sampled_bin_m3) # KS edit 10/9/19
    
    depth_diff <- max_depth - min_depth
    
    # Output
    data.frame(depth, min_depth, max_depth, depth_diff, min_time_s, max_time_s, time_diff_s,
               # n_roi_bin, conc_m3,
               temperature, salinity, density, fluorescence, turbidity, avg_hr, n_frames, vol_sampled_bin_m3,
               towyo = cast_id, max_cast_depth) # MAX CAST PRESSURE ADDED BY KS
  } # end else loop for size error
}