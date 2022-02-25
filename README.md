# Plankton Plotter
A shiny app to visualize plankton data and images from the Video Plankton Recorder (VPR). 

This shiny app is in development at Bedford Institute of Oceanogrpahy. It is currently being tested with *VPR version*, from SeaScan Inc, used in a tow-yo deployment pattern.

This app is designed to assist in the visualization of data from the VPR by resesarch scientists while they are onboard research cruises. It is meant to be a quick and easy way to check data before full processing can take place. The app displays both CTD data and ROI (Region of Interest) data. Each Region of Interest (ROI) represents an object detected within a VPR frame by the autoDeck software. AutoDeck is a software provided with the VPR by SeaScan which processes the raw video output from the VPR and outputs a series of image files (ROIs).

The app has space for inputting metadata and CTD data files, as well as 4 tabs. The tabs are 'Plot', 'Summary', 'Table', and 'Images'. 
The Plot tab displays 4 plots including a profile summary of CTD data (temperature, salinity, density and fluorescence), a profile summary of ROI concentration (shown in ROI per Litre). ROI concentration can be used as a proxy for plankton concentration until further processing. The Plot tab also displays 3 filled contour plots displaying ROI concentration along the path of the VPR over interpolated concentration, temperature and salinity. 
The Summary tab displays the range of each data variable found in the CTD data file.
The Table tab displays a data table with all the CTD and ROI data, as well as some calculated vairables and metadata.
The Images tab displays an image gallery of ROIs for easy browsing and initial identification of plankton.


## The Inputs
The app is able to read in .dat CTD files which are produced by AutoDeck from the raw VPR data. These CTD data files should be distinct for every hour of the VPR deployment. At this time only one 'hour' of data can be displayed at a time.
The app is also able to display, in a gallery format, the ROI images output from autodeck. In the Images tab, a user may select the directory of ROI images which will be displayed.

## The Outputs
The plots displayed in the Plots tab may be downloaded and saved as .png files by using the 'Save' buttons found at the upper right hand corner of each plot. The plot PNGs are automatically named based on metadata provided in the app.

