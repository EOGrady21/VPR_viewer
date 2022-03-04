# Plankton Plotter
A shiny app to visualize plankton data and images from the Video Plankton Recorder (VPR). 

## Getting Started
In order to run this app the user should have (at a minimum) current versions of R & RStudio. The easiest way to get this app is to clone this repository through Git using GitHub Desktop (it can also be downloaded manually) using the green 'Code' button at the top right of the repository. 

Once the code is downloaded onto your local machine you should be able to open the project in RStudio (`plankton_plotter.RProj`). Inside the project you can open the app by opening the script `app.R`, then click the interactive 'Run App' button in RStudio (at the top right of your editor) or type `runApp()` in the console. 

Note that you may be prompted to install required packages before running the app for the first time. The `librarian` package is used to manage required packages. Please install this package before running the app for the first time using `install.packages("librarian")` 

The app expects certain directory structures which would be produced by AutoDeck and expected at the next stage of processing (Visual Plankton). These directory structures should be maintained for best practices and to avoid unexpected errors.

The directory structure is described as follows in the DAVPR manual:

"  (drive):\data\cruise\rois\vprxxx\dxxx\hxx
(drive) can be any drive available on the computer at hand
\data should be there, no freedom
\cruise is the cruise name during which the data was collected, is user specified.
\rois should be there, no freedom
\vprxxx should be there where xxx is a cast number (no leading zeros)
\dxxx\hxx are automatically generated in the proper format with dxxx being the day of the year and hxx the hour of the day as computed from the date information contained in the input file name. If you want these day and hour value to be consistent between data collection and data processing, you need to make sure that both the VPR and the processing computer are set to the same time zone, GMT being our preferred setting.

In the day folder ((drive):\data\cruise\rois\vprxxx\dxxx), JpegLS_ADeck will also create ctd data files with names hxxctd.dat."


## About
This shiny app is in development at the Bedford Institute of Oceanogrpahy. It is currently being tested with 2008 version of the “Digital AutoVPR”, from SeaScan Inc, used in a tow-yo deployment pattern.

This app is designed to assist in the visualization of data from the VPR by resesarch scientists while they are onboard research cruises. It is meant to be a quick and easy way to check data before full processing can take place. The app displays both CTD data and ROI (Region of Interest) data. Each Region of Interest (ROI) represents an object detected within a VPR frame by the autoDeck software. AutoDeck is a software provided with the VPR by SeaScan which processes the raw video output from the VPR and outputs a series of image files (ROIs).

The app has space for inputting metadata and CTD data files, as well as 4 tabs. The tabs are 'Plot', 'Summary', 'Table', and 'Images'. 

The Plot tab displays 4 plots including a profile summary of CTD data (temperature, salinity, density and fluorescence), a profile summary of ROI concentration (shown in ROI per Litre). ROI concentration can be used as a proxy for plankton concentration until further processing. The Plot tab also displays 3 filled contour plots displaying ROI concentration along the path of the VPR over interpolated concentration, temperature and salinity. 
![Web capture_28-2-2022_125647_127 0 0 1](https://user-images.githubusercontent.com/38440373/156025360-6c6b61c6-a741-4681-a782-33b87672c129.jpeg)

The Summary tab displays the range of each data variable found in the CTD data file. Note that if the sliders are used to narrow the data range, this will also be perpetuated through the summary output. Eg. Total data may range from 0-100 DB but if the pressure slider (in the metadata input), is capped at 50 DB, the summary will show that data ranges from 0-50 DB.

The Table tab displays a data table with all the CTD and ROI data, as well as some calculated vairables and metadata. Note that the table will also reflect changes made using the metadata slider ranges. 


The Images tab displays an image gallery of ROIs for easy browsing and initial identification of plankton. Note that these images may also be subset if metadata sliders are used to subset data. 

![Web capture_28-2-2022_125717_127 0 0 1](https://user-images.githubusercontent.com/38440373/156175380-41f14a9b-9697-4ce5-9967-48b7231c0eb8.jpeg)

## The Inputs
The app is able to read in .dat CTD files which are produced by AutoDeck from the raw VPR data. These CTD data files should be distinct for every hour of the VPR deployment. At this time only one 'hour' of data can be displayed at a time.

The expected CTD data columns are as follows:

  - Time (ms)	
  - Conductivity	
  - Temperature	
  - Pressure	
  - Salinity	
  - Fluorescence (reference)	
  - Fluorescence (mv)	
  - Turbidity (reference)	
  - Turbidity (mv)	
  - Altitude

The app is also able to display, in a gallery format, the ROI images output from autodeck. In the Images tab, a user may select the directory of ROI images which will be displayed.

## The Outputs
The plots displayed in the Plots tab may be downloaded and saved as .png files by using the 'Save' buttons found at the upper right hand corner of each plot. The plot PNGs are automatically named based on metadata provided in the app.

## Quality Control
Some initial quality control or subsetting of data may be performed using the metadata input and slider ranges on the left hand side panel. These sliders allow a user to specify the range of certain variables to be displayed. This can be useful in two particular scenarios, A) Where a user finds there are extraneous data points outside normal  ranges which are making the plots difficult to read, B) Where a user would like to focus visualization on a specific subset of the data captured during a deployment. If a user decides to subset the data using the slider ranges, this subset will be perpetuated through all tabs including Summary, Table and Images.
In scenario B, a user may want to browse ROI images within a specific range, this can be very useful if a 'layer' can be visualized through the initial data or even on other instruments onboard. Using the slider ranges to isolate this layer, a user can then browse the images tab to get an initial idea of what the species composition of this layer might be. 



## Sample Data
Sample data is included with this app for the purposes of testing. It should not be considered valid. It is a minimal example in order to test the plots and other app features. If you are looking for a valid example data set to visualize or play with the app please contact the maintainer directly. 

