context("Read in CTD data...")
library(testthat)

test_file <- system.file('www/IML2018051/rois/vpr0/d286',
                        'h22ctd.dat',
                        package = 'VPR_viewer',
                        mustWork = TRUE)
ctd_col <- c("time_ms", "conductivity", "temperature", "pressure", "salinity", "fluor_ref", "fluorescence_mv",
             "turbidity_ref", "turbidity_mv", "altitude_NA")
test_that("CTD file is read in", {
  expect_silent(ctd_dat <- vpr_ctd_read(test_file,
                          station_of_interest = NA,
                          day = '286',
                          hour = '22',
                          col_list = ctd_col)
  )
})

# test_that("tests on GH are working",{
#   expect_equal(1, 1)
# })