# run tests
library(testthat)
library(shinytest)

# test_that("Application works", {
#   # Use compareImages=FALSE because the expected image screenshots were created
#   # on a Mac, and they will differ from screenshots taken on the CI platform,
#   # which runs on Linux.
#   expect_pass(testApp(".", compareImages = FALSE))
# })

test_that('Spelling in help files',{
  
  fns <- list.files(path = 'helpfiles/', full.names = TRUE)
  
  WORDLIST <- c('AutoDeck', 'basepath', 'CTD', 'IML', 'ROI', 'VPR')
  a <- spelling::spell_check_files(fns, ignore = WORDLIST)
  expect_equal(length(a$word), 0)
})
