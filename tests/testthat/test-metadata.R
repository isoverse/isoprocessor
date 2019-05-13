context("Metadata")

test_that("metadata addition works", {

  expect_warning(tryCatch(iso_add_metadata(), error=function(e){}, warning=function(w) warning(w)), "deprecated")

})
