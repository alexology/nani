test_that("nani", {

  data("animal_numbers")
  data("coef_table")


  # animal adults
  load(system.file("testdata", "aau_test.rda", package="nani"))
  aau_test[is.na(aau_test)] <- 0
  aau_test <- dplyr::as_tibble(aau_test)
  attr(aau_test, "type") <- c("adult_animal_unit")

  aau_res <- adult_animal_unit(animal_numbers, coef_table)

  expect_equal(aau_res, aau_test)

})
