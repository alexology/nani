test_that("nani", {

  data("animal_numbers")
  data("coef_table")
  data("crop")
  data("fertilizers_quantity")
  data("cultivated_area")
  data("nfix_table")
  data("depositions")
  data("depositions_ssb")
  
  
  ### animal adults =======================================
  load(system.file("testdata", "aau_test.rda", package="nani"))
  aau_test[is.na(aau_test)] <- 0
  aau_test <- dplyr::as_tibble(aau_test)
  attr(aau_test, "type") <- c("adult_animal_unit")
  aau_test_01 <- aau_test
  i <- unlist(lapply(aau_test_01, is.numeric))
  aau_test_01[i] <- aau_test_01[i] / 10
  

  aau_res <- adult_animal_unit(animal_numbers, coef_table)

  expect_equal(aau_res, aau_test)
  expect_equal(adult_animal_unit(animal_numbers, coef_table, multiplier = 0.1), aau_test_01)
  
  
  
  ### crop production =======================================
  load(system.file("testdata", "crop_production_test.rda", package="nani"))
  crop_production_test[is.na(crop_production_test)] <- 0
  crop_production_test <- dplyr::as_tibble(crop_production_test)
  attr(crop_production_test, "type") <- c("crop_production")
  crop_production_test_01 <- crop_production_test
  i <- unlist(lapply(crop_production_test_01, is.numeric))
  crop_production_test_01[i] <- crop_production_test_01[i] / 10
  
  crop_production_res <- crop_production(crop, coef_table)
  
  expect_equal(crop_production_res, crop_production_test)
  expect_equal(crop_production(crop, coef_table, multiplier = 0.1), crop_production_test_01)
  
  
  
  ### animal feed =======================================
  load(system.file("testdata", "animal_feed_test.rda", package="nani"))
  animal_feed_test[is.na(animal_feed_test)] <- 0
  animal_feed_test <- dplyr::as_tibble(animal_feed_test)
  attr(animal_feed_test, "type") <- c("animal_feed")
  animal_feed_test_01 <- animal_feed_test
  i <- unlist(lapply(animal_feed_test_01, is.numeric))
  animal_feed_test_01[i] <- animal_feed_test_01[i] / 10
  
  animal_feed_res <- animal_feed(crop_production_res, coef_table)
  
  expect_equal(animal_feed_res, animal_feed_test)
  expect_equal(animal_feed(crop_production_res, coef_table, multiplier = 0.1), animal_feed_test_01)
  expect_error(animal_feed(crop, coef_table), "crop production is needed")
  
  
  
  ### animal production =======================================
  load(system.file("testdata", "animal_production_test.rda", package="nani"))
  animal_production_test[is.na(animal_production_test)] <- 0
  animal_production_test <- dplyr::as_tibble(animal_production_test)
  attr(animal_production_test, "type") <- c("animal_production")
  animal_production_test_01 <- animal_production_test
  i <- unlist(lapply(animal_production_test_01, is.numeric))
  animal_production_test_01[i] <- animal_production_test_01[i] / 10

  animal_production_res <- animal_production(animal_numbers, coef_table)

  expect_equal(animal_production_res, animal_production_test)
  expect_equal(animal_production(animal_numbers, coef_table, multiplier = 0.1), animal_production_test_01)
  
  
  
  ### fertilizers =======================================
  load(system.file("testdata", "fertilizers_test.rda", package="nani"))
  fertilizers_test[is.na(fertilizers_test)] <- 0
  fertilizers_test <- dplyr::as_tibble(fertilizers_test)
  attr(fertilizers_test, "type") <- c("fertilizers")
  fertilizers_test_01 <- fertilizers_test
  i <- unlist(lapply(fertilizers_test_01, is.numeric))
  fertilizers_test_01[i] <- fertilizers_test_01[i] / 10
  
  fertilizers_res <- fertilizers(fertilizers_quantity, coef_table)
  fertilizers_res_nani <- fertilizers(fertilizers_quantity, coef_table, multiplier = 1000)
  
  expect_equal(fertilizers_res, fertilizers_test)
  expect_equal(fertilizers(fertilizers_quantity, coef_table, multiplier = 0.1), fertilizers_test_01)
  
  

  ### animal excretion gross =======================================
  load(system.file("testdata", "gross_excretion_test.rda", package="nani"))
  gross_excretion_test[is.na(gross_excretion_test)] <- 0
  gross_excretion_test <- dplyr::as_tibble(gross_excretion_test)
  attr(gross_excretion_test, "type") <- c("animal_excretion_gross")
  gross_excretion_test_01 <- gross_excretion_test
  i <- unlist(lapply(gross_excretion_test_01, is.numeric))
  gross_excretion_test_01[i] <- gross_excretion_test_01[i] / 10
  
  gross_excretion_res <- animal_excretion(animal_numbers, coef_table, type = "gross")
  
  expect_equal(gross_excretion_res, gross_excretion_test)
  expect_equal(animal_excretion(animal_numbers, coef_table, multiplier = 0.1, type = "gross"), gross_excretion_test_01)
  expect_error(animal_excretion(animal_numbers, coef_table, type = "goss"), "type_excretion must be one of net or gross")
  
  
  ### animal consumption =======================================
  load(system.file("testdata", "animal_consumption_test.rda", package="nani"))
  animal_consumption_test[is.na(animal_consumption_test)] <- 0
  animal_consumption_test <- dplyr::as_tibble(animal_consumption_test)
  attr(animal_consumption_test, "type") <- c("animal_feed_consumption")
  animal_consumption_test_01 <- animal_consumption_test
  i <- unlist(lapply(animal_consumption_test_01, is.numeric))
  animal_consumption_test_01[i] <- animal_consumption_test_01[i] / 10
  
  animal_consumption_res <- animal_feed_consumption(animal_numbers, coef_table)
  
  expect_equal(animal_consumption_res, animal_consumption_test)
  expect_equal(animal_feed_consumption(animal_numbers, coef_table, multiplier = 0.1), animal_consumption_test_01)
  
  
  
  ### human consumption =======================================
  load(system.file("testdata", "human_consumption_test.rda", package="nani"))
  human_consumption_test[is.na(human_consumption_test)] <- 0
  human_consumption_test <- dplyr::as_tibble(human_consumption_test)
  attr(human_consumption_test, "type") <- c("human_food_consumption")
  human_consumption_test_01 <- human_consumption_test
  i <- unlist(lapply(human_consumption_test_01, is.numeric))
  human_consumption_test_01[i] <- human_consumption_test_01[i] / 10
  
  human_consumption_res <- human_food_consumption(human_population, coef_table)
  
  expect_equal(human_consumption_res, human_consumption_test)
  expect_equal(human_food_consumption(human_population, coef_table, multiplier = 0.1), human_consumption_test_01)
  
  
  
  ### human excretion =======================================
  load(system.file("testdata", "human_excretion_test.rda", package="nani"))
  human_excretion_test[is.na(human_excretion_test)] <- 0
  human_excretion_test <- dplyr::as_tibble(human_excretion_test)
  attr(human_excretion_test, "type") <- c("human_excretion")
  human_excretion_test_01 <- human_excretion_test
  i <- unlist(lapply(human_excretion_test_01, is.numeric))
  human_excretion_test_01[i] <- human_excretion_test_01[i] * 1000
  
  human_excretion_res <- human_excretion(human_population, coef_table)
  
  expect_equal(human_excretion_res, human_excretion_test)
  expect_equal(human_excretion(human_population, coef_table, multiplier = NULL), human_excretion_test_01)
  
  
  
  ### human non food production =======================================
  load(system.file("testdata", "human_non_food_production_test.rda", package="nani"))
  human_non_food_production_test[is.na(human_non_food_production_test)] <- 0
  human_non_food_production_test <- dplyr::as_tibble(human_non_food_production_test)
  attr(human_non_food_production_test, "type") <- c("human_non_food_production")
  human_non_food_production_test_01 <- human_non_food_production_test
  i <- unlist(lapply(human_non_food_production_test_01, is.numeric))
  human_non_food_production_test_01[i] <- human_non_food_production_test_01[i] * 1000

  human_non_food_production_test_res <- human_non_food_production(human_population, coef_table)

  expect_equal(human_non_food_production_test_res, human_non_food_production_test)
  expect_equal(human_non_food_production(human_population, coef_table, multiplier = 1000), human_non_food_production_test_01)


  
  ### human food =======================================
  load(system.file("testdata", "human_food_test.rda", package="nani"))
  human_food_test[is.na(human_food_test)] <- 0
  human_food_test <- dplyr::as_tibble(human_food_test)
  attr(human_food_test, "type") <- c("human_food")
  human_food_test_01 <- human_food_test
  i <- unlist(lapply(human_food_test_01, is.numeric))
  human_food_test_01[i] <- human_food_test_01[i] / 10
  
  human_food_res <- human_food(crop_production_res, coef_table)
  
  expect_equal(human_food_res, human_food_test)
  expect_equal(human_food(crop_production_res, coef_table, multiplier = 0.1), human_food_test_01)
  expect_error(human_food(crop, coef_table), "crop production is needed")
  
  
  
  ### animal excretion net =======================================
  load(system.file("testdata", "net_excretion_test.rda", package="nani"))
  net_excretion_test[is.na(net_excretion_test)] <- 0
  net_excretion_test <- dplyr::as_tibble(net_excretion_test)
  attr(net_excretion_test, "type") <- c("animal_excretion_net")
  net_excretion_test_01 <- net_excretion_test
  i <- unlist(lapply(net_excretion_test_01, is.numeric))
  net_excretion_test_01[i] <- net_excretion_test_01[i] / 10
  
  net_excretion_res <- animal_excretion(animal_numbers, coef_table, type = "net")
  
  expect_equal(net_excretion_res, net_excretion_test)
  expect_equal(animal_excretion(animal_numbers, coef_table, multiplier = 0.1, type = "net"), net_excretion_test_01)
  expect_error(animal_excretion(animal_numbers, coef_table, type = "goss"), "type_excretion must be one of net or gross")
  
  
  
  ### nfix =======================================
  load(system.file("testdata", "nfix_test.rda", package="nani"))
  nfix_test[is.na(nfix_test)] <- 0
  nfix_test <- dplyr::as_tibble(nfix_test)
  attr(nfix_test, "type") <- c("nitrogen_fixation")
  nfix_test_01 <- nfix_test
  i <- unlist(lapply(nfix_test_01, is.numeric))
  nfix_test_01[i] <- nfix_test_01[i] / 10
  
  nfix_res <- nitrogen_fixation(cultivated_area, crop, coef_table, nfix_table, multiplier = c(100, 0.01, 1))
  nfix_res_1 <- nitrogen_fixation(cultivated_area, crop, coef_table, nfix_table, multiplier = c(0.1))
  nfix_res_null <- nitrogen_fixation(cultivated_area, crop, coef_table, nfix_table, multiplier = NULL)
  i <- unlist(lapply(nfix_res_null, is.numeric))
  nfix_res_null[i] <- nfix_res_null[i] / 10
  
  nfix_res <- nfix_res[, match(colnames(nfix_test), colnames(nfix_res))]
  
  expect_equal(nfix_res, nfix_test)
  expect_equal(nfix_res_1, nfix_res_null)
  
  
  
  ### as nani =======================================
  load(system.file("testdata", "depositions_test.rda", package="nani"))
  depositions_test[is.na(depositions_test)] <- 0
  depositions_test <- dplyr::as_tibble(depositions_test)
  attr(depositions_test, "type") <- c("depositions")
  depositions_test_01 <- depositions_test
  i <- unlist(lapply(depositions_test_01, is.numeric))
  depositions_test_01[i] <- depositions_test_01[i] / 10
  
  depositions_n <- as_nani(depositions, "depositions")
  
  expect_equal(depositions_n, depositions_test)
  expect_equal(as_nani(depositions, "depositions", multiplier = 0.1), depositions_test_01)
  
  
  ### nani =======================================
  load(system.file("testdata", "nani_test.rda", package="nani"))
  nani_test[is.na(nani_test)] <- 0
  nani_test <- dplyr::as_tibble(nani_test)
  nani_test_01 <- nani_test
  i <- unlist(lapply(nani_test_01, is.numeric))
  nani_test_01[i] <- nani_test_01[i] / 10
  
  nani_res <- net_anthropogenic_input(fertilizers_res_nani, depositions_n, nfix_res, crop_production_res, human_food_res,
                                      human_consumption_res, human_excretion_res, animal_feed_res,
                                      animal_consumption_res, animal_production_res, gross_excretion_res)
  
  nani_res <- nani_res[, match(colnames(nani_test), colnames(nani_res))]
  
  expect_equal(nani_res, nani_test)
  expect_error(net_anthropogenic_input(fertilizers_res, depositions_n, nfix_res, crop_production_res, human_food_res,
                                       human_consumption_res, human_excretion_res, animal_feed_res, animal_production_res, gross_excretion_res), "missing input: animal_feed_consumption")
  
  expect_error(net_anthropogenic_input(fertilizers_res, depositions_n, nfix_res, crop_production_res, human_food_res,
                                       human_consumption_res, human_excretion_res, animal_feed_res, animal_production_res), "missing input: animal_feed_consumption, animal_excretion_gross")
  
  
  
  ### nani perc =======================================
  load(system.file("testdata", "nani_perc_test.rda", package="nani"))
  load(system.file("testdata", "wat_perc_test.rda", package="nani"))
  nani_perc_test[is.na(nani_perc_test)] <- 0
  nani_perc_test <- dplyr::as_tibble(nani_perc_test)
  nani_perc_test_01 <- nani_perc_test
  i <- unlist(lapply(nani_perc_test_01, is.numeric))
  nani_perc_test_01[i] <- nani_perc_test_01[i] / 10

  nani_res <- nani_res[, match(colnames(nani_test), colnames(nani_res))]
  
  expect_equal(watershed_percentage(nani_res, wat_perc_test), nani_perc_test)
  expect_equal(watershed_percentage(nani_res, wat_perc_test, multiplier = 0.1), nani_perc_test_01)


    
  ### ssb =======================================
  load(system.file("testdata", "ssb_test.rda", package="nani"))
  ssb_test[is.na(ssb_test)] <- 0
  ssb_test <- dplyr::as_tibble(ssb_test)

  depositions_ssb_n <- as_nani(depositions_ssb, "depositions")
  
  ssb_res <- soil_system_budget(fertilizers_res_nani, depositions_ssb_n, nfix_res, crop_production_res, net_excretion_res)
  
  ssb_res <- ssb_res[, match(colnames(ssb_test), colnames(ssb_res))]
  
  human_consumption_res
  
  expect_equal(ssb_res, ssb_test)
  expect_error(soil_system_budget(fertilizers_res_nani, depositions_ssb_n, nfix_res, crop_production_res),
               "missing input: animal_excretion_net")
  expect_error(soil_system_budget(human_consumption_res, fertilizers_res_nani, depositions_ssb_n, nfix_res, crop_production_res, net_excretion_res),
               "more objects than needed have been provided")
  
  
  
  ### ssb perc =======================================
  load(system.file("testdata", "ssb_perc_test.rda", package="nani"))
  ssb_perc_test[is.na(ssb_perc_test)] <- 0
  ssb_perc_test <- dplyr::as_tibble(ssb_perc_test)
  ssb_perc_test_01 <- ssb_perc_test
  i <- unlist(lapply(ssb_perc_test_01, is.numeric))
  ssb_perc_test_01[i] <- ssb_perc_test_01[i] / 10
  
  ssb_res <- ssb_res[, match(colnames(ssb_test), colnames(ssb_res))]
  
  expect_equal(watershed_percentage(ssb_res, wat_perc_test), ssb_perc_test)
  expect_equal(watershed_percentage(ssb_res, wat_perc_test, multiplier = 0.1), ssb_perc_test_01)
  
  
  
  ### yield  =======================================
  # load(system.file("testdata", "yield_test.rda", package="nani"))
  # yield_test[is.na(yield_test)] <- 0
  # attr(yield_test, "type") <- c("yield")
  # yield_test <- dplyr::as_tibble(yield_test)
  # yield_test_01 <- yield_test
  # i <- unlist(lapply(yield_test_01, is.numeric))
  # yield_test_01[i] <- yield_test_01[i] / 10
  # 
  # yield_res <- yield(cultivated_area, crop, multiplier = 0.1)
  # 
  # expect_equal(yield_res, yield_test)
 
  
  
  
  
})
