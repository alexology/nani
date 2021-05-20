# nani
Calculates net anthropogenic input index and soil system budget for nitrogen and phosphorus.
<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/alexology/nani/branch/main/graph/badge.svg)](https://codecov.io/gh/alexology/nani?branch=main)
<!-- badges: end -->

# Installation

```R
# install the devtools package and then
library(devtools)

install_github("alexology/nani", ref = "main")
```

# Basic usage


```R

library(nani)


# load example files

data("animal_numbers")
data("coef_table")
data("crop")
data("fertilizers_quantity")
data("cultivated_area")
data("nfix_table")
data("depositions")
data("depositions_ssb")
data("province_shape")
data("watershed_perc")


# calculate necessary information for net anthropogenic input and soil system budget

crop_production_n <- crop_production(crop, coef_table)
animal_feed_n <- animal_feed(crop_production_n, coef_table)
animal_production_n <- animal_production(animal_numbers, coef_table)
fertilizers_n <- fertilizers(fertilizers_quantity, coef_table, multiplier = 1000)
gross_excretion_n <- animal_excretion(animal_numbers, coef_table, type = "gross")
net_excretion_n <- animal_excretion(animal_numbers, coef_table, type = "net")
animal_consumption_n <- animal_feed_consumption(animal_numbers, coef_table)
human_consumption_n <- human_food_consumption(human_population, coef_table)
human_excretion_n <- human_excretion(human_population, coef_table)
human_food_n <- human_food(crop_production_n, coef_table)
nfix_n <- nitrogen_fixation(cultivated_area, crop, coef_table, nfix_table, multiplier = c(100, 0.01, 1))
depositions_n <- as_nani(depositions, "depositions")
depositions_ssb_n <- as_nani(depositions_ssb, "depositions")

# nani
nani_n <- net_anthropogenic_input(fertilizers_n, depositions_n, nfix_n, crop_production_n, human_food_n, human_consumption_n, human_excretion_n, animal_feed_n, animal_consumption_n, animal_production_n, gross_excretion_n)


# ssb
ssb_n <- soil_system_budget(fertilizers_n, depositions_ssb_n, nfix_n, crop_production_n, net_excretion_n)

# calculate nani and ssb at watershed level
nani_res_wat <- watershed_percentage(nani_n, watershed_perc)
ssb_res_wat <- watershed_percentage(ssb_n, watershed_perc)

# plot the result
plot_maps(nani_res_wat, province_shape, sp_unit = "DEN_PCM", variable = "net_input", alpha = 0.7)


```

