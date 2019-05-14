library(geozoo)
library(tidyverse)
library(devtools)

d1 <- as_tibble(cube.solid.random(5, 1000)$points) %>%
  add_column(shape="cube")

d2 <- as_tibble(sphere.solid.random(5, 1000)$points) %>%
  add_column(shape="sphere")

colnames(d1) <- colnames(d2)

geozooData <- rbind(d1, d2)

use_data(geozooData)
