#download from https://dcc.ligo.org/public/0152/P1800115/005/Parametrized-EoS_maxmass_posterior_samples.dat

library(tidyverse)

GW170817 <- read_delim("data-raw/Parametrized-EoS_maxmass_posterior_samples.dat", " ", skip = 1, col_names = F)
colnames(GW170817) <- c("m1", "m2", "L1", "L2", "R1", "R2")

use_data(GW170817)
