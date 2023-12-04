library(tidyverse)
library(tidymodels)
library(tidytext)
library(keras)
library(tensorflow)
library(yardstick)
library(dplyr)
setwd("/vignette-group10-RNN/data")
load("data_clean.RData")

set.seed(102722)
partitions <- data_clean %>%
  initial_split(prop = 0.8)

