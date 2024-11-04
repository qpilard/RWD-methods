#Install pacman library
if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")

#Load pacman
# Load pacman
library(pacman)

# Install and load the packages
pacman::p_load(
  cobalt,
  WeightIt,
  gtsummary,
  smd,
  survey,
  ggplot2,
  dplyr,
  ggpubr,
  survminer,
  survival,
  dagitty,
  ggdag
)