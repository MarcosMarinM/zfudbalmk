################################################################################
##                                                                            ##
##           HTML REPORT GENERATION SCRIPT - CONTINUATION                     ##
##                                                                            ##
################################################################################


#### 7. INITIAL SETUP AND ENVIRONMENT CONFIGURATION ####

### 7.1. Load Packages
# 7.1.1. Load necessary packages for HTML report generation.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, purrr, htmltools, stringr, jsonlite, readxl
)


