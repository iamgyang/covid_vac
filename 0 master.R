# To do:

# do we want to get the IRON levels from NMC as opposed to CHAT? 
# https://correlatesofwar.org/data-sets/national-material-capabilities

# change to historical income groups instead of current income groups

rm(list = ls()) # clear the workspace


# Options: ----------------------------------------------------------------

# debugging
options(error=browser)
options(error=NULL)

# disable data.table auto-indexing (causes errors w/ dplyr functions)
options(datatable.auto.index = FALSE)


# Notes -------------------------------------------------------------------



# Directories -------------------------------------------------------------

# clear environment objects
rm(list = ls())

# You will have to edit this to be your own computer's working directories:
user<-Sys.info()["user"]
root_dir <- paste0("C:/Users/", user, "/Dropbox/CGD/Projects/refute_mestieri/")
input_dir <- paste0(root_dir, "input")
output_dir <- paste0(root_dir, "output")
code_dir <- paste0(root_dir, "code")

setwd(input_dir)


# Packages ---------------------------------------------------------------
{
list.of.packages <- c(
    "base", "car", "cowplot", "dplyr", "ggplot2", "ggthemes", "graphics", "grDevices",
    "grid", "gridExtra", "gvlma", "h2o", "lubridate", "MASS", "readxl", "rio", "rms",
    "rsample", "stats", "tidyr", "utils", "zoo", "xtable", "stargazer", "data.table",
    "ggrepel", "foreign", "fst", "countrycode", "wbstats", "quantmod", "R.utils",
    "leaps", "bestglm", "dummies", "caret", "jtools", "huxtable", "haven", "ResourceSelection",
    "betareg", "quantreg", "margins", "plm", "collapse", "kableExtra", "tinytex",
    "LambertW", "scales", "stringr", "imputeTS", "shadowtext", "pdftools", "glue",
    "purrr", "OECD", "RobustLinearReg", "forcats", "WDI", "xlsx")
}

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for (package in list.of.packages) {library(eval((package)), character.only = TRUE)}

# set GGPLOT default theme:
theme_set(theme_clean() + 
              theme(plot.background = 
                        element_rect(color = "white")))


# source(paste0(root_dir, "code/", "helper_functions.R"))
source(paste0("C:/Users/", user, "/Dropbox/Coding_General/personal.functions.R"))

# ... ---------------------------------------------------------------------
# 
# # Cleaning ----------------------------------------------------------------
# 
# source(paste0(code_dir, "/1 cleaning.R"))
# 
# # Splicing ----------------------------------------------------------------
# 
# load("merged2.RData")
# source(paste0(code_dir, "/2 splicing.R"))
# source(paste0(code_dir, "/2 table of variable origins.R"))
# 
# # Analysis ----------------------------------------------------------------
# 
# source(paste0(code_dir, "/3 analysis.R"))
# 
# 
# # Graphs ------------------------------------------------------------------
# 
# source(paste0(code_dir, "/4 graphs.R"))
# 
# 
# # Notes -------------------------------------------------------------------
# 
# # Comin and Mesitieri consider Austria, Belgium, Denmark, Finland, France, Germany, Italy, Netherlands, Norway, Sweden, Switzerland, Untied Kingdom, Japan, Australia, New Zealand, Canada, and the United States as "Western" countries.
# # 
# # For a view on the overlapping list of data variables, their sources, start dates, see Table _____ below.
# # A full list of data variables, their sources, start dates, can be seen in Table _____ below.
# # 
