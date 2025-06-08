# -------------------------------------------------------------
# set working directory to execute the following code files ####
if (!require(here)) install.packages("here"); library(here)
setwd(here())
# -------------------------------------------------------------

#---------------
# section 1 ####
#---------------

#--------------------------------
# load packages and functions ####
source("packages_and_functions/packages.R")
source("packages_and_functions/functions.R")
#--------------------------------

#---------------
# section 2 ####
#---------------

#----------------------
# feature selection ####
source("test_and_prepare_data/feature_selection.R")
#----------------------

#------------------------------
# compare balancing methods ####
source("test_and_prepare_data/compare_balancing_methods.R")
#------------------------------

#---------------
# fetch data ####
source("test_and_prepare_data/fetch_data.R")
#---------------

#---------------
# section 3 ####
#---------------

#--------------------------------------
# apply machine learning algorithms ####
source("machine_learning_methods/random_forest.R")
source("machine_learning_methods/bagging.R")
source("machine_learning_methods/boosting.R")
source("machine_learning_methods/boosting_xgbDART.R")
source("machine_learning_methods/artificial_neuronal_network.R")
source("machine_learning_methods/k_nearest_neighbour.R")
#--------------------------------------

#---------------
# section 4 ####
#---------------

#----------
# plots ####
source("plots/plots.R")
#----------