#required_packages <- c("svDialogs","lubridate","matrixStats","transport","dplyr","tidyverse")
required_packages <- c("svDialogs","lubridate","matrixStats","transport","dplyr","data.table","magicaxis")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

lapply(required_packages, library, character.only = TRUE)

library(svDialogs) # for the popup windows
library(lubridate) # used to convert dates to decimal numbers
library(matrixStats) # for the colMaxs function used in the post-processing step
library(transport) # for the SAS calculations
library(dplyr) # for the SAS calculations
library(data.table) # for the parameter estimation with two tracers
library(magicaxis) # for the automatic scaling of the graphics
#library(tidyverse)

rm(package,required_packages)
