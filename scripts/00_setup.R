# Installing packages ---------------------------------------------------------------------------------

want <- c("here","readr","tidyverse", "rio", "dplyr")  # list of required packages
have <- want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
rm(have, want)

library("rio")
library("tidyverse")
library("readr")
library("here")
library("dplyr")