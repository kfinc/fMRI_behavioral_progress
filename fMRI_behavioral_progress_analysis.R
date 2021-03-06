###############################################################################
#
# Script to vizualize and calculate robust repeated analysis of variance from behavioral 
# dual n-back data from fMRI scanning sessions (groups x sessions)
#
###############################################################################

########################
# LOADING PACKAGES
########################

library(tidyr)
library(ggplot2)
require(plyr)
require(ez)
require(reshape)
require(reshape2)
require(WRS2)
require(rcompanion)
require(psych)
require(data.table) 

########################
# LOADING FILES
########################

# setting working directory
setwd("~/Dropbox/GitHub/fMRI_behavioral_progress")

# loading necessary functions 
pathnames <- list.files(pattern="[.]R$", path="functions/", full.names = TRUE);
sapply(pathnames, FUN = source);

# loading data
beh <- read.csv("data/dual_nback_beh.csv")



########################
# PREPARING DATA
########################

beh$group <- factor(beh$control, labels = c("Experimental", "Control"))
beh <- beh[order(beh$group), ]

# adding case and row variables necessary for repeated analysis of variance
beh$row <- c(1:19, 1:20)
beh$case <- c(1:39)

########################
# RUNNING FUNCTIONS
########################

# defined variables for looping
nback = c("1-back", "2-back", "delta")
measures = c("pRT", "acc", "dp")
modalities = c("ns", "na", "both")

# main loop
for(i in 1:length(modalities)){
   for(j in 1:length(measures)){
     for(k in 1:length(nback)){
      print("#################################################################")
      print(paste(modalities[i], measures[j], nback[k]))
      print("#################################################################")
      
      # cleaning & reshaping table from wide to long format 
      table <- beh_prog_full(beh, modalities[i], measures[j], nback[k])
      print(table)
      
      # potting results (group x session) & saving plots to a folder 
      plot <- plot_maker(table)
      png(filename = paste0("figures/progress_", modalities[i], "_", measures[j], "_",  nback[k], ".png"))
      print(plot)
      dev.off()
      
      # calculating analysis of variance
      stat <- stats_robust(table)
      print(stat)

    }
  }
}


          