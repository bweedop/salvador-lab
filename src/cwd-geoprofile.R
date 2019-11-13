required.packages <- c("devtools", "ggmap", "RgeoProfile")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggmap)
library(devtools)

if(!"RgeoProfile" %in% installed.packages()[, "Package"]) 
    install_github("bobverity/RgeoProfile")
library(RgeoProfile)