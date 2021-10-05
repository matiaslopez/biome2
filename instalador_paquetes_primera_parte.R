rm(list=ls())
setwd("~/git/academico/cuatri_virtual/")

list.of.packages <- c(#"ggplot2", "caret", "emmeans", "car","lme4", "sjPlot", "nlme", "lmerTest" , "ggeffects",
                      "car", "caret", "corrplot", "dplyr", "emmeans", "faraway", "GGally", 
                      "ggcorrplot", "ggeffects", "ggplot2", "gridExtra", "gt", "lm.beta", 
                      "lme4", "lmerTest", "margins", "mgcv", "multcompView", "MuMIn", "nlme", 
                      "pastecs", "plyr", "psych", "readxl", "reshape2", "rgl", "rockchalk", 
                      "sjPlot", "tableone"
                      )

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
lapply(list.of.packages, require, character.only = TRUE)


