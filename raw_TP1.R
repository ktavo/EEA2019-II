rm(list=ls())

setwd("E:/UBA/2019-II/EEA/R Code")


t0       <-  Sys.time()
ar_properties <- read.table("TP1/ar_properties.csv",
                            sep=",",
                            dec=".",
                            header = TRUE,
                            fill = TRUE)
t1       <-  Sys.time()
tcorridaCSV <-  as.numeric( t1 - t0, units = "secs")
glimpse(ar_properties)

