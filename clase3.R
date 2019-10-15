#Práctica 3 Guiada

rm(list=ls())
setwd("E:/UBA/2019-II/EEA/R Code")

#install.packages('ggridges', dependencies=TRUE, repos='http://cran.rstudio.com/')
library("fs")
library("tidyverse")
library("openxlsx")
library("glue")

bases_individuales_path <- dir_ls(path = '../CodigoProf/Fuentes/', regexp= 'individual')
bases_individuales_path

