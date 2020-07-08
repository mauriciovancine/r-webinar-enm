# Webinar: Introdução à Modelagem de Nicho Ecológico

## Horários
Data: 16-07-2020
Horário: 10h - 12h

## Apresentação
Google Presentation: https://docs.google.com/presentation/d/12yra_kP_0U_QvTWWJgdT-u9weewWBDpH0FrDqYvOUS0/edit?usp=sharing
PDF: 

## Script
Script no R: 

## Pacotes à serem instalados
```{r}
# packages
if(!require(here)) install.packages("here")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(spocc)) install.packages("spocc")
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(rvest)) install.packages("rvest")
if(!require(ggsn)) install.packages("ggsn")
if(!require(raster)) install.packages("raster")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(sf)) install.packages("sf")
if(!require(tmap)) install.packages("tmap")
if(!require(wesanderson)) devtools::install_github("karthik/wesanderson")
if(!require(corrr)) install.packages("corrr")
if(!require(ecospat)) install.packages("ecospat")
if(!require(dismo)) install.packages("dismo")
if(!require(kernlab)) install.packages("e1071")
if(!require(randomForest)) install.packages("randomForest")
if(!require(rJava)) install.packages("rJava") # install java - https://www.java.com/pt_BR/download/
```