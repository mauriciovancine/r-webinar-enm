# Webinar: Introdução à Modelagem de Nicho Ecológico

## Horários
Data: 16-07-2020
<br>
Horário: 10h - 12h

## Apresentação
Google Slides: https://docs.google.com/presentation/d/12yra_kP_0U_QvTWWJgdT-u9weewWBDpH0FrDqYvOUS0
<br>
PDF: https://github.com/mauriciovancine/r-webinar-enm/blob/master/00_apresentacao/apres_webinar_enm_esalq.pdf

## Script
Script no R: https://github.com/mauriciovancine/r-webinar-enm/blob/master/01_script/script_apres_webinar_enm_esalq.R

## Pacotes à serem instalados - para quem quiser seguir o script
```r
# packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(spocc)) install.packages("spocc")
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner")
if(!require(rnaturalearth)) install.packages("rnaturalearth")
if(!require(rvest)) install.packages("rvest")
if(!require(ggspatial)) install.packages("ggspatial")
if(!require(raster)) install.packages("raster")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(GGally)) install.packages("GGally")
if(!require(sf)) install.packages("sf")
if(!require(tmap)) install.packages("tmap")
if(!require(wesanderson)) devtools::install_github("karthik/wesanderson")
if(!require(corrr)) install.packages("corrr")
if(!require(ecospat)) install.packages("ecospat")
if(!require(dismo)) install.packages("dismo")
if(!require(kernlab)) install.packages("e1071")
if(!require(randomForest)) install.packages("randomForest")

# install java - windows - https://www.java.com/pt_BR/download/
# install java - ubuntu - sudo apt install -y default-jre default-jdk && sudo R CMD javareconf ## rjava
if(!require(rJava)) install.packages("rJava") 
```