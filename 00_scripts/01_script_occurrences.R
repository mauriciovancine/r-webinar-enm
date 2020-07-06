#' ---
#' title: occ - download and clean
#' author: mauricio vancine
#' date: 2020-07-16
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(CoordinateCleaner)
library(ecospat)
library(lubridate)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(tmap)

# directory
setwd(here::here())
dir.create("01_occurrences"); setwd("01_occurrences")
path <- getwd()
path

# import data -------------------------------------------------------------
# data
li <- rnaturalearth::countries110 %>% 
  sf::st_as_sf()
li

# download ----------------------------------------------------------------
# species
sp <- "Chrysocyon brachyurus"

# directory
setwd(path); dir.create("01_raw"); setwd("01_raw")

# occ
occ_spocc <- spocc::occ(query = sp, 
                          from = "gbif",
                          has_coords = TRUE, 
                          limit = 1e6)
occ_spocc

# get data
occ_data <- spocc::occ2df(occ_spocc) %>% 
      dplyr::mutate(species = sp,
                    longitude = as.numeric(longitude),
                    latitude = as.numeric(latitude),
                    year = date %>% lubridate::year(),
                    base = prov %>% stringr::str_to_lower()) %>% 
      dplyr::select(name, species, longitude, latitude, year, base)
occ_data

# map
occ_data_vector <- occ_data %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tm_shape(li, bbox = occ_data_vector) +
  tm_polygons() +
  tm_shape(occ_data_vector) +
  tm_dots(size = .2, shape = 21, col = "species",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# date filter -------------------------------------------------------------
# directory
setwd(path); dir.create("02_clean"); setwd("02_clean")

# verify
occ_data$year %>% table(useNA = "always")

# year > 1970 and < 2020
occ_data_date <- occ_data %>% 
  dplyr::filter(is.na(year) == FALSE,
                (year > 1970 & year <= 2020)) %>% 
  dplyr::arrange(year)
occ_data_date

# verify
occ_data$year %>% table(useNA = "always")
occ_data_date$year %>% table(useNA = "always")

# histogram
occ_data_date %>% 
  ggplot() + 
  aes(x = year, fill = species) +
  geom_histogram(color = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "Frequency (log10)", fill = "Species") +
  theme_bw() +
  theme(legend.text = element_text(face = "italic"),
        legend.position = "none",
        strip.text = element_text(size = 10, face = "italic"))
ggsave(filename = "occ_plot_date.png", wi = 20, he = 15, un = "cm", dpi = 300)

# map
occ_data_date_vector <- occ_data_date %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tm_shape(li, bbox = occ_data_date_vector) +
  tm_polygons() +
  tm_shape(occ_data_date_vector) +
  tm_dots(size = .2, shape = 21, col = "species",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# bias filter -------------------------------------------------------------
# remove na
occ_data_na <- occ_data_date %>% 
  tidyr::drop_na(longitude, latitude)
occ_data_na

# flag data
flags_bias <- CoordinateCleaner::clean_coordinates(
  x = occ_data_na, 
  species = "species",
  lon = "longitude", 
  lat = "latitude",
  outliers_mtp = 2,
  tests = c("capitals", # radius around capitals
            "centroids", # radius around country and province centroids
            "duplicates", # records from one species with identical coordinates
            "equal", # equal coordinates
            "gbif", # radius around GBIF headquarters
            "institutions", # radius around biodiversity institutions
            "seas", # in the sea
            "urban", # within urban area
            "validity", # outside reference coordinate system
            "zeros" # plain zeros and lat = lon 
  )
)

# results
#' TRUE = clean coordinate entry 
#' FALSE = potentially problematic coordinate entries
flags_bias %>% head
flags_bias %>% summary

# exclude records flagged by any test
occ_data_date_bias <- occ_data_na %>% 
  dplyr::filter(flags_bias$.summary == TRUE)
occ_data_date_bias

# resume data
occ_data_na %>% dplyr::count(species)
occ_data_date_bias %>% dplyr::count(species)

# map
occ_data_date_bias_vector <- occ_data_date_bias %>% 
  tidyr::drop_na(longitude, latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tm_shape(li, bbox = occ_data_date_bias_vector) +
  tm_polygons() +
  tm_shape(occ_data_date_bias_vector) +
  tm_dots(size = .2, shape = 21, col = "species",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# spatial -----------------------------------------------------------------
# prepare
occ_data_date_bias <- occ_data_date_bias %>%
  dplyr::mutate(x = longitude, y = latitude) %>% 
  as.data.frame
occ_data_date_bias

# desaggregation
occ_data_date_bias_spatial <- ecospat::ecospat.occ.desaggregation(
  xy = occ_data_date_bias,
  min.dist = .5, 
  by = "species") %>%
  tibble::as_tibble() %>% 
  dplyr::select(-x, -y)
occ_data_date_bias_spatial

# verify
occ_data_date_bias %>% dplyr::count(species)
occ_data_date_bias_spatial %>% dplyr::count(species)

# map
occ_data_date_bias_spatial_vector <- occ_data_date_bias_spatial %>%
  dplyr::mutate(x = longitude, lon = longitude, y = latitude, lat = latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_date_bias_spatial_vector

tm_shape(li, bbox = li_sa) +
  tm_polygons() +
  tm_shape(occ_data_date_bias_spatial_vector) +
  tm_dots(size = .2, shape = 21, col = "species",  
          palette = "Set1", title = "Species") +
  tm_graticules(lines = FALSE) +
  tm_layout(legend.text.fontface = "italic")

# verify filters ----------------------------------------------------------
# summary
occ_data %>% dplyr::count(species)
occ_data_date %>% dplyr::count(species)
occ_data_date_bias %>% dplyr::count(species)
occ_data_date_bias_spatial %>% dplyr::count(species)

# export ------------------------------------------------------------------
readr::write_csv(occ_data_date_bias_spatial, 
                 paste0("occ_clean_date_bias_spatial.csv"))

# end ---------------------------------------------------------------------