#' ---
#' title: enm introduction
#' author: mauricio vancine
#' date: 2020-07-16
#' ---

# prepare r -------------------------------------------------------------
# memory
rm(list = ls())

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


# 1. occurrences ----------------------------------------------------------
# directory
setwd(here::here())
dir.create("01_occurrences"); setwd("01_occurrences")
path <- getwd()
path

# import limit ----
# data
li <- rnaturalearth::countries110 %>% 
  sf::st_as_sf()
li

# download ----
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

# date filter ----
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

# bias filter ----
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

# spatial ----
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

# verify filters ----
# summary
occ_data %>% dplyr::count(species)
occ_data_date %>% dplyr::count(species)
occ_data_date_bias %>% dplyr::count(species)
occ_data_date_bias_spatial %>% dplyr::count(species)

# export ----
readr::write_csv(occ_data_date_bias_spatial, 
                 paste0("occ_clean_date_bias_spatial.csv"))

# 2. variables ------------------------------------------------------------
# directory
setwd(here::here())
dir.create("02_variables"); setwd("02_variables")
path <- getwd()
path

# limits -----
li <- rnaturalearth::ne_countries(scale = 110, continent = "South America", returnclass = "sf")
li

li_ex <- li %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc()
li_ex

# plot
tm_shape(li_ex) +
  tm_polygons(border.col = "red") +
  tm_shape(li) +
  tm_polygons(col = "gray70")

# directory
dir.create("00_limit"); setwd("00_limit")
getwd()

# export
sf::write_sf(li, "limit.shp")
sf::write_sf(li_ex, "limit_ext.shp")

# download variables ----
# directory
setwd(path); dir.create("01_raw"); setwd("01_raw")
getwd()

# download
download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip",
              destfile = "wc2.1_10m_bio.zip", mode = "wb")

# unzip
dir(pattern = ".zip") %>% 
  unzip()

# import
var <- dir(pattern = ".tif$") %>% 
  raster::stack()
var

# rename
names(var)
names(var) <- c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))
names(var)
var

# plot
plot(var$bio01)

# extent and resolution ----
# directory
setwd(path); dir.create("02_processed"); setwd("02_processed")

# adjust extent and resolution
var_li <- raster::crop(x = var, y = li) %>% 
  raster::mask(li) %>% 
  raster::aggregate(., fact = .5/res(.)[1])
var_li

# plot
tm_shape(var_li$bio01) +
  tm_raster(palette = "-Spectral") +
  tm_shape(li) +
  tm_borders(col = "black") +
  tm_layout(legend.position = c("right", "bottom"))

# correlation ----
# directory
setwd(path); dir.create("03_correlation"); setwd("03_correlation")
getwd()

# extract values
var_da <- var_li %>% 
  raster::values() %>% 
  tibble::as_tibble() %>% 
  tidyr::drop_na()
var_da

# verify
tibble::glimpse(var_da)

# correlation
cor_table <- corrr::correlate(var_da, method = "spearman") 
cor_table

# prepare table
cor_table_summary <- cor_table %>% 
  corrr::shave() %>%
  corrr::fashion()
cor_table_summary

# export
readr::write_csv(cor_table_summary, "var_table_correlacao.csv")

# select variables
# correlated variables
fi_07 <- cor_table %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)
fi_07

# select
var_da_cor07 <- var_da %>% 
  dplyr::select(-fi_07)
var_da_cor07

# verify
var_da_cor07 %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::as_matrix() %>% 
  caret::findCorrelation(cutoff = .7, names = TRUE, verbose = TRUE)

# graphic
var_ggpairs <- var_da_cor07 %>% 
  dplyr::select(sort(tidyselect::peek_vars())) %>% 
  dplyr::sample_n(1e3) %>% 
  ggpairs(lower = list(continuous = wrap(ggally_smooth_loess, pch = 21, 
                                         color = "gray30", fill = "gray50", size = 1)),
          diag = list(continuous = wrap(ggally_barDiag, color = "black", 
                                        color = "gray30", fill = "gray50", bins = 10)),
          upper = list(continuous = wrap(ggally_cor, color = "black", size = 5, 
                                         method = "spearman")),
          axisLabels = "none") +
  theme_bw()
var_ggpairs
ggsave(filename = "var_plot_correlation.png", plot = var_ggpairs, wi = 20, he = 15, un = "cm", dpi = 300)

# create variables -----
# directory
setwd(path); dir.create("04_processed_correlation"); setwd("04_processed_correlation")
getwd()

# export
raster::writeRaster(x = raster::subset(var_li, colnames(var_da_cor07)), 
                    filename = paste0("var_wc21_55km_", colnames(var_da_cor07)), 
                    bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), 
                    format = "GTiff",
                    progress = "text",
                    overwrite = TRUE)

# 3. enm ------------------------------------------------------------------
# maxent
if(file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))){
  print("File maxent.jar found!")
} else{
  print(paste0("File maxent.jar not found! Downloading in ", paste0(system.file(package = "dismo"), "/java")))
  setwd(paste0(system.file(package = "dismo"), "/java"))
  download.file("https://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download",
                "maxent.zip", mode = "wb")
  unzip("maxent.zip")}

# directory
setwd(here::here())

# directory
setwd(path); dir.create("03_enms"); setwd("03_enms")

# data ----
occ <- occ_data_date_bias_spatial
occ

var <- var_li
var

plot(var[[1]])
points(occ[, 1], occ[, 2], pch = 20)

# selecting presence and pseudo-absence data ----
pr_specie <- occ %>% 
  dplyr::select(longitude, latitude) %>% 
  dplyr::mutate(id = seq(nrow(.)))
pr_specie

pa_specie <- dismo::randomPoints(mask = var, n = nrow(pr_specie)) %>% 
  tibble::as_tibble() %>%
  dplyr::rename(longitude = x, latitude = y) %>% 
  dplyr::mutate(id = seq(nrow(.)))
pa_specie

bkg <- dismo::randomPoints(mask = var, n = 1e5, warn = FALSE)
bkg

# partitioning data	----
pr_sample_train <- pr_specie %>% 
  dplyr::sample_frac(.7) %>% 
  dplyr::select(id) %>% 
  dplyr::pull()
pr_sample_train

pa_sample_train <- pa_specie %>% 
  dplyr::sample_frac(.7) %>% 
  dplyr::select(id) %>% 
  dplyr::pull()
pa_sample_train

# train and test data ----
train_pa <- dismo::prepareData(x = var, 
                               p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                               b = pa_specie %>% dplyr::filter(id %in% pa_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
train_pa

train_pb <- dismo::prepareData(x = var, 
                               p = pr_specie %>% dplyr::filter(id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                               b = bkg) %>% na.omit
train_pb

test <- dismo::prepareData(x = var, 
                           p = pr_specie %>% dplyr::filter(!id %in% pr_sample_train) %>% dplyr::select(longitude, latitude), 
                           b = pa_specie %>% dplyr::filter(!id %in% pa_sample_train) %>% dplyr::select(longitude, latitude)) %>% na.omit
test

# model fitting ----
# presence-only - envelope
BIO <- dismo::bioclim(x = train_pa %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))

# presence-only - distance-based
DOM <- dismo::domain(x = train_pa %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb))

# presence-absence - machine learning
RFR <- randomForest::randomForest(formula = pb ~ ., data = train_pa)
SVM <- e1071::svm(formula = pb ~ ., data = train_pa)

# presence-background
Sys.setenv(NOAWT = TRUE)
MAX <- dismo::maxent(x = train_pb %>% dplyr::select(-pb), p = train_pb %>% dplyr::select(pb))

# prediction ----
model_predict_bio <- raster::predict(var, BIO, progress = "text")
model_predict_bio

model_predict_dom <- raster::predict(var, DOM, progress = "text")
model_predict_dom

model_predict_rfr <- raster::predict(var, RFR, progress = "text")
model_predict_rfr

model_predict_svm <- raster::predict(var, SVM, progress = "text")
model_predict_svm

model_predict_max <- raster::predict(var, MAX, progress = "text")
model_predict_max

# 4. evaluation -----------------------------------------------------------
eval <- dismo::evaluate(p = test %>% dplyr::filter(pb == 1) %>% dplyr::select(-pb), 
                        a = test %>% dplyr::filter(pb == 0) %>% dplyr::select(-pb), 
                        model = fit[[a]])

# indices
id_eval_spec_sens <- which(eval@t == dismo::threshold(eval, "spec_sens"))
tss_spec_sens <- eval@TPR[id_eval_spec_sens] + eval@TNR[id_eval_spec_sens] - 1

# 5. maps -----------------------------------------------------------------
# data
da <- raster::rasterToPoints(ens) %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(sui = names(ens))

map_sui <- ggplot() +
  geom_raster(data = da, aes(x, y, fill = sui)) +
  geom_sf(data = li, fill = NA, color = "gray30") +
  scale_color_manual(values = "black", guide = guide_legend(order = 1)) +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", n = 100, type = "continuous"),
                       limits = c(0, 1)) +
  coord_sf(xlim = sf::st_bbox(li)[c(1, 3)], ylim = sf::st_bbox(li)[c(2, 4)]) +
  labs(x = "Longitude", y = "Latitude", fill = "Suitability") +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(.3, "cm"), pad_y = unit(.3, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "white",
                                         size = 0.3, 
                                         linetype = "solid", 
                                         colour = "black"),
        axis.title = element_text(size = 12, face = "plain"),
        legend.position = c(.75, .17))
map_sui

# end ---------------------------------------------------------------------