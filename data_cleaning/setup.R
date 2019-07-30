options(java.parameters = "- Xmx1024m")
library(cleaninginspectoR)
library(plotKML)
library(rgeos)
library(xlsx)
library(rgeos)
library(rgdal)
library(ggmap)
library(raster)
library(sf)
library(googleLanguageR)
library(leaflet)
library(htmlwidgets)
Sys.setlocale("LC_ALL","Arabic")
WGS84 <- crs("+init=epsg:4326")
UTM38N <- crs("+init=epsg:32638")

samplepoints_file = "sample_points/samplepoints.RData"
sampleareas_file = "sample_points/sample_areas.RData"
load(samplepoints_file)
load(sampleareas_file)
samplepoints[["returnee"]] <- samplepoints[["r"]]
sample_areas[["returnee"]] <- sample_areas[["r"]]

gl_auth("test/My First Project-9df13c475b8e.json")

psu <- read.csv("raw_data/combined_sample_ids.csv", stringsAsFactors = F)
partners <- read.csv("test/partners.csv", header = F, stringsAsFactors = F)
