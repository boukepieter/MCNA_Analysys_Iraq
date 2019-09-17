##### actual PIN calculation
library(rgdal)
library(raster)
library(dplyr)
WGS84 <- crs("+init=epsg:4326")
source("C:/Users/REACH-IRQ-GIS/Documents/2019 MCNA/201904 MCNA sampling script/MCNA_sampling/functions.R")
stratification <- readOGR("input/irq_admbnda_adm3_cso_20190603.shp","irq_admbnda_adm3_cso_20190603")

### IDP
file <- "IDPs_MasterList_dataset_DTM_IOMAug 31, 2019.xlsx"
sheet <- "Sheet"
ind <- 10
camps <- 29
lat <- 7
lon <- 8

poplist <- read.population.list(paste("input/",file,sep=""), sheet=sheet, lat=lat, lon=lon, families=ind, camps=camps)
poplist[c(422,2135,2481),c(lat,lon)] <- data.frame(Latitude=c(37.06,36.805,30.018), Longitude=c(42.38,42.084,47.939))
pop_spdf <- SpatialPointsDataFrame(poplist[,c(lon,lat)], poplist, proj4string=WGS84)

pop_spdf <- spatial.join.locs(pop_spdf, stratification, targetcolumn=c("ADM3_EN","ADM2_EN","ADM1_EN"), 
                              column.names=c("COD_Subdistrict","COD_District","COD_Governorate"))
sumlist <- pop_spdf@data[,c(ind,ncol(pop_spdf)-1)] 
summarised <- sumlist %>%
  group_by(COD_District) %>%
  summarise_if(is.numeric, sum, na.rm=T)
write.csv(as.data.frame(summarised), "output/PIN/idp_out_camp_population.csv")

### Returnee
file <- "Returnee_MasterList_dataset_DTM_IOMAug 31, 2019.xlsx"
sheet <- "Sheet"
ind <- 10
camps <- 29
lat <- 7
lon <- 8

poplist <- read.population.list(paste("input/",file,sep=""), sheet=sheet, lat=lat, lon=lon, families=ind, camps=camps)
# tweak Returnee input for wrong subdistrict shape
poplist[c(1,1307,54),c(lat,lon)] <- data.frame(Latitude=c(37.06,36.805,34.43), Longitude=c(42.38,42.084,41.1))

# 2. Spatial join with governorates, districts and subdistricts
pop_spdf <- SpatialPointsDataFrame(poplist[,c(lon,lat)], poplist, proj4string=WGS84)
pop_spdf <- spatial.join.locs(pop_spdf, stratification, targetcolumn=c("ADM3_EN","ADM2_EN","ADM1_EN"), 
                              column.names=c("COD_Subdistrict","COD_District","COD_Governorate"))
sumlist <- pop_spdf@data[,c(ind,ncol(pop_spdf)-1)] 
summarised <- sumlist %>%
  group_by(COD_District) %>%
  summarise_if(is.numeric, sum, na.rm=T)
write.csv(as.data.frame(summarised), "output/PIN/returnee_population.csv", row.names=F)

### in camp
file <- "IDPs_in_camp_CCCM_masterlist_augustus.xlsx"
sheet <- "formal"

poplist <- read.xlsx(sprintf("input/%s", file), sheetName = sheet)
sumlist <- poplist[,c("COD_District", "individuals")] 
summarised <- sumlist %>%
  group_by(COD_District) %>%
  summarise_if(is.numeric, sum, na.rm=T)
write.csv(as.data.frame(summarised), "output/PIN/idp_in_camp_population.csv", row.names=F)

### PIN's
name <- "severity_pop_groups_aggregated"
analysisplan <- read.csv(sprintf("input/dap_%s.csv",name), stringsAsFactors = F)
name <- "severity_2019908"
summary <- read.csv(sprintf("output/raw_results_%s_filtered.csv", name), stringsAsFactors = F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]

j=3
group_pin <- read.csv(sprintf("output/pin_%s_%s.csv", name, groups[j]), stringsAsFactors = F)
selection <- read.csv(sprintf("output/PIN/%s_population.csv", groups[j]))
consequences <- unique(analysisplan$consequence)
consequences <- consequences[-which(consequences %in% c("", "resilience"))]
for (i in 1:length(consequences)) {
  consequence <- analysisplan %>% filter(consequence == consequences[i]) %>% separate(dependent.variable, c("ind","sub")) %>% 
    dplyr::select(ind) %>% unique
  consequence <- group_pin[-c(1:3),c("district",consequence$ind)]
  consequence$max <- apply(consequence[,-1], 1, max, na.rm=T)
  consequence$pop <- selection[match(consequence$district, selection$COD_District),2]
  consequence$pin <- round(as.numeric(consequence$max) * consequence$pop)
  write.csv(consequence,sprintf("output/PIN/%s_%s.csv", groups[j], consequences[i]))
  if(i == 1){
    write.xlsx(consequence, file=sprintf("output/PIN/%s.xlsx", groups[j]), sheetName=consequences[i], row.names=FALSE)
  } else {
    write.xlsx(consequence, file=sprintf("output/PIN/%s.xlsx", groups[j]), sheetName=consequences[i], append=TRUE, row.names=FALSE)
  }
}


##### Movement intentions maps
intentions_aod <- read.xlsx("input/Intentions_DistrictDisplacement.xlsx", sheetIndex = 1, startRow = 3)
intentions_aoo <- read.xlsx("input/Intentions_DistrictOrigin.xlsx", sheetIndex = 1, startRow = 2)
filter_aoo <- names(which(response$district_origin %>% table >= 20))
filter_aoo2 <- names(which(response$district_origin %>% table < 20 & response$district_origin %>% table > 0))
intentions_aoo <- intentions_aoo[which(intentions_aoo$district_origin %in% filter_aoo),]
districts <- readOGR("input/irq_admbnda_adm2_cso_20190603.shp", "irq_admbnda_adm2_cso_20190603")
lookup <- read.csv("input/lookup_table_names.csv", stringsAsFactors = F)[-(1:18),]
intentions_aoo$district[! intentions_aoo$district %in% lookup$name]
districts$ADM2_EN[! districts$ADM2_EN %in% lookup$english]
districts$remain_by_aod <- intentions_aod$remain[match(lookup$name[match(districts$ADM2_EN, lookup$english)], intentions_aod$district)]
districts$return_by_aoo <- intentions_aoo$return[match(lookup$name[match(districts$ADM2_EN, lookup$english)], intentions_aoo$district)]
districts_unsufficient <- districts[which(districts$ADM2_EN %in% lookup$english[match(filter_aoo2, lookup$name)]),]
writeOGR(districts, "output/district_intentions", "district_intentions", driver = "ESRI Shapefile", overwrite_layer = T)
writeOGR(districts_unsufficient, "output/district_intentions", "districts_unsufficient", driver = "ESRI Shapefile", overwrite_layer = T)
