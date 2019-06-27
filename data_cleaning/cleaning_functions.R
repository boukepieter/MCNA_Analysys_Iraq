kobo.xlsx.to.csv <- function(dir, sheetName, anonymise=F, anonymise_cols = NULL) {
  files <- list.files(dir, pattern="*.xlsx")
  parent <- read.xlsx(paste(dir,files[1],sep="/"),
                      sheetName = sheetName)
  child <- read.xlsx(paste(dir,files[1],sep="/"),
                     sheetName = "member")
  
  
  if (anonymise){
    if (is.null(anonymise_cols)) {
      anonymise_cols <- c(grep("*contact*",names(parent)), 
                          grep("*gpslocation*",names(parent)))
    }
    parent_ano <- parent[,-anonymise_cols]
    write.csv(parent_ano,paste0(dir,"/parent_anonymised.csv"))
  } else {
    write.csv(parent,paste0(dir,"/parent.csv"), row.names = F)
  }
  write.csv(child,paste0(dir,"/child.csv"), row.names = F)
}

log.cleaning.change <- function(uuid, old.value, question.name, new.value, issue=NULL,
                                dir) {
  if (file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    log <- read.csv(log_file, stringsAsFactors = F)
  } else {
    log <- data.frame(uuid = character(), question.name = character(), issue = character(), feedback = character(), 
                      changed = logical(), old.value = character(), new.value = character(), stringsAsFactors = F)
  }
  log[nrow(log)+1,"uuid"] <- uuid
  log$question.name[nrow(log)] <- question.name
  log$old.value[nrow(log)] <- old.value
  log$new.value[nrow(log)] <- new.value
  log$issue[nrow(log)] <- issue
  write.csv(log, sprintf("%s/cleaning_logbook.csv",dir), row.names = F)
  return(log)
}

execute.cleaning.changes <- function(dir, uuid_column=NULL) {
  if (!file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    stop("no cleaning file found")
  }
  log <- read.csv(sprintf("%s/cleaning_logbook.csv",dir), stringsAsFactors = F)
  data <- read.csv(sprintf("%s/parent.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
  match <- grep(pattern = "uuid", x = names(data))
  if (is.null(uuid_column)){
    if (length(match) < 1) {
      stop("cannot find uuid column")
    } else if (length(match) > 1) {
      stop("multiple uuid columns found")
    } else {
      uuid_column <- names(data)[match]
    }
  }
  
  for(i in 1:nrow(log)){
    data[which(data[,uuid_column] == log$uuid[i]), log$question.name] <- log$new.value
    log$changed <- TRUE
  }
  write.csv(log, sprintf("%s/cleaning_logbook.csv", dir), row.names = F)
  write.csv(data, sprintf("%s/parent_cleaned.csv", dir), row.names = F)
}

points.inside.cluster <- function(data, samplepoints_file, sampleareas_file) {
  load(samplepoints_file)
  load(sampleareas_file)
  samplepoints[["returnee"]] <- samplepoints[["r"]]
  sample_areas[["returnee"]] <- sample_areas[["r"]]
  
  pop_groups <- data %>% dplyr::select(population_group) %>% table %>% names
  for (i in 1:length(pop_groups)){
    data_pop <- data %>% filter(population_group == pop_groups[i])
    
    clusters <- data_pop %>% dplyr::select(cluster_location_id) %>% table %>% names
    for (j in 1:length(clusters)){
      data_on_cluster <- data_pop %>% 
        filter(cluster_location_id == clusters[j])
      interview_locations <- data_on_cluster %>% 
        dplyr::select(c(X_gpslocation_longitude,X_gpslocation_latitude))
      interview_locations_geo <- SpatialPointsDataFrame(interview_locations,interview_locations, proj4string = WGS84)
      interview_locations_geo$cluster <- clusters[j]
      
      cluster <- strsplit(clusters[j],"_")[[1]][4]
      cluster_points <- samplepoints[[pop_groups[i]]] %>% 
        as("sf") %>% filter(startsWith(label, cluster)) %>% as("Spatial")
      cluster_area <- sample_areas[[pop_groups[i]]] %>% 
        as("sf") %>% filter(startsWith(label, cluster)) %>% as("Spatial")
      
      cat(sprintf("Interviews for %s - %s are inside the cluster area: %s\n", clusters[j], pop_groups[i],
              gContains(cluster_area,interview_locations_geo)))
    }
  }
}
