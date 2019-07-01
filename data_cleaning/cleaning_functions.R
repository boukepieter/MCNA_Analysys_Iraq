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
    write.csv(parent_ano,paste0(dir,"/parent_anonymised.csv"), row.names = F)
  } else {
    write.csv(parent,paste0(dir,"/parent.csv"), row.names = F)
  }
  write.csv(child,paste0(dir,"/child.csv"), row.names = F)
}

anonymise.cleaned.data <- function(dir, anonymise_cols = NULL) {
  parent <- read.csv(sprintf("%s/parent_cleaned.csv", dir), stringsAsFactors = F)
  if (is.null(anonymise_cols)) {
    anonymise_cols <- c(grep("*contact*",names(parent)), 
                        grep("*gpslocation*",names(parent)))
  }
  parent_ano <- parent[,-anonymise_cols]
  write.csv(parent_ano,paste0(dir,"/parent_cleaned_anonymised.csv"), row.names = F)
} 

log.cleaning.change <- function(uuid, action, old.value=NULL, question.name=NULL, new.value=NULL, issue=NULL,
                                dir) {
  action <- ifelse(action == "c", "change", ifelse(action == "d", "deletion", ifelse(action == "f", "flag", action)))
  if (!action %in% c("change", "deletion", "flag")) {
    stop("action given is not a valid action")
  }
  if (action == "change" & (is.null(old.value) | is.null(question.name) | is.null(new.value))) {
    stop("For a change all the parameters of old.value, question.name and new.value should be given.")
  }
  if (action == "flag" & is.null(question.name)) {
    stop("For a flag the parameter question.name should be given.")
  }
  log_file <- sprintf("%s/cleaning_logbook.csv",dir)
  if (file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    log <- read.csv(log_file, stringsAsFactors = F)
  } else {
    log <- data.frame(uuid = character(), question.name = character(), issue = character(), feedback = character(), 
                      action = character(), changed = logical(), old.value = character(), new.value = character(), 
                      stringsAsFactors = F)
  }
  for (i in 1:length(uuid)){
    log[nrow(log)+1,"uuid"] <- uuid[i]
    log$action[nrow(log)] <- action
    log$issue[nrow(log)] <- issue
    if (action != "deletion") {
      log$question.name[nrow(log)] <- question.name
    }
    if (action == "change") {
      log$old.value[nrow(log)] <- old.value
      log$new.value[nrow(log)] <- new.value
    }
  }
  write.csv(log, log_file, row.names = F)
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
    if (log$action[i] == "change") {
      data[which(data[,uuid_column] == log$uuid[i]), log$question.name[i]] <- log$new.value[i]
      log$changed[i] <- TRUE
    } else if (log$action[i] == "deletion") {
      data <- data[-which(data[,uuid_column] == log$uuid[i]), ]
    }
  }
  write.csv(log, sprintf("%s/cleaning_logbook.csv", dir), row.names = F)
  write.csv(data, sprintf("%s/parent_cleaned.csv", dir), row.names = F)
}

points.inside.cluster <- function(data, samplepoints, sample_areas, dir, write_to_file = FALSE, buffer = 1000) {
  
  pop_groups <- data %>% dplyr::select(population_group) %>% table %>% names
  result <- data.frame(cluster = character(), surveys = numeric(), pop_group = character(), inside_cluster = logical(), 
                       inside_buffer = logical(), inside_alternative_cluster = logical(), alternative_cluster = character(), 
                       inside_alternative_group = logical(), alternative_group = character(), message = character(),
                       stringsAsFactors = F)
  
  counter <- 0
  for (i in 1:length(pop_groups)){ # Start loop over population groups
    data_pop <- data %>% filter(population_group == pop_groups[i]) 
    
    clusters <- data_pop %>% dplyr::select(cluster_location_id) %>% table %>% names
    for (j in 1:length(clusters)){ # Start loop over clusters
      counter <- counter + 1
      result[nrow(result) + 1, 1] <- clusters[j]
      result[nrow(result), 3] <- pop_groups[i]
      
      data_on_cluster <- data_pop %>% filter(cluster_location_id == clusters[j])
      interview_locations <- data_on_cluster %>% dplyr::select(c(X_gpslocation_longitude,X_gpslocation_latitude))
      interview_locations_geo <- SpatialPointsDataFrame(interview_locations,interview_locations, proj4string = WGS84)
      result[nrow(result), 2] <- nrow(data_on_cluster)
      
      cluster <- strsplit(clusters[j],"_")[[1]][4]
      cluster_area_sf <- sample_areas[[pop_groups[i]]] %>% 
        as("sf") %>% filter(startsWith(label, cluster))
      
      if (nrow(cluster_area_sf) > 0) { # Check if cluster / population group exists
        cluster_area <- cluster_area_sf %>% as("Spatial")
        inside <- gContains(cluster_area,interview_locations_geo)
        
        message <- sprintf("Interviews for %s - %s are inside the cluster area: %s\n", clusters[j], pop_groups[i],
                           inside)
        cat(message)
        result$message[nrow(result)] <- message
        result$inside_cluster[nrow(result)] <- inside
        
        if (!inside) { # If not inside the area, start check on buffer
          area_UTM <- spTransform(x = cluster_area, UTM38N)
          area_buffer <- spTransform(gBuffer(spgeom = area_UTM, width = 1000), WGS84)
          inside_buffer <- gContains(area_buffer,interview_locations_geo)
          result$inside_buffer[nrow(result)] <- inside_buffer
        }
      } else {
        message <- sprintf("No cluster exists for %s - %s\n", 
                           clusters[j], pop_groups[i])
        cat(message)
        result$inside_cluster[nrow(result)] = FALSE
        result$inside_buffer[nrow(result)] = FALSE
        inside <- FALSE
        inside_buffer <- FALSE
        result$message[nrow(result)] <- message
      }
      if (!inside & !inside_buffer){ # Start check on different clusters
        for (k in 1:length(clusters)) {
          cluster_ <- strsplit(clusters[k],"_")[[1]][4]
          cluster_area_sf_ <- sample_areas[[pop_groups[i]]] %>% 
            as("sf") %>% filter(startsWith(label, cluster_)) 
          if (nrow(cluster_area_sf_) < 1){
            result$inside_alternative_cluster[nrow(result)] <- FALSE
            next
          }
          cluster_area_ <- cluster_area_sf_ %>% as("Spatial")
          inside_ <- gContains(cluster_area_,interview_locations_geo)
          result$inside_alternative_cluster[nrow(result)] <- inside_
          if (inside_) {
            result$alternative_cluster[nrow(result)] <- clusters[k]
            break
          } 
        }
      }
      if (!inside_) { # Start check on different population group
        for (l in (1:length(pop_groups))[-i]) {
          cluster_area_sf <- sample_areas[[pop_groups[l]]] %>% 
            as("sf") %>% filter(startsWith(label, cluster))
          if (nrow(cluster_area_sf) < 1) {
            result$inside_alternative_group[nrow(result)] <- FALSE
            next
          }
          cluster_area <- cluster_area_sf %>% as("Spatial")
          inside_alternative_group <- gContains(cluster_area,interview_locations_geo)
          result$inside_alternative_group[nrow(result)] <- inside_alternative_group
          if (inside_alternative_group){
            result$alternative_group[nrow(result)] <- pop_groups[l]
            break
          }
        }
      }
      
      if (write_to_file) {
        cent <- gCentroid(cluster_area)
        all_points <- rbind(interview_locations_geo@coords,cent@coords)
        loc <- bbox(all_points)
        map <- get_map(c(loc[1,1]-0.05, loc[2,1]-0.05, loc[1,2]+0.05, loc[2,2]+0.05))
        g <- ggmap(map) + geom_point(data = interview_locations, 
                                     aes(x = X_gpslocation_longitude, y = X_gpslocation_latitude),
                                     color = "red", size=0.3) +
          geom_polygon(data=fortify(cluster_area), aes(long, lat), fill="red", colour="red", alpha=0.1) +
          ggtitle(sprintf("%d. %s %s", counter, clusters[j], pop_groups[i]))
        ggsave(sprintf("%s/pics/%d.%s_%s.png", dir, counter, clusters[j], pop_groups[i]), plot = g, dpi=1000)
        writeOGR(interview_locations_geo, dsn = paste(dir,"shapes", sep="/"), 
                 layer=sprintf("%d.%s_%s", counter, clusters[j], pop_groups[i]),
                 driver = "ESRI Shapefile", overwrite_layer = TRUE)
      }
    }
  }
  return(result)
}

points.inside.cluster.separated <- function(data, samplepoints, sample_areas, dir, cluster, pop_group,
                                            alternative_clusters, buffer=1000) {
  
  result <- data.frame(uuid = character(), precision = character(), inside_cluster = logical(), 
                       inside_buffer = logical(), inside_alternative_cluster = logical(), alternative_cluster = character(), 
                       stringsAsFactors = F)
  counter <- 0
  
  data_pop <- data %>% filter(population_group == pop_group) 
  data_on_cluster <- data_pop %>% filter(cluster_location_id == cluster)
  interview_locations <- data_on_cluster %>% dplyr::select(c(X_gpslocation_longitude,X_gpslocation_latitude))
  interview_locations_geo <- SpatialPointsDataFrame(interview_locations,interview_locations, proj4string = WGS84)
  
  cluster_nr <- strsplit(cluster,"_")[[1]][4]
  cluster_area_sf <- sample_areas[[pop_group]] %>% 
    as("sf") %>% filter(startsWith(label, cluster_nr))
  
  
  for (i in 1:nrow(interview_locations)){
    counter <- counter + 1
    result[nrow(result) + 1, 1] <- data_on_cluster[i,"X_uuid"]
    result[nrow(result), 2] <- data_on_cluster$X_gpslocation_precision[i]
    
    if (nrow(cluster_area_sf) > 0) { # Check if cluster / population group exists
      cluster_area <- cluster_area_sf %>% as("Spatial")
      inside <- gContains(cluster_area,interview_locations_geo[i,])
      
      
      if (!inside) { # If not inside the area, start check on buffer
        area_UTM <- spTransform(x = cluster_area, UTM38N)
        area_buffer <- spTransform(gBuffer(spgeom = area_UTM, width = buffer), WGS84)
        inside_buffer <- gContains(area_buffer,interview_locations_geo[i,])
      }
    } else {
      inside <- FALSE
      inside_buffer <- FALSE
    }
    result$inside_cluster[nrow(result)] <- inside
    result$inside_buffer[nrow(result)] <- inside_buffer
    
    if (!inside & !inside_buffer){ # Start check on different clusters
      for (k in 1:length(alternative_clusters)) {
        cluster_ <- strsplit(alternative_clusters[k],"_")[[1]][4]
        cluster_area_sf_ <- sample_areas[[pop_group]] %>% 
          as("sf") %>% filter(startsWith(label, cluster_)) 
        if (nrow(cluster_area_sf_) < 1){
          result$inside_alternative_cluster[nrow(result)] <- FALSE
          next
        }
        cluster_area_ <- cluster_area_sf_ %>% as("Spatial")
        inside_ <- gContains(cluster_area_,interview_locations_geo[i,])
        result$inside_alternative_cluster[nrow(result)] <- inside_
        if (inside_) {
          result$alternative_cluster[nrow(result)] <- alternative_clusters[k]
          break
        } 
      }
    }
  }
  return(result)
}
