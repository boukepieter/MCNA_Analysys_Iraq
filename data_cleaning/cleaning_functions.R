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

anonymise.cleaned.data <- function(dir, name = NULL, anonymise_cols = NULL) {
  name <- ifelse(is.null(name), "parent", name)
  parent <- read.csv(sprintf("%s/%s_cleaned.csv", dir, name), stringsAsFactors = F, encoding = "UTF-8")
  if (is.null(anonymise_cols)) {
    anonymise_cols <- c(grep("*contact*",names(parent)), 
                        grep("*gpslocation*",names(parent)))
  }
  parent_ano <- parent[,-anonymise_cols]
  write.csv(parent_ano,sprintf("%s/%s_cleaned_anonymised.csv", dir, name), row.names = F, fileEncoding = "UTF-8")
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
    log <- data.frame(log_date = character(), uuid = character(), question.name = character(), 
                      issue = character(), feedback = character(), 
                      action = character(), changed = logical(), old.value = character(), 
                      new.value = character(), 
                      stringsAsFactors = F)
  }
  for (i in 1:length(uuid)){
    log[nrow(log)+1,"uuid"] <- uuid[i]
    log$log_date[nrow(log)] <- format(Sys.Date(), "%d-%m-%Y")
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
log.cleaning.change.loop <- function(data, loop, partners, psu, index, action,  
                                         question.name=NULL, new.value=NULL, issue=NULL,
                                         dir) {
  action <- ifelse(action == "c", "change", ifelse(action == "d", "deletion", ifelse(action == "f", "flag", action)))
  if (!action %in% c("change", "deletion", "flag")) {
    stop("action given is not a valid action")
  }
  if (action == "change" & (is.null(question.name) | is.null(new.value))) {
    stop("For a change all the parameters of old.value, question.name and new.value should be given.")
  }
  if (action == "flag" & is.null(question.name)) {
    stop("For a flag the parameter question.name should be given.")
  }
  log_file <- sprintf("%s/cleaning_logbook.csv",dir)
  if (file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    log <- read.csv(log_file, stringsAsFactors = F, encoding = "UTF-8")
  } else {
    log <- data.frame(survey = character(), uuid = character(), governorate = character(), log_date = character(), 
                      location_id = character(), location_name = character(), district = character(),
                      ngo = character(), ngo_name = character(), enumerator = character(), 
                      question.name = character(), 
                      issue = character(), feedback = character(), 
                      action = character(), changed = logical(), old.value = character(), 
                      new.value = character(), 
                      stringsAsFactors = F)
  }
  for (i in 1:length(index)){
    uuid <- loop$X_submission__uuid[which(loop$X_index == index[i])]
    loop_subset <- loop[which(loop$X_submission__uuid == uuid),]
    n <- which(loop_subset$X_index == index[i])
    
    log[nrow(log)+1,"uuid"] <- paste(uuid, n, sep="|")
    row_nr <- which(data$X_uuid == uuid)
    log$survey[nrow(log)] <- "loop"
    log$governorate[nrow(log)] <- data$governorate_mcna[row_nr]
    log$log_date[nrow(log)] <- format(Sys.Date(), "%d-%m-%Y")
    log$location_id[nrow(log)] <- data$cluster_location_id[row_nr]
    log$location_name[nrow(log)] <- psu[match(data$cluster_location_id[row_nr], psu[,3]),7]
    log$district[nrow(log)] <- psu[match(data$cluster_location_id[row_nr], psu[,3]),5]
    log$ngo[nrow(log)] <- data$ngo[row_nr]
    log$ngo_name[nrow(log)] <- partners[match(data$ngo[row_nr], partners[,1]),2]
    log$enumerator[nrow(log)] <- data$enumerator_num[row_nr]
    log$action[nrow(log)] <- action
    log$issue[nrow(log)] <- issue
    if (action != "deletion") {
      log$question.name[nrow(log)] <- question.name
      log$old.value[nrow(log)] <- loop[which(loop$X_index == index[i]), question.name]
    }
    if (action == "change") {
      log$new.value[nrow(log)] <- new.value
    }
  }
  write.csv(log, log_file, row.names = F, fileEncoding = "UTF-8")
  return(log)
}
log.cleaning.change.extended <- function(data, partners, psu, uuid, action,  
                                         question.name=NULL, new.value=NULL, issue=NULL,
                                dir) {
  action <- ifelse(action == "c", "change", ifelse(action == "d", "deletion", ifelse(action == "f", "flag", action)))
  if (!action %in% c("change", "deletion", "flag")) {
    stop("action given is not a valid action")
  }
  if (action == "change" & (is.null(question.name) | is.null(new.value))) {
    stop("For a change all the parameters of old.value, question.name and new.value should be given.")
  }
  if (action == "flag" & is.null(question.name)) {
    stop("For a flag the parameter question.name should be given.")
  }
  log_file <- sprintf("%s/cleaning_logbook.csv",dir)
  if (file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    log <- read.csv(log_file, stringsAsFactors = F, encoding = "UTF-8")
  } else {
    log <- data.frame(survey = character(), uuid = character(), governorate = character(), log_date = character(), 
                      location_id = character(), location_name = character(), district = character(),
                      ngo = character(), ngo_name = character(), enumerator = character(), 
                      question.name = character(), 
                      issue = character(), feedback = character(), 
                      action = character(), changed = logical(), old.value = character(), 
                      new.value = character(), 
                      stringsAsFactors = F)
  }
  for (i in 1:length(uuid)){
    log[nrow(log)+1,"uuid"] <- uuid[i]
    row_nr <- which(data$X_uuid == uuid[i])
    log$survey[nrow(log)] <- ifelse("ngo" %in% names(data), "parent", "loop")
    log$governorate[nrow(log)] <- data$governorate_mcna[row_nr]
    log$log_date[nrow(log)] <- format(Sys.Date(), "%d-%m-%Y")
    log$location_id[nrow(log)] <- data$cluster_location_id[row_nr]
    log$location_name[nrow(log)] <- psu[match(data$cluster_location_id[row_nr], psu[,3]),7]
    log$district[nrow(log)] <- psu[match(data$cluster_location_id[row_nr], psu[,3]),5]
    log$ngo[nrow(log)] <- data$ngo[row_nr]
    log$ngo_name[nrow(log)] <- partners[match(data$ngo[row_nr], partners[,1]),2]
    log$enumerator[nrow(log)] <- data$enumerator_num[row_nr]
    log$action[nrow(log)] <- action
    log$issue[nrow(log)] <- issue
    if (action != "deletion") {
      log$question.name[nrow(log)] <- question.name
      log$old.value[nrow(log)] <- data[row_nr, question.name]
    }
    if (action == "change") {
      log$new.value[nrow(log)] <- new.value
    }
  }
  write.csv(log, log_file, row.names = F, fileEncoding = "UTF-8")
  return(log)
}
execute.cleaning.changes <- function(dir, filenameparent = "parent", filenamechild = "child", uuid_column=NULL) {
  if (!file.exists(sprintf("%s/cleaning_logbook.csv",dir))){
    stop("no cleaning file found")
  }
  log <- read.csv(sprintf("%s/cleaning_logbook.csv",dir), stringsAsFactors = F, encoding = "UTF-8")
  data <- read.csv(sprintf("%s/%s.csv", dir, filenameparent), stringsAsFactors = F, encoding = "UTF-8")
  child <- read.csv(sprintf("%s/%s.csv", dir, filenamechild), stringsAsFactors = F, encoding = "UTF-8")
  match_parent <- grep(pattern = "uuid", x = names(data))
  match_child <- grep(pattern = "uuid", x = names(child))
  if (is.null(uuid_column)){
    if (length(match_parent) < 1 | length(match_child) < 1) {
      stop("cannot find uuid column")
    } else if (length(match_parent) > 1 | length(match_child) > 1) {
      stop("multiple uuid columns found")
    } else {
      uuid_column_parent <- names(data)[match_parent]
      uuid_column_child <- names(child)[match_child]
    }
  }
  parents <- which(log$survey == "parent")
  children <- which(log$survey == "loop")
  for(i in parents){
    if (log$action[i] == "change" & log$uuid[i] %in% data$X_uuid) {
      if (log$question.name[i] %in% names(data)){
      data[which(data[,uuid_column_parent] == log$uuid[i]), trimws(log$question.name[i], which = "right")] <- log$new.value[i]
      log$changed[i] <- TRUE
      } else {
        log$changed[i] <- "ERROR: cannot find question"
      }
    } else if (log$action[i] == "deletion" & log$uuid[i] %in% data$X_uuid) {
      data <- data[-which(data[,uuid_column_parent] == log$uuid[i]), ]
    }
  }
  tobedeleted <- c()
  for(i in children){
    uuid_split <- strsplit(log$uuid[i], split="|", fixed=T)[[1]]
    if (log$action[i] == "change" & uuid_split[1] %in% data$X_uuid) {
      if (log$question.name[i] %in% names(child)){
      child[which(child[,uuid_column_child] == uuid_split[1])[as.numeric(uuid_split[2])], trimws(log$question.name[i], which = "right")] <- log$new.value[i]
      log$changed[i] <- TRUE
      } else {
        log$changed[i] <- "ERROR: cannot find question"
      }
    } else if (log$action[i] == "deletion" & uuid_split[1] %in% data$X_uuid) {
      tobedeleted <- c(tobedeleted, which(child[,uuid_column_child] == uuid_split[1])[as.numeric(uuid_split[2])])
    }
  }
  if (length(tobedeleted > 0)){
  child <- child[-tobedeleted, ]}
  write.csv(log, sprintf("%s/cleaning_logbook.csv", dir), row.names = F, fileEncoding = "UTF-8")
  write.csv(data, sprintf("%s/%s_cleaned.csv", dir, filenameparent), row.names = F, fileEncoding = "UTF-8")
  write.csv(child, sprintf("%s/%s_cleaned.csv", dir, filenamechild), row.names = F, fileEncoding = "UTF-8")
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
      interview_locations <- data_on_cluster %>% dplyr::select(c(X_gpslocation_longitude,X_gpslocation_latitude, X_uuid))
      interview_locations_geo <- SpatialPointsDataFrame(interview_locations[,c(1,2)],interview_locations, 
                                                        proj4string = WGS84)
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
        } else {
          inside_buffer <- TRUE
        }
        result$inside_buffer[nrow(result)] <- inside_buffer
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
      inside_ <- TRUE
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
      
      if (write_to_file & nrow(cluster_area_sf) > 0) {
        cent <- gCentroid(cluster_area)
        all_points <- rbind(interview_locations_geo@coords,cent@coords)
        loc <- bbox(all_points)
        map <- get_map(c(loc[1,1]-0.05, loc[2,1]-0.05, loc[1,2]+0.05, loc[2,2]+0.05))
        g <- ggmap(map) + geom_point(data = interview_locations, 
                                     aes(x = X_gpslocation_longitude, y = X_gpslocation_latitude),
                                     color = "red", size=0.3) +
          geom_polygon(data=fortify(cluster_area), aes(long, lat), fill="red", colour="red", alpha=0.1) +
          ggtitle(sprintf("%d. %s %s (%s)", counter, clusters[j], pop_groups[i], nrow(interview_locations)))
        ggsave(sprintf("%s/pics/%d.%s_%s.png", dir, counter, clusters[j], pop_groups[i]), plot = g, dpi=1000)
      }
      writeOGR(interview_locations_geo, dsn = paste(dir,"shapes", sep="/"), 
               layer=sprintf("%d.%s_%s", counter, clusters[j], pop_groups[i]),
               driver = "ESRI Shapefile", overwrite_layer = TRUE)
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
  interview_locations <- data_on_cluster %>% dplyr::select(c(X_gpslocation_longitude, X_gpslocation_latitude))
  interview_locations_geo <- SpatialPointsDataFrame(interview_locations, interview_locations, proj4string = WGS84)
  
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
      } else {
        inside_buffer <- TRUE
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
translate.others.arabic <- function(data, ignore.cols = NULL) {
  cols <- names(data)[endsWith(names(data), "_other")] 
  if (!is.null(ignore.cols)) {
    cols <- cols[-which(!is.na(match(cols, ignore.cols)))]
  }
  result <- data.frame(question.name = character(), uuid = character(), row = numeric(), arabic = character(), english = character(),
                       stringsAsFactors = F)
  for (i in 1:length(cols)) {
    cat(sprintf("%d/%d\n", i, length(cols)))
    indices <- which(!is.na(data[,cols[i]]) & data[,cols[i]] != "")
    if (length(indices) > 0) {
      for (j in 1:length(indices)) {
        result[nrow(result) + 1, 1] <- cols[i]
        result$row[nrow(result)] <- indices[j]
        result$uuid[nrow(result)] <- data$X_uuid[indices[j]]
        arab <- data[indices[j], cols[i]]
        result$arabic[nrow(result)] <- arab
        if (is.character(arab)) {
          translation <- gl_translate(arab, target = "en")
          result$english[nrow(result)] <- translation$translatedText
        } else {
          result$english[nrow(result)] <- "ERROR: input is not text"
        }
      }
    }
  }
  return(result)
}
general.checks <- function(data, loop) {
  result <- data.frame(index = numeric(), uuid = character(), has_no_issue = logical(), 
                       loop_is_family_size = logical(), one_hoh_in_loop = logical(), 
                       hoh_info_same_as_respondent = logical(), no_older_children = logical(), 
                       no_single_spouse = logical(), child_calculation_good = logical(),
                       not_employed_without_income = logical(),
                       stringsAsFactors = F)
  for (i in 1:nrow(data)){
    children <- loop[which(loop$X_submission__uuid == data$X_uuid[i]),]
    result[nrow(result) + 1, 1] <- i
    result$uuid[nrow(result)] <- data$X_uuid[i]
    
    # -	Check whether the household size number given and the individual roster fits
    result$loop_is_family_size[nrow(result)] <- nrow(children) == as.numeric(data$num_family_member[i])
    
    # HOH is included in the loop
    result$one_hoh_in_loop[nrow(result)] <- length(which("head" == children$relationship)) == 1
    
    # HoH info same as respondent (if respondent is HoH)
    if (data$hhh[i] == "yes" & result$one_hoh_in_loop[nrow(result)]) {
      result$hoh_info_same_as_respondent[nrow(result)] <- all(children %>% filter(relationship == "head") %>% 
                                                                dplyr::select(age, sex) ==
                                                                data[i,c("age_respondent","gender_respondent")])
    }
    
    # Family age corresponds HoH relationship
    if (result$one_hoh_in_loop[nrow(result)]) {
      head_age <- children %>% filter(relationship == "head") %>% dplyr::select(age)
      younger_age <- children %>% filter(relationship %in% c("child", "grandchild", "nephew_niece")) %>% 
        dplyr::select(age)
      if (nrow(younger_age) < 1) {younger_age <- head_age}
      older_age <- children %>% filter(relationship %in% c("parent", "parentinlaw")) %>% dplyr::select(age)
      if (nrow(older_age) < 1) {older_age <- head_age}
      result$no_older_children[nrow(result)] <- all(all(younger_age$age <= head_age), all(older_age$age >= head_age))
    }
    
    # check for sinlge spouses
    if ("spouse" %in% children$relationship) {
      result$no_single_spouse[nrow(result)] <- all(children %>% filter(relationship == "spouse") %>% 
                                                     dplyr::select(marital_status) == "married")
    }
    
    # check child calculation
    result$child_calculation_good[nrow(result)] <- data$tot_child[i] == children %>% filter(age < 18) %>% nrow
    
    # check on employment without income (or income without employment)
    result$not_employed_without_income[nrow(result)] <- ("yes" %in% children$work & 
                                                           data$primary_livelihood.employment[i] == 1) |
      (! "yes" %in% children$work & data$primary_livelihood.employment[i] != 1)
    
    # summarizing has issue
    result$has_no_issue[nrow(result)] <- all(result[nrow(result),4], result[nrow(result),5], result[nrow(result),6], 
                                             result[nrow(result),7], result[nrow(result),8], result[nrow(result),9],
                                             result[nrow(result),10])
    result$has_no_issue[nrow(result)] <- ifelse(is.na(result$has_no_issue[nrow(result)]), TRUE, 
                                                result$has_no_issue[nrow(result)])
  }
  result
}
summarize.result <- function(result) {
  funsum <- function(x) {
    data.frame(true = length(which(x)), false = length(which(!x)), na = length(which(is.na(x))))
  }
  summary <- apply(result[,3:ncol(result)], 2, funsum)
  summary <- matrix(unlist(summary), ncol=3, byrow = T)
  rownames(summary) <- names(result)[3:10]
  colnames(summary) <- c("true", "false", "na")
  summary
}
summary.of.partner <- function(data, loop, partner) {
  data_partner <- data %>% filter(ngo == partner)
  result <- general.checks(data_partner, loop)
  summary <- summarize.result(result)
  round(summary / nrow(data_partner) * 100)
}
set.up.cleaning.dir <- function(dir, name = NULL) {
  main_dir <- dir
  date <- format(Sys.Date(), "%Y%m%d")
  date <- ifelse(is.null(name), date, name)
  dirs <- list.dirs(main_dir, recursive = F)
  old_dirs <- dirs[grep(pattern = "raw_data/[0-9]",x = dirs)]
  dir.create(sprintf("%s/%s", main_dir, date))
  dir.create(sprintf("%s/%s/pics", main_dir, date))
  file.copy(sprintf("%s/cleaning_logbook.csv", old_dirs[length(old_dirs)]), 
            sprintf("%s/%s/cleaning_logbook.csv", main_dir, date))
  file.copy(sprintf("%s/parent_cleaned.csv", old_dirs[length(old_dirs)]), 
            sprintf("%s/%s/parent_cleaned_old.csv", main_dir, date))
  file.copy(sprintf("%s/surveys_cleaned.csv", old_dirs[length(old_dirs)]), 
            sprintf("%s/%s/surveys_cleaned_old.csv", main_dir, date))
  
}
map.finished.districts <- function(data, log, loc_overview, strata, govs, dir) {
  uuid <- log %>% dplyr::filter(question.name %in% c("calc_host", "calc_idp", "calc_returnee", "cluster_location_id", "host_calc") &
                                  action == "flag" & (is.na(feedback) | ! startsWith(feedback, "keep"))) %>% dplyr::select(uuid)
  "11bf3e23-711b-4026-a099-6605a2e5d14e" %in% uuid$uuid # TRUE 
  "5da1b0f4-aa8b-4fb9-be7e-73b57611e01b" %in% uuid$uuid # FALSE 85
  "2250a84f-d30c-45b3-961c-79dfafd78de3" %in% uuid$uuid # FALSE 825
  data_filtered <- data %>% filter(! X_uuid %in% uuid$uuid)
  loc_overview <- loc_overview[which(loc_overview$Survey !=0 & !is.na(loc_overview$Survey)),]
  for (i in 1:nrow(loc_overview)) {
    n <- data_filtered %>% filter(endsWith(data_filtered$cluster_location_id, substr(loc_overview$label[i],1,4)) 
                                  & population_group == tolower(loc_overview$pop_group[i]))
    loc_overview$surveys_done[i] <- nrow(n)
    loc_overview$complete[i] <- min(loc_overview$surveys_done[i] / loc_overview$Survey[i], 1)
  }
  write.csv(loc_overview, sprintf("%s/locations_overview.csv",dir), row.names = F)
  summarized <- loc_overview %>% dplyr::group_by(strata) %>% dplyr::summarise(mean(complete))
  strata$complete <- summarized$`mean(complete)`[match(strata$ADM2_EN, summarized$strata)]
  pal <- colorNumeric("RdYlBu", strata$complete,reverse=F)
  centers <- data.frame(gCentroid(strata, byid = TRUE))
  centers$lbl <- strata$ADM2_EN
  m <- leaflet(strata) %>% 
    addTiles() %>%
    setView(44, 34, zoom = 8) %>% 
    addPolygons(data = strata, color="black",fillColor=~pal(strata$complete),
                fillOpacity=0.75) %>% 
    addLabelOnlyMarkers(data = centers,
                        lng = ~x, lat = ~y, label = ~lbl,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
    addPolygons(data=govs, color = "black", weight = 10, fillOpacity = 0)%>% 
    addPolygons(data = strata, color="black",fillColor=~pal(strata$complete),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                fillOpacity=0,
                popupOptions=popupOptions(maxWidth=300,closeOnClick=T),
                popup = sprintf("%s %%",round(strata$complete * 100,1)))#%>% 
  
  saveWidget(m,sprintf("%s/%s/map_%s_without_buffer.html",getwd(),dir,format(Sys.Date(), "%Y%m%d")))
}
check_if_cleaned <- function(result, check, psu, partners) {
  missing <- which(!result$inside_alternative_cluster)
  table <- data.frame(index=numeric(), cleaned=logical(), district=character(), ngo=character(), stringsAsFactors = F)
  for (i in 1:length(missing)){
    row <- table[i,"index"] <- missing[i]
    uuid <- filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% 
      dplyr::select(X_uuid)
    table$cleaned[i] <- round(length(which(uuid$X_uuid %in% check$uuid)) / nrow(uuid),1)
    table$district[i] <- psu$district[match(result$cluster[row], psu$new_ID)]
    ngo <- filtered_data %>% filter(cluster_location_id == result[row,1] & population_group == result[row,3]) %>% 
      dplyr::select(ngo)
    table$ngo[i] <- partners$V2[match(ngo$ngo, partners$V1)][1]
  }
  return(table)
}
