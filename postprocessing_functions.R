pretty.output <- function(summary, independent.var.value, analysisplan, cluster_lookup_table, lookup_table, severity = FALSE, camp=FALSE) {
  subset <- summary[which(summary$independent.var.value == independent.var.value),]
  independent.var <- subset$independent.var[1]
  if(is.na(independent.var)) {
    analplan_subset <- analysisplan
  } else {
    analplan_subset <- analysisplan[which(analysisplan$independent.variable == independent.var),]
  }
  vars <- unique(subset$dependent.var)
  districts <- unique(subset$repeat.var.value)
  start <- ifelse(camp, 1, 19)
  df <- data.frame(governorate = lookup_table$filter[start:nrow(lookup_table)][match(districts, lookup_table$name[start:nrow(lookup_table)])],  
                   district = districts, stringsAsFactors = F)
  df <- df[with(df, order(governorate, district)),]
  for(i in 1:length(vars)){
    var_result <- subset[which(subset$dependent.var == vars[i]),]
    df[,vars[i]] <- var_result[match(df$district, var_result$repeat.var.value), "numbers"]
    df[,sprintf("%s_min", vars[i])] <- var_result[match(df$district, var_result$repeat.var.value), "min"]
    df[,sprintf("%s_max", vars[i])] <- var_result[match(df$district, var_result$repeat.var.value), "max"]
  }
  extra_heading <- data.frame(t(vars), stringsAsFactors = F)
  colnames(extra_heading) <- vars
  extra_heading[1,] <- t(analplan_subset$Indicator.Group...Sector[match(vars, analplan_subset$dependent.variable)])
  extra_heading[2,] <- t(analplan_subset$research.question[match(vars, analplan_subset$dependent.variable)])
  extra_heading[3,] <- t(analplan_subset$sub.research.question[match(vars, analplan_subset$dependent.variable)])
  extra_heading[4,] <- t(analplan_subset$dependent.variable.type[match(vars, analplan_subset$dependent.variable)])
  if (severity){
    extra_heading[5,] <- t(analplan_subset$consequence[match(vars, analplan_subset$dependent.variable)])
  }
  df <- rbind.fill(df, extra_heading)
  df <- df[c((nrow(df)-(nrow(extra_heading) - 1)):nrow(df),1:(nrow(df)-nrow(extra_heading))),]
  df$district <- lookup_table$english[match(df$district, lookup_table$name)]
  if(!camp){df$governorate <- lookup_table$english[match(df$governorate, lookup_table$name)]}
  df[1:nrow(extra_heading), which(is.na(df[1,]))] <- ""
  df
}

correct.zeroes <- function(summary) {
  zeroes <- which(summary$dependent.var.value == 0 & summary$numbers == 1)
  summary$dependent.var.value[zeroes] <- 1
  summary$numbers[zeroes] <- 0
  summary$min[zeroes] <- 0
  summary$max[zeroes] <- 0
  return(summary)
}

severity_for_pin <- function(filename, analysisplan){
  group_data <- read.csv(filename, stringsAsFactors = F)
  indicators <- names(group_data)[-c(1,2,which(endsWith(names(group_data), "min") | endsWith(names(group_data), "max")))]
  ind_sep <- unique(unlist(strsplit(indicators, "_"))[seq(1,length(indicators)*2,2)])
  for (j in 1:length(ind_sep)){
    ind_cols <- which(startsWith(names(group_data), paste0(ind_sep[j],"_")) & (!endsWith(names(group_data), "min") & 
                                                                                 !endsWith(names(group_data), "max")))
    names_ind_cols <- names(group_data)[ind_cols]
    sum_cols <- names_ind_cols[which(endsWith(names_ind_cols, "3") | endsWith(names_ind_cols, "4") | endsWith(names_ind_cols, "5"))]
    new_df <- as.data.frame(group_data[6:nrow(group_data),sum_cols])
    new_df <- apply(new_df,2,FUN=as.numeric)
    group_data[6:nrow(group_data),ind_sep[j]] <- rowSums(new_df)
  }
  group_pin <- group_data[-c(3,4),c("district", "governorate", ind_sep)]
  dap_selection <- unlist(strsplit(analysisplan$dependent.variable, "_"))[seq(1,nrow(analysisplan)*2,2)] %in% ind_sep
  group_pin[1,] <- c("","",analysisplan$Indicator.Group...Sector[dap_selection][seq(1,length(which(dap_selection)),5)])
  group_pin[2,] <- c("","",analysisplan$research.question[dap_selection][seq(1,length(which(dap_selection)),5)])
  group_pin[3,] <- c("","",analysisplan$consequence[dap_selection][seq(1,length(which(dap_selection)),5)])
  return(group_pin)
}

analysisplan_nationwide <- function(analysisplan) {
  analysisplan$repeat.for.variable <- ""
  return(analysisplan)
}
analysisplan_pop_group_aggregated <- function(analysisplan) {
  analysisplan$independent.variable <- ""
  analysisplan$independent.variable.type <- ""
  return(analysisplan)
}
