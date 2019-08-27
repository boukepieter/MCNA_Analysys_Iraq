pretty.output <- function(summary, independent.var.value, analysisplan, cluster_lookup_table, lookup_table) {
  subset <- summary[which(summary$independent.var.value == independent.var.value),]
  independent.var <- subset$independent.var[1]
  analplan_subset <- analysisplan[which(analysisplan$independent.variable == independent.var),]
  vars <- unique(subset$dependent.var)
  districts <- unique(subset$repeat.var.value)
  df <- data.frame(governorate = cluster_lookup_table$governorate[match(districts, cluster_lookup_table$district)],  
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
  df <- rbind.fill(df, extra_heading)
  df <- df[c((nrow(df)-3):nrow(df),1:(nrow(df)-4)),]
  df$district <- lookup_table$english[match(df$district, lookup_table$name)]
  df$governorate <- lookup_table$english[match(df$governorate, lookup_table$name)]
  df[1:4, which(is.na(df[1,]))] <- ""
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
