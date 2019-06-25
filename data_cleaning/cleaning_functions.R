kobo.xlsx.to.csv <- function(dir, anonymise=F, anonymise_cols = NULL) {
  files <- list.files(dir, pattern="*.xlsx")
  parent <- read.xlsx(paste(dir,files[1],sep="/"),
                      sheetName = "Iraq MCNA VII_HH Survey")
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
    write.csv(parent,paste0(dir,"/parent.csv"))
  }
  write.csv(child,paste0(dir,"/child.csv"))
}