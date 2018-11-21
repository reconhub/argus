#2018/11/20
# Objective:  identify and download R packages needed to be installed on a new Argus installation

# Path of the folder to download packages ### TO BE CHANGED
path_folder <- "C:/Local_drive/travaux/3_Projects/1_Team_projects/Argus/5_Development/Technical/Argus_Dashboard/R scripts/packages/" 

# List of R packages to be loaded to run the R scripts for the 2 monitoring dashboards ### To BE UPDATED ID NEEDED
listPackages <- c("shiny.i18n","RMySQL","lubridate","ggplot2","dplyr","tidyr","data.table","sf","rmarkdown", "svglite", "taskscheduleR", "flexdashboard", "DT") 

# List of all dependencies of above packages
library(tools)
dependencies <- package_dependencies(listPackages,recursive = T)

# List of packages installed with R (https://stats.idre.ucla.edu/r/faq/how-can-i-manage-r-packages/ )

installed <- installed.packages()
installed_base <- names(which(!is.na(installed[,"Priority"])))

# Data frames with list of required packages
tableDepend <- data.frame(package=NA, dependencies=NA)
temp <- NA

for (i in 1:length(dependencies)) {
  if(length(dependencies[[i]])==0) {
    temp <- data.frame(package=names(dependencies)[i], dependencies=NA)
    tableDepend <- rbind(tableDepend,temp)
  } else {
    temp <- data.frame(package=names(dependencies)[i], dependencies=dependencies[[i]])
    tableDepend <- rbind(tableDepend,temp)
  }
}

tableDepend <- tableDepend[-1,]

duplicateDepend <- tableDepend$dependencies[duplicated(tableDepend$dependencies)]

tableDepend$unique <- NA # is the dependency only required for this package?
for (i in 1:length(tableDepend$dependencies)) {
  tableDepend$unique[i] <- ifelse(tableDepend$dependencies[i] %in% duplicateDepend, "No", "Yes")
}

tableDepend$base_install <- NA # is the dependency already installed with R?
for (i in 1:length(tableDepend$dependencies)) {
  tableDepend$base_install[i] <- ifelse(tableDepend$dependencies[i] %in% installed_base, "Yes", "No")
}

unique_packages <- unique(c(listPackages,unique(tableDepend$dependencies)))
unique_packages_to_install <- data.frame(install=unique_packages[-which(unique_packages %in% installed_base)])

# download required packages as zip binaries

unique_packages_to_install <- as.character(unique_packages_to_install$install)

for (i in 1:length(unique_packages_to_install)) {
  download.packages(unique_packages_to_install[i],destdir = path_folder, type = "win.binary")
}

# OPTIONAL: Export  the required packages information to an Excel spreadsheet

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb,sheetName = "dependencies", gridLines = T)
addWorksheet(wb, sheetName = "to_install")

writeDataTable(wb,1, tableDepend , withFilter = T)
writeDataTable(wb,2, unique_packages_to_install)

saveWorkbook(wb, paste0(path_folder,"listDependancies.xlsx"), overwrite = T)

