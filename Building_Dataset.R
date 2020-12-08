library(dplyr)
#Liste der Daten:
path <- "//192.168.178.187/Shared Files/Dokumente/Masterarbeit Python/"
new_files <- list.files(path, pattern = "*.csv")
archive_files <- list.files(path, pattern = "*4.csv") #4, um die alten Dateien zu ignorieren. 
files <- c(new_files, archive_files)

#Einlesen der Daten: 
data_all <- data.frame()
for (i in 1:length(files)) {
  temp_data <- data.table::fread(paste0(path,files[i]), sep = ";", encoding = "UTF-8")
  data_all <- rbind(data_all, temp_data)
}

saveRDS(data_all, "all_comments.rds")
