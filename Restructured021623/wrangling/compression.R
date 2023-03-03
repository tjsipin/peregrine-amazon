library(readr)
library(stringr)

raw_colombia_files <- list.files("~/peregrine_amazon/data/colombia/raw", full.names = T, recursive = T)

for(file in raw_colombia_files){
  print(file)
  if (file.info(file)$size > 50000000) {
    data <- read.csv(file)
    saveRDS(data, paste0(str_sub(file, end = -5), ".rds"), compress = T)
    compressedRDS <- readRDS(paste0(file, ".rds"))
    write_csv(compressedRDS, paste0(file, ".gz"))
  } else {
    next
  }
}

write_csv(read.csv("~/peregrine_amazon/data/colombia/raw/full_colombia_FLDAS_raw.csv"))