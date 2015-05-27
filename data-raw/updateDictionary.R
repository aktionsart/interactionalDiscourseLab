# update the processed translation file translation.bin
# run this every time dictionary.csv is updated
# it reads the look-up table in dictionary.csv and turns it into a 2D list

translationContent <- read.delim("dictionary.csv", header = TRUE, sep = "\t", as.is = TRUE)
translation <- plyr::dlply(translationContent ,plyr::.(key), function(s) key = as.list(s))

devtools::use_data(translation, internal = TRUE, overwrite = TRUE)
