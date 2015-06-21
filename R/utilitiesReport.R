# helpers for creating a Rmd report

makeReportDocx <- function(file, filename, readFile, filteredData,
                           countSpeakers, countTags, proportionTagPerSpeaker, proportionSpeakerPerTag, # speakers and tags
                           tagSequences, speakerSequences, tr                            # interactions
){
  rmarkdown::render(system.file("extdata", "IDLabReport.Rmd", package = "interactionalDiscourseLab"), "word_document", output_file = file)
}

makeReportHTML <- function(file, filename, readFile, filteredData,
                           countSpeakers, countTags, proportionTagPerSpeaker, proportionSpeakerPerTag, # speakers and tags
                           tagSequences, speakerSequences, tr                             # interactions
){
  rmarkdown::render(system.file("extdata", "IDLabReport.Rmd", package = "interactionalDiscourseLab"), "html_document", output_file = file)
}

makeReportPDF <- function(file, filename, readFile, filteredData,
                          countSpeakers, countTags, proportionTagPerSpeaker, proportionSpeakerPerTag, # speakers and tags
                          tagSequences, speakerSequences, tr # interactions
                          ){
  rmarkdown::render(system.file("extdata", "IDLabReport.Rmd", package = "interactionalDiscourseLab"), "pdf_document", output_file = paste0(file,".pdf"))
  file.rename(paste0(file,".pdf"), file)
}

