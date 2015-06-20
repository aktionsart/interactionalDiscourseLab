#' Launches the interface
#'
#' @return opens a web browser with the interface running.
#' @export
#' @import shiny
#' @import rCharts
#' @import magrittr
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom tidyr spread
#' @importFrom stringr str_extract
#' @importFrom stringr str_sub
#' @importFrom lazyeval lazy_dots
#' @import igraph
#' @import ggplot2
#' @import MultinomialCI
#' @import rmarkdown
#'
launchIDLab <- function(){

  app <- list(
  ui = shiny::fluidPage(
    shiny::tags$h1("Interactional Discourse Lab"),
    sidebarLayout(
      sidebarPanel(
      uiOutput("uploadUI"),
      shiny::tags$hr(),
      uiOutput("tagList"),
      shiny::tags$hr(),
      shiny::tags$h3("Language:"),
      radioButtons(inputId = "language", label = "",
                   choices = c("English" = "en", "Espa\u00f1ol" = "es"),
                   selected = "en"),
      uiOutput("savingOptionsUI"),
      uiOutput("timelineSliderUI")
      ),

      mainPanel(
        tabsetPanel(
        id = "theTabs",
        tabPanel("Speakers and tags",
                 htmlOutput("sanityCheck"),
                 shiny::tags$hr(),
                 uiOutput("speakersTagsTitle"),
                 uiOutput("speakerParticipationText"),
                 showOutput("speakerParticipation","nvd3"),
                 shiny::tags$hr(),
                 uiOutput("tagsBySpeakerTitle"),
                 uiOutput("tagsBySpeakerText"),
                 showOutput("tagBySpeakerNormalised","nvd3"),
                 shiny::tags$hr(),
                 uiOutput("speakersByTagTitle"),
                 uiOutput("speakersByTagText"),
                 showOutput("speakerByTagNormalised","nvd3")#,
        ),

        tabPanel("Interactions",
                 htmlOutput("sanityCheck2"),
                 uiOutput("string_NetworkDescription"),
                 uiOutput("string_ConfidenceInterval"),
                 uiOutput("noteTagSelection"),
                 shiny::tags$hr(),
                 uiOutput("interactionTags"),
                 uiOutput("string_TagNetwork"),
                 plotOutput("tagNetwork",height="600px"),
                 shiny::tags$hr(),
                 uiOutput("interactionSpeakers"),
                 uiOutput("string_SpeakerNetwork"),
                 plotOutput("speakerNetwork",height="600px")
        ),

        tabPanel("Timeline",
                 uiOutput("string_TimelineDescription03"),
                 uiOutput("string_TimelineDescription05"),
                 plotOutput("timelineSlider", height="500px"),
                 plotOutput("timelineWhole", height="100px"),
                 uiOutput("string_TimelineDescription06")
        ),

        tabPanel("Saving",
                 htmlOutput("savingDescription")
        ),

        tabPanel("About", value = "About",
                 htmlOutput("about"))


        )))),

    server = function(input, output){
      # # # # # # # # # # # # # #    reactive functions to feed the GUI  # # # # # # # # # # # # # #

      # translates text into current language
      tr <- function(text){
        sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES = FALSE)
      }

      output$savingOptionsUI <- renderUI({
        conditionalPanel(
        condition = "input.theTabs == 'Saving'",
        shiny::tags$hr(),
        shiny::tags$h3(tr("Saving options")),
        downloadButton('downloadReportPDF', tr('Save as pdf')),
        shiny::tags$br(),shiny::tags$br(),
        downloadButton('downloadReportDocx', tr('Save as a Word document')),
        shiny::tags$br(),shiny::tags$br(),
        downloadButton('downloadReportHTML', tr('Save as a webpage')))
      })

      output$timelineSliderUI <- renderUI({
        conditionalPanel(
          condition = "input.theTabs == 'Timeline'",
          shiny::tags$hr(),
          shiny::tags$h3(tr("Timeline controls")),
          sliderInput("startingPoint", tr("Starting point"), 1, totalTurns(), 1, step = 1),
          sliderInput("windowSize", tr("Window size"), 5, max(20, totalTurns()), 20, step = 1)
        )
      })

      output$uploadUI <- renderUI({
        fileInput('file1', tr('Upload your tagged transcript: '),
                  accept=c('text/csv', 'text/comma-separated-values,text/plain'))
      })
      # # # # # # # # # # # # # #    UI: speakers and tags      # # # # # # # # # # # # # #

      output$speakersTagsTitle <- renderUI({
        shiny::tags$h3(tr("Speaker Participation"))
        })

      output$speakerParticipationText <- renderUI({
        shiny::tags$p(tr("string_SpeakerParticipation"))
      })

      output$tagsBySpeakerTitle <- renderUI({
        shiny::tags$h3(tr("Tags used by each speaker"))
      })

      output$tagsBySpeakerText <- renderUI({
        shiny::tags$p(tr("string_TagsUsedByEachSpeaker"))
      })

      output$speakersByTagTitle <- renderUI({
        shiny::tags$h3(tr("Tags used by each speaker"))
      })

      output$speakersByTagText <- renderUI({
        shiny::tags$p(tr("string_SpeakersByTag"))
      })

      # # # # # # # # # # # # # #    UI: Interactions      # # # # # # # # # # # # # #

      output$string_NetworkDescription <- renderUI({
        shiny::tags$p(tr("string_NetworkDescription"))
      })

      output$string_ConfidenceInterval <- renderUI({
        shiny::tags$p(tr("string_ConfidenceInterval"))
      })

      output$noteTagSelection <- renderUI({
        shiny::tags$em(tr("Note: turns considered are those whose tag is in the current selection."))
      })

      output$interactionTags <- renderUI({
        shiny::tags$h3(tr("Interactions between tags, for contiguous turns"))
      })

      output$interactionSpeakers <- renderUI({
        shiny::tags$h3(tr("Interactions between speakers, for contiguous turns"))
      })

      output$string_SpeakerNetwork <- renderUI({
        shiny::tags$p(tr("string_SpeakerNetwork"))
      })

      # # # # # # # # # # # # # #    UI: timelines      # # # # # # # # # # # # # #

      output$string_TimelineDescription03 <- renderUI({
        shiny::tags$p(tr("string_TimelineDescription03"))
      })

      output$string_TimelineDescription05 <- renderUI({
        shiny::tags$p(tr("string_TimelineDescription05"))
      })

      output$string_TimelineDescription06 <- renderUI({
        shiny::tags$p(tr("string_TimelineDescription06"))
      })

      # # # # # # # # # # # # # #    Parsing and computing      # # # # # # # # # # # # # #

            # get the filename from the GUI
      filename <- reactive({
        inFile <- input$file1
        if (!is.null(inFile)) return(inFile$name) else return("example")
      })

      # read, parse and clean up a file, or used the default transcript
      readFile <- reactive({
        inFile <- input$file1
        if (!is.null(inFile))
          parsed <- parses(inFile$datapath)
        else
          parsed <- simulateMeeting(nTurns = 150, seed = 42) %>% dplyr::select(speaker,tag)

        cleanupParsed(parsed)
      })

      # total number of turns
      totalTurns <- reactive({nrow(readFile())})

      # list of tags from the original data
      output$tagList <- renderUI({
        tags <- stringr::str_extract(as.character(readFile()$tag), "^[^,]*") %>% unique %>% sort
        checkboxGroupInput("tagsOfInterest", tr("Selected tags:"), tags, selected = tags)
      })

      # original data filtered for tags of interest
      filteredData <- reactive({
        xx <- readFile()
        xx$period <- makeConsecutiveTaggedTurns(xx$tag %in% getSelectedTagsFromTopLevelTags(as.character(xx$tag), input$tagsOfInterest))
        xx %>% dplyr::filter(tag %in% getSelectedTagsFromTopLevelTags(as.character(xx$tag), input$tagsOfInterest))
      })

      # Global speaker participation
      countSpeakers <- reactive({
        filteredData() %>% dplyr::count(speaker)
      })

      # Global tag usage
      countTags <- reactive({
        filteredData() %>% dplyr::count(tag)
      })


      # tag usage per speaker
      proportionTagPerSpeaker <- reactive({
        filteredData() %>% count0(speaker, tag) %>%
          dplyr::group_by(speaker) %>% dplyr::mutate(proportion = 100*n/sum(n))
      })

      # Speaker occurence per tag
      proportionSpeakerPerTag <- reactive({
        filteredData() %>% count0(speaker, tag) %>%
          dplyr::group_by(tag) %>% dplyr::mutate(proportion = 100*n/sum(n))

      })

      # Short summary of the data
      sanityCheckText <- reactive({
        if(is.null(filteredData()))
          return("Nothing")
        else
        {
          paste("<h3>", tr("Data summary"), "</h3>",
                 "<p>", tr("File"), ": <TT>", filename(), "</TT></p>",
                 "<p>", tr("Unique speakers:"), " <TT>",
                 paste(sort(unique(filteredData()$speaker)), collapse = ', '),
                 "</TT><p/>",
                 "<p>", tr("Number of selected turns:"), nrow(filteredData()), tr("out of"), totalTurns() ,"</p>" )

        }
      })

      turnTaking <- reactive({
        makeTurnTaking(filteredData())

      })

      # # # # # # # # # # # # # #    Sanity check      # # # # # # # # # # # # # #
      output$sanityCheck <- renderText({
        sanityCheckText()
      })

      output$sanityCheck2 <- renderText({
        sanityCheckText()
      })

      # # # # # # # # # # # # # #    Speakers and tags      # # # # # # # # # # # # # #
      output$speakerParticipation <- renderChart({
        p <- rCharts::nPlot(n ~ speaker,  data = countSpeakers(), type = "discreteBarChart")
        p$chart(showValues = TRUE)
        #   p$chart(reduceXTicks = FALSE) # to get all labels
        p$addParams(dom = 'speakerParticipation')
        p
      })

      output$speakerParticipationGGplot2 <- renderPlot({
        makeSpeakerParticipationGgplot2(countSpeakers())
      })

      output$tagBySpeakerNormalised <- renderChart({
        if(nrow(proportionTagPerSpeaker())==0)
          df <- data.frame(speaker = "speaker", tag = "tag", proportion = 1)
        else
          df <- proportionTagPerSpeaker()


        p <- rCharts::nPlot(proportion ~ speaker, group = "tag", data = df, type = "multiBarChart")
        p$chart(reduceXTicks = FALSE) # to get all labels
        p$addParams(dom = 'tagBySpeakerNormalised')
        p
      })

      output$tagBySpeakerNormalisedGGplot2 <- renderPlot({
        makeTagBySpeakerNormalisedGgplot2(proportionTagPerSpeaker())
      })


      output$speakerByTagNormalised <- renderChart({
        if(nrow(proportionSpeakerPerTag())==0)
          df <- data.frame(speaker = "speaker", tag = "tag", proportion = 1)
        else
          df <- proportionSpeakerPerTag()

        p <- nPlot(proportion ~ tag, group = "speaker", data = df, type = "multiBarChart")
        p$chart(reduceXTicks = FALSE) # to get all labels
        p$addParams(dom = 'speakerByTagNormalised')
        p
      })

      output$speakerByTagNormalisedGGplot2 <- renderPlot({
        makeSpeakerByTagNormalisedGgplot2(proportionSpeakerPerTag())

      })
      # # # # # # # # # # # # # #    interactions      # # # # # # # # # # # # # #

      output$tagNetwork <- renderPlot({
        plotSequence(countTagSequence(turnTaking()), countTags()$n, tr("Tag types sequences"))
      })

      output$speakerNetwork <- renderPlot({
        plotSequence(countSpeakerSequence(turnTaking()), countSpeakers()$n, tr("Contiguous turn takings"))
      })

      # # # # # # # # # # # # # #    Timelines      # # # # # # # # # # # # # #


      output$timelineSlider <- renderPlot({
        plotTimelineSlider(filteredData(), totalTurns(), input$startingPoint, input$windowSize)
      })

      output$timelineWhole <- renderPlot({
        plotTimelineWhole(filteredData(), totalTurns())
      })

      # # # # # # # # # # # # # #    Reports      # # # # # # # # # # # # # #
      output$savingDescription <- function(){

        paste(
          tr("Save your analysis either as"),
          "<ul>", "<li>", tr("a pdf document."), "</li>",
  "<li>", tr("a Word document."), "</li>",
  "<li>", tr("a standalone webpage."),"</li>","</ul>",
  "<br/>", tr("Generating the report can take a few seconds."))
      }

      output$downloadReportDocx <- downloadHandler(
        filename = "IDLabreport.docx",
        content = function(file){
          makeReportDocx(file, filename(), filteredData(),
                         countSpeakers(), countTags(), proportionTagPerSpeaker(), proportionSpeakerPerTag(), # speakers and tags
                         countTagSequence(turnTaking()), countSpeakerSequence(turnTaking()))                  # interactions)
        }
      )
      output$downloadReportHTML <- downloadHandler(
        filename = "IDLabreport.html",
        content = function(file){
          makeReportHTML(
            file, filename(), filteredData(),
            countSpeakers(), countTags(), proportionTagPerSpeaker(), proportionSpeakerPerTag(), # speakers and tags
            countTagSequence(turnTaking()), countSpeakerSequence(turnTaking()))                  # interactions)
        }
      )

      output$downloadReportPDF <- downloadHandler(
        filename = "report.pdf",
        content = function(file){
          makeReportPDF(
            file, filename(), filteredData(),
            countSpeakers(), countTags(), proportionTagPerSpeaker(), proportionSpeakerPerTag(), # speakers and tags
            countTagSequence(turnTaking()), countSpeakerSequence(turnTaking())                  # interactions
            )
        },
        contentType = "application/pdf"
      )


      # # # # # # # # # # # # # #    About      # # # # # # # # # # # # # #
      output$about <- renderText({
        paste("<h3>", tr("Authors:"), "</h3>
  <ul>
  <li>Seongsook Choi
  <li>Christophe Ladroue
  </ul>",
  tr("Contact:"), " <a href='mailto:interactionalDiscourseLab@gmail.com'>", "Email", "</a>",
  "<p></p>",
  "<p>", tr("Translation:"), "Nora M. Basurto- Santos, Universidad Veracruzana, Mexico","</p>",
  "<h3>",tr("Instructions"), "</h3>",
  tr("Please visit"), "<a href='http://interactionalDiscourseLab.net'>http://interactionalDiscourseLab.net</a>",
  "<h3>Change log</h3>
  <ul>
  <li>27 May 2015: interactive timeline.</li>
  <li>24 May 2015: complete rewrite, packaging and published the code on github.</li>
  <li>02 Nov 2014: reports can be generated as pdf, Word document or html.</li>
  <li>05 Oct 2014: tag list now shows tag name instead of id.</li>
  </ul>
  ")
      })
  }

  )

  shiny::runApp(app)
}
