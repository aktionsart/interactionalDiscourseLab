context("Simulated meeting")

test_that("simulate meeting",{

  X <- simulateMeeting(seed = 42, nSpeakers = 4, nTurns = 10, nTags = 5)

  expectedSpeaker <- structure(c(1L, 3L, 2L, 2L, 1L, 3L, 3L, 2L, 1L, 2L),
                               .Label = c("Ava", "Isabella", "Thomas"),
                               class = "factor")


  expectedTag <- structure(c(1L, 1L, 1L, 5L, 4L, 5L, 2L, 5L, 5L, 3L),
                           .Label = c("challenge","explanation", "joke", "qualify", "repeat"),
                           class = "factor")

  expect_identical(X$speaker, expectedSpeaker)
  expect_identical(X$tag, expectedTag)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
context("count0")
test_that("count0",{
  X <- data.frame(f1 = factor(letters[1:10]), f2 = 1:10)
  X <- X[-(2:4),]

  E <- data.frame(f1 = factor(letters[1:10]), n = c(1,0,0,0,rep(1,6)))

  expect_identical(X %>% count0(f1) %>% as.data.frame, E)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
context("parsing text file")

test_that("parsing text file, all turns are tagged",{
  X <- simulateMeeting(seed = 43, nTurns = 10)
  filename <- "test_parse.txt"

  connection <- file(filename, "w")
  for(k in 1:nrow(X))
    cat(as.character(X$speaker[k]),"\t", as.character(X$speech[k]), "/", as.character(X$tag[k]), "/\n", file = connection, sep = "")
  close(connection)

  Y <- parses(filename) %>% cleanupParsed

  expect_identical(Y[-3], X[,c(1,3)])
  expect_identical(Y$turn, 1:10)
})

test_that("parsing text file, not all turns are tagged",{
  X <- simulateMeeting(seed = 44, nTurns = 10)
  filename <- "test_parse.txt"

  noTags <- c(1,5,7,8)
  connection <- file(filename, "w")
  for(k in 1:nrow(X)){
    if(k %in% noTags)
      cat(as.character(X$speaker[k]),"\t", as.character(X$speech[k]), "\n", file = connection, sep = "")
    else
      cat(as.character(X$speaker[k]),"\t", as.character(X$speech[k]), "/", as.character(X$tag[k]), "/\n", file = connection, sep = "")
  }
  close(connection)

  Y <- parses(filename) %>% cleanupParsed
  X$tag <- as.character(X$tag)
  X$tag[noTags] <- "NA"
  X$tag <- factor(X$tag)

  expect_identical(Y[,-3], X[,c(1,3)])
  expect_identical(Y$turn, 1:10)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
context("makeTurnTaking")

test_that("makeTurnTaking",{
  X <- simulateMeeting(nTurns=5, seed = 45) %>% select(-speech)  %>% cleanupParsed
  X <- X[-2,] # remove 1 row; tag sequence should not be used at this particular point

  # manually checked expected answer
  E <- structure(list(
                  speaker1 = structure(1:2, .Label = c("Charlie", "Sophie"), class = "factor"),
                  tag1     = structure(c(4L, 3L), .Label = c("agree", "confirmation", "initiate", "repeat"), class = "factor"),
                  speaker2 = structure(c(2L, 1L), .Label = c("Charlie", "Sophie"), class = "factor"),
                  tag2     = structure(3:4, .Label = c("agree", "confirmation", "initiate", "repeat"), class = "factor")),
            class = "data.frame", row.names = c(NA, -2L), .Names = c("speaker1", "tag1", "speaker2", "tag2"))

  expect_identical(makeTurnTaking(X), E)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
context("interactions")

test_that("tagSequences",{
  tags <- simulateMeeting(nTurns = 20, seed = 5) %>% select(-speech)  %>% cleanupParsed %>% makeTurnTaking %>% countTagSequence

  truth <- data.frame(
    tag1 = paste0("simulatedTag", c(1, 2, 2, 2, 3, 3, 5, 5, 5, 5)),
    tag2 = paste0("simulatedTag", c(2, 2, 3, 5, 2, 5, 1, 2, 3, 5)),
    n = c(1, 3, 1, 4, 1, 1, 1, 4, 1, 2),
    proportion = c(100.0, 37.5, 12.5, 50, 50, 50, 12.5, 50, 12.5, 25))

  expect_identical(as.character(tags$tag1), as.character(truth$tag1))
  expect_identical(as.character(tags$tag2), as.character(truth$tag2))
  expect_identical(as.character(tags$n), as.character(truth$n))
  expect_identical(as.character(tags$proportion), as.character(truth$proportion))
})

test_that("speakerSequences",{
  speakers <- simulateMeeting(nTurns = 20, seed = 55) %>% select(-speech)  %>% cleanupParsed %>% makeTurnTaking %>% countSpeakerSequence

  truth <- data.frame(
    speaker1 = paste0("simulatedSpeaker", c(1, 1, 1, 2, 2, 4, 4, 4)),
    speaker2 = paste0("simulatedSpeaker", c(1, 2, 4, 1, 4, 1, 2, 4)),
    n = c(1, 1, 4, 1, 1, 3, 1, 7),
    proportion = c(100/6, 100/6, 100*4/6, 50, 50, 100*3/11, 100/11, 100*7/11))

  expect_identical(as.character(speakers$speaker1), as.character(truth$speaker1))
  expect_identical(as.character(speakers$speaker2), as.character(truth$speaker2))
  expect_identical(as.character(speakers$n), as.character(truth$n))
  expect_identical(as.character(speakers$proportion), as.character(truth$proportion))
})
