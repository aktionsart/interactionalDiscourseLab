################################
##        tags and speakers in ggplot2, for the report
################################

makeSpeakerParticipationGgplot2 <- function(speakers){

  # Speaker participation
  p <- ggplot(speakers) +
    geom_bar(aes(x = speaker, y = n ), position ="dodge", stat = "identity") +
    xlab("") + ylab("turn count") +
    ggtitle("Speaker Participation") + theme_bw()
  p
}

makeTagBySpeakerNormalisedGgplot2 <- function(tags){
  # tag by speaker, proportion
  p <- ggplot(tags) +
    geom_bar(aes(x = speaker, y = proportion, fill = tag ), position ="dodge", stat = "identity", colour = "black") +
    scale_fill_brewer(type="qual",palette = 6) + xlab("") +ylab("Proportion") +
    ggtitle("Speaker's use of each tag ") + theme_bw()
  p

}

makeSpeakerByTagNormalisedGgplot2 <- function(speakers) {
  p <- ggplot(speakers) +
    geom_bar(aes(x = tag, y= proportion, fill = speaker ), position ="dodge", stat = "identity", colour = "black") +
    scale_fill_brewer(type="qual",palette = 6) + xlab("") + ylab("proportion") + theme_bw() + ggtitle("speaker for each tag")
  p
}


################################
##        Interactions
################################

plotNetwork <- function(edgeList, layout, vertex.size = 30, title = ""){
  if(length(vertex.size)>1){
    minSize <- 20
    maxSize <- 100
    vertex.size = ( (maxSize-minSize) / sqrt(100)) * sqrt(vertex.size) + minSize
  }

  plot(graph.data.frame(edgeList),
       edge.width = edgeList$proportion/5,
       edge.label = format(edgeList$proportion,digits=2),
       edge.label.cex = 0.95,
       edge.curved = 0.2,
       edge.color = "#AAAAAA99",
       main = title,
       vertex.size = vertex.size,
       layout = layout)
}

plotSequence <- function(itemSequence, itemCount, title){

  # setting up the layout
  edgeList <- itemSequence
  g <- graph.data.frame(edgeList)
  myLayout <- layout.circle(g)
  nameSequence <- V(g)$name

  vertex.size <- itemCount
  vertex.size <- 100*vertex.size/sum(vertex.size)

  plotNetwork(
    itemSequence,
    myLayout,
    vertex.size = vertex.size,
    title = title)
}

interactionMatrix <- function(u){ # u is from makeTurnTaking()
  # top and secondary levels
  explode <- stringr::str_split_fixed(u$tag1, ",", n = 2)
  u$TL1 <- explode[,1]
  u$SL1 <- explode[,2]

  # top and secondary levels
  explode <- stringr::str_split_fixed(u$tag2, ",", n = 2)
  u$TL2 <- explode[,1]
  u$SL2 <- explode[,2]

  X <- u %>% dplyr::filter(TL1 == TL2, SL1 !="", SL2 !="") %>% count(TL1, SL1, SL2) %>% group_by(TL1,SL1) %>% mutate(freq = n/sum(n))
  if(nrow(X) == 0)
    return(NULL)

  ggplot(X) + geom_tile(aes(x=SL2, y = SL1, fill = freq)) +
    facet_wrap(~TL1, nrow = 2, scales = "free") + xlab("") + ylab("") +
    scale_fill_gradient2()
}

################################
##        time line
################################

plotTimelineSlider <- function(filteredData, n, startingPoint, windowSize){
  themeBlank <- theme(axis.line=element_blank(),
                      axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      legend.position="none",
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),
                      plot.background=element_blank())

  s <- startingPoint
  w <- min( windowSize, n - startingPoint)


  location <- data.frame(xmin = (s - 1)/n * w + s,
                         xmax = (s + w)/n * w + s,
                         ymin = 0.90,
                         ymax = 0.95)

  whole <- data.frame(xmin = s,
                      xmax = s + w,
                      ymin = 0.90,
                      ymax = 0.95)

  ggplot(filteredData %>% dplyr::filter(s <= turn & turn < s + w)) +
    geom_rect(aes(xmin = turn , xmax = turn + 1, fill = factor(speaker), ymin = 1, ymax = 2),   colour = NA) +
    geom_text(aes(x = turn + 0.5, y = 1.25, label = speaker),angle = 90) +
    geom_rect(aes(xmin = turn, xmax = turn + 1, fill = factor(tag), ymin = 2, ymax = 3),   colour = NA) +
    geom_text(aes(x = turn + 0.5, y = 2.25, label = tag), angle = 90) +
    geom_rect(data = whole, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), fill = "white", colour="black") +
    geom_rect(data = location, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), fill = "black") +
    themeBlank


}

plotTimelineWhole <- function(filteredData, totalTurns){
  themeBlank <- theme(axis.line=element_blank(),
                      axis.text.x=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank(),
                      legend.position="none",
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank(),
                      plot.background=element_blank())
  ggplot(filteredData) +
    geom_rect(aes(xmin = turn , xmax = turn + 1, fill = factor(speaker), ymin = 1, ymax = 2),   colour = NA) +
    geom_rect(aes(xmin = turn, xmax = turn + 1, fill = factor(tag), ymin = 2, ymax = 3),   colour = NA) +
    geom_rect(data = data.frame(xmin = 1, xmax = totalTurns + 1 , ymin = 1, ymax = 3), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), colour="black", fill = NA)+
    themeBlank
}

plotTimeline <- function(parsed, windowSize = 30, title = "Timeline"){
  index <- 1:nrow(parsed)

  parsed$x <- (index -1)%% windowSize
  parsed$y <- parsed$period + windowSize * ((index -1)%/% windowSize)

  p <- ggplot(parsed, aes(x = x,  y = y)) +
    geom_path(aes(group = period ), colour='black') +
    geom_point(aes( shape = speaker, colour = tag), size=5) +
    scale_colour_brewer(type='qual',palette=6) +
    xlab("Turns") + ylab("") + theme_bw() +
    ggtitle(title)
  p <- p + theme(axis.title.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks.y=element_blank())
  p
}

plotTimeline2 <- function(parsed, windowSize = 30, title = "Linear Timeline"){
  factors <- c(as.character(unique(parsed$speaker)),as.character(unique(parsed$tag)))
  X <-data.frame(
    index = rep(1:nrow(parsed), 2),
    period = rep(parsed$period, 2),
    speakerTag = c(sort(as.character(parsed$speaker)), sort(as.character(parsed$tag))),
    flag = rep(c(2,1), each = nrow(parsed)))

  X$speakerTag <- factor(X$speakerTag, factors)

  p <- ggplot(X) +
    geom_rect(aes(xmin = index, xmax = index + 1, ymin = flag-1, ymax = flag, fill= speakerTag), colour = 'gray') +
    geom_vline(  xintercept= which(diff(subset(X,flag==1)$period)!=0)+1) +
    scale_fill_brewer(type='qual',palette=3, guide = guide_legend("Speaker / Tag")) +
    theme(axis.text.y=element_blank(), axis.ticks.y = element_blank()) +
    ggtitle(title)
  p
}
