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

################################
##        time line
################################

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