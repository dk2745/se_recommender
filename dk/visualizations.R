# Reputation Histogram Sustainability
require(ggplot2)
usersXMLDF$Reputation <- as.numeric(usersXMLDF$Reputation)
ggplot(data = usersXMLDF) + geom_histogram(aes(x = usersXMLDF$Reputation))
ggplot(data = usersXMLDF) + geom_density(aes(x = usersXMLDF$Reputation))

require(stringr)
require(ggthemes)
usersXMLDF$CreationDate <- as.character(usersXMLDF$CreationDate)
usersXMLDF$CreationDate <- str_sub(usersXMLDF$CreationDate, 1, 10)
usersXMLDF$CreationDate <- as.Date(usersXMLDF$CreationDate)
ggplot(usersXMLDF, aes(y = Reputation, x = CreationDate)) + geom_point() + theme_tufte()

usersXMLDF$LastAccessDate <- as.character(usersXMLDF$LastAccessDate)
usersXMLDF$LastAccessDate <- str_sub(usersXMLDF$LastAccessDate, 1, 10)
usersXMLDF$LastAccessDate <- as.Date(usersXMLDF$LastAccessDate)
ggplot(usersXMLDF, aes(y = Reputation, x = LastAccessDate)) + geom_point() + theme_tufte()

#==================================
# Score of posts and number of comments
postsXMLDF$Score = as.numeric(postsXMLDF$Score)
postsXMLDF$CommentCount = as.numeric(postsXMLDF$CommentCount)
scoreComment <- ggplot(postsXMLDF[postsXMLDF$Score>0 & postsXMLDF$CommentCount>0,],
          aes(x=Score,y=CommentCount))
# hexagon binning
require(hexbin)
scoreComment + geom_hex() + scale_x_log10() + scale_y_log10() + scale_fill_continuous(trans="log10")

#==================================
# Score of posts and view-count
postsXMLDF$Score = as.numeric(postsXMLDF$Score)
postsXMLDF$ViewCount = as.numeric(postsXMLDF$ViewCount)
scoreViewcount <- ggplot(postsXMLDF[postsXMLDF$Score>0 & postsXMLDF$ViewCount>0,],
                       aes(x=Score,y=ViewCount))
# hexagon binning
require(hexbin)
scoreViewcount + geom_hex() + scale_x_log10() + scale_y_log10() + scale_fill_continuous(trans="log10")

#==================================
# Upvotes and reputation of user
usersXMLDF$UpVotes = as.numeric(usersXMLDF$UpVotes)
usersXMLDF$Reputation = as.numeric(usersXMLDF$Reputation)
upvotesReputation <- ggplot(usersXMLDF[usersXMLDF$UpVotes>0 & usersXMLDF$Reputation>0,],
                         aes(x=UpVotes,y=Reputation))
# hexagon binning
require(hexbin)
upvotesReputation + geom_hex() + scale_x_log10() + scale_y_log10() + scale_fill_continuous(trans="log10")

#==================================
# Cluster Plots for entire network
require(ggplot2)
pairs <- data.frame()
for (i in 1:length(tagsList)){ 
  pairs <- c(pairs, t <- if(length(tagsList[[i]])>=2) combn(tagsList[[i]],2,simplify=FALSE))}
matrix <- do.call(rbind,pairs)
graph <- graph.data.frame(matrix,directed=FALSE, vertices=unique(tags$Tags))
E(graph)$width <- count.multiple(graph)
igraph.options(vertex.label=NA, vertex.shape="circle",vertex.size=4)
plot.igraph(graph, layout=layout.kamada.kawai, edge.curved=FALSE)
wc <-  infomap.community(graph)
modularity(wc)
membership(wc)
plot(wc, graph)
plot(graph, vertex.color=membership(wc))

large <- list()
for(i in 1:length(unique(communityTagHash$V2))) {
  if(length(cTagHash[[i]])>5){large <- c(large, cTagHash[[i]][1])} }

l <- layout.kamada.kawai(graph, niter=1000, coolexp=0.5)
d <- data.frame(l)
names(d) <- c("x", "y")
d$cluster <- as.character(factor(wc$membership))
large <- as.character(sapply(large, "[[", 1))
d <- subset(d[d$cluster %in% large, ])
p <- ggplot(d, aes(x=x, y=y, color=cluster))
(pq <- p + geom_point())

edgelist <- get.edgelist(graph, names=FALSE)
edges <- data.frame(d[edgelist[,1],c("x", "y")],
                    d[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")
(pq <- pq + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2),
  data=edges, size=0.25, color="grey", alpha=1/3))

## putting it all together...
p <- ggplot(d, aes(x=x, y=y, color=cluster))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2),
  data=edges, size=0.25, color="white", alpha=1/3) +
  ## note that here I add a border to the points
  geom_point(color="grey20", aes(fill=cluster), shape=21, size=4) +
  scale_fill_discrete(labels=labels) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill="black"),
    axis.line = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(colour = F, fill = "black"),
    legend.key = element_rect(fill = "black", colour = F),
    legend.title = element_text(color="white"),
    legend.text = element_text(color="white")
  ) +
  ## changing size of points in legend
  guides(fill = guide_legend(override.aes = list(size=5)))
pq


