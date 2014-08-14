#===================================================================================================
# CLUSTERING

pairs <- data.frame()
for (i in 1:length(tagsList)){ 
  pairs <- c(pairs, t <- if(length(tagsList[[i]])>=2) combn(tagsList[[i]],2,simplify=FALSE))}
matrix <- do.call(rbind,pairs)
graph <- graph.data.frame(matrix,directed=FALSE, vertices=unique(tags$Tags))
E(graph)$width <- count.multiple(graph)
igraph.options(vertex.label=NA, vertex.shape="circle",vertex.size=4)
plot.igraph(graph, layout=layout.fruchterman.reingold, edge.curved=FALSE)
wc <-  infomap.community(graph)
modularity(wc)
membership(wc)
plot(wc, graph)
plot(graph, vertex.color=membership(wc))

communityTagHash <- as.data.frame(membership(wc))
names(communityTagHash) <- c("Community")
communityTagHash$Community <- as.numeric(as.character(communityTagHash$Community))
names <- as.data.frame(row.names(communityTagHash))
names(names) <- c("Tags")
names$Tags <- as.character(names$Tags)
communityTagHash <- (cbind(names$Tags, communityTagHash$Community))
communityTagHash <- as.data.frame(communityTagHash)
communityTagHash$V1 <- as.character(communityTagHash$V1)
communityTagHash$V2 <- as.numeric(as.character((communityTagHash$V2)))

# Creates List: Community No | Tags
cTagHash <- list()
for(i in 1:length(unique(communityTagHash$V2))) {
  inter <- subset(communityTagHash[communityTagHash$V2 == unique(communityTagHash$V2)[i], ])
  cTagHash <- c(cTagHash,list(c(unique(communityTagHash$V2)[i],inter$V1)))}

# Creates List: Community No | OwnerUserId
cUserHash <- list()
for(i in 1:length(communityTagHash$V1)) {
  cUserHash <- c(cUserHash,list(c(communityTagHash$V2[i], mainHash[[i]][-1])))}

# Create Unique Community No | All OwnerUserId===== This is what we need to validate
first_elems <- sapply(cUserHash, "[", 1)
(first_elems <- as.factor(first_elems)) 
(group <- split(cUserHash, first_elems))
(result <- lapply(group, function(x) {
  c(x[[1]][1], unlist(lapply(x, "[", -1))) }))


