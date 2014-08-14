#===================================================================================================
# RANKING ALGORITHM

users <- list()
freq <- list()
rank <- lapply(result, "[",-1)
rankFreq <- lapply(rank, table)
for(i in 1:length(rankFreq)) {
  users[[i]] <- as.numeric(names(unlist(rankFreq[[i]])))
  freq[[i]] <- unname(unlist(rankFreq[[i]])) }

a=subset(answers,select=c("OwnerUserId", "Score"))
b=aggregate(a,by=a["OwnerUserId"],FUN="sum")

model <- list()
for(i in 1:length(result)) {
  model[[i]] <- data.frame(OwnerUserId=users[[i]],Freq=freq[[i]])
  model[[i]]$Reputation <- answers[match(model[[i]]$OwnerUserId, answers$OwnerUserId),]$Reputation
  model[[i]]$Score <- b[match(model[[i]]$OwnerUserId, b$OwnerUserId),]$Score
  model[[i]] <- na.omit( model[[i]])
  model[[i]] <- subset(model[[i]],select=c("OwnerUserId","Freq.Freq","Reputation","Score")) }

# Ranking based on PCA Scores

for(i in 37:40) {
  a <- model[[i]][2:4]
#  if(length(a) == 3){next}
  pca <- principal(a, nfactors=3, rotate="none", covar=FALSE)
  #plot(pca[[i]]$values, type="b", ylab="Eigenvalues", 
  #     xlab="Component", lab=c(3,3,3))
  d <- principal(a, nfactors = 1, rotate = "varimax", scores = T)
pca.r <- as.data.frame(pca.r$scores)  
model[[i]]$Rank <- as.numeric(pca.r$PC1)
}
a=as.data.frame(scale(a))
plotmatrix(a, colour="lightblue") + geom_smooth(method="lm")

fit <- prcomp(a, scale=T)
PCbiplot(fit, colors=c("black", "red", "yellow"))
library(dplyr)

for(i in 1:12) {
model[[i]] <- arrange(model[[i]],desc(model[[i]]$Rank)) }

for(i in 36:41) {
model[[i]] <- model[[i]][order(model[[i]]$Rank,decreasing=TRUE),]
}


