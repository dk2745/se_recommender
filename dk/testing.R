#===================================================================================================
# TESTING
setwd("/Users/divikhanna/Desktop/Spring/sustainability.stackexchange.com.7z Folder/")

datapostsNEW <- xmlParse("Posts.xml", getDTD = F)
postsXMLDFNEW = ldply(llply(xmlToList(datapostsNEW), function(x) rbind.fill(data.frame(t(x)))))
postsXMLDFNEW = postsXMLDFNEW[,-1]

# Subsetting questions of interest
begin <- as.numeric(as.character(tail(postsXMLDF$Id, 1)))+1
test <- (subset(postsXMLDFNEW, postsXMLDFNEW$Id %in% (begin:(begin+nrow(postsXMLDFNEW)))))

qtest <- na.omit(subset(test[test$PostTypeId == 1, ], 
                            select=c("Id", "Body", "Title", "Tags", "AnswerCount")))
qtest$Id <- as.numeric(as.character(qtest$Id))

atest <- na.omit(subset(test[test$PostTypeId == 2, ], 
                          select=c("ParentId", "CreationDate", "Score", "OwnerUserId")))
#answers$CreationDate <- strptime(paste(substr(as.character(answers$CreationDate),1,10), 
                                       substr(as.character(answers$CreationDate),12,100L)), format="%Y-%m-%d %H:%M:%OS")
atest$ParentId <- as.numeric(as.character(atest$ParentId))
atest$OwnerUserId <- as.numeric(as.character(atest$OwnerUserId))

testTags <- str_extract_all(qtest$Tags,"[a-z-]+")

clist <- list()
for(i in 1:length(testTags)) {
  for(j in 1:length(cTagHash)) {
    for(k in 1:length(testTags[[i]])) {
  if(testTags[[i]][k] %in% cTagHash[[j]][-1]) {clist <- c(clist, cTagHash[[j]][1]) }}}}


