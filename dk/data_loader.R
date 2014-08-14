setwd("/Users/divikhanna/Desktop/sustainability.stackexchange.com.7z Folder/")
require(XML)
require(plyr)
require(stringr)
require(RTextTools)
require(tm)
require(lda)
require(igraph)
require(caret)
require(cluster)
require(psych)
#===================================================================================================
# READING XML
# datapostLinks <- xmlParse("PostLinks.xml", getDTD = F)
# postLinksXMLDF = ldply(xmlToList(datapostLinks))
# postLinksXMLDF = postLinksXMLDF[,-1]
# 
# databadges <- xmlParse("Badges.xml", getDTD = F)
# badgesXMLDF = ldply(xmlToList(databadges))
# badgesXMLDF = badgesXMLDF[,-1]
# 
# datavotes <- xmlParse("Votes.xml", getDTD = F)
# votesXMLDF = ldply(llply(xmlToList(datavotes), function(x) rbind.fill(data.frame(t(x)))))
# votesXMLDF = votesXMLDF[,-1]

dataposts <- xmlParse("Posts.xml", getDTD = F)
postsXMLDF = ldply(llply(xmlToList(dataposts), function(x) rbind.fill(data.frame(t(x)))))
postsXMLDF = postsXMLDF[,-1]

# datacomments <- xmlParse("Comments.xml", getDTD = F)
# commentsXMLDF = ldply(llply(xmlToList(datacomments), function(x) rbind.fill(data.frame(t(x)))))
# commentsXMLDF = commentsXMLDF[,-1]
# 
# datapostHistory <- xmlParse("PostHistory.xml", getDTD = F)
# postHistoryXMLDF = ldply(llply(xmlToList(datapostHistory), function(x) rbind.fill(data.frame(t(x)))))
# postHistoryXMLDF = postHistoryXMLDF[,-1]

datausers <- xmlParse("Users.xml", getDTD = F)
usersXMLDF = ldply(llply(xmlToList(datausers), function(x) rbind.fill(data.frame(t(x)))))
usersXMLDF = usersXMLDF[,-1]


