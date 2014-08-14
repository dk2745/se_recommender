#===================================================================================================
# QUESTIONS-ANSWERS

questions <- na.omit(subset(postsXMLDF[postsXMLDF$PostTypeId == 1, ], 
                    select=c("Id", "Body", "Title", "Tags", "AnswerCount")))
questions$Body <- gsub("<(.|\n\r)*?>","",questions$Body)
questions$Title <- gsub("<(.|\n\r)*?>","",questions$Title)
questions$Id <- as.numeric(as.character(questions$Id))

answers <- na.omit(subset(postsXMLDF[postsXMLDF$PostTypeId == 2, ], 
                          select=c("ParentId", "CreationDate", "Score", "OwnerUserId")))
answers$Score <- as.numeric(as.character(answers$Score))
answers$Reputation <- as.numeric(as.character(usersXMLDF$Reputation[match(answers$OwnerUserId, usersXMLDF$Id)]))
answers$CreationDate <- strptime(paste(substr(as.character(answers$CreationDate),1,10), 
                      substr(as.character(answers$CreationDate),12,100L)), format="%Y-%m-%d %H:%M:%OS")
answers$ParentId <- as.numeric(as.character(answers$ParentId))
answers$OwnerUserId <- as.numeric(as.character(answers$OwnerUserId))

# Creates List: Question Id | OwnerUser Id 
quesHash <- list()
for(i in 1:length(unique(answers$ParentId))) {
  inter <- subset(answers[answers$ParentId == unique(answers$ParentId)[i], ])
  inter <- inter[order(inter$CreationDate, decreasing=TRUE),]
  quesHash <- c(quesHash,list(c(unique(answers$ParentId)[i],inter$OwnerUserId)))}

tags <- data.frame()
tagsList <- str_extract_all(questions$Tags,"[a-z-]+")
for(i in 1:length(tagsList)) {
  tags <- rbind(tags, data.frame(Id=questions$Id[i], Body=questions$Body[i], Title=questions$Title[i],
                                 Tags=tagsList[[i]]))}
  tags$Tags <- as.character(tags$Tags)
  tags$Id <- as.numeric(as.character(tags$Id))

# Creates List: Tags | Question Id 
tagHash <- list()
for(i in 1:length(unique(tags$Tags))) {
  inter <- subset(tags[tags$Tags == unique(tags$Tags)[i], ])
  tagHash <- c(tagHash,list(c(unique(tags$Tags)[i],inter$Id)))}

# Creates List: Tags | OwnerUser Id
mainHash <- list()
for(i in 1:length(unique(tags$Tags))) {
  inter <- subset(answers[answers$ParentId %in% tagHash[[i]][-1], ])
  inter <- inter[order(inter$CreationDate, decreasing=TRUE),]
  mainHash <- c(mainHash,list(c(tagHash[[i]][1],inter$OwnerUserId)))}


