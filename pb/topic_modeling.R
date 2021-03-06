# Ref: http://journal.r-project.org/archive/2013-1/collingwood-jurka-boydstun-etal.pdf

require(RTextTools)
tagsSub <- subset(postsXMLDF, postsXMLDF$PostTypeId==1)
# Make multiple copies of questions with each individual tag
View(tagsSub)
rawPosts <- gsub("<(.|\n)*?>","",tagsSub$Body)
rawTitle <- gsub("<(.|\n)*?>","",tagsSub$Title)
rawTags <- gsub("[<>]"," ",tagsSub$Tags)
#convPosts <- iconv(tagsSub$Body, to = "utf-8") In case we need to explicily convert to utf-8

doc_matrix <- create_matrix(cbind(as.vector(rawPosts),as.vector(rawTitle)), language="english", removeNumbers=TRUE,
                            stemWords=TRUE, weighting=weightTf, removeSparseTerms=.998)

#===== Error: Can't work with multiple labels, still looking at it
container <- create_container(doc_matrix, rawTags, trainSize=1:(0.8*nrow(tagsSub)),
                              testSize=(0.8*nrow(tagsSub)+1):nrow(tagsSub), virgin=FALSE)

#======= Training Models
SVM <- train_model(corpus,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

#======= Classifying data
SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)


#======= Testing algorithm accuracy
analytics <- create_analytics(container,
                              cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
                                    BOOSTING_CLASSIFY, BAGGING_CLASSIFY,
                                    RF_CLASSIFY, GLMNET_CLASSIFY,
                                    NNET_CLASSIFY, TREE_CLASSIFY,
                                    MAXENT_CLASSIFY))

summary(analytics)
# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary
