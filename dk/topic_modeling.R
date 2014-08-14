#===================================================================================================
# TOPIC MODELING
tag_matrix <- create_matrix(cbind(as.vector(tags$Body),as.vector(tags$Title)), language="english", removeNumbers=TRUE,
                            stemWords=TRUE, weighting=weightTf, removeSparseTerms=.998)
# container <- create_container(tag_matrix, tags$Tags, trainSize=1:(0.7*nrow(tags)),
#                               testSize=(0.7*nrow(tags)+1):nrow(tags), virgin=FALSE)
require(topicmodels)
require(RTextTools)
k <- length(unique(unlist(tagsList)))
lda <- LDA(tag_matrix, k)
# corpus <- Corpus(VectorSource(tags$Body))
# corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, tolower)
# corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, removeNumbers)
# corpus <- tm_map(corpus, removeWords, stopwords("english"))
# dictcorpus <- corpus
# corpus <- tm_map(corpus, stemDocument)
# inspect(corpus[1:3])
# corpus <- tm_map(corpus, stemCompletion, dictionary=dictcorpus)
# tdm <- TermDocumentMatrix(corpus, control = list(minWordLength = 1))

num.topics <- length(unique(tags$Tags))
params <- sample(c(-1, 1), num.topics, replace=TRUE)
slda.em(corpus, 172, vocab="english", num.e.iterations=10, num.m.iterations=4, alpha=1.0,
        eta=0.1, params, variance=0.25, logistic = FALSE, lambda = 1.0,
        method = "sLDA", trace = 0L)


