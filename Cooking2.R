library(jsonlite)
library(tm)
library(data.table)
library(Matrix)
library(caret)
library(SnowballC)
library(xgboost)
library(Ckmeans.1d.dp)

#- load data files and flatten
train_raw  <- fromJSON("./Cooking/train.json", flatten = TRUE)
submit_raw <- fromJSON("./Cooking/test.json", flatten = TRUE)

#- pre-process the ingredients (basic)
train_raw$ingredients <- lapply(train_raw$ingredients, FUN=tolower)
train_raw$ingredients <- lapply(train_raw$ingredients, FUN=function(x) gsub("-", "_", x)) # allow dash e.g. "low-fat"
train_raw$ingredients <- lapply(train_raw$ingredients, FUN=function(x) gsub("[^a-z0-9_ ]", "", x)) # allow regular character and spaces

submit_raw$ingredients <- lapply(submit_raw$ingredients, FUN=tolower)
submit_raw$ingredients <- lapply(submit_raw$ingredients, FUN=function(x) gsub("-", "_", x)) # allow dash e.g. "low-fat"
submit_raw$ingredients <- lapply(submit_raw$ingredients, FUN=function(x) gsub("[^a-z0-9_ ]", "", x)) # allow regular character and spaces

#- create a matrix of ingredients in both the TRAIN and SUBMIT set
c_ingredients <- c(Corpus(VectorSource(train_raw$ingredients)), Corpus(VectorSource(submit_raw$ingredients)))


#- create simple DTM
c_ingredientsDTM <- DocumentTermMatrix(c_ingredients)
c_ingredientsDTM <- removeSparseTerms(c_ingredientsDTM, 1-3/nrow(c_ingredientsDTM)) # remove if < 3 occurances
c_ingredientsDTM <- as.data.frame(as.matrix(c_ingredientsDTM))

cuisines <- unique(train_raw$cuisine)

yucky <- function(x,y) {
  subing <- subset(train_raw, cuisine == x)
  ingredients <- Corpus(VectorSource(subing$ingredients))
  dtmi <- DocumentTermMatrix(ingredients)
  list1 <- paste0(findFreqTerms(dtmi, 0,1), collapse="|")
  assign(paste0(x,y), list1, envir=globalenv())
}

lapply(cuisines, y="f", FUN=yucky)

adding <- function(x, y){
  str_count(x$ingredients, y)
}

c_ingredientsDTM$CN1 <- adding(c_ingredientsDTM, chinesef)
italianf <- gsub("[(]", "", italianf)
c_ingredientsDTM$IT1 <- adding(c_ingredientsDTM, italianf)
c_ingredientsDTM$IR1 <- adding(c_ingredientsDTM, irishf)
c_ingredientsDTM$BT1 <- adding(c_ingredientsDTM, britishf)
c_ingredientsDTM$BR1 <- adding(c_ingredientsDTM, brazilianf)
c_ingredientsDTM$JA1 <- adding(c_ingredientsDTM, jamaicanf)
cajun_creolef <- gsub("[)]", "", cajun_creolef)
c_ingredientsDTM$CA1 <- adding(c_ingredientsDTM, cajun_creolef)
c_ingredientsDTM$FI1 <- adding(c_ingredientsDTM, filipinof)
frenchf <- gsub("[)]", "", frenchf)
c_ingredientsDTM$FR1 <- adding(c_ingredientsDTM, frenchf)
c_ingredientsDTM$GR1 <- adding(c_ingredientsDTM, greekf)
indianf <- gsub("[(]", "", indianf)
c_ingredientsDTM$IN1 <- adding(c_ingredientsDTM, indianf)
c_ingredientsDTM$JA1 <- adding(c_ingredientsDTM, japanesef)
c_ingredientsDTM$KO1 <- adding(c_ingredientsDTM, koreanf)
mexicanf <- gsub("[(]", "", mexicanf)
c_ingredientsDTM$ME1 <- adding(c_ingredientsDTM, mexicanf)
c_ingredientsDTM$MO1 <- adding(c_ingredientsDTM, moroccanf)
c_ingredientsDTM$SO1 <- adding(c_ingredientsDTM, southern_usf)
c_ingredientsDTM$SP1 <- adding(c_ingredientsDTM, spanishf)
thaif <- gsub("[)]", "", thaif)
c_ingredientsDTM$TH1 <- adding(c_ingredientsDTM, thaif)
c_ingredientsDTM$VI1 <- adding(c_ingredientsDTM, vietnamesef)
c_ingredientsDTM$RU1 <- adding(c_ingredientsDTM, russianf)

c_ingredientsDTM$ingredients_count  <- rowSums(c_ingredientsDTM) # simple count of ingredients per receipe

#- add cuisine for TRAIN set, default to "italian" for the SUBMIT set
c_ingredientsDTM$cuisine <- as.factor(c(train_raw$cuisine, rep("italian", nrow(submit_raw))))

#- split the DTM into TRAIN and SUBMIT sets
dtm_train  <- c_ingredientsDTM[1:nrow(train_raw), ]
dtm_submit <- c_ingredientsDTM[-(1:nrow(train_raw)), ]


#- Model 4: xgboost (single model for all cuisine types)
#- prepare the spare matrix (note: feature index in xgboost starts from 0)
xgbmat     <- xgb.DMatrix(Matrix(data.matrix(dtm_train[, !colnames(dtm_train) %in% c("cuisine")])), label=as.numeric(dtm_train$cuisine)-1)

#- train our multiclass classification model using softmax
xgb        <- xgboost(xgbmat, max.depth = 3, eta = 0.3, nround = 2, objective = "multi:softmax", num_class = 20)

#- predict on the SUBMIT set and change cuisine back to string
xgb.submit      <- predict(xgb, newdata = data.matrix(dtm_submit[, !colnames(dtm_submit) %in% c("cuisine")]))

xgb.submit.text <- levels(dtm_train$cuisine)[xgb.submit+1]


#- load sample submission file to use as a template
sample_sub <- read.csv('../input/sample_submission.csv')

#- build and write the submission file
submit_match   <- cbind(as.data.frame(submit_raw$id), as.data.frame(xgb.submit.text))
colnames(submit_match) <- c("id", "cuisine")
submit_match   <- data.table(submit_match, key="id")
submit_cuisine <- submit_match[id==sample_sub$id, as.matrix(cuisine)]

submission <- data.frame(id = sample_sub$id, cuisine = submit_cuisine)
write.csv(submission, file = 'xgboost_multiclass.csv', row.names=F, quote=F)

# plot the most important features
names <- colnames(dtm_train[, !colnames(dtm_train) %in% c("cuisine")])
importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:30,])

