library(jsonlite)
library(stringr)
library(tm)
library(randomForest)
training <- fromJSON("./Cooking/train.json", flatten=TRUE)
testing <- fromJSON("./Cooking/test.json", flatten=TRUE)

library(plyr)
training$cuisine <- as.factor(training$cuisine)
d <- ddply(training, "cuisine", summarise, med=median(nchar(ingredients)))

g <- ggplot(d, aes(x=cuisine, y=med))

g+geom_bar(stat="identity", fill=d$med)

yummy <- function(x) {
  subing <- subset(training, cuisine == x)
    ingredients <- Corpus(VectorSource(subing$ingredients))
  dtmi <- DocumentTermMatrix(ingredients)
    n <- round(nrow(subing) * 0.07)
  list1 <- paste0(findFreqTerms(dtmi, n), collapse="|")
  assign(paste(x), list1, envir=globalenv())
}

cuisines <- unique(training$cuisine)

lapply(cuisines, FUN=yummy)

yucky <- function(x,y) {
  subing <- subset(training, cuisine == x)
  ingredients <- Corpus(VectorSource(subing$ingredients))
  dtmi <- DocumentTermMatrix(ingredients)
    list1 <- paste0(findFreqTerms(dtmi, 0,1), collapse="|")
  assign(paste0(x,y), list1, envir=globalenv())
}

lapply(cuisines, y="f", FUN=yucky)

adding <- function(x, y){
  str_count(x$ingredients, y)
}

training$CN <- adding(training, chinese); testing$CN <- adding(testing, chinese)
training$IT <- adding(training, italian); testing$IT <- adding(testing, italian)
training$IR <- adding(training, irish); testing$IR <- adding(testing, irish)
training$BT <- adding(training, british); testing$BT <- adding(testing, british)
training$BR <- adding(training, brazilian); testing$BR <- adding(testing, brazilian)
training$JA <- adding(training, jamaican); testing$JA <- adding(testing, jamaican)
training$CA <- adding(training, cajun_creole); testing$CA <- adding(testing, cajun_creole)
training$FI <- adding(training, filipino); testing$FI <- adding(testing, filipino)
training$FR <- adding(training, french); testing$FR <- adding(testing, french)
training$GR <- adding(training, greek); testing$GR <- adding(testing, greek)
training$IN <- adding(training, indian); testing$IN <- adding(testing, indian)
training$JA <- adding(training, japanese); testing$JA <- adding(testing, japanese)
training$KO <- adding(training, korean); testing$KO <- adding(testing, korean)
training$ME <- adding(training, mexican); testing$ME <- adding(testing, mexican)
training$MO <- adding(training, moroccan); testing$MO <- adding(testing, moroccan)
training$SO <- adding(training, southern_us); testing$SO <- adding(testing, southern_us)
training$SP <- adding(training, spanish); testing$SP <- adding(testing, spanish)
training$TH <- adding(training, thai); testing$TH <- adding(testing, thai)
training$VI <- adding(training, vietnamese); testing$VI <- adding(testing, vietnamese)
training$RU <- adding(training, russian); testing$RU <- adding(testing, russian)

training$CN1 <- adding(training, chinesef); testing$CN1 <- adding(testing, chinesef)
italianf <- gsub("[(]", "", italianf)
training$IT1 <- adding(training, italianf); testing$IT1 <- adding(testing, italianf)
training$IR1 <- adding(training, irishf); testing$IR1 <- adding(testing, irishf)
training$BT1 <- adding(training, britishf); testing$BT1 <- adding(testing, britishf)
training$BR1 <- adding(training, brazilianf); testing$BR1 <- adding(testing, brazilianf)
training$JA1 <- adding(training, jamaicanf); testing$JA1 <- adding(testing, jamaicanf)
cajun_creolef <- gsub("[)]", "", cajun_creolef)
training$CA1 <- adding(training, cajun_creolef); testing$CA1 <- adding(testing, cajun_creolef)
training$FI1 <- adding(training, filipinof); testing$FI1 <- adding(testing, filipinof)
frenchf <- gsub("[)]", "", frenchf)
training$FR1 <- adding(training, frenchf); testing$FR1 <- adding(testing, frenchf)
training$GR1 <- adding(training, greekf); testing$GR1 <- adding(testing, greekf)
indianf <- gsub("[(]", "", indianf)
training$IN1 <- adding(training, indianf); testing$IN1 <- adding(testing, indianf)
training$JA1 <- adding(training, japanesef); testing$JA1 <- adding(testing, japanesef)
training$KO1 <- adding(training, koreanf); testing$KO1 <- adding(testing, koreanf)
mexicanf <- gsub("[(]", "", mexicanf)
training$ME1 <- adding(training, mexicanf); testing$ME1 <- adding(testing, mexicanf)
training$MO1 <- adding(training, moroccanf); testing$MO1 <- adding(testing, moroccanf)
training$SO1 <- adding(training, southern_usf); testing$SO1 <- adding(testing, southern_usf)
training$SP1 <- adding(training, spanishf); testing$SP1 <- adding(testing, spanishf)
thaif <- gsub("[)]", "", thaif)
training$TH1 <- adding(training, thaif); testing$TH1 <- adding(testing, thaif)
training$VI1 <- adding(training, vietnamesef); testing$VI1 <- adding(testing, vietnamesef)
training$RU1 <- adding(training, russianf); testing$RU1 <- adding(testing, russianf)

training$chinese <- adding(training, "chinese"); testing$chinese <- adding(testing, "chinese")
training$italian <- adding(training, "italian"); testing$italian <- adding(testing, "italian")
training$irish <- adding(training, "irish"); testing$irish <- adding(testing, "irish")
training$british <- adding(training, "british"); testing$british <- adding(testing, "british")
training$brazilian <- adding(training, "brazilian"); testing$brazilian <- adding(testing, "brazilian")
training$jamaican <- adding(training, "jamaican"); testing$jamaican <- adding(testing, "jamaican")
training$cajun <- adding(training, "cajun"); testing$cajun <- adding(testing, "cajun")
training$filipino <- adding(training, "filipino"); testing$filipino <- adding(testing, "filipino")
training$french <- adding(training, "french"); testing$french <- adding(testing, "french")
training$greek <- adding(training, "greek"); testing$greek <- adding(testing, "greek")
training$indian <- adding(training, "indian"); testing$indian <- adding(testing, "indian")
training$japanese <- adding(training, "japanese"); testing$japanese <- adding(testing, "japanese")
training$korean <- adding(training, "korean"); testing$korean <- adding(testing, "korean")
training$mexican <- adding(training, "mexican"); testing$mexican <- adding(testing, "mexican")
training$moroccan <- adding(training, "moroccan"); testing$moroccan <- adding(testing, "moroccan")
training$southern <- adding(training, "southern"); testing$southern <- adding(testing, "southern")
training$spanish <- adding(training, "spanish"); testing$spanish <- adding(testing, "spanish")
training$thai <- adding(training, "thai"); testing$thai <- adding(testing, "thai")
training$viet <- adding(training, "vietnamese"); testing$viet <- adding(testing, "vietnamese")
training$russian <- adding(training, "russian"); testing$russian <- adding(testing, "russian")

training$cuisine <- as.factor(training$cuisine)
training$count1 <- nchar(training$ingredients)
testing$count1 <- nchar(testing$ingredients)

library(caret)
inTrain <- createDataPartition(y=training$cuisine, p=.70, list=FALSE)
train1 <- training[inTrain,]
test1 <- training[-inTrain,]

train1 <- train1[ ,c(2, 4:ncol(train1))]
ModelRF <- randomForest(cuisine ~ ., data=train1)
ModelXBG <- train(cuisine ~ ., data=train1, method="xgbTree")

predictXGB <- predict(ModelXBG, newdata=test1)
confusionMatrix(predictXGB, test1$cuisine)

predictRF <- predict(ModelRF, newdata=test1)
confusionMatrix(predictRF, test1$cuisine)

testing$cuisine <- predict(ModelXBG, testing)
submission <- subset(testing, select=c(id, cuisine))
write.csv(submission, "submissionCB.csv", row.names=FALSE)
