
response <- read.csv('sandbox/DataSurveyResponses.csv', colClasses = c('logical',rep('character', 14)))
response.out <- response[response$IsQuestion,]
survey <- list()
survey$question <- c()
survey$response <- list()
for(i in 1:nrow(response.out)){
  a <- as.character((response.out[i,4:15]))
  survey$questionLong[i] <- response.out$Text[i]
  survey$questionShort[i] <- paste('q',response.out$Question[i],sep='')
  survey$response[[i]] <- a[a!='']
}


qNames <- c('q1','q2','q2other', 'q3', 'q4', paste('q5', letters[1:7], sep=''),
            'q5other', 'q6name', 'q6answer', 'q7',
            paste('q8', rep(letters[1:7], each = 3), 1:3, sep = ''), 'q8other',
            paste('q9', rep(letters[1:9], each = 2), 1:2, sep = ''), 'q9other',
            'q10', 'q11')

surveyLookup <- data.frame(qname = qNames, qShort = qNames, qLong = survey$questionLong)

dummyResponse <- as.data.frame(matrix(nrow=1, ncol=length(qNames)))


generateDummyResponse <- function(responses){
  if(length(responses)<2){
    if(responses=='Free Response'){
      outResponse <- NA
    }else{
      outResponse <- responses 
    }
  }else{
    outResponse <- sample(responses, 1)
  }
  return(outResponse)
}

dummyResponse <- t(replicate(1000, sapply(survey$response, generateDummyResponse)))
dummyResponse <- as.data.frame(dummyResponse)
names(dummyResponse) <- qNames

write.csv(dummyResponse, 'sandbox/dummySurveyResponses.csv', quote=F, row.names=F)




