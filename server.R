library(shiny)
library(shinyalert)
library(DT)

glossary <- read.csv('glossary_shinysurvey.csv', colClasses = rep('character', 2))

server <- function(input, output, session){
  
  response <- reactiveValues()
  
  observeEvent(input$startSurvey, {
    updateTabItems(session, 'sidebarMenu', selected = 'tabBasic')
  })
  
  observeEvent(input$submitBasic, {

    updateTabItems(session, 'sidebarMenu', selected = 'tabDataUse')
  })
  
  observeEvent(input$submitDataUse, {

    updateTabItems(session, 'sidebarMenu', selected = 'tabDataSkills')
  })
  
  observeEvent(input$searchButton,{
    ind <- grep(input$searchTerm, glossary$term)
    strTitle <- "<b>Glossary</b>"
    if(length(ind)==0){
      strOut <- 'No terms matched your search'
    }else{
      if(length(ind)>1){
        strOut <- c()
        for(i in 1:length(ind)){
          strng <- paste("<b>", glossary$term[i],":</b> ",glossary$definition[i],'<br><br>',sep='')
          strOut <- paste(strOut,strng,sep="")
        }
      }else{
        strOut <- paste("<b>", glossary$term[ind],":</b> ",glossary$definition[ind],sep='')
      }
    }
    shinyalert(title = strTitle, text = strOut, html=TRUE)
  })
  
  observeEvent(input$submitResponse, {
    
    qNames <- c('q1A','q2A','q3A',
                paste('q1B',q2Bvalues,sep='_'), 'q1Bother',
                paste('q2B',letters[grep('[a-g]',letters)], sep=''),
                'q3B','q1C',
                paste('q2C',q2Cvalues,sep='_'), 'q2Cother',
                paste('q3C', q3Cchoices, sep='_'),
                paste('q4C', q3Cchoices, sep='_'),
                'q5C', 'submissionTime')
  
    response$response <- as.data.frame(matrix(nrow=1,ncol=length(qNames)))
    
    response$response[1, 1] <- input$q1A
    response$response[1, 2] <- input$q2A
    response$response[1, 3] <- input$q3A
    q1Bout <- rep(FALSE, length(q2Bvalues))
    q1Bout[q2Bvalues %in% input$q1B] <- TRUE
    response$response[1, 4:20] <- q1Bout
    if( is.null(input$q1Bother)){
      response$response[1, 21] <- NA
    }else{
      response$response[1, 21] <- input$q1Bother
    }
    
    B_str <- paste('B<-c(', 
                   paste0('input$', paste('q2B',letters[grep('[a-g]',letters)], sep=''), 
                          collapse=','), ')', sep='')
    eval(parse(text=B_str))
    response$response[1, 22:28] <- B
    response$response[1, 29] <- input$q3B
    response$response[1, 30] <- input$q1C
    q2Cout <- rep(FALSE, length(q2Cvalues))
    q2Cout[q2Cvalues %in% input$q2C] <- TRUE
    response$response[1, 31:39] <- q2Cout
    
    if( is.null(input$q2Cother)){
      response$response[1, 40] <- NA
    }else{
      response$response[1, 40] <- input$q2Cother
    }

    C_str1 <- paste('C1<-c(',
                   paste0('input$', paste('q3C',letters[grep('[a-h]',letters)], sep=''), 
                          collapse=','), ')', sep='')
    C_str2 <- paste('C2<-c(',
                   paste0('input$', paste('q4C',letters[grep('[a-h]',letters)], sep=''), 
                          collapse=','), ')', sep='')
    eval(parse(text=C_str1))
    eval(parse(text=C_str2))
    response$response[1, 41:48] <- C1
    response$response[1, 49:56] <- C2
    response$response[1, 57] <- input$q5C
    
    response$response[1, 58] <- as.character(Sys.time())
    
    names(response$response) <- qNames
    
    responseAll <- read.csv('responses.csv')
    responseAll <- rbind(responseAll, response$response)
    write.csv(responseAll, 'responses.csv', row.names=F)
  })
}
