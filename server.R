library(shiny)
library(shinyalert)
library(DT)

glossary <- read.csv('glossary_shinysurvey.csv', colClasses = rep('character', 2))
source('survey_questions.R')
server <- function(input, output, session){
  
  response <- reactiveValues()
  
  #### navigate through tabs ####
  
  observeEvent(input$startSurvey, {
    updateTabItems(session, 'sidebarMenu', selected = 'tabBasic')
  })
  
  observeEvent(input$submitBasic, {

    updateTabItems(session, 'sidebarMenu', selected = 'tabDataUse')
  })
  
  observeEvent(input$submitDataUse, {

    updateTabItems(session, 'sidebarMenu', selected = 'tabDataSkills')
  })
  
  observeEvent(input$submitDataSkills, {
    updateTabItems(session, 'sidebarMenu', selected = 'tabComments')
  })
  
  
  #### Question 5 table ####
  m <- matrix(
    as.character(1:4), nrow = 7, ncol = 4, byrow = TRUE,
    dimnames = list(q5databases , q5choices)
  )
  for (i in seq_len(nrow(m))) {
    m[i, ] <- sprintf(
      '<input type="radio" name="%s" value="%s"/>',
      q5databases[i], m[i, ]
    )
  }
  m
  output$q5 = DT::renderDataTable(
    m, escape = FALSE, selection = 'none', server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE, autoWidth = TRUE,
                   columnDefs = list(list(width = '500px', targets = c(0,1,2,3,4)))),
    callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
  ) 

  #### glossary search ####
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
  
  #### submit survey ####

  observeEvent(input$submitResponse, {

    #### Record responses and write to file ####

    qNames <- c('q1','q2','q2other', 'q3', 'q4', paste('q5', letters[1:7], sep=''),
                'q5other', 'q6name', 'q6answer', 'q7',
                paste('q8', rep(letters[1:7], each = 3), 1:3, sep = ''), 'q8other',
                paste('q9', rep(letters[1:9], each = 2), 1:2, sep = ''), 'q9other',
                'q10', 'q11', 'q12', 'SubmissionTime')
    
    response$response <- as.data.frame(matrix(nrow=1,ncol=length(qNames)))
    
    response$response[1, 1:5] <- c(input$q1, input$q2, input$q2other, input$q3, input$q4)
    k <- 6
    for(dbs in q5databases){
      if(dbs%in%names(input)){
        response$response[1, k] <- input[[dbs]]
      }else{
        next
      }
      k <- k+1
    }
    
    
    response$response[1, 13:16] <- c(input$q5other, input$q6name, input$q6answer, input$q7)
    
    k <- 17
    for(subL in letters[1:7]){
      for(subN in 1:3){
        response$response[1, k] <- input[[paste('q8', subL, subN, sep='')]]
        k <- k+1
      }
    }
    response$response[1, 38] <- input$q8g
    k <- 39
    for(subL in letters[1:9]){
      for(subN in 1:2){
        response$response[1, k] <- input[[paste('q9', subL, subN, sep='')]]
        k <- k+1
      }
    }
    
    response$response[1, 57] <- input$q9i
    response$response[1, 58] <- input$q10
    response$response[1, 59] <- input$q11
    response$response[1, 60] <- input$q12
    response$response[1, 61] <- as.character(Sys.time())
    names(response$response) <- qNames
    responseAll <- read.csv('/export/responses.csv')
    responseAll <- rbind(responseAll, response$response)
    write.csv(responseAll, '/export/responses.csv', row.names=F)
    
    
    
    ### Show modal dialog to confirm submission ###
    observeEvent(input$submitResponse, {
      showModal(modalDialog(
        title = 'Survey submitted!',
        size = 'm',
        p("Thank you for taking the time to complete this survey. Your participation is greatly appreciated. If you have any questions about the survey, please email the Water Boards' Data Management Innovation Team at ", a(href='mailto:WaterData@waterboards.ca.gov', 'WaterData@waterboards.ca.gov'), "."),
        easyClose = FALSE
      ))
    })
    
    ### Reset all input after submission ###
    reset('basicInfoForm')
    reset('dataUseForm')
    reset('dataSkillsForm')
    reset('addCommentsForm')
    
    ### Reset all radio buttons (for Q5)
    shinyjs::runjs(
      '$("[type=radio]").prop("checked", false);'
    )
    
    ### Disable submit button - to deter spam submissions ###
    shinyjs::disable("submitResponse")
    
  })
  
  #### end server code ####
}
