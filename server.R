library(shiny)
library(shinyalert)
library(DT)
library(shinyjs)

glossary <- read.csv('glossary_shinysurvey.csv', colClasses = rep('character', 2))
source('survey_questions.R')
max_nchar_short <- 50
max_nchar_long <- 500
max_nchar_vlong <- 1000
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
  
    #### character limit on free-response questions ####
  
  output$q2other_charlimit <- reactive({ paste0('Only ', max_nchar_short - nchar(input$q2other), ' characters remaining.') })
  output$q3_charlimit <- reactive({ paste0('Only ', max_nchar_short - nchar(input$q3), ' characters remaining.') })
  output$q5other_charlimit <- reactive({ paste0('Only ', max_nchar_short - nchar(input$q5other), ' characters remaining.') })
  output$q6name_charlimit <- reactive({ paste0('Only ', max_nchar_short - nchar(input$q6name), ' characters remaining.') })
  output$q8g_charlimit <- reactive({ paste0('Only ', max_nchar_short - nchar(input$q8g), ' characters remaining.') })
  output$q9i_charlimit <- reactive({ paste0('Only ', max_nchar_short - nchar(input$q9i), ' characters remaining.') })
  output$q10_charlimit <- reactive({ paste0('Only ', max_nchar_long - nchar(input$q10), ' characters remaining.')})
  output$q11_charlimit <- reactive({ paste0('Only ', max_nchar_vlong - nchar(input$q11), ' characters remaining.')})
  
  observeEvent(input$q2other, {
    if( nchar(input$q2other) > max_nchar_short ){
      updateTextInput(session, 'q2other', value = substr(input$q2other, 1, max_nchar_short))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 2 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$q3, {
    if( nchar(input$q3) > max_nchar_short ){
      updateTextInput(session, 'q3', value = substr(input$q3, 1, max_nchar_short))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 3 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$q5other, {
    if( nchar(input$q5other) > max_nchar_short ){
      updateTextInput(session, 'q5other', value = substr(input$q5other, 1, max_nchar_short))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 5 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$q6name, {
    if( nchar(input$q6name) > max_nchar_short ){
      updateTextInput(session, 'q6name', value = substr(input$q6name, 1, max_nchar_short))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 6 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$q8g, {
    if( nchar(input$q8g) > max_nchar_short ){
      updateTextInput(session, 'q8g', value = substr(input$q8g, 1, max_nchar_short))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 8 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$q9i, {
    if( nchar(input$q9i) > max_nchar_short ){
      updateTextInput(session, 'q9i', value = substr(input$q9i, 1, max_nchar_short))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 9 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$q10, {
    if( nchar(input$q10) > max_nchar_long ){
      updateTextInput(session, 'q10', value = substr(input$q10, 1, max_nchar_long))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 10 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$q11, {
    if( nchar(input$q11) > max_nchar_vlong ){
      updateTextInput(session, 'q11', value = substr(input$q11, 1, max_nchar_vlong))
      showModal(modalDialog(
        title = 'Error!',
        'Character limit for Question 11 exceeded!',
        easyClose = TRUE
      ))
    }
  })
  
  #### submit survey ####

  observeEvent(input$submitResponse, {
    
    # limit number of responses to prevent file from blowing up
    # if a response is filled out in its entirety (i.e. char limit reached for free response questions)
    # and assuming all individual characters and numbers are 32-bit UTF (4 bytes)
    # then a single response has a max memory of 7.6 KB
    # using wikipedia's CalEPA page with the # of employees (4550, round to 5000 for any updates)
    # Then response file has a maximum of ~38 MB, round 40 MB for safety
    responseFilesize <- file.info('export/responses.csv')$size
    # filesize returns # of bytes
    if( responseFilesize > 40e6 ){
      showModal(modalDialog(
        title = 'Error!',
        'Number of responses exceeded limit. Please contact WaterData@waterboards.ca.gov for additional info.',
        easyClose = TRUE
      ))
    }

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
    responseAll <- read.csv('export/responses.csv')
    responseAll <- rbind(responseAll, response$response)
    write.csv(responseAll, 'export/responses.csv', row.names=F)
    
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
