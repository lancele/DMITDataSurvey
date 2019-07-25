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
      '<input type="radio" name="%s"" value="%s"/>',
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
    
    
    
    ### Show modal dialog to confirm submission ###
    observeEvent(input$submitResponse, {
      showModal(modalDialog(
        title = 'Survey submitted!',
        size = 'm',
        p("Thank you for taking the time to complete this survey. Your participation is greatly appreciated. If you have any questions about the survey, please email the Water Boards' Data Management Innovation Team at ", a(href='mailto:WaterData@waterboards.ca.gov', 'WaterData@waterboards.ca.gov'), "."),
        easyClose = FALSE
      ))
    })
    
    ### Clear all input after submission ###
    reset('basicInfoForm')
    reset('dataUseForm')
    reset('dataSkillsForm')
    reset('addCommentsForm')
    ### Clear radio buttons (Q5)
    shinyjs::runjs(
      '$("[type=radio]").prop("checked", false);'
    )
  })
  
  #### end server code ####
}