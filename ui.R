library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)

source('survey_questions.R')
glossary <- read.csv('glossary_shinysurvey.csv', colClasses = rep('character', 2))
glossHTML <- c()
for(i in 1:nrow(glossary)){
  strng <- paste('<p><b>',glossary$term[i],': </b>',glossary$definition[i],'</p><br>',sep='')
  glossHTML <- paste(glossHTML,strng,sep='')
}

header <- dashboardHeader(
  title = 'Data Use and Skills Survey'
)

sidebar <- dashboardSidebar(
  useShinyalert(),
  sidebarMenu(
    id = 'sidebarMenu',
    menuItem('Overview', tabName = 'tabOverview'),
    menuItem('A. Basic Information', tabName = 'tabBasic'),
    menuItem('B. Data Use', tabName = 'tabDataUse'),
    menuItem('C. Data Skills', tabName = 'tabDataSkills'),
    menuItem('Glossary', tabName = 'tabGlossary'),
    sidebarSearchForm('searchTerm', 'searchButton', label = 'Search glossary:'),
    br(),
    HTML('<center><img style="max-width: 200px; height:auto;"
        src="waterboards_logo_high_res.jpg"
         width="350" height="243"></center>')
  )
)

body <- dashboardBody(
  # tags$style(".checkbox-inline, .radio-inline {
  #   text-align: left;
  #            margin-left: 20px;
  #            margin-right: 0px;
  #            padding: 0px;
  #            width: 400px;} "),
  tabItems(
    tabItem('tabOverview',
            titlePanel('Overview'),
            fluidRow(
              column(8,
                     p('The Water Boards’ Data Management Innovation Team is conducting a survey to
                       better understand how Water Boards staff and managers use and interact with
                       data in their day-to-day work. The survey consists of questions asking
                       about the data sources you use, specific data-related tasks you perform,
                       and data science topics that are of interest to you. Your answers to these
                       questions are anonymous and will inform a new initiative to offer data
                       science training at the Water Boards.'),
                     p('The last day to participate in the survey is ____Date____. If you have any
              questions, please contact Michelle Tang (michelle.tang@waterboards.ca.gov)
                       in the Office of Information Management and Analysis.')
                     )
            ),
            br(),
            actionButton('startSurvey', 'Start Survey'),
            verbatimTextOutput('testText')
    ),
    tabItem('tabBasic',
            titlePanel('Basic Information'),
            br(),
            selectInput('q1A', '1. At which Water Boards office do you currently work?',
                        choices = q1A, width = '400px'),
            br(),
            textInput('q2A', '2. In what program/functional area do you work (e.g. Nonpoint Source, GAMA)?',
                      width = '400px'),
            br(),
            radioButtons('q3A', '3. Are you a supervisor?', width = '400px',
                         choices = list('Yes' = TRUE,
                                        'No' = FALSE)),
            br(),
            actionButton('submitBasic', 'Submit Responses and Go to Section B')
    ),
    tabItem('tabDataUse',
            titlePanel('Data Use Assessment'),
            br(),
            checkboxGroupInput('q1B', '1. Please choose all the databases you use for your day-to-day work.',
                               choiceNames = q1Bchoices, choiceValues = q1Bvalues, width = '100%'),
            conditionalPanel(
              condition = "input.q1B.indexOf('Other') > -1",
              textInput('q1Bother', 'Provide name of other database:', width = '400px')
            ),
            br(),
            strong('2. How often do you perform the following data-related tasks?'),
            br(), br(),
            radioButtons('q2Ba','a. Search and cross reference data from multiple databases or sources',
                         choices = q2B, inline = TRUE),
            radioButtons('q2Bb','b. Identify errors and problems in data',
                         choices = q2B, inline = TRUE),
            radioButtons('q2Bc','c. Write documentation and metadata',
                         choices = q2B, inline = TRUE),
            radioButtons('q2Bd','d. Analyze data to identify patterns, trends, and other relationships in data',
                         choices = q2B, inline = TRUE),
            radioButtons('q2Be','e. Use machine learning to automate tasks and make predictions',
                         choices = q2B, inline = TRUE),
            radioButtons('q2Bf','f. Present data and findings to others',
                         choices = q2B, inline = TRUE),
            radioButtons('q2Bg','g. Use data to inform decision-making ',
                         choices = q2B, inline = TRUE),
            br(),
            selectInput('q3B', '3. To what extent does your Water Board programs use data to support decision-making?',
                        choices = q3B),
            br(),
            actionButton('submitDataUse', 'Submit Responses and Go to Section C')
    ),
    
    tabItem('tabDataSkills',
            titlePanel('Data Skills Assessment'),
            selectInput('q1C', '1. Are you interested in taking data science courses offered by the Water Boards Training Academy?',
                        choices = q1C),
            br(),
            checkboxGroupInput('q2C', '2. Which data cience topics would you be interested in learning?',
                               choiceNames = q2Cchoices, choiceValues = q2Cvalues, width = '100%'),
            conditionalPanel(
              condition = "input.q2C.indexOf('other') > -1",
              textInput('q2Cother', 'Provide name of other topics:', width = '400px')
            ),
            br(),
            strong('3. What is your skill level with each of the following software, tools, and programming languages?'),
            br(), br(),
            radioButtons('q3Ca', 'Microsoft Excel', choices = q3C, inline = TRUE),
            radioButtons('q3Cb', 'Microsoft Access', choices = q3C, inline = TRUE),
            radioButtons('q3Cc', 'SQL', choices = q3C, inline = TRUE),
            radioButtons('q3Cd', 'Tableau', choices = q3C, inline = TRUE),
            radioButtons('q3Ce', 'Microsoft Office BI', choices = q3C, inline = TRUE),
            radioButtons('q3Cf', 'ArcGIS', choices = q3C, inline = TRUE),
            radioButtons('q3Cg', 'Python', choices = q3C, inline = TRUE),
            radioButtons('q3Ch', 'R', choices = q3C, inline = TRUE),
            br(),
            strong('4. What is your interest level in learning each of the following software, tools, and programming languages?'),
            br(), br(),
            radioButtons('q4Ca', 'Microsoft Excel', choices = q1C, inline = TRUE),
            radioButtons('q4Cb', 'Microsoft Access', choices = q1C, inline = TRUE),
            radioButtons('q4Cc', 'SQL', choices = q1C, inline = TRUE),
            radioButtons('q4Cd', 'Tableau', choices = q1C, inline = TRUE),
            radioButtons('q4Ce', 'Microsoft Office BI', choices = q1C, inline = TRUE),
            radioButtons('q4Cf', 'ArcGIS', choices = q1C, inline = TRUE),
            radioButtons('q4Cg', 'Python', choices = q1C, inline = TRUE),
            radioButtons('q4Ch', 'R', choices = q1C, inline = TRUE),
            br(),
            textAreaInput('q5C', '5. Do you have any additional comments?', width = '600px', height = '150px'),
            br(),
            actionButton('submitResponse', 'Complete Survey'),
            ),
    tabItem('tabGlossary',
            titlePanel('Glossary'),
            br(),
            HTML(glossHTML)
            )
  )
)


ui <- dashboardPage(
  header,
  sidebar,
  body,
  title = 'Data Literacy Survey',
  skin = 'purple'
)
