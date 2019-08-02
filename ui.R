library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(shinyjs)

source('survey_questions.R')
glossary <- read.csv('glossary_shinysurvey.csv', colClasses = rep('character', 2))
glossHTML <- c()
for(i in 1:nrow(glossary)){
  strng <- paste('<p><b>',glossary$term[i],': </b>',glossary$definition[i],'</p><br>',sep='')
  glossHTML <- paste(glossHTML,strng,sep='')
}

header <- dashboardHeader(
  title = 'Data Literacy Survey'
)

sidebar <- dashboardSidebar(
  useShinyalert(),
  useShinyjs(),
  sidebarMenu(
    id = 'sidebarMenu',
    menuItem('Overview', tabName = 'tabOverview'),
    menuItem('A. Basic Information', tabName = 'tabBasic'),
    menuItem('B. Data Use', tabName = 'tabDataUse'),
    menuItem('C. Data Skills', tabName = 'tabDataSkills'),
    menuItem('D. Comments', tabName = 'tabComments'),
    menuItem('Glossary', tabName = 'tabGlossary'),
    sidebarSearchForm('searchTerm', 'searchButton', label = 'Search glossary:'),
    br(),
    HTML('<center><img style="max-width: 200px; height:auto;"
        src="waterboards_logo_high_res.jpg"
         width="350" height="243"></center>')
  )
)

body <- dashboardBody(
  tabItems(
    #### Overview Tab ####
    tabItem('tabOverview',
            titlePanel('Overview'),
            fluidRow(
              column(8,
                     p('The Water Boards’ Data Management Innovation Team is conducting a survey to better understand how Water Boards staff and managers use and interact with data in their day-to-day work. Your responses will inform a new initiative to offer data science training at the Water Boards.'),
                     p('This survey is voluntary and should take about', tags$b('7 minutes'), 'to complete. All responses will be aggregated and analyzed as a group to protect your anonymity. The last day to participate in the survey is', tags$b('September 27, 2019'),'.'),
                     p('We will be holding a drawing for four $25 Amazon Gift Cards. To enter the drawing, enter your email address at the end of the survey and submit your completed survey. We ask for your email address only so that we can notify the gift card winners. We will not share or sell your information. The winners of the drawing will be randomly selected and notified after the survey period closes. Participation in the drawing is optional.'),
                     p('If you have any questions, please email', a('WaterData@waterboards.ca.gov', href = 'mailto:WaterData@waterboards.ca.gov'), 'and include "Data Survey" in the subject line.'),
                     p('We thank you for your time and participation!')
              )
            ),
            br(),
            actionButton('startSurvey', 'Start Survey')
    ),
    #### Basic Tab ####
    tabItem('tabBasic',
            titlePanel('Basic Information'),
            div(
              id='basicInfoForm',
              br(),
              selectInput('q1', '1. At which Water Boards office do you currently work?',
                          choices = q1, width = '400px'),
              br(),
              selectInput('q2', '2. For which State Water Board division do you currently work?',
                          choices = q2, width = '400px'),
              textInput('q2other', 'Provide name of other division, if applicable:', width = '400px'),
              textOutput('q2other_charlimit'),
              textInput('q3', '3. In what program/functional area do you work (e.g. Nonpoint Source, GAMA)?',
                        width = '400px'),
              textOutput('q3_charlimit'),
              br(),
              radioButtons('q4', '4. Are you a supervisor?', width = '400px',
                           choices = list('Yes' = TRUE,
                                          'No' = FALSE)),
              br()
            ),
            actionButton('submitBasic', 'Submit Responses and Go to Section B'),
            br()
    ),
    #### Data Use Tab ####
    tabItem('tabDataUse',
            #tags$head(tags$style(HTML("#foo {background-color:none}"))),
            titlePanel('Data Use Assessment'),
            div(
              id="dataUseForm",
              br(),
              p(strong('5. Which Water Boards data sources do you use for your work, and how do you use them?')),
              fluidRow(
                column(8,
                       DT::dataTableOutput('q5'),
                       textInput('q5other', 'Please name other Water Boards database, if applicable:', width = '100%'),
                       textOutput('q5other_charlimit')
                )
              ),
              br(),
              p(strong(q6prompt)),
              fluidRow(
                column(4,
                       textInput('q6name', 'Name of data source:', width = '100%')
                ),
                column(4,
                       selectInput('q6answer', '', 
                                   choices = list('I do not use this data source' = 1, 
                                                  'I add or contribute data to this data source' = 2,
                                                  'I update the data in this data source' = 3,
                                                  'I look up or download data from this data source' = 4)
                       )
                )
              )
            ),
            br(),
            actionButton('submitDataUse', 'Submit Responses and Go to Section C')
    ),
    #### Data Skills Tab ####
    tabItem('tabDataSkills',
            titlePanel('Data Skills Assessment'),
            div(
              id='dataSkillsForm',
              br(),
              selectInput('q7', '7. Are you interested in taking data science courses offered by the Water Boards Training Academy?',
                          choices = q7),
              br(),
              #### Question 8 ####
              p(strong('8. Please answer the following questions about data-related tasks.')),
              fluidRow(
                column(2,
                       ''),
                column(2,'How often do you perform this task?'),
                column(2,'How would you assess your skill level in performing this task?'),
                column(2,'Are you interested in further developing this skill set?')
              ),
              fluidRow(
                column(2,
                       p('a. Collect and combine data from different sources',
                         style = 'margin-top: 15px;')
                ),
                column(2, selectInput('q8a1', '', choices = q8choicesFreq, width = '100%')
                ),
                column(2, selectInput('q8a2', '', choices = q8choicesExp, width = '100%')),
                column(2, selectInput('q8a3', '', choices = q8choicesInt, width = '100%'))
              ),
              fluidRow(
                column(2,
                       p('b. Clean and validate data to ensure accuracy, consistency, and completeness',
                         style = 'margin-top: 15px;')
                ),
                column(2, selectInput('q8b1', '', choices = q8choicesFreq, width = '100%')
                ),
                column(2, selectInput('q8b2', '', choices = q8choicesExp, width = '100%')),
                column(2, selectInput('q8b3', '', choices = q8choicesInt, width = '100%'))
              ),
              fluidRow(
                column(2, p('c. Create data visualizations (charts and graphs)',
                            style = 'margin-top: 15px;')),
                column(2, selectInput('q8c1', '', choices = q8choicesFreq, width = '100%')
                ),
                column(2, selectInput('q8c2', '', choices = q8choicesExp, width = '100%')),
                column(2, selectInput('q8c3', '', choices = q8choicesInt, width = '100%'))
              ),
              fluidRow(
                column(2,
                       p('d. Use statistics to identify trends and other relationships in data',
                         style = 'margin-top: 15px;')
                ),
                column(2, selectInput('q8d1', '', choices = q8choicesFreq, width = '100%')),
                column(2, selectInput('q8d2', '', choices = q8choicesExp, width = '100%')),
                column(2, selectInput('q8d3', '', choices = q8choicesInt, width = '100%'))
              ),            
              fluidRow(
                column(2, p('e. Write code to transform, analyze, or visualize data',
                            style = 'margin-top: 15px;')
                ),
                column(2, selectInput('q8e1', '', choices = q8choicesFreq, width = '100%')
                ),
                column(2, selectInput('q8e2', '', choices = q8choicesExp, width = '100%')),
                column(2, selectInput('q8e3', '', choices = q8choicesInt, width = '100%'))
              ),           
              fluidRow(
                column(2,
                       p('f. Use machine learning to automate tasks and make predictions',
                         style = 'margin-top: 15px;')
                ),
                column(2, selectInput('q8f1', '', choices = q8choicesFreq, width = '100%')
                ),
                column(2, selectInput('q8f2', '', choices = q8choicesExp, width = '100%')),
                column(2, selectInput('q8f3', '', choices = q8choicesInt, width = '100%'))
              ),
              fluidRow(
                column(2, textInput('q8g', '', placeholder = 'g. Other:'),
                      textOutput('q8g_charlimit')),
                column(2, selectInput('q8g1', '', choices = q8choicesFreq, width = '100%')),
                column(2, selectInput('q8g2', '', choices = q8choicesExp, width = '100%')),
                column(2, selectInput('q8g3', '', choices = q8choicesInt, width = '100%'))
              ),
              br(),
              #### Question 9 ####
              p(strong('9. Please answer the following questions about data science software
                       and computer languages.')),
              fluidRow(
                column(2, ''),
                column(2, 'What is your skill level with this software or computer language?'),
                column(2, 'Are you interested in further developing your skills with this software or
                       computer language?')
                ),
              fluidRow(
                column(2, p('a. Microsoft Excel', style = 'margin-top: 25px;')),
                column(2, selectInput('q9a1', '', choices = q9choicesExp)),
                column(2, selectInput('q9a2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, p('b. Microsoft Access', style = 'margin-top: 25px;')),
                column(2, selectInput('q9b1', '', choices = q9choicesExp)),
                column(2, selectInput('q9b2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, p('c. Structured Query Language (SQL)', style = 'margin-top: 25px;')),
                column(2, selectInput('q9c1', '', choices = q9choicesExp)),
                column(2, selectInput('q9c2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, p('d. Tableau', style = 'margin-top: 25px;')),
                column(2, selectInput('q9d1', '', choices = q9choicesExp)),
                column(2, selectInput('q9d2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, p('e. Microsoft Power BI', style = 'margin-top: 25px;')),
                column(2, selectInput('q9e1', '', choices = q9choicesExp)),
                column(2, selectInput('q9e2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, p('f. ArcGIS', style = 'margin-top: 25px;')),
                column(2, selectInput('q9f1', '', choices = q9choicesExp)),
                column(2, selectInput('q9f2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, p('g. Python', style = 'margin-top: 25px;')),
                column(2, selectInput('q9g1', '', choices = q9choicesExp)),
                column(2, selectInput('q9g2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, p('h. R', style = 'margin-top: 25px;')),
                column(2, selectInput('q9h1', '', choices = q9choicesExp)),
                column(2, selectInput('q9h2', '', choices = q9choicesInt))
              ),
              fluidRow(
                column(2, textInput('q9i', '', placeholder = 'i. Other:'),
                      textOutput('q9i_charlimit')),
                column(2, selectInput('q9i1', '', choices = q9choicesExp)),
                column(2, selectInput('q9i2', '', choices = q9choicesInt))
              )
            ),
            #### ####
            br(),
            actionButton('submitDataSkills', 'Submit Responses and Go to Section D')
    ),
    #### Comments Tab ####
    tabItem('tabComments',
            titlePanel('Additional Comments'),
            br(),
            textAreaInput('q10', q10txt, width = '600px', height = '150px'),
            textOutput('q10_charlimit'),
            br(),
            textAreaInput('q11', '11. Do you have any additional comments?', width = '600px', height = '150px'),
            textOutput('q11_charlimit'),
            br(),
            textInput('q12', q12txt, width = '400px'),
            textOutput('q12_charlimit'),
            br(),
            actionButton('submitResponse', 'Complete Survey'),
            DT::dataTableOutput('testTable')
            ),
    tabItem('tabGlossary',
            titlePanel('Glossary'),
            br(),
            HTML(glossHTML)
            )
  )
  #### End UI ####
)


ui <- dashboardPage(
  header,
  sidebar,
  body,
  title = 'Data Literacy Survey',
  skin = 'purple'
)
