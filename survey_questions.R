

q1 <- list('Region 1 - North Coast' = 'rb1',
           'Region 2 - San Francisco Bay' = 'rb2',
           'Region 3 - Central Coast' = 'rb3',
           'Region 4 - Los Angeles' = 'rb4',
           'Region 5 - Central Valley' = 'rb5',
           'Region 6 - Lahontan' = 'rb6',
           'Region 7 - Colorado River' = 'rb7',
           'Region 8 - Santa Ana' = 'rb8',
           'Region 9 - San Diego' = 'rb9',
           'State Water Board - Sacramento' = 'sb',
           'Division of Drink Water District' = 'dw')

q2 <- list('DAS', 'DDW', 'DFA', 'DIT', 'DWQ', 'DWR', 'Not Applicable',
           'Other')

q5databases <- list('California Environmental Data Exchange Network (CEDEN)',
                     'California Integrated Water Quality System (CIWQS)',
                     'Electronic Water Rights Information Management System (eWRIMS)',
                     'GeoTracker/Groundwater Ambient Monitoring and Assessment (GAMA)',
                     'Stormwater Multi-Application Reporting and Tracking System (SMARTS )',
                     'Water49 GIS Library','Other Water Boards Database')

q5choices <- list('I do not use this data source', 
                  'I add or contribute data to this data source',
                  'I update the data in this data source',
                  'I look up or download data from this data source')

q6prompt <- '6. What, if any, non-Water Board data sources do you use for your work, 
            and how do you use them? Leave this blank if you do not use any non-Water 
            Board data sources.'


q7 <- list('Not interested' = 1,
            'Slightly interested' = 2,
            'Somewhat interested' = 3,
            'Very interested' = 4)

q8choicesExp <- list('No Experience' = 1,
            'Basic' = 2,
            'Proficient' = 3,
            'Expert' = 4)

q8choicesFreq <- list('Never' = 1,
                      'Annually' = 2,
                      'Monthly' = 3,
                      'Weekly' = 4,
                      'Daily' = 5)

q8choicesInt <- q7
q9choicesExp <- q8choicesExp
q9choicesInt <- q8choicesInt

q10txt <-  '10. Are there any additional data analysis related tasks, tools, or 
        topics that you would be interested in learning?'

q12txt <- '12. Do you want to enter the drawing for a $25 Amazon giftcard? If so, please enter your email address. Your participation in the drawing is optional.'
