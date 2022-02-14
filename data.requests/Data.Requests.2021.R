#Alene Onion
#Starting March 2021
#Data Requests

library(rmarkdown)

#Seneca Lake possible assessment. Sarah Rickard requested all data from one particular PWL segment
rmarkdown::render('2020/data.requests/data.request.03.2021.Rmd')

#A summary of current assessments of PWS and expected assessments using the past 10 yrs worth of data
#For discussion with the DWSPP team about potential funding for additional sampling staff
rmarkdown::render('2020/data.requests/data.request.03.2021.PWS.Assessments.Rmd')

#analyzing the best way to calculate a seasonal mean
rmarkdown::render('C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/Defining.Seasonal.Mean.Rmd')
