library(plyr)
library(lme4)
library(car)
library(readr)
library(xlsx)
library(data.table)
library(ggplot2)
library(shiny)
library(forcats)
library(purrr)
library(tidyr)
library(emmeans)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyjs)
library(dplyr) #ALWAYS LOAD DPLYR LAST; OTHERWISE IT WILL MASK FUNCTIONS FROM OTHER PACKAGES

data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c(varname="mean"))
  return(data_sum)
}

session_count<-function(df) {
  seq(1, nrow(df), by=1)
}


FR_data <- read.csv(file="FR_data.csv")
FR_data$fr<- as.character(FR_data$fr)

FR_data$fr<- factor(FR_data$fr) #factoring fr
FR_data <- mutate(FR_data, sex =factor(ifelse(grepl("f",FR_data$subject)==TRUE, "f", "m"), levels = c('f', 'm'))) #adding sex

FR_data <- mutate(FR_data, activity=beam1+beam2+beam3+beam4) # adding locomotor activity
FR_data <- mutate(FR_data, percent.active=active/(active+inactive))
FR_data <- mutate(FR_data, percent.inactive=inactive/(active+inactive))


FR_data$subject<-fct_inorder(FR_data$subject)
FR_data<- as.data.table(FR_data)
FR_data<- FR_data[order(FR_data$edate),]
FR_data<-FR_data %>% group_by(subject)%>% nest()
FR_data<-FR_data %>% mutate(session=map(data,session_count))
FR_data<-FR_data %>% unnest()
FR_data<-with(FR_data, FR_data[order(session),])

# TRIAL AND TIMEOUT ACTIVES

for (q in (1:nrow(FR_data))) ({
  if (FR_data$fr[q]==1) ({
    FR_data$trial.active[q]=FR_data$reward[q]
    FR_data$timeout.active[q]=FR_data$active[q]-FR_data$trial.active[q]
  }) 
  if (FR_data$fr[q]==5) ({
    FR_data$trial.active[q]=FR_data$reward[q]*5
    FR_data$timeout.active[q]=FR_data$active[q]-FR_data$trial.active[q]
  })
  if (FR_data$fr[q]=="pr") ({
    FR_data$trial.active[q]=FR_data$active[q]-FR_data$timeout.active[q]
    FR_data$timeout.active[q]=FR_data$timeout.active[q]
  })
  
}) 
