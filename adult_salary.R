### Logistic 

install.packages("Amelia")
library(dplyr)
library(Amelia)

adult <- read.csv('adult_sal.csv')
head(adult)
str(adult)
summary(adult)

adult <- select(adult, -X)


### 1.Data Cleaning

# classify the employment type further
group_empl <- function(empl){
  empl <- as.character(empl)
  if (empl=='Never-worked' | empl == 'Without-pay'){
    return('Unemployed')
  }
  else if (empl=='Self-emp_inc' | empl == 'Self-emp-not-inc'){
    return('self-emp')
  }
  else if (empl=='Local-gov' | empl == 'State-gov'){
    return('SL-gov')
  }
  else{
    return(empl)
  }
}

adult$type_employer <- sapply(adult$type_employer, group_empl)
table(adult$type_employer)

# classify the marital type further
group_marital <- function(mar){
  mar <- as.character(mar)
  if (mar=='Separated' | mar == 'Divorced' | mar == 'Widowed'){
    return('Not-Married')
  }
  else if (mar=='Never-married'){
    return(mar)
  }
  else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital, group_marital)
table(adult$marital)

# classify the country type further
levels(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')


group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)
table(adult$country)

adult$type_employer <- sapply(adult$type_employer, factor)
adult$country <- sapply(adult$country, factor)
adult$marital <- sapply(adult$marital, factor)

### 2. Missing Data
adult[adult == '?'|adult == ' ?'] <- NA
table(adult$type_employer)
missmap(adult, y.at=c(1), y.labels = c(''),col = c('yellow','black'))
adult <- na.omit(adult)
missmap(adult, y.at=c(1), y.labels = c(''),col = c('yellow','black'))


## EDA (exploratory data analysis)
library(ggplot2)
ggplot(adult, aes(age)) + geom_histogram(aes(fill = income), color='black', binwidth=1) + theme_bw()

ggplot(adult, aes(hr_per_week)) + geom_histogram(fill = 'green', color='black') + theme_bw()
names(adult)[names(adult)=='country'] <- 'region'

ggplot(adult, aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()
+theme(axis.text.x = element_text(angle=90,hjust=1))


## Build The Model
install.packages('caTools')
library(caTools)
set.seed(101)

# Split the sample
sample <- sample.split(adult$income, SplitRatio = 0.7)

# Training the Data
train = subset(adult, sample == TRUE)
test = subset(adult, sample == FALSE)
model = glm(income ~., family = binomial(logit), data = train)
summary(model)

# choose a model by AIC in a Stepwise Algorithm
new.step.model <- step(model)
summary(new.step.model)

test$predicted.income = predict(model, newdata = test, type = "response")

# confusion table
table(test$income, test$predicted.income > 0.5)










