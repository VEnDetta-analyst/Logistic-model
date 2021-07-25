library(ggplot2)
library(gridExtra)
options(jupyter.plot_mimetypes='image/png')
download.file("https://ibm.box.com/shared/static/sv3oy0gyhuiifmosxsvxt5ogfs71iv37.csv",
              destfile = "LoanData.csv", quiet = TRUE)
options(scipen = 999)
LoanData <- read.csv("LoanData.csv")
head(LoanData)
nrow(LoanData)
ncol(LoanData)
library(base)
table(LoanData['loan_status'])

ggplot(LoanData, aes(x=Principal, fill=loan_status)) +geom_histogram(binwidth=120,alpha=0.35,aes(y=0.5*..density..),position='identity')
ggplot(LoanData, aes(x=terms, fill=loan_status)) +geom_histogram(binwidth=10,alpha=0.45,aes(y=1*..density..),position='identity')
ggplot(LoanData, aes(x=age, fill=loan_status)) +geom_histogram(binwidth=1,alpha=0.55,aes(y=1*..density..),position='identity')

ggplot(LoanData,aes(x=dayofweek,fill=loan_status))+geom_histogram(binwidth=1,alpha=0.55,aes(y=1*..density..),position='identity')+scale_x_continuous(limits = c(0, 7))
#converting days of week to categorical value
namevector = c("Weekend")
LoanData[,namevector] = 0
LoanData$Weekend[LoanData$dayofweek>3]=1
head(LoanData)
#coverting gender into indicator variable
namevector = c("Gender01")
LoanData[,namevector] = 0
LoanData$Gender01[LoanData$Gender=='male']=1
head(LoanData)
table(LoanData$Gender01,LoanData$loan_status)
ggplot(LoanData, aes(x=Gender01, fill=loan_status))  +geom_histogram(binwidth=1,alpha=0.55,aes(y=1*..density..),position='identity')+scale_x_continuous(limits = c(0, 2))
#One hot encoding for coversion of categorical variables into binary variables and append them to feature dataframe
library(dummies)
head(LoanData['education'])
LoanData=dummy.data.frame(LoanData,names=c('education'))
head(LoanData[c('educationBechalor','educationcollege','educationHigh School or Below','educationMaster or Above','Weekend','Gender01')])
Colunms = c('Principal','terms','age','educationBechalor', 'educationcollege',  'educationHigh School or Below','educationMaster or Above','Weekend','Gender01')
Data=LoanData[Colunms]
head(Data)
Newcolumn=c("Class")
Data[,Newcolumn]=0
Data$Class[LoanData$loan_status=='PAIDOFF']=1
head(Data[,Newcolumn],10)
head(LoanData$loan_status,10)
#Normalize data
Data[Colunms]=scale(Data[Colunms])
head(Data[Colunms])
#Train Test Split
set.seed(123)
testindex=sample.int(nrow(Data))[1:floor(0.1*nrow(Data))]
Testdata=Data[testindex,]
Traindata=Data[-testindex]
#model fitting
model=glm(Class~.,family = binomial(link="logit"),data=Traindata)
summary(model)
#prediction
fitted.values=predict(model,newdata = Testdata, type='response')
yhat=ifelse(fitted.values>0.5,1,0)
y=Testdata[,c('Class')]
#Accuracy
mean(yhat==y)
