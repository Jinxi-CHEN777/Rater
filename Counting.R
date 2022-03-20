#Counting TP,TN,FP,FN for each user at one TestSet
library(readxl)
setwd("C:/Users/CHEN JINXI/Desktop/Rater")
scores<-read_excel("Data.xlsx",sheet="Scores")
raw<-read_excel("Data.xlsx",sheet = "Raw data")
names(raw)[5]<-"BREAST labels"
case<-read_excel("Data.xlsx",sheet = "Case details")
case=unique(case) #repeated data occurs
case[which(grepl('WA',case$`Test set`)),1]='Western Australia' #to be the same as the test set in raw data
testscore<-data.frame(DeIdentifiedUserId=character(),
                      TestSet=character(),
                      TruePositive=integer(),
                      TrueNegative=integer(),
                      FalsePositive=integer(),
                      FalseNegative=integer())
j=1 #index of testscore
temp=1 #the first row index of next user, eg. 1 for the 1st user, 48 for the 2nd user
TP=0 # of true positive for each user, Abnormal rated with 3,4,5
TN=0 # of true negative for each user, Normal rated with 1,2
FP=0 # of false positive for each user, Normal rated with 3,4,5
FN=0 # of false negative for each user, Abnormal rated with 1,2

while(temp<=nrow(raw)){
  perrateing=getRating(temp,raw)
  temp=perrateing$temp
  rating=perrateing$tempraw
  
  testscore[j,1]=rating[1,4] #DeIdentifiedUserId
  testscore[j,2]=rating[1,1] #TestSet
  for(i in 1:nrow(rating)){
    ind=which(grepl(rating[i,2],case$`BREAST labels`))
    type=case[ind,4]
    
    if(type[1,1]=='Normal'){ #Normal
      if(rating[i,3]>2){
        FP=FP+1
      }else{
        TN=TN+1
      }
    }else{ #Abnormal
      if(rating[i,3]>2){
        TP=TP+1
      }else{
        FN=FN+1
      }
    }
  }
  
  testset=rating[1,1]
  tempcase=case[which(grepl(testset,case$`Test set`)),]
  tempmerge=merge(rating,tempcase,all = TRUE) #images with NA are rated as 1
  rate1ind=which(is.na(tempmerge$Rating))
  rate1=tempmerge$`Case Type`[rate1ind]
  FN=FN+sum(rate1!="Normal") #rate 1 but Abnormal
  TN=TN+sum(rate1=="Normal") #rate 1 and Normal
  testscore[j,3:6]=c(TP,TN,FP,FN)
  
  j=j+1
  TP=0
  TN=0
  FP=0
  FN=0
}

#compare TP,TN,FP,FN in testscore with those in scores sheet
#different number of TP,TN,FP,FN
diff=data.frame(DeIdentifiedUserId=character(),
                TestSetTS=character(),
                IndTS=numeric(),
                TPTS=numeric(),
                TNTS=numeric(),
                FPTS=numeric(),
                FNTS=numeric(),
                IndS=numeric(),
                TPS=numeric(),
                TNS=numeric(),
                FPS=numeric(),
                FNS=numeric())
#data in scores match nothing in testscore
nomatch=data.frame(DeIdentifiedUserId=character(),
                   TestSetTS=character(),
                   IndTS=numeric(),
                   TPTS=numeric(),
                   TNTS=numeric(),
                   FPTS=numeric(),
                   FNTS=numeric(),
                   source=character())
m=1
n=1
iscores=c() #store the index of scores, which row have been compared with testscore
for(i in 1:nrow(testscore)){
  id=testscore[i,1]
  set=testscore[i,2]
  index=which(scores$TestSetName==set & scores$DeIdentifiedUserId==id)
  if(length(index)!=0){
    iscores=c(iscores,index)
    if(sum(testscore[i,3:6]!=scores[index,17:20])>0){
      diff[m,]=c(id,set,i,testscore[i,3:6],index,scores[index,17:20])
      m=m+1
    }
  }else{
    nomatch[n,]=c(id,set,i,testscore[i,3:6],"testscore")
    n=n+1
  }
}
missing=c() #some rows (one user with one test set) in scores match nothing in testscore
for(i in 1:nrow(scores)){
  if(i %in% iscores){
  }else{
    missing=c(missing,i)
  }
}
for(i in 1:length(missing)){
  nomatch[n,]=c(scores[missing[i],c(4,1)],missing[i],scores[missing[i],17:20],"scores")
  n=n+1
}

write.csv(diff,file = "Difference of TPs between Scores and testscore.csv",row.names = F)
write.csv(nomatch,file = "Mismatched data.csv",row.names = F)


testscore$specificity=testscore$TrueNegative/(testscore$TrueNegative+testscore$FalsePositive)
testscore$sensitivity=testscore$TruePositive/(testscore$TruePositive+testscore$FalseNegative)
testscore$FPR=1-testscore$specificity

write.csv(testscore,file = "testscore.csv",row.names = F)


