library(readxl)
setwd("C:/Users/wunai/Desktop/Rater/Data")
raw<-read_excel("Data.xlsx",sheet = "Raw data")
names(raw)[5]<-"BREAST labels"
case<-read_excel("Data.xlsx",sheet = "Case details")
case=unique(case) #repeated data occurs
case[which(grepl('WA',case$`Test set`)),1]='Western Australia' #to be the same as the test set in raw data

#make case names and userIds numeric
case_name_index=data.frame("CaseName"=case$`BREAST labels`, "CaseNameIndex"=seq(1,nrow(case))) #Give Case Names unique numeric indices
raw_data_787=raw[-c(which(raw$DeIdentifiedUserId=="User1561624835339" & raw$TestSetName=="Sydney"), #delete the (User,TestSet) where no cases are rated as positive.
                    which(raw$DeIdentifiedUserId=="breast0032230" & raw$TestSetName=="Sydney")),] #delete the (User,TestSet) which is in Raw data but not in Scores.
UserId=unique(raw_data_787$DeIdentifiedUserId)
UserId_index=data.frame("DeIdentifiedUserId"=UserId,"UserIdIndex"=seq(1,length(UserId))) #Give DeIdentifiedUserId unique numeric indices

#get the dataframe with correct format for Rater package
rating_data=data.frame("item"=numeric(),"rater"=numeric(),"rating"=numeric()) #data in rater's format extracted from Raw Data
temp=1 #index of next (User,TestSet)
while(temp<(nrow(raw_data_787)+1)) {
  rater=which(grepl(raw_data_787$DeIdentifiedUserId[temp],UserId_index$DeIdentifiedUserId)) #get rater index
  perrating=getAllRating(temp,raw_data_787)
  temp=perrating$temp
  ratingdf=perrating$tempraw
  
  #rating>1
  item=c()
  rating=c()
  for(i in 1:nrow(ratingdf)){
    item=c(item,which(grepl(ratingdf$`BREAST labels`[i],case_name_index$CaseName)))
  }
  rating=ratingdf$Rating
  adddf1=data.frame("item"=item,"rater"=rep(rater,nrow(ratingdf)),"rating"=rating)
  
  #rating=1 hidden under rating>1
  itemdf=as.data.frame(table(item))
  index1=which(itemdf$Freq==1)
  adddf2=data.frame("item"=itemdf[index1,1],"rater"=rep(rater,length(index1)),"rating"=rep(1,length(index1)))
  
  #rating=1 two views all rated as 1
  testset=ratingdf[1,1]
  tempcase=case[which(grepl(testset,case$`Test set`)),]
  tempmerge=merge(ratingdf,tempcase,all = TRUE) #images with NA are rated as 1
  rate1ind=which(is.na(tempmerge$Rating))
  item=c()
  if(length(rate1ind)>0){
    for(k in 1:length(rate1ind)){
      item=c(item,which(grepl(tempmerge$`BREAST labels`[rate1ind[k]],case_name_index$CaseName)))
    }
  }
  adddf3=data.frame("item"=item,"rater"=rep(rater,length(item)),"rating"=rep(1,length(item)))
  
  rating_data=rbind(rating_data,adddf1,adddf2,adddf3,adddf3)
}
rating_data=as.data.frame(lapply(rating_data,as.numeric))
