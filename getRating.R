getRating=function(temp,raw){
  otemp=temp
    name=raw[temp,1] #get the deidentified id for this user
    for(j in (temp+1):(nrow(raw)+1)){
      if(identical(raw[j,1],name)){}
      else{
        temp=j #get the first row index for next user
        break
      }
    }
    tempraw1=raw[otemp:(temp-1),c(2,5,9,1)] #raw data for current reader with cols TestSetName, CaseName, Rating, DeIdentifiedUserId, 
    tempraw2=tempraw1[!duplicated(tempraw1[,2]),] #new dataframe with no repeated CaseName
    tempraw2[,3]=rep(0,nrow(tempraw2)) #all rating = 0
    labelcount = 1 #for row index in tempraw1
    labelnumber = 1 #for row index in tempraw2
    label=tempraw1[labelcount,]
    maxR=label[1,3] #largest rating for the 1st image
    tempraw2[labelnumber,3]=maxR
    
    for (k in 1:nrow(tempraw1)) {
      
      if(identical(tempraw1[k,2],label[1,2])){
        
        if(tempraw1[k,3]>maxR){
          maxR=tempraw1[k,3]
          tempraw2[labelnumber,3] = maxR
        }
      }
      else {
        
        labelcount = k
        label=tempraw1[labelcount,]
        maxR=label[1,3]
        labelnumber = labelnumber + 1
        tempraw2[labelnumber,3] = maxR
        
      }
    }
    
  return(list(temp=temp,tempraw=tempraw2))
}
