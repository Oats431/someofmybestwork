#Project 3 Eric Oatey

rm(list = ls())
#I got the colcounts from an online character counter
colcounts=c(11,28,3,28,3,19)
#Using a suggestion from a classmate, I broke the url into 3 parts
file="http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf"
file2="gms.txt"

dates <- 1960:2010

df2=data.frame(numeric(),character(),numeric(),character(),numeric(),character(),numeric(),numeric(),numeric(),numeric(),numeric())

# Div2 takes in temp data frame for each Season, gets rid of all rows that 
#contain a team with less then 6 games played, returns trimmed down temp data 
#frame without the teams with less then 6 games played

#grabed a temp file in order to define Div2 correctly, not sure why it 
#would not define properly without it
temp<- read.fwf(paste(file,dates[1],file2, sep = ""),colcounts)
Div2<-function(temp){
  x<-unique(temp$V2)
  y<-unique(temp$V4)
  z<-unique(c(as.character(x),as.character(y)))
  test4<-numeric()
  for(j in 1:length(z)){
    stringtomatch=z
    test<-which(temp$V2==stringtomatch[j])
    test2<-which(temp$V4==stringtomatch[j])
    test3<-append(test, test2)
    if(length(test3)<6){
      test4<-append(test4,test3)
    }
  }
  test4<-unique(test4)
  temp5<-temp[-test4,]
  return(temp5)
}
#winsandloses caculates the who won/lost each game, adds in  4 colums for
#each game played
awaywin<-numeric()
homewin<-numeric()
awayloss<-numeric()
homeloss<-numeric()
winsandloses<-function(temp){
  temp$V3<-as.numeric(as.character(temp$V3)) #in order to avoid errors, V3 and V4
  temp$V5<-as.numeric(as.character(temp$V5)) #must be turned into numberic columns
  for(i in 1:length(temp$V3)){
    if(temp$V3[i]<temp$V5[i]){
      awaywin[i]=0
      awayloss[i]=1
      homewin[i]=1
      homeloss[i]=0
    }
    else{
      awaywin[i]=1
      awayloss[i]=0
      homewin[i]=0
      homeloss[i]=1
    }
  }
  temp<-cbind(temp,awaywin,awayloss,homewin,homeloss)
  #Below is the function written out in Sortingthedataframe.R that is not working correctly for now
  #temp<-GenerateWins(temp)  NOTE(this line is to be added in later when GenerateWins works correctly)
  return(temp)
}

GenerateWins<-function(temp){
  df3<-data.frame(character(),integer(),integer(),integer(),character())
  colnames(df3)<-c("teams","season", "wins", "losses", "opponents")
  x<-unique(temp$V2)#find all unqiue teams in V2(which are the away teams)
  y<-unique(temp$V4)#find all unqiue teams in V4(which are the home teams)
  z<-unique(c((x),(y))) #Combine x and y together and get rid of any duplicates
  datetemp<-1960
  propr<-c(1:length(z))
  names(propr)<-z
  for(f in 1:length(z)){
    wins=0
    losses=0
    opponents<-character()#for now I have want to make a list of the name of opponents and then afterwards assign them a number based on their position in the data frame
    wintemp<-which(temp$V2==z[f])#for each team, create a list of which rows where they are the away team
    wintemp2<-which(temp$V4==z[f])#for each team, create a list of which rows where they are the home team
    if(length(wintemp)!=0){
      for(j in 1:length(wintemp)){ #go through the rows in which team[f] played as away team
      if(temp$awaywin[wintemp[j]]==1){ #if team[f] won as the away team
        wins=wins+1
        opponents<-append(opponents,temp$V4[wintemp[j]])
      }
      else{#if team[f] lost as the away team
        losses=losses+1
        opponents<-append(opponents,temp$V4[wintemp[j]])
      }
      }
    }
    if(length(wintemp2)!=0){
      for(h in 1:length(wintemp2)){ #go through the rows in which team[f] played as home team
      if(temp$homewin[wintemp2[h]]==1){#if team[f] won as the home team
        wins=wins+1
        opponents<-append(opponents,temp$V2[wintemp2[h]])
      }
      else{#if team[f] lost as the home team
        losses=losses+1
        opponents<-append(opponents,temp$V2[wintemp2[h]])
      }
    }
    }
    
    #create a list to put into dataframe
    opponentsn<-c(propr[opponents])
    final<-list(opponentsn)
    good<-data.frame(z[f],datetemp,wins,losses)
    good$opponents = final
    colnames(good)<-c("teams","season", "wins", "losses", "opponents")
    df3=rbind(df3,good)
  }
  return(df3)
}

#generate the dataframe by grabbing the text file from the site
for (i in 1:51){
  temp<- read.fwf(paste(file,dates[i],file2, sep = ""),colcounts)
  temp<-cbind(temp,dates[i])
  temp$V2<-gsub(" ","",temp$V2)#removing white space from each Row
  temp$V3<-gsub(" ","",temp$V3)
  temp$V4<-gsub(" ","",temp$V4)
  temp$V5<-gsub(" ","",temp$V5)
  temp<-Div2(temp)#
  x<-temp$V3==temp$V5 #drop the ties from the data
  testx<-grep("TRUE",x)
  if(dates[i]<1996){#ties were eliminated in 1996 due to a rule change
    temp<-temp[-testx,]
  }
  temp<-winsandloses(temp)
  temp<-GenerateWins(temp)
  temp$season<-dates[i]
  df2=rbind(df2,temp)
}
save(df2, file = "project#3df.rdata")
#df2 is the final data framed saved here, has 5 columns.
#Column teams are the names of the college 
#column Season disgnates which season of football this row is refering to, from 1960 till 2010
#column wins denotes how many games the team won in that season
#column losses denotes how many games the team lost in that season
#column opponents is a list of numebers. The number that refer to the opponents
  # who played against the team in column teams. 
