###########################################
#Source code to create judge-case observations for the Songer Court of Appeals Database
#
#by Doug M. Johnson and David A. Gelman
#Originally coded on 07/22/2014 and updated on 09/22/2016
###########################################
###########################################

rm(list=ls())

library(foreign)#to read .dta files
library(reshape)#to renaming columns in dataframes

#opton to set your working directory
#setwd('~/CoA Data Write-Up')

#cta96_stata.dta is available from the Judicial Research Initiative website, at www.astsandsciences.sc.ede/poli/juri/appct.htm
frame1=read.dta('cta96_stata.dta')

#Cases are coded liberal or conservative for the issuetype dimension. For 15060 of 
#18195, the main issuetype was decided in a fashion that the coding l or c was possible.

#Identify the Conservative Votes
direct.1=subset(frame1,frame1$direct1==1)

#Identify the Liberal Votes on
direct.3=subset(frame1,frame1$direct1==3)

#Bind the 2 types back together. We now have the 15060 case on which we can make judgecase observations
direct1frame=rbind(direct.1,direct.3)

#Cases in which the directionality on the primary dimension is not discernible. 
#1167 cases coded as a 2 or "mixed"
direct.2=subset(frame1,frame1$direct1==2)
#1966 cases we not on any conventional dimensionality
direct.0=subset(frame1,frame1$direct1==0)

#15060+1167+1966=18193, so 2 cases are lost in this process. We will come back to these cases 
#and code the 2nd case dimension, if possible. 

#We don't need the entire set of information contained in the data for each case, so we subset the data by
#the columns we will eventually use. This step is not necessary and can be omitted. 
frame2=subset(frame1,select=c(casenum,year,month,day,
                              circuit,state,district,
                              method,initiate,numappel,
                              appnatpr,appbus,appnonp,
                              appfed,appsubst,appstate,
                              ap_stid,numresp,r_natpr,
                              r_bus,r_nonp,r_fed,r_subst,
                              r_state,r_stid,casetyp1,geniss,
                              direct1,majvotes,dissent,concur,
                              #The following variables are the judges for each cases vote.
                              codej1,codej2,j2vote1,j2maj1,
                              codej3,j3vote1,j3maj1,
                              codej4,j4vote1,j4maj1,
                              codej5,j5vote1,j5maj1,
                              codej6,j6vote1,j6maj1,
                              codej7,j7vote1,j7maj1,
                              codej8,j8vote1,j8maj1,
                              codej9,j9vote1,j9maj1,
                              codej10,j10vote1,j10maj1,
                              codej11,j11vote1,j11maj1,
                              codej12,j12vote1,j12maj1,
                              codej13,j13vote1,j13maj1,
                              codej14,j14vote1,j14maj1,
                              codej15,j15vote1,j15maj1))


#Codej1 identifies the judge that wrote the majority opinion for the case. Note it does not have whether
#the judge is in the majority because being the opinion writer requires this.
# Every case will remain in this frame, as every case has an opinion writer. 
j1frame=subset(direct1frame,select=c(casenum,year,month,day,
                               circuit,state,district,
                               method,initiate,numappel,
                               appnatpr,appbus,appnonp,
                               appfed,appsubst,appstate,
                               ap_stid,numresp,r_natpr,
                               r_bus,r_nonp,r_fed,r_subst,
                               r_state,r_stid,casetyp1,geniss,
                               direct1,majvotes,dissent,concur,
                               codej1))

#Rename the codej1 variable to a simple Judge ID variable, which we will rename every judge of interest
#moving forward, to be binded back together with the information we care about for each set of judge votes
colnames(j1frame)[32]="JID"

#Code the direction of the judges vote. Being the majority writer, the direction of the case is the 
#same as the direction of the judges vote. 
j1frame$votedirect=j1frame$direct1

#Again, we don't need to check if the judge is in the majority...
j1frame$majority=c(1)
#Nor that they wrote the majority opinion...
j1frame$wroteopin=c(1)

#Now that the first one is done, ie the one that looks somewhat different than the rest because of their 
#majority writing. 

#We can write a loop that does the rest of the steps for us. First, some setting up
#We want the variables that never change that we will subset on. 
sub_vars=c("casenum","year","month","day",
           "circuit","state","district",
           "method","initiate","numappel",
           "appnatpr","appbus","appnonp",
           "appfed","appsubst","appstate",
           "ap_stid","numresp","r_natpr",
           "r_bus","r_nonp","r_fed","r_subst",
           "r_state","r_stid","casetyp1","geniss",
           "direct1","majvotes","dissent","concur")

#create a place to put all of our cases, a judge-case bin, will be 34 columns wide. 
jcbin=matrix(rep(NA),1,35)
jcbin=as.data.frame(jcbin)
names(jcbin)=names(j1frame)

#write 2 loops. the first one has judges  2 and 3, which every case has. the second loop does the remaining
#cases for which there were more than 3 judges. 
for (i in 2:3){ #there are 15 judges, we have judge 1 done though, so start at 2.
  jvar1=paste("codej",i,sep="") #columns for the ith judge have the ith value in them, so creating that value for each judge
  jvar2=paste("j",i,"vote1",sep="") #same
  jvar3=paste("j",i,"maj1",sep="") #same
  j_vars=c(jvar1,jvar2,jvar3) #making a list of the ith judge values
  sub_vars2=c(sub_vars,j_vars) #making a list on which we will subset our data
  tempframe=subset(direct1frame,select=sub_vars2) #subsetting our data 
  colnames(tempframe)[32:34]=c("JID","votedirect","majority") #renaming the columns to make column names consistent across all judges
  tempframe$wroteopin=c(0) #none of these judges wrote the opinion that j1 did
  jcbin=rbind(jcbin,tempframe) #bind with our bin
}

for (i in 4:15){ #there are 15 judges, we have judge 1 thru 3 done though, so start at 4.
  jvar1=paste("codej",i,sep="")
  jvar2=paste("j",i,"vote1",sep="")
  jvar3=paste("j",i,"maj1",sep="")
  j_vars=c(jvar1,jvar2,jvar3)
  sub_vars2=c(sub_vars,j_vars)
  subframe=subset(direct1frame,select=sub_vars2) 
  colnames(subframe)[32:34]=c("JID","votedirect","majority")
  tempframe=subset(subframe,subframe$JID>0) #drop the values for all the cases in which there was not an ith judge
  tempframe$wroteopin=c(0)
  jcbin=rbind(jcbin,tempframe)
}
jcbin=jcbin[-1,] #remove the NA row from our original bin

#Write the judges all into one frame. 
jcbin=rbind(j1frame,jcbin)

#Now we return to the cases in which a 1st dimension (issue) directionality could not be discerned. \
#We check and use the second case dimension. This is 3133 observations
direct2frame=rbind(direct.0,direct.2)

#Identify the Conservative Votes (74 obs)
direct.1=subset(direct2frame,direct2frame$direct2==1)

#Identify the Liberal Votes on (66)
direct.3=subset(direct2frame,direct2frame$direct2==3)

#Bind the 2 types back together. We now have the 15060 case on which we can make judgecase observations
direct2frame=rbind(direct.1,direct.3)

#Now repeat the steps from above. 
#need a new sub_vars to reflect the directionality on the 2nd dimension...
sub2_vars=c("casenum","year","month","day",
                     "circuit","state","district",
                     "method","initiate","numappel",
                     "appnatpr","appbus","appnonp",
                     "appfed","appsubst","appstate",
                     "ap_stid","numresp","r_natpr",
                     "r_bus","r_nonp","r_fed","r_subst",
                     "r_state","r_stid","casetyp1","geniss",
                     "direct2","majvotes","dissent","concur")
j1frame2=subset(direct2frame,select=c(sub2_vars,"codej1"))
colnames(j1frame)[32]="JID"
j1frame2=rename(j1frame2,c(codej1="JID"))

#The majority writing judge...
j1frame2$votedirect=j1frame2$direct2
j1frame2$majority=c(1)
j1frame2$wroteopin=c(1)

#make our bin
jcbin2=matrix(rep(NA),1,35)
jcbin2=as.data.frame(jcbin2)
names(jcbin2)=names(j1frame2)

#judges 2 and 3
for (i in 2:3){ #there are 15 judges, we have judge 1 done though, so start at 2.
  jvar1=paste("codej",i,sep="")
  jvar2=paste("j",i,"vote1",sep="")
  jvar3=paste("j",i,"maj1",sep="")
  j_vars=c(jvar1,jvar2,jvar3)
  sub_vars2=c(sub2_vars,j_vars)
  tempframe=subset(direct2frame,select=sub_vars2)
  colnames(tempframe)[32:34]=c("JID","votedirect","majority")
  tempframe$wroteopin=c(0)
  jcbin2=rbind(jcbin2,tempframe)
}


#and judges 4-5, because there are no cases on the 2nd dimension in which more than 5 judges took part
#in the case. 
for (i in 4:5){ #there are 15 judges, we have judge 1 thru 3 done though, so start at 4.
  jvar1=paste("codej",i,sep="")
  jvar2=paste("j",i,"vote1",sep="")
  jvar3=paste("j",i,"maj1",sep="")
  j_vars=c(jvar1,jvar2,jvar3)
  sub_vars2=c(sub2_vars,j_vars)
  subframe=subset(direct2frame,select=sub_vars2)
  colnames(subframe)[32:34]=c("JID","votedirect","majority")
  tempframe=subset(subframe,subframe$JID>0)
  tempframe$wroteopin=c(0)
  jcbin2=rbind(jcbin2,tempframe)
}
#remove the NA row
jcbin2=jcbin2[-1,]
#Write the judges all into one frame. 
jcbin2=rbind(j1frame2,jcbin2)

#bind the 1st and 2nd dimension judge case observations
#1st dimension cases have a column called direct1, 2nd dimension cases have a column called
#direct2. we need to make them the same name. 
jcbin=rename(jcbin,c(direct1="issdirect"))
jcbin2=rename(jcbin2,c(direct2="issdirect"))

#write the final dataframe for judge-case observations from the Songer data. We got 46448 observations 
#These are uncleaned observations and have several errors in the judge associated with the file
jcfinal=rbind(jcbin,jcbin2)

#write the file to your desired working directory
write.csv(jcfinal,"songer_judgecases.csv")
