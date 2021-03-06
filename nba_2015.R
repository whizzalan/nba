#data=read.csv("stat.csv",sep=",")
library(RCurl)
library(XML)
x <- getURL("https://raw.githubusercontent.com/ShaneKao/nba/b4074a7377011d4de92b1743b0209a00c8ca4816/stat.csv",
            ssl.verifypeer = FALSE)
data <- read.csv(text = x)
#########找出進季後賽球隊###########
summer=function(x){
        J_E=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="0",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }        
        J_W=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="0",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }
        if(x=="Eastern"){
                nlminb(rep(0,14),J_E)$par
        }
        else{
                nlminb(rep(0,14),J_W)$par   
        }
}
#########找出第一輪晉級球隊###########
first=function(x){
        data=data[-which(data$outcome=="0"),]
        J_E=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="1",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }        
        J_W=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="1",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }
        if(x=="Eastern"){
                nlminb(rep(0.1,14),J_E)$par
        }
        else{
                nlminb(rep(0.1,14),J_W)$par   
        }
}
#########找出第二輪晉級球隊###########
semi=function(x){
        data=data[-which(data$outcome%in%c("0","1")),]
        J_E=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="2",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }        
        J_W=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="2",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }
        if(x=="Eastern"){
                nlminb(rep(0,14),J_E)$par
        }
        else{
                nlminb(rep(0,14),J_W)$par   
        }
}
#########找出第三輪晉級球隊###########
conf=function(x){
        data=data[-which(data$outcome%in%c("0","1","2")),]
        J_E=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="3",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }        
        J_W=function(theta){
                #theta=rep(0,14)
                y=ifelse(data[which(data$divsion==x),]$outcome=="3",1,0)
                x=cbind(rep(1,dim(data[which(data$divsion==x),])[1]),
                        as.matrix(data[which(data$divsion==x),][,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }
        if(x=="Eastern"){
                nlminb(rep(0,14),J_E)$par
        }
        else{
                nlminb(rep(0,14),J_W)$par   
        }
}
#########找出第final球隊###########
final=function(xx){
        data=data[-which(data$outcome%in%c("0","1","2","3")),]
        J=function(theta){
                #theta=rep(0,14)
                y=ifelse(data$outcome=="4",1,0)
                x=cbind(rep(1,dim(data)[1]),
                        as.matrix(data[,-c(1,15,16)]))%*%theta
                h=-y*log((1+exp(-x))^(-1))-(1-y)*log(1-(1+exp(-x))^(-1))        
                sum(h)
        }        
        nlminb(rep(0,14),J)$par
}
webpage<-getURL("http://www.basketball-reference.com/leagues/NBA_2015.html#all_misc_stats",
                ssl.verifypeer = FALSE)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
tablelines <- xpathSApply(pagetree, "//tr[@class='']", xmlValue)

data_pp=matrix(0,30,14)
for(i in 1:30){
        lines=strsplit(tablelines[66:95],"\n  ",fixed=FALSE, useBytes = TRUE)[[i]][-c(1,4:10,23,24)]
        lines=gsub("^\ ","",lines)
        data_pp[i,]=lines
}
data_p=as.data.frame(data_pp)
names(data_p)<-names(data)[1:14]
data_p$outcome<-rep(NA,dim(data_p)[1])
data_p$divsion<-ifelse(data_p$Team%in%c("Golden State Warriors","Portland Trail Blazers","Dallas Mavericks",
                                         "New Orleans Pelicans","Houston Rockets","San Antonio Spurs",
                                         "Sacramento Kings","Los Angeles Clippers","Memphis Grizzlies",
                                         "Phoenix Suns","Oklahoma City Thunder",
                                         "Utah Jazz","Denver Nuggets","Los Angeles Lakers","Minnesota Timberwolves"
                                         ),"Western","Eastern")

for(i in 2:14){
        data_p[,i]<-as.numeric(as.character(data_p[,i]))
}

###########################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Eastern"),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Eastern"),][,-c(1,15,16)]))%*%summer("Eastern")
#y_p=ifelse((1+exp(-x))^(-1)>0.5,1,0)
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Eastern"),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1:8),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-0
###############################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Western"),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Western"),][,-c(1,15,16)]))%*%summer("Western")
#y_p=ifelse((1+exp(-x))^(-1)>0.5,1,0)
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Western"),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1:8),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-0
###############################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),-c(1,15,16)]))%*%first("Western")
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1:4),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-1
###############################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),-c(1,15,16)]))%*%first("Eastern")
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1:4),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-1
###############################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),-c(1,15,16)]))%*%semi("Eastern")
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1:2),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-2
###############################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),-c(1,15,16)]))%*%semi("Western")
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1:2),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-2
###############################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),-c(1,15,16)]))%*%conf("Western")
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Western"&is.na(data_p$outcome)),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-3
###############################
x=cbind(rep(1,dim(data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),])[1]),
        as.matrix(data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),-c(1,15,16)]))%*%conf("Eastern")
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(data_p$divsion=="Eastern"&is.na(data_p$outcome)),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-3
##########################
x=cbind(rep(1,dim(data_p[which(is.na(data_p$outcome)),])[1]),
        as.matrix(data_p[which(is.na(data_p$outcome)),-c(1,15,16)]))%*%final("Eastern")
y_p=as.numeric((1+exp(-x))^(-1))
a=data.frame("Team"=data_p[which(is.na(data_p$outcome)),]$Team,"output"=y_p)
a=a[order(a$output),]
a=a[setdiff(1:dim(a)[1],1),]
data_p[which(data_p$Team%in%a$Team),]$outcome<-4
data_p[is.na(data_p$outcome),]$outcome<-5
#final=data_p[,c("Conference","Team","outcome")]
final=data_p[order(data_p$divsion,data_p$outcome,decreasing=TRUE),c("Team","outcome","divsion")]
final[which(final$outcome==0),"outcome"]<-"Enjoy your summer"
final[which(final$outcome==1 & final$divsion=="Eastern"),"outcome"]<-"Lost in Eastern Conference first round"
final[which(final$outcome==1 & final$divsion=="Western"),"outcome"]<-"Lost in Western Conference first round"
final[which(final$outcome==2 & final$divsion=="Eastern"),"outcome"]<-"Lost in Eastern Conference semifinals"
final[which(final$outcome==2 & final$divsion=="Western"),"outcome"]<-"Lost in Western Conference semifinals"
final[which(final$outcome==3 & final$divsion=="Eastern"),"outcome"]<-"Lost in Eastern Conference finals"
final[which(final$outcome==3 & final$divsion=="Western"),"outcome"]<-"Lost in Western Conference finals"
final[which(final$outcome==4),"outcome"]<-"Lost in NBA Finals"
final[which(final$outcome==5),"outcome"]<-"Won NBA Finals"
table1=final[,c("divsion","Team","outcome")]


