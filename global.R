usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("e1071")#svm
usePackage("ggplot2")#Graphs
usePackage("readxl")
usePackage("missMDA")#imputepca
usePackage("stats")
usePackage("pROC")#roccurve
usePackage("devtools")
usePackage("reshape2")#melt function
usePackage("xlsx")#import fichier xls#Fonctions
usePackage("randomForest")
usePackage("missForest")
usePackage("Hmisc")
usePackage("corrplot")
usePackage("penalizedSVM")
  
  
importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
  # datapath: path of the file
  #extention: extention of the file : csv, xls, ou xlsx
  if(extension=="csv"){
    toto <- read.csv2(datapath,header = T,sep =sep,dec=dec,na.strings = NAstring,stringsAsFactors = F,row.names=1,check.names = F )
  }
  if(extension=="xlsx"){
    options(warn=-1)
    filerm<<-file.rename(datapath,paste(datapath, ".xlsx", sep=""))
    options(warn=0)
    toto <-read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = T,skip = skiplines,sheet = sheet)
    # toto <-read.xlsx2(file = datapath,sheetIndex = sheet)
    #toto <-read_excel(datapath,na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
    rnames<-as.character(as.matrix(toto[,1]))
    toto<-toto[,-1]
    row.names(toto)<-rnames
    
  }
  toto<-as.data.frame(toto)
  return(toto)
}
downloaddataset<-function(x,file,cnames=T,rnames=T){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  print(ext)
  if(ext=="csv"){
    write.table(x,file,sep = ",",dec=".",col.names = cnames,row.names = rnames)
  }
  if(ext=="xlsx"){
    write.xlsx(x,file,col.names = cnames,row.names =rnames )
  }
}

transformationlog<-function(x,logtype){
  if(logtype=="log10"){x<-log10(x)}
  if(logtype=="log2"){x<-log2(x)}
  if(logtype=="logn"){x<-log(x)}
  return(x)
}

downloadplot<-function(file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  
  if(ext=="png"){
    png(file)
  }
  if(ext=="jpg"){
    jpeg(file)
  }  
  if(ext=="pdf"){
    pdf(file) 
  }     
}
transformdata<-function(toto,transpose,zeroegalNA){
  #   if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
  #     toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  #   #remove empty rows
  #   if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
  #     toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #   #remove empty columns
  
  
  if(transpose){toto<-t(toto)}
  
  if(zeroegalNA){toto[which(toto==0,arr.ind = T)]<-NA}
  
  #toto<-as.data.frame(toto)
  toto<-as.data.frame(toto[,sort(colnames(toto))])
}

renamvar<-function(names){
  #rename the duplicate name by adding ".1, .2 ....
  #toto is a vector of the col names of the tab
  names[is.na(names)]<-"NA"
  for(i in 1:length(names)){
    ind <- which(names%in%names[i])
    if(length(ind)>1){
      nb<-c(1:length(ind))
      newnames<-paste(names[ind],".",nb,sep="")
      
      names[ind]<-newnames
    }
  }
  return(names)
}


replaceNA<-function(toto,rempNA="z",pos=F,NAstructure=F,thresholdstruct=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){ 
  #rempNA: remplace Non ATtributes values by zero("z"), the mean of the colum (moy), 
  # the mean in each group define by the factor of the first column(moygr), itarative pca (pca), or keep th NA
  if(NAstructure){
    totoNAstruct<-replaceproptestNA(toto = toto,threshold = thresholdstruct ,rempNA =rempNA,maxvaluesgroupmin,minvaluesgroupmax)
    toto[,colnames(totoNAstruct)]<-totoNAstruct
  }
  
  if (rempNA == "none" | sum(is.na(toto))==0 ) {return(toto)}
  cnames<-colnames(toto)
  class<-(toto[,1])
  cat<-levels(class)
  toto<-as.data.frame(toto[,-1],optional = T)
  #toto<-apply(toto,MARGIN = 2,function(x)as.numeric(x))
  n<-ncol(toto) 
  #par default je remplace les NA par 0
  if (rempNA == "z") {
    toto[which(is.na(toto),arr.ind = T)]<-0
  }
  if (rempNA== "moy") {
    toto<-na.aggregate(toto)}
  if(rempNA=="moygr"){
    
    for (i in 1:length(cat)){
      tab<-toto[which(class==cat[i]),]
      tab<-na.aggregate(tab)
      toto[which(class==cat[i]),]<-tab
    }
    toto[which(is.na(toto) ,arr.ind = T )]<-0
  }
  if (rempNA == "pca"){
    
    #prise en compte des liaisons entre variable et de la ressemblance entre individus    
    #nb<-estim_ncpPCA(toto[,(nbqualisup+1):n],ncp.min = 0,ncp.max = 10,method.cv = "Kfold")    #take a lot time
    nindiv<-nrow(toto)
    prctnacol<-apply(X = toto,MARGIN = 2,FUN=function(x){ if(sum(!is.na(x))<=0){x<-rep(0,length=nindiv)}
      else{x}})
    toto<-imputePCA(prctnacol,ncp = min(n-1,5),method.cv="Kfold")$completeObs
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
    toto<-as.data.frame(toto)
    
  }
  if(rempNA=="missforest"){
    toto<-missForest(toto,maxiter = 5)$ximp
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
  }
  
  toto<-cbind(class,toto)
  toto[which(is.na(toto),arr.ind = T)]<-0
  
  colnames(toto)<-cnames
  
  return(toto)
}

selectprctNA<-function(toto,prctNA=100,group=F,restrictif=F){ 
  n<-ncol(toto)
  if (!group){
    NAvec<-vector(length =max(n,0) )
    for(i in 1:n){
      NAvec[i]<-  (sum(is.na(toto[,i]))/nrow(toto)  ) 
    }
    vec<-(NAvec<=(prctNA/100))
    
  } 
  
  if(group){
    nbcat<-length(levels(toto[,1]))
    tabgroup<-matrix(nrow = nbcat, ncol=n )
    for(i in 1:nbcat){
      tab<-toto[which(toto[,1]==levels(toto[,1])[i]),]
      for(j in 1:(n) ){
        tabgroup[i,j]<-(sum(is.na(tab[,j]))/nrow(tab))  
      }  
    }
    if(!restrictif){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){as.logical(max (x <= (prctNA/100))) }) 
    }
    if(restrictif){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){as.logical(min (x <= (prctNA/100))) }) 
    }
  }
  toto<-toto[,as.logical(vec)]
}

densityscore<-function(score,scorepredict,maintitle="Density learning's score and prediction score",threshold,groups,graph=T){
  x<-density(score)$x
  y<-density(score)$y
  xddf <- data.frame(x=x,y=y)
  x<-scorepredict
  y<-rep(x = 0.1,length=length(scorepredict) )
  coordpredict<- data.frame(x=x,y=y)
  if(!graph){
    rawdata<-data.frame(xcurve=xddf$x,ycurve=xddf$y,xpoints=coordpredict$x,ypoints=coordpredict$y)}
  qplot(x,y,data=xddf,geom="line",xlab = "score",ylab="density of learning's score")+
    geom_ribbon(data=subset(xddf ,x>min(density(score)$x) & x<threshold),aes(x=x,ymin=0,ymax=y,fill="blue"),
                colour=NA,alpha=0.5)+
    geom_ribbon(data=subset(xddf ,x>threshold & x<max(density(score)$x)),aes(x=x,ymin=0,ymax=y,fill="red"),
                colour=NA,alpha=0.5)+
    geom_point(data = coordpredict, colour = "black",size=rep(4,length(x)))+
    ggtitle(maintitle)+theme(plot.title=element_text( size=20))+theme(legend.position = "bottom")+
    theme(legend.text=element_text(size=20))+theme(legend.title=element_text(20))+ 
    scale_fill_manual(name="Groups : ",
                      values=c(alpha("blue",alpha = 0.1),alpha("red",alpha = 0.5)),
                      labels=c(as.character(groups[2]),as.character(groups[1])))+guides(colour = guide_legend(override.aes = list(alpha = 0.5)))
}

confirmdata<-function(toto){
  for (i in 1:ncol(toto)){
    toto[,i]<-as.numeric(as.character(toto[,i]))
  } 
  return(toto)
}

replaceNAvalidation<-function(validationdiff,toto,rempNA){
  validationdiffssNA<-as.data.frame(validationdiff)
  for(i in 1:nrow(validationdiff)){
    #print(class(validationdiffssNA))
    #print(class(replaceNAoneline(lineNA = validationdiff[i,],toto = toto,rempNA =rempNA)))
    validationdiffssNA[i,]<-replaceNAoneline(lineNA = validationdiff[i,],toto = toto,rempNA =rempNA)
    #print(i)
  }
  return(validationdiffssNA)
}

replaceNAoneline<-function(lineNA,toto,rempNA){
  alldata<-rbind(lineNA,toto)
  if(rempNA=="moygr"){ 
    #print("impossible de remplacer les NA par la moyenne par group pour la validation")
    linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA ="moy")[1,-1]        }
  
  else{linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA =rempNA)[1,-1]}
  
  return(linessNA)
}
