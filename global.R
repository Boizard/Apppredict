usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("e1071")#svm
usePackage("ggplot2")#Graphs

importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
  # datapath: path of the file
  #extention: extention of the file : csv, xls, ou xlsx
  if(extension=="csv"){
    toto <- read.csv2(datapath,header = F,sep =sep,dec=dec,na.strings = NAstring,stringsAsFactors = F)
  }
  if(extension=="xlsx"){
    options(warn=-1)
    file.rename(datapath,paste(datapath, ".xlsx", sep=""))
    options(warn=0)
    toto <-read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = 1)
    
  }
  toto<-as.data.frame(toto)
  return(toto)
}
downloaddataset<-function(x,file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  if(ext=="csv"){
    write.csv2(x,file)
  }
  if(ext=="xlsx"){
    write.xlsx(x,file)
  }
  
}
transformdata<-function(toto,nrownames=1,ncolnames=1,transpose,zeroegalNA,log){
  if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
    toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  #remove empty rows
  if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
    toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #remove empty columns
  
  if(ncolnames!=0){
    colnames(toto)<-renamvar(toto[ncolnames,])
    toto<-toto[-ncolnames,]
  }
  if(nrownames!=0){
    rownames(toto)<-renamvar(toto[,nrownames])
    toto<-toto[,-nrownames]
  }
  if(transpose){toto<-t(toto)}
  
  if(zeroegalNA){toto[which(toto==0,arr.ind = T)]<-NA}
  
  toto<-as.data.frame(toto)
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

densityscore<-function(score,scorepredict,maintitle="Density learning's score and prediction score",threshold,graph=T){
  x<-density(score)$x
  y<-density(score)$y
  xddf <- data.frame(x=x,y=y)
  x<-scorepredict
  y<-rep(x = 0.1,length=length(scorepredict) )
  coordpredict<- data.frame(x=x,y=y)
  if(!graph){rawdata<-data.frame(xcurve=xddf$x,ycurve=xdff$y,xpoints=coordpredict$x,ypoints=coordpredict$y)}
  qplot(x,y,data=xddf,geom="line",xlab = "score",ylab="density of learning's score")+
    geom_ribbon(data=subset(xddf ,x>min(density(score)$x) & x<threshold),aes(x=x,ymin=0,ymax=y,fill="blue"),
                colour=NA,alpha=0.5)+
    geom_ribbon(data=subset(xddf ,x>threshold & x<max(density(score)$x)),aes(x=x,ymin=0,ymax=y,fill="red"),
                colour=NA,alpha=0.5)+
    geom_point(data = coordpredict, colour = "black",size=rep(4,length(x)))+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))+theme(legend.position = "bottom") +
    scale_fill_manual(name='density',
                      values=c(alpha("blue",alpha = 0.1),alpha("red",alpha = 0.5)),
                      labels=c("negativ","positiv"))+guides(colour = guide_legend(override.aes = list(alpha = 0.5)))
}