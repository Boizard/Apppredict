options(shiny.maxRequestSize=30*1024^2) 
options(xtable.include.colnames=F)
options(xtable.include.rownames=T)

source("global.R")
shinyServer(function(input, output,session){    
  output$modelUploaded <- reactive({
    return(!is.null(input$modelfile))
  })
  outputOptions(output, 'modelUploaded', suspendWhenHidden=FALSE)
  
  output$filepredUploaded <- reactive({
    return(!is.null(input$predictionfile))
  })
  outputOptions(output, 'filepredUploaded', suspendWhenHidden=FALSE) 
  
  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           alt="I2MC logo"))},deleteFile = F)
  
  output$structuredata<-renderImage({return(list(src="pictures/structurdata prediction application.jpg",
              contentType="image/jpeg",alt="structure of the table",width=400,height=300))},deleteFile = F)
             
  
  output$predictionfile<-renderText({
    namelearn<-input$predictionfile$name
  })
  
  output$predictionfile2<-renderText({
    namelearn<-input$predictionfile$name
  })
  output$predictionfile3<-renderText({
    namelearn<-input$predictionfile$name
  })  
MODEL<-reactive({
  if(is.null(input$modelfile)){return(NULL)}
  else{
  load(file = input$modelfile$datapath)
  }
  res<<-state
})

  PREDICT<-reactive({
    resultsmodel<<-MODEL()
    # modelparameters<<-resultsmodel$parameters$modelparameters
    predictionparameters<<-list(confirmdatabuttonpred=input$confirmdatabuttonpred,filetypepred=input$filetypepred,predictionfile=input$predictionfile,
                                NAstringpred=input$NAstringpred,sheetnpred=input$sheetnpred,skipnpred=input$skipnpred,decpred=input$decpred,seppred=input$seppred,
                                transposepred=input$transposepred,zeroegalNApred=input$zeroegalNApred,
                                log=resultsmodel$parameters$transformdataparameters$log,logtype=resultsmodel$parameters$transformdataparameters$logtype,
                                arcsin=resultsmodel$parameters$transformdataparameters$arcsin, 
                                standardization=resultsmodel$parameters$transformdataparameters$standardization,rempNA=resultsmodel$parameters$transformdataparameters$rempNA,
                                modeltype=resultsmodel$parameters$modelparameters$modeltype,invers=resultsmodel$parameters$modelparameters$invers,
                                thresholdmodel=resultsmodel$parameters$modelparameters$thresholdmodel)
    model<-resultsmodel$model$MODEL
    learningmodel<-resultsmodel$model$DATALEARNINGMODEL$learningmodel 
    if(is.null(input$predictionfile)){return(data.frame())}#Pas de fichier
    if(!is.null(input$predictionfile)  ){
      
        datapath<<- predictionparameters$predictionfile$datapath
        prediction<<-importfile(datapath = datapath,extension = predictionparameters$filetypepred,
                              NAstring=predictionparameters$NAstringpred,sheet=predictionparameters$sheetnpred,
                              skiplines=predictionparameters$skipnpred,dec=predictionparameters$decpred,sep=predictionparameters$seppred)
        prediction<-transformdata(toto = prediction,transpose=predictionparameters$transposepred,zeroegalNA=predictionparameters$zeroegalNApred)
        

        if(input$confirmdatabuttonpred!=0){
        prediction<-confirmdata(toto=prediction)
        datastructuresfeatures<-resultsmodel$selectdata$DATASTRUCTUREDFEATURES
        structuredfeatures<-resultsmodel$selectdata$STRUCTUREDFEATURES
        predictiondiff<-prediction[,which(colnames(prediction)%in%colnames(learningmodel))]
        learningselect<-resultsmodel$selectdata$LEARNINGSELECT
        if(resultsmodel$transformdata$transformdataparameters$log) { 
          predictiondiff<-transformationlog(x = predictiondiff+1,logtype =resultsmodel$transformdata$transformdataparameters$logtype )
          learningselect[,-1]<-transformationlog(x = learningselect[,-1]+1,logtype=resultsmodel$transformdata$transformdataparameters$logtype)}
        
        if(resultsmodel$transformdata$transformdataparameters$arcsin){
          maxlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = max,na.rm=T)
          minlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = min,na.rm=T)
          for (i in 2:dim(predictiondiff)[2]){
            predictiondiff[,i]<-(predictiondiff[,i]-minlearn[i-1])/(maxlearn[i-1]-minlearn[i-1])
        #validationdiff[,-1]<-apply(X = as.data.frame(validationdiff[,-1]),MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
            predictiondiff[which(predictiondiff[,i]>1),i]<-1
            predictiondiff[which(predictiondiff[,i]<0),i]<-0
            predictiondiff[,i]<-asin(sqrt(predictiondiff[,i]))

        }
          learningselect[,-1]<-apply(X = learningselect[,-1],MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
          learningselect[,-1]<-asin(sqrt(learningselect[,-1]))
 
        }
        if(resultsmodel$transformdata$transformdataparameters$standardization){
          learningselectval<<-learningselect
          sdselect<-apply(learningselect[,which(colnames(learningselect)%in%colnames(predictiondiff))], 2, sd,na.rm=T)
          predictiondiff<-scale(predictiondiff,center=F,scale=sdselect[-1])
        }
          #NAstructure if NA ->0
          if(!is.null(datastructuresfeatures)){
            predictiondiff[which(is.na(predictiondiff),arr.ind = T)[which(which(is.na(predictiondiff),arr.ind = T)[,2]%in%which(colnames(predictiondiff)%in%datastructuresfeatures$names)),]]<-0
          }
          #
          predictionmodel<- replaceNAvalidation(predictiondiff,toto=learningmodel[,-1],rempNA=predictionparameters$rempNA)
          
          lev<-levels(x = learningmodel[,1])
          names(lev)<-c("positif","negatif")
          
          #prediction a partir du model
          if(predictionparameters$modeltype=="randomforest"){
            scoreprediction <-predict(object=model,type="prob",newdata = predictionmodel)[,lev["positif"]]
            predictclassprediction<-vector(length = length(scoreprediction) ) 
            predictclassprediction[which(scoreprediction>=predictionparameters$thresholdmodel)]<-lev["positif"]
            predictclassprediction[which(scoreprediction<predictionparameters$thresholdmodel)]<-lev["negatif"]
            predictclassprediction<-as.factor(predictclassprediction)
            
          }
          
          if(predictionparameters$modeltype=="svm"){
            scoreprediction =attr(predict(model,newdata =  predictionmodel,decision.values=T),"decision.values")
            if(sum(lev==(strsplit(colnames(scoreprediction),split = "/")[[1]]))==0){scoreprediction<-scoreprediction*(-1)}
            predictclassprediction<-vector(length = length(scoreprediction) ) 
            predictclassprediction[which(scoreprediction>=predictionparameters$thresholdmodel)]<-lev["positif"]
            predictclassprediction[which(scoreprediction<predictionparameters$thresholdmodel)]<-lev["negatif"]
            predictclassprediction<-as.factor(predictclassprediction)
          }
          
#           if(sum(lev==(levels(predictclassprediction)))==0){
#             predictclassprediction<-factor(predictclassprediction,levels = rev(levels(predictclassprediction)),ordered = TRUE)
#           }
#           classprediction<- validation[,1]
#           if(sum(lev==(levels(classval)))==0){
#             classval<-factor(classval,levels = rev(levels(classval)),ordered = TRUE)
#           }
#           
#           #levels(predictclassval)<-paste("test",levels(predictclassval),sep="")
#           levels(predictclassval)<-paste("test",lev,sep="")
          respredictiontionmodel<<-data.frame(predictclassprediction,scoreprediction)
          colnames(respredictiontionmodel) <-c("predictclassprediction","scoreprediction") 
          datapredictionmodel<-list("predictionmodel"=predictionmodel,"respredictiontionmodel"=respredictiontionmodel,"groups"=lev)
        }
        else{datapredictionmodel=NULL
        predictiondiff=NULL}
        
      }
    
    list(prediction=prediction,predictiondiff=predictiondiff,predictionparameters=predictionparameters,
         predictionmodel=datapredictionmodel$predictionmodel,
         respredictiontionmodel=datapredictionmodel$respredictiontionmodel,groups=datapredictionmodel$groups)
    
    
  }) 
   
  #####
  output$JDDprediction=renderDataTable({
    prediction<-PREDICT()$prediction
    colmin<-min(ncol(prediction),100)
    rowmin<-min(nrow(prediction),100)
    cbind(Names=rownames(prediction[1:rowmin,1:colmin]),prediction[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10,
                       "colnames"=FALSE))
  
  output$downloaddataJDDprediction <- downloadHandler(
    filename = function() { paste('dataset', '.','csv', sep='') },
    content = function(file) {
      downloaddataset(   PREDICT()$prediction, file) })
  
  output$JDDpredictiondiff=renderDataTable({
    predictiondiff<-PREDICT()$predictiondiff
    colmin<-min(ncol(predictiondiff),100)
    rowmin<-min(nrow(predictiondiff),100)
    cbind(Names=rownames(predictiondiff[1:rowmin,1:colmin]),predictiondiff[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10,
                       "colnames"=FALSE))
  
  output$downloaddataJDDpredictiondiff <- downloadHandler(
    filename = function() { paste('dataset', '.','csv', sep='') },
    content = function(file) {
      downloaddataset(   PREDICT()$predictiondiff, file) })
  
output$modelparameters=renderTable({
  if(!is.null(input$modelfile)){
    settingstable<<-MODEL()$settingstable
    modelparameters<-cbind(settingstable[3:4,c(2,7)],settingstable[c(17,18),2:3])
    modelparameters<-t(modelparameters)
    modelparameters[,1]<-paste("<strong>",modelparameters[,1],"</strong>")
    modelparameters<-as.data.frame(modelparameters,row.names=modelparameters[,1])
    }
},sanitize.text.function=function(x){x},include.colnames=F)
  
output$modelparameters2=renderTable({
  if(!is.null(input$modelfile)){
    settingstable<<-MODEL()$settingstable
    modelparameters<-cbind(settingstable[3:4,c(2,7)],settingstable[c(17,18),2:3])
    modelparameters<-t(modelparameters)
    modelparameters[,1]<-paste("<strong>",modelparameters[,1],"</strong>")
    modelparameters<-as.data.frame(modelparameters,row.names=modelparameters[,1])
  }
},include.colnames=F,rownames=F,sanitize.text.function=function(x){x})
  
  
  output$modelmainresults=renderTable({
    if(!is.null(input$modelfile)){
    modelmainresults<-MODEL()$settingstable[c(19,20),3:8]
    modelmainresults<-as.data.frame(modelmainresults)
    modelmainresults<-t(modelmainresults)
    modelmainresults<-cbind(modelmainresults[1:3,1:2],modelmainresults[c(4:6),1:2])
    modelmainresults[,1]<-paste("<strong>",modelmainresults[,1],"</strong>")
    modelmainresults[,3]<-paste("<strong>",modelmainresults[,3],"</strong>")
    modelmainresults
    }},include.colnames=F,sanitize.text.function=function(x){x},include.rownames=F)
  
output$resprediction=renderTable({
  if(input$confirmdatabuttonpred!=0){
  res<<-cbind("Names"=rownames(PREDICT()$respredictiontionmodel),"Prediction"=as.character(PREDICT()$respredictiontionmodel[,1]),"Score"=PREDICT()$respredictiontionmodel[,2])
  res[,1]<-paste("<strong>",res[,1],"</strong>")
  res
  }
},sanitize.text.function=function(x){x},include.rownames=F) 
  
  output$downloaddataresprediction <- downloadHandler(
    filename = function() { paste('dataset', '.','csv', sep='') },
    content = function(file) {
      downloaddataset(PREDICT()$respredictiontionmodel, file) })
  
  
  output$plotscorepred <- renderPlot({
    if(input$confirmdatabuttonpred!=0){
      scorepredict<<-PREDICT()$respredictiontionmodel$scoreprediction
      score<<-MODEL()$model$DATALEARNINGMODEL$reslearningmodel$scorelearning
      thresholdmodel<<-MODEL()$parameters$modelparameters$thresholdmodel
      groups<<-PREDICT()$groups
      densityscore(score = score,scorepredict = scorepredict,maintitle="Density learning's score and prediction score",threshold=thresholdmodel,groups=groups)
    }
    })
  
  output$downloadplotscorepred = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =densityscore(score = MODEL()$model$DATALEARNINGMODEL$reslearningmodel$scorelearning,
                        scorepredict = PREDICT()$datapredictionmodel$respredictiontionmodel$scoreprediction,groups=PREDICT()$groups,
                        maintitle="Density learning's score and prediction score",threshold=MODEL()$parameters$modelparameters$thresholdmodel), 
             device = 'jpg')
      
    },
    contentType=NA)
  
  output$downloaddatascorepred <- downloadHandler(
    filename = function() { paste('dataset', '.',"csv", sep='') },
    content = function(file) {
      downloaddataset(densityscore(score = MODEL()$model$DATALEARNINGMODEL$reslearningmodel$scorelearning,
                                   scorepredict = PREDICT()$datapredictionmodel$respredictiontionmodel$scoreprediction,groups=PREDICT()$groups,
                                   maintitle="Density learning's score and prediction score",threshold=MODEL()$parameters$modelparameters$thresholdmodel,graph=F)
                      , file)
    }
  )

}) 
