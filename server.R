options(shiny.maxRequestSize=30*1024^2) 
source("global.R")
shinyServer(function(input, output,session) {    
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
  
  output$predictionfile<-renderText({
    namelearn<-input$predictionfile$name
  })
  
MODEL<-reactive({
  if(is.null(input$modelfile)){return(NULL)}
  else{
  load(file = input$modelfile$datapath)
  res<<-state
  }
  
})

  PREDICT<-reactive({
    if(is.null(input$predictionfile)){return(data.frame())}#Pas de fichier
    if(!is.null(input$predictionfile)  ){
      res<-MODEL()
      print(class(res))
      
      if(input$confirmdatabuttonpred==0){
        datapath<- input$predictionfile$datapath
        tabprediction<<-importfile(datapath = datapath,extension = input$filetypepred,
                              NAstring=input$NAstringpred,sheet=input$sheetnpred,skiplines=input$skipnpred,dec=input$decpred,sep=input$seppred)
        if(input$changedata){
          tabprediction<<-transformdata(toto = tabprediction,nrownames=input$nrownamespred,ncolnames=input$ncolnamespred,
                                   transpose=input$transposepred,zeroegalNA=input$zeroegalNApred)
        }
        resprediction<-data.frame()
        print(dim(tabprediction))
      }
      if(input$confirmdatabuttonpred!=0){
        for (i in 1:ncol(tabprediction)){
         tabprediction[,i]<-as.numeric(as.character(tabprediction[,i]))
        }
         learning<-res$model$decouverte$decouvdiff
         select<-which(colnames(tabprediction)%in%colnames(learning))
         tabprediction<-tabprediction[,select]
  
         if(res$parameters$NAstructure){
           varstructure<-res$varstructure
           for (i in 1:length(varstructure)){
             tabprediction[is.na(tabprediction[,varstructure[i]]),varstructure[i]]<-0 }
         }
      
         if(res$parameters$log) { 
           tabprediction<-log(x = tabprediction+1,base = 2)}
         if(res$parameters$scaled){
           tabprediction<-scale(tabprediction, center = F, scale = TRUE)
         }
         #if ( !"class"%in%colnames(tabprediction)){
           class<-rep(NA,times=nrow(tabprediction))
         tabprediction<-cbind(class,tabprediction)
  
         alldata<-rbind(tabprediction,learning)
       if(res$parameters$rempNA=="moygr"){ 
           print("impossible de remplacer les NA par la moy par group pour la validation")
           tabpredictionssNA<-replaceNA(toto = tabprediction,rempNA ="moy")        }
         else{tabpredictionssNA<-replaceNA(toto = tabprediction,rempNA =res$parameters$rempNA)}
      tabprediction<-tabpredictionssNA[,-1]
      ######prediction
      lev<-res$model$groups
      model<-res$model$model
      if(res$parameters$model=="randomforest"){
        score <-predict(object=model,type="prob",newdata = tabprediction)[,lev["positif"]]
        predictclass<-vector(length = length(score) ) 
        predictclass[which(score>=res$parameters$thresholdmodel)]<-lev["positif"]
        predictclass[which(score<res$parameters$thresholdmodel)]<-lev["negatif"]
        predictclass<-as.factor(predictclass)
  
      }
  
      if(res$parameters$model=="svm"){
        score =attr(predict(model,newdata =  tabprediction,decision.values=T),"decision.values")
        if(sum(lev==(strsplit(colnames(score),split = "/")[[1]]))==0){score<-score*(-1)}
        
        colnames(score)
        predictclass<-vector(length = length(score) ) 
        predictclass[which(score>=res$parameters$thresholdmodel)]<-lev["positif"]
        predictclass[which(score<res$parameters$thresholdmodel)]<-lev["negatif"]
        predictclass<-as.factor(predictclass)
  
      }
      if(sum(lev==(levels(predictclass)))==0){
        predictclass<-factor(predictclass,levels = rev(levels(predictclass)),ordered = TRUE)
      }
  
      resprediction<-data.frame("score"=score,"predictclass"=predictclass)
  
      colnames(resprediction)<-c("score","predictclass")
      }
    }
    parameters<<-res$parameters
    list("tab"=tabprediction,"parameters"=parameters,"resprediction"=resprediction)
    
  }) 
   
  #####
  output$JDDpredict=renderDataTable({
    predict<-PREDICT()$tab
    colmin<-min(ncol(predict),100)
    rowmin<-min(nrow(predict),100)
    cbind(Names=rownames(predict[1:rowmin,1:colmin]),predict[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10))
  
  output$downloaddataJDDpredict <- downloadHandler(
    filename = function() { paste('dataset', '.','csv', sep='') },
    content = function(file) {
      downloaddataset(   PREDICT()$tab, file) })
  
  output$parameters=renderTable({
    parameters<-PREDICT()$parameters
    cbind(Names=rownames(parameters),parameters)})
  
  output$resprediction=renderTable({
    resprediction<-PREDICT()$resprediction
    resprediction}) 
  
  output$downloaddataresprediction <- downloadHandler(
    filename = function() { paste('dataset', '.','csv', sep='') },
    content = function(file) {
      downloaddataset(   PREDICT()$resprediction, file) })
  
  
  output$plotscorepred <- renderPlot({
    scorepredict<-PREDICT()$resprediction$score
    score<-res$model$decouverte$resmodeldecouv$scoredecouv
    thresholdmodel<-res$parameters$thresholdmodel
    densityscore(score,scorepredict,maintitle="Density learning's score and prediction score",threshold=thresholdmodel)
    })
  
  output$downloadplotscorepred = downloadHandler(
    filename = function() { 
      paste('graph','.',input$paramdownplot, sep='') 
    },
    content = function(file) {
      ggsave(file, plot =        densityscore(res$model$decouverte$resmodeldecouv$scoredecouv,PREDICT()$resprediction$score,
                                              maintitle="Density learning's score and prediction score",threshold=res$parameters$thresholdmodel), 
             device = 'jpg')
      
    },
    contentType=NA)

}) 
