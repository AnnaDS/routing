library(shiny)
source("routin_version_comparison.R")
# #options(shiny.trace=TRUE)
# # Define server logic required to draw a histogram
 #C=getCompanies()
 #C=as.character(C[-which(C=="")])
 #sc=C[1]


shinyServer(function(input, output) { 

  print('Server started')
  
  
    # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    withProgress(message = '              Getting companies...         ', min=0, max=100, detail ="", {
    comp=getCompanies()
    if(length(which(comp==""))>0)
      comp=as.character(comp[-which(comp=="")])
    sc=comp[1]
    print("try")
    print(comp)
    selectInput("company", "",comp, selected=sc)
    })
  })

  output$input_type_text <- renderText({
    input$selCmp
    isolate(
    paste("The selected company",input$company)
    )
  })

  output$text1 <- renderText({ 
    input$goButton
   # isolate(
    paste("The selected initial version", input$vers1)
    #)
  })
output$text2 <- renderText({ 
  input$goButton
 # isolate(
    paste("The selected initial version", input$vers2)
  #)
})
  output$vers1 <- renderText({ 
   # input$selCmp
    input$goButton
    isolate(
    paste("The selected stage version", input$vers1))
  })
output$vers2 <- renderText({ 
  #input$selCmp
  input$goButton
  isolate(
    paste("The selected stage version", input$vers2))
})
output$env1 <- renderText({ 
  #input$selCmp
  input$goButton
  isolate(
    paste("The selected init environment", input$env1))
})
output$env2 <- renderText({ 
  #input$selCmp
  input$goButton
  isolate(
    paste("The selected stage environment", input$env2))
})
  output$text3 <- renderText({ 
    input$selCmp
    #input$goButton
    isolate(
    paste("The selected company", input$company))
  })
  output$text4 <- renderText({ 
      paste("Results of assessment")
  })
  output$dates <- renderText({ 
    #input$selCmp
    input$goButton
    isolate(
      paste("The selected dates", input$dates))
  })

output$textR <- renderText({ 
  input$expert
  isolate(
    paste("Routing plan", input$rout_plan))
})
  output$comp <- renderText(paste("Select company"))
  
 # output$comp.names <- getCompanies()
  
  
  datasetInput1 <- reactive({
   # input$selCmp
    switch(input$dataset1,
           "2.0.7" = v1,
           "2.1.1" = v2,
           "2.1.0" = v3)
  })


  
  datasetInput2 <- reactive({
   # input$selCmp
    switch(input$dataset2,
           "2.0.7" = v1,
           "2.1.1" = v2,
           "2.1.0" = v3)
  })
  
  output$caption<-renderText({
    switch(input$plot.type,
           "Assigned_activities"   =   "Assigned_activities",
           "Overdue_time" =	"Overdue_time",
           "Travel_time" 	=	"Travel_time",
           "SLA_violation" 		=	"SLA_violation",
           "Percent_of_pure_revenue"="Percent_of_pure_revenue",
           "Work_time" = "Work_time", 
           "Rout_density"="Rout_density",
           "Overtime"="Overtime",
           "Revenue"="Revenue")
  })

  plot.type<-reactive({
    #input$selCmp
    input$goButton1
    isolate(
    switch(input$plot.type,
                    "Assigned_activities"   = 	"Assigned_activities",
                    "Overdue_time" =	 "Overdue_time",
                    "Travel_time" 	=	"Travel_time",
                    "SLA_violation" 		=	"SLA_violation",
           "Percent_of_pure_revenue"="Percent_of_pure_revenue",
           "Work_time" = "Work_time", 
           "Rout_density"="Rout_density",
           "Overtime"="Overtime",
           "Revenue"="Revenue"))
  })
  
 Dates <- reactive({
   #input$selCmp
    input$goButton
    isolate(input$dates)
  })
 
 Dates2 <- reactive({
   #input$selCmp
   input$goButton
   isolate(input$dates2)
 })

D <-  reactive({
  if(input$selCmp==0) return()
  else {D=getDBD(cmp=as.character(input$company ))
        print("DB selection2")      
        if(nrow(D)==0)
          return ()
        else return (D)
  }
})
output$textD1 <- renderDataTable({ 
  input$selCmp
  withProgress(message = 'Getting information...                                         ', min=0, max=100, detail ="", {
  D <- D()
  print(paste("Data", D))
  if(is.null(D))
    HTML("There is no data for the company", input$company)
  else {
    #print(D)
    D[,seq(2, ncol(D))]
  }
  })
}#, options = list(lengthMenu = c(5, 10), pageLength = 5)
)

x    <-  reactive({
                   if(input$selCmp==0 | input$goButton==0)return()
                   
                   # isolate(getALLRes1(input$dataset1, input$dataset2, cmp=as.character(input$cmp ), input$dates, input$dates2))
                   # isolate(getALLRes1(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2))
                   isolate( getALLResF(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2, input$env1, input$env2, test=input$outliers1, ED=input$method)) }) 

xp    <-  reactive({
                   if(input$expert==0)return()
                   isolate( 
getALLResF(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2, input$env1, input$env2, rp=input$rout_plan,test=input$outliers1, ED=input$method)) }) 



cap    <-  reactive({ if(input$selCmp==0 || input$goButton==0)return()
                    isolate(getCapacity(input$vers1, input$vers2, cmp=as.character( input$company ), input$dates, input$dates2, input$env1, input$env2))}) 
#x    <-  x()
#y=data.frame(as.character(input$dataset1)=median(x[,2]), as.character(input$dataset2)=median(x[,3]))
output$fintable = renderDataTable({
 # input$selCmp
  input$goButton
  if (input$goButton==0) return()
  withProgress(message = '                  Calculate the assessments...                                         ', value = 0.1,detail ="", {
  isolate({
#  isolate({

    x    <-  x()
    print(head(x))
    validate(
      need(!is.null(x), "There is no data! Please check all selection options!")
    )
    print("OKKKK! fin")
    if(input$outliers1==TRUE){
      print("Out found")
      #y=data.frame(N1=sum(as.numeric(x[,3]), na.rm=TRUE),vers1=median(as.numeric(x[,4]), na.rm=TRUE),vers2=median(as.numeric(x[,5]), na.rm=TRUE), N2=sum(as.numeric(x[,6]), na.rm=TRUE))
      x=x[which(as.numeric(x[,2])!=0), ]
#       y=data.frame(N1=sum(as.numeric(x[,3])),
#                    vers1=sum(as.numeric(x[,3])*as.numeric(x[,4]))/sum(as.numeric(x[,3])),
#                    vers2=sum(as.numeric(x[,5])*as.numeric(x[,6]))/sum(as.numeric(x[,6])), 
#                    N2=sum(as.numeric(x[,6])))
#print(x)
x$total=sapply(x$total, as.character)
x$total=sapply(x$total, as.numeric)

dd=abs(x$Val1-x$Val2)
bd=which(dd>(-0.001*log(x$total)+0.0095))
x1=x[bd,]
      y=data.frame(N1=sum(as.numeric(x[,3])),
                    vers1=mean(as.numeric(x1[,4])),
                    vers2=mean(as.numeric(x1[,5])), 
                    #vers1=mean(as.numeric(x[which(abs(x[,4]-x[,5])>0.014),4])),
                   #vers2=mean(as.numeric(x[which(abs(x[,4]-x[,5])>0.014),5])), 
                   N2=sum(as.numeric(x[,6])))
      colnames(y)=c("N1",input$vers1, input$vers2, "N2")
      colnames(y)=c("N1",substr(colnames(y)[2],1,5), substr(colnames(y)[3], 1,5),"N2" )
      y[,2]=round(as.numeric(as.character(y[,2])),4)
      y[,3]=round(as.numeric(as.character(y[,3])),4)
#print(y)
    }
    else{
      print("")
#  y=data.frame(N1=sum(as.numeric(x[,2]), na.rm=TRUE),vers1=median(as.numeric(x[,3]), na.rm=TRUE),vers2=median(as.numeric(x[,4]), na.rm=TRUE), N2=sum(as.numeric(x[,5]), na.rm=TRUE))
if(nrow(x[which(x[,2]==0 && x[,5]==0 ),])>0) x=x[-(which(x[,2]==0 && x[,5]==0 )),]
#       y=data.frame(N1=sum(as.numeric(x[,2])),
#                vers1=sum(as.numeric(x[,3])*as.numeric(x[,2]))/sum(as.numeric(x[,2])),
#                vers2=sum(as.numeric(x[,4])*as.numeric(x[,5]))/sum(as.numeric(x[,5])), 
#                N2=sum(as.numeric(x[,5])))
#print(x)  
y=data.frame(N1=sum(as.numeric(x[,2])),
#              vers1=mean(as.numeric(x[which(abs(x[,3]-x[,4])>0.014),3]), na.rm = TRUE),
#              vers2=mean(as.numeric(x[which(abs(x[,3]-x[,4])>0.014),4]), na.rm = TRUE),
             vers1=mean(as.numeric(x[,3]), na.rm = TRUE),
             vers2=mean(as.numeric(x[,4]), na.rm = TRUE),
             N2=sum(as.numeric(x[,5])))

  colnames(y)=c("N1",input$vers1, input$vers2, "N2")
  colnames(y)=c("N1",substr(colnames(y)[2],1,5), substr(colnames(y)[3], 1,5),"N2" )
  y[,2]=round(as.numeric(as.character(y[,2])),4)
  y[,3]=round(as.numeric(as.character(y[,3])),4)
#print(y)S
    }
  #y=cbind(y, N=nrow(x))
 # print(head(y))
  return(y)
})
  }) 
}, options = list(lengthMenu = c(5, 10, 30, 50), pageLength = 5))

output$General  =  renderDataTable({
  
  input$allReport
  
  if (input$allReport==0) return()
  isolate({
    comp=getCompanies()
    print ('Companies!!!!')
    print(comp)
    #comp=comp[-which(comp=="")])
    #comp=as.character(comp[-which(comp=="")])
    D=NULL
   
    for(i in 1:length(comp)){
      print(comp[i])
      #print(paste(input$vers1, input$vers2, cmp=as.character(comp[i] ), input$dates, input$dates2, input$env1, input$env2))
      CR=getALLResF(input$vers1, input$vers2, cmp=as.character(comp[i] ), input$dates, input$dates2, input$env1, input$env2, test=input$outliers1, ED=input$method)
      #print(CR)
      #print(is.null(CR))
      if(!is.null(CR)){
        if (input$outliers1==TRUE) {
          CR$total=sapply(CR$total, as.character)
          CR$total=sapply(CR$total, as.numeric)
          
          dd=abs(CR$Val1-CR$Val2)
          bd=which(dd>(-0.001*log( CR$total)+0.0095))
          CR=CR[bd,]
          k=3
                                    } else k=2
        #y=data.frame(N1=sum(as.numeric(CR[,k]), na.rm=TRUE),vers1=mean(CR[,k+1],na.rm=TRUE),vers2=mean(CR[,k+2],na.rm=TRUE), N2=sum(as.numeric(CR[,k+3]), na.rm=TRUE))
       
        y=data.frame(N1=sum(as.numeric(CR[,k]), na.rm=TRUE),
                   #  vers1=mean(CR[which(abs(CR[,k+2]-CR[,k+1])>0.014),k+1],na.rm=TRUE),
                  #   vers2=mean(CR[which(abs(CR[,k+2]-CR[,k+1])>0.014),k+2],na.rm=TRUE), 
                     vers1=mean(CR[,k+1],na.rm=TRUE),
                     vers2=mean(CR[,k+2],na.rm=TRUE), 
                     N2=sum(as.numeric(CR[,k+3]), na.rm=TRUE))
        
        
        colnames(y)=c("N1",paste(input$env1,substr(input$vers1,1,5)), paste(input$env2, substr(input$vers2,1,5)), "N2")
        colnames(y)=c("N1",substr(colnames(y)[2],1,5), substr(colnames(y)[3], 1,5),"N2" )
        y[,2]=round(as.numeric(as.character(y[,2])),4)
        y[,3]=round(as.numeric(as.character(y[,3])),4)
        
        if(is.null(D)) D=cbind(company=rep(as.character(comp[i] ),nrow(y)),y)
        else D=rbind(cbind(company=rep(as.character(comp[i] ),nrow(y)),y), D)
      }
#       else {
#         paste("The data is not found")
#         return("The data is not found")
#       }
    }
validate(
  need(!is.null(D), "There is no data! Please check all selection options!")
)
   D=D[order(D$company),]
  print(D)
D
   # return (D)
  })
}, options = list(lengthMenu = c(5, 10, 20, 50, 100), pageLength = 5)) 


# output$finPlanTable = renderDataTable({
#   # input$selCmp
#   input$expert
#   
#   #  isolate({
#   xp    <-  xp()
#   validate(
#     need(!is.null(xp), "There is no data! Please check all selection options for Plan!")
#     )
#   if(is.null(xp)) return(HTML("There is no data! Please check all selection options!"))
#   #print(xp)
#   print("OKKKK!")
#  # print(xp)
#   #})
#   
# }, options = list(lengthMenu = c(5, 10, 20, 50, 100), pageLength = 5))


output$choose_dataset1 <- renderUI({
  input$goButton  
  withProgress(message = '               Detailed routing plan assessment calculation...                                         ', value = 0.1,detail ="", {
  isolate(
  if (input$goButton==FALSE)  
    selectInput("rout_plan", "","", selected="")
  else {
   x <-x()
validate(
      need(!is.null(x), "There is no data! Please check all selection options!")
    )
   print('Rout plan!!')
   # print(x)
   if(is.null(x)) return(selectInput("rout_plan", "","", selected=""))
    x=as.data.frame(x)
    rp=as.character(x[,1])
    rc=rp[1]
  #  print(rp)
    selectInput("rout_plan", "",rp, selected=rc)
  }
  )
  })
})

output$figPlot <- renderPlot({
 # input$selCmp
  input$goButton
   # x    <-  reactive({ x=getALLRes1(input$dataset1, input$dataset2, cmp=input$cpm)})
#  x    <-  reactive({ x=getALLRes1(input$dataset1, input$dataset2, cmp=input$cpm)}) 
  
  #reactive({ x=getALLRes1(input$dataset1, input$dataset2, cmp=input$cpm)}) 
   # print((x))
    # draw the histogram with the specified number of bins
  isolate({
    x    <-  x()
    validate(
      need(!is.null(x), "There is no data! Please check all selection options!")
    )
    plot(x[,2], col = 'red', t = 'l', lwd=3, ylim=c(min(x[,(2:ncol(x))]), max(x[,(2:ncol(x))])))
      for(i in 3:ncol(x)){
        lines(x[,i], col = i*4, t = 'l', lwd=3)
    }
    legn=c(paste("Initial", input$dataset1))
    legc='red'
    for(t in 3:ncol(x)) {lines(x[,t], t='l', col=t*4, lwd=3)
                           legn=c(legn, paste("Stage", input$dataset2[t-2]))
                           legc=c(legc, t*4)
    }
    legend('topleft', legn, col=legc, cex=0.9, lty=1, lwd=3, bty='n');
    })
  })
  
  output$distPlot <- renderPlot({
    input$goButton
    # getALLRes(input$dataset1, input$dataset2, cmp=input$cpm)  # Old Faithful Geyser data
  # print(head(x))
    # draw the histogram with the specified number of bins
  isolate({
    x    <- x()
    hist(x[,2], breaks=round(nrow(x)/10, 0), xlim=c(min(x[,(2:ncol(x))]), max(x[,(2:ncol(x))])), col='red', main='Histogram of the routing quality')
    for(i in 3:ncol(x)){
      hist(x[,i], breaks = round(nrow(x)/10, 0), col = i*4, border = i*8, add=TRUE)
    }
    legn=c(paste("Initial", input$dataset1))
    legc='red'
    for(t in 3:ncol(x)) {lines(x[,t], t='l', col=t*4, lwd=3)
                         legn=c(legn, paste("Stage", input$dataset2[t-2]))
                         legc=c(legc, t*4)
    }
    legend('topleft', legn, col=legc, cex=0.9, lty=1, lwd=3, bty='n');})
  })

output$parPlot <- renderPlot({
  input$goButton1
isolate({
 plot.type<-switch(input$plot.type,
                   "Assigned_activities"   =   1,
                   "Travel_time"=2,
                   "Travel_cost"=3,
                   "SLA_violation_per_activity"=4,
                   "Rout_density"=5,
                   "Overtime" =  6,
                   "Overtime_cost" 	=	7
 )

    x    <-  x()
 #print(x)
 validate(
   need(!is.null(x), "There is no data! Please check all selection options!")
    )
 
    #print(x)
    vmfd=read.csv(paste(input$company,"_FR.csv", sep=''), sep=',')
    if(is.null(vmfd)) {
     # p <- plot()
     # print(p)
      return ()
    }
    print("Recognize")
    #print(head(vmfd))
    assess=c("_AA","_TRT","_TRC","_SlC","_RD","_OVT","_OVC")
    assessN=c("Number of assigned activities","Travel time per activity","Travel cost per activity","SLA violation per activity","Rout density","Overtime per provider", "Overtime cost")
    vmfdS=data.frame()
    #plot.type
    for(i in 1:length(assess)){
      if(i==1) {
        vmfdS=vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])]
      }
      else{vmfdS=cbind(vmfdS, vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])])}}
#     
#     for(h in 1:ncol(vmfdS)){
#       colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
#     }
   par(mfrow=c(2,2))
   # k=1
  
    #vmfdSs= sapply(vmfdS, median, na.rm=TRUE)
vmfdSs= sapply(vmfdS, mean, na.rm=TRUE)
   #print(head(vmfdS))
   #print(paste("!!!!WWWWWWWWW",plot.type))
   k=as.numeric(plot.type)*2-1
   #print(k)
   #print(head(vmfdS))
    #for(i in 1:length(assess)){
      dtes1=rbind(cbind(as.numeric(vmfdS[,k]), rep(colnames(vmfdS)[k], nrow(vmfdS))), cbind(as.numeric(vmfdS[,(k+1)]), rep(colnames(vmfdS)[(k+1)], nrow(vmfdS))))
colnames(dtes1)=c("val","vers")
dtes1=as.data.frame(dtes1)
dtes1$val=as.numeric(as.character(dtes1$val))
#print(head(dtes1))
ds1 <- ddply(dtes1, .(vers), summarise, mean = mean(as.numeric(val), na.rm=TRUE), 
             median = median(as.numeric(val), na.rm=TRUE), sd = sd(as.numeric(val)), sum = sum(val))
# p <- ggplot(dtes1, aes(x = vers, y = val)) +
#   geom_point() +geom_boxplot()+geom_point(color='black',alpha=0.5, position = 'jitter')+
#   geom_point(data = ds1, aes(y = median),
#              colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))
print(ds1)
# p <- ggplot(dtes1, aes(x = vers, y = val)) +
#   geom_point() +geom_boxplot()+geom_point(color='white',alpha=0.5, position = 'jitter')+
#   geom_point(data = ds1, aes(y = median),
#              colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))

p <- ggplot(dtes1, aes(x = vers, y = val)) +
  geom_point() +geom_boxplot()+geom_point(color='white',alpha=0.5, position = 'jitter')+
  geom_point(data = ds1, aes(y = median),
             colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))
  print(p)

})

})
idr=c( 1,0,1,0,1,1)
output$vecPlot <- renderPlot({
  input$expert
  print(paste('Start open the file', paste(input$rout_plan,"_RP.csv", sep='')))
  if(input$expert==FALSE) return (NULL)
  else{
    par(mfrow = c(1, 2))
   print(paste('Start open the file', paste(input$rout_plan,"_RP.csv", sep='')))
    rpv=read.csv(paste(gsub("/","_",input$rout_plan)  ,"_RP.csv", sep=''), sep=',')
  
    F1=(rpv[,grep("F_",colnames(rpv))])
    S2=(rpv[,grep("S_",colnames(rpv))])
print((F1[nrow(F1),]))
print((S2[nrow(S2),]))

    plot(idr, col='red', t='l', lwd=3, main=paste(input$env1,input$vers1), ylab='Assessment', xlab='parameters')
    for(j in (1:nrow(F1))) lines(as.numeric(F1[j,]), col='blue', lwd=2)
#legend(1, 1,c("1-Travel time","2-Work time ratio", "3-Revenue ratio", "4-Overtime ratio", "5-Rout density", "Number of assigned activities"), , bty='n', cex=0.9)
    plot(idr,  col='red',t='l', lwd=3, main=paste(input$env2,input$vers2), ylab='Assessment', xlab='parameters')
    for(j in (1:nrow(S2))) lines(as.numeric(S2[j,]), col='green', lwd=2)
#legend(1, 1,c("1-Travel time","2-Work time ratio", "3-Revenue ratio", "4-Overtime ratio", "5-Rout density", "Number of assigned activities"), , bty='n', cex=0.9)

  }
})
cap    <-  reactive({ if(input$selCmp==0 || input$goButton==0)return()
                      isolate({cc=getCapacity(input$vers1, input$vers2, cmp=as.character(input$company ), input$dates, input$dates2, input$env1, input$env2)
                               assess=c("_work_time_human","_work_time_machine")
                               vmfdS=data.frame()
                               #plot.type
                               for(i in 1:length(assess)){
                                 #print(grep(assess[i], colnames(cc)))
                                 if(i==1) {
                                   vmfdS=cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])]
                                   #vmfdS=cbind(vmfdS, diff=(as.numeric(as.character(vmfdS[,ncol(vmfdS)]))-as.numeric(as.character(vmfdS[,(ncol(vmfdS)-1)]))))
                                   #colnames(vmfdS)[ncol(vmfdS)]=paste("Diff_", assess[i], sep='')
                                 }
                                 else{vmfdS=cbind(vmfdS, cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])])
                                     # vmfdS=cbind(vmfdS, diff=(as.numeric(as.character(vmfdS[,ncol(vmfdS)]))-as.numeric(as.character(vmfdS[,(ncol(vmfdS)-1)]))))
                                    #  colnames(vmfdS)[ncol(vmfdS)]=paste("Diff_", assess[i], sep='')
                                 }}
                               
                               for(h in 1:ncol(vmfdS)){
                                 if(!startsWith(colnames(vmfdS)[h], "Diff")) colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
                                 vmfdS[,h]=as.numeric(as.character(vmfdS[,h]))
                               }
                               vmfdS
                      })}) 

# output$summary <- renderTable({
#   input$goButton
#   isolate({
#     cc    <-  cap()
#   print(head(cc))
# TS=NULL
# if(!is.null(cc)){
# for(i in 1:ncol(cc)){
#   colnames(cc)[i]=gsub("initial_routing_work_","",colnames(cc)[i] )
#   colnames(cc)[i]=gsub("stage_routing_work_","",colnames(cc)[i] )
#   colnames(cc)[i]=gsub("routing_work_","",colnames(cc)[i] )
# }
# print(head(cc))
#   TS=t(summary(cc))
#   for (h in 1:ncol(TS)){
#     colnames(TS)[h]=substr(TS[1,h], 1, as.numeric(gregexpr(pattern =':',TS[,h])[[1]][1]))
#     TS[,h]=trim(gsub(colnames(TS)[h], "",TS[,h] ))
#   }
# }
#   })
#   #summary(vmfdS)
# if(!is.null(TS)) return(TS) 
# else return(data.frame(Capacity=""))
# })
# 
# output$summary1 <- renderPrint({
#   dataset <- cap()
#   for(i in 1:ncol(dataset)){
#     colnames(dataset)[i]=gsub("initial_routing_work_","",colnames(dataset)[i] )
#     colnames(dataset)[i]=gsub("stage_routing_work_","",colnames(dataset)[i] )
#   }
#   summary(dataset)
# })
# 
# output$histHT <- renderPlot({
#  # input$selCmp
#   input$goButton
#   isolate({
#     cc    <-  cap()
#     print(head(cc))
#   cc1=cc[,which(colnames(cc) %in% colnames(cc)[grep("_routing_work_time_human", colnames(cc))])]
# cc1=cc1[, -which(colnames(cc1) %in% colnames(cc1)[grep("Diff", colnames(cc1))])]
#     hist(as.numeric(cc1[,1]), breaks=round(nrow(cc1)/10, 0), xlim=c(min(cc1[,(1:ncol(cc1))]), max(cc1[,(1:ncol(cc1))])), col='red', xlab="seconds",main='Histogram of Work time human')
# hist(as.numeric(cc1[,2]), breaks=round(nrow(cc1)/10, 0), col='blue', add=TRUE)
#     legend('topright', colnames(cc1), col=c("red","blue"), cex=1, lty=1, lwd=4, bty='n');
#     
#   })
# })
# 
# output$histMT <- renderPlot({
#   #input$selCmp
#   input$goButton
#   isolate({
#     cc    <-  cap()
#     print(head(cc))
#     cc1=cc[,which(colnames(cc) %in% colnames(cc)[grep("_routing_work_time_machine", colnames(cc))])]
#     cc1=cc1[, -which(colnames(cc1) %in% colnames(cc1)[grep("Diff", colnames(cc1))])]
#     hist(as.numeric(cc1[,1]), breaks=round(nrow(cc1)/10, 0), xlim=c(min(cc1[,(1:ncol(cc1))]), max(cc1[,(1:ncol(cc1))])), col='red', xlab="seconds",main='Histogram of Work time machine')
#     hist(as.numeric(cc1[,2]), breaks=round(nrow(cc1)/10, 0), col='blue', add=TRUE)
#     legend('topright', colnames(cc1), col=c("red","blue"), cex=1, lty=1, lwd=4, bty='n');
#     
#   })
# })
# 
# output$CapPlot <- renderPlot({
#   #input$selCmp
#   input$goButton
#   # x    <- x() # getALLRes(input$dataset1, input$dataset2, cmp=input$cpm)  # Old Faithful Geyser data
#   # print(head(x))
#   # draw the histogram with the specified number of bins
#   plot.type<-switch(input$plot.type,
#                     "Assigned_activities"   =   1,
#                     "Travel_time"=2,
#                     "Travel_cost"=3,
#                     "SLA_violation_per_activity"=4,
#                     "Rout_density"=5,
#                     "Overtime" =	6,
#                     "Overtime_cost" 	=	7
#   )
#   isolate({
#     cc    <-  cap()
# print(head(cc))
#     assess=c("_routing_work_time_human","_routing_work_time_machine")
#     assessN=c("Number of assigned activities","Travel time","Travel cost","SLA violation per activity","Rout density","Overtime", "Overtime cost")
#     vmfdS=data.frame()
#     #plot.type
#     for(i in 1:length(assess)){
#       if(i==1) {
#         vmfdS=cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])]
#       }
#       else{vmfdS=cbind(vmfdS, cc[,which(colnames(cc) %in% colnames(cc)[grep(assess[i], colnames(cc))])])}}
#     
#     for(h in 1:ncol(vmfdS)){
#       colnames(vmfdS)[h]=substr(colnames(vmfdS)[h], 1,(nchar(colnames(vmfdS)[h])-2))
#     }
#     par(mfrow=c(2,2))
#     # k=1
#     
#     vmfdSs= sapply(vmfdS, median, na.rm=TRUE)
#     #print(head(vmfdS))
#     print(paste("!!!!WWWWWWWWW",plot.type))
#     k=as.numeric(plot.type)*2-1
#     print(k)
#     print(head(vmfdS))
#     #for(i in 1:length(assess)){
#     dtes1=rbind(cbind(as.numeric(vmfdS[,k]), rep(colnames(vmfdS)[k], nrow(vmfdS))), cbind(as.numeric(vmfdS[,(k+1)]), rep(colnames(vmfdS)[(k+1)], nrow(vmfdS))))
#     colnames(dtes1)=c("val","vers")
#     dtes1=as.data.frame(dtes1)
#     dtes1$val=as.numeric(dtes1$val)
#     print(head(dtes1))
#     ds1 <- ddply(dtes1, .(vers), summarise, mean = mean(val, na.rm=TRUE), median = median(val, na.rm=TRUE), sd = sd(val))
#     p <- ggplot(dtes1, aes(x = vers, y = val)) +
#       geom_point() +geom_boxplot()+geom_point(color='white',alpha=0.5, position = 'jitter')+
#       geom_point(data = ds1, aes(y = median),
#                  colour = 'red', size = 10)+ ggtitle(paste(assessN[as.numeric(plot.type)]))
#     print(p)
#   })
#   
# })


output$mytable1 = renderDataTable({
  input$goButton
  if (input$goButton==0) return()
  isolate({
  x=x()
  if(is.null(x)) return(HTML("There is no data! Please check all selection options!"))
  if(input$outliers1==TRUE){
    
    print('Test!!!')
    print(head(x))
    print(class(x))
    x$total=sapply(x$total, as.character)
    x$total=sapply(x$total, as.numeric)

#Select only big diff  
#    dd=abs(x$Val1-x$Val2)
#    bd=which(dd>(-0.001*log(x$total)+0.0095))
#  x=x[bd,]
    
    print(paste("Full ", as.character(dim(x))))
    #print(which(abs(x$Val1-x$Val2)>0.014))
    #x=x[which(abs(x$Val1-x$Val2)>0.014),]
    #print(paste("Considered ", dim(x)))
    
    #xs=ddply(x,.(Routing_Plan),summarise, id_rgt=mean(id_rgt),N1=sum(N1), Val1=median(Val1),Val2=median(Val2),N2=sum(N2), total=mean(total, na.rm=TRUE) )
    xs=ddply(x,.(Routing_Plan),summarise, id_rgt=0,N1=sum(N1), Val1=median(Val1, na.rm=TRUE),Val2=median(Val2, na.rm=TRUE),N2=sum(N2), total=round(mean(total, na.rm=TRUE) ))
    
    if(!is.null(xs)){print(head(xs))
    xs$id_rgt=rep(0, nrow(xs))
    x=rbind(x,xs)
    x[,4]=round(as.numeric(as.character(x[,4])),4)
    x[,5]=round(as.numeric(as.character(x[,5])),4)
    colnames(x)=c("Routing plan","id rgt","N1",input$vers1, input$vers2,"N2", "Number of activities" )
    print(head(x))
    print('Test ends')
    }
  }
  else{
    print("Routing plan agregation")
   # print(x)
    if(nrow(x[which(x[,2]==0 && x[,5]==0 ),])>0) x=x[-(which(x[,2]==0 && x[,5]==0 )),]
   
   #x=x[which(abs(x$Val1-x$Val2)>0.014),]
   
   
   x[,3]=round(as.numeric(as.character(x[,3])),4)
   x[,4]=round(as.numeric(as.character(x[,4])),4)
    colnames(x)=c("Routing plan","N1",input$vers1, input$vers2,"N2" )
  }
  
  return(x)
  })
  
}, options = list(lengthMenu = c(5, 10, 20, 50, 100), pageLength = 20)
)

output$mytable2 = renderDataTable({
  input$expert
  withProgress(message = 'Detailed routing plan assessment...                                         ', value = 0.1,detail ="", {
  xp()
  
  })
  
}, options = list(lengthMenu = c(5, 10, 20, 50, 100), pageLength = 5))

output$input_routing_plan <- renderText({
  input$expert
  isolate(
    paste("Assessment of routing runs for the plan",input$rout_plan)
  )
})
output$plot_params <- renderText({
  input$expert
  if(input$expert==FALSE) return ('')
 # isolate(
    paste(input$rout_plan, "     1-Work time ratio", "2-Travel time","3-Saved costs ratio", "4-Overtime ratio", "5-Rout density", "6-Number of assigned activities")
  #)
})

output$input_comp_name <- renderText({
  input$goButton
  isolate(
    paste("General routing assessment for the company",input$company)
  )
})

output$scomp <- renderText({
  input$selCmp
  isolate(
    paste("The selected company",input$comp)
  )
})

})
