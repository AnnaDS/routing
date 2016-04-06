
init <- function(need) {
  P=installed.packages()[,1]
 # ip <- .packages(all.available = T)
  if (any((need %in% P) == FALSE)) {
    install.packages(need[!(need %in% P)])
  }
  ok <- sapply(1:length(need), function(p) require(need[[p]], character.only = TRUE))
}

init(c("MBESS", "metafor", "plyr", "RMySQL", "ggplot2", "gdata", "DBI", "splancs"))


getCompanies <- function (){ 
  print("Getting companies...")
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  sql='SELECT distinct `company` FROM `rgt`  order by `company`'
  rhs=read.table("rhs_connect.csv",header = TRUE, sep = ",")
  con2 <- dbConnect(MySQL(), user=as.character(rhs[1,1]), password=as.character(rhs[1,2]), dbname=as.character(rhs[1,3]), host=as.character(rhs[1,4]))
  print("Connection is established")
  RN=dbGetQuery(con2,paste(sql))
  print(head(as.character(RN[,1])))
  return(as.character(RN[,1]))
}

getVers <- function (){ 
  print("Getting versions...")
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  sql='SELECT distinct `routing_version` FROM `rgt`  order by `routing_version`'
  rhs=read.table("rhs_connect.csv",header = TRUE, sep = ",")
  con2 <- dbConnect(MySQL(), user=as.character(rhs[1,1]), password=as.character(rhs[1,2]), dbname=as.character(rhs[1,3]), host=as.character(rhs[1,4]))
  print("Connection is established")
  RN=dbGetQuery(con2,paste(sql))
  return(as.character(RN[,1]))
}

replNA <- function(w){
  w <- data.frame(w, stringsAsFactors=FALSE)
  if (nrow(w)>0)
    for(i in 1:ncol(w)){
   w[which(is.na(w[,i])),i]=0
    w[which(as.character(w[,i])=="<NA>"),i]=0
   w[which(as.character(w[,i])==""),i]=0
  }
  return(w)
}
replNAN <- function(w){
  for(i in 1:ncol(w)){
    w[which(is.nan(w[,i])),i]=0
  }
  return(w)
}

#Get data for the definite company (versions, instances, dates)
getDBD <- function (cmp=""){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  sql=paste("SELECT `company`, `routing_version`, `run_type`, min(`run_time`), max(`run_time`) FROM `rgt` WHERE `company` like \"", cmp ,  "\"  group by `routing_version`, `run_type`", sep='')
  rhs=read.table("rhs_connect.csv",header = TRUE, sep = ",")
  con <- dbConnect(MySQL(), user=as.character(rhs[1,1]), password=as.character(rhs[1,2]), dbname=as.character(rhs[1,3]), host=as.character(rhs[1,4]))
  vv=dbGetQuery(con,paste(sql))
  colnames(vv)=c(colnames(vv)[1:3], "Start date", "End date")
  return(vv)
}

#Get data of routing results
getDBF <- function (version1=vvn, init=FALSE, cmp="", dates="", env="prod", rp=""){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  params=data.frame(V1=c("id_rgt","original_run_id","routing_run_id","routing_version","routing_plan_name",
                         #"routing_plan_name",
                         #"routing_work_time_human",
                         #"routing_work_time_machine","input_file","output_file",
                         "sum_travel_time","sum_waiting_time","sum_overdue_time","sum_overtime_soft", "sum_overtime_hard",
                         "company","sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
                         "raw_travel_time_sum","raw_final_travel_time_sum","raw_overdue_time_sum", "raw_sla_violation_sum", "raw_sla_violation_activities",
                         "raw_overtime_soft_sum","raw_overtime_hard_sum",
                         "raw_not_assigned_activities", "raw_not_assigned_activities_unacceptable_travel_time",
                         "raw_not_assigned_activities_provider_workday_stop", "raw_not_assigned_activities_provider_overload",
                         "raw_total_activities","raw_assigned_activities","raw_rejected_activities", "sum_would_be_not_assigned",
                         "fitness","providers_used","info_sum_assigned"))
  
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else if(as.character(params[i,1])=="company"|
              as.character(params[i,1])=="original_run_id"|
              as.character(params[i,1])=="id_rgt" |
              as.character(params[i,1])=="routing_plan_name" |
              as.character(params[i,1])=="routing_version")
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else 
    {
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    }
  }
 
if(length(dates)>0)
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\' and V.run_type like \'",env,"\'",  sep='' )
else 
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )

if(rp!="")
  sqll=paste(sqll, ' and V.routing_plan_name like \'',rp,'\'',  sep='' )

  print(paste(sqll))

# To connect to custom rhs please use
# con <- dbConnect(MySQL(), user="sla1", password="sla1SLA!", dbname="anna_rhs", host="slasrv2.ua1")

rhs=read.table("rhs_connect.csv",header = TRUE, sep = ",")
con2 <- dbConnect(MySQL(), user=as.character(rhs[1,1]), password=as.character(rhs[1,2]), dbname=as.character(rhs[1,3]), host=as.character(rhs[1,4]))
start.time <- Sys.time()
RN=dbGetQuery(con2,paste(sqll))
end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Get data from DB", time.taken))
if(nrow(RN)==0) return (NULL)
numC=c("id_rgt","original_run_id","routing_run_id",
       "sum_travel_time","sum_waiting_time","sum_overdue_time","sum_overtime_soft", "sum_overtime_hard",
       "sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
       "raw_travel_time_sum","raw_final_travel_time_sum","raw_overdue_time_sum", "raw_sla_violation_sum",
       "raw_sla_violation_activities",
       "raw_overtime_soft_sum","raw_overtime_hard_sum",
       "raw_not_assigned_activities", "raw_not_assigned_activities_unacceptable_travel_time",
       "raw_not_assigned_activities_provider_workday_stop", "raw_not_assigned_activities_provider_overload",
       "raw_total_activities","raw_assigned_activities","raw_rejected_activities", "sum_would_be_not_assigned",
       "fitness","providers_used","info_sum_assigned")
for( i in 1:length(numC)){
  RN[,grep(numC[i],colnames(RN))]=sapply(RN[,grep(numC[i],colnames(RN))], as.character)
  RN[,grep(numC[i],colnames(RN))]=sapply(RN[,grep(numC[i],colnames(RN))], as.numeric)  
}

if(nrow(RN)>0){
if(nrow(RN[which(RN[,grep("raw_assigned_activities",colnames(RN))]==0),])>0)
  RN=RN[-(which(RN[,grep("raw_assigned_activities",colnames(RN))]==0)),]
if(nrow(RN[which(RN[,grep("fitness",colnames(RN))]==0),])>0)
  RN=RN[-(which(RN[,grep("fitness",colnames(RN))]==0)),]
if(nrow(RN[which(RN[,grep("providers_used",colnames(RN))]==0),])>0)
  RN=RN[-(which(RN[,grep("providers_used",colnames(RN))]==0)),]
}

#Delete version from column names
if(init) 
  colnames(RN)[grep("stage_",colnames(RN))]=paste(gsub("stage_", "initial_",colnames(RN)[grep("stage_",colnames(RN))]), sep='')
else
  colnames(RN)[grep("stage_",colnames(RN))]=paste(colnames(RN)[grep("stage_",colnames(RN))], sep='')
#print (head(RN))
  return(RN)
}

#Get data of routing results for definite id
getDBF_id <- function (version1=vvn, init=FALSE,  env="prod", id=""){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  params=data.frame(V1=c("id_rgt","original_run_id","routing_run_id","routing_version","routing_plan_name",
                         #"routing_plan_name",
                         #"routing_work_time_human",
                         #"routing_work_time_machine","input_file","output_file",
                         "sum_travel_time","sum_waiting_time","sum_overdue_time","sum_overtime_soft", "sum_overtime_hard",
                         "company","sum_work_time","sum_not_assigned", "sum_sla_violation", "raw_waiting_time_sum", "raw_work_time_sum",
                         "raw_travel_time_sum","raw_final_travel_time_sum","raw_overdue_time_sum", "raw_sla_violation_sum",
                         "raw_sla_violation_activities",
                         "raw_overtime_soft_sum","raw_overtime_hard_sum",
                         "raw_not_assigned_activities", "raw_not_assigned_activities_unacceptable_travel_time",
                         "raw_not_assigned_activities_provider_workday_stop", "raw_not_assigned_activities_provider_overload",
                         "raw_total_activities","raw_assigned_activities","raw_rejected_activities", "sum_would_be_not_assigned",
                         "fitness","providers_used","info_sum_assigned"))
  
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else if(as.character(params[i,1])=="company"|
              as.character(params[i,1])=="original_run_id"|
              as.character(params[i,1])=="id_rgt" |
              as.character(params[i,1])=="routing_plan_name" |
              as.character(params[i,1])=="routing_version")
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else 
    {
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    }
  }
  if(length(id)==1)
     {sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.id_rgt = \'",id,"\'",  sep='' )
  }
      else{
        sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.id_rgt in (",paste(as.character(id),collapse=","),")",  sep='' )
  }

  rhs=read.table("rhs_connect.csv",header = TRUE, sep = ",")
  con2 <- dbConnect(MySQL(), user=as.character(rhs[1,1]), password=as.character(rhs[1,2]), dbname=as.character(rhs[1,3]), host=as.character(rhs[1,4]))
  start.time <- Sys.time()
  RN=dbGetQuery(con2,paste(sqll))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(paste("Get data by id", time.taken))
 # print(nrow(RN))
  if(nrow(RN)==0) return(NULL)
  numC=c("stage_id_rgt","stage_original_run_id","stage_routing_run_id",
         "stage_sum_travel_time","stage_sum_waiting_time","stage_sum_overdue_time","stage_sum_work_time","stage_sum_not_assigned", 
         "stage_sum_sla_violation", "stage_raw_waiting_time_sum", "stage_raw_work_time_sum",
         "stage_raw_travel_time_sum","stage_raw_overtime_soft_sum","stage_raw_overtime_hard_sum","stage_raw_not_assigned_activities",
         "stage_raw_total_activities",
         "stage_fitness","stage_providers_used","stage_info_sum_assigned")

  for( i in 1:length(numC)){
    RN[,grep(numC[i],colnames(RN))]==sapply(RN[,grep(numC[i],colnames(RN))], as.character)
    RN[,grep(numC[i],colnames(RN))]==sapply(RN[,grep(numC[i],colnames(RN))], as.numeric)
  }
# print(RN$stage_raw_not_assigned_activities)
  if(nrow(RN[which(RN[,grep("raw_assigned_activities",colnames(RN))]==0),])>0)
    RN=RN[-(which(RN[,grep("raw_assigned_activities",colnames(RN))]==0)),]
  if(nrow(RN[which(RN[,grep("fitness",colnames(RN))]==0),])>0)
    RN=RN[-(which(RN[,grep("fitness",colnames(RN))]==0)),]
  if(nrow(RN[which(RN[,grep("providers_used",colnames(RN))]==0),])>0)
    RN=RN[-(which(RN[,grep("providers_used",colnames(RN))]==0)),]
#Delete version from column names
if(init) 
  colnames(RN)[grep("stage_",colnames(RN))]=paste(gsub("stage_", "initial_",colnames(RN)[grep("stage_",colnames(RN))]), sep='')
else
  colnames(RN)[grep("stage_",colnames(RN))]=paste(colnames(RN)[grep("stage_",colnames(RN))], sep='')
  #print (head(RN))
  return(RN)
}

#Get data of the capacity usage by routing
getDBwt <- function (version1=vvn,  init=FALSE, cmp="", dates="", env){
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    if (length(dbListResults(con))>0)
      dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)}
  params=data.frame(V1=c("id_rgt","original_run_id","routing_run_id","routing_version","routing_plan_name",
                         "routing_work_time_human",
                        "routing_work_time_machine"))
  sqll='Select '
  h=seq(1,nrow(params))
  for (i in h){
    #print(params[i,1])
    if(sqll=='Select ') sqll=paste(sqll, 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    else 
    {
      sqll=paste(sqll, ", ", 'V.`', as.character(params[i,1]),"`  as  stage_", as.character(params[i,1]), sep='')    
    }
  }
  
if(length(dates)>0)
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" and V.run_time>= \'",dates[1]," 00:00:00\' and V.run_time<= \'",dates[2]," 00:00:00\' and run_type like \'", env, "\' ",  sep='' )
else 
  sqll=paste(sqll, ' FROM `rgt` V WHERE V.`routing_version` like \'', version1,"\' and V.company  like \"", cmp , "\" ",  sep='' )
rhs=read.table("rhs_connect.csv",header = TRUE, sep = ",")
con2 <- dbConnect(MySQL(), user=as.character(rhs[1,1]), password=as.character(rhs[1,2]), dbname=as.character(rhs[1,3]), host=as.character(rhs[1,4]))
RN=dbGetQuery(con2,paste(sqll))
#Delete version from column names
if(init) 
  colnames(RN)[grep("stage_",colnames(RN))]=paste(gsub("stage_", "initial_",colnames(RN)[grep("stage_",colnames(RN))]), sep='')
else
  colnames(RN)[grep("stage_",colnames(RN))]=paste(colnames(RN)[grep("stage_",colnames(RN))], sep='')

numC=c("routing_work_time_human",
"routing_work_time_machine")
for(i in 1:2)
  RN[,grep(numC[i],colnames(RN))]=as.numeric(as.character(RN[,grep(numC[i],colnames(RN))]))
#print(head(RN))
  return(RN)
}

# Not used now
is.null.col=function(l, name){
  for(i in 1:length(l)){
    if(length(l[[i]]$name[which(l[[i]]$name !=0 )])>0)
      return (FALSE)
  }
  return (TRUE)
}

#Form the table for capacity
getCapacity <- function(initialV, stageV,  cmp='', initD, stageD, envI, envS){
  RN=list()
 # print(initD, stageD)
  AFR=NULL
  cnv=c(initialV, stageV)
  dv=data.frame(v=c("2.0.7.2", "2.1.0.2","2.1.1.0","2.2.0.0","2.2.0.0.90"), vF=c("2.0.7", "2.1.0","2.1.1", "2.2.0", ,"2.2.09"))

  initialV[which(initialV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% initialV)])
  stageV[which(stageV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% stageV)])

  inr=getDBwt(version1=as.character(initialV),init=TRUE, cmp=cmp, dates=initD, env=envI)  
  colnames(inr)[grep("initial_id_rgt",colnames(inr))]='original_id'
  colnames(inr)[grep("_company",colnames(inr))]='company'
 #print(head(inr))
  RES=NULL
 
    t=getDBwt(stageV, cmp=cmp, dates=stageD, env=envS)
  
print(gsub("\\.","",initialV))
 if((as.numeric(gsub("\\.","",initialV))<2100 || as.numeric(gsub("\\.","",stageV))<2100) ) v=TRUE
 else v=FALSE
    if (v) colnames(t)[grep("stage_original_run_id",colnames(t))]='original_id'
    colnames(t)[grep("_company",colnames(t))]='company'
    #print(head(t))
if (v) RPt=merge(inr, t, by=intersect(names(inr), names(t)) )
else {
  colnames(t)[grep("_routing_plan_name",colnames(t))]='routing_plan_name'
  colnames(inr)[grep("_routing_plan_name",colnames(inr))]='routing_plan_name'
  rpiN=unique(inr$routing_plan_name)
  rpsN=unique(t$routing_plan_name)
  RPf=intersect(rpiN, rpsN)
  
  RPt=NULL
  for(j in 1:length(RPf)){
    inrRP=inr[which(as.character(inr$routing_plan_name) %in% RPf[j]),]
    tRP=t[which(as.character(t$routing_plan_name) %in% RPf[j]),]
    if(nrow(inrRP)>0 && nrow(tRP)>0)
{   # print('Human time!!!!!')
    #print(as.character(inrRP[grep("_work_time_human",colnames(inrRP)),]))
    RPt=rbind(RPt, data.frame(routing_plan=RPf[j], 
                              FRRR_work_time_human=median(as.numeric(as.character(inrRP[,grep("_work_time_human",colnames(inrRP))]))),
                              FRRR_work_time_machine=median(inrRP[,grep("_work_time_machine",colnames(inrRP))]),
                              SRRR_work_time_human=median(tRP[,grep("_work_time_human",colnames(tRP))]), 
                              SRRR_work_time_machine=median(tRP[,grep("_work_time_machine",colnames(tRP))])))
  }}
  #print(head(RPt))
  inrRP=inr[which(!(as.character(inr$routing_plan_name) %in% RPf)),]
  tRP=t[which(!(as.character(t$routing_plan_name) %in% RPf)),]
  RPt=rbind(RPt, data.frame(routing_plan="Others", FRRR_work_time_human=median(inrRP[,grep("_work_time_human",colnames(inrRP))]),
                            FRRR_work_time_machine=median(inrRP[,grep("_work_time_machine",colnames(inrRP))]),
                            SRRR_work_time_human=median(tRP[,grep("_work_time_human",colnames(tRP))]), 
                            SRRR_work_time_machine=median(tRP[,grep("_work_time_machine",colnames(tRP))])))
}

# print("Capacity!!!!!!!!!!!!!!")
 #print(summary(RPt))
 return(RPt)
 
  
}

#main function
getALLResF <- function(initialV, stageV,  cmp='vm', initD, stageD, init_env, stage_env, rp="", test=FALSE, ED=FALSE){
  print(paste("Company",cmp))
 print(paste(initialV, cmp, initD, init_env,  rp , test, ED))
 print(paste(stageV,  cmp, stageD,  stage_env, rp , test, ED))
  #if(test && !(init_env=='prod' && stage_env=='stage')) return (NULL)
  RN=list()
  AFR=NULL
  cnv=c(initialV, stageV)
  dv=data.frame(v=c("2.0.7.2", "2.1.0.2","2.1.1.0", "2.2.0.0", "2.2.0.0.90"), vF=c("2.0.7", "2.1.0","2.1.1", "2.2.0", "2.2.09"))
  initialV[which(initialV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% initialV)])
  stageV[which(stageV %in%  dv$vF)]=as.character(dv$v[which(dv$vF %in% stageV)])
  inr=getDBF(version1=as.character(initialV),init=TRUE, cmp=cmp, dates=initD, init_env, rp=rp)
 RES=NULL 

  t=getDBF(stageV, cmp=cmp, dates=stageD, stage_env, init=FALSE, rp=rp)
  print(gsub("\\.","",initialV))
 if((as.numeric(gsub("\\.","",initialV))<2100 || as.numeric(gsub("\\.","",stageV))<2100)) v=TRUE
 else v=FALSE
 
# orig_vec=c("TRAS","RAA", "AL", "OvWt", "RD", "sAA", "AAn")
 
#orig_vec=c("TRAS","RAA", "AL", "OvWt", "RD", "sAA")
#configure new order
orig_vec=c("RAA", "TRAS", "AL", "OvWt", "RD", "sAA")
 #idr=c( 0,1,1,0,0.8, 1)
# idr=c( 0,1,1,0,1,1)
 idr=c( 1,0,1,0,1,1)
 
 # Testing for the new version of routing when prod and stage are used
#print(head(t))

 if(#stage_env=="stage" && init_env=='prod' && 
      test && is.null(t) ){
 paste(" No data")
   return(NULL)
 }
  if(#stage_env=="stage" && init_env=='prod' && 
       test && !is.null(t) && nrow(t)>0 && rp==''){
    print("Prod stage option")
    stageV=t[!duplicated(t[,]),]
    stageV=stageV[, -grep("id_rgt",colnames(stageV))]
    colnames(stageV)[grep("stage_original_run",colnames(stageV))]='N_id_rgt'
    colnames(stageV)[grep("stage_routing_plan_name",colnames(stageV))]='routing_plan_name'
   
    stageV=stageV[!duplicated(stageV[,]),]
    ids=unique(as.character(stageV[,grep("N_id_rgt",colnames(stageV))]))
    initD=getDBF_id(version1=as.character(initialV),init=TRUE,  init_env, id=ids)
   # print(head(initD))
    if(is.null(initD)){
      print(paste(" No data"))
      return(NULL)
    }
    # grep("original_run_id",colnames(stageV))
   initD=initD[, -grep("original_run_id",colnames(initD))]
   colnames(initD)[grep("initial_id_rgt",colnames(initD))]="N_id_rgt"
   colnames(initD)[grep("initial_routing_plan_name",colnames(initD))]="routing_plan_name"
   
#    stageV=stageV[, -grep("id_rgt",colnames(stageV))]
#    colnames(stageV)[grep("stage_original_run",colnames(stageV))]='N_id_rgt'
#    colnames(stageV)[grep("stage_routing_plan_name",colnames(stageV))]='routing_plan_name'
   
    ids=unique(intersect(initD[,grep("N_id_rgt",colnames(initD))], stageV[,grep("N_id_rgt",colnames(stageV))]))
   initD=initD[which(initD[,grep("N_id_rgt",colnames(initD))] %in% ids), ]
   stageV=stageV[which(stageV[,grep("N_id_rgt",colnames(stageV))] %in% ids), ]
stageV=stageV[!duplicated(stageV[,]),]
initD=initD[!duplicated(initD[,]),]
   initD=initD[order(initD[,grep("N_id_rgt",colnames(initD))]),]
   stageV=stageV[order(stageV[,grep("N_id_rgt",colnames(stageV))]),]
   print(paste("Nrow initial",nrow(initD)))
   print(paste("Nrow stage",nrow(stageV)))
   if(nrow(initD)!=nrow(stageV)) {
     M=merge(initD, stageV)
     initD=cbind(N_id_rgt=M[,grep("N_id_rgt",colnames(M))], routing_plan_name=M[,grep("routing_plan_name",colnames(M))],
                 M[,grep("initial_",colnames(M))])
     stageV=cbind(N_id_rgt=M[,grep("N_id_rgt",colnames(M))], routing_plan_name=M[,grep("routing_plan_name",colnames(M))],
                 M[,grep("stage_",colnames(M))])
     if(nrow(initD)!=nrow(stageV)) stop("The number of initial and stage routing runs are not equal!!!")
   }
   dd=data.frame(routing_plan_name=initD$routing_plan_name, N_id_rgt=initD$N_id_rgt, total=initD[,grep("raw_total_activities",colnames(initD))])
   R1=getRelAs(initD, (as.numeric(initD[,grep("_raw_total_activities",colnames(initD))])), fin=FALSE, v=v, orig_vec)
#print(R1)
   R2=getRelAs(stageV, (as.numeric(stageV[,grep("_raw_total_activities",colnames(stageV))])), fin=FALSE, v=v, orig_vec)
  gP1=getPlots(initD)
  gP2=getPlots(stageV)
colnames(gP1)=paste("FRRR_", colnames(gP1), sep="")
colnames(gP2)=paste("SRRR_", colnames(gP2), sep="")
   if(!all(dd$N_id_rgt==R1$id_rgt)) stop("The id's of prod assessment are not equal!!!")
  if(!all(dd$N_id_rgt==R2$id_rgt)) stop("The id's of stage assessment are not equal!!!")
   colnames(R1)=paste("FRRR_", colnames(R1), sep="")
   colnames(R2)=paste("SRRR_", colnames(R2), sep="")
   all_colls1=colnames(R1)
   all_colls2=colnames(R2)
   ra11=R1[,c(grep(orig_vec[1], colnames(R1)))]
   ra22=R2[,c(grep(orig_vec[1], colnames(R2)))]
   all_colls11=all_colls1[c(grep(orig_vec[1], colnames(R1)))]
   all_colls22=all_colls2[c(grep(orig_vec[1], colnames(R2)))]
   for(h in 2:length(orig_vec)){
     ra11=cbind(ra11,R1[,c(grep(orig_vec[h], colnames(R1)))])
     ra22=cbind(ra22,R2[,c(grep(orig_vec[h], colnames(R2)))])
     all_colls11=c(all_colls11, all_colls1[c(grep(orig_vec[h], colnames(R1)))])
     all_colls22=c( all_colls22, all_colls2[c(grep(orig_vec[h], colnames(R2)))])
   }
   colnames(ra11)=all_colls11
   colnames(ra22)=all_colls22
   ra1c=NULL
   for(l in 1:nrow(ra11)) {
    # print(ra11[l,])
     #print(idr)
     ra1c=c(ra1c,calc.v(ra11[l,], idr, orig_vec, ED))                  
     if(l==1) {plot(idr, t='l', lwd=3, col='red', main=cmp, ylim=c(0, max(idr,as.numeric(ra11[,seq(2, ncol(ra11))] ),as.numeric(ra22[,seq(2, ncol(ra22))] ))))}
     lines(as.numeric(ra11[l,]), col='blue')
   }
   ra2c=NULL
   for(l in 1:nrow(ra22)) {ra2c=c(ra2c,calc.v(ra22[l,], idr, orig_vec, ED))
                          lines(as.numeric(ra22[l,]), col='darkgreen')}
   if(length(which(is.nan(ra1c)))>0) warning("Not all asessments are numeric in the First selsection")
   if(length(which(is.nan(ra2c)))>0) warning("Not all asessments are numeric in the Second selsection")
    ra1c=round(ra1c,4)
    ra2c=round(ra2c,4)
   D=data.frame(Routing_Plan=as.character(dd$routing_plan_name), id_rgt=dd$N_id_rgt,N1=rep(1,nrow(dd)),  Val1=ra1c,  Val2=ra2c,  N2=rep(1,nrow(dd)), total=dd$total)
R=cbind(D[,c(1,2)],gP1,gP2)
#print(head(R))
write.csv(R,  paste(cmp,"_FR.csv", sep=''), row.names = FALSE)

return(D)
 }
 
 

#  colnames(inr)[grep("initial_id_rgt",colnames(inr))]='N_id_rgt'
#  colnames(inr)[grep("initial_routing_plan_name",colnames(inr))]='routing_plan_name'
#  colnames(inr)[grep("_company",colnames(inr))]='company'
#  colnames(t)[grep("stage_original_run_id",colnames(t))]='N_id_rgt'
#  colnames(t)[grep("stage_routing_plan_name",colnames(t))]='routing_plan_name'
#  colnames(t)[grep("_company",colnames(t))]='company'

#orig_vec=c("RPA","AL", "OvW", "sAA", "RD")


if(rp!=''){
  print('Routing plan stage')
  #print(test)
  if(#stage_env=="stage" & init_env=='prod' &
       test & !is.null(t) & nrow(t)>0){
    print('Test stage')
    colnames(t)[grep("stage_original_run_id",colnames(t))]='N_id_rgt'
    colnames(t)[grep("stage_routing_plan_name",colnames(t))]='routing_plan_name'
    colnames(t)[grep("_company",colnames(t))]='company'
    t=t[!duplicated(t[,]),]
    print(rp)
    print(paste('Number of stage records', nrow(t)))
    t=t[, -grep("stage_id_rgt",colnames(t))]
    ids=unique(as.character(t[,grep("N_id_rgt",colnames(t))]))
    inr=getDBF_id(version1=as.character(initialV),init=TRUE,  init_env, id=ids)
    colnames(inr)[grep("initial_id_rgt",colnames(inr))]='N_id_rgt'
    ids=unique(intersect(as.character(t[,grep("N_id_rgt",colnames(t))]), as.character(inr[,grep("N_id_rgt",colnames(inr))])))
t=t[which(t$N_id_rgt %in% ids),]
inr=inr[which(inr$N_id_rgt %in% ids),]
    colnames(inr)[grep("initial_routing_plan_name",colnames(inr))]='routing_plan_name'
    colnames(inr)[grep("_company",colnames(inr))]='company'
    if(is.null(inr)) return (NULL)
  colnames(inr)[grep("initial_id_rgt",colnames(inr))]='N_id_rgt'
  colnames(inr)[grep("initial_routing_plan_name",colnames(inr))]='routing_plan_name'
  colnames(inr)[grep("_company",colnames(inr))]='company'
  }
  print("Assessment!")
  if(is.null(t) || is.null(inr)) return (NULL)
  
  colnames(inr)[grep("initial_id_rgt",colnames(inr))]='N_id_rgt'
  colnames(inr)[grep("initial_routing_plan_name",colnames(inr))]='routing_plan_name'
  colnames(inr)[grep("_company",colnames(inr))]='company'
  colnames(t)[grep("stage_original_run_id",colnames(t))]='N_id_rgt'
  colnames(t)[grep("stage_routing_plan_name",colnames(t))]='routing_plan_name'
  colnames(t)[grep("_company",colnames(t))]='company'
  
  gP1=getPlots(inr)
  gP2=getPlots(t)
  colnames(gP1)=paste("FRRR_", colnames(gP1), sep="")
  colnames(gP2)=paste("SRRR_", colnames(gP2), sep="")
mcn=max(nrow(gP1),nrow(gP2))
if(nrow(gP1)<mcn) {
vt=as.data.frame(matrix(NaN,nrow=(mcn-nrow(gP1)),ncol=ncol(gP1)))
colnames(vt)=colnames(gP1)
  gP1=rbind(gP1,  vt)
}
if(nrow(gP2)<mcn)  {
  vt=as.data.frame(matrix(NaN,nrow=(mcn-nrow(gP2)),ncol=ncol(gP2)))
  colnames(vt)=colnames(gP2)
  gP2=rbind(gP2,  vt)
}
  R=cbind( gP1,gP2)   
 # print(head(R))
  #write.csv(R,  paste(cmp,"_FR.csv", sep=''), row.names = FALSE)

  vec_RP=c("id_rgt", "AA", "WrtPr", "TrPr", "WtPr", "OvPr", "PR", "Sum", "Sla_act", "Sla_time")
  RA1=getRPdata(inr, (as.numeric(inr[,grep("_raw_total_activities",colnames(inr))])),  v=v)
  #print(head(inr))
  RA2=getRPdata(t, (as.numeric(t[,grep("_raw_total_activities",colnames(t))])),  v=v)
  RA11=getRelAs(inr, (as.numeric(inr[,grep("_raw_total_activities",colnames(inr))])), fin=FALSE, v=v)
  RA22=getRelAs(t, (as.numeric(t[,grep("_raw_total_activities",colnames(t))])), fin=FALSE, v=v)
  colN=1
  for(l in 1:length(orig_vec)){
    colN=c(colN, grep(orig_vec[l],colnames(RA11)))
  }
  RA11=RA11[, colN]
  RA22=RA22[, colN]

 ra1c=NULL
 ra2c=NULL
 png(filename=paste(gsub("/","",rp),".png", sep=''))
# print(head(RA11))
# print(head(RA22))
# print(head(as.vector(as.matrix(RA11[,seq(2, ncol(RA11))]))))
# print(head(as.vector(as.matrix(RA22[,seq(2, ncol(RA22))] ))))
val1=NULL  
for(l in 1:nrow(RA11)) {
    #print(RA1[l,])
    val1=c(val1,calc.v(RA11[l,seq(2, ncol(RA11))], idr, orig_vec, ED ))
    ra1c=rbind(ra1c, data.frame(id_rgt=RA11[l,1], val=val1[length(val1)]))
    if(l==1) {plot(idr, t='l', lwd=3, col='red', main=rp, ylim=c(0, max(idr,as.vector(as.matrix(RA11[,seq(2, ncol(RA11))])),as.vector(as.matrix(RA22[,seq(2, ncol(RA22))] )))))
    legend(1, 1,c("1-Travel time","2-Work time ratio", "3-Revenue ratio", "4-Overtime ratio", "5-Rout density"), , bty='n', cex=0.9)
}
    lines(as.numeric(RA11[l,seq(2, ncol(RA11))]), col='blue')
  }
RA11=cbind(RA11, val=val1)
  val2=NULL
  for(l in 1:nrow(RA22)) {
    #print(RA1[l,])
    val2=c(val2,calc.v(RA22[l,seq(2, ncol(RA22))], idr, orig_vec, ED ))
      
      ra2c=rbind(ra2c, data.frame(id_rgt=RA22[l,1], val=val2[length(val2)]))
    lines(as.numeric(RA22[l,seq(2, ncol(RA22))]), col='green')
  }
RA22=cbind(RA22, val=val2)
colnames(RA11)[seq(2, ncol(RA11))]=paste("F_", colnames(RA11)[seq(2, ncol(RA11))], sep='')
colnames(RA22)[seq(2, ncol(RA22))]=paste("S_", colnames(RA22)[seq(2, ncol(RA22))], sep='')
Nd=merge(RA11, RA22)
print(paste('Dimension after merge ', dim(Nd)))
print(head(Nd))
Nd1=Nd[,c(seq(1,7),seq(9,14))]
targ=data.frame(id_rgt=0, v1=idr[1], v2=idr[2], v3=idr[3], v4=idr[4], 
                v5=idr[5], v6=idr[6], 
                v7=idr[1], v8=idr[2], v9=idr[3], v10=idr[4], 
                v11=idr[5],v12=idr[6])
print(targ)

colnames(targ)=colnames(Nd1)
Nd1=rbind(Nd1, targ)

write.csv(Nd,  paste(gsub("/","_",rp),"_RP.csv", sep=''), row.names = FALSE)
dev.off()


  all_colls1=colnames(RA1)
  all_colls2=colnames(RA2)
  ra11=data.frame(version=rep(as.character(initialV), nrow(RA1)),RA1[,c(grep(vec_RP[1], colnames(RA1)))])
  ra22=data.frame(version=rep(as.character(stageV), nrow(RA2)),RA2[,c(grep(vec_RP[1], colnames(RA2)))])
  for(h in 2:length(vec_RP)){
    ra11=cbind(ra11,RA1[,c(grep(vec_RP[h], colnames(RA1)))])
    ra22=cbind(ra22,RA2[,c(grep(vec_RP[h], colnames(RA2)))])
  }
 colnames(ra11)=c("version","id_rgt", "Assigned activities", "Work time", "Travel time", "Wait time", "Overtime","Providers","Losses %", "SLA_act", "SLA_time")
 colnames(ra22)=c("version","id_rgt", "Assigned activities", "Work time", "Travel time", "Wait time", "Overtime","Providers","Losses %", "SLA_act", "SLA_time")
ra11=cbind(environment=rep(init_env, nrow(ra11)), ra11, Val=ra1c$val)
ra22=cbind(environment=rep(stage_env, nrow(ra22)), ra22, Val=ra2c$val)
#  print(head(ra11))
print(dim(ra22))
print(head(ra22))
col_ord=c("id_rgt", "environment", "version",  "Assigned activities", "Work time", "Travel time", "Wait time",  
        "Overtime", "Providers", "Losses %", "SLA_act", "SLA_time", "Val")
ra11=data.frame(ra11[,c(grep(col_ord[1], colnames(ra11)))],ra11[,c(grep(col_ord[2], colnames(ra11)))],ra11[,c(grep(col_ord[3], colnames(ra11)))],
                ra11[,c(grep(col_ord[4], colnames(ra11)))],ra11[,c(grep(col_ord[5], colnames(ra11)))],ra11[,c(grep(col_ord[6], colnames(ra11)))],
                ra11[,c(grep(col_ord[7], colnames(ra11)))],ra11[,c(grep(col_ord[8], colnames(ra11)))],ra11[,c(grep(col_ord[9], colnames(ra11)))],
                ra11[,c(grep(col_ord[10], colnames(ra11)))],ra11[,c(grep(col_ord[11], colnames(ra11)))],ra11[,c(grep(col_ord[12], colnames(ra11)))],
                ra11[,c(grep(col_ord[13], colnames(ra11)))])

ra22=data.frame(ra22[,c(grep(col_ord[1], colnames(ra22)))],ra22[,c(grep(col_ord[2], colnames(ra22)))],ra22[,c(grep(col_ord[3], colnames(ra22)))],
                ra22[,c(grep(col_ord[4], colnames(ra22)))],ra22[,c(grep(col_ord[5], colnames(ra22)))],ra22[,c(grep(col_ord[6], colnames(ra22)))],
                ra22[,c(grep(col_ord[7], colnames(ra22)))],ra22[,c(grep(col_ord[8], colnames(ra22)))],ra22[,c(grep(col_ord[9], colnames(ra22)))],
                ra22[,c(grep(col_ord[10], colnames(ra22)))],ra22[,c(grep(col_ord[11], colnames(ra22)))],ra22[,c(grep(col_ord[12], colnames(ra22)))],
                ra22[,c(grep(col_ord[13], colnames(ra22)))])
print('Before merge')
 #ra11=merge(ra11, ra1c, by=intersect(colnames(ra11), names(ra1c)) )
 #ra22=merge(ra22, ra2c, by=intersect(colnames(ra22), names(ra2c)) )
 

colnames(ra11)=c("id_rgt","environment","version", "Assigned activities", "Work time", "Travel time", "Wait time", "Overtime","Providers","Losses %", "SLA_act", "SLA_time","Val")
colnames(ra22)=c("id_rgt","environment","version", "Assigned activities", "Work time", "Travel time", "Wait time", "Overtime","Providers","Losses %", "SLA_act", "SLA_time", "Val")
print(head(ra11))
print(head(ra22))
print(dim(ra22)) 
for(p in 4:ncol(ra11)){
    ra11[,p]=round(as.numeric(as.character( ra11[,p])),4)
    ra22[,p]=round(as.numeric(as.character( ra22[,p])),4)
  }

  RR=rbind(ra11,  ra22)

RR=RR[!duplicated(RR), ]
#print(head(RR ))
write.csv(RR,  paste(gsub("/","",rp,),".csv", sep=''), row.names = FALSE)

  return (RR)
}

# print(paste(as.character(initialV), cmp, initD, init_env, rp))
# print(nrow(inr)==0)
# print(paste(as.character(stageV), cmp, stageD, stage_env, rp))
# print(nrow(t)==0)
if(is.null(t) || is.null(inr)) return (NULL)
print('Null is passed')
colnames(inr)[grep("initial_id_rgt",colnames(inr))]='N_id_rgt'
colnames(inr)[grep("initial_routing_plan_name",colnames(inr))]='routing_plan_name'
colnames(inr)[grep("_company",colnames(inr))]='company'
colnames(t)[grep("stage_original_run_id",colnames(t))]='N_id_rgt'
colnames(t)[grep("stage_routing_plan_name",colnames(t))]='routing_plan_name'
colnames(t)[grep("_company",colnames(t))]='company'
RPi=unique(inr[,grep("routing_plan_name",colnames(inr))])
RPs=unique(t[,grep("routing_plan_name",colnames(t))])
RPf=intersect(RPi, RPs)
RPi=RPi[-which(RPi %in% RPf)]
RPs=RPs[-which(RPs %in% RPf)]
#print (paste("Such routing plans are found", RPf))
# Add number of intersect Routing plans to the report!
# It may be the percentage

inrf=inr[which(as.character(inr$routing_plan_name) %in% RPf), ]
tf=t[which(as.character(t$routing_plan_name) %in% RPf), ]
inrr=inr[which(!(as.character(inr$routing_plan_name) %in% RPf)), ]
tr=t[which(!(as.character(t$routing_plan_name) %in% RPf)), ]

# print(paste("Number of intersected routing plans for 1-st sample",length(inrf)))
# print(paste("Number ofintersected routing plans for 2-nd sample",length(tf)))
# 
# print(paste("Number of not intersected routing plans for 1-st sample",length(inrr)))
# print(paste("Number of not intersected routing plans for 2-nd sample",length(tr)))
gc()

FR1=NULL
FR2=NULL
FR=NULL
FRS=NULL
all_colls=NULL
N1=NULL
N2=NULL

RRA1=NULL
RRA2=NULL
if(length(RPf)>0)
for(i in 1:length(RPf)){
   #print(paste("Routing plan", RPf[i]))
  R1=inrf[which(as.character(inrf$routing_plan_name) %in% RPf[i]),]
  gc()
  R2=tf[which(as.character(tf$routing_plan_name) %in% RPf[i]),]
 gc()

  RA1=getRelAs(R1, (as.numeric(R1[,grep("_raw_total_activities",colnames(R1))])), fin=FALSE, v=v, orig_vec)
  RA2=getRelAs(R2, (as.numeric(R2[,grep("_raw_total_activities",colnames(R2))])), fin=FALSE, v=v, orig_vec)
  colnames(RA1)=paste("FRRR_", colnames(RA1), sep="")
  colnames(RA2)=paste("SRRR_", colnames(RA2), sep="")
  all_colls1=colnames(RA1)
  all_colls2=colnames(RA2)
  FR=rbind(FR,cbind(Routing_plan=RPf[i], t(as.table(sapply(RA1, median, na.rm=TRUE))), t(as.table(sapply(RA2, median, na.rm=TRUE)))))
 #print(paste("aggregate with RP", RPf[i]))

#FRS=rbind(FRS,cbind(Routing_plan=RPf[i], t(as.table(sapply(RA1, sum))), t(as.table(sapply(RA2, sum)))))
 ra11=RA1[,c(grep(orig_vec[1], colnames(RA1)))]
 ra22=RA2[,c(grep(orig_vec[1], colnames(RA2)))]
 all_colls11=all_colls1[c(grep(orig_vec[1], colnames(RA1)))]
 all_colls22=all_colls2[c(grep(orig_vec[1], colnames(RA2)))]
 for(h in 2:length(orig_vec)){
   ra11=cbind(ra11,RA1[,c(grep(orig_vec[h], colnames(RA1)))])
   ra22=cbind(ra22,RA2[,c(grep(orig_vec[h], colnames(RA2)))])
   all_colls11=c(all_colls11, all_colls1[c(grep(orig_vec[h], colnames(RA1)))])
   all_colls22=c( all_colls22, all_colls2[c(grep(orig_vec[h], colnames(RA2)))])
 }
 RA1=ra11
 RA2=ra22
 colnames(RA1)=all_colls11
 colnames(RA2)=all_colls22
  if(v) {
    Mlosses=max(RA1[, grep("_AL",colnames(RA1))], RA2[, grep("_AL",colnames(RA2))])
    RA1[, grep("_AL",colnames(RA1))]=RA1[, grep("_AL",colnames(RA1))]/Mlosses
    RA2[, grep("_AL",colnames(RA2))]=RA2[, grep("_AL",colnames(RA2))]/Mlosses
  }

  ra1c=NULL
#print(tail(RA1))
  for(l in 1:nrow(RA1)) {#print(paste("Routing plan", RPf[i]))
                         #print(RA1[l,])
                         ra1c=c(ra1c,calc.v(RA1[l,], idr, orig_vec, ED ))
                         #print(ra1c[length(ra1c)])
  if(l==1) {plot(idr, t='l', lwd=3, col='red', main=RPf[i], ylim=c(0, max(idr,as.numeric(RA1[,seq(2, ncol(RA1))] ),as.numeric(RA2[,seq(2, ncol(RA2))] ))))}
  lines(as.numeric(RA1[l,]), col='blue')}
 RRA1=rbind(RRA1, RA1)
  ra2c=NULL

  for(l in 1:nrow(RA2)) {ra2c=c(ra2c,calc.v(RA2[l,], idr, orig_vec, ED))
                         lines(as.numeric(RA2[l,]), col='darkgreen')}
#print("end")
 RRA2=rbind(RRA2, RA2)
  if(length(which(is.nan(ra1c)))>0) warning("Not all asessments are numeric in the First selsection")
  if(length(which(is.nan(ra2c)))>0) warning("Not all asessments are numeric in the Second selsection")
  FR1[i]=median(ra1c, na.rm=TRUE)
  FR2[i]=median(ra2c, na.rm=TRUE)
  N1[i]=length(ra1c)
  N2[i]=length(ra2c)
# print(paste("Routing plan", RPf[i]))
# print(paste("initial",median(ra1c)))
# print(paste("stage",median(ra2c)))

}
# write.csv(RRA1, "RRA1.csv", row.names=FALSE)
# write.csv(RRA2, "RRA2.csv", row.names=FALSE)
vec_colln=c("id_rgt","AA", "TRT", "TRC", "SlC", "OVT", "OVC", "RD", "TRAS", "RAA", "AL", "OvWt",  "sAA")
Ifr=as.data.frame(matrix(NaN,nrow=0,ncol=length(vec_colln)))
Ifr=as.data.frame(matrix(NaN,nrow=0,ncol=length(vec_colln)))
Ifr=NULL
Sfr=NULL
print("Not intersected other rputing plans")
if(nrow(inrr)>0) {Ifr=getRelAs(inrr, (as.numeric(inrr[,grep("_raw_total_activities",colnames(inrr))])), fin=FALSE, v=v, orig_vec)
                  colnames(Ifr)=paste("FRRR_", colnames(Ifr), sep="")
                  all_colls1=colnames(Ifr)
                  Ifr= t(as.table(sapply(Ifr, median, na.rm=TRUE)))
                  IfrS= t(as.table(sapply(Ifr, sum)))
                  Ifr <- data.frame(matrix(c(as.numeric(Ifr)), nrow=1))
                  colnames(Ifr)=all_colls1
}
else {
  Ifr=data.frame(matrix(vector(), 1, length(all_colls1), dimnames=list(c(), all_colls1)), stringsAsFactors=F)
  colnames(Ifr)=paste("FRRR_", colnames(Ifr), sep="")
#   Ifr=as.data.frame(matrix(NaN,nrow=(mcn-nrow(gP1)),ncol=13))
#   
#   Ifr=data.frame(matrix(vector(), 1, length(all_colls1), dimnames=list(c(), all_colls1)), stringsAsFactors=F)
}
if(nrow(tr)>0) {Sfr=getRelAs(tr, (as.numeric(tr[,grep("_raw_total_activities",colnames(tr))])), fin=FALSE, v=v, orig_vec)
                colnames(Sfr)=paste("SRRR_", colnames(Sfr), sep="")
                all_colls2=colnames(Sfr)
                Sfr= t(as.table(sapply(Sfr, median, na.rm=TRUE)))
                SfrS= t(as.table(sapply(Sfr, sum)))
                Sfr <- data.frame(matrix(c(as.numeric(Sfr)), nrow=1))  
                colnames(Sfr)=all_colls2
}
else{
  Sfr=data.frame(matrix(vector(), 1, length(all_colls2), dimnames=list(c(), all_colls2)), stringsAsFactors=F)
  #print(dim(Sfr))
  colnames(Sfr)=paste("SRRR_", colnames(Sfr), sep="")
  #Sfr=data.frame(matrix(vector(), 1, length(all_colls2), dimnames=list(c(), all_colls2)), stringsAsFactors=F)
}

Ofr=data.frame(Routing_plan="Others")

if(nrow(Ifr)>0 && length(which(is.na(Ifr)))!=length(Ifr) && nrow(Sfr)>0 && length(which(is.na(Sfr)))!=length(Sfr) ){
Rrf=cbind(Ofr,Ifr, Sfr)
#RrfS=cbind(Ofr,IfrS, SfrS)

if(!v && !is.null(FR)) {
  FR=rbind(Rrf, FR)
 # FRS=rbind(RrfS, FRS)
}
else FR=Rrf

}
FR=as.data.frame(FR)

#FRS=as.data.frame(FRS)
colnames(FR)=c("Routing_plan", all_colls1, all_colls2)
#colnames(FRS)=c("Routing_plan", all_colls1, all_colls2)
write.csv(FR,  paste(cmp,"_FR.csv", sep=''), row.names = FALSE)
#write.csv(FRS,  paste(cmp,"_FRS.csv", sep=''), row.names = FALSE)
  
FRR=cbind(Routing_plan=RPf, FN=N1, FRRR=FR1, SRRR=FR2, SN=N2)

Irrr=split(inrr, list(inrr[,grep("routing_plan_name",colnames(inrr))]), drop=TRUE)
Srrr=split(tr, list(tr[,grep("routing_plan_name",colnames(tr))]), drop=TRUE)
FRO=NULL
FR1O=NULL
if(length(Irrr)>0) {for(i in 1:length(Irrr)){
  RA=getRelAs(Irrr[[i]], (as.numeric(Irrr[[i]][,grep("_raw_total_activities",colnames(Irrr[[i]]))])), fin=FALSE, v=v, orig_vec)
 #colnames(RA)=all_colls11
  ra11=RA[,c(grep(orig_vec[1], colnames(RA)))]

  #all_colls11=all_colls1[c(grep(orig_vec[1], colnames(RA1)))]
  #all_colls22=all_colls2[c(grep(orig_vec[1], colnames(RA2)))]
  for(h in 2:length(orig_vec)){
    ra11=cbind(ra11,RA[,c(grep(orig_vec[h], colnames(RA)))]) 
  }
  RA=ra11

  if(v) {
    Mlosses=max(RA[, grep("_AL",colnames(RA))])
    RA[, grep("_AL",colnames(RA))]=RA[, grep("_AL",colnames(RA))]/Mlosses
  }
  rac=NULL
  #print(head(RA))
  for(l in 1:nrow(RA)) rac=c(rac,calc.v(RA[l,], idr, orig_vec, ED))
  if(length(which(is.nan(rac)))>0) warning("Not all asessments are numeric in the First selsection")
  FR1O[i]=median(rac, na.rm=TRUE)
}}

FR2O=NULL
if(length(Srrr)>0) {for(i in 1:length(Srrr)){
  RA=getRelAs(Srrr[[i]], (as.numeric(Srrr[[i]][,grep("_raw_total_activities",colnames(Srrr[[i]]))])), fin=FALSE, v=v, orig_vec)
 # colnames(RA)=all_colls22
 
  ra22=RA[,c(grep(orig_vec[1], colnames(RA)))]
  
  #all_colls11=all_colls1[c(grep(orig_vec[1], colnames(RA1)))]
  #all_colls22=all_colls2[c(grep(orig_vec[1], colnames(RA2)))]
  for(h in 2:length(orig_vec)){
    ra22=cbind(ra22,RA[,c(grep(orig_vec[h], colnames(RA)))]) 
  }
  RA=ra22
  
  if(v) {
    Mlosses=max(RA[, grep("_AL",colnames(RA))])
    RA[, grep("_AL",colnames(RA))]=RA[, grep("_AL",colnames(RA))]/Mlosses
  }
  rac=NULL
  for(l in 1:nrow(RA)) rac=c(rac,calc.v(RA[l,], idr, orig_vec, ED))
  if(length(which(is.nan(rac)))>0) warning("Not all asessments are numeric in the Second selsection")
  FR2O[i]=median(rac, na.rm=TRUE)
}}
Ofr=data.frame(Routing_plan="Others")
FR1O=median(FR1O, na.rm=TRUE)
FR2O=median(FR2O, na.rm=TRUE)
if(is.null(FR1O))FR1O=NA
if(is.null(FR2O))FR2O=NA

Ofr=cbind(Ofr, FN=nrow(inrr), FRRR=(FR1O), SRRR=(FR2O), SN=nrow(tr))

FRR[,3]=round(as.numeric(as.character(FRR[,3])),4)
FRR[,4]=round(as.numeric(as.character(FRR[,4])),4)

colnames(Ofr)=colnames(FRR)
Ofr[,2]=as.numeric(as.character(Ofr[,2]))
Ofr[,5]=as.numeric(as.character(Ofr[,5]))
if(!is.na(Ofr[,3]))Ofr[,3]=round(as.numeric(as.character(Ofr[,3])), 4)
if(!is.na(Ofr[,4]))Ofr[,4]=round(as.numeric(as.character(Ofr[,4])), 4)

FRR[,1]=as.character(FRR[,1])
Ofr[,1]=as.character(Ofr[,1])

if(!v) FRR=rbind( Ofr, FRR,deparse.level = 0)
write.csv(FRR,  paste(cmp,".csv", sep=''), row.names = FALSE)
return (FRR)  
}
# Calculate the area between the target vector and gotten result
calc.v <- function(F, idr, orig_vec, ED=FALSE){
  #cc=NULL
  #print(F)
 #if(F[6]==1) idr=c(0,1,1,0,0.7) else idr=c(0,1, 1,0,0.9)
 F=as.numeric(F[seq(1,length(idr))])
# print(paste('calculated vector', F))
#print(paste('target vector', idr))
 if(ED){
   ED=sum((idr-F)^2)
   return(1-ED/length(idr))
 }
 else {
 PP=polygons(idr, F)
 S=area.poly(PP)
 #print(1-S/length(idr))
return(1-S/length(idr))}
}

# Not used now
calc.area <- function(idr){
  ss=0
  for(i in 1:(length(idr)-1)){
    ss=(ss+(min(idr[i:(i+1)])+(max(idr[i:(i+1)])-min(idr[i:(i+1)]))/2)) 
  }
  return(ss)
}

# Not used now
grepCol <- function(data, colname, not_contain=""){
  if(not_contain=="")
    return (as.numeric(data[,grep(colname,colnames(data))]))
}

# Calculate the artificial target parameters of routing result
getAssessments <- function(w, total=NULL, v=TRUE){
  
  w=replNA(w)

  #fn=which(as.numeric(w[,grep("_fitness",colnames(w))])==0)
  #if(length(fn)>0)  w=w[-fn,] # remove the routs that don't provide any results
  if(is.null(total))
    total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
  else if(length(total)==1)  total=replNA(total)
 
 # if (length(which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0))>0)
  #  w=w[-which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0),]
  total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
 AA=as.numeric(w[,grep("raw_assigned_activities",colnames(w))])  
 REJ_A=as.numeric(w[,grep("raw_rejected_activities",colnames(w))])

  sAA=AA/(as.numeric(total)-as.numeric(w[,grep("raw_rejected_activities",colnames(w))])
          -as.numeric(w[,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))])
          -as.numeric(w[,grep("raw_not_assigned_activities_provider_workday_stop",colnames(w))])
          -as.numeric(w[,grep("raw_not_assigned_activities_provider_overload",colnames(w))]))
 AA=total-REJ_A-as.numeric(w[,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))])
 -as.numeric(w[,grep("raw_not_assigned_activities_provider_workday_stop",colnames(w))])
 -as.numeric(w[,grep("raw_not_assigned_activities_provider_overload",colnames(w))])
 AAn=(total-REJ_A)/(total-REJ_A+sum(
   as.numeric(w[,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))]),
 as.numeric(w[,grep("raw_not_assigned_activities_provider_workday_stop",colnames(w))]),
 as.numeric(w[,grep("raw_not_assigned_activities_provider_overload",colnames(w))]),
 as.numeric(w[,grep("raw_not_assigned_activities_unacceptable_overdue",colnames(w))])))
#print(paste("sAA",sAA))
  wna=as.numeric(w[,grep("sum_not_assigned",colnames(w))])
  OvC=as.numeric(w[,grep("sum_overdue_time",colnames(w))])/AA
  OvCt=as.numeric(w[,grep("raw_overdue_time_sum",colnames(w))])/AA
  TrC=as.numeric(w[,grep("sum_travel_time",colnames(w))])/AA
  TrCt=as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/AA
  SlC=as.numeric(w[,grep("sum_sla_violation",colnames(w))])/AA
  SlCt=as.numeric(w[,grep("raw_sla_violation_sum",colnames(w))])/AA
  #print(paste("OvCt", length(OvCt), "TrCt", length(TrCt), "SlCt", length(SlCt)))
  RP=as.numeric(w[,grep("info_sum_assigned",colnames(w))])

  cn=grep("raw_not_assigned_activities",colnames(w))
  wc=cn[which(gsub("stage_","",gsub("initial_","",colnames(w)[cn]))=="raw_not_assigned_activities")]


  SNA=as.numeric(w[,grep("sum_not_assigned",colnames(w))])/as.numeric(w[,wc])
  if(v){
    AL=round((as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
          as.numeric(w[,grep("_sum_travel_time",colnames(w))])+
          as.numeric(w[,grep("_sum_sla_violation",colnames(w))]))/as.numeric(total), 5)
    R=(as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
         as.numeric(w[,grep("_sum_travel_time",colnames(w))])+
         as.numeric(w[,grep("_sum_sla_violation",colnames(w))])+
         as.numeric(w[,grep("_sum_overtime_soft",colnames(w))])+
         as.numeric(w[,grep("_sum_overtime_hard",colnames(w))]))
    RAA=R/AA
   # RPL=(RP-AL)/(RP+AL)
  } else {
    #Rule if all activities are assigned and 
    losses=as.numeric(w[,grep("sum_travel_time",colnames(w))])+
      # as.numeric(w[,grep("sum_sla_violation",colnames(w))])+
      as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
      as.numeric(w[,grep("sum_overtime_hard",colnames(w))])+
      as.numeric(w[,grep("sum_work_time",colnames(w))])+
      as.numeric(w[,grep("sum_waiting_time",colnames(w))])
    R=(RP-(losses))
    R1=losses/RP
R1[which(R1>1)]=losses[which(R1>1)]/(RP[which(R1>1)]+as.numeric(w[which(R1>1),grep("sum_would_be_not_assigned",colnames(w))]))    

#update
#R1[which(R1>1)]=1
#R[which(R<0)]=0
    
    Rt=NULL
    for(h in 1:nrow(w)){
      cn=grep("raw_not_assigned_activities",colnames(w))
      wc=cn[which(gsub("stage_","",gsub("initial_","",colnames(w)[cn]))=="raw_not_assigned_activities")]
      
      if(as.numeric(w[h,wc])==0){
        Rt[h]=as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])/(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])+
                                                                        as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))]))
#print(paste("case1", h, Rt[h]))
      
      }
        else if(as.numeric(w[h, grep("info_sum_assigned",colnames(w))])/AA[h]>
                  as.numeric(w[h, grep("sum_not_assigned",colnames(w))])/as.numeric(w[h, wc] )){
        Rt[h]=as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])/(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])+
                                                                        as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))])+
                                                                        as.numeric(w[h,grep("raw_waiting_time_sum",colnames(w))]))
       #print(paste("case2", h, Rt[h]))
      }
      else if(as.numeric(w[h,wc])==
        as.numeric(w[h,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))])
        #,
        #as.numeric(w[h,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))]),
        #as.numeric(w[h,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))]),))
      ){
        Rt[h]=(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])#+ as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))])
               )/
          (as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])+
                                                                         as.numeric(w[h,grep("raw_waiting_time_sum",colnames(w))])+
                                                                         as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))]))
      }
      else {
        Rt[h]=as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])/(as.numeric(w[h,grep("raw_work_time_sum",colnames(w))])+
                                                                         as.numeric(w[h,grep("raw_waiting_time_sum",colnames(w))])+
                                                                         as.numeric(w[h,grep("raw_travel_time_sum",colnames(w))]))
        #print(paste("case3", h, Rt[h]))   
      }
      
     
    }
    
AL=round((1-R1),5)
    RAA=Rt
  }
  
  WT=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])+as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])+as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])
  RD=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])/WT
# Overtime in minutes
Ov=(as.numeric(w[,grep("raw_overtime_soft_sum",colnames(w))])+as.numeric(w[,grep("raw_overtime_hard_sum",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))])
 #OvWt=Ov/(WT/as.numeric(w[,grep("providers_used",colnames(w))]))

OvWt=Ov/(max(Ov, WT)/as.numeric(w[,grep("providers_used",colnames(w))]))

#Overtime in costs
OVtC=  (as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
          as.numeric(w[,grep("sum_overtime_hard",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))])
OVtCW=OVtC/((as.numeric(w[,grep("sum_work_time",colnames(w))])+as.numeric(w[,grep("sum_travel_time",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))]))
#update
# OvWt[which(OvWt>1)]=NAOVtCW
# OVtCW[which(OvWt>1)]=NA
# 
# OVtCW[which(OvWt>1)]
# 
# 
# OvWt[which(OvWt>1)]=OVtCW[which(OvWt>1)]
# OvWt[which(OvWt>1)]=1
WT=WT/as.numeric(w[,grep("providers_used",colnames(w))])
  RSAA=RP/(RP+as.numeric(w[,grep("sum_not_assigned",colnames(w))])) # % of assigned cost
  RSLA=((as.numeric(w[,grep("sum_overdue_time",colnames(w))])+
      as.numeric(w[,grep("sum_travel_time",colnames(w))])+
      as.numeric(w[,grep("sum_sla_violation",colnames(w))])+
      as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
      as.numeric(w[,grep("sum_overtime_hard",colnames(w))])+
        as.numeric(w[,grep("sum_waiting_time",colnames(w))])+
        as.numeric(w[,grep("sum_work_time",colnames(w))])))/RP# % of losses
RCD= as.numeric(w[,grep("sum_work_time",colnames(w))])/(#as.numeric(w[,grep("_sum_overdue_time",colnames(w))])+
                                                             as.numeric(w[,grep("sum_travel_time",colnames(w))])+
                                                            # as.numeric(w[,grep("_sum_sla_violation",colnames(w))])+
                                                             as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
                                                             as.numeric(w[,grep("sum_overtime_hard",colnames(w))])+
                                                              as.numeric(w[,grep("sum_waiting_time",colnames(w))])+
                                                             as.numeric(w[,grep("sum_work_time",colnames(w))]))
  
  RPA=R/RP
# Travel_as=(as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))]))/
#   sum((as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))])),
#       (as.numeric(w[,grep("raw_work_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))])),
#       (as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])/as.numeric(w[,grep("providers_used",colnames(w))])))
Travel_as=(as.numeric(w[,grep("raw_travel_time_sum",colnames(w))]))/
  ((as.numeric(w[,grep("raw_travel_time_sum",colnames(w))]))+
      (as.numeric(w[,grep("raw_work_time_sum",colnames(w))]))+
      (as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])))

 #R=data.frame(id_rgt=as.numeric(w[,grep("N_id_rgt",colnames(w))]),AA=AA,  sAA=sAA,OvC=OvCt, TrC=TrCt, SlC=SlCt, RP=RP, AL=AL, WT=WT, RD=RD, RCD=RCD, Ovt=Ov, OvWt=OvWt,RV=R, SNA=SNA, RAA=RAA,RSAA=RSAA, RSLA=RSLA, TRAS=Travel_as, stringsAsFactors=F)
  
R=data.frame(id_rgt=as.numeric(w[,grep("N_id_rgt",colnames(w))]),AA=AA/as.numeric(w[,grep("providers_used",colnames(w))]),  TRT=as.numeric(w[,grep("sum_travel_time",colnames(w))]),
             TRC=as.numeric(w[,grep("raw_travel_time_sum",colnames(w))]), SlC=SlC, OVT=Ov, OVC=OVtC, RD, TRAS=Travel_as, RAA=RAA, 
             AL=AL,OvWt=OvWt, sAA=sAA, AAn=AAn, total=total, stringsAsFactors=F)


as.numeric(w[,grep("sum_travel_time",colnames(w))])
if (length(AL[which(AL>1 | AL<0)])>0){
 k=unique(which(AL>1 | AL<0))

  print(paste("Incorrect value of relative costs =", AL[k], " ind = ",k, 
             "routing plan name ", as.character(w[,grep("routing_plan_name",colnames(w))][k])[1] ,
             "N_id_rgt ", as.numeric(w[k,grep("N_id_rgt",colnames(w))])[1])#, call.=TRUE
  )}
#print(replNAN(R))
  return (replNAN(R))
}

getPlots <- function(w){
  w=replNA(w)
  total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
  REJ_A=as.numeric(w[,grep("raw_rejected_activities",colnames(w))])
  AA=total-REJ_A-as.numeric(w[,grep("raw_not_assigned_activities_unacceptable_travel_time",colnames(w))])
  -as.numeric(w[,grep("raw_not_assigned_activities_provider_workday_stop",colnames(w))])
  -as.numeric(w[,grep("raw_not_assigned_activities_provider_overload",colnames(w))])
  SlC=as.numeric(w[,grep("sum_sla_violation",colnames(w))])/AA
  Ov=(as.numeric(w[,grep("raw_overtime_soft_sum",colnames(w))])+as.numeric(w[,grep("raw_overtime_hard_sum",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))])
  OVtC=  (as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
            as.numeric(w[,grep("sum_overtime_hard",colnames(w))]))/as.numeric(w[,grep("providers_used",colnames(w))])
  WT=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])+as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])+as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])
  RD=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])/WT
  R=data.frame(id_rgt=as.numeric(w[,grep("N_id_rgt",colnames(w))]),AA=AA/as.numeric(w[,grep("providers_used",colnames(w))]),  TRT=as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/AA,
               TRC=as.numeric(w[,grep("sum_travel_time",colnames(w))])/AA, SlC=SlC, RD, OVT=Ov, OVC=OVtC, stringsAsFactors=F)
  return(R)
}

# Form the calculated vector in the definite format
getRelAs <- function(w, total, fin=FALSE, v=FALSE, orig_vec){
  rw <- getAssessments(w, total, v)
  rw=as.data.frame(rw)
#print(head(rw))
if(fin){
rw1=rw[, c(grep(orig_vec[1], colnames(rw)))]
for(j in 2:length(orig_vec)){
  rw1=cbind(rw1,rw[, c(grep(orig_vec[j], colnames(rw)))])
}
return (rw1)
}
else return (rw)
}

# Get the detailed data for routing runs
getRPdata <- function(w, total=NULL, v=TRUE){
  w=replNA(w)
  #print(head(w))
  fn=which(as.numeric(w[,grep("_fitness",colnames(w))])==0)
 # if(length(fn)>0)  w=w[-fn,] # remove the routs that don't provide any results
  if(is.null(total))
    total=as.numeric(w[,grep("raw_total_activities",colnames(w))])
  else if(length(total)==1)  total=replNA(total)

  if (length(which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0))>0)
    w=w[-which(as.numeric(w[,grep("raw_assigned_activities",colnames(w))])==0),]
  pr=as.numeric(w[,grep("providers_used",colnames(w))])
  TrPr=as.numeric(w[,grep("raw_travel_time_sum",colnames(w))])/pr
  WrtPr=as.numeric(w[,grep("raw_work_time_sum",colnames(w))])/pr
  WtPr=as.numeric(w[,grep("raw_waiting_time_sum",colnames(w))])/pr
  total=as.numeric(w[,grep("raw_total_activities",colnames(w))])

 cn=grep("raw_not_assigned_activities",colnames(w))
 wc=cn[which(gsub("stage_","",gsub("initial_","",colnames(w)[cn]))=="raw_not_assigned_activities")]
  AA=total- as.numeric(w[,wc])- as.numeric(w[,grep("raw_rejected_activities",colnames(w))])
 
  AAPr=AA/pr
  OvPr=(as.numeric(w[,grep("raw_overtime_soft_sum",colnames(w))])+as.numeric(w[,grep("raw_overtime_hard_sum",colnames(w))]))/pr
  id=as.numeric(w[,grep("N_id_rgt",colnames(w))])
  
  sla_act=as.numeric(w[,grep("raw_sla_violation_activities",colnames(w))])/AA
 # sla_time=(as.numeric(w[,grep("raw_sla_violation_sum",colnames(w))])/as.numeric(w[,grep("raw_sla_violation_activities",colnames(w))]))/as.numeric(w[,grep("raw_work_time_sum",colnames(w))])
  sla_time=(as.numeric(w[,grep("raw_sla_violation_sum",colnames(w))])/as.numeric(w[,grep("raw_sla_violation_activities",colnames(w))]))#/as.numeric(w[,grep("raw_work_time_sum",colnames(w))])
  sum=100*(as.numeric(w[,grep("sum_travel_time",colnames(w))])+as.numeric(w[,grep("sum_overdue_time",colnames(w))])+
         as.numeric(w[,grep("sum_work_time",colnames(w))])+as.numeric(w[,grep("sum_overtime_soft",colnames(w))])+
         as.numeric(w[,grep("sum_overtime_hard",colnames(w))]))/as.numeric(w[,grep("info_sum_assigned",colnames(w))])
 #print(paste("id_rgt",length(id), "AA", length(AAPr), "WrtPr", length(WrtPr), "TrPr", length(TrPr), "WtPr", length(WtPr), "OvPr", length(OvPr), "PR", length(pr), "Sum", length(sum), "Sla_act",length(sla_act), "Sla_time", length(sla_time)))
R=data.frame(id_rgt=id, AA=AAPr, WrtPr=WrtPr, TrPr=TrPr, WtPr=WtPr, OvPr=OvPr, PR=pr, Sum=sum, Sla_act=sla_act, Sla_time=sla_time, stringsAsFactors=F)  
#  print("RP end 2")
 # R=data.frame(id_rgt=id, AA=AAPr, WrtPr=WrtPr, TrPr=TrPr, WtPr=WtPr, OvPr=OvPr, PR=pr, stringsAsFactors=F)  
  return (replNAN(R))
}


draw.As <- function(company){
  vmfd=read.csv(paste(company,"_FD.csv", sep=''), sep=',')
  assess=c("AA_","OvC_","TrC_","SlC_","AL_","WT_","RD_","Ovt_")
  assessN=c("Number of assigned activities","Overdue cost","Travel cost","SLA violation cost","Summary relative cost","Work time","Rout density","Overtime")
  vmfdS=data.frame()
  for(i in 1:length(assess)){
    if(i==1) {
      vmfdS=vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])]
    }
    else{vmfdS=cbind(vmfdS, vmfd[,which(colnames(vmfd) %in% colnames(vmfd)[grep(assess[i], colnames(vmfd))])])}}
  par(mfrow=c(2,2))
  k=1
  vmfdSs= sapply(vmfdS, median, na.rm=TRUE)
  for(i in 1:length(assess)){
    boxplot(vmfdS[,c(k,(k+1))], col=c('red', 'blue', 'green'), main = paste(company,assessN[i]))
    #  barplot(vmfdSs[seq(k,(k+2))], col=c('red', 'blue', 'green'), main = paste(company,assessN[i]))
    k=k+3
  }
}

# Calculate the area between vectors
point <- function(x1,y1,x2,y2,x3,y3,x4,y4){
  dx1=(x2-x1)
  dy1=(y2-y1)
  dx2=(x4-x3)
  dy2=(y4-y3)
  x = dy1 * dx2 - dy2 * dx1;

  if(!x || !dx2)
    return  (F);
  y = x3 * y4 - y3 * x4;
  x = ((x1 * y2 - y1 * x2) * dx2 - y * dx1) / x;
  y = (dy2 * x - y) / dx2;
  return (c(x,y))
}
area.poly <- function(PP){
  S=0
  for(i in 1:length(PP)){
       S=S+areapl(PP[[i]])
  }
  return (S)
}
polygons <- function(idr, R11){
idrx=seq(1, length(idr))
V1=idr
#print("R111111111")
#print(R11)
V2=as.numeric(R11)
PP=list()
k=1
for(i in 1:(length(V1)-1)){
  P=point(idrx[i],V1[i],idrx[i+1],V1[i+1],idrx[i],V2[i],idrx[i+1],V2[i+1])
  if(P[1]<max(idrx[i:(i+1)]) && P[2]<max(c(V1[i:(i+1)], V2[i:(i+1)])) &&
                                          (P[1]>min(idrx[1:(i+1)]) && P[2]>min(c(V1[i:(i+1)], V2[i:(i+1)])))){
    xx=c(idrx[i], idrx[i], P[1], idrx[i])
    yy=c(V1[i], V2[i], P[2], V1[i])
    PP[[k]]=poly(cbind(x=xx,y=yy), raw=TRUE)
    k=k+1
    xx=c(idrx[i+1], idrx[i+1], P[1], idrx[i+1])
    yy=c(V1[i+1], V2[i+1], P[2], V1[i+1])
    PP[[k]]=poly(cbind(x=xx,y=yy), raw=TRUE)
   # print(paste('point', i, 'value', PP[[k]]))
  }
  else {
    xx=c(idrx[i], idrx[i], idrx[i+1], idrx[i+1],idrx[i])
    if(V2[i]<V1[i])
    yy=c(V2[i], V1[i], V1[i+1], V2[i+1],V2[i])
    else if(V2[i]>=V1[i])
      yy=c(V1[i], V2[i], V2[i+1], V1[i+1], V1[i])
    PP[[k]]=poly(cbind(x=xx,y=yy), raw=TRUE)
   # print(paste('point', i, 'value', PP[[k]]))
  }
  k=k+1
}
return(PP)
}