require(shiny)
source("routin_version_comparison.R")
# Code to make a message that shiny is loading
# Make the loading bar
loadingBar <- tags$div(class="progress progress-striped active",
                       tags$div(class="bar", style="width: 100%;"))
# Code for loading message
loadingMsg <- tags$div(class="modal", tabindex="-1", role="dialog", 
                       "aria-labelledby"="myModalLabel", "aria-hidden"="true",
                       tags$div(class="modal-header",
                                tags$h3(id="myModalHeader", "Loading...")),
                       tags$div(class="modal-footer",
                                loadingBar))
# The conditional panel to show when shiny is busy
loadingPanel <- conditionalPanel(paste("input.goButton > 0 &&", 
                                       "$('html').hasClass('shiny-busy')"),
                                 loadingMsg)


# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
 
  headerPanel(
    list(tags$head(tags$style("#text1{color: black;  font-size: 20px; font-style: italic; }
                              #text2{color: black;  font-size: 20px; font-style: italic; }
                              #text3{color: black;  font-size: 20px; font-style: italic; }
                              #text4{color: red;  font-size: 40px; font-style: bold; }; }
                              #textD{color: red;  font-size: 40px; font-style: bold; }")),
         
         #HTML('<h1 style="color:red;  font-size: 40px; font-style: bold; align="center" ">  Results Assessment </h1>')
         #,
         fluidRow( column(offset=0, width=6,HTML('<img src="oracle-toa-logo.png", height="70px", width="550px", style="float:top"/>' )),
                   column(offset=10, width=6, HTML('<img src="rrr.png", height="90px", width="225px", style="float:bottom"/>'))),
         #HTML('<img src="oracle-toa-logo.png", height="70px", width="550px", style="float:top"/>' )
        # ,
         HTML('<h1 style="color:red;  font-size: 40px; font-style: bold; align="center" ">                       </h1>')#,
         #HTML('<img src="rrr.png", height="60px", width="150px", style="float:bottom"/>' )
        
         ),
    
    img(src="rrr.png", height = 50, width = 50)
    ),

  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
#   fluidRow(
#     column(8,
#            hr(),
#            verbatimTextOutput('comp'),
#            selectInput('in1', 'Options', c(Choose='','comp.names'), selectize=FALSE)
#     ),
#     column(2,actionButton("cmpOK", "Ok"))
#     ),
#   fluidRow(
#       column(4,
#              hr(),
#              verbatimTextOutput('out1'),
#              selectInput('in1', 'Options', c(Choose='', state.name), selectize=FALSE)
#       ),
#       column(2,
#              hr(),
#              verbatimTextOutput('out2'),
#              selectInput('in2', 'Options', state.name, selectize=FALSE)
#       )),
  sidebarLayout(

    sidebarPanel(width = 4,
                 align="left",
#                  br(),
#                  fluidRow( column(offset=1, width=12,actionButton("allReport", h4("General assessment for all companies")))),
#                  br(),
#                  checkboxInput("outliers", "Only unique run id", FALSE),
                 br(),
#                  fluidRow( column(3,h4(tags$div(paste0('Select company'), style='color:blue'), 
#                            h5(tags$div(uiOutput("choose_dataset"), style='color:red')),
#                            h5(tags$div(actionButton("selCmp", h5("Get data"))))))),
                 fluidRow( column(offset=0, width=4,h4("Select company")),
                           column(offset=0, width=5,h5(uiOutput("choose_dataset"))),
                           column(offset=0, width=3,h3(actionButton("selCmp", "Get information")))),
fluidRow( column(offset=0, width=5,h4(checkboxInput("outliers1", "One-to-one comparison", FALSE))
                 ,h4(checkboxInput("method", "Calculate Euclidean distance", FALSE))
                 )
          #,
         # column(offset=0, width=7,h5(tags$div(actionButton("goButton", "Get company assessment", icon = NULL, width = "25px")))),
         #column(offset=0, width=7,h5(tags$div(actionButton("goButton", "Get company assessment"), style="width:200px;")))
         ),
# fluidRow( column(offset=0, width=4,h4("Select routing plan")),
#           column(offset=0, width=8,h5(uiOutput("choose_dataset1")))),
# fluidRow( column(offset=5, width=7,h5(actionButton("expert", "Get routing plan assessment")))),
              #htmlOutput("choose_dataset"),
#                  selectInput("input_type", "Select company","comp"
#                  ),
                 #uiOutput("choose_dataset"),
                # actionButton("selCmp", "Get data"),
# actionButton("goButton", "Get company assessment"),
# checkboxInput("out1", "Only unique run id", FALSE),
h2(textOutput("input_type_text")),
#checkboxInput("outliers", "Merge routing runs (for stage only)", FALSE),


br(),
# br(),
# textOutput("textD"),
h4("Select parameters"),
fluidRow(column(offset=1, width=3,h5("Environment")),
         column(offset=0, width=4,selectInput("env1","", 
                     choices = c("prod", "test", "train", "stage"),
                     selected = "prod")),
          column(offset=0, width=4,selectInput("env2","", 
                     choices = c("prod", "test", "train", "stage"),
                     selected = "prod"))
           ),
fluidRow(column(offset=1, width=3,h5("Version")),
         column(offset=0, width=4, selectInput("vers1","", 
                                               choices = getVers(),
                                               selected =  getVers()[1]))
         ,
         column(offset=0, width=4,selectInput("vers2","", 
                                              choices = getVers(),
                                              selected =  getVers()[1]))
         ),
fluidRow(column(offset=0, width=8,h4("Select dates for run periods"))),


fluidRow(column(offset=4, width=8,dateRangeInput("dates", label = h5("Date range for 1-st sample"), start = Sys.Date(), end = Sys.Date()))),
fluidRow(column(offset=4, width=8,dateRangeInput("dates2", label = h5("Date range for 2-nd sample"), start = Sys.Date(), end =Sys.Date()))),
br()

         ),

    
    # Show a summary of the dataset and an HTML table with the 
    # requested number of observations
    mainPanel(
     
      #img(src="oracle-toa-logo.png", height = 400, width = 400),
        textOutput("text4"),
      br(),
     # verbatimTextOutput("textD1"),
     # textOutput("textD1"),
     tabsetPanel(
       tabPanel('Complex routing comparison', br(),
                #if (allReport==TRUE)
                fluidRow(column(offset=0, width=12,dataTableOutput("General"))),
               # else
                 # fluidRow(   column(offset=0, width=12, HTML('<img src="WFM-in-the-cloud.png", height="600px", width="800px", style="float:top"/>' ))),
               fluidRow(   column(offset=0, width=12, HTML('<img src="Unknown-4.png", height="500px", width="700px", style="float:top"/>' ))),
               
               fluidRow(  
                            column(offset=0, allign='right', width=12,actionButton("allReport", h4("General assessment for all companies"))))
                                 #, dataTableOutput("General")
                                # )
#                 ,
#                 selectInput("plot.type",h4("Detailed analysis of parameters:"), 
#                             list(Assigned_activities = "Assigned_activities", Travel_time = "Travel_time", Travel_cost = "Travel_cost", SLA_violation_per_activity = "SLA_violation_per_activity"
#                                  , Rout_density = "Rout_density", Overtime = "Overtime", Overtime_cost="Overtime_cost")
#                 ),
#                 br(),
#                 actionButton("goButton1", "Show"),
#                 
#                 plotOutput("parPlot")
                ),
       tabPanel('Information',dataTableOutput("textD1"),
                fluidRow( column(offset=0, width=7,h5(tags$div(actionButton("goButton", "Get company assessment"), style="width:200px;"))))),
       tabPanel(textOutput("input_comp_name"),dataTableOutput("fintable"), 
                selectInput("plot.type",h4("Detailed analysis of parameters:"), 
                                                                                      list(Assigned_activities = "Assigned_activities", Travel_time = "Travel_time", Travel_cost = "Travel_cost", SLA_violation_per_activity = "SLA_violation_per_activity"
                                                                                           , Rout_density = "Rout_density", Overtime = "Overtime", Overtime_cost="Overtime_cost")
       ),
       br(),
       actionButton("goButton1", "Show"),
       
       plotOutput("parPlot")
       ),
       tabPanel('Assessment for routing plans',dataTableOutput("mytable1")),
       tabPanel(textOutput("input_routing_plan"),fluidRow( column(offset=0, width=2,h4("Select routing plan")),
                                                           column(offset=0, width=6,h5(uiOutput("choose_dataset1"))),
 column(offset=0, width=4,h5(actionButton("expert", "Get routing plan assessment")))),
                dataTableOutput("mytable2"),
 textOutput("plot_params"),
                plotOutput("vecPlot")
#                 , selectInput("plot.type",h4("Detailed analysis of parameters:"), 
#                                                                                           list(Assigned_activities = "Assigned_activities", Travel_time = "Travel_time", Travel_cost = "Travel_cost", SLA_violation_per_activity = "SLA_violation_per_activity"
#                                                                                                , Rout_density = "Rout_density", Overtime = "Overtime", Overtime_cost="Overtime_cost")
#        ),
#        br(),
#        actionButton("goButton1", "Show"),
#        
#        plotOutput("parPlot"))
#        
       #       tableOutput("view")
     )
)#,
#      tabsetPanel(
#        #tags$head(tags$style(".table .alignRight {color: blue; text-align:right;}")),
#        tabPanel('',dataTableOutput("textD1"))
#      ),
#       br(),
#       textOutput("text1"),
#       br(),
#       textOutput("text2"),
#       br(),
#       textOutput("text3"),
#       br(),
     # textOutput("dates"),
      
#       tabsetPanel(
#           tabPanel('Complex routing assessment',dataTableOutput("fintable"))
#         #       tableOutput("view")
#       ),
#tabPanel('Complex routing assessment',dataTableOutput("fintable")),
#h4('Complex routing assessment'),
#tableOutput("fintable"),
#       ,
#        
#       
#       

#plotOutput("parPlot2"),
# tabsetPanel(
#   tabPanel('Assessment for routing plans',dataTableOutput("mytable1"))
#   
# #       tableOutput("view")
#     ),
# 
# h4(textOutput("input_routing_plan")),
# tabsetPanel(
#   tabPanel('',dataTableOutput("mytable2"))
#   
#   #       tableOutput("view")
# ),

# selectInput("plot.type",h4("Detailed analysis of parameters:"), 
#             list(Assigned_activities = "Assigned_activities", Travel_time = "Travel_time", Travel_cost = "Travel_cost", SLA_violation_per_activity = "SLA_violation_per_activity"
#                  , Rout_density = "Rout_density", Overtime = "Overtime", Overtime_cost="Overtime_cost")
# ),
# br(),
# actionButton("goButton1", "Show"),
# 
# plotOutput("parPlot")
#,
# h4("Summary of performance time"),
# # tableOutput("summary"),
# tabsetPanel(
#   tabPanel('',dataTableOutput("summary"))
#   
#   #       tableOutput("view")
# )

#verbatimTextOutput("summary1"),
#plotOutput("histHT"),
#plotOutput("histMT")

#,
#verbatimTextOutput("summary")#,
# plotOutput("figPlot"),
# plotOutput("distPlot")
  )
)))