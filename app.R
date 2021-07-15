library(shiny)
library(igraph)
library(magrittr)
library(visNetwork)
library(data.table)
library(DT)
library(shinydashboard)
library("shinydashboardPlus")
library(shinythemes)

set.seed(5)

drugs<-read.csv2("Files/drug_list.txt",stringsAsFactors = F,sep="\t",header=F)[,1]

ui <- navbarPage("DDInteract",theme=shinytheme("flatly"),
                 tabPanel("Networks",
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              selectInput("net_choice","Select the population of interest:",choices = c("Catalonia","Indianapolis","Blumenau")),
                              selectInput("age_choice","Select the age range of interest:",choices = c("All","00-04","05-09","10-14","15-19","20-24",
                                                                                                       "25-29","30-34","35-39","40-44","45-49","50-54",
                                                                                                       "55-59","60-64","65-69","70-74","75-79","80-84",
                                                                                                       "85-89",">90")),
                              selectInput("sev_choice", "Select the severity of the interactions:",choices=c("All","Minor","Moderate","Major")),
                              selectInput("pos_neg_interactions", "Select the gender associated risk:",choices=c("All","Higher for females","Higher for males")),
                              selectInput("significance","Select the significant interactions:",choice=c("All","Significant")),
                              sliderInput("strength","Select the strength of the association:", min=0, max=1, value=0),
                              sliderInput("population","Select the number of patients with interactions every 100,000 inhabitants:",
                                          min=0,max=1000,value=0)
                              ),
                            mainPanel(
                              width=10,
                              h2("Co-administration patterns of interacting drugs vary depending on age, gender and healthcare system", align = "center"),
                              # h3("Jon Sanchez-Valle, Rion Brattig Correia, Marta Camacho-Artacho, Alba Lepore, Mauro M. Mattos, Luis M. Rocha, Alfonso Valencia",align="center"),
                              fluidRow(
                                htmlOutput("txt"),
                                box(
                                  textOutput('textf'),
                                  tags$head(tags$style("#textf{color: #B64539}")),
                                  textOutput('textm'),
                                  tags$head(tags$style("#textm{color: #4A74A9}")),
                                  textOutput('texte'),
                                  textOutput('textn'),
                                  textOutput('textc'),width=12,align="center"),
                                box(visNetworkOutput("net_plot", height = "700px"),width = 10),
                                box(imageOutput("myImage",width="100%"),width = 2,align = "center"),
                                box(
                                  textOutput("selected_net"),
                                  textOutput("selected_age"),
                                  textOutput("selected_sev"),width=12,align="center"),
                                DTOutput("net_dt_table",width='95%'),
                                align="center"
                              )
                            )
                          )
                 ),
                 navbarMenu("Co-dispensation patterns",
                 tabPanel("Single drugs",
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              selectInput("drug","Select your drug of interest:",drugs,selected = drugs[701])
                              # selectizeInput("drug","Select your drug of interest:",choices=drugs,options=list(maxOptions=5))
                            ),
                            mainPanel(
                              width=10,
                              h2("Co-administration patterns of interacting drugs vary depending on age, gender and healthcare system", align = "center"),
                              # h3("Jon Sanchez-Valle, Rion Brattig Correia, Marta Camacho-Artacho, Alba Lepore, Mauro M. Mattos, Luis M. Rocha, Alfonso Valencia",align="center"),
                              fluidRow(
                                textOutput('text1'),
                                tags$head(tags$style("#text1{color: #B64539}")),
                                textOutput('text2'),
                                tags$head(tags$style("#text2{color: #4A74A9}")),
                                textOutput("selected_drug"),
                                box(plotOutput(outputId = "bluddidistributions"),width=4),
                                box(plotOutput(outputId = "catddidistributions"),width=4),
                                box(plotOutput(outputId = "indddidistributions"),width=4),
                                br(),
                                br(),
                                h5("Percentage of patients taking co-dispensations"),
                                box(plotOutput(outputId = "blucoaddistributions"),width=4),
                                box(plotOutput(outputId = "catcoaddistributions"),width=4),
                                box(plotOutput(outputId = "indcoaddistributions"),width=4),
                                br(),
                                br(),
                                h5("Percentage of patients taking the selected drug"),
                                box(plotOutput(outputId = "bludistributions"),width=4),
                                box(plotOutput(outputId = "catdistributions"),width=4),
                                box(plotOutput(outputId = "inddistributions"),width=4),
                                align="center"
                              )
                            )
                          )
                 ),
                 tabPanel("Drug pairs",
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              selectInput("drug1","Select one drug:",drugs,selected = drugs[701]),
                              selectInput("drug2","Select a second drug:",drugs,selected = drugs[16])
                            ),
                            mainPanel(
                              width=10,
                              h2("Co-administration patterns of interacting drugs vary depending on age, gender and healthcare system", align = "center"),
                              # h3("Jon Sanchez-Valle, Rion Brattig Correia, Marta Camacho-Artacho, Alba Lepore, Mauro M. Mattos, Luis M. Rocha, Alfonso Valencia",align="center"),
                              fluidRow(
                                textOutput('text3'),
                                tags$head(tags$style("#text3{color: #B64539}")),
                                textOutput('text4'),
                                tags$head(tags$style("#text4{color: #4A74A9}")),
                                h5("Percentage of patients taking the interacting drugs"),
                                box(plotOutput(outputId = "bluddipairdistributions"),width=4),
                                box(plotOutput(outputId = "catddipairdistributions"),width=4),
                                box(plotOutput(outputId = "indddipairdistributions"),width=4),
                                br(),
                                br(),
                                textOutput("selected_drug1"),
                                box(plotOutput(outputId = "bludrug1distributions"),width=4),
                                box(plotOutput(outputId = "catdrug1distributions"),width=4),
                                box(plotOutput(outputId = "inddrug1distributions"),width=4),
                                br(),
                                br(),
                                textOutput("selected_drug2"),
                                box(plotOutput(outputId = "bludrug2distributions"),width=4),
                                box(plotOutput(outputId = "catdrug2distributions"),width=4),
                                box(plotOutput(outputId = "inddrug2distributions"),width=4),
                                align="center"
                              )
                            )
                          )
                 )),
                 
                 tabPanel("Documentation",
                          sidebarLayout(
                            sidebarPanel(
                              width = 0
                            ),
                            mainPanel(
                              width=12,
                              h1("Co-administration patterns of interacting drugs vary depending on age, gender and healthcare system", align = "center"),
                              h3("Jon Sanchez-Valle, Rion Brattig Correia, Marta Camacho-Artacho, Alba Lepore, Mauro M. Mattos, Luis M. Rocha, Alfonso Valencia",align="center"),
                              br(),
                              br(),
                              h4("Blumenau:",align="left"),
                              h5("Blumenau, in Southern Brazil, is a city with a very high Human Development Index and a population of approximately 340,000 inhabitants. 
                                 Brazil has a universal public health-care system, and Blumenau possesses a city-wide Health Information System (HIS) with prescription and 
                                 dispensation information for its entire population. Drug dispensing data for 18 months (January 2014 - June 2015) were gathered from the Pronto HIS. 
                                 Electronic Health Records were anonymized at the source and only drug dispensation and demographic variables such as age, gender, heighborhood and 
                                 educational level were kept. In the study period, drugs were administered to 132,722 people.",align="left"),
                              h4("Catalonia:",align="left"),
                              h5("Catalonia is an autonomous community in the northeast of Spain with a population of 7,6 million people. The Spanish National Health Service is a 
                                 public healthcare system with copayment prescription charges that provides universal coverage, financed mainly by tax revenue. The Catalan Health 
                                 Institute manages primary health care teams that serve 74% of the population. The coded clinical information recorded in Electronic Health Records by its 
                                 274 primary health care teams is contained at the Catalan Health Institute's Information System for the Development of Research in Primary Care (SIDIAP) since 2006.  
                                 Diagnoses, drug prescriptions, and demographic variables such as age and gender from January 2008 to December 2018 were gathered. 
                                 In the study period, 5,5 million people were diagnosed with at least one disease or bought a drug."),
                              h4("Indianapolis:",align="left"),
                              h5("Indianapolis is a city in the United States with a private healthcare system and a population of 876,682 people. The Indiana University 
                                 Health System provides information on drug dispensation and disease diagnosis using ICD10 codes from primary, secondary and tertiary. Electronic Health Records 
                                 from January 2017 to December 2018 were analyzed.",align="left"),
                              h4("Strength of the association:",align="left"),
                              h5("The strength of the association (edges' width) is calculated as the mean number of days/months two drugs known to interact
                                 are given together vs. separated in the population.",align="left"),
                              h4("Relative Risks (RR):",align="left"),
                              h5("The Relative Risks for females are calculated as the percentage of females taking the drug-drug interaction divided 
                                 by the percentage of males taking the drug-drug interactions (we assign them positive values), while the RR for males 
                                 is calculated the other way around (and a negative value is assign). In the network, those interactions with a higher 
                                 risk associated to females are colored in red, while those at a higher risk for males are colored in blue.",align="left"),
                              h4("Nodes' size and color:",align="left"),
                              h5("The size of the node ranges from 0 to 1, and is calculated as the probability of the drug to be involved in an interaction. The 
                                 color is based on the drugs.com category they belong to.",align="left"),
                              h4("Severity and description of the interaction:",align="left"),
                              h5("The severity of the interaction is extracted from drugs.com, while the description of the interaction is extracted from DrugBank."),
                              h4("Significance of the interactions:",align="left"),
                              h5("Fisher's exact tests are conducted to analyze the significance of the co-dispensation of each pair of drugs based on the number 
                                 of patients taking each of the drugs, the number of patients taking both drugs together and the total number of patients 
                                 in the given population at the selected age range.",align="left"),
                              br(),
                              br()
                            )
                          )
                 ),
                 tabPanel("Who we are",
                          sidebarLayout(
                            sidebarPanel(
                              width = 0
                              # selectInput("drug","Select your drug of interest:",drugs)
                            ),
                            mainPanel(
                              width=12,
                              # h1("Co-administration patterns of interacting drugs vary depending on age, gender and healthcare system", align = "center"),
                              # h3("Jon Sanchez-Valle, Rion Brattig Correia, Marta Camacho-Artacho, Alba Lepore, Mauro M. Mattos, Luis M. Rocha, Alfonso Valencia",align="center"),
                              fluidRow(
                                box(tags$a(imageOutput("jonimage"),href="https://www.bsc.es/es/sanchez-jon/publications",target="_blank"),
                                    class="darkableImage",onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100",
                                    onmouseover="this.style.opacity=0.6;this.filters.alpha.opacity=60",
                                    width = 2,align = "center",height = 2),
                                box(tags$a(imageOutput("rionimage"),href="https://rionbr.github.io/",target="_blank"),
                                    class="darkableImage",onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100",
                                    onmouseover="this.style.opacity=0.6;this.filters.alpha.opacity=60",
                                    width = 2,align = "center",height = 2),
                                box(tags$a(imageOutput("martaimage"),href="https://www.lafarmaciadewalia.com/nuestro-equipo",target="_blank"),
                                    class="darkableImage",onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100",
                                    onmouseover="this.style.opacity=0.6;this.filters.alpha.opacity=60",
                                    width = 2,align = "center",height = 2),
                                box(tags$a(imageOutput("albaimage"),href="https://orcid.org/0000-0002-9481-2557",target="_blank"),
                                    class="darkableImage",onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100",
                                    onmouseover="this.style.opacity=0.6;this.filters.alpha.opacity=60",
                                    width = 2,align = "center",height = 2),
                                box(tags$a(imageOutput("mauroimage"),href="http://dsc.inf.furb.br/professores/informacoes/mauro-marcelo-mattos",target="_blank"),
                                    class="darkableImage",onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100",
                                    onmouseover="this.style.opacity=0.6;this.filters.alpha.opacity=60",
                                    width=2,align = "center",height = 2),
                                box(tags$a(imageOutput("luisimage"),href="https://informatics.indiana.edu/rocha/",target="_blank"),
                                    class="darkableImage",onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100",
                                    onmouseover="this.style.opacity=0.6;this.filters.alpha.opacity=60",
                                    width = 2,align = "center",height = 2),
                                box(tags$a(imageOutput("alfonsoimage"),href="https://www.icrea.cat/en/Web/ScientificStaff/alfonsovalencia-244256#researcher-nav",target="_blank"),
                                    class="darkableImage",onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100",
                                    onmouseover="this.style.opacity=0.6;this.filters.alpha.opacity=60",
                                    width = 2,align = "center",height = 2)
                              )
                            )
                          )
                 )
)

server <- function(input, output) {
  ## Section one ##
  ## @@ @@ @@ @@ ##
  output$myImage <- renderImage({
    filename <- normalizePath(file.path('./www','Colores_categorias.png'))
    list(src = filename,align="center",width="100%",height="100%")
  }, deleteFile = FALSE)
  ## Text ##
  output$selected_net <- renderText({
    paste("Population: ",input$net_choice,sep="")
  })
  output$selected_age <- renderText({
    paste("Age range: ",input$age_choice,sep="")
  })
  output$selected_sev <- renderText({
    paste("Severity: ",input$sev_choice,sep="")
  })
  ## Network ##
  output$net_plot <- renderVisNetwork({
    df <- read.csv(paste("Files/",input$net_choice,"_network_",input$age_choice,".txt",sep=""),stringsAsFactors = F,sep="\t",header=T)
    nodeinf<-read.csv(paste("Files/",input$net_choice,"_nodes.txt",sep=""),stringsAsFactors = F,sep="\t",header=T)
    nodecolor<-nodeinf$color ; names(nodecolor)<-nodeinf$id
    ## Create a range from 1 to 10 ##
    rangevals <- function(x){(10-1)*((x-min(x))/(max(x)-min(x)))+1}
    nodesize<-rangevals(nodeinf$value) ; names(nodesize)<-nodeinf$id
    ## Select the severity of the interaction ##
    if(input$sev_choice == 'All'){df <- df}
    if(input$sev_choice == 'Minor'){df <- df[df$severity=="Minor",]}
    if(input$sev_choice == 'Moderate'){df <- df[df$severity=="Moderate",]}
    if(input$sev_choice == 'Major'){df <- df[df$severity=="Major",]}
    ## Select the gender-associated risk representation ##
    if(input$pos_neg_interactions == 'All'){df <- df}
    if(input$pos_neg_interactions == 'Higher for females'){df <- df[as.numeric(df$RR) > 0,]}
    if(input$pos_neg_interactions == 'Higher for males'){df <- df[as.numeric(df$RR) < 0,]}
    ## Select the significant interactions ##
    if(input$significance == 'All'){df<-df}
    if(input$significance == "Significant"){df<-df[as.numeric(df$fdr)<=0.05,]}
    ## Select the strength of the association
    df <- df[((df$strength >= input$strength[1])), ]
    ## Select the number of patients taking the DDI
    df <- df[((df$onethousand >= input$population)),]
    graph <- graph_from_data_frame(df, directed=FALSE)
    # is_weighted(graph)
    E(graph)$weight <- df$strength
    E(graph)$color<-df$color
    V(graph)$color<-as.character(nodecolor[names(V(graph))])
    V(graph)$size<-as.character(nodesize[names(V(graph))])
    # Visualize the communities
    nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, color = V(graph)$color,value=V(graph)$size)
    # nodes <- data.frame(id = V(graph)$name, title = V(graph)$name, color = V(graph)$color,size=V(graph)$size)
    nodes <- nodes[order(nodes$id, decreasing = F),]
    edges <- get.data.frame(graph, what="edges")[1:2]
    edges$color <- df$color
    edges$value <- df$strength
    
    visNetwork(nodes, edges) %>%
      visExport() %>%
      visOptions(highlightNearest = list(enabled=TRUE, degree=1,algorithm="hierarchical",labelOnly=FALSE), 
                 nodesIdSelection = list(enabled=TRUE,style="width: 300px; height: 26px",main="Select your drug of interest")) %>%
      visIgraphLayout() %>%
      visInteraction(multiselect = T) %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
})
  observeEvent(input$current_node_id, {
    visNetworkProxy("net_plot") %>%
      visGetSelectedNodes()
  })
  ## Text ##
  output$textf <- renderText({"Red edges denote a higher risk for females"})
  output$textm <- renderText({"Blue edges denote a higher risk for males"})
  output$texte <- renderText({"The edges' width denote the strength of the association of interacting drugs (see Documentation)"})
  output$textn <- renderText({"The size of the node represents the propensity to be involved in an interaction (see Documentation)"})
  output$textc <- renderText({"Nodes are colored based on the category they belong to (drugs.com)"})
  # Table ##
  output$net_dt_table <- renderDT({
    df <- read.csv(paste("Files/",input$net_choice,"_network_",input$age_choice,".txt",sep=""),stringsAsFactors = F,sep="\t",header=T)
    ## Select the severity of the interaction ##
    if(input$sev_choice == 'All'){df <- df}
    if(input$sev_choice == 'Minor'){df <- df[df$severity=="Minor",]}
    if(input$sev_choice == 'Moderate'){df <- df[df$severity=="Moderate",]}
    if(input$sev_choice == 'Major'){df <- df[df$severity=="Major",]}
    ## Select the gender-associated risk representation ##
    if(input$pos_neg_interactions == 'All'){df <- df}
    if(input$pos_neg_interactions == 'Higher for females'){df <- df[as.numeric(df$RR) > 0,]}
    if(input$pos_neg_interactions == 'Higher for males'){df <- df[as.numeric(df$RR) < 0,]}
    ## Select the significant interactions ##
    if(input$significance == 'All'){df<-df}
    if(input$significance == "Significant"){df<-df[as.numeric(df$fdr)<=0.05,]}
    ## Select the strength of the association
    df <- df[((abs(df$strength) >= input$strength[1])), ]
    ## Select the number of patients taking the DDI
    df <- df[((df$onethousand >= input$population)),]
    ## Round the FDR and the number of patients
    df$fdr<-round(as.numeric(df$fdr),3)
    df$onethousand<-round(as.numeric(df$onethousand))
    if(is.null(input$net_plot_selected)){
      colnames(df) <- c("drug1", "drug2","strength", "RR","severity","patients(fem/mal)","description","color","pats/100,000","fdr")
      df
    }else{
      filtered <- df[(df$drug1 == input$net_plot_selected | df$drug2 == input$net_plot_selected),]
      colnames(filtered) <- c("drug1", "drug2","strength", "RR","severity","patients(fem/mal)","description","color","pats/100,000","fdr")
      filtered
    }
  })
  
  ## Section 2.1 ##
  ## @@ @@ @@ @@ ##
  ## Text ##
  output$selected_drug <- renderText({
    paste("Percentage of patients taking DDI co-dispensations associated with ",input$drug,sep="")
  })
  output$text1 <- renderText({"Females are colored in red"})
  output$text2 <- renderText({"Males are colored in blue"})
  ## Plot Blumenau DDI ##
  output$bluddidistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Blumenau_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femblu<-fread(paste("Files2/Blumenau_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malblu<-fread(paste("Files2/Blumenau_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femblu$pat_inter==0))<19 || length(which(malblu$pat_inter==0))<19){
        plot(c(1:19,1:19),c(femblu$pat_inter,malblu$pat_inter),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Blumenau\nDDI",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femblu$pat_inter,col="#C53A32",lwd=2)
        lines(1:19,malblu$pat_inter,col="#3C76AE",lwd=2)
        maxim<-max(c(femblu$pat_inter,malblu$pat_inter))
        axis(1,at=1:19,lab=femblu$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femblu$pat_inter==0))==19 && length(which(malblu$pat_inter==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDDI",cex.main=1.5)
        title("No interactions\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Blumenau_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDDI",cex.main=1.5)
      title("No interactions\ndetected",line=-10)
    }
  })
  ## Plot Catalonia DDI ##
  output$catddidistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Catalonia_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femcat<-fread(paste("Files2/Catalonia_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malcat<-fread(paste("Files2/Catalonia_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femcat$pat_inter==0))<19 || length(which(malcat$pat_inter==0))<19){
        plot(c(1:19,1:19),c(femcat$pat_inter,malcat$pat_inter),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Catalonia\nDDI",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femcat$pat_inter,col="#C53A32",lwd=2)
        lines(1:19,malcat$pat_inter,col="#3C76AE",lwd=2)
        maxim<-max(c(femcat$pat_inter,malcat$pat_inter))
        axis(1,at=1:19,lab=femcat$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femcat$pat_inter==0))==19 && length(which(malcat$pat_inter==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDDI",cex.main=1.5)
        title("No interactions\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Catalonia_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDDI",cex.main=1.5)
      title("No interactions\ndetected",line=-10)
    }
  })
  ## Plot Indianapolis DDI ##
  output$indddidistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Indianapolis_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femind<-fread(paste("Files2/Indianapolis_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malind<-fread(paste("Files2/Indianapolis_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femind$pat_inter==0))<19 || length(which(malind$pat_inter==0))<19){
        plot(c(1:19,1:19),c(femind$pat_inter,malind$pat_inter),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Indianapolis\nDDI",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femind$pat_inter,col="#C53A32",lwd=2)
        lines(1:19,malind$pat_inter,col="#3C76AE",lwd=2)
        maxim<-max(c(femind$pat_inter,malind$pat_inter))
        axis(1,at=1:19,lab=femind$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femind$pat_inter==0))==19 && length(which(malind$pat_inter==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDDI",cex.main=1.5)
        title("No interactions\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Indianapolis_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDDI",cex.main=1.5)
      title("No interactions\ndetected",line=-10)
    }
  })
  ## Plot Blumenau Coad ##
  output$blucoaddistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Blumenau_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femblu<-fread(paste("Files2/Blumenau_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malblu<-fread(paste("Files2/Blumenau_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femblu$pat_coad==0))<19 || length(which(malblu$pat_coad==0))<19){
        plot(c(1:19,1:19),c(femblu$pat_coad,malblu$pat_coad),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Blumenau\nCo-dispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femblu$pat_coad,col="#C53A32",lwd=2)
        lines(1:19,malblu$pat_coad,col="#3C76AE",lwd=2)
        maxim<-max(c(femblu$pat_coad,malblu$pat_coad))
        axis(1,at=1:19,lab=femblu$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femblu$pat_coad==0))==19 && length(which(malblu$pat_coad==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nCo-dispensations",cex.main=1.5)
        title("No co-dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Blumenau_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nCo-dispensations",cex.main=1.5)
      title("No co-dispensations\ndetected",line=-10)
    }
  })
  ## Plot Catalonia Coad ##
  output$catcoaddistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Catalonia_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femcat<-fread(paste("Files2/Catalonia_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malcat<-fread(paste("Files2/Catalonia_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femcat$pat_coad==0))<19 || length(which(malcat$pat_coad==0))<19){
        plot(c(1:19,1:19),c(femcat$pat_coad,malcat$pat_coad),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Catalonia\nCo-dispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femcat$pat_coad,col="#C53A32",lwd=2)
        lines(1:19,malcat$pat_coad,col="#3C76AE",lwd=2)
        maxim<-max(c(femcat$pat_coad,malcat$pat_coad))
        axis(1,at=1:19,lab=femcat$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femcat$pat_coad==0))==19 && length(which(malcat$pat_coad==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nCo-dispensations",cex.main=1.5)
        title("No co-dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Catalonia_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nCo-dispensations",cex.main=1.5)
      title("No co-dispensations\ndetected",line=-10)
    }
  })
  ## Plot Indianapolis Coad ##
  output$indcoaddistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Indianapolis_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femind<-fread(paste("Files2/Indianapolis_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malind<-fread(paste("Files2/Indianapolis_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femind$pat_coad==0))<19 || length(which(malind$pat_coad==0))<19){
        plot(c(1:19,1:19),c(femind$pat_coad,malind$pat_coad),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Indianapolis\nCo-dispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femind$pat_coad,col="#C53A32",lwd=2)
        lines(1:19,malind$pat_coad,col="#3C76AE",lwd=2)
        maxim<-max(c(femind$pat_coad,malind$pat_coad))
        axis(1,at=1:19,lab=femind$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femind$pat_coad==0))==19 && length(which(malind$pat_coad==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nCo-dispensations",cex.main=1.5)
        title("No co-dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Indianapolis_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nCo-dispensations",cex.main=1.5)
      title("No co-dispensations\ndetected",line=-10)
    }
  })
  ## Plot Blumenau Pat ##
  output$bludistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Blumenau_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femblu<-fread(paste("Files2/Blumenau_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malblu<-fread(paste("Files2/Blumenau_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femblu$patient==0))<19 || length(which(malblu$patient==0))<19){
        plot(c(1:19,1:19),c(femblu$patient,malblu$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Blumenau\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femblu$patient,col="#C53A32",lwd=2)
        lines(1:19,malblu$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femblu$patient,malblu$patient))
        axis(1,at=1:19,lab=femblu$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femblu$patient==0))==19 && length(which(malblu$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Blumenau_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  ## Plot Catalonia Pat ##
  output$catdistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Catalonia_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femcat<-fread(paste("Files2/Catalonia_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malcat<-fread(paste("Files2/Catalonia_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femcat$patient==0))<19 || length(which(malcat$patient==0))<19){
        plot(c(1:19,1:19),c(femcat$patient,malcat$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Catalonia\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femcat$patient,col="#C53A32",lwd=2)
        lines(1:19,malcat$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femcat$patient,malcat$patient))
        axis(1,at=1:19,lab=femcat$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femcat$patient==0))==19 && length(which(malcat$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Catalonia_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  ## Plot Indianapolis Pat ##
  output$inddistributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Indianapolis_females_",input$drug,".txt",sep=""),list.files("Files2/")))>0){
      femind<-fread(paste("Files2/Indianapolis_females_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malind<-fread(paste("Files2/Indianapolis_males_",input$drug,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femind$patient==0))<19 || length(which(malind$patient==0))<19){
        plot(c(1:19,1:19),c(femind$patient,malind$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Indianapolis\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femind$patient,col="#C53A32",lwd=2)
        lines(1:19,malind$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femind$patient,malind$patient))
        axis(1,at=1:19,lab=femind$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femind$patient==0))==19 && length(which(malind$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Indianapolis_females_",input$drug,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  
  
  
  
  
  ## Section 2.2 ##
  ## @@ @@ @@ @@ ##
  output$text3 <- renderText({"Females are colored in red"})
  output$text4 <- renderText({"Males are colored in blue"})
  ## Plot Blumenau DDI ##
  output$bluddipairdistributions<-renderPlot({
    ## Identify the subtables ##
    blufem<-list.files("Files3/")[grep("Blumenau_females",list.files("Files3/"))]
    blufemcual<-intersect(grep(paste("_",input$drug1,sep=""),blufem),grep(paste("_",input$drug2,sep=""),blufem))
    blumal<-list.files("Files3/")[grep("Blumenau_males",list.files("Files3/"))]
    blumalcual<-intersect(grep(paste("_",input$drug1,sep=""),blumal),grep(paste("_",input$drug2,sep=""),blumal))
    if(length(blufemcual)>0){
      femblu<-fread(paste("Files3/",blufem[blufemcual],sep=""),stringsAsFactors = F,sep="\t")
      malblu<-fread(paste("Files3/",blumal[blumalcual],sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femblu$patient==0))<19 || length(which(malblu$patient==0))<19){
        plot(c(1:19,1:19),c(femblu$patient,malblu$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Blumenau\nDDI",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femblu$patient,col="#C53A32",lwd=2)
        lines(1:19,malblu$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femblu$patient,malblu$patient))
        axis(1,at=1:19,lab=femblu$age_group,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femblu$patient==0))==19 && length(which(malblu$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDDI",cex.main=1.5)
        title("No interactions\ndetected",line=-10)
      }
    }
    if(length(blufemcual)==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDDI",cex.main=1.5)
      title("No interactions\ndetected",line=-10)
    }
  })
  ## Plot Catalonia DDI ##
  output$catddipairdistributions<-renderPlot({
    ## Identify the subtables ##
    catfem<-list.files("Files3/")[grep("Catalonia_females",list.files("Files3/"))]
    catfemcual<-intersect(grep(paste("_",input$drug1,sep=""),catfem),grep(paste("_",input$drug2,sep=""),catfem))
    catmal<-list.files("Files3/")[grep("Catalonia_males",list.files("Files3/"))]
    catmalcual<-intersect(grep(paste("_",input$drug1,sep=""),catmal),grep(paste("_",input$drug2,sep=""),catmal))
    if(length(catfemcual)>0){
      femcat<-fread(paste("Files3/",catfem[catfemcual],sep=""),stringsAsFactors = F,sep="\t")
      malcat<-fread(paste("Files3/",catmal[catmalcual],sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femcat$patient==0))<19 || length(which(malcat$patient==0))<19){
        plot(c(1:19,1:19),c(femcat$patient,malcat$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Catalonia\nDDI",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femcat$patient,col="#C53A32",lwd=2)
        lines(1:19,malcat$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femcat$patient,malcat$patient))
        axis(1,at=1:19,lab=femcat$age_group,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femcat$patient==0))==19 && length(which(malcat$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDDI",cex.main=1.5)
        title("No interactions\ndetected",line=-10)
      }
    }
    if(length(catfemcual)==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDDI",cex.main=1.5)
      title("No interactions\ndetected",line=-10)
    }
  })
  ## Plot Indianapolis DDI ##
  output$indddipairdistributions<-renderPlot({
    ## Identify the subtables ##
    indfem<-list.files("Files3/")[grep("Indianapolis_females",list.files("Files3/"))]
    indfemcual<-intersect(grep(paste("_",input$drug1,sep=""),indfem),grep(paste("_",input$drug2,sep=""),indfem))
    indmal<-list.files("Files3/")[grep("Indianapolis_males",list.files("Files3/"))]
    indmalcual<-intersect(grep(paste("_",input$drug1,sep=""),indmal),grep(paste("_",input$drug2,sep=""),indmal))
    if(length(indfemcual)>0){
      femind<-fread(paste("Files3/",indfem[indfemcual],sep=""),stringsAsFactors = F,sep="\t")
      malind<-fread(paste("Files3/",indmal[indmalcual],sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femind$patient==0))<19 || length(which(malind$patient==0))<19){
        plot(c(1:19,1:19),c(femind$patient,malind$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Indianapolis\nDDI",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femind$patient,col="#C53A32",lwd=2)
        lines(1:19,malind$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femind$patient,malind$patient))
        axis(1,at=1:19,lab=femind$age_group,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femind$patient==0))==19 && length(which(malind$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDDI",cex.main=1.5)
        title("No interactions\ndetected",line=-10)
      }
    }
    if(length(indfemcual)==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDDI",cex.main=1.5)
      title("No interactions\ndetected",line=-10)
    }
  })
  ## Text ##
  output$selected_drug1 <- renderText({
    paste("Percentage of patients taking ",input$drug1,sep="")
  })
  ## Plot Blumenau drug1 ##
  output$bludrug1distributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Blumenau_females_",input$drug1,".txt",sep=""),list.files("Files2/")))>0){
      femblu<-fread(paste("Files2/Blumenau_females_",input$drug1,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malblu<-fread(paste("Files2/Blumenau_males_",input$drug1,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femblu$patient==0))<19 || length(which(malblu$patient==0))<19){
        plot(c(1:19,1:19),c(femblu$patient,malblu$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Blumenau\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femblu$patient,col="#C53A32",lwd=2)
        lines(1:19,malblu$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femblu$patient,malblu$patient))
        axis(1,at=1:19,lab=femblu$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femblu$patient==0))==19 && length(which(malblu$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Blumenau_females_",input$drug1,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  ## Plot Catalonia drug1 ##
  output$catdrug1distributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Catalonia_females_",input$drug1,".txt",sep=""),list.files("Files2/")))>0){
      femcat<-fread(paste("Files2/Catalonia_females_",input$drug1,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malcat<-fread(paste("Files2/Catalonia_males_",input$drug1,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femcat$patient==0))<19 || length(which(malcat$patient==0))<19){
        plot(c(1:19,1:19),c(femcat$patient,malcat$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Catalonia\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femcat$patient,col="#C53A32",lwd=2)
        lines(1:19,malcat$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femcat$patient,malcat$patient))
        axis(1,at=1:19,lab=femcat$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femcat$patient==0))==19 && length(which(malcat$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Catalonia_females_",input$drug1,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  ## Plot Indianapolis drug1 ##
  output$inddrug1distributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Indianapolis_females_",input$drug1,".txt",sep=""),list.files("Files2/")))>0){
      femind<-fread(paste("Files2/Indianapolis_females_",input$drug1,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malind<-fread(paste("Files2/Indianapolis_males_",input$drug1,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femind$patient==0))<19 || length(which(malind$patient==0))<19){
        plot(c(1:19,1:19),c(femind$patient,malind$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Indianapolis\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femind$patient,col="#C53A32",lwd=2)
        lines(1:19,malind$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femind$patient,malind$patient))
        axis(1,at=1:19,lab=femind$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femind$patient==0))==19 && length(which(malind$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Indianapolis_females_",input$drug1,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  ## Text ##
  output$selected_drug2 <- renderText({
    paste("Percentage of patients taking ",input$drug2,sep="")
  })
  ## Plot Blumenau drug2 ##
  output$bludrug2distributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Blumenau_females_",input$drug2,".txt",sep=""),list.files("Files2/")))>0){
      femblu<-fread(paste("Files2/Blumenau_females_",input$drug2,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malblu<-fread(paste("Files2/Blumenau_males_",input$drug2,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femblu$patient==0))<19 || length(which(malblu$patient==0))<19){
        plot(c(1:19,1:19),c(femblu$patient,malblu$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Blumenau\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femblu$patient,col="#C53A32",lwd=2)
        lines(1:19,malblu$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femblu$patient,malblu$patient))
        axis(1,at=1:19,lab=femblu$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femblu$patient==0))==19 && length(which(malblu$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Blumenau_females_",input$drug2,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Blumenau\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  ## Plot Catalonia drug2 ##
  output$catdrug2distributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Catalonia_females_",input$drug2,".txt",sep=""),list.files("Files2/")))>0){
      femcat<-fread(paste("Files2/Catalonia_females_",input$drug2,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malcat<-fread(paste("Files2/Catalonia_males_",input$drug2,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femcat$patient==0))<19 || length(which(malcat$patient==0))<19){
        plot(c(1:19,1:19),c(femcat$patient,malcat$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Catalonia\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femcat$patient,col="#C53A32",lwd=2)
        lines(1:19,malcat$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femcat$patient,malcat$patient))
        axis(1,at=1:19,lab=femcat$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femcat$patient==0))==19 && length(which(malcat$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Catalonia_females_",input$drug2,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Catalonia\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  ## Plot Indianapolis drug2 ##
  output$inddrug2distributions<-renderPlot({
    ## Identify the subtables ##
    if(length(grep(paste("Indianapolis_females_",input$drug2,".txt",sep=""),list.files("Files2/")))>0){
      femind<-fread(paste("Files2/Indianapolis_females_",input$drug2,".txt",sep=""),stringsAsFactors = F,sep="\t")
      malind<-fread(paste("Files2/Indianapolis_males_",input$drug2,".txt",sep=""),stringsAsFactors = F,sep="\t")
      if(length(which(femind$patient==0))<19 || length(which(malind$patient==0))<19){
        plot(c(1:19,1:19),c(femind$patient,malind$patient),col=c(rep("#C53A32",19),rep("#3C76AE",19)),
             pch=c(rep(15,19),rep(15,19)),las=2,main="Indianapolis\nDispensations",cex.main=1.5,
             ylab="Percentage of patients",axes=F,xlab="")
        title(xlab="Age ranges",line=4)
        lines(1:19,femind$patient,col="#C53A32",lwd=2)
        lines(1:19,malind$patient,col="#3C76AE",lwd=2)
        maxim<-max(c(femind$patient,malind$patient))
        axis(1,at=1:19,lab=femind$edades,cex.axis=1.2,las=2)
        if(maxim>1){axis(2,at=round(seq(0,maxim,maxim/20)),cex.axis=0.8,las=2)}
        if(maxim<1){axis(2,at=round(seq(0,maxim,maxim/20),3),cex.axis=0.8,las=2)}
      }
      if(length(which(femind$patient==0))==19 && length(which(malind$patient==0))==19){
        plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDispensations",cex.main=1.5)
        title("No dispensations\ndetected",line=-10)
      }
    }
    if(length(grep(paste("Indianapolis_females_",input$drug2,".txt",sep=""),list.files("Files2/")))==0){
      plot(0,0,type="n",ylab="",axes=F,xlab="",main="Indianapolis\nDispensations",cex.main=1.5)
      title("No dispensations\ndetected",line=-10)
    }
  })
  
  ## Section three ##
  ## @@ @@ @ @@ @@ ##
  ## Documentation
  
  ## Section four ##
  ## @@ @@  @@ @@ ##
  output$jonimage <- renderImage({
    filename <- normalizePath(file.path('./www','jon.png'))
    list(src = filename,align="center",width="90%")
  }, deleteFile = FALSE)
  output$rionimage <- renderImage({
    filename <- normalizePath(file.path('./www','rion.png'))
    list(src = filename,align="center",width="90%")
  }, deleteFile = FALSE)
  output$martaimage <- renderImage({
    filename <- normalizePath(file.path('./www','marta.png'))
    list(src = filename,align="center",width="90%")
  }, deleteFile = FALSE)
  output$albaimage <- renderImage({
    filename <- normalizePath(file.path('./www','alba.png'))
    list(src = filename,align="center",width="90%")
  }, deleteFile = FALSE)
  output$mauroimage<-renderImage({
    filename <- normalizePath(file.path('./www','mauro.png'))
    list(src = filename,align="center",width="90%")
  }, deleteFile = FALSE)
  output$luisimage <- renderImage({
    filename <- normalizePath(file.path('./www','luis.png'))
    list(src = filename,align="center",width="90%")
  }, deleteFile = FALSE)
  output$alfonsoimage <- renderImage({
    filename <- normalizePath(file.path('./www','alfonso.png'))
    list(src = filename,align="center",width="90%")
  }, deleteFile = FALSE)
}
shinyApp(ui = ui, server = server)



# To share the library
# library(rsconnect)
# rsconnect::deployApp('/Users/jonsanchezvalle/Desktop/ddishiny')























