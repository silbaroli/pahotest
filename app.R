library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(reshape2)
library(htmltools)
library(stringr)
library(ggspatial)
library(sf)



options(spinner.color = "grey", spinner.color.background = "#ffffff", spinner.size = 2, shiny.reactlog=TRUE)

ui <- dashboardPage(skin = "blue",
        dashboardHeader(title = "COVID-19 situation"),
        dashboardSidebar(
          sliderInput("date","Year",min = 2019,max=year(Sys.Date()),value = c(2020,2023)),
          hr(),
          selectInput("sex","Sex",choices = c("Male"="M","Female"="F","Missing"="I"),
                      multiple = T,selected = c("M","F","I")),
          hr(),
          selectInput("age","Age group",choices=c("<10y"="1","10-19y"="2","20-29y"="3","30-39y"="4",
                                                  "40-49y"="5","50-59y"="6","60y+"="7","Missing"="9"),
                      multiple = T,selected = c("1","2","3","4","5","6","7","9"))

        ),
        dashboardBody(
          fluidRow(
            h2("COVID-19: Epidemiological situation"),
            hr(),
            valueBoxOutput("top1", width = 3),
            valueBoxOutput("top2", width = 3),
            valueBoxOutput("top3", width = 3),
            valueBoxOutput("top4", width = 3),
            hr(),
            box(width = 6,
              tabsetPanel(
                tabPanel("Epicurve",
                  h4(htmlOutput("title_plot1", align = "center")),
                  withSpinner(plotOutput("plot1", width = 'auto', height = 600), type = 2)
                ),
                tabPanel("Sex",
                  h4(htmlOutput("title_plot2", align = "center")),
                  withSpinner(plotOutput("plot2", width = 'auto', height = 600), type = 2)
                ),
                tabPanel("Age group",
                  h4(htmlOutput("title_plot3", align = "center")),
                  withSpinner(plotOutput("plot3", width = 'auto', height = 600), type = 2)
                ),
                tabPanel("Symptoms",
                  h4(htmlOutput("title_plot4", align = "center")),
                  withSpinner(plotOutput("plot4", width = 'auto', height = 600), type = 2)
                ),
                tabPanel("Clinical",
                         h4(htmlOutput("title_plot5", align = "center")),
                         withSpinner(plotOutput("plot5", width = 'auto', height = 600), type = 2)
                )
              )
            ),
            box(width = 6,
              h4(htmlOutput("title_plot6", align = "center")),
              withSpinner(plotOutput("plot6", width = 'auto', height = 640), type = 2)
            )
          )
      )
)



server <- function(input, output, session) {

  mun <- read_sf("shapefile/MA_Municipios_2022.shp")

  data <- reactive({
    db=readxl::read_excel("data/data.xlsx",sheet = "srag_20_22_test") %>%
      mutate(epiweek=epiweek(as.Date(dt_sin_pri,format="%d/%m/%Y"))) %>%
      mutate(count=ifelse(as.numeric(criterio)==1,1,0)) %>%
      mutate(male=ifelse(count==1 & cs_sexo=="M",1,0)) %>%
      mutate(female=ifelse(count==1 & cs_sexo=="F",1,0)) %>%
      mutate(risk=ifelse(count==1 & as.numeric(risc_factor)==1,1,0)) %>%
      mutate(age=as.numeric(nu_idade_n)) %>%
      mutate(agegroup=case_when(age>=0 & age<=9 ~ 1,
                                age>=10 & age<=19 ~ 2,
                                age>=20 & age<=29 ~ 3,
                                age>=30 & age<=39 ~ 4,
                                age>=40 & age<=49 ~ 5,
                                age>=50 & age<=59 ~ 6,
                                age>=60 ~ 7,
                                TRUE ~ 9)) %>%
      mutate(fever=ifelse(count==1 & as.numeric(febre)==1,1,0),
             cough=ifelse(count==1 & as.numeric(tosse)==1,1,0),
             `sore throat` = ifelse(count==1 & as.numeric(garganta)==1,1,0),
             dyspnea = ifelse(count==1 & as.numeric(dispneia)==1,1,0),
             `respiratory distress` = ifelse(count==1 & as.numeric(desc_resp)==1,1,0),
             saturation = ifelse(count==1 & as.numeric(saturacao)==1,1,0)) %>%
      mutate(cardiopathy= ifelse(count==1 & as.numeric(cardiopati)==1,1,0),
             obesity = ifelse(count==1 & as.numeric(obesidade)==1,1,0),
             hospitalized = ifelse(count==1 & as.numeric(hospital)==1,1,0),
             `evolution to death` = ifelse(count==1 & as.numeric(evolucao)==1,1,0)) %>%
      filter(year(as.Date(dt_sin_pri,format="%d/%m/%Y"))>=min(input$date) & year(as.Date(dt_sin_pri,format="%d/%m/%Y"))<=max(input$date)) %>%
      filter(cs_sexo %in% input$sex) %>%
      filter(agegroup %in% input$age)
  })

  output$top1 <- renderValueBox({

    valueBox(
      prettyNum(sum(data()$count,na.rm=T),
        big.mark = ",",decimal.mark = ".",scientific = FALSE),
      "Confirmed cases",
      color = "blue",
      icon = icon("disease")
    )
  })

  output$top2 <- renderValueBox({

    valueBox(
      paste0(
        prettyNum(
          sum(data()$male,na.rm = T),
          big.mark = ",",decimal.mark = ".",scientific = FALSE),
        " (",
        round(sum(data()$male,na.rm=T)/sum(data()$count,na.rm = T)*100,1),
        "%)"
      ),
      "Male",
      color = "light-blue",
      icon = icon("cross")
    )
  })

  output$top3 <- renderValueBox({

    valueBox(
      paste0(
        prettyNum(
          sum(data()$female,na.rm = T),
          big.mark = ",",decimal.mark = ".",scientific = FALSE),
        " (",
        round(sum(data()$female,na.rm=T)/sum(data()$count,na.rm = T)*100,1),
        "%)"
      ),
      "Female",
      color = "purple",
      icon = icon("envelope")
    )
  })

  output$top4 <- renderValueBox({

    valueBox(
      paste0(
        prettyNum(
          sum(data()$risk,na.rm = T),
          big.mark = ",",decimal.mark = ".",scientific = FALSE),
        " (",
        round(sum(data()$risk,na.rm=T)/sum(data()$count,na.rm = T)*100,1),
        "%)"
      ),
      "Risk factor",
      color = "purple",
      icon = icon("envelope")
    )
  })

  output$title_plot1 <- renderUI({
    HTML("Number of COVID-19 confirmed cases by epidemiological week. Maranhão-Brazil, 2019 to 2022.")
  })

  output$title_plot2 <- renderUI({
    HTML("Number of COVID-19 confirmed cases by sex and epidemiological week. Maranhão-Brazil, 2019 to 2022.")
  })

  output$title_plot3 <- renderUI({
    HTML("Number of COVID-19 confirmed cases by age group. Maranhão-Brazil, 2019 to 2022.")
  })

  output$title_plot4 <- renderUI({
    HTML("Number of COVID-19 confirmed cases by sypmtoms. Maranhão-Brazil, 2019 to 2022.")
  })

  output$title_plot5 <- renderUI({
    HTML("Number of COVID-19 confirmed cases by clinical characteristic. Maranhão-Brazil, 2019 to 2022.")
  })

  output$title_plot6 <- renderUI({
    HTML("Number of COVID-19 confirmed cases by municipalities. Maranhão-Brazil, 2019 to 2022.")
  })


  output$plot1 <- renderPlot({
    df=data() %>%
      group_by(sem_pri=as.numeric(sem_pri)) %>%
      summarise(count=sum(count,na.rm = T))

    ggplot(df,aes(x=sem_pri,y=count))+
      geom_bar(stat="identity",fill="#0099DA")+
      geom_text(aes(label = count), vjust = 0.5,hjust=1.2, colour = "black",size=3.5,angle=90)+
      theme_classic()+
      theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            legend.text=element_text(size = 12),
            text=element_text(size = 12))+
      labs(x="Epidemiological week",y="Number of cases")+
      scale_x_continuous(breaks = seq(1,52,2),expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))
  })

  output$plot2 <- renderPlot({
    df=data() %>%
      group_by(epiweek,cs_sexo) %>%
      summarise(count=sum(count,na.rm=T)) %>%
      mutate(cs_sexo=factor(cs_sexo,levels=c("M","F","I")))

    ggplot(df,aes(x=epiweek,y=count,fill=cs_sexo))+
      geom_bar(stat="identity",position=position_fill(reverse = T))+
      theme_classic()+
      theme(legend.position = "bottom")+
      theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            legend.text=element_text(size = 12),
            text=element_text(size = 12))+
      scale_fill_brewer(palette = "Set2",labels=c("M"="Male","F"="Female","I"="Missing"))+
      labs(x="Epidemiological week",y="Number of cases",fill="")+
      scale_x_continuous(breaks = seq(1,52,3),expand=c(0,0))+
      scale_y_continuous(labels = scales::percent,expand=c(0,0))
  })

  output$plot3 <- renderPlot({
    df=data() %>%
      group_by(agegroup) %>%
      summarise(count=sum(count,na.rm=T)) %>%
      mutate(agegroup=factor(agegroup,levels=c(1:7,9),labels=c("<10y","10-19y","20-29y","30-39y",
                                                               "40-49y","50-59y","60y+","Missing"))) %>%
      mutate(per=round(count/sum(count)*100,0))

    ggplot(df,aes(x=agegroup,y=count))+
      geom_bar(stat="identity",fill="#0099DA")+
      geom_text(aes(label = paste0(count,"\n (",per,"%)")), vjust = 0, colour = "black")+
      theme_classic()+
      theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            legend.text=element_text(size = 12),
            text=element_text(size = 12))+
      labs(x="Age group",y="Number of cases")+
      scale_x_discrete(expand = c(0,0))
  })

  output$plot4 <- renderPlot({
    df=db %>%
      summarize(total=sum(count,na.rm=T),
                fever=sum(fever,na.rm=T),
                cough=sum(cough,na.rm=T),
                `sore throat`=sum(`sore throat`,na.rm=T),
                dyspnea=sum(dyspnea,na.rm=T),
                `respiratory distress`=sum(`respiratory distress`,na.rm=T),
                saturation=sum(saturation,na.rm=T))

    df=reshape2::melt(df) %>%
      mutate(per=round(value/max(value)*100,0)) %>%
      filter(variable!="total") %>%
      arrange(desc(value))

    ggplot(df,aes(x=reorder(variable,-value),y=value))+
      geom_bar(stat="identity",fill="#0099DA")+
      geom_text(aes(label = paste0(value,"\n (",per,"%)")), vjust = 1.1, colour = "black")+
      theme_classic()+
      theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            legend.text=element_text(size = 12),
            text=element_text(size = 12))+
      labs(x="Symptoms",y="Number of cases")+
      scale_x_discrete(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))
  })

  output$plot5 <- renderPlot({
    df=data() %>%
      summarise(total=sum(count,na.rm=T),
                cardiopathy=sum(cardiopathy,na.rm=T),
                obesity=sum(obesity,na.rm=T),
                hospitalized=sum(hospitalized,na.rm=T),
                `evolution to death`=sum(`evolution to death`,na.rm=T))

    df=reshape2::melt(df) %>%
      mutate(per=round(value/max(value)*100,1)) %>%
      filter(variable!="total")

    ggplot(df,aes(x=reorder(variable,-value),y=value))+
      geom_bar(stat="identity",fill="#0099DA")+
      geom_text(aes(label = paste0(value," (",per,"%)")), vjust = 1.2, colour = "black")+
      theme_classic()+
      theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),
            axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            legend.text=element_text(size = 12),
            text=element_text(size = 12))+
      labs(x="Clinical characteristics",y="Number of cases")+
      scale_x_discrete(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))

  })

  output$plot6 <- renderPlot({

    df=mun %>%
      mutate(CD_MUN=substr(CD_MUN,1,6)) %>%
      left_join(data() %>%
                  group_by(co_mun_not) %>%
                  summarise(count=sum(count,na.rm=T)),
                by=c("CD_MUN"="co_mun_not")) %>%
      mutate(count=ifelse(is.na(count),0,count)) %>%
      mutate(cat=case_when(count==0 ~ 1,
                           count>=1 & count<=10 ~ 2,
                           count>=11 & count<=50 ~ 3,
                           count>=51 & count<=300 ~ 4,
                           count>=301 ~ 5,
                           TRUE ~ 9)) %>%
      mutate(cat=factor(cat,levels=c(1:5),labels=c("No cases","1 to 10","11 to 50","51 to 300","301 or more")))

    ggplot()+
      geom_sf(data = df,aes(fill = factor(cat),geometry = geometry),color = '#969696',size = .2)+
      scale_fill_manual("",values=c("No cases"="white","1 to 10"="#6FC06C","11 to 50"="#F3B272","51 to 300"="#F3758E","301 or more"="#B09CCA"))+
      labs(fill="Number of cases")+
      theme_void()+
      theme(legend.position = "right")+
      annotation_north_arrow(style=north_arrow_nautical())+
      annotation_scale(pad_x = unit(5, "cm"),
                       pad_y = unit(1, "cm"))
  })

}


shinyApp(ui = ui, server = server)
