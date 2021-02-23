library(shiny)
library(tidyverse)
library(patchwork)
library(ggthemes)
library(ggrepel)
library(choroplethr)
library(choroplethrMaps)

## Data needed to run:
load(file="MarginData.Rda")
StateFile <- US %>% select(State) %>% distinct() %>% 
    filter(!(State %in% c("Alaska","District of Columbia"))) %>%
    bind_cols(scalea=10000,scaleb=0.025)
StateFile[StateFile$State %in% c("Mississippi","South Dakota","West Virginia","Wyoming"), "scalea"] <- 1000
StateFile[StateFile$State %in% c("Arkansas","Hawaii","Iowa","Louisiana","Montana",
                                 "North Dakota","New Hampshire","Nevada","Rhode Island",
                                 "Vermont"), "scalea"] <- 2500
StateFile[StateFile$State %in% c("Alabama","Delaware","Idaho","Illinois","Kentucky",
                                 "Maine","Nebraska","New Jersey","New Mexico","Oklahoma","South Carolina"), "scalea"] <- 5000
StateFile[StateFile$State %in% c("Arizona","California","Texas","Washington"), "scalea"] <- 20000
StateFile[StateFile$State %in% c("California","Florida"), "scalea"] <- 25000
StateFile[StateFile$State %in% c("Connecticut","Delaware","Hawaii","New Hampshire","Vermont"), "scaleb"] <- 0.01
StateFile[StateFile$State %in% c("Arizona","Arkansas","California","Florida","Georgia","New York",
                                 "Utah"),"scaleb"] <- 0.05
StateFile[StateFile$State %in% c("Texas"), "scaleb"] <- 0.1

## Mapping Function:
StateMaps <- function(state,cities=NULL,scalea=10000,scaleb=0.025,focus.county=NULL) {
    data <- US %>% filter(State==state)
    DF.shift <- data %>% mutate(region=as.numeric(FIPS), value=Shift) %>% select(region,value)
    DF.perc <- data %>% mutate(region=as.numeric(FIPS), value=PercShift) %>% select(region,value)
    if(!is.null(cities)) {
        cit <- cities %>% filter(State==state) %>% mutate(out_val=0)
        if (dim(cit)[1] > 1) {
            for (i in 2:(dim(cit)[1])) {
                for (j in 1:(i-1)) {
                    if (cit[j,"out_val"]==0) {
                        dist <- sqrt((cit[i,"Latitude"]-cit[j,"Latitude"])^2+(cit[i,"Longitude"]-cit[j,"Longitude"])^2)
                        cit[i,"out_val"] <- cit[i,"out_val"] + if_else(dist<0.3,1,0)
                    }
                }
            }
        }
        cit <- cit %>% filter(out_val==0 | Capital==1)
    }
    map.shift <- county_choropleth(df=DF.shift, title="", num_colors=0,
                                   county_zoom=DF.shift$region)+
        labs(title="Change in Vote Margin")+theme(title=element_text(size=14))+
        scale_fill_gradient2(name=NULL,
                             low="#b2182b",mid="#f7f7f7",high="#2166ac",midpoint=0,
                             breaks=seq(-200000,200000,by=scalea),
                             labels=c(paste0("R+",format(seq(200000,scalea,by=-1*scalea),big.mark=",",scientific=FALSE,trim=TRUE)),
                                      "No Change",
                                      paste0("D+",format(seq(scalea,200000,by=scalea),big.mark=",",scientific=FALSE,trim=TRUE))))
    if (!is.null(cities)) {
        map.shift <- map.shift + geom_point(data=cit %>% filter(Capital==1),
                                            aes(x=Longitude,y=Latitude), inherit.aes=FALSE,
                                            size=1.5, color="black", pch=8) +
            geom_point(data=cit %>% filter(Capital==0),
                       aes(x=Longitude,y=Latitude), inherit.aes=FALSE,
                       size=1.5, color="black", pch=16) +
            geom_text_repel(data=cit,
                            aes(x=Longitude,y=Latitude,label=City), inherit.aes=FALSE, size=4, box.padding=.13)
    }
    map.perc <- county_choropleth(df=DF.perc, title="", num_colors=0,
                                  county_zoom=DF.perc$region)+
        labs(title="Change in Percentage Margin")+theme(title=element_text(size=14))+
        scale_fill_gradient2(name=NULL,
                             low="#b2182b",mid="#f7f7f7",high="#2166ac",midpoint=0,
                             breaks=seq(-.5,.5,by=scaleb),
                             labels=c(paste0("R+",format(seq(.5,scaleb,by=-1*scaleb)*100,nsmall=1,digits=1),"%"),
                                      "No Change",
                                      paste0("D+",format(seq(scaleb,.5,by=scaleb)*100,nsmall=1,digits=1),"%")))
    if (!is.null(cities)) {
        map.perc <- map.perc + geom_point(data=cit %>% filter(Capital==1),
                                          aes(x=Longitude,y=Latitude), inherit.aes=FALSE,
                                          size=1.5, color="black", pch=8) +
            geom_point(data=cit %>% filter(Capital==0),
                       aes(x=Longitude,y=Latitude), inherit.aes=FALSE,
                       size=1.5, color="black", pch=16) +
            geom_text_repel(data=cit,
                            aes(x=Longitude,y=Latitude,label=City), inherit.aes=FALSE, size=4, box.padding=.13)
    }
    if (!is.null(focus.county)) {
        if (focus.county != "-None-") {
            data(county.map, package="choroplethrMaps", envir=environment())
            focus.FIPS <- unlist(US %>% filter(State==state & County==focus.county) %>% pull(FIPS))
            outline.df <- county.map[county.map$region==as.numeric(focus.FIPS), ]
            map.shift <- map.shift + geom_polygon(data=outline.df, aes(long, lat, group = group), 
                                                  color = "yellow", fill = NA, size = 1.5)
            map.perc <- map.perc + geom_polygon(data=outline.df, aes(long, lat, group = group), 
                                                color = "yellow", fill = NA, size = 1.5)
        }
    }
    return(list(Shift=map.shift,Perc=map.perc))
}

## County Bar Plotting Functions:
Data_Plotting <- function(data) {
    Plot_data <- data %>% select(D.16,D.20,R.16,R.20,O.16,O.20,D.Diff,R.Diff,O.Diff) %>% 
        pivot_longer(cols=c(D.16,D.20,R.16,R.20,O.16,O.20,D.Diff,R.Diff,O.Diff), names_to="Type") %>%
        mutate(Party=substr(Type,1,1),Year=substr(Type,3,nchar(Type))) %>%
        mutate(Year_Factor=factor(Year, levels=c("16","20","Diff"), labels=c("2016","2020",paste0("Difference, 2020-2016"))),
               Party_Factor=factor(Party, levels=c("D","R","O"), labels=c("D","R","O")))
    Plot_data2 <- Plot_data %>%
        left_join(Plot_data %>% select(Year,value) %>% group_by(Year) %>% summarize(sum=sum(value)) %>% ungroup(),
                  by="Year") %>%
        mutate(perc=value/sum) %>% mutate(PercLabel=format(perc*100, trim=TRUE, digits=1, nsmall=1)) %>%
        mutate(Bar_Label=if_else(Year=="Diff",if_else(value>0,paste0("+",format(value, nsmall=0, big.mark=",", trim=TRUE)),
                                                      format(value, nsmall=0, big.mark=",", trim=TRUE)),
                                 format(value, nsmall=0, big.mark=",", trim=FALSE))) %>%
        mutate(Bar_Label_wP=paste0(Bar_Label,", ",PercLabel,"%"))
    
    Plot_data_labels <- Plot_data2 %>% select(Year_Factor,Party,perc) %>% group_by(Year_Factor) %>% 
        pivot_wider(id_cols=Year_Factor,names_from=Party,values_from=perc) %>% 
        left_join(Plot_data2 %>% group_by(Year_Factor) %>% summarize(sum=sum(value))) %>%
        mutate(Margin=D-R) %>%
        mutate(MargLabel=if_else(Margin>0,paste0("D+",format(abs(Margin)*100, nsmall=1, digits=1),"%"),
                                 paste0("R+",format(abs(Margin)*100, nsmall=1, digits=1),"%")),
               MargWin=if_else(Margin>0,"D","R"),
               Max=if_else(Margin>0,D*sum,R*sum)) %>% select(Year_Factor,Margin,Max,MargLabel,MargWin,sum)
    
    Plot_data2 <- Plot_data2 %>% left_join(Plot_data_labels %>% select(Year_Factor,Max,MargLabel,MargWin) %>% 
                                               bind_cols(Party_Factor=factor("R", levels=c("D","R","O"), labels=c("D","R","O"))))
    return(list(a=Plot_data2, b=Plot_data_labels))
}

Plotting <- function(data, ylim, scale_amt, buffer, labsize=NULL) {
    Datasets <- Data_Plotting(data)
    plot <- ggplot(Datasets$a, mapping=aes(x=Year_Factor, y=value/scale_amt, fill=Party_Factor, label=Bar_Label)) +
        geom_col(stat="identity", position="dodge", width=.8) +
        scale_fill_manual(name="Party:",
                          labels=c("D"="Democratic","R"="Republican","O"="Other"),
                          values=c("O"="yellow","R"="red","D"="blue"),
                          breaks=c("D","R","O")) +
        scale_y_continuous(limits=ylim, expand=expansion(add=c(0,0))) +
        labs(title=paste0(data$County,if_else(data$State=="Louisiana"," Parish, "," County, "),data$State),
             x=NULL, y=paste0("Votes (",format(scale_amt, digits=0, nsmall=0, big.mark=","),"s)")) +
        geom_text(vjust=-.1, stat="identity", position=position_dodge(width=.8), size=3.5) +
        geom_label(mapping=aes(x=Year_Factor, y=Max/scale_amt+buffer, label=MargLabel, color=MargWin),
                   inherit.aes=FALSE, size=4) +
        scale_color_manual(name=NULL,
                           labels=c("D","R"),
                           values=c("blue","red")) +
        guides(color=FALSE) +
        theme_classic() +
        theme(legend.position="bottom", axis.line.x=element_blank(), axis.ticks.x=element_blank(),
              axis.text.x=element_text(size=12), title=element_text(size=14))
    plot
}

# UI:
ui <- fluidPage(

    # Application title
    titlePanel("County-Level Presidential Vote Changes, 2016-2020"),
    sidebarLayout(
        sidebarPanel(
            selectInput("InState", label = h3("Choose State"), 
                        choices = as.list(StateFile$State), 
                        selected = "Pennsylvania"),
            
            uiOutput("CountySel"),
            h4("Created by Lee Kennedy-Shaffer, 2021"),
            h5("Code Available:"),
            h6("https://github.com/leekshaffer/pres-vote-margins"),
            h5("Data Sources:"),
            h6("MIT Election Lab, https://electionlab.mit.edu/data;"),
            h6("New York Times, https://www.nytimes.com/interactive/2020/ 11/03/us/elections/results-president.html;"),
            h6("Github, https://github.com/tonmcg/"),
            h5("Mapping Interface:"),
            h6("R choroplethr package, https://arilamstein.com/open-source/")
        ),
        mainPanel(
           plotOutput("Map1", width="90%"),
           plotOutput("Map2", width="90%"),
           plotOutput("Bar", width="80%")
        )
    )
)

# Server logic:
server <- function(input, output) {
    output$Map1 <- renderPlot({
        res <- StateMaps(state=input$InState, cities=Cities, 
                         scalea=unlist(StateFile[StateFile$State==input$InState,"scalea"]),
                         scaleb=unlist(StateFile[StateFile$State==input$InState,"scaleb"]),
                         focus.county=input$InCounty)
        res$Shift
        })
    output$Map2 <- renderPlot({
        res <- StateMaps(state=input$InState, cities=Cities, 
                         scalea=unlist(StateFile[StateFile$State==input$InState,"scalea"]),
                         scaleb=unlist(StateFile[StateFile$State==input$InState,"scaleb"]),
                         focus.county=input$InCounty)
        res$Perc
        })
    output$CountySel <- renderUI({
        if(is.null(input$InState)) {
            return()
        }
        selectInput("InCounty", label = h3(if_else(input$InState=="Louisiana","Choose Focus Parish","Choose Focus County")), 
                    choices = as.list(c("-None-",as.vector(US %>% filter(State==input$InState) %>% pull(County)))))
    })
    output$Bar <- renderPlot({
        if(is.null(input$InState)) {
            return()
        }
        if (is.null(input$InCounty)) {
            return()
        }
        if (input$InCounty=="-None-") {
            return()
        } else {
            county.data <- US %>% filter(State==input$InState & County==input$InCounty) %>%
                mutate(D.16=D.2016, R.16=R.2016, O.16=O.2016, D.20=D.2020, R.20=R.2020, O.20=O.2020,
                       D.Diff=D.2020-D.2016, R.Diff=R.2020-R.2016, O.Diff=O.2020-O.2016)
            ylimits <- c(min(c(as.numeric(county.data %>% select(D.16,R.16,O.16,D.20,R.20,O.20,D.Diff,R.Diff,O.Diff)),0)),
                         max(county.data %>% select(D.16,R.16,O.16,D.20,R.20,O.20,D.Diff,R.Diff,O.Diff)))*1.2/1000
            Plotting(data=county.data, ylim=ylimits,
                     scale_amt=1000, buffer=(ylimits[2]-ylimits[1])*.08)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
