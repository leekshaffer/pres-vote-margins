### Load Required Packages: ###
require(tidyverse)
require(patchwork)
require(readxl)
require(ggthemes)
require(ggrepel)
require(choroplethr)
require(choroplethrMaps)

### Folder Structure: ###
plot_wd <- paste0(getwd(),"/Plots/")
data_wd <- paste0(getwd(),"/Data/")

### Read in Supporting Data Sets: ###
StateAbbrs <- read_csv(file=paste0(data_wd,"StateAbbrs.csv"))
Cities <- read_xlsx(path=paste0(data_wd,"Cities.xlsx"), sheet="Res")

### Read in US Data: ###
### 2000-2016 data from https://doi.org/10.7910/DVN/VOQCHQ ###
US0016 <- read_csv(file=paste0(data_wd,"countypres_2000-2016.csv")) %>% 
  select(year,state,state_po,FIPS,party,candidatevotes,totalvotes) %>%
  filter(!is.na(FIPS) & !is.na(state_po)) %>%
  mutate(Cand=if_else(is.na(party),"W",
                      if_else(party=="democrat","D",if_else(party=="republican","R","G"))),
         FIPS=formatC(FIPS, width=5, format="d", flag="0")) %>%
  select(!party) %>% rename(State=state, StateA=state_po, Total=totalvotes) %>%
  pivot_wider(id_cols=c(year,State,StateA,FIPS,Total),
              names_from=Cand,
              values_from=candidatevotes) %>% select(!c(G,W))
US0016$FIPS[US0016$FIPS=="46113"] <- "46102" ## Fix Oglala Lakota County, SD FIPS code
AKFix <- US0016 %>% filter(StateA=="AK") %>% 
  rename(FIPS2=FIPS) %>% mutate(FIPS=paste0("029",substr(FIPS2,4,5))) %>% select(!FIPS2) ## Fix Alaska FIPS codes
MOFix <- US0016 %>% filter(FIPS %in% c("29095","36000")) %>% group_by(year,State,StateA) %>%
  summarize(Total=sum(Total, na.rm=TRUE), D=sum(D, na.rm=TRUE), R=sum(R, na.rm=TRUE)) %>%
  mutate(FIPS="29095") %>% ungroup() ## Combine Kansas City, MO & Jackson County, MO results
US0016 <- US0016 %>% filter(!(FIPS %in% c("29095","36000","51515")) & StateA != "AK") %>% ## Also drops FIPS 51515 in VA which has no votes
  bind_rows(AKFix) %>%
  bind_rows(MOFix) %>% 
  mutate(O=Total-D-R,
         Margin=D-R, DPerc=D/Total, RPerc=R/Total,
         PercMargin=DPerc-RPerc)

### 2020 Data from https://github.com/tonmcg/US_County_Level_Election_Results_08-20 ###
US2020 <- read_csv(file=paste0(data_wd,"2020_US_County_Level_Presidential_Results.csv")) %>%
  left_join(StateAbbrs) %>%
  mutate(FIPS=formatC(county_fips, width=5, format="d", flag="0")) %>%
  select(state_name, state_abbr, FIPS,
         votes_dem, votes_gop, total_votes) %>%
  rename(State=state_name, StateA=state_abbr,
         D=votes_dem, R=votes_gop, Total=total_votes) %>%
  mutate(O=Total-D-R, Margin=D-R, DPerc=D/Total, RPerc=R/Total, PercMargin=DPerc-RPerc)

### Combine 2016 and 2020 Data: ###
US2016 <- US0016 %>% filter(year==2016) %>% select(!year)
US <- full_join(US2016, US2020, 
                by=c("State","StateA","FIPS"), suffix=c(".2016",".2020"))
US$Shift <- US$Margin.2020 - US$Margin.2016
US$PercShift <- US$PercMargin.2020 - US$PercMargin.2016


### Function for Mapping: ###
StateMaps <- function(state,outfile=NULL,cities=NULL,scalea=10000,scaleb=0.025,hjust=0.5,vjust=1.5) {
  data <- US %>% filter(State==state)
  DF.shift <- data %>% mutate(region=as.numeric(FIPS), value=Shift) %>% select(region,value)
  DF.perc <- data %>% mutate(region=as.numeric(FIPS), value=PercShift) %>% select(region,value)
  if(!is.null(cities)) {
    cit <- cities %>% filter(State==state) %>% mutate(out_val=0)
    for (i in 2:(dim(cit)[1])) {
      for (j in 1:(i-1)) {
        if (cit[j,"out_val"]==0) {
          dist <- sqrt((cit[i,"Latitude"]-cit[j,"Latitude"])^2+(cit[i,"Longitude"]-cit[j,"Longitude"])^2)
          cit[i,"out_val"] <- cit[i,"out_val"] + if_else(dist<0.3,1,0)
        }
      }
    }
    cit <- cit %>% filter(out_val==0)
  }
  map.shift <- county_choropleth(df=DF.shift, title="", num_colors=0,
                    county_zoom=DF.shift$region)+
    labs(title="A) Change in Vote Margin")+theme(title=element_text(size=10))+
    scale_fill_gradient2(name=NULL,
                         low="#b2182b",mid="#f7f7f7",high="#2166ac",midpoint=0,
                         breaks=seq(-100000,100000,by=scalea),
                         labels=c(paste0("R+",format(seq(100000,scalea,by=-1*scalea),big.mark=",",scientific=FALSE,trim=TRUE)),
                                  "No Change",
                                  paste0("D+",format(seq(scalea,100000,by=scalea),big.mark=",",scientific=FALSE,trim=TRUE))))
  if (!is.null(cities)) {
    map.shift <- map.shift + geom_point(data=cit,
                                        aes(x=Longitude,y=Latitude), inherit.aes=FALSE,
                                        size=1, color="black") +
      geom_text_repel(data=cit,
                aes(x=Longitude,y=Latitude,label=City), inherit.aes=FALSE, size=2.8, box.padding=.13) #,
                # color="black", hjust=hjust, vjust=vjust, position="dodge")
  }
  map.perc <- county_choropleth(df=DF.perc, title="", num_colors=0,
                    county_zoom=DF.perc$region)+
    labs(title="B) Change in Percentage Margin")+theme(title=element_text(size=10))+
    scale_fill_gradient2(name=NULL,
                         low="#b2182b",mid="#f7f7f7",high="#2166ac",midpoint=0,
                         breaks=seq(-.2,.2,by=scaleb),
                         labels=c(paste0("R+",format(seq(.2,scaleb,by=-1*scaleb)*100,nsmall=1,digits=1),"%"),
                                  "No Change",
                                  paste0("D+",format(seq(scaleb,.2,by=scaleb)*100,nsmall=1,digits=1),"%")))
  if (!is.null(cities)) {
    map.perc <- map.perc + geom_point(data=cit,
                                        aes(x=Longitude,y=Latitude), inherit.aes=FALSE,
                                        size=1, color="black") +
      geom_text_repel(data=cit,
                 aes(x=Longitude,y=Latitude,label=City), inherit.aes=FALSE, size=2.8, box.padding=.13) #,
                 # color="black",hjust=hjust,vjust=vjust,size=3, position="dodge")
  }
  if(!is.null(outfile)) {
    ggsave(filename=paste0(plot_wd,outfile,".png"),
          plot=map.shift+map.perc,
          width=8,height=4, units="in", dpi=300)
  }
  return(list(Shift=map.shift,Perc=map.perc))
}

### Creation of Maps: ###
StateMaps("Michigan",outfile="Maps_MI",
          cities=Cities,
          scalea=10000,scaleb=0.025)
StateMaps("Wisconsin",outfile="Maps_WI", 
          cities=Cities,
          scalea=10000,scaleb=0.025)
StateMaps(state="Pennsylvania",outfile="Maps_PA",
          cities=Cities,
          scalea=10000,scaleb=0.025)
StateMaps(state="Arizona",outfile="Maps_AZ",
          cities=Cities,
          scalea=20000,scaleb=0.05)
StateMaps(state="Georgia",outfile="Maps_GA",
          cities=Cities,
          scalea=10000,scaleb=0.05)
StateMaps(state="New York",outfile="Maps_NY",
          cities=Cities,
          scalea=10000,scaleb=0.05)

Fig2AB <- StateMaps("Michigan",outfile=NULL,
                    cities=Cities,
                    scalea=10000,scaleb=0.025,
                    hjust=1.1, vjust=0.5)
Fig2CD <- StateMaps(state="Arizona",outfile=NULL,
                    cities=Cities,
                    scalea=20000,scaleb=0.05,
                    hjust=1.1, vjust=0.5)
ggsave(filename=paste0(plot_wd,"Fig2.eps"),
       plot=Fig2AB$Shift+labs(subtitle="(a)", title="Change in Vote Margin")+theme(plot.title=element_text(hjust=1))+
         Fig2AB$Perc+labs(subtitle="(b)", title="Change in Percentage Margin")+theme(plot.title=element_text(hjust=1))+
         Fig2CD$Shift+labs(subtitle="(c)", title=NULL)+
         Fig2CD$Perc+labs(subtitle="(d)", title=NULL),
       width=7, height=7, units="in")

### Function for Combining 2016 and 2020 Data: ###
Summaries <- function(data) {
  data %>% mutate(Margin.16=D.16-R.16,
                  DPerc.16=D.16/Total.16,
                  RPerc.16=R.16/Total.16,
                  MarginPerc.16=DPerc.16-RPerc.16,
                  Margin.20=D.20-R.20,
                  DPerc.20=D.20/Total.20,
                  RPerc.20=R.20/Total.20,
                  MarginPerc.20=DPerc.20-RPerc.20,
                  VoteShift=Margin.20-Margin.16,
                  PercShift=MarginPerc.20-MarginPerc.16,
                  DRVoteShift=D.20+R.20-D.16-R.16,
                  T.Diff=Total.20-Total.16,
                  D.Diff=D.20-D.16,
                  R.Diff=R.20-R.16,
                  O.Diff=O.20-O.16)
}

### Read In New York State Data: ###
### Excel files from https://www.elections.ny.gov/2020ElectionResults.html with some manual pre-processing###
NYS2016 <- read_xls(path=paste0(data_wd,"2016President_ed.xls"))
NYS2016$D <- NYS2016$DEM + NYS2016$WOR + NYS2016$WEP
NYS2016$R <- NYS2016$REP + NYS2016$CON
NYS2016$O <- NYS2016$Total-NYS2016$D-NYS2016$R

NYS2020 <- read_xlsx(path=paste0(data_wd,"2020President_ed.xlsx")) %>% rename(County_Full=County)
NYS2020$County <- substr(NYS2020$County_Full, 1, unlist(gregexpr("County", NYS2020$County_Full))-2)
NYS2020$D <- NYS2020$DEM + NYS2020$WOR
NYS2020$R <- NYS2020$REP + NYS2020$CON
NYS2020$O <- NYS2020$Total-NYS2020$D-NYS2020$R

NYS <- left_join(NYS2016 %>% select(County,D,R,O,Total), 
                 NYS2020 %>% select(County,D,R,O,Total),
                 by="County", suffix=c(".16",".20"))
NYS <- Summaries(NYS)

### Read In Detroit City Data: ###
### PDF files from https://detroitmi.gov/document/november-8-2016-official-general-election-results ###
Detroit <- tibble(city="Detroit",
                  D.16=234871, R.16=7682, Total.16=248780,
                  D.20=240936, R.20=12889, Total.20=257619)
Detroit$O.16 <- Detroit$Total.16 - Detroit$D.16 - Detroit$R.16
Detroit$O.20 <- Detroit$Total.20 - Detroit$D.20 - Detroit$R.20
Detroit <- Summaries(Detroit)

### Bar Plotting Functions: ###
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
    mutate(Val_Label=if_else(Year=="Diff",if_else(value>0,paste0("+",format(value, nsmall=0, big.mark=",", trim=TRUE)),
                                                  format(value, nsmall=0, big.mark=",", trim=TRUE)),
                             format(value, nsmall=0, big.mark=",", trim=FALSE))) %>%
    mutate(Val_Label2=paste0(Val_Label,", ",PercLabel,"%")) %>%
    mutate(Bar_Label=Val_Label, Bar_Label_wP=Val_Label2)
    # mutate(Bar_Label=if_else(Party=="O","",Val_Label), Bar_Label_wP=if_else(Party=="O","",Val_Label2))
  
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

Plotting <- function(data, ylim, scale_amt, buffer, outfile, labsize=NULL) {
  Datasets <- Data_Plotting(data)
  plot <- ggplot(Datasets$a, mapping=aes(x=Year_Factor, y=value/scale_amt, fill=Party_Factor, label=Bar_Label)) +
    geom_col(stat="identity", position="dodge", width=.8) +
    scale_fill_manual(name="Party:",
                      labels=c("D"="Democratic","R"="Republican","O"="Other"),
                      values=c("O"="yellow","R"="red","D"="blue"),
                      breaks=c("D","R","O")) +
    scale_y_continuous(limits=ylim, expand=expansion(add=c(0,0))) +
    labs(title=NULL,
         x=NULL, y=paste0("Votes (",format(scale_amt, digits=0, nsmall=0, big.mark=","),"s)")) +
    geom_text(vjust=-.1, stat="identity", position=position_dodge(width=.8), size=2.6) +
    geom_label(mapping=aes(x=Year_Factor, y=Max/scale_amt+buffer, label=MargLabel, color=MargWin),
               inherit.aes=FALSE, size=3) +
    scale_color_manual(name=NULL,
                       labels=c("D","R"),
                       values=c("blue","red")) +
    guides(color=FALSE) +
    theme_classic() +
    theme(legend.position="bottom", axis.line.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.x=element_text(size=10))
  # if (is.null(labsize)) {
  #   plot <- plot + geom_text(position=position_stack(vjust=0.5), color="black")
  # } else {
  #   plot <- plot + geom_text(data=Datasets$a %>% filter(Year != "Diff"),
  #                            position=position_stack(vjust=0.5), color="black") +
  #     geom_text(data=Datasets$a %>% filter(Year == "Diff"),
  #               position=position_stack(vjust=0.5), color="black", size=labsize)
  # }
  
  ggsave(filename=paste0(plot_wd,outfile,".eps"), plot=plot,
         width=5, height=5, units="in")
  plot
}

### Bar Plot of Brooklyn Data: ###
NYS %>% filter(County=="Kings") %>% select(County, Total.20, VoteShift, PercShift, DRVoteShift)
Plotting(data=NYS %>% filter(County=="Kings"), ylim=c(-20,800), scale_amt=1000, buffer=70, outfile="Bar_Brooklyn")

### Bar Plot of Detroit Data: ###
Detroit %>% select(Total.20, VoteShift, PercShift, DRVoteShift)
Plotting(data=Detroit, ylim=c(-10,300), scale_amt=1000, buffer=20, outfile="Bar_Detroit", labsize=2.3)




