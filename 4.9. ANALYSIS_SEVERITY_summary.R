#' @title summary figures of severity
#' @description script to analyze the four dimension of severity
#' it sums up probability, intensity, persistence and complexity
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder Probability

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")


# summary table with all dimensions


get_severity_typology=function(mydata,ctries,shocks,path=NULL){
  myctries=mydata$ISO3_Code %>% unique()
  severity=severity_summary_table(mydata,myctries,shocks)
  
  severity_all=severity %>% filter(ISO3_Code %in% ctries)
  
  severity_all=severity_all %>% ungroup() %>% group_by(shocks) %>% 
    summarise_at(vars(probability:area),mean) %>% ungroup()%>%
    mutate(probability=round(probability,2),
           priority=round(priority,2),
           persistence=round(persistence,2),
           complexity=round(complexity,2),
           probability_rank=round(probability_rank,2),
           priority_rank=round(priority_rank,2),
           persistence_rank=round(persistence_rank,2),
           complexity_rank=round(complexity_rank,2),
           area=round(area,2)) %>% arrange(shocks) %>% rename(crisis=shocks) %>%
    dplyr::select(-c(probability_rank,priority_rank,persistence_rank,complexity_rank,area))
  
  stargazer::stargazer(title="Severity:"
                       , severity_all
                       , type="latex"
                       , digits=2
                       , no.space=T
                       , align=T
                       , summary=F
                       , rownames=F
                       , table.placement = "H"
                       , column.sep.width="3pt"
                       , font.size = "footnotesize"
                       , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary.tex")
  )
  
  footnote=c("The table display the average cross countries for all severity dimensions for high income 
             countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
             allocated to each crisis, persistence the average duration and complexity the average correlation.")
  
  cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_footnote.tex"))
  
  
  priority_table=get_priority_table(mydata,shocks)
  priority_table=priority_table %>% dplyr::select(crisis=shock,p.95)
  
  severity_all=severity_all %>% left_join(priority_table,by="crisis")
  #severity_all=severity_all%>% filter(crisis!="Sovereign")
  
  severity_all$typology=c("Paralyzers","Hotspots","Paralyzers","Spreaders","Paralyzers","Paralyzers","Spreaders","Sparks","Spreaders","Spreaders","Sparks",
                          "Spreaders","Sparks","Hotspots","Spreaders","Paralyzers","Hotspots","Sparks","Sparks","Spreaders")
  
  panel_A=ggplot(data=severity_all)+
    geom_smooth(method="lm",aes(x=log(priority*100+1),y=probability),se=F,color="darkgrey",size=0.3)+
    geom_text_repel(aes(x=log(priority*100+1),y=probability,label=crisis,color=typology))+
    theme_bw()+
    scale_color_manual(values=c("darkred","darkgreen","orange","darkblue"))+
    #scale_x_log10()+
    labs(y="probability",
         x="priority",
         title=NULL)+
    #lims(y=c(1,5))+
    lims(x=c(0,4))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="bottom",
          legend.text = element_text(size =13),
          legend.title = element_blank())
  
  #  ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/priority_probability.png"))
  
  panel_B= ggplot(data=severity_all)+
    #geom_point(aes(x=probability,y=p.95))+
    geom_smooth(method="lm",aes(x=log(p.95),y=probability),se=F,color="darkgrey",size=0.3)+
    geom_text_repel(aes(x=log(p.95),y=probability,label=crisis,color=typology))+
    theme_bw()+
    scale_color_manual(values=c("darkblue","darkgreen","darkred","orange"))+
    labs(y="Probability",
         x="log P.95 priority",
         title=NULL)+
    #lims(y=c(1,5))+
    #lims(x=c(0,0.65))+
    #scale_color_grey()+ 
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="bottom",
          legend.text = element_text(size =13),
          legend.title = element_blank())
   # ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p.95_probability.png"))
  
  
  panel_C= ggplot(data=severity_all)+
    #geom_point(aes(x=probability,y=p.95))+
    geom_smooth(method="lm",aes(x=log(p.95),y=complexity),se=F,color="darkgrey",size=0.3)+
    geom_text_repel(aes(x=log(p.95),y=complexity,label=crisis,color=typology))+
    #geom_hline(yintercept=0.17,color="darkgrey",size=0.3)+
    theme_bw()+
    scale_color_manual(values=c("darkblue","darkgreen","darkred","orange"))+
    scale_x_log10()+
    labs(y="Complexity",
         x="log P.95 priority",
         title=NULL)+
    #lims(y=c(0.05,0.25))+
    #lims(x=c(-0.1,0.2))+
    #scale_color_grey()+ 
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="bottom",
          legend.text = element_text(size =13),
          legend.title = element_blank())
   # ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p.95_complexity.png"))
  
  panel_D=ggplot(data=severity_all)+
    geom_smooth(method="lm",aes(x=log(p.95),y=persistence),se=F,color="darkgrey",size=0.3)+
    geom_text_repel(aes(x=log(p.95),y=persistence,label=crisis,color=typology))+
    theme_bw()+
    scale_color_manual(values=c("darkblue","darkgreen","darkred","orange"))+
    labs(y="Persistence", 
         x="log P.95 priority",
         title=NULL)+
    lims(x=c(1,4.5))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="bottom",
          legend.text = element_text(size =13),
          legend.title = element_blank())
   # ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p95_persistence.png"))
  return(list(Panel_A=panel_A,Panel_B=panel_B,Panel_C=panel_C,Panel_D=panel_D))
}


#Total
ctries=mydata$ISO3_Code %>% unique()
severity_all=get_severity_typology(mydata, ctries,shocks)
severity_all$Panel_B$data

severity_all$Panel_A
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/priority_probability.png"))
severity_all$Panel_B
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p.95_probability.png"))
severity_all$Panel_C
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p.95_complexity.png"))
severity_all$Panel_D
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p95_persistence.png"))



footnote=c("RWTC (Remedy Worst Than Curse) crisis refer to particular severe events with high policy trade offs and
           risky policy response, ETC (Easy To Control) refer to events with high probability and low priority denoting crisis with
           no policy trade offs, ETPC (Easy To Prevent and Control) refer to events with low probability and low priority denoting crisis with
           no policy trade offs and strong prevention capacity, TTID (This time Is Different) crisis refer to crisis for which the preparation
           capacity is inversely related to its probability.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity Typology/severity_typology_footnote.tex"))

# High income
ctries=ctry_groups %>% filter(Income_group==" High income" & !iso3c %in% c("ATG","BHS","BHR","BRB","BRN","KWT","MAC",
                                                                           "MLT","OMN","PLW","PAN","PRI","QAT","SMR",
                                                                           "SAU","SYC","SGP","KNA","TTO","ARE"))
ctries=ctries$iso3c

severity_HighIncome=get_severity_typology(mydata, ctries,shocks)

severity_HighIncome$Panel_A
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/priority_probability_HighIncome.png"))
severity_HighIncome$Panel_B
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p.95_probability_HighIncome.png"))
severity_HighIncome$Panel_C
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p.95_complexity_HighIncome.png"))
severity_HighIncome$Panel_D
ggsave(paste0(path_data_directory,"/output/figures/Severity Typology/p95_persistence_HighIncome.png"))


# Middle Income

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
ctries=ctries$iso3c
severity_MiddleIncome=get_severity_typology(mydata, ctries,shocks)

severity_MiddleIncome$Panel_A
severity_MiddleIncome$Panel_B
severity_MiddleIncome$Panel_C
severity_MiddleIncome$Panel_D
# Low Income


ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
ctries=ctries$iso3c
severity_LowIncome=get_severity_typology(mydata, ctries,shocks)

severity_LowIncome$Panel_B

#high income
ctries=ctry_groups %>% filter(Income_group==" High income")
ctries=ctries$iso3c
severity_HighIncome=severity_all %>% filter(ISO3_Code %in% ctries) %>%
   ungroup() %>% group_by(shocks) %>% 
  summarise_at(vars(probability:area),mean) %>% ungroup()%>%
  mutate(probability=round(probability,2),
         priority=round(priority,2),
         persistence=round(persistence,2),
         complexity=round(complexity,2),
         probability_rank=round(probability_rank,2),
         priority_rank=round(priority_rank,2),
         persistence_rank=round(persistence_rank,2),
         complexity_rank=round(complexity_rank,2),
         area=round(area,2)) %>% arrange(-area)%>% arrange(-area) %>%
  dplyr::select(crisis=shocks,probability,rank=probability_rank,
                priority,rank=priority_rank,
                persistence,rank=persistence_rank,
                complexity,rank=complexity_rank,
                area)

stargazer::stargazer(title="Severity: High Income"
                     , severity_HighIncome
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_HighIncome.tex")
)

footnote=c("The table display the average cross countries for all severity dimensions for high income 
countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
allocated to each crisis, persistence the average duration and complexity the average correlation.
The columns rank show for each dimension the average percentile of countries in the high income groups compare to all countries.
The column Area is an aggregate measure constructed by multipling the ranking of all first dimension.
Housing crisis is the highest ranking crisis among high income countries according to the aggregate index. The highest average probability
is economic slowdowns, with a 54 percent probability on average and ranking on average in the 70th percentile. 
The highest average priority is sovereign crisis with an average of 18 percent although high income countries ranks only in the
28th percentile in order of priorities. The more persistent crisis is contagion crisis with an average of 3.1 years and an average ranking
of 50 percent. The more complex crisis are Financial crisis with an average contemporaneous correlation with all other
crisis of 28 percent that locate it in the 64th percentile.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_HighIncome_footnote.tex"))


#middle income

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
ctries=ctries$iso3c
severity_MiddleIncome=severity %>% filter(ISO3_Code %in% ctries) %>%
  ungroup() %>% group_by(shocks) %>% 
  summarise_at(vars(probability:area),mean) %>% ungroup()%>%
  mutate(probability=round(probability,2),
         priority=round(priority,2),
         persistence=round(persistence,2),
         complexity=round(complexity,2),
         probability_rank=round(probability_rank,2),
         priority_rank=round(priority_rank,2),
         persistence_rank=round(persistence_rank,2),
         complexity_rank=round(complexity_rank,2),
         area=round(area,2)) %>% arrange(-area) %>%
  arrange(-area) %>%
  dplyr::select(crisis=shocks,probability,rank=probability_rank,
                priority,rank=priority_rank,
                persistence,rank=persistence_rank,
                complexity,rank=complexity_rank,
                area)

stargazer::stargazer(title="Severity: Middle Income"
                     , severity_MiddleIncome
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_MiddleIncome.tex")
)

footnote=c("The table display the average cross countries for all severity dimensions for high income 
countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
allocated to each crisis, persistence the average duration and complexity the average correlation.
The columns rank show for each dimension the average percentile of countries in the high income groups compare to all countries.
The column Area is an aggregate measure constructed by multipling the ranking of all first dimension.
Currency crisis is the highest ranking crisis among middle income countries according to the aggregate index. The highest average probability
is sovereign crisis, with a 64 percent probability on average and ranking on average in the 55th percentile. 
The highest average priority is sovereign crisis with an average of 37 percent, locating middle income countries in the
51th percentile in order of priorities. The more persistent crisis are sovereign with an average of 5.5 years and an average ranking
in the 51th percentile. The more complex crisis are contagion crisis with an average contemporaneous correlation with all other
crisis of 27 percent that locate it in the 53th percentile.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_MiddleIncome_footnote.tex"))



#Low income

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
ctries=ctries$iso3c
severity_LowIncome=severity %>% filter(ISO3_Code %in% ctries) %>%
  ungroup() %>% group_by(shocks) %>% 
  summarise_at(vars(probability:area),mean) %>% ungroup()%>%
  mutate(probability=round(probability,2),
         priority=round(priority,2),
         persistence=round(persistence,2),
         complexity=round(complexity,2),
         probability_rank=round(probability_rank,2),
         priority_rank=round(priority_rank,2),
         persistence_rank=round(persistence_rank,2),
         complexity_rank=round(complexity_rank,2),
         area=round(area,2)) %>% arrange(-area) %>%
  arrange(-area) %>%
  dplyr::select(crisis=shocks,probability,rank=probability_rank,
                priority,rank=priority_rank,
                persistence,rank=persistence_rank,
                complexity,rank=complexity_rank,
                area)

stargazer::stargazer(title="Severity: Low Income"
                     , severity_LowIncome
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_LowIncome.tex")
)

footnote=c("The table display the average cross countries for all severity dimensions for high income 
countries. Probability denote the likelihood of occurence, priority the average proportion of the report 
allocated to each crisis, persistence the average duration and complexity the average correlation.
The columns rank show for each dimension the average percentile of countries in the high income groups compare to all countries.
The column Area is an aggregate measure constructed by multipling the ranking of all first dimension.
Epidemic crisis is the highest ranking crisis among low and lower middle income countries according to the aggregate index. The highest average probability
is sovereign crisis, with a 72 percent probability on average and ranking on average in the 65th percentile. 
The highest average priority is sovereign crisis with an average of 50 percent, locating low and lower middle income countries in the
66th percentile in order of priorities. The more persistent crisis are sovereign with an average of 6.6 years and an average ranking
in the 64th percentile. The more complex crisis are contagion crisis with an average contemporaneous correlation with all other
crisis of 26 percent that locate it in the 50th percentile.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_summary_LowIncome_footnote.tex"))


# country examples
ctries=c("ARG","FRA","USA","MEX","SWE","LKA","ESP","GRC","JPN")
severity_ctries=lapply(ctries,function(x){
  dt=severity %>% filter(ISO3_Code==x)
  dt=dt %>%  mutate(probability=round(probability,2),
                    priority=round(priority,2),
                    persistence=round(persistence,2),
                    complexity=round(complexity,2),
                    probability_rank=round(probability_rank,2),
                    priority_rank=round(priority_rank,2),
                    persistence_rank=round(persistence_rank,2),
                    complexity_rank=round(complexity_rank,2),
                    area=round(area,2)) %>% arrange(-area) %>%
   arrange(-area) %>% dplyr::select(-ISO3_Code)%>%
    dplyr::select(crisis=shocks,probability,rank=probability_rank,
                  priority,rank=priority_rank,
                  persistence,rank=persistence_rank,
                  complexity,rank=complexity_rank,
                  area)
  
  stargazer::stargazer(title=paste0("Severity:",x)
                       , dt
                       , type="latex"
                       , digits=2
                       , no.space=T
                       , align=T
                       , summary=F
                       , rownames=T
                       , table.placement = "H"
                       , column.sep.width="3pt"
                       , font.size = "footnotesize"
                       , out=paste0(path_data_directory,"/output/figures/severity/Severity_summary_",x,".tex")
  )
  dt
})


## worst performing episodes

ctries=ctry_groups %>% filter(Income_group==" High income" & !iso3c %in% c("ATG","BHS","BHR","BRB","BRN","KWT","MAC",
                                                                           "MLT","OMN","PLW","PAN","PRI","QAT","SMR",
                                                                           "SAU","SYC","SGP","KNA","TTO","ARE"))
ctries=ctries$iso3c
top_proba_highIncome=severity %>%
  filter(ISO3_Code %in% ctries) %>% arrange(-probability_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,probability) %>% mutate(probability=round(probability,2)) %>%
  arrange(-probability) %>%
  filter(probability<1)
top_proba_highIncome=top_proba_highIncome[1:10,]
top_proba_highIncome=t(top_proba_highIncome)
top_proba_highIncome=cbind(c("","","Proba."),top_proba_highIncome)

top_priority_highIncome=severity %>% filter(ISO3_Code %in% ctries) %>% arrange(-priority_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,priority) %>% mutate(priority=round(priority,2)) %>%
  arrange(-priority)
top_priority_highIncome=top_priority_highIncome[1:10,]
top_priority_highIncome=t(top_priority_highIncome)
top_priority_highIncome=cbind(c("","","Prior."),top_priority_highIncome)

top_persistence_highIncome=severity %>% filter(ISO3_Code %in% ctries) %>% arrange(-persistence_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,persistence) %>% mutate(persistence=round(persistence,2)) %>%
  arrange(-persistence)
top_persistence_highIncome=top_persistence_highIncome[1:10,]
top_persistence_highIncome=t(top_persistence_highIncome)
top_persistence_highIncome=cbind(c("","","Persist."),top_persistence_highIncome)

top_complex_highIncome=severity %>% filter(ISO3_Code %in% ctries) %>% arrange(-complexity_rank) %>%
  dplyr::select(ISO3_Code,crisis=shocks,complexity) %>% mutate(complexity=round(complexity,2)) %>%
  arrange(-complexity)
top_complex_highIncome=top_complex_highIncome[1:10,]
top_complex_highIncome=t(top_complex_highIncome)
top_complex_highIncome=cbind(c("","","Complex."),top_complex_highIncome)


top_severity=rbind(top_proba_highIncome,
                   rep("",11),
                   top_priority_highIncome,
                   rep("",11),
                   top_persistence_highIncome,
                   rep("",11),
                   top_complex_highIncome)
colnames(top_severity)=c("Crisis",1:10)

stargazer::stargazer(title="Severity: most severe peformence among high income countries"
                     , top_severity
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=F
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="0pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/severity/Severity_worst_episodes.tex")
)


footnote=c("The table displays the top 10 worst performing countries in each severity dimensions. The highest probability
           of occurence is contagion crisis in Estonia with a probability of 96 percent. Sovereign crisis in Croatia and Argentina
           have being allocated the highest priority with a average close to 50 percent of the total length of reports. The longuest
           episodes are contagion in Estonia (an average of 20 years), sovereign default in Poland (12 years), and sovereign crisis
           in Uruguay (9.2 years). The more complex crisis are Economic recession, World outcome and sovereign crisis in Canada, with an average
           correlation with other crisis of 0.42. The table exclude from the sample of High income small countries 
           (ATG,BHS,BHR,BRB,BRN,KWT,MAC,MLT,OMN,PLW,PAN,PRI,QAT,SMR,SAU,SYC,SGP,KNA,TTO,ARE)")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Severity/severity_Worst_episodes_footnote.tex"))








# very high proba countries

excl= c("ATG","BHS","BHR","BRB","BRN","KWT","MAC","MLT","OMN","PLW","PAN","PRI","QAT","SMR","SAU","SYC","SGP","KNA","TTO","ARE")
ctries=mydata %>% filter(year<1990 & !ISO3_Code %in% excl)
ctries=mydata %>% filter(!ISO3_Code %in% excl)
ctries=ctries$ISO3_Code %>% unique()

top_events(severity %>% filter(ISO3_Code %in% ctries),"probability",path=paste0(path_data_directory,"/output/figures/severity/","probability","_worst_episodes.tex")) 
top_events(severity %>% filter(ISO3_Code %in% ctries),"persistence",path=paste0(path_data_directory,"/output/figures/severity/","persistence","_worst_episodes.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"priority",path=paste0(path_data_directory,"/output/figures/severity/","priority","_worst_episodes.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"complexity",path=paste0(path_data_directory,"/output/figures/severity/","complexity","_worst_episodes.tex"))


ctries=ctry_groups %>% filter(Income_group==" High income" & !iso3c %in% c("ATG","BHS","BHR","BRB","BRN","KWT","MAC",
                                                                           "MLT","OMN","PLW","PAN","PRI","QAT","SMR",
                                                                           "SAU","SYC","SGP","KNA","TTO","ARE"))
ctries=ctries$iso3c %>% unique()

top_events(severity %>% filter(ISO3_Code %in% ctries),"probability",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","probability","_worst_episodes_HighIncome.tex")) 
top_events(severity %>% filter(ISO3_Code %in% ctries),"persistence",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","persistence","_worst_episodes_HighIncome.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"priority",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","priority","_worst_episodes_HighIncome.tex"))
top_events(severity %>% filter(ISO3_Code %in% ctries),"complexity",percentile=0.8,path=paste0(path_data_directory,"/output/figures/severity/","complexity","_worst_episodes_HighIncome.tex"))






# what is the dataframe I am looking for?
# 

severity_summary_table=function(mydata,ctry="FRA",shocks){
  #' @title summary table of severity measures
  #' @describeIn Table summarizing probability, intensity
  #' duration and complexity
  #' @param mydata the tf.idf database 
  #' @param ctry the country to which display the figure
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @return ggplot object
  #' @author Manuel Betin
  #' @export
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  
  probability=mydata %>% 
    dplyr::select(year,ISO3_Code,shocks) %>%
    group_by(ISO3_Code) %>% 
    filter(ISO3_Code%in%ctry& year>=1945) %>%
    ungroup()%>%
    mutate_at(vars(shocks), get_prob) %>% 
    summarise_at(vars(shocks),mean,na.rm=T) %>%
    rename_all(paste0,"_prob") %>% 
    mutate(iso3c = ctry) %>% 
    select(iso3c, everything())

  return(probability)
}
#   cond_mean=function(x){
#     mean(ifelse(x<lowerbound,NA,x),na.rm=T)
#   }
#   
#   intensity=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
#     filter(ISO3_Code%in%ctry& year>=1945) %>% ungroup() %>%
#     summarise_at(vars(shocks),cond_mean) %>%
#     rename_all(paste0,"_intensity") %>% 
#     mutate(iso3c = ctry) %>% 
#     select(iso3c, everything())
# 
#   
#   duration=get_duration(mydata %>% filter(ISO3_Code%in%ctry),shocks)
#   duration=duration %>%
#     group_by(shocks,statistic) %>%
#     summarize(mean=mean(stat,na.rm=T)) %>%
#     spread(key=statistic,value=mean) %>%
#     ungroup()
#   
#   colnames(duration)=c("shocks","p25","p75","max","mean","median","min")
#   
#   duration=duration%>% 
#     ungroup() %>%
#     dplyr::select(shocks,duration=mean) %>%
#     mutate(shocks=str_remove_all(shocks," ")) %>% 
#     spread(shocks, duration) %>%
#     rename_all(paste0, "_duration") %>% 
#     mutate(iso3c = ctry) %>% 
#     select(iso3c, everything())
#     
#   
# summary <- list(probability, intensity, duration) %>% 
#   reduce(merge, by = "iso3c")
# 
#   
# }


# K-means : -----

# Run function for all countries and rbind:

data_cluster <-unique(mydata$ISO3_Code) %>% 
  future_map(~ severity_summary_table(mydata, .x, shocks)) %>% 
  reduce(rbind)

# First column as index:

data_cluster <- data_cluster %>% 
  remove_rownames() %>% 
  column_to_rownames(var = 'iso3c')

# Calculate and display k-means:

k <- data_cluster %>% 
  na.omit() %>% 
  stats::kmeans(3, nstart = 25)

top_events(severity %>% filter(ISO3_Code %in% ctries),"probability",percentile=0.1,direction="lower")
top_events(severity %>% filter(ISO3_Code %in% ctries),"persistence",percentile=0.2,direction="lower")
top_events(severity %>% filter(ISO3_Code %in% ctries),"priority",percentile=0.1,direction="lower")
top_events(severity %>% filter(ISO3_Code %in% ctries),"complexity",percentile=0.1,direction="lower")

fviz_cluster(k, data = data_cluster) +
  theme_minimal() 
