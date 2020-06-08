 ## 4.1 ANALYSIS_COMPARISON_timeseries_benchmark.R

TS_compare_benchmark=function(mydata,ctries,var1,var2,benchmark_name="Benchmark",ylabel=NULL,path=NULL){
  #'@title plot own index with respect to historic benchmarks
  #'@description plot own index with respect to historic benchmarks
  #'@param mydata dataset containing the tf-idf of crisis and the 
  #'benchmark qualitative variables 
  #'@param ctries a vector of country codes for which to display the figure
  #'@param var1 the quantative variables of comparison
  #'@param var2 the variable of interest to evaluate
  #'@param ylabel the ylabel for the plot
  #'@return ggplot figure
  #'@author Manuel Betin
  #'@export
  #'
  
  fig=lapply(ctries,function(ctry){
    myfig=mydata %>% 
      filter(ISO3_Code==ctry) %>%
      ggplot()+
      geom_line(aes(x=year,y=(get(var1)-mean(get(var1),na.rm=T))/sd(get(var1),na.rm=T),color=gsub("_"," ",var1)))+
      geom_line(aes(x=year,y=(get(var2)-mean(get(var2),na.rm=T))/sd(get(var2),na.rm=T),color=benchmark_name))+
      theme_bw()+
      labs(y=ylabel,
           x=NULL,
           title=NULL)+
      scale_x_continuous(breaks=seq(1945,2020,5))+ #set y ticks
      scale_color_grey()+
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(size =15,angle=90),
            axis.title.x = element_text(size = 11),
            legend.title = element_blank(),
            axis.title.y = element_text(size=15),
            axis.text.y = element_text(size=15),
            plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5),
            legend.position="bottom")
    if(!is.null(path)){
      myfig + ggsave(filename=paste0("Comparison_benchmark_",ctry,".png"),device = 'png',path=path)
    }else{
      myfig
    }
  })
  names(fig)=ctries
  fig
}

Corr_compare_benchmark=function(mydata,vars){
  dt=mydata%>%ungroup()%>%
    dplyr::select(vars) %>% na.omit() %>%cor() %>% data.frame()
  dt=dt %>% dplyr::select(vars)
  dt
}

## 4.3 ANALYSIS_PROBABILITY_event_studies.R

get_timeserie=function(mydata,ctry,shocks,lowerbound=0,path=NULL){
  #' @title plot event study of crisis by country
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param ctry the country to which display the figure
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  figs=lapply(shocks,function(x){
    myfig=mydata %>% 
      filter(ISO3_Code == ctry) %>% ungroup() %>%
      mutate(proba=ifelse(get_prob(get(x))==1,year,NA),
             max=ifelse(max(get(x),na.rm=T)==get(x),get(x),NA)) %>%
      ggplot(aes(year, get(x) , group =1)) +
      geom_vline(aes(xintercept=proba),color="lightgrey",size=7) +
      geom_line(col = "darkblue",alpha=0.6, size = 1.2) +
      geom_point(col = "darkblue",size=1.8) +
      geom_text(aes(x=proba,y=max(get(x),na.rm=T)*2/3,label=proba),angle = 90,size=3.5)+
      theme_bw()+
      labs(y="Term frequency (%)",
           x=NULL,
           title=NULL)+
      #lims(y=c(ymin,ymax))+
      scale_x_continuous(breaks=seq(1945,2020,5))+ #set y ticks
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(size =15,angle=90),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size=15),
            axis.text.y = element_text(size=15),
            plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
            plot.subtitle =element_text(size =7, hjust = 0.5),
            legend.position="none")
    
    if(!is.null(path)){
      myfig + ggsave(filename=paste0(ctry,"_",x,".png"),device = 'png',path=path, width = 7, height = 4, dpi = 300)
    }else{
      myfig
    }
  })
  names(figs)=shocks
  figs
}

## 4.4 ANALYSIS_PROBABILITY_share_countries.R

plot_share_country=function(mydata,shocks,ymin=0,ymax=1,rollmean=5,lowerbound=0,dotpercentile=0.99,path=NULL){
  #' @title plot share of countries experiencing the shocks
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param ymin minimum for y axis
  #' @param ymax maximum for x axis
  #' @param rollmean the number of years to consider from the moving 
  #' average smoothing
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param dotpercentile percentile above which the year is highlighted by
  #' a red dot 
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  
  
  if(any(shocks %in% names(lexicon()))){
    fig=lapply(shocks,function(x){
      
      myfig=mydata %>% 
        mutate_at(vars(Epidemics:World_outcomes), get_prob) %>% 
        ungroup() %>% 
        # group_by(ISO3_Code)%>%
        # mutate(N=n())%>% ungroup() %>%
        group_by(year) %>% 
        filter(year >= 1946) %>% 
        summarise(var = mean(get(x), na.rm = T),
                  N=n()) %>%
        mutate(var_ma = zoo::rollmean(var, rollmean, align = "center", fill = NA))  %>% 
        ungroup()%>%
        mutate(max_var=ifelse(var_ma>=quantile(var_ma,na.rm=T,p=dotpercentile),var_ma,NA)) %>%
        ggplot(aes(year, col="share")) +
        geom_bar(stat="identity",aes(y = var),fill ="darkgrey",color="darkgrey",width = 0.5,position = position_dodge(width = 1),alpha=0.2) +
        geom_line(aes(y = var_ma),col ="darkblue",size=1) +
        geom_point(aes(y=max_var),col="red",size=2)+
        geom_text(aes(y=0.95,x=year,label=ifelse(year %in% c(1950,1960,1970,1980,1990,2000,2010,2019),paste0("N=",N),"")),color="black",size=5,angle = 90,alpha=0.9)+
        theme_bw()+
        labs(y="Share of countries (%)",
             x=NULL,
             title=NULL)+
        lims(y=c(ymin,ymax))+
        scale_x_continuous(breaks=seq(1945,2020,5))+ #set y ticks
        theme(panel.grid.minor = element_blank(),
              axis.text.x = element_text(size =15,angle=90),
              axis.title.x = element_text(size = 11),
              axis.title.y = element_text(size=15),
              axis.text.y = element_text(size=15),
              plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
              plot.subtitle =element_text(size =7, hjust = 0.5),
              legend.position="none")
      
      if(!is.null(path)){
        myfig + ggsave(filename=paste0("Share_",x,".png"),device = 'png',path=path)
      }else{
        myfig
      }
    })
    names(fig)=shocks
    fig
  }else{
    warning("please provide a valid name of shock amoung names(lexicon())")
  }
  
}


## 4.5 ANALYSIS_PROBABILITY_summary.R

get_probability=function(mydata,shocks,period_range=c(1960,2019),lowerbound=0,path=NULL){
  #' @title plot event study of crisis by country
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param ctry the country to which display the figure
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  
  myfig=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>% ungroup() %>%
    filter(year >= period_range[1] & year<=period_range[2]) %>% ungroup()%>%
    mutate_at(vars(shocks), get_prob) %>% 
    summarise_at(vars(shocks),mean,na.rm=T) %>%
    gather(key="shock") %>% mutate(shock=as.character(shock),
                                   shock=ifelse(shock=="Balance_payment_crisis","B.o.P.",shock),
                                   shock=ifelse(shock=="World_outcomes","World",shock),
                                   shock=ifelse(shock=="Sovereign_default","Sovereign",shock),
                                   shock=ifelse(shock=="Natural_disaster","Nat. disaster",shock),
                                   shock=ifelse(shock=="Currency_crisis_severe","Currency",shock),
                                   shock=ifelse(shock=="Soft_recession","Eco. slowdown",shock),
                                   shock=ifelse(shock=="Severe_recession","Eco. recession",shock),
                                   shock=gsub("_","",shock),
                                   shock=gsub("crisis","",shock),
                                   shock = fct_reorder(shock,value))%>%
    ggplot() +
    geom_bar(stat="identity",aes(x=shock,y=value),fill="darkgrey",col = "black",alpha=0.6) +
    geom_text(aes(x=shock,y=value,label=round(value,2)),color = "grey",alpha=1,vjust=-1)+
    theme_bw()+
    labs(y="Share of countries",
         x=NULL,
         title=NULL)+
    lims(y=c(0,1))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15,angle=90, hjust =1,vjust =0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="none")
  
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("Probability_shocks_",period_range[1],"-",period_range[2],".png"),device = 'png',path=path)
  }else{
    myfig
  }
}

## 4.6 ANALYSIS_PERSISTENCE_summary.R

get_duration=function(mydata,shocks,lowerbound=0){
  #' @title get the number of years of each episode of crisis
  #' @description compute the number of years of each episode
  #' of crisis
  #' @param mydata a database with the tfidf
  #' @param shocks the names of the shocks to consider
  #' @author Manuel Betin
  #' @return a dataset with summary statistics on the
  #' duration of episodes
  #' 
  #' 
  #' 
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  get_prob <- function(x){
    ifelse(x > lowerbound,1,0)
  }
  ctry="ATG"
  var="Financial_crisis"
  ctries=mydata$ISO3_Code %>% unique()
  all=lapply(ctries,function(ctry){
    print(ctry)
    dt=lapply(shocks,function(var){
      #print(var)
      dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>% 
        filter(ISO3_Code==ctry & year>=1945) %>% ungroup() %>%
        group_by(year)%>%
        summarise_at(vars(shocks),cond_mean) %>%
        mutate_at(vars(shocks),get_prob)%>% dplyr::select(year,var) %>%
        mutate(var:=ifelse(is.na(get(var)),0,get(var))) %>%
        arrange(year) 
      epi=0
      dt$episode=0
      count=0
      for(i in 2:(dim(dt)[1])){
        if(i+count<=dim(dt)[1]){
          i=i+count
          if(dt[[i,"var"]]>0 & dt[[i-1,"var"]]==0){
            count=1
            epi=ifelse(count==1,epi+1,epi)
            dt[i,"episode"]=epi
            dt[i,var]=count
            if(i+count<=dim(dt)[1]){
              while(dt[[ifelse(i+count>=dim(dt)[1],dim(dt)[1],i+count),"var"]]==1 & i+count<=dim(dt)[1]){
                count=count+1
                dt[i,var]=count
                dt[i,"episode"]=epi
              }
            }
          }
        }
      }
      dt=dt %>% filter(episode>0)
      dt=dt[,1:2]
      colnames(dt)[2]="duration"
      if(dim(dt)[1]!=0){
        dt$ISO3_Code=ctry
        dt$shocks=var
        dt  
      }else{
        dt=NA
      }
      #summary(dt[,var],na.rm=T) %>% data.frame()
    })
    names(dt)=shocks
    dt=do.call(rbind,dt) 
    if(dim(dt)[2]==1){
      NA
    }else{
      dt
    }
  })
  all=do.call(rbind,all)
  all=data.frame(all)
  return(all)
}

get_duration_fig=function(mydata,shocks,lowerbound=0,path=NULL){
  #' @title barplot of duration of crisis
  #' @description ggplot figure showing the average duration 
  #' of episodes
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  avg_duration=get_duration(mydata,shocks)
  
  avg_dur=avg_duration %>%
    group_by(shocks) %>%
    summarize(mean=mean(duration,na.rm=T),
              min=min(duration,na.rm=T),
              p25=quantile(duration,0.25,na.rm=T),
              median=quantile(duration,0.5,na.rm=T),
              p75=quantile(duration,0.75,na.rm=T),
              p95=quantile(duration,0.95,na.rm=T),
              max=max(duration,na.rm=T)) %>%
    arrange(-mean) %>% filter(!is.na(shocks)) %>%
    mutate(shocks=ifelse(shocks=="Balance_payment_crisis","B.o.P.",shocks),
           shocks=ifelse(shocks=="World_outcomes","World",shocks),
           shocks=ifelse(shocks=="Sovereign_default","Sovereign",shocks),
           shocks=ifelse(shocks=="Natural_disaster","Nat. disaster",shocks),
           shocks=ifelse(shocks=="Currency_crisis_severe","Currency",shocks),
           shocks=ifelse(shocks=="Soft_recession","Eco. slowdown",shocks),
           shocks=ifelse(shocks=="Severe_recession","Eco. recession",shocks),
           shocks=gsub("_","",shocks),
           shocks=gsub("crisis","",shocks),
           shocks = fct_reorder(shocks,mean))
  
  myfig=ggplot(avg_dur)+
    geom_errorbar(aes(x=shocks,ymin = p25, ymax = p95),color="grey")+
    #geom_bar(stat="identity",aes(x=shocks,y=mean),fill="darkgrey",col = "black",alpha=0.6)+
    geom_point(aes(x=shocks,y=mean),fill = "red",alpha=0.6,shape=21)+
    geom_text(aes(x=shocks,y=p95,label=round(p95,1)),color = "grey",alpha=1,vjust=-2,size=4)+
    geom_text(aes(x=shocks,y=p25,label=round(p25,1)),color = "grey",alpha=1,vjust=2,size=4)+
    geom_text(aes(x=shocks,y=mean,label=round(mean,1)),color = "black",alpha=1,vjust=-1,size=4)+
    theme_bw()+
    labs(y="years",
         x=NULL,
         title=NULL)+
    lims(y=c(-3,max(avg_dur$p95)+5))+
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15,angle=90, hjust =1,vjust =0.5),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="bottom")
  
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("duration_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}  

## 4.7 ANALYSIS_INTENSITY_summary.R

get_intensity=function(mydata,shocks,lowerbound=0,path=NULL){
  #' @title plot event study of crisis by country
  #' @describeIn ggplot figure showing the share of countries with
  #' positive tf.idf for the selected shock and the corresponding
  #' moving average
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  buckets=list("2013-2020"=c(2013,2020),
               "2003-2013"=c(2003,2013),
               "1992-2003"=c(1992,2003),
               "1976-1992"=c(1976,1992),
               "1950-1976"=c(1950,1976))
  
  #buckets=list("1950-2020"=c(1950,2020))
  
  data= lapply(buckets,function(x){
    data1=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
      filter(year >= x[1] & year<=x[2]) %>% ungroup()%>%
      summarise_at(vars(shocks),cond_mean) %>%
      gather(key="shock") %>% mutate(shock=ifelse(shock=="Balance_payment_crisis","B.o.P.",shock),
                                     shock=ifelse(shock=="World_outcomes","World",shock),
                                     shock=ifelse(shock=="Sovereign_default","Sovereign",shock),
                                     shock=ifelse(shock=="Natural_disaster","Nat. disaster",shock),
                                     shock=ifelse(shock=="Currency_crisis_severe","Currency",shock),
                                     shock=ifelse(shock=="Soft_recession","Eco. slowdown",shock),
                                     shock=ifelse(shock=="Severe_recession","Eco. recession",shock),
                                     shock=gsub("_","",shock),
                                     shock=gsub("crisis","",shock),
                                     shock = fct_reorder(shock,value))%>%
      mutate(value=value/sum(value,na.rm=T),
             bucket=paste0(x[1],"-",x[2])) #%>%
  })
  data=do.call(rbind,data)
  
  myfig=ggplot(data) +
    geom_bar(stat="identity",aes(x=shock,y=value,group=bucket,fill=bucket),col = "black",alpha=0.9) +
    theme_bw()+
    labs(y="% of total shock",
         x=NULL,
         title=NULL)+
    #lims(y=c(0,1))+
    scale_fill_grey()+ 
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(size =15,angle=90, hjust =1,vjust =0.5),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=15),
          axis.text.y = element_text(size=15),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="right",
          legend.title = element_blank())
  
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("Intensity_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}




get_share_priority= function()
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }

dt=mydata %>% 
  dplyr::select(year,ISO3_Code,shocks) %>%
  group_by(ISO3_Code, year)%>%
  filter(year>=1960) %>%
  gather("shock","value", Soft_recession:Social_crisis) %>% 
  group_by(year, ISO3_Code) %>% 
  mutate(max = max(value, na.rm = T)) %>% 
  mutate(value = case_when(value == max & value != 0 ~ 1,
                           TRUE ~ 0))

list = dt %>% 
split(dt$shock) %>% 
  map(~ .x %>% group_by(year) %>% summarise(share_priority = mean(value, na.rm = TRUE)))
  

list[["Natural_disaster"]] %>% 
  ggplot(aes(year, share_priority)) +
  geom_col(fill = "darkgrey",col = "black") +
  theme_minimal() +
  

  
  summarise_at(vars(shocks),cond_mean) %>%
  
  
  

get_first_priority=function(mydata,shocks,lowerbound=0,path=NULL){
  #' @title plot time series with first priority
  #' @description ggplot figure showing the first priority for 
  #' each year
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return ggplot object
  #' @author Umberto collodel
  #' @export
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  
  dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    group_by(year)%>%filter(year>=1960) %>%
    summarise_at(vars(shocks),cond_mean) %>%
    gather(key="shock",value="value",-c("year")) %>% ungroup() %>%
    mutate(shock=as.character(shock),
           shock=ifelse(shock=="Balance_payment_crisis","B.o.P.",shock),
           shock=ifelse(shock=="World_outcomes","World",shock),
           shock=ifelse(shock=="Sovereign_default","Sovereign",shock),
           shock=ifelse(shock=="Natural_disaster","Nat. disaster",shock),
           shock=ifelse(shock=="Currency_crisis_severe","Currency",shock),
           shock=ifelse(shock=="Soft_recession","Eco. slowdown",shock),
           shock=ifelse(shock=="Severe_recession","Eco. recession",shock),
           shock=gsub("_","",shock),
           shock=gsub("crisis","",shock))%>%
    group_by(year) %>%
    mutate(max_value=max(value,na.rm=T)) %>%
    mutate(tot_value=round(max_value/sum(value,na.rm=T)*100,2)) %>%
    filter(max_value==value) %>%
    arrange(-year) %>% filter(year>1945) %>%
    mutate(shock_1=ifelse(year %% 2==1,"",shock),
           mylabel1=ifelse(year %% 2==1,"",paste0(gsub("_"," ",shock))),
           mylabel2=ifelse(year %% 2==0,"",paste0(gsub("_"," ",shock))),
           myprioritylabel1=ifelse(year %% 2==1,"",paste0(" (",tot_value," %)")),
           myprioritylabel2=ifelse(year %% 2==0,"",paste0(" (",tot_value," %)")),
           myyearlabel1=ifelse(year %% 2==1,"",as.character(year)),
           myyearlabel2=ifelse(year %% 2==0,"",as.character(year)),
           year1=ifelse(year %% 2==1,NA,20),
           year2=ifelse(year %% 2==0,NA,-20)) %>% arrange(tot_value)
  
  
  myfig=ggplot(dt)+
    geom_text(aes(x=year,y=year1,label=mylabel1,
                  vjust="-0.2",hjust="left"),#color=mylabel1),
              angle=60,size=3.5)+
    geom_text(aes(x=year,y=year2,label=mylabel2,
                  vjust="left",hjust="right"),#color=mylabel2),
              angle=60,size=3.5)+
    geom_text(aes(x=year,y=0,label=myyearlabel1,
                  vjust=-0.4,hjust=-0.7),color="black",
              angle=90,size=2.5)+
    geom_text(aes(x=year,y=0,label=myyearlabel2,
                  vjust=-0.4,hjust=1.5),color="black",
              angle=90,size=2.5)+
    #geom_hline(aes(yintercept=0))+
    geom_line(aes(x=year,y=0),color="darkgrey")+
    geom_col(aes(x=year,y=year1-1),width = 0.05,color="darkgrey")+
    geom_col(aes(x=year,y=year2+1),width = 0.05,color="darkgrey")+
    scale_color_grey()+
    theme_minimal()+
    labs(y=NULL,
         x=NULL,
         title=NULL)+
    ylim(c(-70,70))+
    xlim(c(1955,2030))+
    scale_fill_grey()+
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size=11),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size=11),
          axis.text.y = element_blank(),
          plot.title=element_text(face="bold",colour ="black",size=15, hjust =0.5),
          plot.subtitle =element_text(size =7, hjust = 0.5),
          legend.position="none",
          legend.title = element_blank())
  
  myfig
  if(!is.null(path)){
    myfig + ggsave(filename=paste0("TS_priority_shocks",".png"),device = 'png',path=path)
  }else{
    myfig
  }
}  

get_priority_table=function(mydata,shocks,lowerbound=0,path=NULL){
  
  #' @title table with summary statistics of priorities
  #' @description table with summary statistics of priorities 
  #' @param mydata the tf.idf database 
  #' @param shocks a vector with the name of the shock of interest (from lexicon() 
  #' categories)
  #' @param lowerbound the threshold value for the tf.idf to be considered
  #' as a crisis
  #' @param path the path of the directory to save the figures
  #' 
  #' @return dataframe 
  #' @author Manuel Betin
  #' @export
  
  cond_mean=function(x){
    mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
  }
  
  dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>% ungroup() %>%
    group_by(ISO3_Code)%>% filter(year>=1960) %>%
    summarise_at(vars(shocks),cond_mean) %>%
    gather(key="shock",value="value",-c("ISO3_Code")) %>% ungroup() %>%
    group_by(ISO3_Code) %>%
    mutate(tot=sum(value,na.rm=T),
           weight=value/tot*100) %>%
    group_by(shock) %>%
    summarize(mean=mean(weight,na.rm=T)%>% round(1),
              sd=sd(weight,na.rm=T)%>% round(1),
              p.10=quantile(weight,p=0.10,na.rm=T)%>% round(1),
              p.50=quantile(weight,p=0.50,na.rm=T)%>% round(1),
              p.80=quantile(weight,p=0.8,na.rm=T)%>% round(1),
              p.95=quantile(weight,p=0.95,na.rm=T)%>% round(1)) %>% arrange(-mean) %>%
    mutate(shock=ifelse(shock=="Balance_payment_crisis","B.o.P.",shock),
           shock=ifelse(shock=="World_outcomes","World",shock),
           shock=ifelse(shock=="Sovereign_default","Sovereign",shock),
           shock=ifelse(shock=="Natural_disaster","Nat. disaster",shock),
           shock=ifelse(shock=="Currency_crisis_severe","Currency",shock),
           shock=ifelse(shock=="Soft_recession","Eco. slowdown",shock),
           shock=ifelse(shock=="Severe_recession","Eco. recession",shock),
           shock=gsub("_","",shock),
           shock=gsub("crisis","",shock),
           shock=as.character(shock))
  
  return(dt)
}

## 4.8 ANALYSIS_CENTRALITY_summary.R


## 4.9 ANALYSIS_SEVERITY_summary.R

severity_summary_table=function(mydata,ctry="FRA",shocks,lowerbound=0){
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
  probability=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    filter(ISO3_Code%in%ctry& year>=1945) %>%
    ungroup()%>%
    mutate_at(vars(shocks), get_prob) %>% 
    group_by(ISO3_Code) %>%
    summarize_at(vars(shocks),mean,na.rm=T) %>%
    gather(key="shocks",value = "value",-"ISO3_Code") %>% dplyr::rename(probability=value)
  
  cond_mean=function(x){
    mean(ifelse(x<lowerbound,NA,x),na.rm=T)
  }
  
  intensity=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
    filter(ISO3_Code%in%ctry& year>=1945) %>% ungroup() %>%
    group_by(ISO3_Code) %>%
    summarise_at(vars(shocks),cond_mean) %>%
    gather(key="shocks",value=value,-"ISO3_Code")%>% dplyr::rename(intensity=value) %>%
    group_by(ISO3_Code) %>%
    mutate(intensity=as.numeric(intensity),
           priority=intensity/sum(intensity,na.rm=T)) %>% dplyr::select(-intensity)
  
  
  myduration=get_duration(mydata %>% filter(ISO3_Code%in%ctry),shocks)
  myduration=myduration %>%
    group_by(ISO3_Code,shocks) %>%
    summarize(persistence=mean(duration,na.rm=T) %>% round(.,2)) %>%
    arrange(-persistence) %>% filter(!is.na(shocks)) %>% arrange(shocks)
  
  mycomplexity=lapply(ctry,function(x){
    mycomplexity=mydata  %>%
      filter(ISO3_Code%in%x& year>=1945) %>%
      ungroup() %>%
      dplyr::select(shocks) %>% na.omit() %>%
      cor() %>% data.frame()
    mycomplexity[is.na(mycomplexity)]=0
    mycomplexity%>% summarise_at(vars(shocks),mean,na.rm=T) %>%
      gather(key="shocks",value="complexity") %>%
      mutate(ISO3_Code=x)
  })
  mycomplexity=do.call(rbind,mycomplexity)
  
  summary_crisis=probability %>% left_join(intensity,by=c("ISO3_Code","shocks"))
  summary_crisis=summary_crisis %>% left_join(myduration,by=c("ISO3_Code","shocks"))
  summary_crisis=summary_crisis %>% left_join(mycomplexity,by=c("ISO3_Code","shocks"))
  
  summary_crisis[is.na(summary_crisis)]=0
  
  summary_crisis=summary_crisis %>%
    group_by(shocks) %>%
    mutate(probability_rank=rank(probability)/max(rank(probability)),
           priority_rank=rank(priority)/max(rank(priority)),
           persistence_rank=rank(persistence)/max(rank(persistence)),
           complexity_rank=rank(complexity)/max(rank(complexity)),
           area=probability_rank*priority_rank*persistence_rank*complexity_rank) %>% ungroup() %>%
    mutate(shocks=ifelse(shocks=="Balance_payment_crisis","B.o.P.",shocks),
           shocks=ifelse(shocks=="World_outcomes","World",shocks),
           shocks=ifelse(shocks=="Sovereign_default","Sovereign",shocks),
           shocks=ifelse(shocks=="Natural_disaster","Nat. disaster",shocks),
           shocks=ifelse(shocks=="Currency_crisis_severe","Currency",shocks),
           shocks=ifelse(shocks=="Soft_recession","Eco. slowdown",shocks),
           shocks=ifelse(shocks=="Severe_recession","Eco. recession",shocks),
           shocks=gsub("_","",shocks),
           shocks=gsub("crisis","",shocks)) %>% 
    arrange(-area) %>% arrange(ISO3_Code)
  
  return(summary_crisis)
}

top_events=function(severity,var,percentile=0.95,direction="higher",path=NULL){
  
  top_events=severity %>% ungroup() %>%
    dplyr::select(ISO3_Code,crisis=shocks,var) %>% group_by(crisis) %>%
    mutate(myvar=round(get(var),2),
           perc_threshold=quantile(myvar,percentile)) %>%
    arrange(-myvar)
  
  if(direction=="lower"){
    top_events=top_events %>% filter(myvar<perc_threshold)
  }else {
    top_events=top_events %>% filter(myvar>perc_threshold)
  }
  
  
  top_events=top_events %>% group_by(crisis) %>% summarize(!!paste0("p",percentile,".threshold"):=unique(perc_threshold),
                                                           !!paste0("Countries with highest ",var):=paste(paste0(ISO3_Code,"(",myvar,")"),collapse=", "))
  
  
  if(!is.null(path)){
    stargazer::stargazer(title=paste0(var,": Tail countries")
                         , top_events
                         , type="latex"
                         , digits=2
                         , no.space=T
                         , align=F
                         , summary=F
                         , rownames=F
                         , table.placement = "H"
                         , column.sep.width="0pt"
                         , font.size = "footnotesize"
                         , out=path)
  }
  return(top_events) 
}


