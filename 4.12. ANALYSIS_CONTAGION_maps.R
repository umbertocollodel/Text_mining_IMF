#' @title maps displaying the contagion 
#' @description produce maps of the world to observe the 
#' contagion across countries
#' @author Manuel Betin
#' @return figures and tables in the folder Probability

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions

path_data_directory="../Betin_Collodel/2. Text mining IMF_data"


# Average data over year:

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE)



#Evolution of contagion
var="Contagion"
mycontagionmaps=lapply(1980:2017,function(x){
  get_map_crisis(mydata,var,x,threshold=1) + 
    ggsave(paste0(path_data_directory,"/output/figures/Maps/",var,"/maps_",var,"_",x,".png")) 
})
names(mycontagionmaps)=1980:2015

#Evolution of Severe_recession
var="Severe_recession"
mySevere_recession=lapply(1980:2017,function(x){
  get_map_crisis(mydata,var,x,threshold=1) + 
    ggsave(paste0(path_data_directory,"/output/figures/Maps/",var,"/maps_",var,"_",x,".png")) 
})
names(mycontagionmaps)=1980:2017

#Evolution of World_outcomes
var="World_outcomes"
mySevere_recession=lapply(1980:2017,function(x){
  get_map_crisis(mydata,var,x,threshold=1) + 
    ggsave(paste0(path_data_directory,"/output/figures/Maps/",var,"/maps_",var,"_",x,".png")) 
})
names(mycontagionmaps)=1980:2017

#Evolution of Expectations
var="Expectations"
mySevere_recession=lapply(1960:2015,function(x){
  get_map_crisis(mydata,var,x,threshold=1) + 
    ggsave(paste0(path_data_directory,"/output/figures/Maps/",var,"/maps_",var,"_",x,".png")) 
})
names(mycontagionmaps)=1980:2017


#More vulnerable countries to contagion

cond_mean=function(x){
  mean(ifelse(x<=lowerbound,NA,x),na.rm=T)
}

lowerbound=0

dt=mydata %>% dplyr::select(year,ISO3_Code,shocks) %>%
  group_by(ISO3_Code,year)%>%filter(year>=1960) %>%
  summarise_at(vars(shocks),cond_mean) %>%
  gather(key="shock",value="value",-c("year","ISO3_Code")) %>% ungroup() %>%
  group_by(ISO3_Code,year) %>%
  mutate(priority=round(value/sum(value,na.rm=T)*100,2)) %>%
  group_by(ISO3_Code,shock) %>%
  summarise_at(vars("priority"),cond_mean) %>%
  filter(!is.na(priority)) %>%
  group_by(shock) %>%
  mutate(priority_rank=rank(priority)/max(rank(priority))) %>% arrange(-priority_rank)

a=dt %>% filter(shock=="Contagion" & priority<100)

