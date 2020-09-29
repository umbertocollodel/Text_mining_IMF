#' @title summary figures of persistence
#' @description produce the barplot comparison
#' the persistence of each crisis
#' @author Manuel Betin, Umberto Collodel
#' @return figures in the folder duration

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

# Average data over year:

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)

# Duration of events ####

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")


duration_table=get_duration(mydata,shocks)

avg_dur=duration_table %>%
  group_by(shocks) %>%
  summarize(Mean=mean(duration,na.rm=T) %>% round(.,2),
            P75=quantile(duration,0.75,na.rm=T) %>% round(.,2),
            P95=quantile(duration,0.95,na.rm=T) %>% round(.,2)) %>%
    arrange(-Mean) %>% filter(!is.na(shocks)) %>%
  mutate(shocks=ifelse(shocks=="Balance_payment_crisis","B.o.P.",shocks),
         shocks=ifelse(shocks=="World_outcomes","World",shocks),
         shocks=ifelse(shocks=="Sovereign_default","Sovereign",shocks),
         shocks=ifelse(shocks=="Natural_disaster","Nat. disaster",shocks),
         shocks=ifelse(shocks=="Currency_crisis_severe","Currency",shocks),
         shocks=ifelse(shocks=="Soft_recession","Eco. slowdown",shocks),
         shocks=ifelse(shocks=="Severe_recession","Eco. recession",shocks),
         shocks=gsub("_","",shocks),
         shocks=gsub("crisis","",shocks)
         ) %>% 
  rename(Category = shocks)
  
stargazer::stargazer(title="Persistence: summary table"
                     , avg_dur
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=T
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/figures/duration/duration_summary.tex")
)

# ctries=c("USA")
# get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries),shocks=shocks)



get_duration_fig(mydata,shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration"))

ctries=ctry_groups %>% filter(Income_group==" High income")
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration/HighIncome"))

ctries=ctry_groups %>% filter(Income_group==" Upper middle income")
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration/MiddleIncome"))

ctries=ctry_groups %>% filter(Income_group %in% c(" Low income"," Lower middle income"))
get_duration_fig(mydata %>% filter(ISO3_Code %in% ctries$iso3c),shocks=shocks,
                 path=paste0(path_data_directory,"/output/figures/Duration/LowIncome"))

footnote=c("Red dot denote the duration in years of each crisis measure as the number of consecutive years with 
stricly positive tf.idf. horizontal lines denote the 25 percentile and 95 percentile of the duration of each crisis.")

cat(footnote,file=paste0(path_data_directory,"/output/figures/Duration/Duration_shock_footnote.tex"))



