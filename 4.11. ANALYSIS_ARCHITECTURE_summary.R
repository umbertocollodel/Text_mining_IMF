#' @title Table of architecture of crisis
#' @description script to generate the architecture of the crisis
#' for the country and period selected
#' @author Manuel Betin, Umberto Collodel
#' @return Latex tables

#INSTRUCTIONS: To run this file separatly please first run 4.ANALYSIS_source.R from line 1 to ligne 51 to load the 
#packages and functions


path_data_directory="../Betin_Collodel/2. Text mining IMF_data"

mydata <- rio::import(paste0(path_data_directory,"/datasets/tagged docs/tf_idf.RData")) %>% 
  #mutate(year = as.numeric(year)) %>% 
  group_by(ISO3_Code, year) %>%
  summarise_if(is.double, mean, na.rm = TRUE) %>%
  filter(year<2020)

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis','Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")


#-----------------------------------------------
ctry="MEX"
start_year=1993
end_year=1996
architecture_MEX_1994=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: Mexico 1994"
                     , architecture_MEX_1994
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_MEX_1994_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year = end_year)

architecture_MEX_1994_fig$`1996`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))


#-----------------------------------------------
ctry="MEX"
start_year=1978
end_year=1983
architecture_MEX_1982=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)


stargazer::stargazer(title="Architecture of the crisis: Mexico 1982"
                     , architecture_MEX_1982
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_MEX_1982_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_MEX_1982_fig$`1983`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))


#-----------------------------------------------
ctry="THA"
start_year=1995
end_year=1999
architecture_THA_1997=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: Thailand 1997"
                     , architecture_THA_1997
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_THA_1997_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_THA_1997_fig$`1999`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))

#-----------------------------------------------
ctry="IDN"
start_year=1995
end_year=1999
architecture_IDN_1997=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: Indonesia 1997"
                     , architecture_IDN_1997
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_IDN_1997=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_IDN_1997$`1999`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))

#-----------------------------------------------
ctry="JPN"
start_year=1988
end_year=1994
architecture_JPN_1990=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)


stargazer::stargazer(title="Architecture of the crisis: Japan 1990"
                     , architecture_JPN_1990
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_JPN_1990_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_JPN_1990_fig$`1994`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))


#-----------------------------------------------
ctry="USA"
start_year=1986
end_year=1992
architecture_USA_1986=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: United States 1886-1992"
                     , architecture_USA_1986
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_USA_1986_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_USA_1986_fig$`1992`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))

#-----------------------------------------------
ctry="USA"
start_year=2007
end_year=2013
architecture_USA_2007=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: United States 2008"
                     , architecture_USA_2007
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_USA_2007_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year,lowerbound = 0)

architecture_USA_2007_fig$`2013`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))

#-----------------------------------------------
ctry="USA"
start_year=1976
end_year=1982
architecture_USA_1980=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: United States 1980"
                     , architecture_USA_1980
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_USA_1980_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_USA_1980_fig$`1982`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))


#-----------------------------------------------
ctry="USA"
start_year=2002
end_year=2006
architecture_USA_2002=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: United States 2002-2006"
                     , architecture_USA_2002
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_USA_2002_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_USA_2002_fig$`2006`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))


#-----------------------------------------------
ctry="ARG"
start_year=1997
end_year=2003
architecture_ARG_2001=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)


stargazer::stargazer(title="Architecture of the crisis: Argentina 2001"
                     , architecture_ARG_2001
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_ARG_2001_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_ARG_2001_fig$`2003`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))


#-----------------------------------------------
ctry="LBN"
start_year=2012
end_year=2018
architecture_LBN_2016=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: Lebanon 2016"
                     , architecture_LBN_2016
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)

architecture_LBN_2016_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_LBN_2016_fig$`2018`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))


#-----------------------------------------------




#-----------------------------------------------
ctry="GRC"
start_year=2008
end_year=2014
architecture_GRC_2010=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: Greece 2010"
                     , architecture_GRC_2010
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)


architecture_GRC_2010_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_GRC_2010_fig$`2014`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))

#-----------------------------------------------
ctry="GBR"
start_year=1989
end_year=1994
architecture_GBR_1990=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

stargazer::stargazer(title="Architecture of the crisis: United Kingdom 1990"
                     , architecture_GBR_1990
                     , type="latex"
                     , digits=2
                     , no.space=T
                     , align=T
                     , summary=F
                     , rownames=F
                     , table.placement = "H"
                     , column.sep.width="3pt"
                     , font.size = "footnotesize"
                     , out=paste0(path_data_directory,"/output/tables/Architecture_",ctry,"_",start_year,"_",end_year,".tex")
)


architecture_GBR_1990_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_GBR_1990_fig$`1994`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))

#-----------------------------------------------
ctry="ALB"
start_year=1995
end_year=1998
#ponzi scheeme in Albanie (From Schiller book p94 exuberance irrationnelle)
architecture_ALB_1997=get_architecture(mydata,shocks,ctries=ctry,start_year=start_year,end_year=end_year)

architecture_ALB_1997_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_ALB_1997_fig$`1998`




#-----------------------------------------------
ctry="DEU"
start_year=2008
end_year=2014


architecture_DEU_2008_fig=get_architecture_fig(mydata,shocks,ctry=ctry,start_year=start_year,end_year=end_year)

architecture_DEU_2008_fig$`2014`
ggsave(paste0(path_data_directory,"/output/figures/Architecture/Architecture_",ctry,"_",start_year,"_",end_year,".png"))



