##### *********************************************************************************************#####
##### set up#####
##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

##install common packages
library("devtools") #make sure you have the library
#install_github("manuelbetin/SetUpProject",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
#install_github("manuelbetin/TextMiningCrisis",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")

packages <- c("dplyr"
              , "ggplot2"
              , "plotly"
              , "pdftools"
              , "lubridate"
              , 'tictoc'
              , "rio"
              , "tidytext"
              , "stringr"
              , "stringi"
              ,"tidyr"
              , "TextMiningCrisis"
              , "SetUpProject",
              "forcats",
              "rnaturalearthdata"
              )

## load common packages
SetUpProject::load.my.packages(packages)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
######## SECTION OF PARAMETERS ##########

current_path = rstudioapi::getActiveDocumentContext()$path
root_path=dirname(current_path)

#***************************************************************************************####

output=list()
output[["Session_info"]]=sessionInfo()

# Import term frequency data and merge with metadata from urls ####

#import term frequency dataframe
LoI_tf_idf=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")

#import metadata from url extractions
name_links_dt="urls_Requests_Reviews_articleIV.RData" #name of the dataframe containing the urls
dt_meta=rio::import(paste0("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/",name_links_dt)) %>%
  mutate(file=paste0(ID,"_",period,"_",type_doc_programs))
  
#merge datasets with metadata from extraction and clean variable names


mydata=LoI_tf_idf %>% full_join(dt_meta,by=c("file")) %>% 
  rename(iso3c=ID) %>% dplyr::select(-c(ISO3_Code,Period,year)) %>%
  dplyr::select(iso3c,period,file,title,type_doc,type_program,type_doc_programs,type_doc_consultations,perf_criteria,membership,
                statements,repurchase_transaction,technical_assistance,expost_assessment,
                exchange_system,overdue_obligations,Review_number,pdf,type_hierarchy,
                hierarchy,waiver,modification,everything()) %>% 
  rename(review_number = Review_number)
  
#create normalization of variables

indexes_2normalize = names(mydata) %>% # the indexes created must all have upper case for the first letter.
  str_subset("^[A-Z]{1}")

mydata=mydata %>% 
  ungroup () %>%
  mutate_at(vars(indexes_2normalize), funs(norm = (. - mean(.,na.rm=T))/sd(.,na.rm=T)))

output[["mydata"]]=mydata

# regroup type documents into lower level categories: request, reviews, cancelations and modifications, consultations and technical assistance
mydata=mydata %>% mutate(type=ifelse(modification=="modification" | type_doc %in% c("modification"),"modification",NA),
                         type=ifelse(type_doc %in% c("request"),"request",type),
                         type=ifelse(type_doc %in% c("review","request and review","extension",
                                                     "cancelation and request","performance criteria","waiver",
                                                     "Use fund","purchase transac",
                                                     "request for postponement"),"review",type),
                         type=ifelse(type_doc %in% c("cancelation"),"cancelation",type),
                         type=ifelse(type_doc %in% c("consultations","Eco developments","Article IV","Article XIV","exchange system"),"consultation",type),
                         type=ifelse(type_doc %in% c("request for technical assistance"),"technical assistance",type))


# for consultations and technical assistance we do not have info on hierarchy so we cannot spot corrections and supplement
# we will consider that they are all clean docs
mydata =mydata %>% mutate(type_hierarchy=ifelse(type %in% c("consultations","technical assistance"),"b_Clean",type_hierarchy)) 

# seach for unconfirmed requests
mydata =mydata %>% mutate(type=ifelse(type %in% c("request") & is.na(type_hierarchy),"request not confirmed",type),
                          type=ifelse(type %in% c("review") & is.na(type_hierarchy),"review not confirmed",type),
                          type=ifelse(type %in% c("modification") & is.na(type_hierarchy),"modification not confirmed",type)) 


mydata=mydata %>% dplyr::select(iso3c,period,type,everything())

output[["N_doc_by_type"]]=mydata %>% group_by(type,type_hierarchy) %>% summarize(n=n())

#merge with lending arrangement data

IMFprograms=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/Lending arrangements/IMF_programs_clean.RData")

df1 <- mydata
df2 <- IMFprograms

# merge tables to match requests with docum

df2 <- df2 %>%
  rename(startdate = Period) %>%
  rename(enddate = Date_Expiration) %>%
  rename(ID = ISO3_Code)  %>%
  mutate(startdate_ext=startdate %m-% months(1),
         enddate_ext=enddate %m+% months(1)) %>%
  dplyr::select(-c(file))

df1 <- df1 %>%
  rename(Loss_Date = period) %>%
  #rename(perf_crit = Performance_criteria) %>%
  rename(ID = iso3c)

# Joining data

dt_merged <- full_join(df1, df2, by="ID") %>% 
  filter(Loss_Date >= startdate_ext & Loss_Date <= enddate_ext)

dt_merged=dt_merged %>% mutate(myID=paste0(ID,"_",Loss_Date))
cols=setdiff(names(dt_merged),names(df1))
df=df1%>% mutate(myID=paste0(ID,"_",Loss_Date)) %>% filter(!myID %in% dt_merged$myID) 

for(i in 1:length(cols)){
  df[[cols[i]]]=NA
}

dt_merged=rbind(dt_merged,df)
dt_merged=dt_merged %>%rename(iso3c=ID, period=Loss_Date) %>% arrange(iso3c, period)



#Only keep verified requests and reviews
dt_merged=dt_merged %>% filter(!type %in% c("modification not confirmed",
                                  "request not confirmed",
                                  "review not confirmed"))

#Look at proportion of document that matched with a program
dt_merged %>% group_by(type) %>% summarize(n=sum(ifelse(!is.na(Amount_agreed_quotas),1,0))/n())

mydata=dt_merged

names(mydata)
# Clean review numbers ####
mydata=mydata %>% group_by(iso3c,startdate) %>% 
  #find first and last documents of each program
  mutate(first_doc=ifelse(is.na(first(review_number)), first(type_doc),first(review_number)), last_review_N=last(review_number)) %>%
  ungroup() %>%
  # regroup review numbers into 3 periods: first review, midterm review and last review
  mutate(review_number_new=ifelse(review_number=="review_midterm" & duration_IMFprogram==1,"review",NA),
                         review_number_new=ifelse(review_number=="review_1" & duration_IMFprogram==1,"review",review_number_new),
                         review_number_new=ifelse(review_number==last_review_N,"3 Last Review",review_number_new),
                         review_number_new=ifelse(type==first_doc | type=="Request","1 Request",review_number_new),#,
                         review_number_new=ifelse(is.na(review_number_new) & !is.na(type),"2 midterm Review",review_number_new),
         review_number_new=ifelse(is.na(myID),"0 No crisis",review_number_new))


output[["N_doc_by_sequence"]]=mydata %>% group_by(review_number_new) %>% summarize(n=n())


# averages on the full duration of the programs ####

myvars=str_remove(names(mydata)[str_detect(names(mydata),"_norm")],"_norm")
mydata=mydata %>% group_by(iso3c,startdate) %>% 
  mutate_at(vars(myvars),funs(program_mean=mean(.,na.rm=T)))

# Export clean data ####
rio::export(mydata,"../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData")
