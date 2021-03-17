
##install common packages
library("devtools") #make sure you have the library
github_token=rio::import("/Users/manubetin/Dropbox/Manuel/Professionnel/github_token/github_token.txt")

#install_github("manuelbetin/SetUpProject",auth_token=github_token[[1]])
install_github("manuelbetin/TextMiningCrisis",auth_token=github_token[[1]])

packages <- c("dplyr"
              , 'tictoc'
              , "rio"
              , "tidytext"
              , "stringr"
              , "stringi"
              , "tidyr"
              , "ggplot2"
              , "lubridate"
              , 'crayon'
              , "DT"
              , "plotly"
              , "TextMiningCrisis"
              , "SetUpProject")

## load common packages
SetUpProject::load.my.packages(packages)

 #--------------------------------------------

## Documentation
#' @title consolidate urls of IMF documents
#' @aliases consolidate_urls.R
#' @description Use dataset of urls downloaded from the scrapping of the IMF archives 
#' and consolidate the country specific file and then clean the urls based on the metadata available. This script perform the text
#' analysis on the title of the document to find the type of document, country and details
#' relevant for the filtering of the relevant documents to be downloaded. This code is 
#' the first step of the selection of document and could be update or corrected to find
#' other alternative documents or to make sure some type of documents have not been forgotted
#' @author Manuel B??tin
#' @return A dataset containing the urls for different categories of documents in
#' urls_Requests_Reviews_articleIV.RData

#--------------------------------------------


# consolidate all urls by country into a single file
files=list.files("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_by_ctry")

dt=lapply(files,function(x){
  dt=rio::import(paste0("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_by_ctry/",x))  
  dt=dt %>% mutate(iso3=str_remove(x,".csv"))
  dt
})
dt=do.call(rbind,dt)
dt=dt %>% mutate(period=as.Date(date,"%B %d %Y"),
                 year=year(period),
                 title=tolower(title)) %>% dplyr::select(-date)

#export consolidated urls database

rio::export(dt,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/consolidated_urls_by_ctry.RData")

#extraction of old files on the archives of the IMF
dt=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/consolidated_urls_by_ctry.RData")

#recent extraction on the website of the IMF
dt2=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/recent_IMF_urls.RData")
dt=rbind(dt,dt2)

#--------------------------------------------
## functions #####
# create functions to extract the type of document from the title each functions will be 
# then use to modify the database of url by including columns when a document belong to a 
# category

find_IMFprograms=function(dt){
  
  if(!any(c("title","year") %in% names(dt))){
    print("please provide a valide database containing at least the columns title and year")
   dt
  }else{
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"request"),"request",NA))
  
  # find the number of the review ---------
  dt=dt %>% mutate(Review_number=ifelse(str_detect(title,"review") & str_detect(title,"first"),"review_1",
                                        ifelse(str_detect(title,"review") & str_detect(title,"second"),"review_2",
                                               ifelse(str_detect(title,"review") & str_detect(title,"third"),"review_3",
                                                      ifelse(str_detect(title,"review") & str_detect(title,"fourth"),"review_4",
                                                             ifelse(str_detect(title,"review") & str_detect(title,"fifth"),"review_5",
                                                                    ifelse(str_detect(title,"review") & str_detect(title,"sixth"),"review_6",
                                                                           ifelse(str_detect(title,"review") & str_detect(title,"seventh"),"review_7",
                                                                                  ifelse(str_detect(title,"review") & str_detect(title,"eight"),"review_8",
                                                                                         ifelse(str_detect(title,"review") & str_detect(title,"ninth"),"review_9",
                                                                                                ifelse(str_detect(title,"review") & str_detect(title,"tenth"),"review_10",
                                                                                                       ifelse(str_detect(title, "review") & str_detect(title, "midterm"), "review_midterm", NA))))))))))))
  

  dt=dt %>% mutate(Review_number=ifelse(str_detect(title,"review") & is.na(Review_number),"review",Review_number),
                   type_doc_programs=ifelse(str_detect(title,"review") & !str_detect(title,"request"),"review",type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,"review") & str_detect("request"),"request and review",type_doc_programs))
  
  
  # find use of fund ressource -------
  
  dt=dt %>% mutate(type_doc_programs=ifelse(is.na(type_doc_programs) & str_detect(title,"use of fund"),"Use fund",type_doc_programs))
  
  # find cancellations -------
  dt=dt %>% mutate(Cancellation=ifelse(str_detect(title,"cancellation"),"cancellation",NA))
  
  # find extensions -------
  
  dt=dt %>% mutate(Extension=ifelse(str_detect(title,"prolongation") | str_detect(title,"extension") | str_detect(title,"lengthening") | str_detect(title,"augmentation") | str_detect(title, "postponement"),"extension",NA))

  # find requests for technical assistance ----
  
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"request for technical assistance"),"request for technical assistance",type_doc_programs))
  
  # find compensatory -------
  
  dt=dt %>% mutate(type_doc_programs=ifelse(is.na(type_doc_programs) & str_detect(title,"performance criteria"),"performance criteria",type_doc_programs))
  dt=dt %>% mutate(performance_criteria=ifelse(str_detect(title,"performance criteria") | str_detect(title,"performance criterion"),"performance criteria",NA))
  dt=dt %>% mutate(waiver=ifelse(str_detect(title,"waiver"),"waiver",NA))
  dt=dt %>% mutate(modification=ifelse(str_detect(title,"request for modification"),"modification",NA))
  
  dt=dt %>% mutate(type_doc_programs=ifelse(str_detect(title,"waiver") & str_detect(title,"performance criteria") & is.na(type_doc_programs),"performance criteria and waiver",type_doc_programs),
                   type_doc_programs=ifelse(str_detect(title,"waiver") & is.na(type_doc_programs),"waiver",type_doc_programs))
  
  dt=dt %>% mutate(type_doc_programs=ifelse(!is.na(type_doc_programs) & (str_detect(title,"purchase transaction")),"purchase transac",type_doc_programs))
  
  #print("New columns have been created \n
  #      type_doc_programs,Review_number")
  dt
  }
  
}

find_increase_quotas=function(dt){
  dt=dt %>% mutate(increase_quotas=ifelse(str_detect(title,"increase in quotas"),1,NA))
  dt
}

find_membership=function(dt){
  dt=dt %>% mutate(membership=ifelse(str_detect(title,"application for membership"),1,NA))
  dt
}

find_name_from_title=function(dt){
  
  if(!any(c("title","year") %in% names(dt))){
    print("please provide a valide database containing at least the columns title and year")
    dt
  }else{
  dt=dt %>% mutate(title2=str_replace(title,":","-")) %>% separate(title2,into="country",sep="-") %>% dplyr::select(iso3,country,period,title,everything())
  
  dt= dt  %>% mutate(country=str_trim(gsub('[^ -~]', '', country),"both"))
  
  ctries=countrycode::countrycode(list_countries(),origin="iso3c",destination="country.name") %>% tolower()
  
  nonstandard_ctrynames=c(COD="zaire",SOM="somalia",YEM="yemen arab republic","yugoslavia",CIV="ivory coast",WSM="western samoa",HUN="hungarian people's republic",KOR="korea",
                          MMR="burma",VCT="st. vincent and the grenadines",GMB="the gambia",CIV="cote d'ivoire",COD="people's republic of the congo",CHN="people's republic of china",
                          EGY="arab republic of egypt",MOZ="people's republic of mozambique",TTO="trinidad and tobago",STP="sao tome and principe",LAO="lao people's democratic republic",
                          MOZ="republic of mozambique",POL="republic of poland",CZE="czech and slovak federal republic",RUS='russian federation',CZE="czech republic",SVK="slovak republic",
                          LVA='republic of latvia',KGZ="kyrgyz republic",MDA="republic of moldova",VNM="viet nam",LTU="republic of lithuania",EST="republic of estonia",KAZ="republic of kazakhstan",
                          MKD="former yugoslav republic of macedonia",COG="republic of congo",HRV="republic of croatia",ARM="republic of armenia",BLR="republic of belarus",UZB="republic of uzbekistan",
                          AZE="azerbaijan republic",GEO="republic of georgia",KAZ="republic of kazakstan",BIH="republic of bosnia and herzegovina",YEM="republic of yemen",TJK="republic of tajikistan",
                          BIH="bosnia and herzegovina",KOR="republic of korea",KNA="st. kitts and nevis",GNQ="guinea bissau",MEX="mexico <U+0097> arrangement under the flexible credit line",
                          MEX="mexico<U+0097>review under the flexible credit line arrangement",COL="colombia<U+0097>review under the flexible credit line arrangement")
  
  nonstandard_ctrynames2=as.data.frame(nonstandard_ctrynames)
  nonstandard_ctrynames2$iso3c=names(nonstandard_ctrynames)
  names(nonstandard_ctrynames2)=c("iso3_new","iso3c")
  nonstandard_ctrynames2=nonstandard_ctrynames2 %>% mutate(iso3_new=as.character(iso3_new))
  
  dt=dt %>% mutate(iso3_error=ifelse(!country %in% c(ctries,nonstandard_ctrynames),country,""),
                   iso3_new=as.character(ifelse(country %in% c(ctries,nonstandard_ctrynames),country,"")))
  
  dt=dt %>% left_join(nonstandard_ctrynames2,by=c("iso3_new"))
  
  #correct manually some cases and transform to iso3c
  dt=dt %>% mutate(iso3c=ifelse(is.na(iso3c),countrycode::countrycode(iso3_new,origin="country.name",destination="iso3c"),iso3c),
                   iso3c=ifelse(str_detect(iso3,"mexico"),"MEX",iso3c),
                   #iso3c=ifelse(str_detect(title,"germany"),"DEU",iso3c),
                   iso3c=ifelse(str_detect(iso3,"philippines"),"PHL",iso3c),
                   iso3c=ifelse(str_detect(iso3,"macedonia"),"MKD",iso3c),
                   iso3c=ifelse(str_detect(iso3,"yugoslavia"),"YUG",iso3c))
  
  
   mycountries=c(ctries,nonstandard_ctrynames)
for(j in 1:length(mycountries)){
  iso3ccode=countrycode::countrycode(mycountries[j],origin="country.name",destination="iso3c")
    dt=dt%>%mutate(iso3c=ifelse(is.na(iso3c) & str_detect(title,mycountries[j]),iso3ccode,iso3c))
  }


  
  dt=dt %>% dplyr::select(-c(iso3_new,iso3_error)) %>% rename(iso3_from_title=iso3c) %>%
    dplyr::select(iso3,country,iso3_from_title,period,year,pdf,everything())
  
  
  }
  dt
}

find_repurchase=function(dt){
  dt=dt %>% mutate(repurchase_transaction=ifelse(str_detect(title,"repurchase transaction"),1,NA))
  dt
}

find_consultations=function(dt){
  
  if(!any(c("title","year") %in% names(dt))){
    print("please provide a valide database containing at least the columns title and year")
    dt
  }else{
    
  # find non program reports  
  dt=dt %>% mutate(type_doc_consultations=ifelse(str_detect(title,"article iv consultation"),"Article IV",
                                             ifelse(str_detect(title,"article xiv consultation"),"Article XIV",
                                                   ifelse(str_detect(title,"recent economic developments"),"Eco developments",
                                                          ifelse(str_detect(title,"selected issues"),"Selected issues",
                                                                 ifelse(str_detect(title,"article viii"),"Article VIII",
                                                                        ifelse(str_detect(title,"background papers"),"Article IV",NA)))))))
  
 dt=dt %>% mutate(type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"consultations"),"consultations",type_doc_consultations))
 dt=dt %>% mutate(type_doc_consultations=ifelse(str_detect(title,"summing up"),NA,type_doc_consultations),
                  type_doc_consultations=ifelse(str_detect(title,"stand-by arrangement"),NA,type_doc_consultations)) 
 
 dt=dt %>% mutate(type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"exchange system"),"exchange system",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"exchange rate adjustment"),"exchange system",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"exchange arrangement"),"exchange system",type_doc_consultations))
 
 
 dt=dt %>% mutate(type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"economic report"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"the u.s. economy in 1949"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"the u. s. economy in 1951-52"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"the 1953-54 business contraction in the u.s. (sm/54/67)"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"analysis of certain factors operating in 1953 downturn in the u.s. economy"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"the 1954-55 business recovery in the united states (sm/55/20)"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"recent business expansion in the united states (sm/55/73)"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"united states business developments in the first half of 1958 (sm/58/57)"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"the united states economy in recession and recovery (sm/60/30)"),"consultations",type_doc_consultations),
                  type_doc_consultations=ifelse(is.na(type_doc_consultations) & str_detect(title,"american recovery--why at half speed?"),"consultations",type_doc_consultations)
                  )
 
  dt
  }
}

find_statements=function(dt){
  dt=dt %>% mutate(statements=ifelse(str_detect(title,"statement"),1,NA))
  dt
}

find_technical_assistance=function(dt){
  dt=dt %>% mutate(technical_assistance=ifelse(str_detect(title,"technical assistance"),1,NA))
  dt
}

find_board_governors=function(dt){
  dt=dt %>% mutate(board_governors=ifelse(str_detect(title,"board of governors"),1,NA))
  dt
}

find_overdue_financial_obligations=function(dt){
  dt=dt %>% mutate(overdue_obligations=ifelse(str_detect(title,"overdue financial obligations"),1,NA))
  dt
}

find_expost_assesments=function(dt){
  dt=dt %>% mutate(expost_assessment=ifelse(str_detect(title,"ex post assessment"),1,
                                            ifelse(str_detect(title,"post-program"),1,NA)))
  dt
}

find_exchange_system=function(dt){
  dt=dt %>% mutate(exchange_system=ifelse(str_detect(title,"exchange system"),1,
                                          ifelse(str_detect(title,"exchange rate adjustment"),1,
                                                            ifelse(str_detect(title,"exchange arrangements"),1,NA))))
  dt
}

find_program_type=function(dt){
  
  dt=dt %>% mutate(type_program=ifelse(str_detect(title,"extended fund facility"),"EFF",
                                                       ifelse(str_detect(title,"esaf arrangement"),"ESAF",
                                                              ifelse(str_detect(title,"systemic transformation facility"),"STF",
                                                                     ifelse(str_detect(title,"poverty reduction and growth facility"),"PRGF",
                                                                            ifelse(str_detect(title,"flexible credit line"),"FCL",
                                                                                   ifelse(str_detect(title,"precautionary and liquidity Line") | str_detect(title,"precautionary credit line") ,"PLL",
                                                                                          ifelse(str_detect(title,"saf arrangement"),"SAF",
                                                                                                 ifelse(str_detect(title,"stand-by arrangement") | str_detect(title,"stand-by arrangement") | str_detect(title,"stand-by arrangement"),"SBA",
                                                                                                        ifelse(str_detect(title,"extended arrangement"),"EA","Other"))))))))))
  
  
  dt=dt %>% mutate(type_program=ifelse(is.na(type_program) & str_detect(title,"extended fund facility"),"EFF",type_program),
                                   type_program=ifelse(is.na(type_program) & str_detect(title,"extended arrangement"),"EA",type_program))
  dt
  }

create_file_name=function(dt){
  dt=dt %>% mutate(file=paste0(iso3_from_title,"_",period,"_",ifelse(!is.na(type_doc_programs),type_doc_programs,
                                                                     ifelse(!is.na(type_doc_consultations),type_doc_consultations,"other_doc"))))
  dt
  }

## apply functions by piping all functions ####

dt=dt %>% 
  find_IMFprograms() %>%
  find_consultations() %>%
  find_name_from_title() %>%
  find_membership() %>%
  find_statements() %>% 
  find_repurchase() %>%
  find_technical_assistance() %>%
  find_expost_assesments()%>%
  find_board_governors() %>%
  find_exchange_system() %>%
  find_overdue_financial_obligations() %>%
  find_program_type() %>%
  create_file_name() %>%
  find_increase_quotas()


dt_non_tagged=dt %>% filter(is.na(type_doc_programs) & is.na(type_doc_consultations) &
                    is.na(repurchase_transaction) &
                    is.na(statements) &
                    is.na(repurchase_transaction)  &
                    is.na(technical_assistance) &
                    is.na(expost_assessment) &
                    is.na(board_governors) &
                    is.na(exchange_system) &
                    is.na(overdue_obligations))


#from recent extraction from the website take the iso3 that is already correct
dt=dt %>% mutate(iso3_from_title=ifelse(str_detect(pdf,"www.imf.org"),iso3,iso3_from_title))

dt=dt  %>% filter(iso3_from_title==iso3)

dt_overdue=dt  %>% filter(!is.na(overdue_obligations))
rio::export(dt_overdue,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_overdue.RData")

dt_IMF_programs=dt  %>% filter(!is.na(type_doc_programs))

dt_IMF_programs_request=dt_IMF_programs %>% filter(type_doc_programs=="request")
rio::export(dt_IMF_programs_request,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_program_request.RData")

dt_IMF_programs_reviews=dt_IMF_programs %>% filter(str_detect(type_doc_programs,"review"))
rio::export(dt_IMF_programs_reviews,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_program_reviews.RData")


# In the present case the relevant documents are those that concerns the consultations

dt_IMF_consultations=dt %>% 
  filter(type_doc_consultations %in% c("Article IV","Article XIV","Article VIII","Eco developments","consultations","exchange system") | !is.na(type_doc_programs)) %>%
  group_by(iso3_from_title,period,year,type_doc_consultations) %>% 
  summarize_all(funs(first)) %>% 
  ungroup() %>%
  mutate(name_file=paste0(iso3_from_title,"_",period,"_",type_doc_programs))  %>%
  rename(Loss_Date = period) %>%
  #rename(perf_crit = Performance_criteria) %>%
  rename(perf_criteria = performance_criteria) %>%
  rename(ID = iso3_from_title)

rio::export(dt_IMF_consultations,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_consultations.RData")

dt_IMF_statements=dt %>% filter(!is.na(statements))
rio::export(dt_IMF_statements,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_statements.RData")

dt_IMF_tec_assistance=dt %>% filter(!is.na(technical_assistance))
rio::export(dt_IMF_tec_assistance,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_statements.RData")

dt_IMF_repurchase_transaction=dt %>% filter(!is.na(repurchase_transaction))
rio::export(dt_IMF_repurchase_transaction,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_repurchase.RData")


dt_IMF_exchange_system=dt %>%
  filter(!is.na(exchange_system))
  
rio::export(dt_IMF_repurchase_transaction,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_exchange_system.RData")


# Consolidate old and new extractions to obtain a single database with all documents of interests:
# crisis periods: requests and reviews
# non crisis periods: article IV, consultations, recent economic development, article XIV and exchange system
#this is important because for all requests and review the old extractions has more metadata so we can
# remove files that correspond to corrections.

dt=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_imf_consultations.RData")


dt_old=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_extraction_with_hierarchies.RData")

dt=dt %>% ungroup() %>% dplyr::select(ID,Loss_Date,title,pdf,type_doc_programs,type_doc_consultations,
                                      type_doc_programs,Review_number,perf_criteria,waiver,modification,
                                      type_doc_consultations,membership,statements,
                                      repurchase_transaction,technical_assistance,expost_assessment,
                                      board_governors,exchange_system,overdue_obligations)



dt_old=dt_old %>% ungroup() %>% dplyr::select(ID,Loss_Date,hierarchy,
                                              title,pdf,type_doc,type_program,
                                              Review_number_program=Review_number,
                                              perf_criterion_program=perf_criterct,
                                              waiver_program=waiver,
                                              modification_program=modification,
                                              type_hierarchy)

dt1=dt %>% dplyr::select(ID,Loss_Date,title,pdf)
dt_old1=dt_old %>% dplyr::select(ID,Loss_Date,title,pdf)

mydt=rbind(dt1,dt_old1) %>% distinct()

mydt = mydt %>%
  left_join(dt,by=c("ID","Loss_Date","title","pdf")) %>%
  left_join(dt_old,by=c("ID","Loss_Date","title","pdf")) %>% 
  distinct() %>%
  mutate(type_doc_programs=ifelse(is.na(type_doc_programs) & !is.na(type_doc),type_doc,type_doc_programs)) %>%
  filter(type_hierarchy!="Correction" | is.na(type_hierarchy)) %>%
  mutate(Review_number=ifelse(is.na(Review_number),Review_number_program,NA),
                   perf_criteria=ifelse(is.na(perf_criteria),perf_criterion_program,NA),
                   waiver=ifelse(is.na(waiver),waiver_program,NA),
                   modification=ifelse(is.na(modification),modification_program,NA),
         type_doc_programs=ifelse(is.na(type_doc_programs),type_doc_consultations,type_doc_programs)) %>%
  dplyr::select(ID,period=Loss_Date,title,hierarchy,pdf,type_doc_programs,type_program,type_doc_consultations,
                Review_number,perf_criteria,waiver,modification,membership,statements,
                repurchase_transaction,technical_assistance,expost_assessment,exchange_system,overdue_obligations,
                type_hierarchy) %>% arrange(ID,period)

#create a summary of the documents that we keep by country and type of document
summary_available_documents=mydt %>% group_by(ID) %>% summarize(n=n(),
                                                      first=first(period),
                                                      last=last(period))


#export the final database of interest containing consultations, requests and reviews and that 
#will provide data for non crisis and crisis period
rio::export(summary_available_documents,"../Betin_Collodel/2. Text mining IMF_data/output/summary available files/summary_N_urls_Requests_Reviews_articleIV.csv")
rio::export(mydt,"../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_Requests_Reviews_articleIV.RData")

