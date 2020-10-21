#' @title Perform the analysis on the tf-idf
#' @description scripts that produce all figures,
#' tables and analysis of the paper
#' @author Manuel Betin, Umberto Collodel
#' @return figures, tables and all output available
#' in the paper

#----------------------------------------------------------------------------------

##clean environment
rm(list = ls())

## set the working directory were the script is located
current_path = here::here()
setwd(current_path)
root_path=current_path
##install common packages
devtools::install_github("manuelbetin/SetUpProject",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
devtools::install_github("manuelbetin/TextMiningCrisis",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")
devtools::install_github("manuelbetin/PICindex",auth_token="7502b84abd98de5cb4ce921b9d7ef788bc245181")

packages <- c("dplyr","ggplot2","plotly","pdftools","lubridate",'tictoc'
              ,"rio","tidytext","stringr","stringi","tidyr","network","GGally"
              ,"forcats", "TextMiningCrisis", "SetUpProject", "PICindex","gridExtra"
              ,"ggrepel","igraph","gganimate","tidyselect","purrr"
)

## load common packages
SetUpProject::load.my.packages(packages)
#----------------------------------------------------------------------------------

output=list()
output[["Session_info"]]=sessionInfo()

# Import data ####

#Complete information combining tf, url metadata and quantititve measures
mydata=rio::import("../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData")

ctry_classif=SetUpProject::ctry_classif()%>% rename(iso3c=ISO3_Code)
ctry_groups=SetUpProject::ctry_groups()%>% rename(iso3c=ISO3_Code)

shocks=c("Soft_recession","Sovereign_default","Natural_disaster",'Commodity_crisis',
         'Political_crisis','Banking_crisis',
         'Financial_crisis','Inflation_crisis','Trade_crisis','World_outcomes','Contagion',
         'Expectations','Balance_payment_crisis',"Epidemics","Migration","Housing_crisis",
         'Severe_recession',"Currency_crisis_severe","Wars","Social_crisis")

source("4.0 ANALYSIS_functions.R")

source("4.1. ANALYSIS_COMPARISON_timesserie_benchmark.R")

source("4.2. ANALYSIS_THEORY_transformation_function_figure.R")

source("4.3. ANALYSIS_PROBABILITY_event_studies.R")

source("4.4. ANALYSIS_PROBABILITY_share_countries.R")

source("4.5. ANALYSIS_PROBABILITY_summary.R")

source("4.6. ANALYSIS_PERSISTENCE_summary.R")

source("4.7. ANALYSIS_INTENSITY_summary.R")

source("4.8. ANALYSIS_CENTRALITY_summary.R")

source("4.9. ANALYSIS_SEVERITY_summary.R")

source("4.10. ANALYSIS_PIC_summary.R")

