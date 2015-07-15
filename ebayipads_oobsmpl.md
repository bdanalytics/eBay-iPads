# eBay:iPads:: sold classification:: oobsmpl
bdanalytics  

**  **    
**Date: (Fri) Jul 17, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://inclass.kaggle.com/c/15-071x-the-analytics-edge-summer-2015/download/eBayiPadTrain.csv  
    New:        https://inclass.kaggle.com/c/15-071x-the-analytics-edge-summer-2015/download/eBayiPadTest.csv  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

Regression results:
First run:
    <glb_sel_mdl_id>: 
        OOB_RMSE=<0.4f>; new_RMSE=<0.4f>; <feat1>=<imp>; <feat2>=<imp>

Classification results:
template:
    prdline.my == "Unknown" -> 296
    Low.cor.X.glm: Leaderboard: 0.83458
        newobs_tbl=[N=471, Y=327]; submit_filename=template_Final_glm_submit.csv
        OOB_conf_mtrx=[YN=125, NY=76]=201; max.Accuracy.OOB=0.7710;
            opt.prob.threshold.OOB=0.6
            startprice=100.00; biddable=95.42; productline=49.22; 
            D.T.like=29.75; D.T.use=26.32; D.T.box=21.53; 

prdline: -> Worse than template
    prdline.my == "Unknown" -> 285
    All.X.no.rnorm.rf: Leaderboard: 0.82649
        newobs_tbl=[N=485, Y=313]; submit_filename=prdline_Final_rf_submit.csv
        OOB_conf_mtrx=[YN=119, NY=80]=199; max.Accuracy.OOB=0.8339;
            opt.prob.threshold.OOB=0.5
            startprice=100.00; biddable=84.25; D.sum.TfIdf=7.28; 
            D.T.use=4.26; D.T.veri=2.78; D.T.scratch=1.99; D.T.box=; D.T.like=; 
    Low.cor.X.glm: Leaderboard: 0.81234
        newobs_tbl=[N=471, Y=327]; submit_filename=prdline_Low_cor_X_glm_submit.csv
        OOB_conf_mtrx=[YN=125, NY=74]=199; max.Accuracy.OOB=0.8205;
            opt.prob.threshold.OOB=0.6
            startprice=100.00; biddable=96.07; prdline.my=51.37; 
            D.T.like=29.39; D.T.use=25.43; D.T.box=22.27; D.T.veri=; D.T.scratch=; 

oobssmpl: -> 
    Low.cor.X.glm: Leaderboard: 
        newobs_tbl=[N=440, Y=358]; submit_filename=oobsmpl_Final_glm_submit
        OOB_conf_mtrx=[YN=114, NY=84]=198; max.Accuracy.OOB=0.7780;
            opt.prob.threshold.OOB=0.5
            startprice=100.00; biddable=93.87; prdline.my=60.48; D.sum.TfIdf=; 
            D.T.condition=8.69; D.T.screen=7.96; D.T.use=7.50; D.T.veri=; D.T.scratch=;

### Prediction Accuracy Enhancement Options:
- import.data chunk:
    - which obs should be in fit vs. OOB (currently dirty.0 vs .1 is split 50%)
    
- inspect.data chunk:
    - For date variables
        - Appropriate factors ?
        - Different / More last* features ?
        
- scrub.data chunk:        
- transform.data chunk:
    - derive features from multiple features
    
- manage.missing.data chunk:
    - Not fill missing vars
    - Fill missing numerics with a different algorithm
    - Fill missing chars with data based on clusters 
    
- extract.features chunk:
    - Text variables: move to date extraction chunk ???
        - Mine acronyms
        - Mine places

- Review set_global_options chunk after features are finalized

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("tm")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://inclass.kaggle.com/c/15-071x-the-analytics-edge-summer-2015/download/eBayiPadTrain.csv"
glb_newdt_url <- "https://inclass.kaggle.com/c/15-071x-the-analytics-edge-summer-2015/download/eBayiPadTest.csv"
glb_out_pfx <- "oobsmpl_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newobs_dataset <- TRUE    # or TRUE
    glb_split_entity_newobs_datasets <- TRUE   # or FALSE
    glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
    glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
    glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
    glb_split_sample.seed <- 123               # or any integer

glb_max_fitobs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- !glb_is_regression; 
    glb_is_binomial <- TRUE #or FALSE

glb_rsp_var_raw <- "sold"

# for classification, the response variable has to be a factor
glb_rsp_var <- "sold.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
#     return(log(raw))
    ret_vals <- rep_len(NA, length(raw)); ret_vals[!is.na(raw)] <- ifelse(raw[!is.na(raw)] == 1, "Y", "N"); return(relevel(as.factor(ret_vals), ref="N"))
#     #as.factor(paste0("B", raw))
#     #as.factor(gsub(" ", "\\.", raw))    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA))
```

```
## [1] Y    Y    N    N    <NA>
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
#     return(exp(var))
    as.numeric(var) - 1
#     #as.numeric(var)
#     #gsub("\\.", " ", levels(var)[as.numeric(var)])
#     c("<=50K", " >50K")[as.numeric(var)]
#     #c(FALSE, TRUE)[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA)))
```

```
## [1]  1  1  0  0 NA
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# description = The text description of the product provided by the seller.
# biddable = Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0).
# startprice = The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0).
# condition = The condition of the product (new, used, etc.)
# cellular = Whether the iPad has cellular connectivity (cellular=1) or not (cellular=0).
# carrier = The cellular carrier for which the iPad is equipped (if cellular=1); listed as "None" if cellular=0.
# color = The color of the iPad.
# storage = The iPad's storage capacity (in gigabytes).
# productline = The name of the product being sold.

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- c("UniqueID")
glb_category_vars <- c("prdline.my")
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

# Derived features
glb_derive_lst <- NULL;

# Add logs of numerics that are not distributed normally ->  do automatically ???

glb_derive_lst[["prdline.my"]] <- list(
    mapfn=function(productline) { return(productline) }    
    , args=c("productline"))
#     mapfn=function(Rasmussen) { return(ifelse(sign(Rasmussen) >= 0, 1, 0)) }
#     mapfn=function(PropR) { return(as.factor(ifelse(PropR >= 0.5, "Y", "N"))) }
#     mapfn=function(purpose) { return(relevel(as.factor(purpose), ref="all_other")) }
#     mapfn=function(Week) { return(substr(Week, 1, 10)) }
#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }
#     , args=c("raw"))
#     mapfn=function(PTS, oppPTS) { return(PTS - oppPTS) }
#     , args=c("PTS", "oppPTS"))

# # If glb_allobs_df is not sorted in the desired manner
#     mapfn=function(Week) { return(coredata(lag(zoo(orderBy(~Week, glb_allobs_df)$ILI), -2, na.pad=TRUE))) }
#     mapfn=function(ILI) { return(coredata(lag(zoo(ILI), -2, na.pad=TRUE))) }
#     mapfn=function(ILI.2.lag) { return(log(ILI.2.lag)) }

# glb_derive_lst[["<txt_var>.niso8859.log"]] <- list(
#     mapfn=function(<txt_var>) { match_lst <- gregexpr("&#[[:digit:]]{3};", <txt_var>)
#                         match_num_vctr <- unlist(lapply(match_lst, 
#                                                         function(elem) length(elem)))
#                         return(log(1 + match_num_vctr)) }
#     , args=c("<txt_var>"))

#     mapfn=function(raw) { mod_raw <- raw;
#         mod_raw <- gsub("&#[[:digit:]]{3};", " ", mod_raw);
#         # Modifications for this exercise only
#         mod_raw <- gsub("\\bgoodIn ", "good In", mod_raw);
#                           return(mod_raw)

#         # Create user-specified pattern vectors 
# #sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
#         if (txt_var %in% c("Snippet", "Abstract")) {
#             txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
#                 as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
#                                                    glb_allobs_df[, txt_var]))
#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])

# glb_derive_lst[["<var1>"]] <- glb_derive_lst[["<var2>"]]

glb_derive_vars <- names(glb_derive_lst)
# tst <- "PropR.fctr"; args_lst <- NULL; for (arg in glb_derive_lst[[tst]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; print(head(args_lst[[arg]])); print(head(drv_vals <- do.call(glb_derive_lst[[tst]]$mapfn, args_lst))); 
# print(which_ix <- which(args_lst[[arg]] == 0.75)); print(drv_vals[which_ix]); 

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- c("description")   
#Sys.setlocale("LC_ALL", "C") # For english
glb_txt_munge_filenames_pfx <- "mytxt_"

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- c(0.950) # Generates ??? terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- c("productline") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL #"Low.cor.X.glm"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](ebayipads_oobsmpl_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 9.832  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/eBayiPadTrain.csv..."
## [1] "dimensions of data in ./data/eBayiPadTrain.csv: 1,861 rows x 11 cols"
##                                                                                            description
## 1                                                        iPad is in 8.5+ out of 10 cosmetic condition!
## 2 Previously used, please read description. May show signs of use such as scratches to the screen and 
## 3                                                                                                     
## 4                                                                                                     
## 5 Please feel free to buy. All products have been thoroughly inspected, cleaned and tested to be 100% 
## 6                                                                                                     
##   biddable startprice               condition cellular carrier      color
## 1        0     159.99                    Used        0    None      Black
## 2        1       0.99                    Used        1 Verizon    Unknown
## 3        0     199.99                    Used        0    None      White
## 4        0     235.00 New other (see details)        0    None    Unknown
## 5        0     199.99      Seller refurbished  Unknown Unknown    Unknown
## 6        1     175.00                    Used        1    AT&T Space Gray
##   storage productline sold UniqueID
## 1      16      iPad 2    0    10001
## 2      16      iPad 2    1    10002
## 3      16      iPad 4    1    10003
## 4      16 iPad mini 2    0    10004
## 5 Unknown     Unknown    0    10005
## 6      32 iPad mini 2    1    10006
##                                                                                                        description
## 65                                                                                                                
## 283                                                              Pristine condition, comes with a case and stylus.
## 948  \x89\xdb\xcfUsed Apple Ipad 16 gig 1st generation in Great working condition and 100% functional.Very little 
## 1354                                                                                                              
## 1366         Item still in complete working order, minor scratches, normal wear and tear but no damage. screen is 
## 1840                                                                                                              
##      biddable startprice          condition cellular carrier      color
## 65          0     195.00               Used        0    None    Unknown
## 283         1      20.00               Used        0    None    Unknown
## 948         0     110.00 Seller refurbished        0    None      Black
## 1354        0     300.00               Used        0    None      White
## 1366        1     125.00               Used  Unknown Unknown    Unknown
## 1840        0     249.99               Used        1  Sprint Space Gray
##      storage productline sold UniqueID
## 65        16   iPad mini    0    10065
## 283       64      iPad 1    0    10283
## 948       32      iPad 1    0    10948
## 1354      16    iPad Air    1    11354
## 1366 Unknown      iPad 1    1    11366
## 1840      16    iPad Air    1    11840
##                                                                                            description
## 1856  Overall item is in good condition and is fully operational and ready to use. Comes with box and 
## 1857 Used. Tested. Guaranteed to work. Physical condition grade B+ does have some light scratches and 
## 1858     This item is brand new and was never used; however, the box and/or packaging has been opened.
## 1859                                                                                                  
## 1860     This unit has minor scratches on case and several small scratches on the display. \nIt is in 
## 1861  30 Day Warranty.  Fully functional engraved iPad 1st Generation with signs of normal wear which 
##      biddable startprice               condition cellular carrier
## 1856        0      89.50                    Used        1    AT&T
## 1857        0     239.95                    Used        0    None
## 1858        0     329.99 New other (see details)        0    None
## 1859        0     400.00                     New        0    None
## 1860        0      89.00      Seller refurbished        0    None
## 1861        0     119.99                    Used        1    AT&T
##           color storage productline sold UniqueID
## 1856    Unknown      16      iPad 1    0    11856
## 1857      Black      32      iPad 4    1    11857
## 1858 Space Gray      16    iPad Air    0    11858
## 1859       Gold      16 iPad mini 3    0    11859
## 1860      Black      64      iPad 1    1    11860
## 1861      Black      64      iPad 1    0    11861
## 'data.frame':	1861 obs. of  11 variables:
##  $ description: chr  "iPad is in 8.5+ out of 10 cosmetic condition!" "Previously used, please read description. May show signs of use such as scratches to the screen and " "" "" ...
##  $ biddable   : int  0 1 0 0 0 1 1 0 1 1 ...
##  $ startprice : num  159.99 0.99 199.99 235 199.99 ...
##  $ condition  : chr  "Used" "Used" "Used" "New other (see details)" ...
##  $ cellular   : chr  "0" "1" "0" "0" ...
##  $ carrier    : chr  "None" "Verizon" "None" "None" ...
##  $ color      : chr  "Black" "Unknown" "White" "Unknown" ...
##  $ storage    : chr  "16" "16" "16" "16" ...
##  $ productline: chr  "iPad 2" "iPad 2" "iPad 4" "iPad mini 2" ...
##  $ sold       : int  0 1 1 0 0 1 1 0 1 1 ...
##  $ UniqueID   : int  10001 10002 10003 10004 10005 10006 10007 10008 10009 10010 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- read.delim("data/hygiene.txt", header=TRUE, fill=TRUE, sep="\t",
#                             fileEncoding='iso-8859-1')
# glb_trnobs_df <- read.table("data/hygiene.dat.labels", col.names=c("dirty"),
#                             na.strings="[none]")
# glb_trnobs_df$review <- readLines("data/hygiene.dat", n =-1)
# comment(glb_trnobs_df) <- "glb_trnobs_df"                                

# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
# glb_trnobs_df <- 
#     glb_trnobs_df %>% dplyr::filter(Year >= 1999)
                                
if (glb_is_separate_newobs_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newobs_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newobs_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
## [1] "Reading file ./data/eBayiPadTest.csv..."
## [1] "dimensions of data in ./data/eBayiPadTest.csv: 798 rows x 10 cols"
##                                                                                                  description
## 1                                                                                                   like new
## 2 Item is in great shape. I upgraded to the iPad Air 2 and don&#039;t need the mini any longer, even though 
## 3        This iPad is working and is tested 100%. It runs great. It is in good condition. Cracked digitizer.
## 4                                                                                                           
## 5        Grade A condition means that the Ipad is 100% working condition. Cosmetically 8/9 out of 10 - Will 
## 6                   Brand new factory sealed iPad in an OPEN BOX...THE BOX ITSELF IS HEAVILY DISTRESSED(see 
##   biddable startprice                condition cellular carrier   color
## 1        0     105.00                     Used        1    AT&T Unknown
## 2        0     195.00                     Used        0    None Unknown
## 3        0     219.99                     Used        0    None Unknown
## 4        1     100.00                     Used        0    None Unknown
## 5        0     210.99 Manufacturer refurbished        0    None   Black
## 6        0     514.95  New other (see details)        0    None    Gold
##   storage productline UniqueID
## 1      32      iPad 1    11862
## 2      16 iPad mini 2    11863
## 3      64      iPad 3    11864
## 4      16   iPad mini    11865
## 5      32      iPad 3    11866
## 6      64  iPad Air 2    11867
##                                                                                               description
## 1                                                                                                like new
## 142                                             iPad mini 1st gen wi-fi 16gb is in perfect working order.
## 309     In excellent condition. Minor scratches on the back. Screen in mint condition. Comes in original 
## 312 iPad is in Great condition, the screen is in great condition showing only a few minor scratches, the 
## 320                                                                   Good condition and fully functional
## 369                                                                                                      
##     biddable startprice condition cellular carrier   color storage
## 1          0     105.00      Used        1    AT&T Unknown      32
## 142        1       0.99      Used        0    None Unknown      16
## 309        0     200.00      Used        1    AT&T   Black      32
## 312        1       0.99      Used        0    None Unknown      16
## 320        1      60.00      Used        0    None   White      16
## 369        1     197.97      Used        0    None Unknown      64
##     productline UniqueID
## 1        iPad 1    11862
## 142   iPad mini    12003
## 309      iPad 3    12170
## 312 iPad mini 2    12173
## 320      iPad 1    12181
## 369 iPad mini 3    12230
##                                                                                              description
## 793  Crack on digitizer near top. Top line of digitizer does not respond to touch. Other than that, all 
## 794                                                                                                     
## 795                                                                                                     
## 796                                                                                                     
## 797                                                                                                     
## 798 Slightly Used. Includes everything you need plus a nice leather case!\nThere is a slice mark on the 
##     biddable startprice                condition cellular carrier   color
## 793        0     104.00 For parts or not working        1 Unknown   Black
## 794        0      95.00                     Used        1    AT&T Unknown
## 795        1     199.99 Manufacturer refurbished        0    None   White
## 796        0     149.99                     Used        0    None Unknown
## 797        0       7.99                      New  Unknown Unknown Unknown
## 798        0     139.00                     Used        1 Unknown   Black
##     storage productline UniqueID
## 793      16      iPad 2    12654
## 794      64      iPad 1    12655
## 795      16      iPad 4    12656
## 796      16      iPad 2    12657
## 797 Unknown      iPad 3    12658
## 798      32     Unknown    12659
## 'data.frame':	798 obs. of  10 variables:
##  $ description: chr  "like new" "Item is in great shape. I upgraded to the iPad Air 2 and don&#039;t need the mini any longer, even though " "This iPad is working and is tested 100%. It runs great. It is in good condition. Cracked digitizer." "" ...
##  $ biddable   : int  0 0 0 1 0 0 0 0 0 1 ...
##  $ startprice : num  105 195 220 100 211 ...
##  $ condition  : chr  "Used" "Used" "Used" "Used" ...
##  $ cellular   : chr  "1" "0" "0" "0" ...
##  $ carrier    : chr  "AT&T" "None" "None" "None" ...
##  $ color      : chr  "Unknown" "Unknown" "Unknown" "Unknown" ...
##  $ storage    : chr  "32" "16" "64" "16" ...
##  $ productline: chr  "iPad 1" "iPad mini 2" "iPad 3" "iPad mini" ...
##  $ UniqueID   : int  11862 11863 11864 11865 11866 11867 11868 11869 11870 11871 ...
##  - attr(*, "comment")= chr "glb_newobs_df"
## NULL
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Combine trnent & newobs into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"

# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Train"))
    glb_newobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Test"))    
    glb_id_var <- ".rownames"
}
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0  9.832 10.247   0.416
## 2 inspect.data          2          0 10.248     NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Loading required package: reshape2
```

![](ebayipads_oobsmpl_files/figure-html/inspect.data-1.png) 

```
##       sold.0 sold.1 sold.NA
## Test      NA     NA     798
## Train   1001    860      NA
##          sold.0    sold.1 sold.NA
## Test         NA        NA       1
## Train 0.5378829 0.4621171      NA
## [1] "numeric data missing in glb_allobs_df: "
## sold 
##  798 
## [1] "numeric data w/ 0s in glb_allobs_df: "
## biddable     sold 
##     1446     1001 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline 
##           0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   sold sold.fctr   .n
## 1    0         N 1001
## 2    1         Y  860
## 3   NA      <NA>  798
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

![](ebayipads_oobsmpl_files/figure-html/inspect.data-2.png) 

```
##       sold.fctr.N sold.fctr.Y sold.fctr.NA
## Test           NA          NA          798
## Train        1001         860           NA
##       sold.fctr.N sold.fctr.Y sold.fctr.NA
## Test           NA          NA            1
## Train   0.5378829   0.4621171           NA
```

```r
# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: biddable"
```

![](ebayipads_oobsmpl_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: startprice"
```

![](ebayipads_oobsmpl_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_oobsmpl_files/figure-html/inspect.data-5.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5) +
#         geom_vline(xintercept=84))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0 10.248 15.538    5.29
## 3   scrub.data          2          1 15.538     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##      sold sold.fctr 
##       798       798 
## [1] "numeric data w/ 0s in glb_allobs_df: "
## biddable     sold 
##     1446     1001 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline 
##           0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

findOffendingCharacter <- function(x, maxStringLength=256){  
  print(x)
  for (c in 1:maxStringLength){
    offendingChar <- substr(x,c,c)
    #print(offendingChar) #uncomment if you want the indiv characters printed
    #the next character is the offending multibyte Character
  }    
}
# string_vector <- c("test", "Se\x96ora", "works fine")
# lapply(string_vector, findOffendingCharacter)
# lapply(glb_allobs_df$description[29], findOffendingCharacter)

sel_obs <- function(vars_lst, ignore.case=TRUE, perl=FALSE) {
    tmp_df <- glb_allobs_df
    
    # Does not work for Popular == NAs ???
#     if (!is.null(Popular)) {
#         if (is.na(Popular))
#             tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
#             tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
#     }    
#     if (!is.null(NewsDesk)) 
#         tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    
    for (var in names(vars_lst)) {
        if (grepl(".contains", var))
            tmp_df <- tmp_df[grep(vars_lst[var], 
                                  tmp_df[, unlist(strsplit(var, ".contains"))],
                                  ignore.case=ignore.case, perl=perl), ]
        else 
            tmp_df <- tmp_df[tmp_df[, var] == vars_lst[var], ]
    }

    return(glb_allobs_df[, glb_id_var] %in% tmp_df[, glb_id_var])
}
#print(glb_allobs_df[sel_obs(list(description.contains="mini(?!m)"), perl=TRUE), "description"])

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                        c(glb_id_var, glb_rsp_var, glb_category_vars, glb_txt_vars, cols),
                            FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(list(description.contains="mini(?!m)"), perl=TRUE)
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 15.538 16.366   0.828
## 4 transform.data          2          2 16.367     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Derivations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (new_feat in glb_derive_vars) {
    print(sprintf("Creating new feature: %s...", new_feat))
    args_lst <- NULL 
    for (arg in glb_derive_lst[[new_feat]]$args) 
        args_lst[[arg]] <- glb_allobs_df[, arg]
    glb_allobs_df[, new_feat] <- do.call(glb_derive_lst[[new_feat]]$mapfn, args_lst)
}
```

```
## [1] "Creating new feature: prdline.my..."
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 4   transform.data          2          2 16.367 16.448   0.081
## 5 extract.features          3          0 16.448     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 16.455  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

#stop(here"); sav_allobs_df <- glb_allobs_df #; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    for (sfx in c("", ".POSIX"))
        glb_exclude_vars_as_features <- 
            union(glb_exclude_vars_as_features, 
                    paste(glb_date_vars, sfx, sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last2 <- as.numeric(merge(z-lag(z, -2), b, all=TRUE)); last2[is.na(last2)] <- 0
        glb_allobs_df[, paste0(feat, ".last2.log")] <- log(1 + last2)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last2.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last2.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}
rm(last1, last10, last100)
```

```
## Warning in rm(last1, last10, last100): object 'last1' not found
```

```
## Warning in rm(last1, last10, last100): object 'last10' not found
```

```
## Warning in rm(last1, last10, last100): object 'last100' not found
```

```r
#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 16.455 16.468
## 2 extract.features_factorize.str.vars          2          0 16.469     NA
##   elapsed
## 1   0.013
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##   description     condition      cellular       carrier         color 
## "description"   "condition"    "cellular"     "carrier"       "color" 
##       storage   productline          .src    prdline.my 
##     "storage" "productline"        ".src"  "prdline.my"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               c(glb_exclude_vars_as_features, glb_txt_vars))) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- 
            relevel(factor(glb_allobs_df[, var]),
                    names(which.max(table(glb_allobs_df[, var], useNA = "ifany"))))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: condition: # of unique
## values: 6
```

```
## Warning: Creating factors of string variable: cellular: # of unique values:
## 3
```

```
## Warning: Creating factors of string variable: carrier: # of unique values:
## 7
```

```
## Warning: Creating factors of string variable: color: # of unique values: 5
```

```
## Warning: Creating factors of string variable: storage: # of unique values:
## 5
```

```
## Warning: Creating factors of string variable: prdline.my: # of unique
## values: 12
```

```r
if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(rex_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(rex_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }

#     match_lst <- gregexpr("\\bok(?!ay)", txt_vctr[746], ignore.case = FALSE, perl=TRUE); print(match_lst)
    dsp_pattern <- function(rex_str, ignore.case=TRUE, print.all=TRUE) {
        match_lst <- gregexpr(rex_str, txt_vctr, ignore.case = ignore.case, perl=TRUE)
        match_lst <- regmatches(txt_vctr, match_lst)
        match_df <- data.frame(matches=sapply(match_lst, 
                                              function (elems) paste(elems, collapse="#")))
        match_df <- subset(match_df, matches != "")
        if (print.all)
            print(match_df)
        return(match_df)
    }
    
    dsp_matches <- function(rex_str, ix) {
        print(match_pos <- gregexpr(rex_str, txt_vctr[ix], perl=TRUE))
        print(str_sub(txt_vctr[ix], (match_pos[[1]] / 100) *  99 +   0, 
                                    (match_pos[[1]] / 100) * 100 + 100))        
    }

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], 
                        glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#chk.equal( 1, 100)
#dsp.equal(86, 90)
    
    txt_map_filename <- paste0(glb_txt_munge_filenames_pfx, "map.csv")
    if (!file.exists(txt_map_filename))
        stop(txt_map_filename, " not found!")
    glb_txt_map_df <- read.csv(txt_map_filename, comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[163, "rex_str"])
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining OK in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "(?<!(BO|HO|LO))OK(?!(E\\!|ED|IE|IN|S ))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "Ok(?!(a\\.|ay|in|ra|um))", ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "(?<!( b| B| c| C| g| G| j| M| p| P| w| W| r| Z|\\(b|ar|bo|Bo|co|Co|Ew|gk|go|ho|ig|jo|kb|ke|Ke|ki|lo|Lo|mo|mt|no|No|po|ra|ro|sm|Sm|Sp|to|To))ok(?!(ay|bo|e |e\\)|e,|e\\.|eb|ed|el|en|er|es|ey|i |ie|in|it|ka|ke|ki|ly|on|oy|ra|st|u |uc|uy|yl|yo))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))
    }    
    # txt_vctr <- glb_txt_lst[[glb_txt_vars[1]]]
    # print(chk_pattern_freq(rex_str <- "(?<!( b| c| C| p|\\(b|bo|co|lo|Lo|Sp|to|To))ok(?!(ay|e |e\\)|e,|e\\.|ed|el|en|es|ey|ie|in|on|ra))", ignore.case=FALSE))
    # print(chk_pattern_freq(rex_str <- "ok(?!(ay|el|on|ra))", ignore.case=FALSE))
    # dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
    # dsp_matches(rex_str, ix=8)
    # substr(txt_vctr[86], 5613, 5620)
    # substr(glb_allobs_df[301, "review"], 550, 650)

#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "([[:upper:]]\\.( *)){2,}", ignore.case=FALSE))
        
        # Check for names
        print(subset(chk_pattern_freq(rex_str <- "(([[:upper:]]+)\\.( *)){1}",
                                      ignore.case=FALSE),
                     .n > 1))
        # dsp_pattern(rex_str="(OK\\.( *)){1}", ignore.case=FALSE)
        # dsp_matches(rex_str="(OK\\.( *)){1}", ix=557)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)(\\B)", ix=461)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)", ix=461)        
        #print(str_sub(txt_vctr[676], 10100, 10200))
        #print(str_sub(txt_vctr[74], 1, -1))        
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]
        if (nrow(filtered_df <- subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))) > 0)
            print(orderBy(~ -.n +pattern, filtered_df))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, content_transformer(tolower), lazy=TRUE)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument, lazy=TRUE)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, lazy=TRUE, 
                             preserve_intra_word_dashes=TRUE, lazy=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        txt_compound_filename <- paste0(glb_txt_munge_filenames_pfx, "compound.csv")
        if (!file.exists(txt_compound_filename))
            stop(txt_compound_filename, " not found!")
        filter_df <- read.csv(txt_compound_filename, comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, content_transformer(tolower), lazy=TRUE) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument, lazy=TRUE)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, lazy=TRUE) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english")), lazy=TRUE) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument, lazy=TRUE) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        

        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        # Create <txt_var>.P.mini & air
        txt_X_df[, paste(txt_var_pfx, ".P.mini", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("mini(?!m)", glb_allobs_df[, txt_var],
                                               perl=TRUE))    
        txt_X_df[, paste(txt_var_pfx, ".P.air", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("(?<![fhp])air", glb_allobs_df[, txt_var],
                                               perl=TRUE))    
    
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)
    
#     if (sum(is.na(glb_allobs_df$D.P.http)) > 0)
#         stop("Why is this happening ?")

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}
```

```
## Loading required package: stringr
## Loading required package: tm
## Loading required package: NLP
## 
## Attaching package: 'NLP'
## 
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 16.469 16.833
## 3       extract.features_process.text          3          0 16.834     NA
##   elapsed
## 2   0.365
## 3      NA
## [1] "Building glb_txt_lst..."
## [1] "running gsub for 10 (of 171): #\\bA\\.D\\.P\\.#..."
## [1] "running gsub for 20 (of 171): #\\bA\\.P\\.#..."
## [1] "running gsub for 30 (of 171): #\\bC\\.I\\.C\\.C\\.#..."
## [1] "running gsub for 40 (of 171): #\\bE\\.J\\. Manuel#..."
## [1] "running gsub for 50 (of 171): #\\bF\\.T\\.C\\.#..."
## [1] "running gsub for 60 (of 171): #\\bI\\.M\\.F\\.#..."
## [1] "running gsub for 70 (of 171): #\\bJ\\.F\\.K\\.#..."
## [1] "running gsub for 80 (of 171): #\\bM\\.&A\\.#..."
## [1] "running gsub for 90 (of 171): #\\bN\\.R\\.A\\.#..."
## [1] "running gsub for 100 (of 171): #\\bP\\.R\\.#..."
## [1] "running gsub for 110 (of 171): #\\bS\\.E\\.O\\.#..."
## [1] "running gsub for 120 (of 171): #\\bU\\.S\\.(A\\.)*#..."
## [1] "running gsub for 130 (of 171): #\\bW\\.M\\.D\\.#..."
## [1] "running gsub for 140 (of 171): #\\bLos( |-)((Angeles|Angeless)\\b)+#..."
## [1] "running gsub for 150 (of 171): #(\\w)-(Shanghai|USA)\\b#..."
## [1] "running gsub for 160 (of 171): #(one|two|three|four|five|six|seven|eight|nine)-#..."
## [1] "running gsub for 170 (of 171): #\\b[Ss]eventy-[Oo]ne\\b#..."
## [1] "Remaining OK in description:"
##   pattern .n
## 1      OK  7
## [[1]]
## [1] 3
## attr(,"match.length")
## [1] 2
## attr(,"useBytes")
## [1] TRUE
## attr(,"capture.start")
##         
## [1,] 0 0
## attr(,"capture.length")
##         
## [1,] 0 0
## attr(,"capture.names")
## [1] "" ""
## 
## [1] "ROKEN: Device has at least one or more problems: \nFor Parts or Repair"
## [[1]]
## [1] 3
## attr(,"match.length")
## [1] 2
## attr(,"useBytes")
## [1] TRUE
## attr(,"capture.start")
##         
## [1,] 0 0
## attr(,"capture.length")
##         
## [1,] 0 0
## attr(,"capture.names")
## [1] "" ""
## 
## [1] "ROKEN DEVICE: Problem with Apple ID"
## [[1]]
## [1] 3
## attr(,"match.length")
## [1] 2
## attr(,"useBytes")
## [1] TRUE
## attr(,"capture.start")
##         
## [1,] 0 0
## attr(,"capture.length")
##         
## [1,] 0 0
## attr(,"capture.names")
## [1] "" ""
## 
## [1] "ROKEN: Device has at least one or more problems: \nFor Parts or Repair"
## [[1]]
## [1] 3
## attr(,"match.length")
## [1] 2
## attr(,"useBytes")
## [1] TRUE
## attr(,"capture.start")
##         
## [1,] 0 0
## attr(,"capture.length")
##         
## [1,] 0 0
## attr(,"capture.names")
## [1] "" ""
## 
## [1] "ROKEN: Device has at least one or more problems: \nFor Parts or Repair"
## [[1]]
## [1] 3
## attr(,"match.length")
## [1] 2
## attr(,"useBytes")
## [1] TRUE
## attr(,"capture.start")
##         
## [1,] 0 0
## attr(,"capture.length")
##         
## [1,] 0 0
## attr(,"capture.names")
## [1] "" ""
## 
## [1] "ROKEN: Device has at least one or more problems: \nFor Parts or Repair"
## [[1]]
## [1] 3
## attr(,"match.length")
## [1] 2
## attr(,"useBytes")
## [1] TRUE
## attr(,"capture.start")
##         
## [1,] 0 0
## attr(,"capture.length")
##         
## [1,] 0 0
## attr(,"capture.names")
## [1] "" ""
## 
## [1] "ROKEN: Device has at least one or more problems: \nFor Parts or Repair"
## [[1]]
## [1] 3
## attr(,"match.length")
## [1] 2
## attr(,"useBytes")
## [1] TRUE
## attr(,"capture.start")
##         
## [1,] 0 0
## attr(,"capture.length")
##         
## [1,] 0 0
## attr(,"capture.names")
## [1] "" ""
## 
## [1] "ROKEN SCREEN"
## [1] pattern .n     
## <0 rows> (or 0-length row.names)
## [1] pattern .n     
## <0 rows> (or 0-length row.names)
## [1] "Remaining Acronyms in description:"
## [1] pattern .n     
## <0 rows> (or 0-length row.names)
##        pattern .n
## 1        ONLY.  7
## 2  CONDITION.   6
## 3         GB.   4
## 4       BOX.    2
## 5   CONDITION.  2
## 6     CORNER.   2
## 7         ESN.  2
## 8       GOOD.   2
## 9      ICLOUD.  2
## 10       IMEI.  2
## 11      IPADS.  2
## 12    LOCKED.   2
## 13      LOCKS.  2
## 14         ON.  2
## 15 SCRATCHES.   2
## 16    TEARS.    2
## 17       USE.   2
## [1] "Remaining #\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+# terms in description: "
##          pattern .n
## 2       New Open  3
## 4  New Condition  2
## 7  New Digitizer  1
## 8     New Opened  1
## 9    New Scratch  1
## 10    New Screen  1
## 14     New-Other  1
## [1] "    consider cleaning if relevant to problem domain; geography name; .n > 1"
## [1] "Remaining #\\b(N|S|E|W|C)( |\\.)(\\w)+# terms in description: "
##   pattern .n
## 1 C Stock  3
## 2  W blue  1
## [1] "Remaining #\\b(North|South|East|West|Central)( |\\.)(\\w)+# terms in description: "
##                                                    label step_major
## 3                          extract.features_process.text          3
## 4 extract.features_process.text_reporting_compound_terms          3
##   step_minor    bgn    end elapsed
## 3          0 16.834 19.188   2.354
## 4          1 19.188     NA      NA
## [1] "Remaining compound terms in description: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn    end elapsed
## 4          1 19.188 19.192   0.005
## 5          0 19.193     NA      NA
## [1] "Building glb_corpus_lst..."
##                           label step_major step_minor    bgn    end
## 5 extract.features_build.corpus          4          0 19.193 19.779
## 6  extract.features_extract.DTM          5          0 19.780     NA
##   elapsed
## 5   0.587
## 6      NA
## [1] "Extracting TfIDf terms for description..."
```

```
## Warning in weighting(x): empty document(s): character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) character(0) character(0) character(0) character(0)
## character(0) charact
```

```
##                          label step_major step_minor    bgn    end elapsed
## 6 extract.features_extract.DTM          5          0 19.780 23.183   3.403
## 7  extract.features_report.DTM          6          0 23.183     NA      NA
## [1] "Reporting TfIDf terms for description..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 2659, terms: 1123)>>
## Non-/sparse entries: 9175/2976882
## Sparsity           : 100%
## Maximal term length: 27
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 2659, terms: 10)>>
## Non-/sparse entries: 1989/24601
## Sparsity           : 93%
## Maximal term length: 9
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](ebayipads_oobsmpl_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_oobsmpl_files/figure-html/extract.features-2.png) ![](ebayipads_oobsmpl_files/figure-html/extract.features-3.png) 

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 23.183 25.694   2.511
## 8   extract.features_bind.DTM          7          0 25.695     NA      NA
## [1] "Binding DTM for description..."
##                       label step_major step_minor    bgn    end elapsed
## 8 extract.features_bind.DTM          7          0 25.695 25.703   0.008
## 9 extract.features_bind.DXM          8          0 25.703     NA      NA
## [1] "Binding DXM for description..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_oobsmpl_files/figure-html/extract.features-4.png) 

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
# dsp_obs(list(description.contains="mini(?!m)"), perl=TRUE, cols="D.P.mini", all=TRUE)
# dsp_obs(list(D.P.mini=1), cols="D.P.mini", all=TRUE)
# dsp_obs(list(D.P.mini=1, productline="Unknown"), cols="D.P.mini", all=TRUE)

# dsp_obs(list(description.contains="(?<![fhp])air"), perl=TRUE, all=TRUE)
# dsp_obs(list(description.contains="air"), perl=FALSE, cols="D.P.air", all=TRUE)
# dsp_obs(list(D.P.air=1, productline="Unknown"), cols="D.P.air", all=TRUE)

glb_allobs_df[(glb_allobs_df$D.P.mini == 1) & (glb_allobs_df$productline == "Unknown"), "prdline.my"] <- "iPad mini"
print(mycreate_sqlxtab_df(glb_allobs_df, c("prdline.my", "productline", "D.P.mini", glb_rsp_var)))
```

```
##          prdline.my      productline D.P.mini sold.fctr  .n
## 1            iPad 2           iPad 2        0      <NA> 154
## 2            iPad 2           iPad 2        0         Y 147
## 3            iPad 2           iPad 2        0         N 139
## 4         iPad mini        iPad mini        0         N 138
## 5         iPad mini        iPad mini        0         Y 126
## 6            iPad 1           iPad 1        0         Y 125
## 7           Unknown          Unknown        0         N 121
## 8         iPad mini        iPad mini        0      <NA> 108
## 9            iPad 1           iPad 1        0         N 102
## 10         iPad Air         iPad Air        0         N 102
## 11       iPad Air 2       iPad Air 2        0         N 100
## 12           iPad 4           iPad 4        0         N  93
## 13          Unknown          Unknown        0      <NA>  89
## 14           iPad 1           iPad 1        0      <NA>  88
## 15          Unknown          Unknown        0         Y  81
## 16           iPad 3           iPad 3        0         Y  80
## 17         iPad Air         iPad Air        0         Y  78
## 18         iPad Air         iPad Air        0      <NA>  74
## 19           iPad 3           iPad 3        0         N  73
## 20       iPad Air 2       iPad Air 2        0         Y  71
## 21           iPad 4           iPad 4        0      <NA>  68
## 22           iPad 4           iPad 4        0         Y  64
## 23       iPad Air 2       iPad Air 2        0      <NA>  62
## 24      iPad mini 3      iPad mini 3        0         N  61
## 25      iPad mini 2      iPad mini 2        0         N  56
## 26           iPad 3           iPad 3        0      <NA>  55
## 27      iPad mini 2      iPad mini 2        0      <NA>  52
## 28      iPad mini 2      iPad mini 2        0         Y  48
## 29      iPad mini 3      iPad mini 3        0      <NA>  35
## 30      iPad mini 3      iPad mini 3        0         Y  27
## 31        iPad mini        iPad mini        1         N   7
## 32        iPad mini        iPad mini        1         Y   5
## 33      iPad mini 2      iPad mini 2        1      <NA>   4
## 34 iPad mini Retina iPad mini Retina        0         Y   4
## 35        iPad mini          Unknown        1      <NA>   3
## 36        iPad mini        iPad mini        1      <NA>   3
## 37      iPad mini 3      iPad mini 3        1      <NA>   3
## 38 iPad mini Retina iPad mini Retina        0         N   3
## 39      iPad mini 2      iPad mini 2        1         N   2
## 40      iPad mini 3      iPad mini 3        1         N   2
## 41           iPad 5           iPad 5        0         Y   1
## 42        iPad mini          Unknown        1         N   1
## 43        iPad mini          Unknown        1         Y   1
## 44        iPad mini        iPad mini        2         Y   1
## 45      iPad mini 2      iPad mini 2        1         Y   1
## 46 iPad mini Retina iPad mini Retina        1         N   1
```

```r
glb_allobs_df[glb_allobs_df$UniqueID == 11863, "D.P.air"] <- 0
glb_allobs_df[(glb_allobs_df$D.P.air == 1) & (glb_allobs_df$productline == "Unknown"), "prdline.my"] <- "iPad Air"
print(mycreate_sqlxtab_df(glb_allobs_df, c("prdline.my", "productline", "D.P.air", glb_rsp_var)))
```

```
##          prdline.my      productline D.P.air sold.fctr  .n
## 1            iPad 2           iPad 2       0      <NA> 154
## 2            iPad 2           iPad 2       0         Y 147
## 3         iPad mini        iPad mini       0         N 145
## 4            iPad 2           iPad 2       0         N 139
## 5         iPad mini        iPad mini       0         Y 132
## 6            iPad 1           iPad 1       0         Y 125
## 7           Unknown          Unknown       0         N 120
## 8         iPad mini        iPad mini       0      <NA> 111
## 9            iPad 1           iPad 1       0         N 102
## 10         iPad Air         iPad Air       0         N  98
## 11       iPad Air 2       iPad Air 2       0         N  97
## 12           iPad 4           iPad 4       0         N  92
## 13          Unknown          Unknown       0      <NA>  88
## 14           iPad 1           iPad 1       0      <NA>  88
## 15          Unknown          Unknown       0         Y  80
## 16           iPad 3           iPad 3       0         Y  79
## 17         iPad Air         iPad Air       0         Y  75
## 18           iPad 3           iPad 3       0         N  73
## 19         iPad Air         iPad Air       0      <NA>  73
## 20       iPad Air 2       iPad Air 2       0         Y  69
## 21           iPad 4           iPad 4       0      <NA>  68
## 22           iPad 4           iPad 4       0         Y  64
## 23      iPad mini 3      iPad mini 3       0         N  63
## 24       iPad Air 2       iPad Air 2       0      <NA>  60
## 25      iPad mini 2      iPad mini 2       0         N  58
## 26      iPad mini 2      iPad mini 2       0      <NA>  56
## 27           iPad 3           iPad 3       0      <NA>  55
## 28      iPad mini 2      iPad mini 2       0         Y  49
## 29      iPad mini 3      iPad mini 3       0      <NA>  38
## 30      iPad mini 3      iPad mini 3       0         Y  27
## 31         iPad Air         iPad Air       1         N   4
## 32 iPad mini Retina iPad mini Retina       0         N   4
## 33 iPad mini Retina iPad mini Retina       0         Y   4
## 34         iPad Air         iPad Air       1         Y   3
## 35        iPad mini          Unknown       0      <NA>   3
## 36       iPad Air 2       iPad Air 2       1      <NA>   2
## 37       iPad Air 2       iPad Air 2       1         N   2
## 38       iPad Air 2       iPad Air 2       1         Y   2
## 39           iPad 3           iPad 3       1         Y   1
## 40           iPad 4           iPad 4       1         N   1
## 41           iPad 5           iPad 5       0         Y   1
## 42         iPad Air          Unknown       1      <NA>   1
## 43         iPad Air          Unknown       1         N   1
## 44         iPad Air          Unknown       1         Y   1
## 45         iPad Air         iPad Air       1      <NA>   1
## 46       iPad Air 2       iPad Air 2       2         N   1
## 47        iPad mini          Unknown       0         N   1
## 48        iPad mini          Unknown       0         Y   1
```

```r
glb_allobs_df[glb_allobs_df$UniqueID == 12156, "prdline.my"] <- "iPad 1"
glb_allobs_df[glb_allobs_df$UniqueID == 11811, "prdline.my"] <- "iPad 2"
glb_allobs_df[glb_allobs_df$UniqueID == 11767, "prdline.my"] <- "iPad 2"
glb_allobs_df[glb_allobs_df$UniqueID == 11767, "storage"] <- "32"

# dsp_obs(list(prdline.my="Unknown"), all=TRUE)

glb_allobs_df$prdline.my.fctr <- as.factor(glb_allobs_df$prdline.my)
glb_allobs_df$storage.fctr <- as.factor(glb_allobs_df$storage)

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                        label step_major step_minor    bgn    end elapsed
## 9  extract.features_bind.DXM          8          0 25.703 36.509  10.806
## 10      extract.features_end          9          0 36.510     NA      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                                    label step_major
## 9                              extract.features_bind.DXM          8
## 6                           extract.features_extract.DTM          5
## 7                            extract.features_report.DTM          6
## 3                          extract.features_process.text          3
## 5                          extract.features_build.corpus          4
## 2                    extract.features_factorize.str.vars          2
## 1                                   extract.features_bgn          1
## 8                              extract.features_bind.DTM          7
## 4 extract.features_process.text_reporting_compound_terms          3
##   step_minor    bgn    end elapsed duration
## 9          0 25.703 36.509  10.806   10.806
## 6          0 19.780 23.183   3.403    3.403
## 7          0 23.183 25.694   2.511    2.511
## 3          0 16.834 19.188   2.354    2.354
## 5          0 19.193 19.779   0.587    0.586
## 2          0 16.469 16.833   0.365    0.364
## 1          0 16.455 16.468   0.013    0.013
## 8          0 25.695 25.703   0.008    0.008
## 4          1 19.188 19.192   0.005    0.004
## [1] "Total Elapsed Time: 36.509 secs"
```

![](ebayipads_oobsmpl_files/figure-html/extract.features-5.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](ebayipads_oobsmpl_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 16.448 37.899  21.451
## 6     cluster.data          4          0 37.900     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 37.900 38.828   0.928
## 7 manage.missing.data          4          1 38.829     NA      NA
```

```r
# If mice crashes with error: Error in get(as.character(FUN), mode = "function", envir = envir) : object 'State' of mode 'function' was not found
#   consider excluding 'State' as a feature

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in : "
##      sold sold.fctr 
##       798       798 
## [1] "numeric data w/ 0s in : "
##                biddable                    sold           cellular.fctr 
##                    1446                    1001                    1595 
##              D.T.condit           D.T.condition                D.T.good 
##                    2455                    2385                    2463 
##                D.T.ipad                 D.T.new             D.T.scratch 
##                    2432                    2520                    2431 
##              D.T.screen                D.T.this                 D.T.use 
##                    2453                    2503                    2474 
##                D.T.work                 D.T.box           D.T.excellent 
##                    2485                    2553                    2602 
##               D.T.great                D.T.like                D.T.used 
##                    2539                    2591                    2540 
##                D.T.veri             D.nwrds.log         D.nwrds.unq.log 
##                    2567                    1521                    1529 
##             D.sum.TfIdf D.ratio.sum.TfIdf.nwrds             D.nchrs.log 
##                    1529                    1529                    1521 
##             D.nuppr.log             D.ndgts.log           D.npnct01.log 
##                    1523                    2428                    2581 
##           D.npnct02.log           D.npnct03.log           D.npnct04.log 
##                    2659                    2616                    2659 
##           D.npnct05.log           D.npnct06.log           D.npnct07.log 
##                    2594                    2556                    2659 
##           D.npnct08.log           D.npnct09.log           D.npnct10.log 
##                    2583                    2643                    2650 
##           D.npnct11.log           D.npnct12.log           D.npnct13.log 
##                    2303                    2539                    1934 
##           D.npnct14.log           D.npnct15.log           D.npnct16.log 
##                    2584                    2638                    2548 
##           D.npnct17.log           D.npnct18.log           D.npnct19.log 
##                    2659                    2658                    2659 
##           D.npnct20.log           D.npnct21.log           D.npnct22.log 
##                    2659                    2659                    2659 
##           D.npnct23.log           D.npnct24.log           D.npnct25.log 
##                    2659                    1521                    2659 
##           D.npnct26.log           D.npnct27.log           D.npnct28.log 
##                    2659                    2659                    2651 
##           D.npnct29.log           D.npnct30.log         D.nstopwrds.log 
##                    2659                    2659                    1669 
##                D.P.http                D.P.mini                 D.P.air 
##                    2659                    2625                    2639 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline  prdline.my 
##           0           0
```

```r
# glb_allobs_df <- na.omit(glb_allobs_df)

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col],
                                                       inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    
    # complete(mice()) changes attributes of factors even though values don't change
    for (col in ret_vars) {
        if (inherits(out_impent_df[, col], "factor")) {
            if (identical(as.numeric(out_impent_df[, col]), 
                          as.numeric(inp_impent_df[, col])))
                ret_vars <- setdiff(ret_vars, col)
        }
    }
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
##      sold sold.fctr 
##       798       798 
## [1] "numeric data w/ 0s in : "
##                biddable                    sold           cellular.fctr 
##                    1446                    1001                    1595 
##              D.T.condit           D.T.condition                D.T.good 
##                    2455                    2385                    2463 
##                D.T.ipad                 D.T.new             D.T.scratch 
##                    2432                    2520                    2431 
##              D.T.screen                D.T.this                 D.T.use 
##                    2453                    2503                    2474 
##                D.T.work                 D.T.box           D.T.excellent 
##                    2485                    2553                    2602 
##               D.T.great                D.T.like                D.T.used 
##                    2539                    2591                    2540 
##                D.T.veri             D.nwrds.log         D.nwrds.unq.log 
##                    2567                    1521                    1529 
##             D.sum.TfIdf D.ratio.sum.TfIdf.nwrds             D.nchrs.log 
##                    1529                    1529                    1521 
##             D.nuppr.log             D.ndgts.log           D.npnct01.log 
##                    1523                    2428                    2581 
##           D.npnct02.log           D.npnct03.log           D.npnct04.log 
##                    2659                    2616                    2659 
##           D.npnct05.log           D.npnct06.log           D.npnct07.log 
##                    2594                    2556                    2659 
##           D.npnct08.log           D.npnct09.log           D.npnct10.log 
##                    2583                    2643                    2650 
##           D.npnct11.log           D.npnct12.log           D.npnct13.log 
##                    2303                    2539                    1934 
##           D.npnct14.log           D.npnct15.log           D.npnct16.log 
##                    2584                    2638                    2548 
##           D.npnct17.log           D.npnct18.log           D.npnct19.log 
##                    2659                    2658                    2659 
##           D.npnct20.log           D.npnct21.log           D.npnct22.log 
##                    2659                    2659                    2659 
##           D.npnct23.log           D.npnct24.log           D.npnct25.log 
##                    2659                    1521                    2659 
##           D.npnct26.log           D.npnct27.log           D.npnct28.log 
##                    2659                    2659                    2651 
##           D.npnct29.log           D.npnct30.log         D.nstopwrds.log 
##                    2659                    2659                    1669 
##                D.P.http                D.P.mini                 D.P.air 
##                    2659                    2625                    2639 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline  prdline.my 
##           0           0
```

## Step `4.1: manage missing data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Last call for data modifications 
#stop(here") # sav_allobs_df <- glb_allobs_df
# glb_allobs_df[(glb_allobs_df$PropR == 0.75) & (glb_allobs_df$State == "Hawaii"), "PropR.fctr"] <- "N"

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 7 manage.missing.data          4          1 38.829 38.973   0.144
## 8     select.features          5          0 38.973     NA      NA
```

## Step `5.0: select features`

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
## Warning in cor(data.matrix(entity_df[, sel_feats]), y =
## as.numeric(entity_df[, : the standard deviation is zero
```

```
##                                              id         cor.y
## sold                                       sold  1.0000000000
## biddable                               biddable  0.5485860292
## startprice                           startprice -0.4556701005
## UniqueID                               UniqueID -0.1904241613
## condition.fctr                   condition.fctr -0.1531007887
## D.npnct05.log                     D.npnct05.log -0.1178427734
## prdline.my.fctr                 prdline.my.fctr -0.0898840683
## D.npnct14.log                     D.npnct14.log -0.0784119354
## cellular.fctr                     cellular.fctr -0.0770423554
## carrier.fctr                       carrier.fctr -0.0677807650
## D.ndgts.log                         D.ndgts.log -0.0633901377
## D.nwrds.unq.log                 D.nwrds.unq.log -0.0619524203
## D.npnct09.log                     D.npnct09.log -0.0617301891
## D.nwrds.log                         D.nwrds.log -0.0587159086
## D.nchrs.log                         D.nchrs.log -0.0565815056
## D.ratio.nstopwrds.nwrds D.ratio.nstopwrds.nwrds  0.0554866401
## D.nuppr.log                         D.nuppr.log -0.0554742013
## D.npnct12.log                     D.npnct12.log -0.0549329137
## D.T.like                               D.T.like -0.0533938586
## D.npnct28.log                     D.npnct28.log -0.0523776347
## D.T.box                                 D.T.box -0.0515596986
## D.npnct06.log                     D.npnct06.log -0.0497583609
## D.T.this                               D.T.this -0.0477867670
## D.npnct24.log                     D.npnct24.log -0.0459503420
## D.npnct16.log                     D.npnct16.log -0.0447153098
## D.nstopwrds.log                 D.nstopwrds.log -0.0439279746
## color.fctr                           color.fctr -0.0426291775
## D.T.condit                           D.T.condit -0.0415362400
## D.npnct15.log                     D.npnct15.log  0.0400921803
## D.npnct08.log                     D.npnct08.log -0.0394718187
## D.npnct13.log                     D.npnct13.log -0.0349433657
## D.T.screen                           D.T.screen  0.0312904991
## D.T.new                                 D.T.new -0.0302966020
## D.sum.TfIdf                         D.sum.TfIdf -0.0300344031
## D.npnct03.log                     D.npnct03.log  0.0258721719
## D.T.used                               D.T.used  0.0251046087
## D.npnct10.log                     D.npnct10.log -0.0240301079
## D.T.ipad                               D.T.ipad -0.0234288421
## .rnorm                                   .rnorm -0.0228561937
## D.npnct18.log                     D.npnct18.log -0.0214919447
## D.T.excellent                     D.T.excellent  0.0203632794
## D.npnct11.log                     D.npnct11.log -0.0188179512
## D.T.great                             D.T.great  0.0171206415
## D.ratio.sum.TfIdf.nwrds D.ratio.sum.TfIdf.nwrds  0.0119528209
## D.T.condition                     D.T.condition -0.0118174892
## D.P.mini                               D.P.mini -0.0111321924
## storage.fctr                       storage.fctr -0.0103459049
## D.T.use                                 D.T.use -0.0097129508
## D.P.air                                 D.P.air -0.0091681483
## D.T.scratch                         D.T.scratch -0.0091382072
## D.T.good                               D.T.good  0.0063426895
## D.npnct01.log                     D.npnct01.log  0.0042941114
## D.T.work                               D.T.work -0.0035586740
## D.T.veri                               D.T.veri -0.0001659508
## D.npnct02.log                     D.npnct02.log            NA
## D.npnct04.log                     D.npnct04.log            NA
## D.npnct07.log                     D.npnct07.log            NA
## D.npnct17.log                     D.npnct17.log            NA
## D.npnct19.log                     D.npnct19.log            NA
## D.npnct20.log                     D.npnct20.log            NA
## D.npnct21.log                     D.npnct21.log            NA
## D.npnct22.log                     D.npnct22.log            NA
## D.npnct23.log                     D.npnct23.log            NA
## D.npnct25.log                     D.npnct25.log            NA
## D.npnct26.log                     D.npnct26.log            NA
## D.npnct27.log                     D.npnct27.log            NA
## D.npnct29.log                     D.npnct29.log            NA
## D.npnct30.log                     D.npnct30.log            NA
## D.P.http                               D.P.http            NA
##                         exclude.as.feat    cor.y.abs
## sold                                  1 1.0000000000
## biddable                              0 0.5485860292
## startprice                            0 0.4556701005
## UniqueID                              1 0.1904241613
## condition.fctr                        0 0.1531007887
## D.npnct05.log                         0 0.1178427734
## prdline.my.fctr                       0 0.0898840683
## D.npnct14.log                         0 0.0784119354
## cellular.fctr                         0 0.0770423554
## carrier.fctr                          0 0.0677807650
## D.ndgts.log                           0 0.0633901377
## D.nwrds.unq.log                       0 0.0619524203
## D.npnct09.log                         0 0.0617301891
## D.nwrds.log                           0 0.0587159086
## D.nchrs.log                           0 0.0565815056
## D.ratio.nstopwrds.nwrds               0 0.0554866401
## D.nuppr.log                           0 0.0554742013
## D.npnct12.log                         0 0.0549329137
## D.T.like                              0 0.0533938586
## D.npnct28.log                         0 0.0523776347
## D.T.box                               0 0.0515596986
## D.npnct06.log                         0 0.0497583609
## D.T.this                              0 0.0477867670
## D.npnct24.log                         0 0.0459503420
## D.npnct16.log                         0 0.0447153098
## D.nstopwrds.log                       0 0.0439279746
## color.fctr                            0 0.0426291775
## D.T.condit                            0 0.0415362400
## D.npnct15.log                         0 0.0400921803
## D.npnct08.log                         0 0.0394718187
## D.npnct13.log                         0 0.0349433657
## D.T.screen                            0 0.0312904991
## D.T.new                               0 0.0302966020
## D.sum.TfIdf                           0 0.0300344031
## D.npnct03.log                         0 0.0258721719
## D.T.used                              0 0.0251046087
## D.npnct10.log                         0 0.0240301079
## D.T.ipad                              0 0.0234288421
## .rnorm                                0 0.0228561937
## D.npnct18.log                         0 0.0214919447
## D.T.excellent                         0 0.0203632794
## D.npnct11.log                         0 0.0188179512
## D.T.great                             0 0.0171206415
## D.ratio.sum.TfIdf.nwrds               0 0.0119528209
## D.T.condition                         0 0.0118174892
## D.P.mini                              0 0.0111321924
## storage.fctr                          0 0.0103459049
## D.T.use                               0 0.0097129508
## D.P.air                               0 0.0091681483
## D.T.scratch                           0 0.0091382072
## D.T.good                              0 0.0063426895
## D.npnct01.log                         0 0.0042941114
## D.T.work                              0 0.0035586740
## D.T.veri                              0 0.0001659508
## D.npnct02.log                         0           NA
## D.npnct04.log                         0           NA
## D.npnct07.log                         0           NA
## D.npnct17.log                         0           NA
## D.npnct19.log                         0           NA
## D.npnct20.log                         0           NA
## D.npnct21.log                         0           NA
## D.npnct22.log                         0           NA
## D.npnct23.log                         0           NA
## D.npnct25.log                         0           NA
## D.npnct26.log                         0           NA
## D.npnct27.log                         0           NA
## D.npnct29.log                         0           NA
## D.npnct30.log                         0           NA
## D.P.http                              0           NA
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(D.nchrs.log, D.nuppr.log)=0.9995"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0566"
## [1] "cor(sold.fctr, D.nuppr.log)=-0.0555"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nuppr.log as highly correlated with
## D.nchrs.log
```

```
## [1] "cor(D.nchrs.log, D.nwrds.log)=0.9931"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0566"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0587"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nchrs.log as highly correlated with
## D.nwrds.log
```

```
## [1] "cor(D.nwrds.log, D.nwrds.unq.log)=0.9892"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0587"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0620"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.npnct24.log, D.ratio.nstopwrds.nwrds)=-0.9641"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0460"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0555"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct24.log as highly correlated with
## D.ratio.nstopwrds.nwrds
```

```
## [1] "cor(D.npnct06.log, D.npnct16.log)=0.9556"
## [1] "cor(sold.fctr, D.npnct06.log)=-0.0498"
## [1] "cor(sold.fctr, D.npnct16.log)=-0.0447"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct16.log as highly correlated with
## D.npnct06.log
```

```
## [1] "cor(D.nwrds.unq.log, D.sum.TfIdf)=0.9437"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0620"
## [1] "cor(sold.fctr, D.sum.TfIdf)=-0.0300"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.sum.TfIdf as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.unq.log, D.ratio.nstopwrds.nwrds)=-0.9223"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0620"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0555"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.ratio.nstopwrds.nwrds as highly correlated
## with D.nwrds.unq.log
```

```
## [1] "cor(D.nstopwrds.log, D.nwrds.unq.log)=0.8970"
## [1] "cor(sold.fctr, D.nstopwrds.log)=-0.0439"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0620"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nstopwrds.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(carrier.fctr, cellular.fctr)=0.8345"
## [1] "cor(sold.fctr, carrier.fctr)=-0.0678"
## [1] "cor(sold.fctr, cellular.fctr)=-0.0770"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified carrier.fctr as highly correlated with
## cellular.fctr
```

```
## [1] "cor(D.npnct13.log, D.nwrds.unq.log)=0.7214"
## [1] "cor(sold.fctr, D.npnct13.log)=-0.0349"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0620"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct13.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.T.like, D.T.new)=0.7047"
## [1] "cor(sold.fctr, D.T.like)=-0.0534"
## [1] "cor(sold.fctr, D.T.new)=-0.0303"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.T.new as highly correlated with D.T.like
```

```
##                         id         cor.y exclude.as.feat    cor.y.abs
## 66                    sold  1.0000000000               1 1.0000000000
## 2                 biddable  0.5485860292               0 0.5485860292
## 46 D.ratio.nstopwrds.nwrds  0.0554866401               0 0.0554866401
## 23           D.npnct15.log  0.0400921803               0 0.0400921803
## 59              D.T.screen  0.0312904991               0 0.0312904991
## 11           D.npnct03.log  0.0258721719               0 0.0258721719
## 62                D.T.used  0.0251046087               0 0.0251046087
## 52           D.T.excellent  0.0203632794               0 0.0203632794
## 54               D.T.great  0.0171206415               0 0.0171206415
## 47 D.ratio.sum.TfIdf.nwrds  0.0119528209               0 0.0119528209
## 53                D.T.good  0.0063426895               0 0.0063426895
## 9            D.npnct01.log  0.0042941114               0 0.0042941114
## 63                D.T.veri -0.0001659508               0 0.0001659508
## 64                D.T.work -0.0035586740               0 0.0035586740
## 58             D.T.scratch -0.0091382072               0 0.0091382072
## 43                 D.P.air -0.0091681483               0 0.0091681483
## 61                 D.T.use -0.0097129508               0 0.0097129508
## 68            storage.fctr -0.0103459049               0 0.0103459049
## 45                D.P.mini -0.0111321924               0 0.0111321924
## 51           D.T.condition -0.0118174892               0 0.0118174892
## 19           D.npnct11.log -0.0188179512               0 0.0188179512
## 26           D.npnct18.log -0.0214919447               0 0.0214919447
## 1                   .rnorm -0.0228561937               0 0.0228561937
## 55                D.T.ipad -0.0234288421               0 0.0234288421
## 18           D.npnct10.log -0.0240301079               0 0.0240301079
## 48             D.sum.TfIdf -0.0300344031               0 0.0300344031
## 57                 D.T.new -0.0302966020               0 0.0302966020
## 21           D.npnct13.log -0.0349433657               0 0.0349433657
## 16           D.npnct08.log -0.0394718187               0 0.0394718187
## 50              D.T.condit -0.0415362400               0 0.0415362400
## 5               color.fctr -0.0426291775               0 0.0426291775
## 39         D.nstopwrds.log -0.0439279746               0 0.0439279746
## 24           D.npnct16.log -0.0447153098               0 0.0447153098
## 32           D.npnct24.log -0.0459503420               0 0.0459503420
## 60                D.T.this -0.0477867670               0 0.0477867670
## 14           D.npnct06.log -0.0497583609               0 0.0497583609
## 49                 D.T.box -0.0515596986               0 0.0515596986
## 36           D.npnct28.log -0.0523776347               0 0.0523776347
## 56                D.T.like -0.0533938586               0 0.0533938586
## 20           D.npnct12.log -0.0549329137               0 0.0549329137
## 40             D.nuppr.log -0.0554742013               0 0.0554742013
## 7              D.nchrs.log -0.0565815056               0 0.0565815056
## 41             D.nwrds.log -0.0587159086               0 0.0587159086
## 17           D.npnct09.log -0.0617301891               0 0.0617301891
## 42         D.nwrds.unq.log -0.0619524203               0 0.0619524203
## 8              D.ndgts.log -0.0633901377               0 0.0633901377
## 3             carrier.fctr -0.0677807650               0 0.0677807650
## 4            cellular.fctr -0.0770423554               0 0.0770423554
## 22           D.npnct14.log -0.0784119354               0 0.0784119354
## 65         prdline.my.fctr -0.0898840683               0 0.0898840683
## 13           D.npnct05.log -0.1178427734               0 0.1178427734
## 6           condition.fctr -0.1531007887               0 0.1531007887
## 69                UniqueID -0.1904241613               1 0.1904241613
## 67              startprice -0.4556701005               0 0.4556701005
## 10           D.npnct02.log            NA               0           NA
## 12           D.npnct04.log            NA               0           NA
## 15           D.npnct07.log            NA               0           NA
## 25           D.npnct17.log            NA               0           NA
## 27           D.npnct19.log            NA               0           NA
## 28           D.npnct20.log            NA               0           NA
## 29           D.npnct21.log            NA               0           NA
## 30           D.npnct22.log            NA               0           NA
## 31           D.npnct23.log            NA               0           NA
## 33           D.npnct25.log            NA               0           NA
## 34           D.npnct26.log            NA               0           NA
## 35           D.npnct27.log            NA               0           NA
## 37           D.npnct29.log            NA               0           NA
## 38           D.npnct30.log            NA               0           NA
## 44                D.P.http            NA               0           NA
##                 cor.high.X   freqRatio percentUnique zeroVar   nzv
## 66                    <NA>    1.163953    0.10746910   FALSE FALSE
## 2                     <NA>    1.223417    0.10746910   FALSE FALSE
## 46         D.nwrds.unq.log   13.387500    4.67490596   FALSE FALSE
## 23                    <NA>  153.500000    0.16120365   FALSE  TRUE
## 59                    <NA>   45.105263    0.80601827   FALSE  TRUE
## 11                    <NA>   83.318182    0.16120365   FALSE  TRUE
## 62                    <NA>  161.818182    0.85975282   FALSE  TRUE
## 52                    <NA>  227.875000    0.80601827   FALSE  TRUE
## 54                    <NA>  127.214286    0.75228372   FALSE  TRUE
## 47                    <NA>   63.294118   35.14239656   FALSE FALSE
## 53                    <NA>   39.181818    1.02095648   FALSE  TRUE
## 9                     <NA>   53.029412    0.32240731   FALSE  TRUE
## 63                    <NA>  163.090909    0.85975282   FALSE  TRUE
## 64                    <NA>   82.761905    0.91348737   FALSE  TRUE
## 58                    <NA>   35.416667    0.80601827   FALSE  TRUE
## 43                    <NA>  123.000000    0.16120365   FALSE  TRUE
## 61                    <NA>   57.533333    0.96722192   FALSE  TRUE
## 68                    <NA>    2.739003    0.26867276   FALSE FALSE
## 45                    <NA>   92.000000    0.16120365   FALSE  TRUE
## 51                    <NA>   52.375000    0.80601827   FALSE  TRUE
## 19                    <NA>    9.385965    0.37614186   FALSE FALSE
## 26                    <NA> 1860.000000    0.10746910   FALSE  TRUE
## 1                     <NA>    1.000000  100.00000000   FALSE FALSE
## 55                    <NA>   51.484848    0.91348737   FALSE  TRUE
## 18                    <NA>  309.000000    0.16120365   FALSE  TRUE
## 48         D.nwrds.unq.log   63.294118   34.87372380   FALSE FALSE
## 57                D.T.like  147.166667    0.91348737   FALSE  TRUE
## 21         D.nwrds.unq.log    5.210728    0.48361096   FALSE FALSE
## 16                    <NA>   69.653846    0.21493821   FALSE  TRUE
## 50                    <NA>   52.000000    0.85975282   FALSE  TRUE
## 5                     <NA>    1.576837    0.26867276   FALSE FALSE
## 39         D.nwrds.unq.log   14.144578    0.80601827   FALSE FALSE
## 24           D.npnct06.log   31.280702    0.16120365   FALSE  TRUE
## 32 D.ratio.nstopwrds.nwrds    1.355696    0.10746910   FALSE FALSE
## 60                    <NA>   40.744186    0.69854917   FALSE  TRUE
## 14                    <NA>   33.773585    0.16120365   FALSE  TRUE
## 49                    <NA>  111.875000    0.85975282   FALSE  TRUE
## 36                    <NA>  463.750000    0.16120365   FALSE  TRUE
## 56                    <NA>  259.142857    0.85975282   FALSE  TRUE
## 20                    <NA>   27.261538    0.21493821   FALSE  TRUE
## 40             D.nchrs.log   16.507692    4.35249866   FALSE FALSE
## 7              D.nwrds.log   14.875000    5.64212789   FALSE FALSE
## 41         D.nwrds.unq.log   12.310345    1.39709833   FALSE FALSE
## 17                    <NA>  308.666667    0.21493821   FALSE  TRUE
## 42                    <NA>    7.685714    0.91348737   FALSE FALSE
## 8                     <NA>   27.063492    0.69854917   FALSE  TRUE
## 3            cellular.fctr    3.192529    0.37614186   FALSE FALSE
## 4                     <NA>    2.128846    0.16120365   FALSE FALSE
## 22                    <NA>   35.372549    0.26867276   FALSE  TRUE
## 65                    <NA>    1.032258    0.64481462   FALSE FALSE
## 13                    <NA>   40.355556    0.10746910   FALSE  TRUE
## 6                     <NA>    4.006920    0.32240731   FALSE FALSE
## 69                    <NA>    1.000000  100.00000000   FALSE FALSE
## 67                    <NA>    2.807692   30.14508329   FALSE FALSE
## 10                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 12                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 15                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 25                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 27                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 28                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 29                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 30                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 31                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 33                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 34                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 35                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 37                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 38                    <NA>    0.000000    0.05373455    TRUE  TRUE
## 44                    <NA>    0.000000    0.05373455    TRUE  TRUE
##    myNearZV is.cor.y.abs.low
## 66    FALSE            FALSE
## 2     FALSE            FALSE
## 46    FALSE            FALSE
## 23    FALSE            FALSE
## 59    FALSE            FALSE
## 11    FALSE            FALSE
## 62    FALSE            FALSE
## 52    FALSE             TRUE
## 54    FALSE             TRUE
## 47    FALSE             TRUE
## 53    FALSE             TRUE
## 9     FALSE             TRUE
## 63    FALSE             TRUE
## 64    FALSE             TRUE
## 58    FALSE             TRUE
## 43    FALSE             TRUE
## 61    FALSE             TRUE
## 68    FALSE             TRUE
## 45    FALSE             TRUE
## 51    FALSE             TRUE
## 19    FALSE             TRUE
## 26     TRUE             TRUE
## 1     FALSE            FALSE
## 55    FALSE            FALSE
## 18    FALSE            FALSE
## 48    FALSE            FALSE
## 57    FALSE            FALSE
## 21    FALSE            FALSE
## 16    FALSE            FALSE
## 50    FALSE            FALSE
## 5     FALSE            FALSE
## 39    FALSE            FALSE
## 24    FALSE            FALSE
## 32    FALSE            FALSE
## 60    FALSE            FALSE
## 14    FALSE            FALSE
## 49    FALSE            FALSE
## 36    FALSE            FALSE
## 56    FALSE            FALSE
## 20    FALSE            FALSE
## 40    FALSE            FALSE
## 7     FALSE            FALSE
## 41    FALSE            FALSE
## 17    FALSE            FALSE
## 42    FALSE            FALSE
## 8     FALSE            FALSE
## 3     FALSE            FALSE
## 4     FALSE            FALSE
## 22    FALSE            FALSE
## 65    FALSE            FALSE
## 13    FALSE            FALSE
## 6     FALSE            FALSE
## 69    FALSE            FALSE
## 67    FALSE            FALSE
## 10     TRUE               NA
## 12     TRUE               NA
## 15     TRUE               NA
## 25     TRUE               NA
## 27     TRUE               NA
## 28     TRUE               NA
## 29     TRUE               NA
## 30     TRUE               NA
## 31     TRUE               NA
## 33     TRUE               NA
## 34     TRUE               NA
## 35     TRUE               NA
## 37     TRUE               NA
## 38     TRUE               NA
## 44     TRUE               NA
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning: Removed 5 rows containing missing values (geom_point).
```

```
## Warning: Removed 5 rows containing missing values (geom_point).
```

```
## Warning: Removed 5 rows containing missing values (geom_point).
```

![](ebayipads_oobsmpl_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##               id       cor.y exclude.as.feat  cor.y.abs cor.high.X
## 26 D.npnct18.log -0.02149194               0 0.02149194       <NA>
## 10 D.npnct02.log          NA               0         NA       <NA>
## 12 D.npnct04.log          NA               0         NA       <NA>
## 15 D.npnct07.log          NA               0         NA       <NA>
## 25 D.npnct17.log          NA               0         NA       <NA>
## 27 D.npnct19.log          NA               0         NA       <NA>
## 28 D.npnct20.log          NA               0         NA       <NA>
## 29 D.npnct21.log          NA               0         NA       <NA>
## 30 D.npnct22.log          NA               0         NA       <NA>
## 31 D.npnct23.log          NA               0         NA       <NA>
## 33 D.npnct25.log          NA               0         NA       <NA>
## 34 D.npnct26.log          NA               0         NA       <NA>
## 35 D.npnct27.log          NA               0         NA       <NA>
## 37 D.npnct29.log          NA               0         NA       <NA>
## 38 D.npnct30.log          NA               0         NA       <NA>
## 44      D.P.http          NA               0         NA       <NA>
##    freqRatio percentUnique zeroVar  nzv myNearZV is.cor.y.abs.low
## 26      1860    0.10746910   FALSE TRUE     TRUE             TRUE
## 10         0    0.05373455    TRUE TRUE     TRUE               NA
## 12         0    0.05373455    TRUE TRUE     TRUE               NA
## 15         0    0.05373455    TRUE TRUE     TRUE               NA
## 25         0    0.05373455    TRUE TRUE     TRUE               NA
## 27         0    0.05373455    TRUE TRUE     TRUE               NA
## 28         0    0.05373455    TRUE TRUE     TRUE               NA
## 29         0    0.05373455    TRUE TRUE     TRUE               NA
## 30         0    0.05373455    TRUE TRUE     TRUE               NA
## 31         0    0.05373455    TRUE TRUE     TRUE               NA
## 33         0    0.05373455    TRUE TRUE     TRUE               NA
## 34         0    0.05373455    TRUE TRUE     TRUE               NA
## 35         0    0.05373455    TRUE TRUE     TRUE               NA
## 37         0    0.05373455    TRUE TRUE     TRUE               NA
## 38         0    0.05373455    TRUE TRUE     TRUE               NA
## 44         0    0.05373455    TRUE TRUE     TRUE               NA
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
##      sold sold.fctr 
##       798       798 
## [1] "numeric data w/ 0s in : "
##                biddable                    sold           cellular.fctr 
##                    1446                    1001                    1595 
##              D.T.condit           D.T.condition                D.T.good 
##                    2455                    2385                    2463 
##                D.T.ipad                 D.T.new             D.T.scratch 
##                    2432                    2520                    2431 
##              D.T.screen                D.T.this                 D.T.use 
##                    2453                    2503                    2474 
##                D.T.work                 D.T.box           D.T.excellent 
##                    2485                    2553                    2602 
##               D.T.great                D.T.like                D.T.used 
##                    2539                    2591                    2540 
##                D.T.veri             D.nwrds.log         D.nwrds.unq.log 
##                    2567                    1521                    1529 
##             D.sum.TfIdf D.ratio.sum.TfIdf.nwrds             D.nchrs.log 
##                    1529                    1529                    1521 
##             D.nuppr.log             D.ndgts.log           D.npnct01.log 
##                    1523                    2428                    2581 
##           D.npnct03.log           D.npnct05.log           D.npnct06.log 
##                    2616                    2594                    2556 
##           D.npnct08.log           D.npnct09.log           D.npnct10.log 
##                    2583                    2643                    2650 
##           D.npnct11.log           D.npnct12.log           D.npnct13.log 
##                    2303                    2539                    1934 
##           D.npnct14.log           D.npnct15.log           D.npnct16.log 
##                    2584                    2638                    2548 
##           D.npnct24.log           D.npnct28.log         D.nstopwrds.log 
##                    1521                    2651                    1669 
##                D.P.mini                 D.P.air 
##                    2625                    2639 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1521           0           0           0           0           0 
## productline  prdline.my 
##           0           0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 38.973 40.432    1.46
## 9 partition.data.training          6          0 40.433     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    
    set.seed(glb_split_sample.seed)
    
    OOB_size <- nrow(glb_newobs_df) * 1.1
    if (is.null(glb_category_vars)) {
        require(caTools)
        split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                              SplitRatio=OOB_size / nrow(glb_trnobs_df))
        glb_OOBobs_df <- glb_trnobs_df[split ,]            
        glb_fitobs_df <- glb_trnobs_df[!split, ] 
    } else {
        sample_vars <- c(glb_rsp_var_raw, glb_category_vars)
        rspvar_freq_df <- orderBy(reformulate(glb_rsp_var_raw), 
                                  mycreate_sqlxtab_df(glb_trnobs_df, glb_rsp_var_raw))
        OOB_rspvar_size <- 1.0 * OOB_size * rspvar_freq_df$.n / sum(rspvar_freq_df$.n) 
        newobs_freq_df <- orderBy(reformulate(glb_category_vars),
                                  mycreate_sqlxtab_df(glb_newobs_df, glb_category_vars))
        trnobs_freq_df <- orderBy(reformulate(glb_category_vars),
                                  mycreate_sqlxtab_df(glb_trnobs_df, glb_category_vars))
        allobs_freq_df <- merge(newobs_freq_df, trnobs_freq_df, by=glb_category_vars,
                                all=TRUE, sort=TRUE, suffixes=c(".Tst", ".Train"))
        allobs_freq_df[is.na(allobs_freq_df)] <- 0
        OOB_strata_size <- ceiling(
            as.vector(matrix(allobs_freq_df$.n.Tst * 1.0 / sum(allobs_freq_df$.n.Tst)) %*%
                      matrix(OOB_rspvar_size, nrow=1)))
        OOB_strata_size[OOB_strata_size == 0] <- 1
        OOB_strata_df <- expand.grid(glb_rsp_var_raw=rspvar_freq_df[, glb_rsp_var_raw],
                                     glb_category_vars=allobs_freq_df[, glb_category_vars])
        names(OOB_strata_df) <- sample_vars
        OOB_strata_df <- orderBy(reformulate(sample_vars), OOB_strata_df)
        
        trnobs_univ_df <- orderBy(reformulate(sample_vars),
                                       mycreate_sqlxtab_df(glb_trnobs_df, sample_vars))
        trnobs_univ_df <- merge(trnobs_univ_df, OOB_strata_df, all=TRUE)
        tmp_trnobs_df <- orderBy(reformulate(c(glb_rsp_var_raw, glb_category_vars)),
                                glb_trnobs_df)
        require(sampling)
        split_strata <- strata(tmp_trnobs_df, 
                               stratanames=c(glb_rsp_var_raw, glb_category_vars),
                               size=OOB_strata_size[!is.na(trnobs_univ_df$.n)],
                               method="srswor")
        glb_OOBobs_df <- getdata(tmp_trnobs_df, split_strata)[, names(glb_trnobs_df)]
        glb_fitobs_df <- glb_trnobs_df[!glb_trnobs_df[, glb_id_var] %in% 
                                        glb_OOBobs_df[, glb_id_var], ]
    }
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## Loading required package: sampling
## 
## Attaching package: 'sampling'
## 
## The following objects are masked from 'package:survival':
## 
##     cluster, strata
## 
## The following object is masked from 'package:caret':
## 
##     cluster
```

```r
if (!is.null(glb_max_fitobs) && (nrow(glb_fitobs_df) > glb_max_fitobs)) {
    warning("glb_fitobs_df restricted to glb_max_fitobs: ", 
            format(glb_max_fitobs, big.mark=","))
    org_fitobs_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitobs_df[split <- sample.split(org_fitobs_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitobs), ]
    org_fitobs_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}
```

```
##     sold.0 sold.1 sold.NA
##         NA     NA     798
## Fit    522    447      NA
## OOB    479    413      NA
##        sold.0    sold.1 sold.NA
##            NA        NA       1
## Fit 0.5386997 0.4613003      NA
## OOB 0.5369955 0.4630045      NA
##          prdline.my .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
## 2            iPad 2    154    171     0.19298246    0.191704036
## 8         iPad mini    114    126     0.14285714    0.141255605
## 1            iPad 1     89     99     0.11152882    0.110986547
## 12          Unknown     87     97     0.10902256    0.108744395
## 6          iPad Air     75     84     0.09398496    0.094170404
## 4            iPad 4     68     76     0.08521303    0.085201794
## 7        iPad Air 2     62     69     0.07769424    0.077354260
## 9       iPad mini 2     56     63     0.07017544    0.070627803
## 3            iPad 3     55     61     0.06892231    0.068385650
## 10      iPad mini 3     38     43     0.04761905    0.048206278
## 11 iPad mini Retina     NA      2             NA    0.002242152
## 5            iPad 5     NA      1             NA    0.001121076
```

```r
# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 69 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                  id exclude.as.feat rsp_var
## sold.fctr sold.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                  id      cor.y exclude.as.feat cor.y.abs cor.high.X
## 66             sold  1.0000000            TRUE 1.0000000       <NA>
## 69         UniqueID -0.1904242            TRUE 0.1904242       <NA>
## sold.fctr sold.fctr         NA            TRUE        NA       <NA>
##           freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 66         1.163953     0.1074691   FALSE FALSE    FALSE            FALSE
## 69         1.000000   100.0000000   FALSE FALSE    FALSE            FALSE
## sold.fctr        NA            NA      NA    NA       NA               NA
##           interaction.feat rsp_var_raw id_var rsp_var
## 66                      NA        TRUE     NA      NA
## 69                      NA       FALSE   TRUE      NA
## sold.fctr               NA          NA     NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
##  [1] "D.npnct18.log" "D.npnct02.log" "D.npnct04.log" "D.npnct07.log"
##  [5] "D.npnct17.log" "D.npnct19.log" "D.npnct20.log" "D.npnct21.log"
##  [9] "D.npnct22.log" "D.npnct23.log" "D.npnct25.log" "D.npnct26.log"
## [13] "D.npnct27.log" "D.npnct29.log" "D.npnct30.log" "D.P.http"
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 2659   64
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 1861   63
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 969  63
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 892  63
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 798  63
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)
```

```
## Warning in rm(split): object 'split' not found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 40.433 41.384   0.951
## 10              fit.models          7          0 41.384     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[glb_feats_df$id == max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_feats_df$id == glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a higher correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl(model_id="Baseline", 
                         model_method="mybaseln_classfr",
                        indep_vars_vctr=glb_Baseline_mdl_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.5386997 0.4613003 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.5386997 0.4613003
## 2 0.5386997 0.4613003
## 3 0.5386997 0.4613003
## 4 0.5386997 0.4613003
## 5 0.5386997 0.4613003
## 6 0.5386997 0.4613003
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.MFO.myMFO_classfr.N
## 1         N                                   522
## 2         Y                                   447
##          Prediction
## Reference   N   Y
##         N 522   0
##         Y 447   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.386997e-01   0.000000e+00   5.067192e-01   5.704443e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   5.131824e-01   8.826336e-99 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.5386997 0.4613003
## 2 0.5386997 0.4613003
## 3 0.5386997 0.4613003
## 4 0.5386997 0.4613003
## 5 0.5386997 0.4613003
## 6 0.5386997 0.4613003
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.MFO.myMFO_classfr.N
## 1         N                                   479
## 2         Y                                   413
##          Prediction
## Reference   N   Y
##         N 479   0
##         Y 413   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.369955e-01   0.000000e+00   5.036362e-01   5.701097e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   5.137205e-01   2.217817e-91 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.387                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.5386997
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.5067192             0.5704443             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.5369955
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5036362             0.5701097             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.6313559
## 3        0.2 0.6313559
## 4        0.3 0.6313559
## 5        0.4 0.6313559
## 6        0.5 0.4523282
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Random.myrandom_classfr.Y
## 1         N                                         522
## 2         Y                                         447
##          Prediction
## Reference   N   Y
##         N   0 522
##         Y   0 447
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   4.613003e-01   0.000000e+00   4.295557e-01   4.932808e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   9.999994e-01  4.227904e-115 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6329502
## 3        0.2 0.6329502
## 4        0.3 0.6329502
## 5        0.4 0.6329502
## 6        0.5 0.4830097
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Random.myrandom_classfr.Y
## 1         N                                         479
## 2         Y                                         413
##          Prediction
## Reference   N   Y
##         N   0 479
##         Y   0 413
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   4.630045e-01   0.000000e+00   4.298903e-01   4.963638e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   9.999959e-01  9.595926e-106 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.252                 0.001   0.4877665
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.4       0.6313559        0.4613003
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.4295557             0.4932808             0   0.5196257
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6329502        0.4630045
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.4298903             0.4963638             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: biddable, startprice"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.526 on full training set
```

```
## Loading required package: rpart.plot
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 969 
## 
##          CP nsplit rel error
## 1 0.5257271      0         1
## 
## Node number 1: 969 observations
##   predicted class=N  expected loss=0.4613003  P(node) =1
##     class counts:   522   447
##    probabilities: 0.539 0.461 
## 
## n= 969 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 969 447 N (0.5386997 0.4613003) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1         N                                      522
## 2         Y                                      447
##          Prediction
## Reference   N   Y
##         N 522   0
##         Y 447   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.386997e-01   0.000000e+00   5.067192e-01   5.704443e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   5.131824e-01   8.826336e-99 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1         N                                      479
## 2         Y                                      413
##          Prediction
## Reference   N   Y
##         N 479   0
##         Y 413   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.369955e-01   0.000000e+00   5.036362e-01   5.701097e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   5.137205e-01   2.217817e-91 
##               model_id model_method                feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart biddable, startprice               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.679                 0.011         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.5386997
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.5067192             0.5704443             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.5369955
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5036362             0.5701097             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: biddable, startprice"
## Fitting cp = 0 on full training set
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 969 
## 
##            CP nsplit rel error
## 1 0.525727069      0 1.0000000
## 2 0.017897092      1 0.4742729
## 3 0.007270694      3 0.4384787
## 4 0.002237136      7 0.4093960
## 5 0.000000000     18 0.3847875
## 
## Variable importance
##   biddable startprice 
##         51         49 
## 
## Node number 1: 969 observations,    complexity param=0.5257271
##   predicted class=N  expected loss=0.4613003  P(node) =1
##     class counts:   522   447
##    probabilities: 0.539 0.461 
##   left son=2 (530 obs) right son=3 (439 obs)
##   Primary splits:
##       biddable   < 0.5     to the left,  improve=150.6565, (0 missing)
##       startprice < 126.475 to the right, improve=127.7985, (0 missing)
##   Surrogate splits:
##       startprice < 152.995 to the right, agree=0.76, adj=0.469, (0 split)
## 
## Node number 2: 530 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.2075472  P(node) =0.5469556
##     class counts:   420   110
##    probabilities: 0.792 0.208 
##   left son=4 (265 obs) right son=5 (265 obs)
##   Primary splits:
##       startprice < 261.995 to the right, improve=6.656604, (0 missing)
## 
## Node number 3: 439 observations,    complexity param=0.01789709
##   predicted class=Y  expected loss=0.2323462  P(node) =0.4530444
##     class counts:   102   337
##    probabilities: 0.232 0.768 
##   left son=6 (154 obs) right son=7 (285 obs)
##   Primary splits:
##       startprice < 127.5   to the right, improve=39.11474, (0 missing)
## 
## Node number 4: 265 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.1283019  P(node) =0.2734778
##     class counts:   231    34
##    probabilities: 0.872 0.128 
##   left son=8 (18 obs) right son=9 (247 obs)
##   Primary splits:
##       startprice < 279.995 to the left,  improve=0.6357956, (0 missing)
## 
## Node number 5: 265 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.2867925  P(node) =0.2734778
##     class counts:   189    76
##    probabilities: 0.713 0.287 
##   left son=10 (216 obs) right son=11 (49 obs)
##   Primary splits:
##       startprice < 91      to the right, improve=2.416806, (0 missing)
## 
## Node number 6: 154 observations,    complexity param=0.01789709
##   predicted class=N  expected loss=0.4805195  P(node) =0.1589267
##     class counts:    80    74
##    probabilities: 0.519 0.481 
##   left son=12 (24 obs) right son=13 (130 obs)
##   Primary splits:
##       startprice < 414.995 to the right, improve=5.601066, (0 missing)
## 
## Node number 7: 285 observations
##   predicted class=Y  expected loss=0.07719298  P(node) =0.2941176
##     class counts:    22   263
##    probabilities: 0.077 0.923 
## 
## Node number 8: 18 observations
##   predicted class=N  expected loss=0  P(node) =0.01857585
##     class counts:    18     0
##    probabilities: 1.000 0.000 
## 
## Node number 9: 247 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.1376518  P(node) =0.254902
##     class counts:   213    34
##    probabilities: 0.862 0.138 
##   left son=18 (196 obs) right son=19 (51 obs)
##   Primary splits:
##       startprice < 319.995 to the right, improve=1.22551, (0 missing)
## 
## Node number 10: 216 observations
##   predicted class=N  expected loss=0.2546296  P(node) =0.2229102
##     class counts:   161    55
##    probabilities: 0.745 0.255 
## 
## Node number 11: 49 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.4285714  P(node) =0.0505676
##     class counts:    28    21
##    probabilities: 0.571 0.429 
##   left son=22 (9 obs) right son=23 (40 obs)
##   Primary splits:
##       startprice < 29.37   to the left,  improve=0.9388889, (0 missing)
## 
## Node number 12: 24 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.0247678
##     class counts:    20     4
##    probabilities: 0.833 0.167 
## 
## Node number 13: 130 observations,    complexity param=0.007270694
##   predicted class=Y  expected loss=0.4615385  P(node) =0.1341589
##     class counts:    60    70
##    probabilities: 0.462 0.538 
##   left son=26 (123 obs) right son=27 (7 obs)
##   Primary splits:
##       startprice < 397     to the left,  improve=0.4574288, (0 missing)
## 
## Node number 18: 196 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.1122449  P(node) =0.2022704
##     class counts:   174    22
##    probabilities: 0.888 0.112 
##   left son=36 (87 obs) right son=37 (109 obs)
##   Primary splits:
##       startprice < 407.49  to the left,  improve=0.5860584, (0 missing)
## 
## Node number 19: 51 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.2352941  P(node) =0.05263158
##     class counts:    39    12
##    probabilities: 0.765 0.235 
##   left son=38 (44 obs) right son=39 (7 obs)
##   Primary splits:
##       startprice < 319.425 to the left,  improve=1.833461, (0 missing)
## 
## Node number 22: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.009287926
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 23: 40 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.475  P(node) =0.04127967
##     class counts:    21    19
##    probabilities: 0.525 0.475 
##   left son=46 (23 obs) right son=47 (17 obs)
##   Primary splits:
##       startprice < 77.5    to the right, improve=0.7581841, (0 missing)
## 
## Node number 26: 123 observations,    complexity param=0.007270694
##   predicted class=Y  expected loss=0.4715447  P(node) =0.126935
##     class counts:    58    65
##    probabilities: 0.472 0.528 
##   left son=52 (22 obs) right son=53 (101 obs)
##   Primary splits:
##       startprice < 299.995 to the right, improve=0.7634593, (0 missing)
## 
## Node number 27: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.007223942
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 36: 87 observations
##   predicted class=N  expected loss=0.06896552  P(node) =0.08978328
##     class counts:    81     6
##    probabilities: 0.931 0.069 
## 
## Node number 37: 109 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.146789  P(node) =0.1124871
##     class counts:    93    16
##    probabilities: 0.853 0.147 
##   left son=74 (102 obs) right son=75 (7 obs)
##   Primary splits:
##       startprice < 424.995 to the right, improve=7.549251, (0 missing)
## 
## Node number 38: 44 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.04540764
##     class counts:    36     8
##    probabilities: 0.818 0.182 
## 
## Node number 39: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.007223942
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 46: 23 observations
##   predicted class=N  expected loss=0.3913043  P(node) =0.02373581
##     class counts:    14     9
##    probabilities: 0.609 0.391 
## 
## Node number 47: 17 observations
##   predicted class=Y  expected loss=0.4117647  P(node) =0.01754386
##     class counts:     7    10
##    probabilities: 0.412 0.588 
## 
## Node number 52: 22 observations
##   predicted class=N  expected loss=0.4090909  P(node) =0.02270382
##     class counts:    13     9
##    probabilities: 0.591 0.409 
## 
## Node number 53: 101 observations,    complexity param=0.007270694
##   predicted class=Y  expected loss=0.4455446  P(node) =0.1042312
##     class counts:    45    56
##    probabilities: 0.446 0.554 
##   left son=106 (70 obs) right son=107 (31 obs)
##   Primary splits:
##       startprice < 244.98  to the left,  improve=1.352603, (0 missing)
## 
## Node number 74: 102 observations
##   predicted class=N  expected loss=0.09803922  P(node) =0.1052632
##     class counts:    92    10
##    probabilities: 0.902 0.098 
## 
## Node number 75: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.007223942
##     class counts:     1     6
##    probabilities: 0.143 0.857 
## 
## Node number 106: 70 observations,    complexity param=0.007270694
##   predicted class=N  expected loss=0.5  P(node) =0.07223942
##     class counts:    35    35
##    probabilities: 0.500 0.500 
##   left son=212 (15 obs) right son=213 (55 obs)
##   Primary splits:
##       startprice < 199.995 to the right, improve=3.436364, (0 missing)
## 
## Node number 107: 31 observations
##   predicted class=Y  expected loss=0.3225806  P(node) =0.03199174
##     class counts:    10    21
##    probabilities: 0.323 0.677 
## 
## Node number 212: 15 observations
##   predicted class=N  expected loss=0.2  P(node) =0.01547988
##     class counts:    12     3
##    probabilities: 0.800 0.200 
## 
## Node number 213: 55 observations,    complexity param=0.002237136
##   predicted class=Y  expected loss=0.4181818  P(node) =0.05675955
##     class counts:    23    32
##    probabilities: 0.418 0.582 
##   left son=426 (31 obs) right son=427 (24 obs)
##   Primary splits:
##       startprice < 159.995 to the left,  improve=1.363099, (0 missing)
## 
## Node number 426: 31 observations,    complexity param=0.002237136
##   predicted class=N  expected loss=0.483871  P(node) =0.03199174
##     class counts:    16    15
##    probabilities: 0.516 0.484 
##   left son=852 (8 obs) right son=853 (23 obs)
##   Primary splits:
##       startprice < 149.995 to the left,  improve=0.2556101, (0 missing)
## 
## Node number 427: 24 observations
##   predicted class=Y  expected loss=0.2916667  P(node) =0.0247678
##     class counts:     7    17
##    probabilities: 0.292 0.708 
## 
## Node number 852: 8 observations
##   predicted class=N  expected loss=0.375  P(node) =0.008255934
##     class counts:     5     3
##    probabilities: 0.625 0.375 
## 
## Node number 853: 23 observations
##   predicted class=Y  expected loss=0.4782609  P(node) =0.02373581
##     class counts:    11    12
##    probabilities: 0.478 0.522 
## 
## n= 969 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##   1) root 969 447 N (0.53869969 0.46130031)  
##     2) biddable< 0.5 530 110 N (0.79245283 0.20754717)  
##       4) startprice>=261.995 265  34 N (0.87169811 0.12830189)  
##         8) startprice< 279.995 18   0 N (1.00000000 0.00000000) *
##         9) startprice>=279.995 247  34 N (0.86234818 0.13765182)  
##          18) startprice>=319.995 196  22 N (0.88775510 0.11224490)  
##            36) startprice< 407.49 87   6 N (0.93103448 0.06896552) *
##            37) startprice>=407.49 109  16 N (0.85321101 0.14678899)  
##              74) startprice>=424.995 102  10 N (0.90196078 0.09803922) *
##              75) startprice< 424.995 7   1 Y (0.14285714 0.85714286) *
##          19) startprice< 319.995 51  12 N (0.76470588 0.23529412)  
##            38) startprice< 319.425 44   8 N (0.81818182 0.18181818) *
##            39) startprice>=319.425 7   3 Y (0.42857143 0.57142857) *
##       5) startprice< 261.995 265  76 N (0.71320755 0.28679245)  
##        10) startprice>=91 216  55 N (0.74537037 0.25462963) *
##        11) startprice< 91 49  21 N (0.57142857 0.42857143)  
##          22) startprice< 29.37 9   2 N (0.77777778 0.22222222) *
##          23) startprice>=29.37 40  19 N (0.52500000 0.47500000)  
##            46) startprice>=77.5 23   9 N (0.60869565 0.39130435) *
##            47) startprice< 77.5 17   7 Y (0.41176471 0.58823529) *
##     3) biddable>=0.5 439 102 Y (0.23234624 0.76765376)  
##       6) startprice>=127.5 154  74 N (0.51948052 0.48051948)  
##        12) startprice>=414.995 24   4 N (0.83333333 0.16666667) *
##        13) startprice< 414.995 130  60 Y (0.46153846 0.53846154)  
##          26) startprice< 397 123  58 Y (0.47154472 0.52845528)  
##            52) startprice>=299.995 22   9 N (0.59090909 0.40909091) *
##            53) startprice< 299.995 101  45 Y (0.44554455 0.55445545)  
##             106) startprice< 244.98 70  35 N (0.50000000 0.50000000)  
##               212) startprice>=199.995 15   3 N (0.80000000 0.20000000) *
##               213) startprice< 199.995 55  23 Y (0.41818182 0.58181818)  
##                 426) startprice< 159.995 31  15 N (0.51612903 0.48387097)  
##                   852) startprice< 149.995 8   3 N (0.62500000 0.37500000) *
##                   853) startprice>=149.995 23  11 Y (0.47826087 0.52173913) *
##                 427) startprice>=159.995 24   7 Y (0.29166667 0.70833333) *
##             107) startprice>=244.98 31  10 Y (0.32258065 0.67741935) *
##          27) startprice>=397 7   2 Y (0.28571429 0.71428571) *
##       7) startprice< 127.5 285  22 Y (0.07719298 0.92280702) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.7129859
## 3        0.2 0.7344435
## 4        0.3 0.7968923
## 5        0.4 0.7977011
## 6        0.5 0.7971698
## 7        0.6 0.7790262
## 8        0.7 0.7558442
## 9        0.8 0.7280108
## 10       0.9 0.7185792
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           446
## 2         Y                                           100
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            76
## 2                                           347
##          Prediction
## Reference   N   Y
##         N 446  76
##         Y 100 347
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.183695e-01   6.331346e-01   7.926235e-01   8.421635e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   7.658838e-75   8.297306e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6731449
## 3        0.2 0.6814815
## 4        0.3 0.7294982
## 5        0.4 0.7111682
## 6        0.5 0.6973684
## 7        0.6 0.6860304
## 8        0.7 0.6800000
## 9        0.8 0.6706587
## 10       0.9 0.6706949
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           373
## 2         Y                                           115
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                           106
## 2                                           298
##          Prediction
## Reference   N   Y
##         N 373 106
##         Y 115 298
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.522422e-01   5.010074e-01   7.225457e-01   7.802619e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   3.066024e-40   5.904816e-01 
##                    model_id model_method                feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart biddable, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                       0.47                 0.008
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8775832                    0.4       0.7977011        0.8183695
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7926235             0.8421635     0.6331346    0.790888
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7294982        0.7522422
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7225457             0.7802619     0.5010074
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: biddable, startprice"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00727 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-11.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 969 
## 
##            CP nsplit rel error
## 1 0.525727069      0 1.0000000
## 2 0.017897092      1 0.4742729
## 3 0.007270694      3 0.4384787
## 
## Variable importance
##   biddable startprice 
##         57         43 
## 
## Node number 1: 969 observations,    complexity param=0.5257271
##   predicted class=N  expected loss=0.4613003  P(node) =1
##     class counts:   522   447
##    probabilities: 0.539 0.461 
##   left son=2 (530 obs) right son=3 (439 obs)
##   Primary splits:
##       biddable   < 0.5     to the left,  improve=150.6565, (0 missing)
##       startprice < 126.475 to the right, improve=127.7985, (0 missing)
##   Surrogate splits:
##       startprice < 152.995 to the right, agree=0.76, adj=0.469, (0 split)
## 
## Node number 2: 530 observations
##   predicted class=N  expected loss=0.2075472  P(node) =0.5469556
##     class counts:   420   110
##    probabilities: 0.792 0.208 
## 
## Node number 3: 439 observations,    complexity param=0.01789709
##   predicted class=Y  expected loss=0.2323462  P(node) =0.4530444
##     class counts:   102   337
##    probabilities: 0.232 0.768 
##   left son=6 (154 obs) right son=7 (285 obs)
##   Primary splits:
##       startprice < 127.5   to the right, improve=39.11474, (0 missing)
## 
## Node number 6: 154 observations,    complexity param=0.01789709
##   predicted class=N  expected loss=0.4805195  P(node) =0.1589267
##     class counts:    80    74
##    probabilities: 0.519 0.481 
##   left son=12 (24 obs) right son=13 (130 obs)
##   Primary splits:
##       startprice < 414.995 to the right, improve=5.601066, (0 missing)
## 
## Node number 7: 285 observations
##   predicted class=Y  expected loss=0.07719298  P(node) =0.2941176
##     class counts:    22   263
##    probabilities: 0.077 0.923 
## 
## Node number 12: 24 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.0247678
##     class counts:    20     4
##    probabilities: 0.833 0.167 
## 
## Node number 13: 130 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.1341589
##     class counts:    60    70
##    probabilities: 0.462 0.538 
## 
## n= 969 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 969 447 N (0.53869969 0.46130031)  
##    2) biddable< 0.5 530 110 N (0.79245283 0.20754717) *
##    3) biddable>=0.5 439 102 Y (0.23234624 0.76765376)  
##      6) startprice>=127.5 154  74 N (0.51948052 0.48051948)  
##       12) startprice>=414.995 24   4 N (0.83333333 0.16666667) *
##       13) startprice< 414.995 130  60 Y (0.46153846 0.53846154) *
##      7) startprice< 127.5 285  22 Y (0.07719298 0.92280702) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-13.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.6313559
## 3        0.2 0.6364943
## 4        0.3 0.7726218
## 5        0.4 0.7726218
## 6        0.5 0.7726218
## 7        0.6 0.7185792
## 8        0.7 0.7185792
## 9        0.8 0.7185792
## 10       0.9 0.7185792
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-14.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 440
## 2         Y                                 114
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  82
## 2                                 333
##          Prediction
## Reference   N   Y
##         N 440  82
##         Y 114 333
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.977296e-01   5.909173e-01   7.710339e-01   8.226019e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   1.071676e-63   2.680913e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6329502
## 3        0.2 0.6320682
## 4        0.3 0.7478043
## 5        0.4 0.7478043
## 6        0.5 0.7478043
## 7        0.6 0.6706949
## 8        0.7 0.6706949
## 9        0.8 0.6706949
## 10       0.9 0.6706949
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-16.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 393
## 2         Y                                 115
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  86
## 2                                 298
##          Prediction
## Reference   N   Y
##         N 393  86
##         Y 115 298
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.746637e-01   5.446437e-01   7.458018e-01   8.016983e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   3.787245e-49   4.827181e-02 
##          model_id model_method                feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart biddable, startprice               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.949                 0.011   0.8255676
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5       0.7726218        0.7894737
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7710339             0.8226019      0.571941   0.7965723
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7478043        0.7746637
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7458018             0.8016983     0.5446437
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009287926      0.01597718
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: biddable, startprice"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-17.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-18.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-19.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0828  -0.7376  -0.2970   0.6044   2.7762  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.1277322  0.1758847   0.726    0.468    
## biddable     1.9492275  0.1698126  11.479   <2e-16 ***
## startprice  -0.0058665  0.0006211  -9.446   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1337.51  on 968  degrees of freedom
## Residual deviance:  906.51  on 966  degrees of freedom
## AIC: 912.51
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.6697747
## 3        0.2 0.7204207
## 4        0.3 0.7479839
## 5        0.4 0.7771679
## 6        0.5 0.7636787
## 7        0.6 0.7602906
## 8        0.7 0.7484197
## 9        0.8 0.6990291
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-22.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               412
## 2         Y                                93
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                               110
## 2                               354
##          Prediction
## Reference   N   Y
##         N 412 110
##         Y  93 354
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.905057e-01   5.796347e-01   7.635025e-01   8.157303e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   4.625452e-60   2.614461e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6628099
## 3        0.2 0.6883721
## 4        0.3 0.7178947
## 5        0.4 0.7408284
## 6        0.5 0.7427854
## 7        0.6 0.7291667
## 8        0.7 0.7317073
## 9        0.8 0.6615385
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-24.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               391
## 2         Y                               117
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                88
## 2                               296
##          Prediction
## Reference   N   Y
##         N 391  88
##         Y 117 296
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.701794e-01   5.355819e-01   7.411419e-01   7.974197e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   2.794959e-47   5.051172e-02 
##        model_id model_method                feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm biddable, startprice               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.923                 0.011   0.8592854
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.4       0.7771679        0.7915377
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7635025             0.8157303     0.5781217   0.8223119
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7427854        0.7701794
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7411419             0.7974197     0.5355819    912.5107
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01251223      0.02532251
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)             
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: biddable, startprice, biddable:D.nwrds.unq.log, biddable:D.T.like, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:cellular.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-25.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-26.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-27.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1656  -0.7374  -0.2899   0.6201   2.8003  
## 
## Coefficients:
##                                      Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         0.1603089  0.1781228   0.900   0.3681
## biddable                            1.6190963  2.8770894   0.563   0.5736
## startprice                         -0.0060165  0.0006372  -9.443   <2e-16
## `biddable:D.nwrds.unq.log`          0.3826251  1.8998454   0.201   0.8404
## `biddable:D.T.like`                -0.6314219  0.7342195  -0.860   0.3898
## `biddable:D.npnct06.log`            0.6709083  1.0923018   0.614   0.5391
## `biddable:D.ratio.nstopwrds.nwrds`  0.3675223  2.8732634   0.128   0.8982
## `biddable:D.nchrs.log`              1.0243780  1.3863636   0.739   0.4600
## `biddable:D.nwrds.log`             -1.9438664  2.6476493  -0.734   0.4628
## `biddable:cellular.fctr1`           0.3094437  0.3105257   0.997   0.3190
## `biddable:cellular.fctrUnknown`    -0.8380709  0.3768660  -2.224   0.0262
##                                       
## (Intercept)                           
## biddable                              
## startprice                         ***
## `biddable:D.nwrds.unq.log`            
## `biddable:D.T.like`                   
## `biddable:D.npnct06.log`              
## `biddable:D.ratio.nstopwrds.nwrds`    
## `biddable:D.nchrs.log`                
## `biddable:D.nwrds.log`                
## `biddable:cellular.fctr1`             
## `biddable:cellular.fctrUnknown`    *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1337.51  on 968  degrees of freedom
## Residual deviance:  896.27  on 958  degrees of freedom
## AIC: 918.27
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.6708171
## 3        0.2 0.7210526
## 4        0.3 0.7489960
## 5        0.4 0.7753304
## 6        0.5 0.7654609
## 7        0.6 0.7585366
## 8        0.7 0.7560664
## 9        0.8 0.6798867
## 10       0.9 0.2111554
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-30.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         413
## 2         Y                                          95
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                         109
## 2                                         352
##          Prediction
## Reference   N   Y
##         N 413 109
##         Y  95 352
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.894737e-01   5.773607e-01   7.624276e-01   8.147476e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   1.491405e-59   3.627265e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6639004
## 3        0.2 0.6902985
## 4        0.3 0.7178947
## 5        0.4 0.7434679
## 6        0.5 0.7424242
## 7        0.6 0.7279895
## 8        0.7 0.7213115
## 9        0.8 0.6342229
## 10       0.9 0.2073434
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-32.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         363
## 2         Y                                         100
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                         116
## 2                                         313
##          Prediction
## Reference   N   Y
##         N 363 116
##         Y 100 313
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.578475e-01   5.143253e-01   7.283498e-01   7.856310e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   2.288444e-42   3.074342e-01 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                             feats
## 1 biddable, startprice, biddable:D.nwrds.unq.log, biddable:D.T.like, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.941                 0.015
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8593261                    0.4       0.7753304        0.7894737
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7624276             0.8147476     0.5739487   0.8220844
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7434679        0.7578475
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7283498              0.785631     0.5143253    918.2717
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008191181      0.01641682
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: biddable, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.npnct08.log, D.T.condit, color.fctr, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-33.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-34.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-35.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4494  -0.6476  -0.1489   0.5762   3.7614  
## 
## Coefficients: (1 not defined because of singularities)
##                                            Estimate Std. Error z value
## (Intercept)                                0.549285   0.747141   0.735
## biddable                                   1.669934   0.203447   8.208
## D.npnct15.log                              1.634624   1.074840   1.521
## D.T.screen                                -0.500128   0.756696  -0.661
## D.npnct03.log                              0.261222   1.364464   0.191
## D.T.used                                   0.007099   0.600843   0.012
## D.T.excellent                              0.266740   0.399992   0.667
## D.T.great                                  0.132775   0.587850   0.226
## D.ratio.sum.TfIdf.nwrds                    0.027812   0.276785   0.100
## D.T.good                                  -0.069589   0.607954  -0.114
## D.npnct01.log                             -0.907332   0.849304  -1.068
## D.T.veri                                   0.099823   0.816962   0.122
## D.T.work                                   0.376708   0.739715   0.509
## D.T.scratch                                0.393439   0.719887   0.547
## D.P.air                                   -1.462526   1.047010  -1.397
## D.T.use                                    0.475565   0.853770   0.557
## storage.fctr16                            -0.394703   0.635061  -0.622
## storage.fctr32                             0.019785   0.648333   0.031
## storage.fctr64                             0.144251   0.637235   0.226
## storage.fctrUnknown                        0.801171   0.811666   0.987
## D.P.mini                                   0.489395   0.896087   0.546
## D.T.condition                              0.517055   0.847573   0.610
## D.npnct11.log                              0.366762   0.334335   1.097
## .rnorm                                    -0.134883   0.096995  -1.391
## D.T.ipad                                   0.285937   0.915310   0.312
## D.npnct10.log                              0.129428   1.660022   0.078
## D.npnct08.log                             -0.460183   0.709655  -0.648
## D.T.condit                                 0.249940   0.524936   0.476
## color.fctrBlack                            0.223018   0.251124   0.888
## color.fctrGold                             0.168227   0.609980   0.276
## `color.fctrSpace Gray`                     0.151872   0.340833   0.446
## color.fctrWhite                            0.319402   0.253428   1.260
## D.T.this                                   0.518550   1.050050   0.494
## D.npnct06.log                             -0.803564   0.912236  -0.881
## D.T.box                                   -0.108414   0.842232  -0.129
## D.npnct28.log                             -1.997913 932.426720  -0.002
## D.T.like                                   0.069369   0.494053   0.140
## D.npnct12.log                             -1.037034   0.758094  -1.368
## D.npnct09.log                             -8.402339 557.713545  -0.015
## D.nwrds.unq.log                           -0.201648   0.163667  -1.232
## D.ndgts.log                                0.470754   0.421324   1.117
## cellular.fctr1                             0.378635   0.222719   1.700
## cellular.fctrUnknown                      -0.265270   0.517793  -0.512
## D.npnct14.log                              0.090695   0.828304   0.109
## `prdline.my.fctriPad 2`                    0.598130   0.345694   1.730
## `prdline.my.fctriPad 3`                    1.037743   0.389782   2.662
## `prdline.my.fctriPad 4`                    0.914501   0.450335   2.031
## `prdline.my.fctriPad 5`                          NA         NA      NA
## `prdline.my.fctriPad Air`                  1.631788   0.486359   3.355
## `prdline.my.fctriPad Air 2`                3.085394   0.596495   5.173
## `prdline.my.fctriPad mini`                 0.557883   0.350809   1.590
## `prdline.my.fctriPad mini 2`               1.591777   0.533071   2.986
## `prdline.my.fctriPad mini 3`               0.705251   0.754143   0.935
## `prdline.my.fctriPad mini Retina`          2.209463   1.039294   2.126
## prdline.my.fctrUnknown                    -0.524367   0.478375  -1.096
## D.npnct05.log                             -2.199460   1.634299  -1.346
## `condition.fctrFor parts or not working`  -0.893925   0.344079  -2.598
## `condition.fctrManufacturer refurbished`   0.664466   0.783604   0.848
## condition.fctrNew                         -0.178644   0.347314  -0.514
## `condition.fctrNew other (see details)`    0.030578   0.488619   0.063
## `condition.fctrSeller refurbished`        -0.891650   0.435204  -2.049
## startprice                                -0.011123   0.001279  -8.696
##                                          Pr(>|z|)    
## (Intercept)                              0.462228    
## biddable                                 2.25e-16 ***
## D.npnct15.log                            0.128308    
## D.T.screen                               0.508653    
## D.npnct03.log                            0.848176    
## D.T.used                                 0.990574    
## D.T.excellent                            0.504859    
## D.T.great                                0.821306    
## D.ratio.sum.TfIdf.nwrds                  0.919963    
## D.T.good                                 0.908869    
## D.npnct01.log                            0.285375    
## D.T.veri                                 0.902750    
## D.T.work                                 0.610569    
## D.T.scratch                              0.584702    
## D.P.air                                  0.162456    
## D.T.use                                  0.577516    
## storage.fctr16                           0.534258    
## storage.fctr32                           0.975655    
## storage.fctr64                           0.820914    
## storage.fctrUnknown                      0.323609    
## D.P.mini                                 0.584965    
## D.T.condition                            0.541834    
## D.npnct11.log                            0.272646    
## .rnorm                                   0.164342    
## D.T.ipad                                 0.754741    
## D.npnct10.log                            0.937854    
## D.npnct08.log                            0.516688    
## D.T.condit                               0.633979    
## color.fctrBlack                          0.374498    
## color.fctrGold                           0.782708    
## `color.fctrSpace Gray`                   0.655893    
## color.fctrWhite                          0.207552    
## D.T.this                                 0.621424    
## D.npnct06.log                            0.378386    
## D.T.box                                  0.897577    
## D.npnct28.log                            0.998290    
## D.T.like                                 0.888338    
## D.npnct12.log                            0.171328    
## D.npnct09.log                            0.987980    
## D.nwrds.unq.log                          0.217927    
## D.ndgts.log                              0.263857    
## cellular.fctr1                           0.089121 .  
## cellular.fctrUnknown                     0.608434    
## D.npnct14.log                            0.912810    
## `prdline.my.fctriPad 2`                  0.083589 .  
## `prdline.my.fctriPad 3`                  0.007759 ** 
## `prdline.my.fctriPad 4`                  0.042284 *  
## `prdline.my.fctriPad 5`                        NA    
## `prdline.my.fctriPad Air`                0.000793 ***
## `prdline.my.fctriPad Air 2`              2.31e-07 ***
## `prdline.my.fctriPad mini`               0.111772    
## `prdline.my.fctriPad mini 2`             0.002826 ** 
## `prdline.my.fctriPad mini 3`             0.349701    
## `prdline.my.fctriPad mini Retina`        0.033509 *  
## prdline.my.fctrUnknown                   0.273016    
## D.npnct05.log                            0.178363    
## `condition.fctrFor parts or not working` 0.009376 ** 
## `condition.fctrManufacturer refurbished` 0.396459    
## condition.fctrNew                        0.607001    
## `condition.fctrNew other (see details)`  0.950100    
## `condition.fctrSeller refurbished`       0.040481 *  
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1337.51  on 968  degrees of freedom
## Residual deviance:  812.64  on 908  degrees of freedom
## AIC: 934.64
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-36.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.7116166
## 3        0.2 0.7572993
## 4        0.3 0.7700000
## 5        0.4 0.7813853
## 6        0.5 0.7954023
## 7        0.6 0.7918171
## 8        0.7 0.7662835
## 9        0.8 0.6966292
## 10       0.9 0.4501718
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               445
## 2         Y                               101
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                77
## 2                               346
##          Prediction
## Reference   N   Y
##         N 445  77
##         Y 101 346
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.163055e-01   6.289656e-01   7.904595e-01   8.402123e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   1.124987e-73   8.472177e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-38.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6806283
## 3        0.2 0.6951456
## 4        0.3 0.7245119
## 5        0.4 0.7418605
## 6        0.5 0.7405542
## 7        0.6 0.7413333
## 8        0.7 0.7011494
## 9        0.8 0.6248038
## 10       0.9 0.3877159
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               351
## 2         Y                                94
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                               128
## 2                               319
##          Prediction
## Reference   N   Y
##         N 351 128
##         Y  94 319
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.511211e-01   5.023247e-01   7.213857e-01   7.791873e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   8.019721e-40   2.677277e-02 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 biddable, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.npnct08.log, D.T.condit, color.fctr, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.235                 0.129
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8890475                    0.5       0.7954023        0.7636739
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7904595             0.8402123     0.5225041   0.8242202
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7418605        0.7511211
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7213857             0.7791873     0.5023247     934.642
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0170513      0.03600209
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 41.384 65.926  24.542
## 11 fit.models          7          1 65.927     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor   bgn end elapsed
## 1 fit.models_1_bgn          1          0 70.29  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here"); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 70.290 70.309   0.019
## 2 fit.models_1_glm          2          0 70.309     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-1.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_1-2.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_1-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4666  -0.6341  -0.1250   0.5651   3.8127  
## 
## Coefficients: (1 not defined because of singularities)
##                                            Estimate Std. Error z value
## (Intercept)                               1.065e+01  5.802e+00   1.837
## biddable                                  1.679e+00  2.100e-01   7.993
## D.ratio.nstopwrds.nwrds                  -1.011e+01  5.789e+00  -1.746
## D.npnct15.log                             1.600e+00  1.177e+00   1.359
## D.T.screen                               -6.056e-01  8.352e-01  -0.725
## D.npnct03.log                            -1.693e+00  1.829e+00  -0.926
## D.T.used                                 -1.834e-01  8.613e-01  -0.213
## D.T.excellent                             6.418e-01  4.014e-01   1.599
## D.T.great                                 4.107e-02  6.617e-01   0.062
## D.ratio.sum.TfIdf.nwrds                  -9.351e-01  6.160e-01  -1.518
## D.T.good                                 -2.419e-01  7.465e-01  -0.324
## D.npnct01.log                            -1.212e+00  1.002e+00  -1.210
## D.T.veri                                  3.086e-01  8.772e-01   0.352
## D.T.work                                  6.618e-01  8.315e-01   0.796
## D.T.scratch                               6.427e-01  8.525e-01   0.754
## D.P.air                                  -1.265e+00  1.100e+00  -1.150
## D.T.use                                   7.229e-02  1.024e+00   0.071
## storage.fctr16                           -3.407e-01  6.596e-01  -0.516
## storage.fctr32                            4.736e-02  6.736e-01   0.070
## storage.fctr64                            1.249e-01  6.625e-01   0.189
## storage.fctrUnknown                       8.773e-01  8.466e-01   1.036
## D.P.mini                                  9.960e-01  1.098e+00   0.907
## D.T.condition                             7.379e-01  1.084e+00   0.681
## D.npnct11.log                             7.982e-01  4.323e-01   1.847
## .rnorm                                   -1.239e-01  9.986e-02  -1.240
## D.T.ipad                                  4.921e-01  9.949e-01   0.495
## D.npnct10.log                             1.751e+00  2.028e+00   0.863
## D.sum.TfIdf                               2.251e-01  2.013e-01   1.118
## D.T.new                                   1.575e+00  1.026e+00   1.535
## D.npnct13.log                            -6.606e-02  4.957e-01  -0.133
## D.npnct08.log                            -2.172e-01  8.343e-01  -0.260
## D.T.condit                                2.459e-01  6.708e-01   0.367
## color.fctrBlack                           2.146e-01  2.600e-01   0.825
## color.fctrGold                            6.885e-02  6.350e-01   0.108
## `color.fctrSpace Gray`                    2.357e-01  3.494e-01   0.675
## color.fctrWhite                           3.921e-01  2.612e-01   1.501
## D.nstopwrds.log                           3.009e+00  1.773e+00   1.697
## D.npnct16.log                             4.344e+00  2.120e+00   2.049
## D.npnct24.log                             1.091e+01  8.304e+00   1.314
## D.T.this                                  4.128e-01  1.249e+00   0.330
## D.npnct06.log                            -4.102e+00  2.351e+00  -1.745
## D.T.box                                  -1.419e+00  9.557e-01  -1.484
## D.npnct28.log                            -2.084e+00  8.629e+02  -0.002
## D.T.like                                 -1.600e+00  8.986e-01  -1.781
## D.npnct12.log                            -3.745e-01  9.023e-01  -0.415
## D.nuppr.log                               8.814e+00  8.405e+00   1.049
## D.nchrs.log                              -1.409e+01  9.870e+00  -1.428
## D.nwrds.log                               2.959e+00  3.279e+00   0.902
## D.npnct09.log                            -7.681e+00  4.612e+02  -0.017
## D.nwrds.unq.log                          -1.421e+00  8.731e-01  -1.628
## D.ndgts.log                               7.992e-01  6.724e-01   1.188
## `carrier.fctrAT&T`                        1.489e+01  9.781e+02   0.015
## carrier.fctrOther                         2.758e+01  1.382e+03   0.020
## carrier.fctrSprint                        1.458e+01  9.781e+02   0.015
## `carrier.fctrT-Mobile`                    1.396e+01  9.781e+02   0.014
## carrier.fctrUnknown                       1.509e+01  9.781e+02   0.015
## carrier.fctrVerizon                       1.500e+01  9.781e+02   0.015
## cellular.fctr1                           -1.446e+01  9.781e+02  -0.015
## cellular.fctrUnknown                     -1.522e+01  9.781e+02  -0.016
## D.npnct14.log                            -4.078e-01  9.203e-01  -0.443
## `prdline.my.fctriPad 2`                   5.687e-01  3.572e-01   1.592
## `prdline.my.fctriPad 3`                   1.182e+00  4.117e-01   2.872
## `prdline.my.fctriPad 4`                   8.142e-01  4.693e-01   1.735
## `prdline.my.fctriPad 5`                          NA         NA      NA
## `prdline.my.fctriPad Air`                 1.723e+00  5.111e-01   3.370
## `prdline.my.fctriPad Air 2`               3.338e+00  6.215e-01   5.370
## `prdline.my.fctriPad mini`                5.317e-01  3.649e-01   1.457
## `prdline.my.fctriPad mini 2`              1.662e+00  5.522e-01   3.010
## `prdline.my.fctriPad mini 3`              8.264e-01  7.708e-01   1.072
## `prdline.my.fctriPad mini Retina`         2.286e+00  1.081e+00   2.114
## prdline.my.fctrUnknown                   -6.261e-01  5.045e-01  -1.241
## D.npnct05.log                            -2.387e+00  1.896e+00  -1.259
## `condition.fctrFor parts or not working` -1.094e+00  3.567e-01  -3.067
## `condition.fctrManufacturer refurbished`  6.024e-01  8.302e-01   0.726
## condition.fctrNew                        -2.202e-01  3.581e-01  -0.615
## `condition.fctrNew other (see details)`  -1.446e-01  5.326e-01  -0.272
## `condition.fctrSeller refurbished`       -7.365e-01  4.484e-01  -1.642
## startprice                               -1.166e-02  1.337e-03  -8.719
##                                          Pr(>|z|)    
## (Intercept)                              0.066283 .  
## biddable                                 1.32e-15 ***
## D.ratio.nstopwrds.nwrds                  0.080731 .  
## D.npnct15.log                            0.174131    
## D.T.screen                               0.468389    
## D.npnct03.log                            0.354564    
## D.T.used                                 0.831352    
## D.T.excellent                            0.109819    
## D.T.great                                0.950508    
## D.ratio.sum.TfIdf.nwrds                  0.129001    
## D.T.good                                 0.745908    
## D.npnct01.log                            0.226451    
## D.T.veri                                 0.724968    
## D.T.work                                 0.426081    
## D.T.scratch                              0.450882    
## D.P.air                                  0.250113    
## D.T.use                                  0.943746    
## storage.fctr16                           0.605527    
## storage.fctr32                           0.943945    
## storage.fctr64                           0.850440    
## storage.fctrUnknown                      0.300107    
## D.P.mini                                 0.364487    
## D.T.condition                            0.496018    
## D.npnct11.log                            0.064807 .  
## .rnorm                                   0.214880    
## D.T.ipad                                 0.620895    
## D.npnct10.log                            0.388027    
## D.sum.TfIdf                              0.263396    
## D.T.new                                  0.124822    
## D.npnct13.log                            0.893978    
## D.npnct08.log                            0.794621    
## D.T.condit                               0.713954    
## color.fctrBlack                          0.409094    
## color.fctrGold                           0.913670    
## `color.fctrSpace Gray`                   0.499833    
## color.fctrWhite                          0.133235    
## D.nstopwrds.log                          0.089641 .  
## D.npnct16.log                            0.040456 *  
## D.npnct24.log                            0.188765    
## D.T.this                                 0.741071    
## D.npnct06.log                            0.080979 .  
## D.T.box                                  0.137738    
## D.npnct28.log                            0.998073    
## D.T.like                                 0.074960 .  
## D.npnct12.log                            0.678073    
## D.nuppr.log                              0.294354    
## D.nchrs.log                              0.153332    
## D.nwrds.log                              0.366823    
## D.npnct09.log                            0.986712    
## D.nwrds.unq.log                          0.103520    
## D.ndgts.log                              0.234646    
## `carrier.fctrAT&T`                       0.987852    
## carrier.fctrOther                        0.984075    
## carrier.fctrSprint                       0.988107    
## `carrier.fctrT-Mobile`                   0.988611    
## carrier.fctrUnknown                      0.987688    
## carrier.fctrVerizon                      0.987767    
## cellular.fctr1                           0.988201    
## cellular.fctrUnknown                     0.987587    
## D.npnct14.log                            0.657695    
## `prdline.my.fctriPad 2`                  0.111324    
## `prdline.my.fctriPad 3`                  0.004083 ** 
## `prdline.my.fctriPad 4`                  0.082751 .  
## `prdline.my.fctriPad 5`                        NA    
## `prdline.my.fctriPad Air`                0.000751 ***
## `prdline.my.fctriPad Air 2`              7.87e-08 ***
## `prdline.my.fctriPad mini`               0.145067    
## `prdline.my.fctriPad mini 2`             0.002612 ** 
## `prdline.my.fctriPad mini 3`             0.283672    
## `prdline.my.fctriPad mini Retina`        0.034533 *  
## prdline.my.fctrUnknown                   0.214562    
## D.npnct05.log                            0.208139    
## `condition.fctrFor parts or not working` 0.002165 ** 
## `condition.fctrManufacturer refurbished` 0.468025    
## condition.fctrNew                        0.538508    
## `condition.fctrNew other (see details)`  0.785948    
## `condition.fctrSeller refurbished`       0.100488    
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1337.51  on 968  degrees of freedom
## Residual deviance:  785.15  on 892  degrees of freedom
## AIC: 939.15
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-4.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.7205276
## 3        0.2 0.7673993
## 4        0.3 0.7871486
## 5        0.4 0.7961165
## 6        0.5 0.8009153
## 7        0.6 0.7913148
## 8        0.7 0.7770701
## 9        0.8 0.7138889
## 10       0.9 0.4991681
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           445                            77
## 2         Y                            97                           350
##          Prediction
## Reference   N   Y
##         N 445  77
##         Y  97 350
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.204334e-01   6.375374e-01   7.947886e-01   8.441134e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   5.071854e-76   1.497581e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-6.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6838825
## 3        0.2 0.6961652
## 4        0.3 0.7142857
## 5        0.4 0.7374562
## 6        0.5 0.7315855
## 7        0.6 0.7267905
## 8        0.7 0.7188811
## 9        0.8 0.6296296
## 10       0.9 0.4149533
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           351                           128
## 2         Y                            97                           316
##          Prediction
## Reference   N   Y
##         N 351 128
##         Y  97 316
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.477578e-01   4.953483e-01   7.179071e-01   7.759620e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   1.384866e-38   4.550026e-02 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.366                 0.176
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8974946                    0.5       0.8009153        0.7574819
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7947886             0.8441134     0.5103726   0.8156723
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7374562        0.7477578
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7179071              0.775962     0.4953483    939.1513
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02876648      0.06032482
##                   label step_major step_minor    bgn    end elapsed
## 2      fit.models_1_glm          2          0 70.309 75.369    5.06
## 3 fit.models_1_bayesglm          3          0 75.369     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## 
## arm (Version 1.8-6, built: 2015-7-7)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/Kaggle_eBay_iPads
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3838  -0.6489  -0.1475   0.5755   3.7025  
## 
## Coefficients:
##                                           Estimate Std. Error z value
## (Intercept)                               1.991412   2.285603   0.871
## biddable                                  1.663804   0.202424   8.219
## D.ratio.nstopwrds.nwrds                  -1.316612   2.213100  -0.595
## D.npnct15.log                             1.340396   1.088486   1.231
## D.T.screen                               -0.596517   0.774824  -0.770
## D.npnct03.log                            -0.471488   1.512259  -0.312
## D.T.used                                  0.094107   0.631360   0.149
## D.T.excellent                             0.411714   0.414664   0.993
## D.T.great                                 0.236186   0.610025   0.387
## D.ratio.sum.TfIdf.nwrds                  -0.250916   0.438982  -0.572
## D.T.good                                  0.013422   0.625580   0.021
## D.npnct01.log                            -1.411432   0.893562  -1.580
## D.T.veri                                  0.026448   0.826339   0.032
## D.T.work                                  0.604670   0.754088   0.802
## D.T.scratch                               0.408186   0.752332   0.543
## D.P.air                                  -1.013709   1.029735  -0.984
## D.T.use                                   0.275408   0.921889   0.299
## storage.fctr16                           -0.385483   0.531597  -0.725
## storage.fctr32                           -0.026067   0.547501  -0.048
## storage.fctr64                            0.082547   0.541935   0.152
## storage.fctrUnknown                       0.664710   0.693948   0.958
## D.P.mini                                  1.136228   1.048031   1.084
## D.T.condition                             0.694420   0.897298   0.774
## D.npnct11.log                             0.454961   0.347014   1.311
## .rnorm                                   -0.125352   0.096643  -1.297
## D.T.ipad                                  0.483505   0.927323   0.521
## D.npnct10.log                             0.972999   1.799203   0.541
## D.sum.TfIdf                               0.110638   0.150047   0.737
## D.T.new                                   1.815163   0.900403   2.016
## D.npnct13.log                            -0.367556   0.363557  -1.011
## D.npnct08.log                            -0.548680   0.745787  -0.736
## D.T.condit                                0.203766   0.574528   0.355
## color.fctrBlack                           0.173820   0.249447   0.697
## color.fctrGold                            0.138107   0.582206   0.237
## `color.fctrSpace Gray`                    0.204460   0.334470   0.611
## color.fctrWhite                           0.350746   0.249900   1.404
## D.nstopwrds.log                           0.841922   0.635379   1.325
## D.npnct16.log                             3.171609   1.814439   1.748
## D.npnct24.log                             0.965056   2.569251   0.376
## D.T.this                                 -0.100361   1.132509  -0.089
## D.npnct06.log                            -3.459204   1.992381  -1.736
## D.T.box                                  -0.952496   0.889028  -1.071
## D.npnct28.log                            -0.049196   2.192994  -0.022
## D.T.like                                 -1.117442   0.821572  -1.360
## D.npnct12.log                            -0.792353   0.798547  -0.992
## D.nuppr.log                              -0.201537   0.519364  -0.388
## D.nchrs.log                              -0.194265   0.501887  -0.387
## D.nwrds.log                              -0.020754   0.739745  -0.028
## D.npnct09.log                            -1.732555   5.139365  -0.337
## D.nwrds.unq.log                          -0.858232   0.552457  -1.553
## D.ndgts.log                               0.332924   0.448083   0.743
## `carrier.fctrAT&T`                        0.232354   0.726143   0.320
## carrier.fctrOther                         0.662862   1.900101   0.349
## carrier.fctrSprint                        0.117004   0.953926   0.123
## `carrier.fctrT-Mobile`                   -0.359912   1.020340  -0.353
## carrier.fctrUnknown                       0.483090   0.718737   0.672
## carrier.fctrVerizon                       0.453471   0.751785   0.603
## cellular.fctr1                            0.111153   0.699449   0.159
## cellular.fctrUnknown                     -0.572200   0.785487  -0.728
## D.npnct14.log                            -0.314827   0.859177  -0.366
## `prdline.my.fctriPad 2`                   0.449728   0.330378   1.361
## `prdline.my.fctriPad 3`                   0.908481   0.372486   2.439
## `prdline.my.fctriPad 4`                   0.626103   0.423823   1.477
## `prdline.my.fctriPad 5`                   0.000000   2.500000   0.000
## `prdline.my.fctriPad Air`                 1.444492   0.456739   3.163
## `prdline.my.fctriPad Air 2`               2.888613   0.550730   5.245
## `prdline.my.fctriPad mini`                0.350776   0.330576   1.061
## `prdline.my.fctriPad mini 2`              1.386523   0.501865   2.763
## `prdline.my.fctriPad mini 3`              0.445529   0.684079   0.651
## `prdline.my.fctriPad mini Retina`         1.765515   0.967413   1.825
## prdline.my.fctrUnknown                   -0.725185   0.461802  -1.570
## D.npnct05.log                            -1.585515   1.415922  -1.120
## `condition.fctrFor parts or not working` -0.928539   0.338008  -2.747
## `condition.fctrManufacturer refurbished`  0.574266   0.736336   0.780
## condition.fctrNew                        -0.162513   0.342423  -0.475
## `condition.fctrNew other (see details)`  -0.032576   0.492279  -0.066
## `condition.fctrSeller refurbished`       -0.776439   0.421232  -1.843
## startprice                               -0.011019   0.001215  -9.067
##                                          Pr(>|z|)    
## (Intercept)                               0.38360    
## biddable                                  < 2e-16 ***
## D.ratio.nstopwrds.nwrds                   0.55190    
## D.npnct15.log                             0.21816    
## D.T.screen                                0.44137    
## D.npnct03.log                             0.75521    
## D.T.used                                  0.88151    
## D.T.excellent                             0.32076    
## D.T.great                                 0.69863    
## D.ratio.sum.TfIdf.nwrds                   0.56760    
## D.T.good                                  0.98288    
## D.npnct01.log                             0.11421    
## D.T.veri                                  0.97447    
## D.T.work                                  0.42264    
## D.T.scratch                               0.58743    
## D.P.air                                   0.32490    
## D.T.use                                   0.76514    
## storage.fctr16                            0.46837    
## storage.fctr32                            0.96203    
## storage.fctr64                            0.87894    
## storage.fctrUnknown                       0.33813    
## D.P.mini                                  0.27830    
## D.T.condition                             0.43899    
## D.npnct11.log                             0.18983    
## .rnorm                                    0.19461    
## D.T.ipad                                  0.60209    
## D.npnct10.log                             0.58865    
## D.sum.TfIdf                               0.46091    
## D.T.new                                   0.04381 *  
## D.npnct13.log                             0.31202    
## D.npnct08.log                             0.46191    
## D.T.condit                                0.72284    
## color.fctrBlack                           0.48591    
## color.fctrGold                            0.81249    
## `color.fctrSpace Gray`                    0.54100    
## color.fctrWhite                           0.16045    
## D.nstopwrds.log                           0.18515    
## D.npnct16.log                             0.08047 .  
## D.npnct24.log                             0.70720    
## D.T.this                                  0.92939    
## D.npnct06.log                             0.08253 .  
## D.T.box                                   0.28399    
## D.npnct28.log                             0.98210    
## D.T.like                                  0.17379    
## D.npnct12.log                             0.32108    
## D.nuppr.log                               0.69798    
## D.nchrs.log                               0.69870    
## D.nwrds.log                               0.97762    
## D.npnct09.log                             0.73603    
## D.nwrds.unq.log                           0.12031    
## D.ndgts.log                               0.45748    
## `carrier.fctrAT&T`                        0.74898    
## carrier.fctrOther                         0.72720    
## carrier.fctrSprint                        0.90238    
## `carrier.fctrT-Mobile`                    0.72429    
## carrier.fctrUnknown                       0.50150    
## carrier.fctrVerizon                       0.54638    
## cellular.fctr1                            0.87374    
## cellular.fctrUnknown                      0.46633    
## D.npnct14.log                             0.71405    
## `prdline.my.fctriPad 2`                   0.17343    
## `prdline.my.fctriPad 3`                   0.01473 *  
## `prdline.my.fctriPad 4`                   0.13960    
## `prdline.my.fctriPad 5`                   1.00000    
## `prdline.my.fctriPad Air`                 0.00156 ** 
## `prdline.my.fctriPad Air 2`              1.56e-07 ***
## `prdline.my.fctriPad mini`                0.28864    
## `prdline.my.fctriPad mini 2`              0.00573 ** 
## `prdline.my.fctriPad mini 3`              0.51486    
## `prdline.my.fctriPad mini Retina`         0.06800 .  
## prdline.my.fctrUnknown                    0.11634    
## D.npnct05.log                             0.26281    
## `condition.fctrFor parts or not working`  0.00601 ** 
## `condition.fctrManufacturer refurbished`  0.43545    
## condition.fctrNew                         0.63507    
## `condition.fctrNew other (see details)`   0.94724    
## `condition.fctrSeller refurbished`        0.06529 .  
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1337.51  on 968  degrees of freedom
## Residual deviance:  794.34  on 891  degrees of freedom
## AIC: 950.34
## 
## Number of Fisher Scoring iterations: 13
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.7184943
## 3        0.2 0.7629428
## 4        0.3 0.7815631
## 5        0.4 0.7918015
## 6        0.5 0.7986111
## 7        0.6 0.7927711
## 8        0.7 0.7708067
## 9        0.8 0.7085077
## 10       0.9 0.4482759
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                450
## 2         Y                                102
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 72
## 2                                345
##          Prediction
## Reference   N   Y
##         N 450  72
##         Y 102 345
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.204334e-01   6.369521e-01   7.947886e-01   8.441134e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   5.071854e-76   2.791461e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6736475
## 3        0.2 0.7048544
## 4        0.3 0.7186147
## 5        0.4 0.7363531
## 6        0.5 0.7336683
## 7        0.6 0.7326203
## 8        0.7 0.6971429
## 9        0.8 0.6204724
## 10       0.9 0.3831418
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                348
## 2         Y                                 96
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                131
## 2                                317
##          Prediction
## Reference   N   Y
##         N 348 131
##         Y  96 317
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.455157e-01   4.912002e-01   7.155892e-01   7.738105e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   8.982535e-38   2.402937e-02 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.381                 0.269
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8942546                    0.5       0.7986111        0.7678019
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7947886             0.8441134     0.5312525    0.819327
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7363531        0.7455157
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7155892             0.7738105     0.4912002    950.3381
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0233741      0.04896774
##                   label step_major step_minor    bgn    end elapsed
## 3 fit.models_1_bayesglm          3          0 75.369 80.922   5.553
## 4    fit.models_1_rpart          4          0 80.923     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0157 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-13.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_1-14.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 969 
## 
##           CP nsplit rel error
## 1 0.52572707      0 1.0000000
## 2 0.01789709      1 0.4742729
## 3 0.01565996      3 0.4384787
## 
## Variable importance
##                               biddable 
##                                     51 
##                             startprice 
##                                     39 
##                      condition.fctrNew 
##                                      2 
## condition.fctrFor parts or not working 
##                                      2 
##              prdline.my.fctriPad Air 2 
##                                      2 
##                          D.npnct03.log 
##                                      1 
##                   color.fctrSpace Gray 
##                                      1 
##                prdline.my.fctriPad Air 
##                                      1 
##                  prdline.my.fctriPad 2 
##                                      1 
##                            D.sum.TfIdf 
##                                      1 
##  condition.fctrNew other (see details) 
##                                      1 
## 
## Node number 1: 969 observations,    complexity param=0.5257271
##   predicted class=N  expected loss=0.4613003  P(node) =1
##     class counts:   522   447
##    probabilities: 0.539 0.461 
##   left son=2 (530 obs) right son=3 (439 obs)
##   Primary splits:
##       biddable                   < 0.5       to the left,  improve=150.656500, (0 missing)
##       startprice                 < 126.475   to the right, improve=127.798500, (0 missing)
##       condition.fctrNew          < 0.5       to the right, improve= 21.222440, (0 missing)
##       prdline.my.fctriPad mini 3 < 0.5       to the right, improve=  9.639246, (0 missing)
##       D.ratio.nstopwrds.nwrds    < 0.2071429 to the left,  improve=  6.077179, (0 missing)
##   Surrogate splits:
##       startprice                             < 152.995   to the right, agree=0.760, adj=0.469, (0 split)
##       condition.fctrFor parts or not working < 0.5       to the left,  agree=0.562, adj=0.034, (0 split)
##       D.npnct03.log                          < 0.3465736 to the left,  agree=0.556, adj=0.021, (0 split)
##       prdline.my.fctriPad 2                  < 0.5       to the left,  agree=0.554, adj=0.016, (0 split)
##       D.sum.TfIdf                            < 7.685369  to the left,  agree=0.553, adj=0.014, (0 split)
## 
## Node number 2: 530 observations
##   predicted class=N  expected loss=0.2075472  P(node) =0.5469556
##     class counts:   420   110
##    probabilities: 0.792 0.208 
## 
## Node number 3: 439 observations,    complexity param=0.01789709
##   predicted class=Y  expected loss=0.2323462  P(node) =0.4530444
##     class counts:   102   337
##    probabilities: 0.232 0.768 
##   left son=6 (154 obs) right son=7 (285 obs)
##   Primary splits:
##       startprice                 < 127.5     to the right, improve=39.114740, (0 missing)
##       condition.fctrNew          < 0.5       to the right, improve= 6.198332, (0 missing)
##       prdline.my.fctriPad mini 3 < 0.5       to the right, improve= 2.813945, (0 missing)
##       D.ratio.nstopwrds.nwrds    < 0.1882353 to the left,  improve= 2.510720, (0 missing)
##       prdline.my.fctrUnknown     < 0.5       to the right, improve= 2.194095, (0 missing)
##   Surrogate splits:
##       condition.fctrNew                     < 0.5       to the right, agree=0.706, adj=0.162, (0 split)
##       prdline.my.fctriPad Air 2             < 0.5       to the right, agree=0.695, adj=0.130, (0 split)
##       color.fctrSpace Gray                  < 0.5       to the right, agree=0.674, adj=0.071, (0 split)
##       prdline.my.fctriPad Air               < 0.5       to the right, agree=0.672, adj=0.065, (0 split)
##       condition.fctrNew other (see details) < 0.5       to the right, agree=0.667, adj=0.052, (0 split)
## 
## Node number 6: 154 observations,    complexity param=0.01789709
##   predicted class=N  expected loss=0.4805195  P(node) =0.1589267
##     class counts:    80    74
##    probabilities: 0.519 0.481 
##   left son=12 (24 obs) right son=13 (130 obs)
##   Primary splits:
##       startprice            < 414.995   to the right, improve=5.601066, (0 missing)
##       D.T.use               < 0.1281762 to the right, improve=2.608787, (0 missing)
##       D.nstopwrds.log       < 2.249905  to the right, improve=2.608787, (0 missing)
##       D.nwrds.log           < 2.970086  to the right, improve=2.608787, (0 missing)
##       prdline.my.fctriPad 3 < 0.5       to the left,  improve=2.593262, (0 missing)
##   Surrogate splits:
##       color.fctrGold             < 0.5       to the right, agree=0.851, adj=0.042, (0 split)
##       prdline.my.fctriPad mini 3 < 0.5       to the right, agree=0.851, adj=0.042, (0 split)
## 
## Node number 7: 285 observations
##   predicted class=Y  expected loss=0.07719298  P(node) =0.2941176
##     class counts:    22   263
##    probabilities: 0.077 0.923 
## 
## Node number 12: 24 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.0247678
##     class counts:    20     4
##    probabilities: 0.833 0.167 
## 
## Node number 13: 130 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.1341589
##     class counts:    60    70
##    probabilities: 0.462 0.538 
## 
## n= 969 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 969 447 N (0.53869969 0.46130031)  
##    2) biddable< 0.5 530 110 N (0.79245283 0.20754717) *
##    3) biddable>=0.5 439 102 Y (0.23234624 0.76765376)  
##      6) startprice>=127.5 154  74 N (0.51948052 0.48051948)  
##       12) startprice>=414.995 24   4 N (0.83333333 0.16666667) *
##       13) startprice< 414.995 130  60 Y (0.46153846 0.53846154) *
##      7) startprice< 127.5 285  22 Y (0.07719298 0.92280702) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.6313559
## 3        0.2 0.6364943
## 4        0.3 0.7726218
## 5        0.4 0.7726218
## 6        0.5 0.7726218
## 7        0.6 0.7185792
## 8        0.7 0.7185792
## 9        0.8 0.7185792
## 10       0.9 0.7185792
## 11       1.0 0.0000000
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      440
## 2         Y                                      114
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       82
## 2                                      333
##          Prediction
## Reference   N   Y
##         N 440  82
##         Y 114 333
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.977296e-01   5.909173e-01   7.710339e-01   8.226019e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##   1.071676e-63   2.680913e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6329502
## 3        0.2 0.6320682
## 4        0.3 0.7478043
## 5        0.4 0.7478043
## 6        0.5 0.7478043
## 7        0.6 0.6706949
## 8        0.7 0.6706949
## 9        0.8 0.6706949
## 10       0.9 0.6706949
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      393
## 2         Y                                      115
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       86
## 2                                      298
##          Prediction
## Reference   N   Y
##         N 393  86
##         Y 115 298
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.746637e-01   5.446437e-01   7.458018e-01   8.016983e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   3.787245e-49   4.827181e-02 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.531                 0.064
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8255676                    0.5       0.7726218        0.7853457
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7710339             0.8226019     0.5603172   0.7965723
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7478043        0.7746637
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7458018             0.8016983     0.5446437
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.006444786      0.01305058
##                label step_major step_minor    bgn   end elapsed
## 4 fit.models_1_rpart          4          0 80.923 86.07   5.147
## 5    fit.models_1_rf          5          0 86.071    NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-18.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 39 on full training set
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-19.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_1-20.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        969   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           1938   matrix     numeric  
## oob.times        969   -none-     numeric  
## classes            2   -none-     character
## importance        76   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                969   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            76   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6313559
## 2        0.1 0.8270120
## 3        0.2 0.9293139
## 4        0.3 0.9727965
## 5        0.4 0.9922309
## 6        0.5 0.9944259
## 7        0.6 0.9841270
## 8        0.7 0.9306220
## 9        0.8 0.8611465
## 10       0.9 0.7408451
## 11       1.0 0.1866126
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-22.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   518
## 2         Y                                     1
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                     4
## 2                                   446
##          Prediction
## Reference   N   Y
##         N 518   4
##         Y   1 446
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.948400e-01   9.896229e-01   9.879998e-01   9.983225e-01   5.386997e-01 
## AccuracyPValue  McnemarPValue 
##  1.544343e-248   3.710934e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6329502
## 2        0.1 0.6603284
## 3        0.2 0.7098039
## 4        0.3 0.7351351
## 5        0.4 0.7458432
## 6        0.5 0.7373868
## 7        0.6 0.7154930
## 8        0.7 0.6706949
## 9        0.8 0.6240000
## 10       0.9 0.4964539
## 11       1.0 0.1438202
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_1-24.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   364
## 2         Y                                    99
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                   115
## 2                                   314
##          Prediction
## Reference   N   Y
##         N 364 115
##         Y  99 314
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.600897e-01   5.188223e-01   7.306732e-01   7.877768e-01   5.369955e-01 
## AccuracyPValue  McnemarPValue 
##   3.092559e-43   3.051844e-01 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     16.341                 4.776
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9998736                    0.5       0.9944259        0.8070175
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9879998             0.9983225      0.608212   0.8221603
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7458432        0.7600897
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7306732             0.7877768     0.5188223
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01814076      0.03737143
```

```r
# User specified
#   Ensure at least 2 vars in each regression; else varImp crashes
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
# require(gdata) # needed for trim
# model_id <- "";
# indep_vars_vctr <- head(subset(glb_models_df, grepl("All\\.X\\.", model_id), select=feats)
#                         , 1)[, "feats"]
# indep_vars_vctr <- trim(unlist(strsplit(indep_vars_vctr, "[,]")))
# indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")

    # easier to include features
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
# for (method in c("bayesglm")) {
#     ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                     n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
#     csm_mdl_id <- paste0(model_id, ".", method)
#     csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
# }

# Ntv.1.lm <- lm(reformulate(indep_vars_vctr, glb_rsp_var), glb_trnobs_df); print(summary(Ntv.1.lm))

#print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              biddable, startprice
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         biddable, startprice
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   biddable, startprice
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     biddable, startprice
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                biddable, startprice, biddable:D.nwrds.unq.log, biddable:D.T.like, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                               biddable, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.npnct08.log, D.T.condit, color.fctr, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.387
## Random.myrandom_classfr                 0                      0.252
## Max.cor.Y.cv.0.rpart                    0                      0.679
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.470
## Max.cor.Y.rpart                         3                      0.949
## Max.cor.Y.glm                           1                      0.923
## Interact.High.cor.Y.glm                 1                      0.941
## Low.cor.X.glm                           1                      1.235
## All.X.glm                               1                      1.366
## All.X.bayesglm                          1                      2.381
## All.X.no.rnorm.rpart                    3                      1.531
## All.X.no.rnorm.rf                       3                     16.341
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.003   0.5000000
## Random.myrandom_classfr                   0.001   0.4877665
## Max.cor.Y.cv.0.rpart                      0.011   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.008   0.8775832
## Max.cor.Y.rpart                           0.011   0.8255676
## Max.cor.Y.glm                             0.011   0.8592854
## Interact.High.cor.Y.glm                   0.015   0.8593261
## Low.cor.X.glm                             0.129   0.8890475
## All.X.glm                                 0.176   0.8974946
## All.X.bayesglm                            0.269   0.8942546
## All.X.no.rnorm.rpart                      0.064   0.8255676
## All.X.no.rnorm.rf                         4.776   0.9998736
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6313559
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.7977011
## Max.cor.Y.rpart                              0.5       0.7726218
## Max.cor.Y.glm                                0.4       0.7771679
## Interact.High.cor.Y.glm                      0.4       0.7753304
## Low.cor.X.glm                                0.5       0.7954023
## All.X.glm                                    0.5       0.8009153
## All.X.bayesglm                               0.5       0.7986111
## All.X.no.rnorm.rpart                         0.5       0.7726218
## All.X.no.rnorm.rf                            0.5       0.9944259
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.5386997             0.5067192
## Random.myrandom_classfr          0.4613003             0.4295557
## Max.cor.Y.cv.0.rpart             0.5386997             0.5067192
## Max.cor.Y.cv.0.cp.0.rpart        0.8183695             0.7926235
## Max.cor.Y.rpart                  0.7894737             0.7710339
## Max.cor.Y.glm                    0.7915377             0.7635025
## Interact.High.cor.Y.glm          0.7894737             0.7624276
## Low.cor.X.glm                    0.7636739             0.7904595
## All.X.glm                        0.7574819             0.7947886
## All.X.bayesglm                   0.7678019             0.7947886
## All.X.no.rnorm.rpart             0.7853457             0.7710339
## All.X.no.rnorm.rf                0.8070175             0.9879998
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.5704443     0.0000000   0.5000000
## Random.myrandom_classfr               0.4932808     0.0000000   0.5196257
## Max.cor.Y.cv.0.rpart                  0.5704443     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8421635     0.6331346   0.7908880
## Max.cor.Y.rpart                       0.8226019     0.5719410   0.7965723
## Max.cor.Y.glm                         0.8157303     0.5781217   0.8223119
## Interact.High.cor.Y.glm               0.8147476     0.5739487   0.8220844
## Low.cor.X.glm                         0.8402123     0.5225041   0.8242202
## All.X.glm                             0.8441134     0.5103726   0.8156723
## All.X.bayesglm                        0.8441134     0.5312525   0.8193270
## All.X.no.rnorm.rpart                  0.8226019     0.5603172   0.7965723
## All.X.no.rnorm.rf                     0.9983225     0.6082120   0.8221603
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6329502
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.3       0.7294982
## Max.cor.Y.rpart                              0.5       0.7478043
## Max.cor.Y.glm                                0.5       0.7427854
## Interact.High.cor.Y.glm                      0.4       0.7434679
## Low.cor.X.glm                                0.4       0.7418605
## All.X.glm                                    0.4       0.7374562
## All.X.bayesglm                               0.4       0.7363531
## All.X.no.rnorm.rpart                         0.5       0.7478043
## All.X.no.rnorm.rf                            0.4       0.7458432
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.5369955             0.5036362
## Random.myrandom_classfr          0.4630045             0.4298903
## Max.cor.Y.cv.0.rpart             0.5369955             0.5036362
## Max.cor.Y.cv.0.cp.0.rpart        0.7522422             0.7225457
## Max.cor.Y.rpart                  0.7746637             0.7458018
## Max.cor.Y.glm                    0.7701794             0.7411419
## Interact.High.cor.Y.glm          0.7578475             0.7283498
## Low.cor.X.glm                    0.7511211             0.7213857
## All.X.glm                        0.7477578             0.7179071
## All.X.bayesglm                   0.7455157             0.7155892
## All.X.no.rnorm.rpart             0.7746637             0.7458018
## All.X.no.rnorm.rf                0.7600897             0.7306732
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.5701097     0.0000000
## Random.myrandom_classfr               0.4963638     0.0000000
## Max.cor.Y.cv.0.rpart                  0.5701097     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart             0.7802619     0.5010074
## Max.cor.Y.rpart                       0.8016983     0.5446437
## Max.cor.Y.glm                         0.7974197     0.5355819
## Interact.High.cor.Y.glm               0.7856310     0.5143253
## Low.cor.X.glm                         0.7791873     0.5023247
## All.X.glm                             0.7759620     0.4953483
## All.X.bayesglm                        0.7738105     0.4912002
## All.X.no.rnorm.rpart                  0.8016983     0.5446437
## All.X.no.rnorm.rf                     0.7877768     0.5188223
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                  0.009287926      0.01597718          NA
## Max.cor.Y.glm                    0.012512235      0.02532251    912.5107
## Interact.High.cor.Y.glm          0.008191181      0.01641682    918.2717
## Low.cor.X.glm                    0.017051302      0.03600209    934.6420
## All.X.glm                        0.028766481      0.06032482    939.1513
## All.X.bayesglm                   0.023374100      0.04896774    950.3381
## All.X.no.rnorm.rpart             0.006444786      0.01305058          NA
## All.X.no.rnorm.rf                0.018140759      0.03737143          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 5  fit.models_1_rf          5          0  86.071 106.312  20.241
## 6 fit.models_1_end          6          0 106.313      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn    end elapsed
## 11 fit.models          7          1  65.927 106.32  40.393
## 12 fit.models          7          2 106.320     NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id",
                                    grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df),
                                    grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              biddable, startprice
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         biddable, startprice
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   biddable, startprice
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     biddable, startprice
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                biddable, startprice, biddable:D.nwrds.unq.log, biddable:D.T.like, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                               biddable, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.npnct08.log, D.T.condit, color.fctr, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.4877665
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.8775832
## Max.cor.Y.rpart                         3   0.8255676
## Max.cor.Y.glm                           1   0.8592854
## Interact.High.cor.Y.glm                 1   0.8593261
## Low.cor.X.glm                           1   0.8890475
## All.X.glm                               1   0.8974946
## All.X.bayesglm                          1   0.8942546
## All.X.no.rnorm.rpart                    3   0.8255676
## All.X.no.rnorm.rf                       3   0.9998736
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6313559
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.7977011
## Max.cor.Y.rpart                              0.5       0.7726218
## Max.cor.Y.glm                                0.4       0.7771679
## Interact.High.cor.Y.glm                      0.4       0.7753304
## Low.cor.X.glm                                0.5       0.7954023
## All.X.glm                                    0.5       0.8009153
## All.X.bayesglm                               0.5       0.7986111
## All.X.no.rnorm.rpart                         0.5       0.7726218
## All.X.no.rnorm.rf                            0.5       0.9944259
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.5386997     0.0000000   0.5000000
## Random.myrandom_classfr          0.4613003     0.0000000   0.5196257
## Max.cor.Y.cv.0.rpart             0.5386997     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8183695     0.6331346   0.7908880
## Max.cor.Y.rpart                  0.7894737     0.5719410   0.7965723
## Max.cor.Y.glm                    0.7915377     0.5781217   0.8223119
## Interact.High.cor.Y.glm          0.7894737     0.5739487   0.8220844
## Low.cor.X.glm                    0.7636739     0.5225041   0.8242202
## All.X.glm                        0.7574819     0.5103726   0.8156723
## All.X.bayesglm                   0.7678019     0.5312525   0.8193270
## All.X.no.rnorm.rpart             0.7853457     0.5603172   0.7965723
## All.X.no.rnorm.rf                0.8070175     0.6082120   0.8221603
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6329502
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.3       0.7294982
## Max.cor.Y.rpart                              0.5       0.7478043
## Max.cor.Y.glm                                0.5       0.7427854
## Interact.High.cor.Y.glm                      0.4       0.7434679
## Low.cor.X.glm                                0.4       0.7418605
## All.X.glm                                    0.4       0.7374562
## All.X.bayesglm                               0.4       0.7363531
## All.X.no.rnorm.rpart                         0.5       0.7478043
## All.X.no.rnorm.rf                            0.4       0.7458432
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.5369955     0.0000000
## Random.myrandom_classfr          0.4630045     0.0000000
## Max.cor.Y.cv.0.rpart             0.5369955     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart        0.7522422     0.5010074
## Max.cor.Y.rpart                  0.7746637     0.5446437
## Max.cor.Y.glm                    0.7701794     0.5355819
## Interact.High.cor.Y.glm          0.7578475     0.5143253
## Low.cor.X.glm                    0.7511211     0.5023247
## All.X.glm                        0.7477578     0.4953483
## All.X.bayesglm                   0.7455157     0.4912002
## All.X.no.rnorm.rpart             0.7746637     0.5446437
## All.X.no.rnorm.rf                0.7600897     0.5188223
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         2.58397933           333.3333333
## Random.myrandom_classfr                   3.96825397          1000.0000000
## Max.cor.Y.cv.0.rpart                      1.47275405            90.9090909
## Max.cor.Y.cv.0.cp.0.rpart                 2.12765957           125.0000000
## Max.cor.Y.rpart                           1.05374078            90.9090909
## Max.cor.Y.glm                             1.08342362            90.9090909
## Interact.High.cor.Y.glm                   1.06269926            66.6666667
## Low.cor.X.glm                             0.80971660             7.7519380
## All.X.glm                                 0.73206442             5.6818182
## All.X.bayesglm                            0.41999160             3.7174721
## All.X.no.rnorm.rpart                      0.65316786            15.6250000
## All.X.no.rnorm.rf                         0.06119577             0.2093802
##                           inv.aic.fit
## MFO.myMFO_classfr                  NA
## Random.myrandom_classfr            NA
## Max.cor.Y.cv.0.rpart               NA
## Max.cor.Y.cv.0.cp.0.rpart          NA
## Max.cor.Y.rpart                    NA
## Max.cor.Y.glm             0.001095878
## Interact.High.cor.Y.glm   0.001089002
## Low.cor.X.glm             0.001069928
## All.X.glm                 0.001064791
## All.X.bayesglm            0.001052257
## All.X.no.rnorm.rpart               NA
## All.X.no.rnorm.rf                  NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

```
## Warning: Removed 87 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 5            Max.cor.Y.rpart        0.7746637   0.7965723     0.5446437
## 11      All.X.no.rnorm.rpart        0.7746637   0.7965723     0.5446437
## 6              Max.cor.Y.glm        0.7701794   0.8223119     0.5355819
## 12         All.X.no.rnorm.rf        0.7600897   0.8221603     0.5188223
## 7    Interact.High.cor.Y.glm        0.7578475   0.8220844     0.5143253
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7522422   0.7908880     0.5010074
## 8              Low.cor.X.glm        0.7511211   0.8242202     0.5023247
## 9                  All.X.glm        0.7477578   0.8156723     0.4953483
## 10            All.X.bayesglm        0.7455157   0.8193270     0.4912002
## 1          MFO.myMFO_classfr        0.5369955   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5369955   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4630045   0.5196257     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 5           NA                    0.5
## 11          NA                    0.5
## 6     912.5107                    0.5
## 12          NA                    0.4
## 7     918.2717                    0.4
## 4           NA                    0.3
## 8     934.6420                    0.4
## 9     939.1513                    0.4
## 10    950.3381                    0.4
## 1           NA                    0.5
## 3           NA                    0.5
## 2           NA                    0.4
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 38 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Max.cor.Y.rpart"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
#     if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
#         warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
#         glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
#     }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_2-4.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 969 
## 
##            CP nsplit rel error
## 1 0.525727069      0 1.0000000
## 2 0.017897092      1 0.4742729
## 3 0.007270694      3 0.4384787
## 
## Variable importance
##   biddable startprice 
##         57         43 
## 
## Node number 1: 969 observations,    complexity param=0.5257271
##   predicted class=N  expected loss=0.4613003  P(node) =1
##     class counts:   522   447
##    probabilities: 0.539 0.461 
##   left son=2 (530 obs) right son=3 (439 obs)
##   Primary splits:
##       biddable   < 0.5     to the left,  improve=150.6565, (0 missing)
##       startprice < 126.475 to the right, improve=127.7985, (0 missing)
##   Surrogate splits:
##       startprice < 152.995 to the right, agree=0.76, adj=0.469, (0 split)
## 
## Node number 2: 530 observations
##   predicted class=N  expected loss=0.2075472  P(node) =0.5469556
##     class counts:   420   110
##    probabilities: 0.792 0.208 
## 
## Node number 3: 439 observations,    complexity param=0.01789709
##   predicted class=Y  expected loss=0.2323462  P(node) =0.4530444
##     class counts:   102   337
##    probabilities: 0.232 0.768 
##   left son=6 (154 obs) right son=7 (285 obs)
##   Primary splits:
##       startprice < 127.5   to the right, improve=39.11474, (0 missing)
## 
## Node number 6: 154 observations,    complexity param=0.01789709
##   predicted class=N  expected loss=0.4805195  P(node) =0.1589267
##     class counts:    80    74
##    probabilities: 0.519 0.481 
##   left son=12 (24 obs) right son=13 (130 obs)
##   Primary splits:
##       startprice < 414.995 to the right, improve=5.601066, (0 missing)
## 
## Node number 7: 285 observations
##   predicted class=Y  expected loss=0.07719298  P(node) =0.2941176
##     class counts:    22   263
##    probabilities: 0.077 0.923 
## 
## Node number 12: 24 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.0247678
##     class counts:    20     4
##    probabilities: 0.833 0.167 
## 
## Node number 13: 130 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.1341589
##     class counts:    60    70
##    probabilities: 0.462 0.538 
## 
## n= 969 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 969 447 N (0.53869969 0.46130031)  
##    2) biddable< 0.5 530 110 N (0.79245283 0.20754717) *
##    3) biddable>=0.5 439 102 Y (0.23234624 0.76765376)  
##      6) startprice>=127.5 154  74 N (0.51948052 0.48051948)  
##       12) startprice>=414.995 24   4 N (0.83333333 0.16666667) *
##       13) startprice< 414.995 130  60 Y (0.46153846 0.53846154) *
##      7) startprice< 127.5 285  22 Y (0.07719298 0.92280702) *
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##            importance Max.cor.Y.rpart.importance
## startprice        100                        100
## biddable            0                          0
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
    
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))

#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}

if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_2-5.png) ![](ebayipads_oobsmpl_files/figure-html/fit.models_2-6.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Max.cor.Y.rpart.prob
## 27      10027         Y                              0.2075472
## 68      10068         Y                              0.2075472
## 328     10328         Y                              0.2075472
## 763     10763         Y                              0.2075472
## 875     10875         Y                              0.2075472
## 1535    11535         Y                              0.2075472
## 20      10020         N                              0.2075472
## 1283    11283         N                              0.2075472
## 631     10631         Y                              0.9228070
## 801     10801         N                              0.5384615
## 941     10941         N                              0.5384615
## 182     10182         N                              0.9228070
## 526     10526         N                              0.9228070
## 841     10841         N                              0.9228070
## 1622    11622         N                              0.9228070
##      sold.fctr.predict.Max.cor.Y.rpart
## 27                                   N
## 68                                   N
## 328                                  N
## 763                                  N
## 875                                  N
## 1535                                 N
## 20                                   N
## 1283                                 N
## 631                                  Y
## 801                                  Y
## 941                                  Y
## 182                                  Y
## 526                                  Y
## 841                                  Y
## 1622                                 Y
##      sold.fctr.predict.Max.cor.Y.rpart.accurate
## 27                                        FALSE
## 68                                        FALSE
## 328                                       FALSE
## 763                                       FALSE
## 875                                       FALSE
## 1535                                      FALSE
## 20                                         TRUE
## 1283                                       TRUE
## 631                                        TRUE
## 801                                       FALSE
## 941                                       FALSE
## 182                                       FALSE
## 526                                       FALSE
## 841                                       FALSE
## 1622                                      FALSE
##      sold.fctr.predict.Max.cor.Y.rpart.error .label
## 27                               -0.29245283  10027
## 68                               -0.29245283  10068
## 328                              -0.29245283  10328
## 763                              -0.29245283  10763
## 875                              -0.29245283  10875
## 1535                             -0.29245283  11535
## 20                                0.00000000  10020
## 1283                              0.00000000  11283
## 631                               0.00000000  10631
## 801                               0.03846154  10801
## 941                               0.03846154  10941
## 182                               0.42280702  10182
## 526                               0.42280702  10526
## 841                               0.42280702  10841
## 1622                              0.42280702  11622
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Max.cor.Y.rpart.prob
## 297     10297         Y                              0.1666667
## 775     10775         Y                              0.1666667
## 1042    11042         Y                              0.1666667
## 1053    11053         Y                              0.1666667
## 1300    11300         Y                              0.1666667
## 27      10027         Y                              0.2075472
##      sold.fctr.predict.Max.cor.Y.rpart
## 297                                  N
## 775                                  N
## 1042                                 N
## 1053                                 N
## 1300                                 N
## 27                                   N
##      sold.fctr.predict.Max.cor.Y.rpart.accurate
## 297                                       FALSE
## 775                                       FALSE
## 1042                                      FALSE
## 1053                                      FALSE
## 1300                                      FALSE
## 27                                        FALSE
##      sold.fctr.predict.Max.cor.Y.rpart.error
## 297                               -0.3333333
## 775                               -0.3333333
## 1042                              -0.3333333
## 1053                              -0.3333333
## 1300                              -0.3333333
## 27                                -0.2924528
##      UniqueID sold.fctr sold.fctr.predict.Max.cor.Y.rpart.prob
## 1173    11173         Y                              0.2075472
## 683     10683         N                              0.5384615
## 1004    11004         N                              0.5384615
## 1622    11622         N                              0.9228070
## 567     10567         N                              0.9228070
## 1700    11700         N                              0.9228070
##      sold.fctr.predict.Max.cor.Y.rpart
## 1173                                 N
## 683                                  Y
## 1004                                 Y
## 1622                                 Y
## 567                                  Y
## 1700                                 Y
##      sold.fctr.predict.Max.cor.Y.rpart.accurate
## 1173                                      FALSE
## 683                                       FALSE
## 1004                                      FALSE
## 1622                                      FALSE
## 567                                       FALSE
## 1700                                      FALSE
##      sold.fctr.predict.Max.cor.Y.rpart.error
## 1173                             -0.29245283
## 683                               0.03846154
## 1004                              0.03846154
## 1622                              0.42280702
## 567                               0.42280702
## 1700                              0.42280702
##      UniqueID sold.fctr sold.fctr.predict.Max.cor.Y.rpart.prob
## 1704    11704         N                               0.922807
## 413     10413         N                               0.922807
## 1243    11243         N                               0.922807
## 1602    11602         N                               0.922807
## 1700    11700         N                               0.922807
## 1769    11769         N                               0.922807
##      sold.fctr.predict.Max.cor.Y.rpart
## 1704                                 Y
## 413                                  Y
## 1243                                 Y
## 1602                                 Y
## 1700                                 Y
## 1769                                 Y
##      sold.fctr.predict.Max.cor.Y.rpart.accurate
## 1704                                      FALSE
## 413                                       FALSE
## 1243                                      FALSE
## 1602                                      FALSE
## 1700                                      FALSE
## 1769                                      FALSE
##      sold.fctr.predict.Max.cor.Y.rpart.error
## 1704                                0.422807
## 413                                 0.422807
## 1243                                0.422807
## 1602                                0.422807
## 1700                                0.422807
## 1769                                0.422807
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_2-7.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
# FN_OOB_ids <- c(4721, 4020, 693, 92)
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_feats_df$id[1:5]])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])

write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

# write.csv(glb_chunks_df, paste0(glb_out_pfx, tail(glb_chunks_df, 1)$label, "_",
#                                 tail(glb_chunks_df, 1)$step_minor,  "_chunks1.csv"),
#           row.names=FALSE)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn     end elapsed
## 12 fit.models          7          2 106.32 119.769  13.449
## 13 fit.models          7          3 119.77      NA      NA
```


```r
    if (sum(is.na(glb_allobs_df$D.P.http)) > 0)
        stop("fit.models_3: Why is this happening ?")
```

```
## Warning in is.na(glb_allobs_df$D.P.http): is.na() applied to non-(list or
## vector) of type 'NULL'
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "sold.fctr.predict.Max.cor.Y.rpart.prob"    
## [2] "sold.fctr.predict.Max.cor.Y.rpart"         
## [3] "sold.fctr.predict.Max.cor.Y.rpart.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](ebayipads_oobsmpl_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 119.770 123.636   3.866
## 14 fit.data.training          8          0 123.636      NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

    if (sum(is.na(glb_allobs_df$D.P.http)) > 0)
        stop("fit.data.training_0: Why is this happening ?")
```

```
## Warning in is.na(glb_allobs_df$D.P.http): is.na() applied to non-(list or
## vector) of type 'NULL'
```

```r
# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.rpart"
## [1] "    indep_vars: biddable, startprice"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_oobsmpl_files/figure-html/fit.data.training_0-1.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 1861 
## 
##            CP nsplit rel error
## 1 0.515116279      0 1.0000000
## 2 0.011627907      1 0.4848837
## 3 0.007270694      3 0.4616279
## 
## Variable importance
##   biddable startprice 
##         59         41 
## 
## Node number 1: 1861 observations,    complexity param=0.5151163
##   predicted class=N  expected loss=0.4621171  P(node) =1
##     class counts:  1001   860
##    probabilities: 0.538 0.462 
##   left son=2 (1024 obs) right son=3 (837 obs)
##   Primary splits:
##       biddable   < 0.5     to the left,  improve=278.4233, (0 missing)
##       startprice < 100.5   to the right, improve=214.6830, (0 missing)
##   Surrogate splits:
##       startprice < 100.5   to the right, agree=0.75, adj=0.443, (0 split)
## 
## Node number 2: 1024 observations
##   predicted class=N  expected loss=0.2148438  P(node) =0.5502418
##     class counts:   804   220
##    probabilities: 0.785 0.215 
## 
## Node number 3: 837 observations,    complexity param=0.01162791
##   predicted class=Y  expected loss=0.2353644  P(node) =0.4497582
##     class counts:   197   640
##    probabilities: 0.235 0.765 
##   left son=6 (295 obs) right son=7 (542 obs)
##   Primary splits:
##       startprice < 139.995 to the right, improve=62.99321, (0 missing)
## 
## Node number 6: 295 observations,    complexity param=0.01162791
##   predicted class=Y  expected loss=0.4983051  P(node) =0.1585169
##     class counts:   147   148
##    probabilities: 0.498 0.502 
##   left son=12 (38 obs) right son=13 (257 obs)
##   Primary splits:
##       startprice < 413.495 to the right, improve=6.11944, (0 missing)
## 
## Node number 7: 542 observations
##   predicted class=Y  expected loss=0.09225092  P(node) =0.2912413
##     class counts:    50   492
##    probabilities: 0.092 0.908 
## 
## Node number 12: 38 observations
##   predicted class=N  expected loss=0.2368421  P(node) =0.02041913
##     class counts:    29     9
##    probabilities: 0.763 0.237 
## 
## Node number 13: 257 observations
##   predicted class=Y  expected loss=0.459144  P(node) =0.1380978
##     class counts:   118   139
##    probabilities: 0.459 0.541 
## 
## n= 1861 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 1861 860 N (0.53788286 0.46211714)  
##    2) biddable< 0.5 1024 220 N (0.78515625 0.21484375) *
##    3) biddable>=0.5 837 197 Y (0.23536440 0.76463560)  
##      6) startprice>=139.995 295 147 Y (0.49830508 0.50169492)  
##       12) startprice>=413.495 38   9 N (0.76315789 0.23684211) *
##       13) startprice< 413.495 257 118 Y (0.45914397 0.54085603) *
##      7) startprice< 139.995 542  50 Y (0.09225092 0.90774908) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_oobsmpl_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold   f.score
## 1        0.0 0.6321205
## 2        0.1 0.6321205
## 3        0.2 0.6321205
## 4        0.3 0.7606992
## 5        0.4 0.7606992
## 6        0.5 0.7606992
## 7        0.6 0.7018545
## 8        0.7 0.7018545
## 9        0.8 0.7018545
## 10       0.9 0.7018545
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Final.rpart.N
## 1         N                             833
## 2         Y                             229
##   sold.fctr.predict.Final.rpart.Y
## 1                             168
## 2                             631
##          Prediction
## Reference   N   Y
##         N 833 168
##         Y 229 631
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.866738e-01   5.687299e-01   7.673485e-01   8.050912e-01   5.378829e-01 
## AccuracyPValue  McnemarPValue 
##  1.194566e-110   2.601200e-03
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](ebayipads_oobsmpl_files/figure-html/fit.data.training_0-3.png) 

```
##      model_id model_method                feats max.nTuningRuns
## 1 Final.rpart        rpart biddable, startprice               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.982                 0.015   0.8131247
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5       0.7606992        0.7802279
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.7673485             0.8050912     0.5532193
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0165079      0.03542625
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 123.636 128.431   4.795
## 15 fit.data.training          8          1 128.431      NA      NA
```

```r
#```

#```{r fit.data.training_1, cache=FALSE}
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.5
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##            Max.cor.Y.rpart.importance importance Final.rpart.importance
## startprice                        100        100                    100
## biddable                            0          0                      0
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](ebayipads_oobsmpl_files/figure-html/fit.data.training_0-4.png) ![](ebayipads_oobsmpl_files/figure-html/fit.data.training_0-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## 3       10003         Y                          0.2148438
## 11      10011         Y                          0.2148438
## 14      10014         Y                          0.2148438
## 15      10015         Y                          0.2148438
## 17      10017         Y                          0.2148438
## 19      10019         Y                          0.2148438
## 92      10092         Y                          0.2368421
## 1       10001         N                          0.2148438
## 2       10002         Y                          0.9077491
## 91      10091         Y                          0.9077491
## 1397    11397         N                          0.2148438
## 103     10103         N                          0.5408560
## 112     10112         N                          0.5408560
## 120     10120         N                          0.5408560
## 135     10135         N                          0.5408560
## 127     10127         N                          0.9077491
##      sold.fctr.predict.Final.rpart sold.fctr.predict.Final.rpart.accurate
## 3                                N                                  FALSE
## 11                               N                                  FALSE
## 14                               N                                  FALSE
## 15                               N                                  FALSE
## 17                               N                                  FALSE
## 19                               N                                  FALSE
## 92                               N                                  FALSE
## 1                                N                                   TRUE
## 2                                Y                                   TRUE
## 91                               Y                                   TRUE
## 1397                             N                                   TRUE
## 103                              Y                                  FALSE
## 112                              Y                                  FALSE
## 120                              Y                                  FALSE
## 135                              Y                                  FALSE
## 127                              Y                                  FALSE
##      sold.fctr.predict.Final.rpart.error .label
## 3                            -0.28515625  10003
## 11                           -0.28515625  10011
## 14                           -0.28515625  10014
## 15                           -0.28515625  10015
## 17                           -0.28515625  10017
## 19                           -0.28515625  10019
## 92                           -0.26315789  10092
## 1                             0.00000000  10001
## 2                             0.00000000  10002
## 91                            0.00000000  10091
## 1397                          0.00000000  11397
## 103                           0.04085603  10103
## 112                           0.04085603  10112
## 120                           0.04085603  10120
## 135                           0.04085603  10135
## 127                           0.40774908  10127
## [1] "Inaccurate: "
##    UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## 3     10003         Y                          0.2148438
## 11    10011         Y                          0.2148438
## 14    10014         Y                          0.2148438
## 15    10015         Y                          0.2148438
## 17    10017         Y                          0.2148438
## 19    10019         Y                          0.2148438
##    sold.fctr.predict.Final.rpart sold.fctr.predict.Final.rpart.accurate
## 3                              N                                  FALSE
## 11                             N                                  FALSE
## 14                             N                                  FALSE
## 15                             N                                  FALSE
## 17                             N                                  FALSE
## 19                             N                                  FALSE
##    sold.fctr.predict.Final.rpart.error
## 3                           -0.2851562
## 11                          -0.2851562
## 14                          -0.2851562
## 15                          -0.2851562
## 17                          -0.2851562
## 19                          -0.2851562
##      UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## 686     10686         Y                          0.2148438
## 962     10962         Y                          0.2148438
## 297     10297         Y                          0.2368421
## 602     10602         N                          0.5408560
## 1777    11777         N                          0.5408560
## 416     10416         N                          0.9077491
##      sold.fctr.predict.Final.rpart sold.fctr.predict.Final.rpart.accurate
## 686                              N                                  FALSE
## 962                              N                                  FALSE
## 297                              N                                  FALSE
## 602                              Y                                  FALSE
## 1777                             Y                                  FALSE
## 416                              Y                                  FALSE
##      sold.fctr.predict.Final.rpart.error
## 686                          -0.28515625
## 962                          -0.28515625
## 297                          -0.26315789
## 602                           0.04085603
## 1777                          0.04085603
## 416                           0.40774908
##      UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## 1718    11718         N                          0.9077491
## 1741    11741         N                          0.9077491
## 1748    11748         N                          0.9077491
## 1769    11769         N                          0.9077491
## 1796    11796         N                          0.9077491
## 1836    11836         N                          0.9077491
##      sold.fctr.predict.Final.rpart sold.fctr.predict.Final.rpart.accurate
## 1718                             Y                                  FALSE
## 1741                             Y                                  FALSE
## 1748                             Y                                  FALSE
## 1769                             Y                                  FALSE
## 1796                             Y                                  FALSE
## 1836                             Y                                  FALSE
##      sold.fctr.predict.Final.rpart.error
## 1718                           0.4077491
## 1741                           0.4077491
## 1748                           0.4077491
## 1769                           0.4077491
## 1796                           0.4077491
## 1836                           0.4077491
```

![](ebayipads_oobsmpl_files/figure-html/fit.data.training_0-6.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

# print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])

print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "sold.fctr.predict.Final.rpart.prob"
## [2] "sold.fctr.predict.Final.rpart"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](ebayipads_oobsmpl_files/figure-html/fit.data.training_0-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 128.431 131.535   3.104
## 16  predict.data.new          9          0 131.535      NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
# sav_newobs_df <- glb_newobs_df
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newobs_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.5
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

![](ebayipads_oobsmpl_files/figure-html/predict.data.new-1.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

![](ebayipads_oobsmpl_files/figure-html/predict.data.new-2.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## 1862    11862      <NA>                          0.2148438
## 1865    11865      <NA>                          0.9077491
## 1891    11891      <NA>                          0.9077491
## 2625    12625      <NA>                          0.2148438
##      sold.fctr.predict.Final.rpart sold.fctr.predict.Final.rpart.accurate
## 1862                             N                                     NA
## 1865                             Y                                     NA
## 1891                             Y                                     NA
## 2625                             N                                     NA
##      sold.fctr.predict.Final.rpart.error .label
## 1862                                   0  11862
## 1865                                   0  11865
## 1891                                   0  11891
## 2625                                   0  12625
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## NA         NA      <NA>                                 NA
## NA.1       NA      <NA>                                 NA
## NA.2       NA      <NA>                                 NA
## NA.3       NA      <NA>                                 NA
## NA.4       NA      <NA>                                 NA
## NA.5       NA      <NA>                                 NA
##      sold.fctr.predict.Final.rpart sold.fctr.predict.Final.rpart.accurate
## NA                            <NA>                                     NA
## NA.1                          <NA>                                     NA
## NA.2                          <NA>                                     NA
## NA.3                          <NA>                                     NA
## NA.4                          <NA>                                     NA
## NA.5                          <NA>                                     NA
##      sold.fctr.predict.Final.rpart.error
## NA                                    NA
## NA.1                                  NA
## NA.2                                  NA
## NA.3                                  NA
## NA.4                                  NA
## NA.5                                  NA
##        UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## NA.264       NA      <NA>                                 NA
## NA.289       NA      <NA>                                 NA
## NA.453       NA      <NA>                                 NA
## NA.566       NA      <NA>                                 NA
## NA.602       NA      <NA>                                 NA
## NA.645       NA      <NA>                                 NA
##        sold.fctr.predict.Final.rpart
## NA.264                          <NA>
## NA.289                          <NA>
## NA.453                          <NA>
## NA.566                          <NA>
## NA.602                          <NA>
## NA.645                          <NA>
##        sold.fctr.predict.Final.rpart.accurate
## NA.264                                     NA
## NA.289                                     NA
## NA.453                                     NA
## NA.566                                     NA
## NA.602                                     NA
## NA.645                                     NA
##        sold.fctr.predict.Final.rpart.error
## NA.264                                  NA
## NA.289                                  NA
## NA.453                                  NA
## NA.566                                  NA
## NA.602                                  NA
## NA.645                                  NA
##        UniqueID sold.fctr sold.fctr.predict.Final.rpart.prob
## NA.792       NA      <NA>                                 NA
## NA.793       NA      <NA>                                 NA
## NA.794       NA      <NA>                                 NA
## NA.795       NA      <NA>                                 NA
## NA.796       NA      <NA>                                 NA
## NA.797       NA      <NA>                                 NA
##        sold.fctr.predict.Final.rpart
## NA.792                          <NA>
## NA.793                          <NA>
## NA.794                          <NA>
## NA.795                          <NA>
## NA.796                          <NA>
## NA.797                          <NA>
##        sold.fctr.predict.Final.rpart.accurate
## NA.792                                     NA
## NA.793                                     NA
## NA.794                                     NA
## NA.795                                     NA
## NA.796                                     NA
## NA.797                                     NA
##        sold.fctr.predict.Final.rpart.error
## NA.792                                  NA
## NA.793                                  NA
## NA.794                                  NA
## NA.795                                  NA
## NA.796                                  NA
## NA.797                                  NA
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

![](ebayipads_oobsmpl_files/figure-html/predict.data.new-3.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
#     submit_df <- glb_newobs_df[, c(paste0(glb_rsp_var_out, glb_fin_mdl_id)), FALSE]
#     names(submit_df)[1] <- "BDscience"
#     submit_df$BDscience <- as.numeric(submit_df$BDscience) - 1
#     #submit_df <-rbind(submit_df, data.frame(bdanalytics=c(" ")))
#     print("Submission Stats:")
#     print(table(submit_df$BDscience, useNA = "ifany"))
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
```

```
## [1] 0.5
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Max.cor.Y.rpart"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.rpart"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 969  63
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 5            Max.cor.Y.rpart        0.7746637   0.7965723     0.5446437
## 11      All.X.no.rnorm.rpart        0.7746637   0.7965723     0.5446437
## 6              Max.cor.Y.glm        0.7701794   0.8223119     0.5355819
## 12         All.X.no.rnorm.rf        0.7600897   0.8221603     0.5188223
## 7    Interact.High.cor.Y.glm        0.7578475   0.8220844     0.5143253
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7522422   0.7908880     0.5010074
## 8              Low.cor.X.glm        0.7511211   0.8242202     0.5023247
## 9                  All.X.glm        0.7477578   0.8156723     0.4953483
## 10            All.X.bayesglm        0.7455157   0.8193270     0.4912002
## 1          MFO.myMFO_classfr        0.5369955   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5369955   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4630045   0.5196257     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 5           NA                    0.5
## 11          NA                    0.5
## 6     912.5107                    0.5
## 12          NA                    0.4
## 7     918.2717                    0.4
## 4           NA                    0.3
## 8     934.6420                    0.4
## 9     939.1513                    0.4
## 10    950.3381                    0.4
## 1           NA                    0.5
## 3           NA                    0.5
## 2           NA                    0.4
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
        print(sprintf("%s new confusion matrix & accuracy: ", glb_fin_mdl_id))
        print(t(confusionMatrix(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)], 
                                glb_newobs_df[, glb_rsp_var])$table))
    }    

}    
```

```
## [1] "Max.cor.Y.rpart OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 393  86
##         Y 115 298
##          prdline.my .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 8         iPad mini    126    114     0.14285714    0.141255605
## 2            iPad 2    171    154     0.19298246    0.191704036
## 12          Unknown     97     87     0.10902256    0.108744395
## 1            iPad 1     99     89     0.11152882    0.110986547
## 3            iPad 3     61     55     0.06892231    0.068385650
## 4            iPad 4     76     68     0.08521303    0.085201794
## 9       iPad mini 2     63     56     0.07017544    0.070627803
## 7        iPad Air 2     69     62     0.07769424    0.077354260
## 6          iPad Air     84     75     0.09398496    0.094170404
## 10      iPad mini 3     43     38     0.04761905    0.048206278
## 11 iPad mini Retina      2     NA             NA    0.002242152
## 5            iPad 5      1     NA             NA    0.001121076
##    accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 8                  36                90        0.7142857
## 2                  35               136        0.7953216
## 12                 24                73        0.7525773
## 1                  18                81        0.8181818
## 3                  17                44        0.7213115
## 4                  17                59        0.7763158
## 9                  16                47        0.7460317
## 7                  15                54        0.7826087
## 6                  12                72        0.8571429
## 10                 10                33        0.7674419
## 11                  1                 1        0.5000000
## 5                   0                 1        1.0000000
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

# if (glb_is_classification) {
#     print("FN_OOB_ids:")
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         glb_txt_vars])
#     print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
#                                 union(myfind_chr_cols_df(glb_OOBobs_df),
#                     grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
# }

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##            Max.cor.Y.rpart.importance importance Final.rpart.importance
## startprice                        100        100                    100
## biddable                            0          0                      0
```

```r
print("glb_newobs_df prediction stats:")
```

```
## [1] "glb_newobs_df prediction stats:"
```

```r
print(myplot_histogram(glb_newobs_df, paste0(glb_rsp_var_out, glb_fin_mdl_id)))
```

![](ebayipads_oobsmpl_files/figure-html/predict.data.new-4.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 439 359
```

```r
# players_df <- data.frame(id=c("Chavez", "Giambi", "Menechino", "Myers", "Pena"),
#                          OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
#                          SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
#                         cost=c(1400000, 1065000, 295000, 800000, 300000))
# players_df$RS.predict <- predict(glb_models_lst[[csm_mdl_id]], players_df)
# print(orderBy(~ -RS.predict, players_df))

if (length(diff <- setdiff(names(glb_trnobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

if (length(diff <- setdiff(names(glb_fitobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
if (length(diff <- setdiff(names(glb_OOBobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
if (length(diff <- setdiff(names(glb_newobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)

# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn     end elapsed
## 16     predict.data.new          9          0 131.535 134.904   3.369
## 17 display.session.info         10          0 134.904      NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 11              fit.models          7          1  65.927 106.320  40.393
## 10              fit.models          7          0  41.384  65.926  24.542
## 5         extract.features          3          0  16.448  37.899  21.451
## 12              fit.models          7          2 106.320 119.769  13.449
## 2             inspect.data          2          0  10.248  15.538   5.290
## 14       fit.data.training          8          0 123.636 128.431   4.795
## 13              fit.models          7          3 119.770 123.636   3.866
## 16        predict.data.new          9          0 131.535 134.904   3.369
## 15       fit.data.training          8          1 128.431 131.535   3.104
## 8          select.features          5          0  38.973  40.432   1.460
## 9  partition.data.training          6          0  40.433  41.384   0.951
## 6             cluster.data          4          0  37.900  38.828   0.928
## 3               scrub.data          2          1  15.538  16.366   0.828
## 1              import.data          1          0   9.832  10.247   0.416
## 7      manage.missing.data          4          1  38.829  38.973   0.144
## 4           transform.data          2          2  16.367  16.448   0.081
##    duration
## 11   40.393
## 10   24.542
## 5    21.451
## 12   13.449
## 2     5.290
## 14    4.795
## 13    3.866
## 16    3.369
## 15    3.104
## 8     1.459
## 9     0.951
## 6     0.928
## 3     0.828
## 1     0.415
## 7     0.144
## 4     0.081
## [1] "Total Elapsed Time: 134.904 secs"
```

![](ebayipads_oobsmpl_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.1 (2015-06-18)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.4 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] gdata_2.17.0        randomForest_4.6-10 arm_1.8-6          
##  [4] lme4_1.1-8          Matrix_1.2-2        MASS_7.3-42        
##  [7] rpart.plot_1.5.2    rpart_4.1-10        ROCR_1.0-7         
## [10] gplots_2.17.0       sampling_2.7        tm_0.6-2           
## [13] NLP_0.1-8           stringr_1.0.0       dplyr_0.4.2        
## [16] plyr_1.8.3          sqldf_0.4-10        RSQLite_1.0.0      
## [19] DBI_0.3.1           gsubfn_0.6-6        proto_0.3-10       
## [22] reshape2_1.4.1      doMC_1.3.3          iterators_1.0.7    
## [25] foreach_1.4.2       doBy_4.5-13         survival_2.38-3    
## [28] caret_6.0-47        ggplot2_1.0.1       lattice_0.20-33    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6         class_7.3-13        gtools_3.5.0       
##  [4] assertthat_0.1      digest_0.6.8        slam_0.1-32        
##  [7] R6_2.1.0            BradleyTerry2_1.0-6 chron_2.3-47       
## [10] coda_0.17-1         evaluate_0.7        e1071_1.6-4        
## [13] lazyeval_0.1.10     minqa_1.2.4         SparseM_1.6        
## [16] car_2.0-25          nloptr_1.0.4        rmarkdown_0.7      
## [19] labeling_0.3        splines_3.2.1       munsell_0.4.2      
## [22] compiler_3.2.1      mgcv_1.8-6          htmltools_0.2.6    
## [25] nnet_7.3-10         lpSolve_5.6.11      codetools_0.2-14   
## [28] brglm_0.5-9         bitops_1.0-6        nlme_3.1-121       
## [31] gtable_0.1.2        magrittr_1.5        formatR_1.2        
## [34] scales_0.2.5        KernSmooth_2.23-15  stringi_0.5-5      
## [37] RColorBrewer_1.1-2  tools_3.2.1         abind_1.4-3        
## [40] pbkrtest_0.4-2      yaml_2.1.13         colorspace_1.2-6   
## [43] caTools_1.17.1      knitr_1.10.5        quantreg_5.11
```
