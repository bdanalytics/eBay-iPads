# eBay:iPads:: sold classification:: category
bdanalytics  

**  **    
**Date: (Sat) Jul 18, 2015**    

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
    Low.cor.X.glm: Leaderboard: 0.83402
        newobs_tbl=[N=440, Y=358]; submit_filename=oobsmpl_Final_glm_submit
        OOB_conf_mtrx=[YN=114, NY=84]=198; max.Accuracy.OOB=0.7780;
            opt.prob.threshold.OOB=0.5
            startprice=100.00; biddable=93.87; prdline.my=60.48; D.sum.TfIdf=; 
            D.T.condition=8.69; D.T.screen=7.96; D.T.use=7.50; D.T.veri=; D.T.scratch=;

category: -> 
    Low.cor.X.glm: Leaderboard: 0.82381
        newobs_tbl=[N=470, Y=328]; submit_filename=category_Final_glm_submit
        OOB_conf_mtrx=[YN=119, NY=57]=176; max.Accuracy.OOB=0.8011;
            opt.prob.threshold.OOB=0.6
            startprice=100.00; biddable=79.19; prdline.my=55.22; D.sum.TfIdf=; 
            D.T.ipad=27.05; D.T.like=21.44; D.T.box=20.67; D.T.condition=; D.T.screen=; 

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
glb_out_pfx <- "category_"
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

![](ebayipads_category_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 8.788  NA      NA
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
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 8.788 9.198    0.41
## 2 inspect.data          2          0 9.198    NA      NA
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

![](ebayipads_category_files/figure-html/inspect.data-1.png) 

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

![](ebayipads_category_files/figure-html/inspect.data-2.png) 

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

![](ebayipads_category_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: startprice"
```

![](ebayipads_category_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_category_files/figure-html/inspect.data-5.png) 

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
## 2 inspect.data          2          0  9.198 20.508   11.31
## 3   scrub.data          2          1 20.508     NA      NA
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
## 3     scrub.data          2          1 20.508 21.565   1.057
## 4 transform.data          2          2 21.566     NA      NA
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
## 4   transform.data          2          2 21.566 21.662   0.096
## 5 extract.features          3          0 21.663     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor   bgn end elapsed
## 1 extract.features_bgn          1          0 21.67  NA      NA
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
## 1                extract.features_bgn          1          0 21.670 21.685
## 2 extract.features_factorize.str.vars          2          0 21.686     NA
##   elapsed
## 1   0.015
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
## 2 extract.features_factorize.str.vars          2          0 21.686 24.628
## 3       extract.features_process.text          3          0 24.629     NA
##   elapsed
## 2   2.943
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
## 3          0 24.629 27.358   2.729
## 4          1 27.358     NA      NA
## [1] "Remaining compound terms in description: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn    end elapsed
## 4          1 27.358 27.364   0.006
## 5          0 27.364     NA      NA
## [1] "Building glb_corpus_lst..."
##                           label step_major step_minor    bgn    end
## 5 extract.features_build.corpus          4          0 27.364 28.011
## 6  extract.features_extract.DTM          5          0 28.012     NA
##   elapsed
## 5   0.648
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
## 6 extract.features_extract.DTM          5          0 28.012 31.548   3.536
## 7  extract.features_report.DTM          6          0 31.549     NA      NA
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

![](ebayipads_category_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_category_files/figure-html/extract.features-2.png) ![](ebayipads_category_files/figure-html/extract.features-3.png) 

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 31.549 34.082   2.533
## 8   extract.features_bind.DTM          7          0 34.083     NA      NA
## [1] "Binding DTM for description..."
##                       label step_major step_minor    bgn    end elapsed
## 8 extract.features_bind.DTM          7          0 34.083 34.091   0.008
## 9 extract.features_bind.DXM          8          0 34.091     NA      NA
## [1] "Binding DXM for description..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_category_files/figure-html/extract.features-4.png) 

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

#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
tmp_allobs_df <- glb_allobs_df[, "prdline.my", FALSE]
names(tmp_allobs_df) <- "old.prdline.my"
glb_allobs_df$prdline.my <-
    plyr::revalue(glb_allobs_df$prdline.my, c(      
        # "iPad 1"    = "iPad",
        # "iPad 2"    = "iPad2+",
        "iPad 3"    = "iPad 3+",
        "iPad 4"    = "iPad 3+",
        "iPad 5"    = "iPad 3+",
        
        "iPad Air"      = "iPadAir",
        "iPad Air 2"    = "iPadAir",
        
        "iPad mini"         = "iPadmini",
        "iPad mini 2"       = "iPadmini 2+",
        "iPad mini 3"       = "iPadmini 2+",
        "iPad mini Retina"  = "iPadmini 2+"
    ))
tmp_allobs_df$prdline.my <- glb_allobs_df[, "prdline.my"]
print(mycreate_sqlxtab_df(tmp_allobs_df, c("prdline.my", "old.prdline.my")))
```

```
##     prdline.my   old.prdline.my  .n
## 1       iPad 2           iPad 2 442
## 2     iPadmini        iPad mini 393
## 3       iPad 1           iPad 1 316
## 4      Unknown          Unknown 285
## 5      iPadAir         iPad Air 257
## 6      iPadAir       iPad Air 2 233
## 7      iPad 3+           iPad 4 225
## 8      iPad 3+           iPad 3 208
## 9  iPadmini 2+      iPad mini 2 163
## 10 iPadmini 2+      iPad mini 3 128
## 11 iPadmini 2+ iPad mini Retina   8
## 12     iPad 3+           iPad 5   1
```

```r
print(mycreate_sqlxtab_df(tmp_allobs_df, c("prdline.my")))
```

```
##    prdline.my  .n
## 1     iPadAir 490
## 2      iPad 2 442
## 3     iPad 3+ 434
## 4    iPadmini 393
## 5      iPad 1 316
## 6 iPadmini 2+ 299
## 7     Unknown 285
```

```r
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
## 9  extract.features_bind.DXM          8          0 34.091 44.946  10.856
## 10      extract.features_end          9          0 44.947     NA      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                                    label step_major
## 9                              extract.features_bind.DXM          8
## 6                           extract.features_extract.DTM          5
## 2                    extract.features_factorize.str.vars          2
## 3                          extract.features_process.text          3
## 7                            extract.features_report.DTM          6
## 5                          extract.features_build.corpus          4
## 1                                   extract.features_bgn          1
## 8                              extract.features_bind.DTM          7
## 4 extract.features_process.text_reporting_compound_terms          3
##   step_minor    bgn    end elapsed duration
## 9          0 34.091 44.946  10.856   10.855
## 6          0 28.012 31.548   3.536    3.536
## 2          0 21.686 24.628   2.943    2.942
## 3          0 24.629 27.358   2.729    2.729
## 7          0 31.549 34.082   2.533    2.533
## 5          0 27.364 28.011   0.648    0.647
## 1          0 21.670 21.685   0.015    0.015
## 8          0 34.083 34.091   0.008    0.008
## 4          1 27.358 27.364   0.006    0.006
## [1] "Total Elapsed Time: 44.946 secs"
```

![](ebayipads_category_files/figure-html/extract.features-5.png) 

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

![](ebayipads_category_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 21.663 46.337  24.674
## 6     cluster.data          4          0 46.337     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 46.337 47.324   0.987
## 7 manage.missing.data          4          1 47.324     NA      NA
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
## 7 manage.missing.data          4          1 47.324 47.467   0.143
## 8     select.features          5          0 47.468     NA      NA
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
## prdline.my.fctr                 prdline.my.fctr -0.0871560229
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
## prdline.my.fctr                       0 0.0871560229
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
## 65         prdline.my.fctr -0.0871560229               0 0.0871560229
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
## 65                    <NA>    1.135048    0.37614186   FALSE FALSE
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

![](ebayipads_category_files/figure-html/select.features-1.png) 

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
## 8         select.features          5          0 47.468 48.979   1.511
## 9 partition.data.training          6          0 48.980     NA      NA
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
## Fit    525    451      NA
## OOB    476    409      NA
##        sold.0    sold.1 sold.NA
##            NA        NA       1
## Fit 0.5379098 0.4620902      NA
## OOB 0.5378531 0.4621469      NA
##    prdline.my .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
## 2      iPad 2    154    171      0.1929825      0.1932203
## 4     iPadAir    137    152      0.1716792      0.1717514
## 3     iPad 3+    123    136      0.1541353      0.1536723
## 5    iPadmini    114    126      0.1428571      0.1423729
## 6 iPadmini 2+     94    104      0.1177945      0.1175141
## 1      iPad 1     89     99      0.1115288      0.1118644
## 7     Unknown     87     97      0.1090226      0.1096045
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
## [1] 976  63
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 885  63
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
## 9  partition.data.training          6          0 48.980 50.031   1.051
## 10              fit.models          7          0 50.031     NA      NA
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
## 0.5379098 0.4620902 
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
## 1 0.5379098 0.4620902
## 2 0.5379098 0.4620902
## 3 0.5379098 0.4620902
## 4 0.5379098 0.4620902
## 5 0.5379098 0.4620902
## 6 0.5379098 0.4620902
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.MFO.myMFO_classfr.N
## 1         N                                   525
## 2         Y                                   451
##          Prediction
## Reference   N   Y
##         N 525   0
##         Y 451   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.379098e-01   0.000000e+00   5.060449e-01   5.695455e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   5.131267e-01   1.189217e-99 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.5379098 0.4620902
## 2 0.5379098 0.4620902
## 3 0.5379098 0.4620902
## 4 0.5379098 0.4620902
## 5 0.5379098 0.4620902
## 6 0.5379098 0.4620902
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.MFO.myMFO_classfr.N
## 1         N                                   476
## 2         Y                                   409
##          Prediction
## Reference   N   Y
##         N 476   0
##         Y 409   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.378531e-01   0.000000e+00   5.043611e-01   5.710923e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   5.137840e-01   1.646732e-90 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.869                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.5379098
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.5060449             0.5695455             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.5378531
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5043611             0.5710923             0
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

![](ebayipads_category_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6320953
## 3        0.2 0.6320953
## 4        0.3 0.6320953
## 5        0.4 0.6320953
## 6        0.5 0.4714912
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Random.myrandom_classfr.Y
## 1         N                                         525
## 2         Y                                         451
##          Prediction
## Reference   N   Y
##         N   0 525
##         Y   0 451
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   4.620902e-01   0.000000e+00   4.304545e-01   4.939551e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   9.999991e-01  9.406788e-116 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](ebayipads_category_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6321484
## 3        0.2 0.6321484
## 4        0.3 0.6321484
## 5        0.4 0.6321484
## 6        0.5 0.4847001
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Random.myrandom_classfr.Y
## 1         N                                         476
## 2         Y                                         409
##          Prediction
## Reference   N   Y
##         N   0 476
##         Y   0 409
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   4.621469e-01   0.000000e+00   4.289077e-01   4.956389e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   9.999972e-01  4.314099e-105 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.251                 0.001   0.5040735
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.4       0.6320953        0.4620902
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.4304545             0.4939551             0   0.5214656
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6321484        0.4621469
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.4289077             0.4956389             0
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
## Fitting cp = 0.494 on full training set
```

```
## Loading required package: rpart.plot
```

![](ebayipads_category_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##          CP nsplit rel error
## 1 0.4944568      0         1
## 
## Node number 1: 976 observations
##   predicted class=N  expected loss=0.4620902  P(node) =1
##     class counts:   525   451
##    probabilities: 0.538 0.462 
## 
## n= 976 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 976 451 N (0.5379098 0.4620902) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1         N                                      525
## 2         Y                                      451
##          Prediction
## Reference   N   Y
##         N 525   0
##         Y 451   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.379098e-01   0.000000e+00   5.060449e-01   5.695455e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   5.131267e-01   1.189217e-99 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1         N                                      476
## 2         Y                                      409
##          Prediction
## Reference   N   Y
##         N 476   0
##         Y 409   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.378531e-01   0.000000e+00   5.043611e-01   5.710923e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   5.137840e-01   1.646732e-90 
##               model_id model_method                feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart biddable, startprice               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.129                 0.012         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.5379098
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.5060449             0.5695455             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.5378531
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5043611             0.5710923             0
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

![](ebayipads_category_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##              CP nsplit rel error
## 1  0.4944567627      0 1.0000000
## 2  0.0155210643      1 0.5055432
## 3  0.0133037694      3 0.4745011
## 4  0.0110864745      4 0.4611973
## 5  0.0051736881      5 0.4501109
## 6  0.0044345898      8 0.4345898
## 7  0.0036954915      9 0.4301552
## 8  0.0022172949     12 0.4190687
## 9  0.0002217295     20 0.3991131
## 10 0.0000000000     30 0.3968958
## 
## Variable importance
## startprice   biddable 
##         51         49 
## 
## Node number 1: 976 observations,    complexity param=0.4944568
##   predicted class=N  expected loss=0.4620902  P(node) =1
##     class counts:   525   451
##    probabilities: 0.538 0.462 
##   left son=2 (539 obs) right son=3 (437 obs)
##   Primary splits:
##       biddable   < 0.5     to the left,  improve=135.9194, (0 missing)
##       startprice < 151.435 to the right, improve=117.2326, (0 missing)
##   Surrogate splits:
##       startprice < 102.35  to the right, agree=0.747, adj=0.435, (0 split)
## 
## Node number 2: 539 observations,    complexity param=0.005173688
##   predicted class=N  expected loss=0.2244898  P(node) =0.5522541
##     class counts:   418   121
##    probabilities: 0.776 0.224 
##   left son=4 (378 obs) right son=5 (161 obs)
##   Primary splits:
##       startprice < 184.97  to the right, improve=10.0818, (0 missing)
## 
## Node number 3: 437 observations,    complexity param=0.01552106
##   predicted class=Y  expected loss=0.2448513  P(node) =0.4477459
##     class counts:   107   330
##    probabilities: 0.245 0.755 
##   left son=6 (159 obs) right son=7 (278 obs)
##   Primary splits:
##       startprice < 149.965 to the right, improve=33.34958, (0 missing)
## 
## Node number 4: 378 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1613757  P(node) =0.3872951
##     class counts:   317    61
##    probabilities: 0.839 0.161 
##   left son=8 (273 obs) right son=9 (105 obs)
##   Primary splits:
##       startprice < 251.44  to the right, improve=1.711437, (0 missing)
## 
## Node number 5: 161 observations,    complexity param=0.005173688
##   predicted class=N  expected loss=0.3726708  P(node) =0.164959
##     class counts:   101    60
##    probabilities: 0.627 0.373 
##   left son=10 (90 obs) right son=11 (71 obs)
##   Primary splits:
##       startprice < 129.995 to the left,  improve=1.038815, (0 missing)
## 
## Node number 6: 159 observations,    complexity param=0.01552106
##   predicted class=N  expected loss=0.4968553  P(node) =0.1629098
##     class counts:    80    79
##    probabilities: 0.503 0.497 
##   left son=12 (20 obs) right son=13 (139 obs)
##   Primary splits:
##       startprice < 413.495 to the right, improve=5.504769, (0 missing)
## 
## Node number 7: 278 observations
##   predicted class=Y  expected loss=0.0971223  P(node) =0.2848361
##     class counts:    27   251
##    probabilities: 0.097 0.903 
## 
## Node number 8: 273 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1318681  P(node) =0.2797131
##     class counts:   237    36
##    probabilities: 0.868 0.132 
##   left son=16 (24 obs) right son=17 (249 obs)
##   Primary splits:
##       startprice < 279.995 to the left,  improve=0.9151331, (0 missing)
## 
## Node number 9: 105 observations
##   predicted class=N  expected loss=0.2380952  P(node) =0.107582
##     class counts:    80    25
##    probabilities: 0.762 0.238 
## 
## Node number 10: 90 observations,    complexity param=0.003695492
##   predicted class=N  expected loss=0.3222222  P(node) =0.09221311
##     class counts:    61    29
##    probabilities: 0.678 0.322 
##   left son=20 (35 obs) right son=21 (55 obs)
##   Primary splits:
##       startprice < 97      to the right, improve=2.604618, (0 missing)
## 
## Node number 11: 71 observations,    complexity param=0.005173688
##   predicted class=N  expected loss=0.4366197  P(node) =0.0727459
##     class counts:    40    31
##    probabilities: 0.563 0.437 
##   left son=22 (40 obs) right son=23 (31 obs)
##   Primary splits:
##       startprice < 152.43  to the right, improve=3.4199, (0 missing)
## 
## Node number 12: 20 observations
##   predicted class=N  expected loss=0.15  P(node) =0.0204918
##     class counts:    17     3
##    probabilities: 0.850 0.150 
## 
## Node number 13: 139 observations,    complexity param=0.01330377
##   predicted class=Y  expected loss=0.4532374  P(node) =0.142418
##     class counts:    63    76
##    probabilities: 0.453 0.547 
##   left son=26 (72 obs) right son=27 (67 obs)
##   Primary splits:
##       startprice < 205.995 to the right, improve=2.336116, (0 missing)
## 
## Node number 16: 24 observations
##   predicted class=N  expected loss=0  P(node) =0.02459016
##     class counts:    24     0
##    probabilities: 1.000 0.000 
## 
## Node number 17: 249 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1445783  P(node) =0.255123
##     class counts:   213    36
##    probabilities: 0.855 0.145 
##   left son=34 (242 obs) right son=35 (7 obs)
##   Primary splits:
##       startprice < 288.5   to the right, improve=1.16179, (0 missing)
## 
## Node number 20: 35 observations
##   predicted class=N  expected loss=0.1714286  P(node) =0.03586066
##     class counts:    29     6
##    probabilities: 0.829 0.171 
## 
## Node number 21: 55 observations,    complexity param=0.003695492
##   predicted class=N  expected loss=0.4181818  P(node) =0.05635246
##     class counts:    32    23
##    probabilities: 0.582 0.418 
##   left son=42 (11 obs) right son=43 (44 obs)
##   Primary splits:
##       startprice < 28.975  to the left,  improve=1.536364, (0 missing)
## 
## Node number 22: 40 observations,    complexity param=0.002217295
##   predicted class=N  expected loss=0.3  P(node) =0.04098361
##     class counts:    28    12
##    probabilities: 0.700 0.300 
##   left son=44 (31 obs) right son=45 (9 obs)
##   Primary splits:
##       startprice < 177     to the left,  improve=1.516846, (0 missing)
## 
## Node number 23: 31 observations,    complexity param=0.002217295
##   predicted class=Y  expected loss=0.3870968  P(node) =0.0317623
##     class counts:    12    19
##    probabilities: 0.387 0.613 
##   left son=46 (9 obs) right son=47 (22 obs)
##   Primary splits:
##       startprice < 143.495 to the left,  improve=0.7197784, (0 missing)
## 
## Node number 26: 72 observations,    complexity param=0.01108647
##   predicted class=N  expected loss=0.4583333  P(node) =0.07377049
##     class counts:    39    33
##    probabilities: 0.542 0.458 
##   left son=52 (13 obs) right son=53 (59 obs)
##   Primary splits:
##       startprice < 240.5   to the left,  improve=4.615711, (0 missing)
## 
## Node number 27: 67 observations,    complexity param=0.002217295
##   predicted class=Y  expected loss=0.358209  P(node) =0.06864754
##     class counts:    24    43
##    probabilities: 0.358 0.642 
##   left son=54 (44 obs) right son=55 (23 obs)
##   Primary splits:
##       startprice < 182.495 to the left,  improve=1.388974, (0 missing)
## 
## Node number 34: 242 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1363636  P(node) =0.2479508
##     class counts:   209    33
##    probabilities: 0.864 0.136 
##   left son=68 (26 obs) right son=69 (216 obs)
##   Primary splits:
##       startprice < 626.995 to the right, improve=0.5584046, (0 missing)
## 
## Node number 35: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.007172131
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 42: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.01127049
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 43: 44 observations,    complexity param=0.003695492
##   predicted class=N  expected loss=0.4772727  P(node) =0.04508197
##     class counts:    23    21
##    probabilities: 0.523 0.477 
##   left son=86 (33 obs) right son=87 (11 obs)
##   Primary splits:
##       startprice < 59.995  to the right, improve=1.833333, (0 missing)
## 
## Node number 44: 31 observations
##   predicted class=N  expected loss=0.2258065  P(node) =0.0317623
##     class counts:    24     7
##    probabilities: 0.774 0.226 
## 
## Node number 45: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.009221311
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## Node number 46: 9 observations
##   predicted class=N  expected loss=0.4444444  P(node) =0.009221311
##     class counts:     5     4
##    probabilities: 0.556 0.444 
## 
## Node number 47: 22 observations
##   predicted class=Y  expected loss=0.3181818  P(node) =0.02254098
##     class counts:     7    15
##    probabilities: 0.318 0.682 
## 
## Node number 52: 13 observations
##   predicted class=N  expected loss=0.07692308  P(node) =0.01331967
##     class counts:    12     1
##    probabilities: 0.923 0.077 
## 
## Node number 53: 59 observations,    complexity param=0.00443459
##   predicted class=Y  expected loss=0.4576271  P(node) =0.06045082
##     class counts:    27    32
##    probabilities: 0.458 0.542 
##   left son=106 (8 obs) right son=107 (51 obs)
##   Primary splits:
##       startprice < 387.5   to the right, improve=0.5185278, (0 missing)
## 
## Node number 54: 44 observations,    complexity param=0.002217295
##   predicted class=Y  expected loss=0.4318182  P(node) =0.04508197
##     class counts:    19    25
##    probabilities: 0.432 0.568 
##   left son=108 (8 obs) right son=109 (36 obs)
##   Primary splits:
##       startprice < 174.975 to the right, improve=0.729798, (0 missing)
## 
## Node number 55: 23 observations
##   predicted class=Y  expected loss=0.2173913  P(node) =0.02356557
##     class counts:     5    18
##    probabilities: 0.217 0.783 
## 
## Node number 68: 26 observations
##   predicted class=N  expected loss=0.03846154  P(node) =0.02663934
##     class counts:    25     1
##    probabilities: 0.962 0.038 
## 
## Node number 69: 216 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1481481  P(node) =0.2213115
##     class counts:   184    32
##    probabilities: 0.852 0.148 
##   left son=138 (209 obs) right son=139 (7 obs)
##   Primary splits:
##       startprice < 587.99  to the left,  improve=1.137794, (0 missing)
## 
## Node number 86: 33 observations
##   predicted class=N  expected loss=0.3939394  P(node) =0.03381148
##     class counts:    20    13
##    probabilities: 0.606 0.394 
## 
## Node number 87: 11 observations
##   predicted class=Y  expected loss=0.2727273  P(node) =0.01127049
##     class counts:     3     8
##    probabilities: 0.273 0.727 
## 
## Node number 106: 8 observations
##   predicted class=N  expected loss=0.375  P(node) =0.008196721
##     class counts:     5     3
##    probabilities: 0.625 0.375 
## 
## Node number 107: 51 observations,    complexity param=0.002217295
##   predicted class=Y  expected loss=0.4313725  P(node) =0.0522541
##     class counts:    22    29
##    probabilities: 0.431 0.569 
##   left son=214 (42 obs) right son=215 (9 obs)
##   Primary splits:
##       startprice < 342.5   to the left,  improve=0.9561158, (0 missing)
## 
## Node number 108: 8 observations
##   predicted class=N  expected loss=0.375  P(node) =0.008196721
##     class counts:     5     3
##    probabilities: 0.625 0.375 
## 
## Node number 109: 36 observations
##   predicted class=Y  expected loss=0.3888889  P(node) =0.03688525
##     class counts:    14    22
##    probabilities: 0.389 0.611 
## 
## Node number 138: 209 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.138756  P(node) =0.2141393
##     class counts:   180    29
##    probabilities: 0.861 0.139 
##   left son=276 (28 obs) right son=277 (181 obs)
##   Primary splits:
##       startprice < 504.5   to the right, improve=0.6865651, (0 missing)
## 
## Node number 139: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.007172131
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 214: 42 observations,    complexity param=0.002217295
##   predicted class=Y  expected loss=0.4761905  P(node) =0.04303279
##     class counts:    20    22
##    probabilities: 0.476 0.524 
##   left son=428 (14 obs) right son=429 (28 obs)
##   Primary splits:
##       startprice < 299.495 to the right, improve=0.3809524, (0 missing)
## 
## Node number 215: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.009221311
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 276: 28 observations
##   predicted class=N  expected loss=0.03571429  P(node) =0.02868852
##     class counts:    27     1
##    probabilities: 0.964 0.036 
## 
## Node number 277: 181 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1546961  P(node) =0.1854508
##     class counts:   153    28
##    probabilities: 0.845 0.155 
##   left son=554 (169 obs) right son=555 (12 obs)
##   Primary splits:
##       startprice < 498.94  to the left,  improve=0.8202513, (0 missing)
## 
## Node number 428: 14 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.01434426
##     class counts:     8     6
##    probabilities: 0.571 0.429 
## 
## Node number 429: 28 observations,    complexity param=0.002217295
##   predicted class=Y  expected loss=0.4285714  P(node) =0.02868852
##     class counts:    12    16
##    probabilities: 0.429 0.571 
##   left son=858 (21 obs) right son=859 (7 obs)
##   Primary splits:
##       startprice < 272.525 to the left,  improve=1.52381, (0 missing)
## 
## Node number 554: 169 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1420118  P(node) =0.1731557
##     class counts:   145    24
##    probabilities: 0.858 0.142 
##   left son=1108 (21 obs) right son=1109 (148 obs)
##   Primary splits:
##       startprice < 439.995 to the right, improve=0.4273187, (0 missing)
## 
## Node number 555: 12 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.01229508
##     class counts:     8     4
##    probabilities: 0.667 0.333 
## 
## Node number 858: 21 observations,    complexity param=0.002217295
##   predicted class=N  expected loss=0.4761905  P(node) =0.02151639
##     class counts:    11    10
##    probabilities: 0.524 0.476 
##   left son=1716 (7 obs) right son=1717 (14 obs)
##   Primary splits:
##       startprice < 257.475 to the right, improve=0.7619048, (0 missing)
## 
## Node number 859: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.007172131
##     class counts:     1     6
##    probabilities: 0.143 0.857 
## 
## Node number 1108: 21 observations
##   predicted class=N  expected loss=0.04761905  P(node) =0.02151639
##     class counts:    20     1
##    probabilities: 0.952 0.048 
## 
## Node number 1109: 148 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.1554054  P(node) =0.1516393
##     class counts:   125    23
##    probabilities: 0.845 0.155 
##   left son=2218 (128 obs) right son=2219 (20 obs)
##   Primary splits:
##       startprice < 412.5   to the left,  improve=0.9669764, (0 missing)
## 
## Node number 1716: 7 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.007172131
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 1717: 14 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.01434426
##     class counts:     6     8
##    probabilities: 0.429 0.571 
## 
## Node number 2218: 128 observations
##   predicted class=N  expected loss=0.1328125  P(node) =0.1311475
##     class counts:   111    17
##    probabilities: 0.867 0.133 
## 
## Node number 2219: 20 observations,    complexity param=0.0002217295
##   predicted class=N  expected loss=0.3  P(node) =0.0204918
##     class counts:    14     6
##    probabilities: 0.700 0.300 
##   left son=4438 (13 obs) right son=4439 (7 obs)
##   Primary splits:
##       startprice < 425.495 to the right, improve=1.586813, (0 missing)
## 
## Node number 4438: 13 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.01331967
##     class counts:    11     2
##    probabilities: 0.846 0.154 
## 
## Node number 4439: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.007172131
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## n= 976 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##    1) root 976 451 N (0.53790984 0.46209016)  
##      2) biddable< 0.5 539 121 N (0.77551020 0.22448980)  
##        4) startprice>=184.97 378  61 N (0.83862434 0.16137566)  
##          8) startprice>=251.44 273  36 N (0.86813187 0.13186813)  
##           16) startprice< 279.995 24   0 N (1.00000000 0.00000000) *
##           17) startprice>=279.995 249  36 N (0.85542169 0.14457831)  
##             34) startprice>=288.5 242  33 N (0.86363636 0.13636364)  
##               68) startprice>=626.995 26   1 N (0.96153846 0.03846154) *
##               69) startprice< 626.995 216  32 N (0.85185185 0.14814815)  
##                138) startprice< 587.99 209  29 N (0.86124402 0.13875598)  
##                  276) startprice>=504.5 28   1 N (0.96428571 0.03571429) *
##                  277) startprice< 504.5 181  28 N (0.84530387 0.15469613)  
##                    554) startprice< 498.94 169  24 N (0.85798817 0.14201183)  
##                     1108) startprice>=439.995 21   1 N (0.95238095 0.04761905) *
##                     1109) startprice< 439.995 148  23 N (0.84459459 0.15540541)  
##                       2218) startprice< 412.5 128  17 N (0.86718750 0.13281250) *
##                       2219) startprice>=412.5 20   6 N (0.70000000 0.30000000)  
##                         4438) startprice>=425.495 13   2 N (0.84615385 0.15384615) *
##                         4439) startprice< 425.495 7   3 Y (0.42857143 0.57142857) *
##                    555) startprice>=498.94 12   4 N (0.66666667 0.33333333) *
##                139) startprice>=587.99 7   3 N (0.57142857 0.42857143) *
##             35) startprice< 288.5 7   3 N (0.57142857 0.42857143) *
##          9) startprice< 251.44 105  25 N (0.76190476 0.23809524) *
##        5) startprice< 184.97 161  60 N (0.62732919 0.37267081)  
##         10) startprice< 129.995 90  29 N (0.67777778 0.32222222)  
##           20) startprice>=97 35   6 N (0.82857143 0.17142857) *
##           21) startprice< 97 55  23 N (0.58181818 0.41818182)  
##             42) startprice< 28.975 11   2 N (0.81818182 0.18181818) *
##             43) startprice>=28.975 44  21 N (0.52272727 0.47727273)  
##               86) startprice>=59.995 33  13 N (0.60606061 0.39393939) *
##               87) startprice< 59.995 11   3 Y (0.27272727 0.72727273) *
##         11) startprice>=129.995 71  31 N (0.56338028 0.43661972)  
##           22) startprice>=152.43 40  12 N (0.70000000 0.30000000)  
##             44) startprice< 177 31   7 N (0.77419355 0.22580645) *
##             45) startprice>=177 9   4 Y (0.44444444 0.55555556) *
##           23) startprice< 152.43 31  12 Y (0.38709677 0.61290323)  
##             46) startprice< 143.495 9   4 N (0.55555556 0.44444444) *
##             47) startprice>=143.495 22   7 Y (0.31818182 0.68181818) *
##      3) biddable>=0.5 437 107 Y (0.24485126 0.75514874)  
##        6) startprice>=149.965 159  79 N (0.50314465 0.49685535)  
##         12) startprice>=413.495 20   3 N (0.85000000 0.15000000) *
##         13) startprice< 413.495 139  63 Y (0.45323741 0.54676259)  
##           26) startprice>=205.995 72  33 N (0.54166667 0.45833333)  
##             52) startprice< 240.5 13   1 N (0.92307692 0.07692308) *
##             53) startprice>=240.5 59  27 Y (0.45762712 0.54237288)  
##              106) startprice>=387.5 8   3 N (0.62500000 0.37500000) *
##              107) startprice< 387.5 51  22 Y (0.43137255 0.56862745)  
##                214) startprice< 342.5 42  20 Y (0.47619048 0.52380952)  
##                  428) startprice>=299.495 14   6 N (0.57142857 0.42857143) *
##                  429) startprice< 299.495 28  12 Y (0.42857143 0.57142857)  
##                    858) startprice< 272.525 21  10 N (0.52380952 0.47619048)  
##                     1716) startprice>=257.475 7   2 N (0.71428571 0.28571429) *
##                     1717) startprice< 257.475 14   6 Y (0.42857143 0.57142857) *
##                    859) startprice>=272.525 7   1 Y (0.14285714 0.85714286) *
##                215) startprice>=342.5 9   2 Y (0.22222222 0.77777778) *
##           27) startprice< 205.995 67  24 Y (0.35820896 0.64179104)  
##             54) startprice< 182.495 44  19 Y (0.43181818 0.56818182)  
##              108) startprice>=174.975 8   3 N (0.62500000 0.37500000) *
##              109) startprice< 174.975 36  14 Y (0.38888889 0.61111111) *
##             55) startprice>=182.495 23   5 Y (0.21739130 0.78260870) *
##        7) startprice< 149.965 278  27 Y (0.09712230 0.90287770) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6798479
## 3        0.2 0.7527076
## 4        0.3 0.7937824
## 5        0.4 0.7964602
## 6        0.5 0.7935409
## 7        0.6 0.7813620
## 8        0.7 0.7445443
## 9        0.8 0.6983696
## 10       0.9 0.6886145
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           432
## 2         Y                                            91
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            93
## 2                                           360
##          Prediction
## Reference   N   Y
##         N 432  93
##         Y  91 360
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.114754e-01   6.208893e-01   7.854988e-01   8.355569e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   7.376156e-72   9.412324e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6611296
## 3        0.2 0.7033399
## 4        0.3 0.7460317
## 5        0.4 0.7490909
## 6        0.5 0.7515763
## 7        0.6 0.7385020
## 8        0.7 0.7358230
## 9        0.8 0.7246377
## 10       0.9 0.7203514
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           390
## 2         Y                                           111
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            86
## 2                                           298
##          Prediction
## Reference   N   Y
##         N 390  86
##         Y 111 298
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.774011e-01   5.503014e-01   7.485294e-01   8.044124e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.304860e-49   8.727897e-02 
##                    model_id model_method                feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart biddable, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.463                 0.009
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8769591                    0.4       0.7964602        0.8114754
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7854988             0.8355569     0.6208893   0.8257253
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7515763        0.7774011
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7485294             0.8044124     0.5503014
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
## Fitting cp = 0.0155 on full training set
```

![](ebayipads_category_files/figure-html/fit.models_0-11.png) ![](ebayipads_category_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##           CP nsplit rel error
## 1 0.49445676      0 1.0000000
## 2 0.01552106      1 0.5055432
## 
## Variable importance
##   biddable startprice 
##         70         30 
## 
## Node number 1: 976 observations,    complexity param=0.4944568
##   predicted class=N  expected loss=0.4620902  P(node) =1
##     class counts:   525   451
##    probabilities: 0.538 0.462 
##   left son=2 (539 obs) right son=3 (437 obs)
##   Primary splits:
##       biddable   < 0.5     to the left,  improve=135.9194, (0 missing)
##       startprice < 151.435 to the right, improve=117.2326, (0 missing)
##   Surrogate splits:
##       startprice < 102.35  to the right, agree=0.747, adj=0.435, (0 split)
## 
## Node number 2: 539 observations
##   predicted class=N  expected loss=0.2244898  P(node) =0.5522541
##     class counts:   418   121
##    probabilities: 0.776 0.224 
## 
## Node number 3: 437 observations
##   predicted class=Y  expected loss=0.2448513  P(node) =0.4477459
##     class counts:   107   330
##    probabilities: 0.245 0.755 
## 
## n= 976 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 976 451 N (0.5379098 0.4620902)  
##   2) biddable< 0.5 539 121 N (0.7755102 0.2244898) *
##   3) biddable>=0.5 437 107 Y (0.2448513 0.7551487) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_0-13.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6320953
## 3        0.2 0.6320953
## 4        0.3 0.7432432
## 5        0.4 0.7432432
## 6        0.5 0.7432432
## 7        0.6 0.7432432
## 8        0.7 0.7432432
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-14.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 418
## 2         Y                                 121
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                 107
## 2                                 330
##          Prediction
## Reference   N   Y
##         N 418 107
##         Y 121 330
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.663934e-01   5.290552e-01   7.385542e-01   7.926151e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   1.631860e-49   3.892678e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_0-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6321484
## 3        0.2 0.6321484
## 4        0.3 0.7663782
## 5        0.4 0.7663782
## 6        0.5 0.7663782
## 7        0.6 0.7663782
## 8        0.7 0.7663782
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-16.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 386
## 2         Y                                  99
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  90
## 2                                 310
##          Prediction
## Reference   N   Y
##         N 386  90
##         Y  99 310
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.864407e-01   5.697529e-01   7.579436e-01   8.130160e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.590529e-53   5.606244e-01 
##          model_id model_method                feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart biddable, startprice               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.998                 0.012   0.7639489
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.7       0.7432432        0.7786629
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7385542             0.7926151     0.5507393   0.7844353
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.7663782        0.7864407
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7579436              0.813016     0.5697529
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.04549097      0.09113973
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

![](ebayipads_category_files/figure-html/fit.models_0-17.png) ![](ebayipads_category_files/figure-html/fit.models_0-18.png) ![](ebayipads_category_files/figure-html/fit.models_0-19.png) ![](ebayipads_category_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0395  -0.7606  -0.3009   0.6612   2.7166  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.1914963  0.1725490   1.110    0.267    
## biddable     1.7833180  0.1652852  10.789   <2e-16 ***
## startprice  -0.0057128  0.0006147  -9.294   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  953.02  on 973  degrees of freedom
## AIC: 959.02
## 
## Number of Fisher Scoring iterations: 4
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_0-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6636225
## 3        0.2 0.7106164
## 4        0.3 0.7480620
## 5        0.4 0.7521368
## 6        0.5 0.7421965
## 7        0.6 0.7349398
## 8        0.7 0.7223650
## 9        0.8 0.6572638
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-22.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               392
## 2         Y                                99
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                               133
## 2                               352
##          Prediction
## Reference   N   Y
##         N 392 133
##         Y  99 352
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.622951e-01   5.243685e-01   7.343087e-01   7.886889e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   9.901028e-48   3.026871e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_0-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6562756
## 3        0.2 0.6832872
## 4        0.3 0.7133891
## 5        0.4 0.7558685
## 6        0.5 0.7643312
## 7        0.6 0.7519789
## 8        0.7 0.7361111
## 9        0.8 0.6923077
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-24.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               400
## 2         Y                               109
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                76
## 2                               300
##          Prediction
## Reference   N   Y
##         N 400  76
##         Y 109 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.909605e-01   5.771095e-01   7.626579e-01   8.173104e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.496581e-55   1.863833e-02 
##        model_id model_method                feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm biddable, startprice               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                       1.04                 0.012   0.8473846
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.4       0.7521368        0.7755923
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7343087             0.7886889     0.5460308   0.8351149
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7643312        0.7909605
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7626579             0.8173104     0.5771095     959.016
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03748683      0.07662323
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

![](ebayipads_category_files/figure-html/fit.models_0-25.png) ![](ebayipads_category_files/figure-html/fit.models_0-26.png) ![](ebayipads_category_files/figure-html/fit.models_0-27.png) ![](ebayipads_category_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0501  -0.7600  -0.2983   0.6525   2.7246  
## 
## Coefficients:
##                                      Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         0.2021368  0.1745774   1.158   0.2469
## biddable                            3.5135745  2.4974334   1.407   0.1595
## startprice                         -0.0057617  0.0006276  -9.181   <2e-16
## `biddable:D.nwrds.unq.log`         -0.4510499  1.5828223  -0.285   0.7757
## `biddable:D.T.like`                -0.7234717  0.5395318  -1.341   0.1799
## `biddable:D.npnct06.log`            0.2028376  1.0309099   0.197   0.8440
## `biddable:D.ratio.nstopwrds.nwrds` -1.6496297  2.4899745  -0.663   0.5076
## `biddable:D.nchrs.log`              0.0006151  1.0796468   0.001   0.9995
## `biddable:D.nwrds.log`             -0.0411852  2.0496420  -0.020   0.9840
## `biddable:cellular.fctr1`           0.1219763  0.2883652   0.423   0.6723
## `biddable:cellular.fctrUnknown`    -0.7669881  0.3979048  -1.928   0.0539
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
## `biddable:cellular.fctrUnknown`    .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  945.06  on 965  degrees of freedom
## AIC: 967.06
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6641280
## 3        0.2 0.7112254
## 4        0.3 0.7490347
## 5        0.4 0.7547974
## 6        0.5 0.7505774
## 7        0.6 0.7363305
## 8        0.7 0.7203065
## 9        0.8 0.6178624
## 10       0.9 0.1169102
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-30.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         392
## 2         Y                                          97
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                         133
## 2                                         354
##          Prediction
## Reference   N   Y
##         N 392 133
##         Y  97 354
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.643443e-01   5.286153e-01   7.364311e-01   7.906524e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   1.285576e-48   2.100850e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6562756
## 3        0.2 0.6832872
## 4        0.3 0.7105538
## 5        0.4 0.7514723
## 6        0.5 0.7580026
## 7        0.6 0.7579787
## 8        0.7 0.7458101
## 9        0.8 0.6286645
## 10       0.9 0.0430622
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-32.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         400
## 2         Y                                         113
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          76
## 2                                         296
##          Prediction
## Reference   N   Y
##         N 400  76
##         Y 113 296
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.864407e-01   5.676667e-01   7.579436e-01   8.130160e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.590529e-53   8.828761e-03 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                             feats
## 1 biddable, startprice, biddable:D.nwrds.unq.log, biddable:D.T.like, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.029                 0.018
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8481153                    0.4       0.7547974        0.7755923
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7364311             0.7906524     0.5457753   0.8351868
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7580026        0.7864407
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7579436              0.813016     0.5676667    967.0557
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03748683      0.07640194
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

![](ebayipads_category_files/figure-html/fit.models_0-33.png) ![](ebayipads_category_files/figure-html/fit.models_0-34.png) ![](ebayipads_category_files/figure-html/fit.models_0-35.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](ebayipads_category_files/figure-html/fit.models_0-36.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.9056  -0.6964  -0.1687   0.6291   3.5433  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                               1.572e+00  7.139e-01   2.201
## biddable                                  1.450e+00  1.983e-01   7.314
## D.npnct15.log                             7.602e-01  8.053e-01   0.944
## D.T.screen                                2.495e-01  7.429e-01   0.336
## D.npnct03.log                             3.347e-01  1.470e+00   0.228
## D.T.used                                 -1.452e-01  5.266e-01  -0.276
## D.T.excellent                             1.699e-01  3.591e-01   0.473
## D.T.great                                -8.464e-01  6.483e-01  -1.306
## D.ratio.sum.TfIdf.nwrds                  -4.515e-02  2.119e-01  -0.213
## D.T.good                                  1.997e-01  6.648e-01   0.300
## D.npnct01.log                             9.810e-01  5.975e-01   1.642
## D.T.veri                                  2.711e-02  6.315e-01   0.043
## D.T.work                                  1.048e+00  6.567e-01   1.595
## D.T.scratch                               5.078e-01  7.398e-01   0.686
## D.P.air                                  -1.502e-01  1.109e+00  -0.135
## D.T.use                                   5.411e-01  6.879e-01   0.787
## storage.fctr16                           -6.758e-01  6.100e-01  -1.108
## storage.fctr32                           -7.115e-01  6.277e-01  -1.133
## storage.fctr64                           -2.951e-01  6.105e-01  -0.483
## storage.fctrUnknown                      -2.647e-01  7.960e-01  -0.333
## D.P.mini                                 -1.979e-01  8.954e-01  -0.221
## D.T.condition                            -6.769e-01  6.919e-01  -0.978
## D.npnct11.log                             2.918e-02  3.501e-01   0.083
## .rnorm                                   -5.042e-02  9.188e-02  -0.549
## D.T.ipad                                 -2.384e+00  9.595e-01  -2.485
## D.npnct10.log                             1.832e+00  1.479e+00   1.238
## D.npnct08.log                            -3.525e-01  7.535e-01  -0.468
## D.T.condit                               -4.593e-01  5.305e-01  -0.866
## color.fctrBlack                          -2.564e-01  2.377e-01  -1.079
## color.fctrGold                            4.437e-01  5.672e-01   0.782
## `color.fctrSpace Gray`                    1.062e-01  3.432e-01   0.310
## color.fctrWhite                          -2.810e-01  2.444e-01  -1.150
## D.T.this                                  2.036e-01  9.865e-01   0.206
## D.npnct06.log                            -1.070e+00  9.123e-01  -1.172
## D.T.box                                  -1.741e+00  9.056e-01  -1.923
## D.npnct28.log                            -1.448e+00  1.187e+03  -0.001
## D.T.like                                 -1.047e+00  5.213e-01  -2.008
## D.npnct12.log                             1.773e-01  7.310e-01   0.243
## D.npnct09.log                            -9.279e+00  1.085e+03  -0.009
## D.nwrds.unq.log                           2.277e-02  1.487e-01   0.153
## D.ndgts.log                               1.932e-01  4.489e-01   0.430
## cellular.fctr1                            7.722e-02  2.167e-01   0.356
## cellular.fctrUnknown                     -2.655e-01  4.701e-01  -0.565
## D.npnct14.log                            -1.988e-01  7.728e-01  -0.257
## `prdline.my.fctriPad 2`                   7.757e-01  3.475e-01   2.232
## `prdline.my.fctriPad 3+`                  1.109e+00  3.515e-01   3.154
## prdline.my.fctriPadAir                    2.359e+00  4.644e-01   5.080
## prdline.my.fctriPadmini                   2.065e-01  3.494e-01   0.591
## `prdline.my.fctriPadmini 2+`              1.007e+00  4.526e-01   2.224
## prdline.my.fctrUnknown                   -3.223e-01  4.556e-01  -0.707
## D.npnct05.log                            -3.148e+00  1.741e+00  -1.809
## `condition.fctrFor parts or not working` -7.705e-01  3.285e-01  -2.345
## `condition.fctrManufacturer refurbished` -3.734e-01  7.310e-01  -0.511
## condition.fctrNew                         7.490e-01  3.472e-01   2.157
## `condition.fctrNew other (see details)`   8.880e-01  4.346e-01   2.043
## `condition.fctrSeller refurbished`       -9.066e-01  3.833e-01  -2.365
## startprice                               -1.194e-02  1.303e-03  -9.164
##                                          Pr(>|z|)    
## (Intercept)                               0.02770 *  
## biddable                                 2.59e-13 ***
## D.npnct15.log                             0.34520    
## D.T.screen                                0.73701    
## D.npnct03.log                             0.81989    
## D.T.used                                  0.78278    
## D.T.excellent                             0.63607    
## D.T.great                                 0.19171    
## D.ratio.sum.TfIdf.nwrds                   0.83127    
## D.T.good                                  0.76382    
## D.npnct01.log                             0.10060    
## D.T.veri                                  0.96576    
## D.T.work                                  0.11061    
## D.T.scratch                               0.49249    
## D.P.air                                   0.89226    
## D.T.use                                   0.43151    
## storage.fctr16                            0.26792    
## storage.fctr32                            0.25701    
## storage.fctr64                            0.62889    
## storage.fctrUnknown                       0.73946    
## D.P.mini                                  0.82503    
## D.T.condition                             0.32792    
## D.npnct11.log                             0.93358    
## .rnorm                                    0.58317    
## D.T.ipad                                  0.01295 *  
## D.npnct10.log                             0.21559    
## D.npnct08.log                             0.63997    
## D.T.condit                                0.38657    
## color.fctrBlack                           0.28066    
## color.fctrGold                            0.43407    
## `color.fctrSpace Gray`                    0.75687    
## color.fctrWhite                           0.25020    
## D.T.this                                  0.83650    
## D.npnct06.log                             0.24101    
## D.T.box                                   0.05454 .  
## D.npnct28.log                             0.99903    
## D.T.like                                  0.04464 *  
## D.npnct12.log                             0.80833    
## D.npnct09.log                             0.99317    
## D.nwrds.unq.log                           0.87829    
## D.ndgts.log                               0.66685    
## cellular.fctr1                            0.72164    
## cellular.fctrUnknown                      0.57222    
## D.npnct14.log                             0.79693    
## `prdline.my.fctriPad 2`                   0.02559 *  
## `prdline.my.fctriPad 3+`                  0.00161 ** 
## prdline.my.fctriPadAir                   3.78e-07 ***
## prdline.my.fctriPadmini                   0.55459    
## `prdline.my.fctriPadmini 2+`              0.02613 *  
## prdline.my.fctrUnknown                    0.47932    
## D.npnct05.log                             0.07052 .  
## `condition.fctrFor parts or not working`  0.01900 *  
## `condition.fctrManufacturer refurbished`  0.60955    
## condition.fctrNew                         0.03097 *  
## `condition.fctrNew other (see details)`   0.04101 *  
## `condition.fctrSeller refurbished`        0.01802 *  
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  846.71  on 919  degrees of freedom
## AIC: 960.71
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.7009646
## 3        0.2 0.7471162
## 4        0.3 0.7659574
## 5        0.4 0.7747368
## 6        0.5 0.7972509
## 7        0.6 0.7913669
## 8        0.7 0.7564103
## 9        0.8 0.6637807
## 10       0.9 0.3821429
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-38.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               451
## 2         Y                               103
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                74
## 2                               348
##          Prediction
## Reference   N   Y
##         N 451  74
##         Y 103 348
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.186475e-01   6.335373e-01   7.930128e-01   8.423427e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   6.599723e-76   3.532537e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6672474
## 3        0.2 0.6954851
## 4        0.3 0.7249737
## 5        0.4 0.7454128
## 6        0.5 0.7696139
## 7        0.6 0.7671958
## 8        0.7 0.7475593
## 9        0.8 0.6810478
## 10       0.9 0.4465291
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               391
## 2         Y                               100
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                85
## 2                               309
##          Prediction
## Reference   N   Y
##         N 391  85
##         Y 100 309
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.909605e-01   5.784228e-01   7.626579e-01   8.173104e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.496581e-55   3.033385e-01 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 biddable, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.npnct08.log, D.T.condit, color.fctr, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.288                 0.118
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.881512                    0.5       0.7972509        0.7581784
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7930128             0.8423427     0.5119935   0.8382507
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7696139        0.7909605
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7626579             0.8173104     0.5784228    960.7058
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01730177      0.03592571
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 50.031 80.247  30.216
## 11 fit.models          7          1 80.247     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor   bgn end elapsed
## 1 fit.models_1_bgn          1          0 84.51  NA      NA
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
## 1 fit.models_1_bgn          1          0 84.510 84.525   0.015
## 2 fit.models_1_glm          2          0 84.526     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_category_files/figure-html/fit.models_1-1.png) ![](ebayipads_category_files/figure-html/fit.models_1-2.png) ![](ebayipads_category_files/figure-html/fit.models_1-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](ebayipads_category_files/figure-html/fit.models_1-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.9833  -0.6917  -0.1657   0.6131   3.6180  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                               5.717e+00  6.410e+00   0.892
## biddable                                  1.433e+00  2.034e-01   7.048
## D.ratio.nstopwrds.nwrds                  -3.916e+00  6.399e+00  -0.612
## D.npnct15.log                             3.568e-01  1.065e+00   0.335
## D.T.screen                                5.292e-01  7.827e-01   0.676
## D.npnct03.log                            -4.285e-01  1.729e+00  -0.248
## D.T.used                                 -3.419e-01  7.795e-01  -0.439
## D.T.excellent                             4.056e-01  4.007e-01   1.012
## D.T.great                                -8.166e-01  7.110e-01  -1.148
## D.ratio.sum.TfIdf.nwrds                  -5.164e-01  5.863e-01  -0.881
## D.T.good                                  2.582e-01  6.803e-01   0.380
## D.npnct01.log                             4.627e-01  7.550e-01   0.613
## D.T.veri                                  8.269e-03  6.795e-01   0.012
## D.T.work                                  1.142e+00  6.857e-01   1.665
## D.T.scratch                               9.096e-01  8.183e-01   1.112
## D.P.air                                  -5.148e-01  1.152e+00  -0.447
## D.T.use                                   8.130e-01  7.913e-01   1.027
## storage.fctr16                           -8.941e-01  6.100e-01  -1.466
## storage.fctr32                           -9.173e-01  6.317e-01  -1.452
## storage.fctr64                           -4.662e-01  6.074e-01  -0.768
## storage.fctrUnknown                      -2.990e-01  7.990e-01  -0.374
## D.P.mini                                 -3.277e-01  1.001e+00  -0.327
## D.T.condition                            -3.240e-01  8.581e-01  -0.378
## D.npnct11.log                             5.959e-02  4.088e-01   0.146
## .rnorm                                   -3.414e-02  9.315e-02  -0.367
## D.T.ipad                                 -2.350e+00  1.018e+00  -2.308
## D.npnct10.log                             1.790e+00  1.503e+00   1.191
## D.sum.TfIdf                               3.108e-01  1.861e-01   1.670
## D.T.new                                  -1.815e-01  9.764e-01  -0.186
## D.npnct13.log                            -6.264e-01  4.729e-01  -1.325
## D.npnct08.log                            -6.511e-01  8.156e-01  -0.798
## D.T.condit                               -1.842e-01  6.424e-01  -0.287
## color.fctrBlack                          -2.037e-01  2.454e-01  -0.830
## color.fctrGold                            4.391e-01  5.811e-01   0.756
## `color.fctrSpace Gray`                    1.254e-01  3.524e-01   0.356
## color.fctrWhite                          -2.542e-01  2.520e-01  -1.009
## D.nstopwrds.log                           1.758e+00  1.901e+00   0.925
## D.npnct16.log                             1.177e+00  1.805e+00   0.652
## D.npnct24.log                            -7.072e-01  7.627e+00  -0.093
## D.T.this                                 -4.390e-02  1.129e+00  -0.039
## D.npnct06.log                            -2.339e+00  2.022e+00  -1.156
## D.T.box                                  -1.717e+00  9.028e-01  -1.902
## D.npnct28.log                            -1.729e+00  1.298e+03  -0.001
## D.T.like                                 -9.469e-01  8.912e-01  -1.063
## D.npnct12.log                             3.087e-01  7.942e-01   0.389
## D.nuppr.log                              -4.659e+00  6.689e+00  -0.696
## D.nchrs.log                               4.227e+00  7.724e+00   0.547
## D.nwrds.log                              -1.917e+00  2.895e+00  -0.662
## D.npnct09.log                            -9.410e+00  1.189e+03  -0.008
## D.nwrds.unq.log                          -5.520e-02  8.967e-01  -0.062
## D.ndgts.log                               2.764e-01  6.146e-01   0.450
## `carrier.fctrAT&T`                        1.479e+01  9.426e+02   0.016
## carrier.fctrOther                         3.015e+01  1.359e+03   0.022
## carrier.fctrSprint                        1.559e+01  9.426e+02   0.017
## `carrier.fctrT-Mobile`                    1.310e+01  9.426e+02   0.014
## carrier.fctrUnknown                       1.500e+01  9.426e+02   0.016
## carrier.fctrVerizon                       1.504e+01  9.426e+02   0.016
## cellular.fctr1                           -1.484e+01  9.426e+02  -0.016
## cellular.fctrUnknown                     -1.525e+01  9.426e+02  -0.016
## D.npnct14.log                            -5.488e-01  7.908e-01  -0.694
## `prdline.my.fctriPad 2`                   7.980e-01  3.569e-01   2.236
## `prdline.my.fctriPad 3+`                  1.147e+00  3.626e-01   3.163
## prdline.my.fctriPadAir                    2.498e+00  4.812e-01   5.190
## prdline.my.fctriPadmini                   1.921e-01  3.594e-01   0.535
## `prdline.my.fctriPadmini 2+`              1.066e+00  4.620e-01   2.307
## prdline.my.fctrUnknown                   -3.880e-01  4.648e-01  -0.835
## D.npnct05.log                            -3.549e+00  1.987e+00  -1.786
## `condition.fctrFor parts or not working` -9.350e-01  3.406e-01  -2.745
## `condition.fctrManufacturer refurbished` -4.652e-01  7.391e-01  -0.629
## condition.fctrNew                         7.518e-01  3.516e-01   2.138
## `condition.fctrNew other (see details)`   8.565e-01  4.816e-01   1.778
## `condition.fctrSeller refurbished`       -7.438e-01  3.961e-01  -1.878
## startprice                               -1.237e-02  1.350e-03  -9.162
##                                          Pr(>|z|)    
## (Intercept)                               0.37242    
## biddable                                 1.81e-12 ***
## D.ratio.nstopwrds.nwrds                   0.54055    
## D.npnct15.log                             0.73751    
## D.T.screen                                0.49898    
## D.npnct03.log                             0.80427    
## D.T.used                                  0.66096    
## D.T.excellent                             0.31148    
## D.T.great                                 0.25079    
## D.ratio.sum.TfIdf.nwrds                   0.37842    
## D.T.good                                  0.70430    
## D.npnct01.log                             0.53999    
## D.T.veri                                  0.99029    
## D.T.work                                  0.09596 .  
## D.T.scratch                               0.26634    
## D.P.air                                   0.65509    
## D.T.use                                   0.30419    
## storage.fctr16                            0.14271    
## storage.fctr32                            0.14647    
## storage.fctr64                            0.44273    
## storage.fctrUnknown                       0.70827    
## D.P.mini                                  0.74350    
## D.T.condition                             0.70577    
## D.npnct11.log                             0.88411    
## .rnorm                                    0.71399    
## D.T.ipad                                  0.02102 *  
## D.npnct10.log                             0.23363    
## D.sum.TfIdf                               0.09489 .  
## D.T.new                                   0.85253    
## D.npnct13.log                             0.18531    
## D.npnct08.log                             0.42471    
## D.T.condit                                0.77432    
## color.fctrBlack                           0.40650    
## color.fctrGold                            0.44986    
## `color.fctrSpace Gray`                    0.72206    
## color.fctrWhite                           0.31316    
## D.nstopwrds.log                           0.35497    
## D.npnct16.log                             0.51412    
## D.npnct24.log                             0.92612    
## D.T.this                                  0.96899    
## D.npnct06.log                             0.24750    
## D.T.box                                   0.05722 .  
## D.npnct28.log                             0.99894    
## D.T.like                                  0.28799    
## D.npnct12.log                             0.69749    
## D.nuppr.log                               0.48616    
## D.nchrs.log                               0.58422    
## D.nwrds.log                               0.50797    
## D.npnct09.log                             0.99369    
## D.nwrds.unq.log                           0.95091    
## D.ndgts.log                               0.65287    
## `carrier.fctrAT&T`                        0.98748    
## carrier.fctrOther                         0.98229    
## carrier.fctrSprint                        0.98681    
## `carrier.fctrT-Mobile`                    0.98891    
## carrier.fctrUnknown                       0.98731    
## carrier.fctrVerizon                       0.98727    
## cellular.fctr1                            0.98744    
## cellular.fctrUnknown                      0.98709    
## D.npnct14.log                             0.48766    
## `prdline.my.fctriPad 2`                   0.02534 *  
## `prdline.my.fctriPad 3+`                  0.00156 ** 
## prdline.my.fctriPadAir                   2.10e-07 ***
## prdline.my.fctriPadmini                   0.59299    
## `prdline.my.fctriPadmini 2+`              0.02105 *  
## prdline.my.fctrUnknown                    0.40392    
## D.npnct05.log                             0.07408 .  
## `condition.fctrFor parts or not working`  0.00605 ** 
## `condition.fctrManufacturer refurbished`  0.52913    
## condition.fctrNew                         0.03251 *  
## `condition.fctrNew other (see details)`   0.07535 .  
## `condition.fctrSeller refurbished`        0.06040 .  
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  829.99  on 903  degrees of freedom
## AIC: 975.99
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.7036145
## 3        0.2 0.7484444
## 4        0.3 0.7714844
## 5        0.4 0.7783669
## 6        0.5 0.8018328
## 7        0.6 0.7956469
## 8        0.7 0.7680608
## 9        0.8 0.6761364
## 10       0.9 0.3985891
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_1-6.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           453                            72
## 2         Y                           101                           350
##          Prediction
## Reference   N   Y
##         N 453  72
##         Y 101 350
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.227459e-01   6.418189e-01   7.973128e-01   8.462141e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   2.759431e-78   3.327076e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6736658
## 3        0.2 0.6972302
## 4        0.3 0.7230930
## 5        0.4 0.7417520
## 6        0.5 0.7542998
## 7        0.6 0.7549148
## 8        0.7 0.7451524
## 9        0.8 0.6687403
## 10       0.9 0.4526902
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           410                            66
## 2         Y                           121                           288
##          Prediction
## Reference   N   Y
##         N 410  66
##         Y 121 288
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.887006e-01   5.709041e-01   7.603001e-01   8.151638e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.563801e-54   7.852062e-05 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.299                 0.167
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8870109                    0.5       0.8018328        0.7448639
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7973128             0.8462141     0.4851323   0.8358417
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7549148        0.7887006
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7603001             0.8151638     0.5709041     975.995
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01656657       0.0360217
##                   label step_major step_minor    bgn    end elapsed
## 2      fit.models_1_glm          2          0 84.526 89.552   5.026
## 3 fit.models_1_bayesglm          3          0 89.553     NA      NA
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

![](ebayipads_category_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8995  -0.7006  -0.1794   0.6241   3.5425  
## 
## Coefficients:
##                                           Estimate Std. Error z value
## (Intercept)                               1.326245   2.234300   0.594
## biddable                                  1.430800   0.197868   7.231
## D.ratio.nstopwrds.nwrds                   0.279396   2.161167   0.129
## D.npnct15.log                             0.246466   0.875519   0.282
## D.T.screen                                0.371823   0.740844   0.502
## D.npnct03.log                            -0.151577   1.559747  -0.097
## D.T.used                                 -0.076093   0.593349  -0.128
## D.T.excellent                             0.295404   0.376820   0.784
## D.T.great                                -0.772169   0.660316  -1.169
## D.ratio.sum.TfIdf.nwrds                  -0.362930   0.381269  -0.952
## D.T.good                                  0.214973   0.648729   0.331
## D.npnct01.log                             0.689684   0.616139   1.119
## D.T.veri                                  0.057455   0.636317   0.090
## D.T.work                                  1.126267   0.658422   1.711
## D.T.scratch                               0.823797   0.767894   1.073
## D.P.air                                  -0.235538   1.101105  -0.214
## D.T.use                                   0.761266   0.716233   1.063
## storage.fctr16                           -0.649785   0.517477  -1.256
## storage.fctr32                           -0.680394   0.539923  -1.260
## storage.fctr64                           -0.255036   0.523657  -0.487
## storage.fctrUnknown                      -0.154791   0.682778  -0.227
## D.P.mini                                 -0.055113   0.835883  -0.066
## D.T.condition                            -0.366521   0.737127  -0.497
## D.npnct11.log                             0.065254   0.361604   0.180
## .rnorm                                   -0.035862   0.091422  -0.392
## D.T.ipad                                 -2.337551   0.969029  -2.412
## D.npnct10.log                             1.936410   1.497852   1.293
## D.sum.TfIdf                               0.256656   0.146651   1.750
## D.T.new                                   0.031996   0.916280   0.035
## D.npnct13.log                            -0.470544   0.363514  -1.294
## D.npnct08.log                            -0.542010   0.776839  -0.698
## D.T.condit                               -0.237640   0.566436  -0.420
## color.fctrBlack                          -0.236315   0.236702  -0.998
## color.fctrGold                            0.416282   0.547182   0.761
## `color.fctrSpace Gray`                    0.111843   0.338024   0.331
## color.fctrWhite                          -0.265657   0.243764  -1.090
## D.nstopwrds.log                           0.312093   0.614174   0.508
## D.npnct16.log                             1.036123   1.541581   0.672
## D.npnct24.log                             0.541401   2.499237   0.217
## D.T.this                                 -0.215903   1.062622  -0.203
## D.npnct06.log                            -1.956217   1.749153  -1.118
## D.T.box                                  -1.616395   0.898596  -1.799
## D.npnct28.log                            -1.094778   9.057892  -0.121
## D.T.like                                 -0.849249   0.839968  -1.011
## D.npnct12.log                             0.172285   0.736248   0.234
## D.nuppr.log                              -0.142553   0.503497  -0.283
## D.nchrs.log                              -0.086732   0.483338  -0.179
## D.nwrds.log                              -0.115711   0.731453  -0.158
## D.npnct09.log                            -1.933400   6.369257  -0.304
## D.nwrds.unq.log                          -0.280589   0.557809  -0.503
## D.ndgts.log                               0.277186   0.471696   0.588
## `carrier.fctrAT&T`                       -0.004495   0.730304  -0.006
## carrier.fctrOther                         1.129533   1.732369   0.652
## carrier.fctrSprint                        0.729582   0.878420   0.831
## `carrier.fctrT-Mobile`                   -1.248360   1.076500  -1.160
## carrier.fctrUnknown                       0.266600   0.721073   0.370
## carrier.fctrVerizon                       0.256039   0.754848   0.339
## cellular.fctr1                           -0.071768   0.701243  -0.102
## cellular.fctrUnknown                     -0.518954   0.802494  -0.647
## D.npnct14.log                            -0.470746   0.764241  -0.616
## `prdline.my.fctriPad 2`                   0.685289   0.338070   2.027
## `prdline.my.fctriPad 3+`                  0.986573   0.338442   2.915
## prdline.my.fctriPadAir                    2.278939   0.445695   5.113
## prdline.my.fctriPadmini                   0.083488   0.336700   0.248
## `prdline.my.fctriPadmini 2+`              0.890909   0.430584   2.069
## prdline.my.fctrUnknown                   -0.432301   0.438283  -0.986
## D.npnct05.log                            -2.652538   1.549202  -1.712
## `condition.fctrFor parts or not working` -0.813717   0.327602  -2.484
## `condition.fctrManufacturer refurbished` -0.387686   0.680418  -0.570
## condition.fctrNew                         0.707624   0.339567   2.084
## `condition.fctrNew other (see details)`   0.822568   0.447928   1.836
## `condition.fctrSeller refurbished`       -0.757709   0.377560  -2.007
## startprice                               -0.011833   0.001261  -9.381
##                                          Pr(>|z|)    
## (Intercept)                               0.55279    
## biddable                                 4.79e-13 ***
## D.ratio.nstopwrds.nwrds                   0.89714    
## D.npnct15.log                             0.77832    
## D.T.screen                                0.61574    
## D.npnct03.log                             0.92258    
## D.T.used                                  0.89796    
## D.T.excellent                             0.43308    
## D.T.great                                 0.24225    
## D.ratio.sum.TfIdf.nwrds                   0.34115    
## D.T.good                                  0.74036    
## D.npnct01.log                             0.26298    
## D.T.veri                                  0.92805    
## D.T.work                                  0.08716 .  
## D.T.scratch                               0.28336    
## D.P.air                                   0.83062    
## D.T.use                                   0.28784    
## storage.fctr16                            0.20923    
## storage.fctr32                            0.20761    
## storage.fctr64                            0.62624    
## storage.fctrUnknown                       0.82065    
## D.P.mini                                  0.94743    
## D.T.condition                             0.61903    
## D.npnct11.log                             0.85679    
## .rnorm                                    0.69486    
## D.T.ipad                                  0.01585 *  
## D.npnct10.log                             0.19608    
## D.sum.TfIdf                               0.08010 .  
## D.T.new                                   0.97214    
## D.npnct13.log                             0.19552    
## D.npnct08.log                             0.48536    
## D.T.condit                                0.67482    
## color.fctrBlack                           0.31810    
## color.fctrGold                            0.44679    
## `color.fctrSpace Gray`                    0.74074    
## color.fctrWhite                           0.27580    
## D.nstopwrds.log                           0.61135    
## D.npnct16.log                             0.50151    
## D.npnct24.log                             0.82850    
## D.T.this                                  0.83899    
## D.npnct06.log                             0.26341    
## D.T.box                                   0.07205 .  
## D.npnct28.log                             0.90380    
## D.T.like                                  0.31199    
## D.npnct12.log                             0.81498    
## D.nuppr.log                               0.77708    
## D.nchrs.log                               0.85759    
## D.nwrds.log                               0.87430    
## D.npnct09.log                             0.76147    
## D.nwrds.unq.log                           0.61495    
## D.ndgts.log                               0.55678    
## `carrier.fctrAT&T`                        0.99509    
## carrier.fctrOther                         0.51439    
## carrier.fctrSprint                        0.40622    
## `carrier.fctrT-Mobile`                    0.24619    
## carrier.fctrUnknown                       0.71159    
## carrier.fctrVerizon                       0.73446    
## cellular.fctr1                            0.91848    
## cellular.fctrUnknown                      0.51784    
## D.npnct14.log                             0.53792    
## `prdline.my.fctriPad 2`                   0.04266 *  
## `prdline.my.fctriPad 3+`                  0.00356 ** 
## prdline.my.fctriPadAir                   3.17e-07 ***
## prdline.my.fctriPadmini                   0.80417    
## `prdline.my.fctriPadmini 2+`              0.03854 *  
## prdline.my.fctrUnknown                    0.32396    
## D.npnct05.log                             0.08686 .  
## `condition.fctrFor parts or not working`  0.01300 *  
## `condition.fctrManufacturer refurbished`  0.56883    
## condition.fctrNew                         0.03717 *  
## `condition.fctrNew other (see details)`   0.06630 .  
## `condition.fctrSeller refurbished`        0.04477 *  
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  833.81  on 903  degrees of freedom
## AIC: 979.81
## 
## Number of Fisher Scoring iterations: 11
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.7003205
## 3        0.2 0.7440213
## 4        0.3 0.7692308
## 5        0.4 0.7793031
## 6        0.5 0.7986348
## 7        0.6 0.7927273
## 8        0.7 0.7625160
## 9        0.8 0.6846591
## 10       0.9 0.3756708
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                448
## 2         Y                                100
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 77
## 2                                351
##          Prediction
## Reference   N   Y
##         N 448  77
##         Y 100 351
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.186475e-01   6.338821e-01   7.930128e-01   8.423427e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   6.599723e-76   9.820449e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6724587
## 3        0.2 0.7002854
## 4        0.3 0.7272727
## 5        0.4 0.7437071
## 6        0.5 0.7602484
## 7        0.6 0.7595269
## 8        0.7 0.7486034
## 9        0.8 0.6728972
## 10       0.9 0.4339623
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                386
## 2         Y                                103
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 90
## 2                                306
##          Prediction
## Reference   N   Y
##         N 386  90
##         Y 103 306
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.819209e-01   5.603441e-01   7.532341e-01   8.087166e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.518661e-51   3.877094e-01 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.239                 0.214
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.886238                    0.5       0.7986348        0.7489476
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7930128             0.8423427     0.4938653   0.8374956
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7602484        0.7819209
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7532341             0.8087166     0.5603441    979.8148
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02808116      0.05813155
##                   label step_major step_minor    bgn    end elapsed
## 3 fit.models_1_bayesglm          3          0 89.553 94.994   5.441
## 4    fit.models_1_rpart          4          0 94.995     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0266 on full training set
```

![](ebayipads_category_files/figure-html/fit.models_1-13.png) ![](ebayipads_category_files/figure-html/fit.models_1-14.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 976 
## 
##           CP nsplit rel error
## 1 0.49445676      0 1.0000000
## 2 0.02660754      1 0.5055432
## 
## Variable importance
##                biddable              startprice D.ratio.sum.TfIdf.nwrds 
##                      66                      29                       2 
## prdline.my.fctriPadmini               D.T.great              D.T.screen 
##                       2                       1                       1 
## 
## Node number 1: 976 observations,    complexity param=0.4944568
##   predicted class=N  expected loss=0.4620902  P(node) =1
##     class counts:   525   451
##    probabilities: 0.538 0.462 
##   left son=2 (539 obs) right son=3 (437 obs)
##   Primary splits:
##       biddable          < 0.5       to the left,  improve=135.919400, (0 missing)
##       startprice        < 151.435   to the right, improve=117.232600, (0 missing)
##       condition.fctrNew < 0.5       to the right, improve= 11.614270, (0 missing)
##       D.npnct05.log     < 0.3465736 to the right, improve=  6.629592, (0 missing)
##       D.nwrds.unq.log   < 2.249905  to the right, improve=  4.816042, (0 missing)
##   Surrogate splits:
##       startprice              < 102.35    to the right, agree=0.747, adj=0.435, (0 split)
##       D.ratio.sum.TfIdf.nwrds < 0.8721891 to the left,  agree=0.564, adj=0.025, (0 split)
##       prdline.my.fctriPadmini < 0.5       to the left,  agree=0.564, adj=0.025, (0 split)
##       D.T.great               < 0.4266606 to the left,  agree=0.561, adj=0.021, (0 split)
##       D.T.screen              < 0.4356448 to the left,  agree=0.560, adj=0.018, (0 split)
## 
## Node number 2: 539 observations
##   predicted class=N  expected loss=0.2244898  P(node) =0.5522541
##     class counts:   418   121
##    probabilities: 0.776 0.224 
## 
## Node number 3: 437 observations
##   predicted class=Y  expected loss=0.2448513  P(node) =0.4477459
##     class counts:   107   330
##    probabilities: 0.245 0.755 
## 
## n= 976 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 976 451 N (0.5379098 0.4620902)  
##   2) biddable< 0.5 539 121 N (0.7755102 0.2244898) *
##   3) biddable>=0.5 437 107 Y (0.2448513 0.7551487) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_1-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.6320953
## 3        0.2 0.6320953
## 4        0.3 0.7432432
## 5        0.4 0.7432432
## 6        0.5 0.7432432
## 7        0.6 0.7432432
## 8        0.7 0.7432432
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_category_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      418
## 2         Y                                      121
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                      107
## 2                                      330
##          Prediction
## Reference   N   Y
##         N 418 107
##         Y 121 330
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.663934e-01   5.290552e-01   7.385542e-01   7.926151e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##   1.631860e-49   3.892678e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6321484
## 3        0.2 0.6321484
## 4        0.3 0.7663782
## 5        0.4 0.7663782
## 6        0.5 0.7663782
## 7        0.6 0.7663782
## 8        0.7 0.7663782
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      386
## 2         Y                                       99
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       90
## 2                                      310
##          Prediction
## Reference   N   Y
##         N 386  90
##         Y  99 310
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.864407e-01   5.697529e-01   7.579436e-01   8.130160e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.590529e-53   5.606244e-01 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.422                 0.056
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7639489                    0.7       0.7432432        0.7786755
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7385542             0.7926151     0.5490774   0.7844353
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.7663782        0.7864407
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7579436              0.813016     0.5697529
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03268626       0.0682405
##                label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_rpart          4          0 94.995 99.879   4.884
## 5    fit.models_1_rf          5          0 99.879     NA      NA
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

![](ebayipads_category_files/figure-html/fit.models_1-18.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 36 on full training set
```

![](ebayipads_category_files/figure-html/fit.models_1-19.png) ![](ebayipads_category_files/figure-html/fit.models_1-20.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        976   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           1952   matrix     numeric  
## oob.times        976   -none-     numeric  
## classes            2   -none-     character
## importance        71   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                976   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            71   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.models_1-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320953
## 2        0.1 0.8177697
## 3        0.2 0.9185336
## 4        0.3 0.9678112
## 5        0.4 0.9900990
## 6        0.5 0.9933481
## 7        0.6 0.9854423
## 8        0.7 0.9227111
## 9        0.8 0.8469388
## 10       0.9 0.7366947
## 11       1.0 0.1402062
```

![](ebayipads_category_files/figure-html/fit.models_1-22.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   522
## 2         Y                                     3
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                     3
## 2                                   448
##          Prediction
## Reference   N   Y
##         N 522   3
##         Y   3 448
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.938525e-01   9.876338e-01   9.866677e-01   9.977407e-01   5.379098e-01 
## AccuracyPValue  McnemarPValue 
##  7.118232e-249   1.000000e+00 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_category_files/figure-html/fit.models_1-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6321484
## 2        0.1 0.6638225
## 3        0.2 0.7111111
## 4        0.3 0.7459807
## 5        0.4 0.7638073
## 6        0.5 0.7604563
## 7        0.6 0.7550744
## 8        0.7 0.7309353
## 9        0.8 0.6951220
## 10       0.9 0.5976628
## 11       1.0 0.1614350
```

![](ebayipads_category_files/figure-html/fit.models_1-24.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   359
## 2         Y                                    84
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                   117
## 2                                   325
##          Prediction
## Reference   N   Y
##         N 359 117
##         Y  84 325
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.728814e-01   5.457239e-01   7.438293e-01   8.001036e-01   5.378531e-01 
## AccuracyPValue  McnemarPValue 
##   1.010474e-47   2.400145e-02 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, D.T.ipad, D.npnct10.log, D.sum.TfIdf, D.T.new, D.npnct13.log, D.npnct08.log, D.T.condit, color.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, carrier.fctr, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     14.385                 4.385
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9989082                    0.5       0.9933481        0.7919899
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9866677             0.9977407     0.5778484   0.8498233
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7638073        0.7728814
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7438293             0.8001036     0.5457239
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.04144412      0.08388316
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
## MFO.myMFO_classfr                       0                      0.869
## Random.myrandom_classfr                 0                      0.251
## Max.cor.Y.cv.0.rpart                    0                      1.129
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.463
## Max.cor.Y.rpart                         3                      0.998
## Max.cor.Y.glm                           1                      1.040
## Interact.High.cor.Y.glm                 1                      1.029
## Low.cor.X.glm                           1                      1.288
## All.X.glm                               1                      1.299
## All.X.bayesglm                          1                      2.239
## All.X.no.rnorm.rpart                    3                      1.422
## All.X.no.rnorm.rf                       3                     14.385
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.003   0.5000000
## Random.myrandom_classfr                   0.001   0.5040735
## Max.cor.Y.cv.0.rpart                      0.012   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.009   0.8769591
## Max.cor.Y.rpart                           0.012   0.7639489
## Max.cor.Y.glm                             0.012   0.8473846
## Interact.High.cor.Y.glm                   0.018   0.8481153
## Low.cor.X.glm                             0.118   0.8815120
## All.X.glm                                 0.167   0.8870109
## All.X.bayesglm                            0.214   0.8862380
## All.X.no.rnorm.rpart                      0.056   0.7639489
## All.X.no.rnorm.rf                         4.385   0.9989082
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6320953
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.7964602
## Max.cor.Y.rpart                              0.7       0.7432432
## Max.cor.Y.glm                                0.4       0.7521368
## Interact.High.cor.Y.glm                      0.4       0.7547974
## Low.cor.X.glm                                0.5       0.7972509
## All.X.glm                                    0.5       0.8018328
## All.X.bayesglm                               0.5       0.7986348
## All.X.no.rnorm.rpart                         0.7       0.7432432
## All.X.no.rnorm.rf                            0.5       0.9933481
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.5379098             0.5060449
## Random.myrandom_classfr          0.4620902             0.4304545
## Max.cor.Y.cv.0.rpart             0.5379098             0.5060449
## Max.cor.Y.cv.0.cp.0.rpart        0.8114754             0.7854988
## Max.cor.Y.rpart                  0.7786629             0.7385542
## Max.cor.Y.glm                    0.7755923             0.7343087
## Interact.High.cor.Y.glm          0.7755923             0.7364311
## Low.cor.X.glm                    0.7581784             0.7930128
## All.X.glm                        0.7448639             0.7973128
## All.X.bayesglm                   0.7489476             0.7930128
## All.X.no.rnorm.rpart             0.7786755             0.7385542
## All.X.no.rnorm.rf                0.7919899             0.9866677
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.5695455     0.0000000   0.5000000
## Random.myrandom_classfr               0.4939551     0.0000000   0.5214656
## Max.cor.Y.cv.0.rpart                  0.5695455     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8355569     0.6208893   0.8257253
## Max.cor.Y.rpart                       0.7926151     0.5507393   0.7844353
## Max.cor.Y.glm                         0.7886889     0.5460308   0.8351149
## Interact.High.cor.Y.glm               0.7906524     0.5457753   0.8351868
## Low.cor.X.glm                         0.8423427     0.5119935   0.8382507
## All.X.glm                             0.8462141     0.4851323   0.8358417
## All.X.bayesglm                        0.8423427     0.4938653   0.8374956
## All.X.no.rnorm.rpart                  0.7926151     0.5490774   0.7844353
## All.X.no.rnorm.rf                     0.9977407     0.5778484   0.8498233
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6321484
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.5       0.7515763
## Max.cor.Y.rpart                              0.7       0.7663782
## Max.cor.Y.glm                                0.5       0.7643312
## Interact.High.cor.Y.glm                      0.5       0.7580026
## Low.cor.X.glm                                0.5       0.7696139
## All.X.glm                                    0.6       0.7549148
## All.X.bayesglm                               0.5       0.7602484
## All.X.no.rnorm.rpart                         0.7       0.7663782
## All.X.no.rnorm.rf                            0.4       0.7638073
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.5378531             0.5043611
## Random.myrandom_classfr          0.4621469             0.4289077
## Max.cor.Y.cv.0.rpart             0.5378531             0.5043611
## Max.cor.Y.cv.0.cp.0.rpart        0.7774011             0.7485294
## Max.cor.Y.rpart                  0.7864407             0.7579436
## Max.cor.Y.glm                    0.7909605             0.7626579
## Interact.High.cor.Y.glm          0.7864407             0.7579436
## Low.cor.X.glm                    0.7909605             0.7626579
## All.X.glm                        0.7887006             0.7603001
## All.X.bayesglm                   0.7819209             0.7532341
## All.X.no.rnorm.rpart             0.7864407             0.7579436
## All.X.no.rnorm.rf                0.7728814             0.7438293
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.5710923     0.0000000
## Random.myrandom_classfr               0.4956389     0.0000000
## Max.cor.Y.cv.0.rpart                  0.5710923     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8044124     0.5503014
## Max.cor.Y.rpart                       0.8130160     0.5697529
## Max.cor.Y.glm                         0.8173104     0.5771095
## Interact.High.cor.Y.glm               0.8130160     0.5676667
## Low.cor.X.glm                         0.8173104     0.5784228
## All.X.glm                             0.8151638     0.5709041
## All.X.bayesglm                        0.8087166     0.5603441
## All.X.no.rnorm.rpart                  0.8130160     0.5697529
## All.X.no.rnorm.rf                     0.8001036     0.5457239
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                   0.04549097      0.09113973          NA
## Max.cor.Y.glm                     0.03748683      0.07662323    959.0160
## Interact.High.cor.Y.glm           0.03748683      0.07640194    967.0557
## Low.cor.X.glm                     0.01730177      0.03592571    960.7058
## All.X.glm                         0.01656657      0.03602170    975.9950
## All.X.bayesglm                    0.02808116      0.05813155    979.8148
## All.X.no.rnorm.rpart              0.03268626      0.06824050          NA
## All.X.no.rnorm.rf                 0.04144412      0.08388316          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 5  fit.models_1_rf          5          0  99.879 117.904  18.025
## 6 fit.models_1_end          6          0 117.905      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1  80.247 117.912  37.665
## 12 fit.models          7          2 117.912      NA      NA
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
## Random.myrandom_classfr                 0   0.5040735
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.8769591
## Max.cor.Y.rpart                         3   0.7639489
## Max.cor.Y.glm                           1   0.8473846
## Interact.High.cor.Y.glm                 1   0.8481153
## Low.cor.X.glm                           1   0.8815120
## All.X.glm                               1   0.8870109
## All.X.bayesglm                          1   0.8862380
## All.X.no.rnorm.rpart                    3   0.7639489
## All.X.no.rnorm.rf                       3   0.9989082
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6320953
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.7964602
## Max.cor.Y.rpart                              0.7       0.7432432
## Max.cor.Y.glm                                0.4       0.7521368
## Interact.High.cor.Y.glm                      0.4       0.7547974
## Low.cor.X.glm                                0.5       0.7972509
## All.X.glm                                    0.5       0.8018328
## All.X.bayesglm                               0.5       0.7986348
## All.X.no.rnorm.rpart                         0.7       0.7432432
## All.X.no.rnorm.rf                            0.5       0.9933481
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.5379098     0.0000000   0.5000000
## Random.myrandom_classfr          0.4620902     0.0000000   0.5214656
## Max.cor.Y.cv.0.rpart             0.5379098     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8114754     0.6208893   0.8257253
## Max.cor.Y.rpart                  0.7786629     0.5507393   0.7844353
## Max.cor.Y.glm                    0.7755923     0.5460308   0.8351149
## Interact.High.cor.Y.glm          0.7755923     0.5457753   0.8351868
## Low.cor.X.glm                    0.7581784     0.5119935   0.8382507
## All.X.glm                        0.7448639     0.4851323   0.8358417
## All.X.bayesglm                   0.7489476     0.4938653   0.8374956
## All.X.no.rnorm.rpart             0.7786755     0.5490774   0.7844353
## All.X.no.rnorm.rf                0.7919899     0.5778484   0.8498233
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6321484
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.5       0.7515763
## Max.cor.Y.rpart                              0.7       0.7663782
## Max.cor.Y.glm                                0.5       0.7643312
## Interact.High.cor.Y.glm                      0.5       0.7580026
## Low.cor.X.glm                                0.5       0.7696139
## All.X.glm                                    0.6       0.7549148
## All.X.bayesglm                               0.5       0.7602484
## All.X.no.rnorm.rpart                         0.7       0.7663782
## All.X.no.rnorm.rf                            0.4       0.7638073
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.5378531     0.0000000
## Random.myrandom_classfr          0.4621469     0.0000000
## Max.cor.Y.cv.0.rpart             0.5378531     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart        0.7774011     0.5503014
## Max.cor.Y.rpart                  0.7864407     0.5697529
## Max.cor.Y.glm                    0.7909605     0.5771095
## Interact.High.cor.Y.glm          0.7864407     0.5676667
## Low.cor.X.glm                    0.7909605     0.5784228
## All.X.glm                        0.7887006     0.5709041
## All.X.bayesglm                   0.7819209     0.5603441
## All.X.no.rnorm.rpart             0.7864407     0.5697529
## All.X.no.rnorm.rf                0.7728814     0.5457239
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         1.15074799           333.3333333
## Random.myrandom_classfr                   3.98406375          1000.0000000
## Max.cor.Y.cv.0.rpart                      0.88573959            83.3333333
## Max.cor.Y.cv.0.cp.0.rpart                 2.15982721           111.1111111
## Max.cor.Y.rpart                           1.00200401            83.3333333
## Max.cor.Y.glm                             0.96153846            83.3333333
## Interact.High.cor.Y.glm                   0.97181730            55.5555556
## Low.cor.X.glm                             0.77639752             8.4745763
## All.X.glm                                 0.76982294             5.9880240
## All.X.bayesglm                            0.44662796             4.6728972
## All.X.no.rnorm.rpart                      0.70323488            17.8571429
## All.X.no.rnorm.rf                         0.06951686             0.2280502
##                           inv.aic.fit
## MFO.myMFO_classfr                  NA
## Random.myrandom_classfr            NA
## Max.cor.Y.cv.0.rpart               NA
## Max.cor.Y.cv.0.cp.0.rpart          NA
## Max.cor.Y.rpart                    NA
## Max.cor.Y.glm             0.001042735
## Interact.High.cor.Y.glm   0.001034067
## Low.cor.X.glm             0.001040901
## All.X.glm                 0.001024595
## All.X.bayesglm            0.001020601
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

![](ebayipads_category_files/figure-html/fit.models_2-1.png) 

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

![](ebayipads_category_files/figure-html/fit.models_2-2.png) 

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
## 8              Low.cor.X.glm        0.7909605   0.8382507     0.5784228
## 6              Max.cor.Y.glm        0.7909605   0.8351149     0.5771095
## 9                  All.X.glm        0.7887006   0.8358417     0.5709041
## 7    Interact.High.cor.Y.glm        0.7864407   0.8351868     0.5676667
## 5            Max.cor.Y.rpart        0.7864407   0.7844353     0.5697529
## 11      All.X.no.rnorm.rpart        0.7864407   0.7844353     0.5697529
## 10            All.X.bayesglm        0.7819209   0.8374956     0.5603441
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7774011   0.8257253     0.5503014
## 12         All.X.no.rnorm.rf        0.7728814   0.8498233     0.5457239
## 1          MFO.myMFO_classfr        0.5378531   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5378531   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4621469   0.5214656     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 8     960.7058                    0.5
## 6     959.0160                    0.5
## 9     975.9950                    0.6
## 7     967.0557                    0.5
## 5           NA                    0.7
## 11          NA                    0.7
## 10    979.8148                    0.5
## 4           NA                    0.5
## 12          NA                    0.4
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

![](ebayipads_category_files/figure-html/fit.models_2-3.png) 

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
## [1] "Best model id: Low.cor.X.glm"
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

![](ebayipads_category_files/figure-html/fit.models_2-4.png) ![](ebayipads_category_files/figure-html/fit.models_2-5.png) ![](ebayipads_category_files/figure-html/fit.models_2-6.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](ebayipads_category_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.9056  -0.6964  -0.1687   0.6291   3.5433  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                               1.572e+00  7.139e-01   2.201
## biddable                                  1.450e+00  1.983e-01   7.314
## D.npnct15.log                             7.602e-01  8.053e-01   0.944
## D.T.screen                                2.495e-01  7.429e-01   0.336
## D.npnct03.log                             3.347e-01  1.470e+00   0.228
## D.T.used                                 -1.452e-01  5.266e-01  -0.276
## D.T.excellent                             1.699e-01  3.591e-01   0.473
## D.T.great                                -8.464e-01  6.483e-01  -1.306
## D.ratio.sum.TfIdf.nwrds                  -4.515e-02  2.119e-01  -0.213
## D.T.good                                  1.997e-01  6.648e-01   0.300
## D.npnct01.log                             9.810e-01  5.975e-01   1.642
## D.T.veri                                  2.711e-02  6.315e-01   0.043
## D.T.work                                  1.048e+00  6.567e-01   1.595
## D.T.scratch                               5.078e-01  7.398e-01   0.686
## D.P.air                                  -1.502e-01  1.109e+00  -0.135
## D.T.use                                   5.411e-01  6.879e-01   0.787
## storage.fctr16                           -6.758e-01  6.100e-01  -1.108
## storage.fctr32                           -7.115e-01  6.277e-01  -1.133
## storage.fctr64                           -2.951e-01  6.105e-01  -0.483
## storage.fctrUnknown                      -2.647e-01  7.960e-01  -0.333
## D.P.mini                                 -1.979e-01  8.954e-01  -0.221
## D.T.condition                            -6.769e-01  6.919e-01  -0.978
## D.npnct11.log                             2.918e-02  3.501e-01   0.083
## .rnorm                                   -5.042e-02  9.188e-02  -0.549
## D.T.ipad                                 -2.384e+00  9.595e-01  -2.485
## D.npnct10.log                             1.832e+00  1.479e+00   1.238
## D.npnct08.log                            -3.525e-01  7.535e-01  -0.468
## D.T.condit                               -4.593e-01  5.305e-01  -0.866
## color.fctrBlack                          -2.564e-01  2.377e-01  -1.079
## color.fctrGold                            4.437e-01  5.672e-01   0.782
## `color.fctrSpace Gray`                    1.062e-01  3.432e-01   0.310
## color.fctrWhite                          -2.810e-01  2.444e-01  -1.150
## D.T.this                                  2.036e-01  9.865e-01   0.206
## D.npnct06.log                            -1.070e+00  9.123e-01  -1.172
## D.T.box                                  -1.741e+00  9.056e-01  -1.923
## D.npnct28.log                            -1.448e+00  1.187e+03  -0.001
## D.T.like                                 -1.047e+00  5.213e-01  -2.008
## D.npnct12.log                             1.773e-01  7.310e-01   0.243
## D.npnct09.log                            -9.279e+00  1.085e+03  -0.009
## D.nwrds.unq.log                           2.277e-02  1.487e-01   0.153
## D.ndgts.log                               1.932e-01  4.489e-01   0.430
## cellular.fctr1                            7.722e-02  2.167e-01   0.356
## cellular.fctrUnknown                     -2.655e-01  4.701e-01  -0.565
## D.npnct14.log                            -1.988e-01  7.728e-01  -0.257
## `prdline.my.fctriPad 2`                   7.757e-01  3.475e-01   2.232
## `prdline.my.fctriPad 3+`                  1.109e+00  3.515e-01   3.154
## prdline.my.fctriPadAir                    2.359e+00  4.644e-01   5.080
## prdline.my.fctriPadmini                   2.065e-01  3.494e-01   0.591
## `prdline.my.fctriPadmini 2+`              1.007e+00  4.526e-01   2.224
## prdline.my.fctrUnknown                   -3.223e-01  4.556e-01  -0.707
## D.npnct05.log                            -3.148e+00  1.741e+00  -1.809
## `condition.fctrFor parts or not working` -7.705e-01  3.285e-01  -2.345
## `condition.fctrManufacturer refurbished` -3.734e-01  7.310e-01  -0.511
## condition.fctrNew                         7.490e-01  3.472e-01   2.157
## `condition.fctrNew other (see details)`   8.880e-01  4.346e-01   2.043
## `condition.fctrSeller refurbished`       -9.066e-01  3.833e-01  -2.365
## startprice                               -1.194e-02  1.303e-03  -9.164
##                                          Pr(>|z|)    
## (Intercept)                               0.02770 *  
## biddable                                 2.59e-13 ***
## D.npnct15.log                             0.34520    
## D.T.screen                                0.73701    
## D.npnct03.log                             0.81989    
## D.T.used                                  0.78278    
## D.T.excellent                             0.63607    
## D.T.great                                 0.19171    
## D.ratio.sum.TfIdf.nwrds                   0.83127    
## D.T.good                                  0.76382    
## D.npnct01.log                             0.10060    
## D.T.veri                                  0.96576    
## D.T.work                                  0.11061    
## D.T.scratch                               0.49249    
## D.P.air                                   0.89226    
## D.T.use                                   0.43151    
## storage.fctr16                            0.26792    
## storage.fctr32                            0.25701    
## storage.fctr64                            0.62889    
## storage.fctrUnknown                       0.73946    
## D.P.mini                                  0.82503    
## D.T.condition                             0.32792    
## D.npnct11.log                             0.93358    
## .rnorm                                    0.58317    
## D.T.ipad                                  0.01295 *  
## D.npnct10.log                             0.21559    
## D.npnct08.log                             0.63997    
## D.T.condit                                0.38657    
## color.fctrBlack                           0.28066    
## color.fctrGold                            0.43407    
## `color.fctrSpace Gray`                    0.75687    
## color.fctrWhite                           0.25020    
## D.T.this                                  0.83650    
## D.npnct06.log                             0.24101    
## D.T.box                                   0.05454 .  
## D.npnct28.log                             0.99903    
## D.T.like                                  0.04464 *  
## D.npnct12.log                             0.80833    
## D.npnct09.log                             0.99317    
## D.nwrds.unq.log                           0.87829    
## D.ndgts.log                               0.66685    
## cellular.fctr1                            0.72164    
## cellular.fctrUnknown                      0.57222    
## D.npnct14.log                             0.79693    
## `prdline.my.fctriPad 2`                   0.02559 *  
## `prdline.my.fctriPad 3+`                  0.00161 ** 
## prdline.my.fctriPadAir                   3.78e-07 ***
## prdline.my.fctriPadmini                   0.55459    
## `prdline.my.fctriPadmini 2+`              0.02613 *  
## prdline.my.fctrUnknown                    0.47932    
## D.npnct05.log                             0.07052 .  
## `condition.fctrFor parts or not working`  0.01900 *  
## `condition.fctrManufacturer refurbished`  0.60955    
## condition.fctrNew                         0.03097 *  
## `condition.fctrNew other (see details)`   0.04101 *  
## `condition.fctrSeller refurbished`        0.01802 *  
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1347.41  on 975  degrees of freedom
## Residual deviance:  846.71  on 919  degrees of freedom
## AIC: 960.71
## 
## Number of Fisher Scoring iterations: 14
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
##                                            importance
## startprice                               100.00000000
## biddable                                  79.80954040
## prdline.my.fctriPadAir                    55.42496678
## `prdline.my.fctriPad 3+`                  34.41293234
## D.T.ipad                                  27.10904281
## `condition.fctrSeller refurbished`        25.79969773
## `condition.fctrFor parts or not working`  25.58447029
## `prdline.my.fctriPad 2`                   24.34979172
## `prdline.my.fctriPadmini 2+`              24.26241952
## condition.fctrNew                         23.53196621
## `condition.fctrNew other (see details)`   22.28763325
## D.T.like                                  21.90219439
## D.T.box                                   20.96837457
## D.npnct05.log                             19.72484707
## D.npnct01.log                             17.90620310
## D.T.work                                  17.39888212
## D.T.great                                 14.23487436
## D.npnct10.log                             13.50159822
## D.npnct06.log                             12.78249366
## color.fctrWhite                           12.53594991
## storage.fctr32                            12.35709363
## storage.fctr16                            12.07759735
## color.fctrBlack                           11.76072506
## D.T.condition                             10.66373063
## D.npnct15.log                             10.28852505
## D.T.condit                                 9.43637821
## D.T.use                                    8.57146597
## color.fctrGold                             8.52393782
## prdline.my.fctrUnknown                     7.70696581
## D.T.scratch                                7.47732189
## prdline.my.fctriPadmini                    6.43549260
## cellular.fctrUnknown                       6.15062015
## .rnorm                                     5.97562544
## `condition.fctrManufacturer refurbished`   5.56050310
## storage.fctr64                             5.26115741
## D.T.excellent                              5.15100222
## D.npnct08.log                              5.09152488
## D.ndgts.log                                4.68472975
## cellular.fctr1                             3.87495114
## D.T.screen                                 3.65167582
## storage.fctrUnknown                        3.61617086
## `color.fctrSpace Gray`                     3.36548180
## D.T.good                                   3.26595470
## D.T.used                                   2.99561023
## D.npnct14.log                              2.79495547
## D.npnct12.log                              2.63421316
## D.npnct03.log                              2.47158594
## D.P.mini                                   2.39946802
## D.ratio.sum.TfIdf.nwrds                    2.31207504
## D.T.this                                   2.23894157
## D.nwrds.unq.log                            1.65802150
## D.P.air                                    1.46490212
## D.npnct11.log                              0.89620875
## D.T.veri                                   0.45522172
## D.npnct09.log                              0.08005932
## D.npnct28.log                              0.00000000
##                                          Low.cor.X.glm.importance
## startprice                                           100.00000000
## biddable                                              79.80954040
## prdline.my.fctriPadAir                                55.42496678
## `prdline.my.fctriPad 3+`                              34.41293234
## D.T.ipad                                              27.10904281
## `condition.fctrSeller refurbished`                    25.79969773
## `condition.fctrFor parts or not working`              25.58447029
## `prdline.my.fctriPad 2`                               24.34979172
## `prdline.my.fctriPadmini 2+`                          24.26241952
## condition.fctrNew                                     23.53196621
## `condition.fctrNew other (see details)`               22.28763325
## D.T.like                                              21.90219439
## D.T.box                                               20.96837457
## D.npnct05.log                                         19.72484707
## D.npnct01.log                                         17.90620310
## D.T.work                                              17.39888212
## D.T.great                                             14.23487436
## D.npnct10.log                                         13.50159822
## D.npnct06.log                                         12.78249366
## color.fctrWhite                                       12.53594991
## storage.fctr32                                        12.35709363
## storage.fctr16                                        12.07759735
## color.fctrBlack                                       11.76072506
## D.T.condition                                         10.66373063
## D.npnct15.log                                         10.28852505
## D.T.condit                                             9.43637821
## D.T.use                                                8.57146597
## color.fctrGold                                         8.52393782
## prdline.my.fctrUnknown                                 7.70696581
## D.T.scratch                                            7.47732189
## prdline.my.fctriPadmini                                6.43549260
## cellular.fctrUnknown                                   6.15062015
## .rnorm                                                 5.97562544
## `condition.fctrManufacturer refurbished`               5.56050310
## storage.fctr64                                         5.26115741
## D.T.excellent                                          5.15100222
## D.npnct08.log                                          5.09152488
## D.ndgts.log                                            4.68472975
## cellular.fctr1                                         3.87495114
## D.T.screen                                             3.65167582
## storage.fctrUnknown                                    3.61617086
## `color.fctrSpace Gray`                                 3.36548180
## D.T.good                                               3.26595470
## D.T.used                                               2.99561023
## D.npnct14.log                                          2.79495547
## D.npnct12.log                                          2.63421316
## D.npnct03.log                                          2.47158594
## D.P.mini                                               2.39946802
## D.ratio.sum.TfIdf.nwrds                                2.31207504
## D.T.this                                               2.23894157
## D.nwrds.unq.log                                        1.65802150
## D.P.air                                                1.46490212
## D.npnct11.log                                          0.89620875
## D.T.veri                                               0.45522172
## D.npnct09.log                                          0.08005932
## D.npnct28.log                                          0.00000000
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

```
## Warning in glb_analytics_diag_plots(obs_df = glb_OOBobs_df, mdl_id =
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 40
```

![](ebayipads_category_files/figure-html/fit.models_2-8.png) ![](ebayipads_category_files/figure-html/fit.models_2-9.png) ![](ebayipads_category_files/figure-html/fit.models_2-10.png) ![](ebayipads_category_files/figure-html/fit.models_2-11.png) ![](ebayipads_category_files/figure-html/fit.models_2-12.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Low.cor.X.glm.prob
## 20      10020         N                         3.870037e-01
## 1397    11397         N                         1.887071e-05
## 285     10285         Y                         9.214141e-01
## 132     10132         N                         5.294811e-01
## 739     10739         N                         5.302088e-01
## 801     10801         N                         5.366599e-01
## 107     10107         N                         5.462835e-01
## 1579    11579         N                         5.513668e-01
## 77      10077         N                         5.567902e-01
## 526     10526         N                         5.919302e-01
## 941     10941         N                         7.478061e-01
## 1622    11622         N                         7.638926e-01
## 184     10184         N                         7.652351e-01
## 182     10182         N                         7.873171e-01
## 841     10841         N                         8.145620e-01
##      sold.fctr.predict.Low.cor.X.glm
## 20                                 N
## 1397                               N
## 285                                Y
## 132                                Y
## 739                                Y
## 801                                Y
## 107                                Y
## 1579                               Y
## 77                                 Y
## 526                                Y
## 941                                Y
## 1622                               Y
## 184                                Y
## 182                                Y
## 841                                Y
##      sold.fctr.predict.Low.cor.X.glm.accurate
## 20                                       TRUE
## 1397                                     TRUE
## 285                                      TRUE
## 132                                     FALSE
## 739                                     FALSE
## 801                                     FALSE
## 107                                     FALSE
## 1579                                    FALSE
## 77                                      FALSE
## 526                                     FALSE
## 941                                     FALSE
## 1622                                    FALSE
## 184                                     FALSE
## 182                                     FALSE
## 841                                     FALSE
##      sold.fctr.predict.Low.cor.X.glm.error .label
## 20                              0.00000000  10020
## 1397                            0.00000000  11397
## 285                             0.00000000  10285
## 132                             0.02948109  10132
## 739                             0.03020880  10739
## 801                             0.03665987  10801
## 107                             0.04628354  10107
## 1579                            0.05136680  11579
## 77                              0.05679022  10077
## 526                             0.09193021  10526
## 941                             0.24780611  10941
## 1622                            0.26389257  11622
## 184                             0.26523512  10184
## 182                             0.28731706  10182
## 841                             0.31456199  10841
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Low.cor.X.glm.prob
## 1804    11804         Y                          0.004530965
## 1225    11225         Y                          0.008760822
## 1524    11524         Y                          0.012606233
## 1817    11817         Y                          0.018292558
## 1514    11514         Y                          0.020492820
## 803     10803         Y                          0.022469720
##      sold.fctr.predict.Low.cor.X.glm
## 1804                               N
## 1225                               N
## 1524                               N
## 1817                               N
## 1514                               N
## 803                                N
##      sold.fctr.predict.Low.cor.X.glm.accurate
## 1804                                    FALSE
## 1225                                    FALSE
## 1524                                    FALSE
## 1817                                    FALSE
## 1514                                    FALSE
## 803                                     FALSE
##      sold.fctr.predict.Low.cor.X.glm.error
## 1804                            -0.4954690
## 1225                            -0.4912392
## 1524                            -0.4873938
## 1817                            -0.4817074
## 1514                            -0.4795072
## 803                             -0.4775303
##      UniqueID sold.fctr sold.fctr.predict.Low.cor.X.glm.prob
## 1125    11125         Y                           0.06508736
## 1727    11727         Y                           0.31946082
## 870     10870         Y                           0.36009803
## 1753    11753         Y                           0.48700398
## 1748    11748         N                           0.63836949
## 1591    11591         N                           0.79695395
##      sold.fctr.predict.Low.cor.X.glm
## 1125                               N
## 1727                               N
## 870                                N
## 1753                               N
## 1748                               Y
## 1591                               Y
##      sold.fctr.predict.Low.cor.X.glm.accurate
## 1125                                    FALSE
## 1727                                    FALSE
## 870                                     FALSE
## 1753                                    FALSE
## 1748                                    FALSE
## 1591                                    FALSE
##      sold.fctr.predict.Low.cor.X.glm.error
## 1125                           -0.43491264
## 1727                           -0.18053918
## 870                            -0.13990197
## 1753                           -0.01299602
## 1748                            0.13836949
## 1591                            0.29695395
##      UniqueID sold.fctr sold.fctr.predict.Low.cor.X.glm.prob
## 488     10488         N                            0.8879928
## 594     10594         N                            0.9276160
## 1156    11156         N                            0.9356438
## 1391    11391         N                            0.9673954
## 1471    11471         N                            0.9737615
## 1506    11506         N                            0.9847317
##      sold.fctr.predict.Low.cor.X.glm
## 488                                Y
## 594                                Y
## 1156                               Y
## 1391                               Y
## 1471                               Y
## 1506                               Y
##      sold.fctr.predict.Low.cor.X.glm.accurate
## 488                                     FALSE
## 594                                     FALSE
## 1156                                    FALSE
## 1391                                    FALSE
## 1471                                    FALSE
## 1506                                    FALSE
##      sold.fctr.predict.Low.cor.X.glm.error
## 488                              0.3879928
## 594                              0.4276160
## 1156                             0.4356438
## 1391                             0.4673954
## 1471                             0.4737615
## 1506                             0.4847317
```

![](ebayipads_category_files/figure-html/fit.models_2-13.png) 

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
##         label step_major step_minor     bgn     end elapsed
## 12 fit.models          7          2 117.912 132.792   14.88
## 13 fit.models          7          3 132.793      NA      NA
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
## [1] "sold.fctr.predict.Low.cor.X.glm.prob"    
## [2] "sold.fctr.predict.Low.cor.X.glm"         
## [3] "sold.fctr.predict.Low.cor.X.glm.accurate"
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

![](ebayipads_category_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn   end elapsed
## 13        fit.models          7          3 132.793 137.8   5.007
## 14 fit.data.training          8          0 137.800    NA      NA
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
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: biddable, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.npnct08.log, D.T.condit, color.fctr, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_category_files/figure-html/fit.data.training_0-1.png) ![](ebayipads_category_files/figure-html/fit.data.training_0-2.png) ![](ebayipads_category_files/figure-html/fit.data.training_0-3.png) ![](ebayipads_category_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.6705  -0.7078  -0.2375   0.6394   3.4525  
## 
## Coefficients:
##                                            Estimate Std. Error z value
## (Intercept)                               1.291e+00  4.947e-01   2.611
## biddable                                  1.572e+00  1.369e-01  11.487
## D.npnct15.log                             1.493e+00  6.237e-01   2.393
## D.T.screen                                2.672e-01  4.763e-01   0.561
## D.npnct03.log                            -3.551e-01  9.724e-01  -0.365
## D.T.used                                  1.488e-01  4.465e-01   0.333
## D.T.excellent                             2.820e-01  2.509e-01   1.124
## D.T.great                                -7.881e-03  4.018e-01  -0.020
## D.ratio.sum.TfIdf.nwrds                  -9.174e-04  1.108e-01  -0.008
## D.T.good                                  1.357e-01  4.482e-01   0.303
## D.npnct01.log                             3.751e-01  4.105e-01   0.914
## D.T.veri                                 -1.851e-01  4.495e-01  -0.412
## D.T.work                                 -1.990e-02  4.640e-01  -0.043
## D.T.scratch                               3.700e-01  4.955e-01   0.747
## D.P.air                                  -1.155e+00  8.211e-01  -1.406
## D.T.use                                   5.846e-01  4.679e-01   1.249
## storage.fctr16                           -1.029e+00  4.139e-01  -2.487
## storage.fctr32                           -1.028e+00  4.263e-01  -2.410
## storage.fctr64                           -4.420e-01  4.135e-01  -1.069
## storage.fctrUnknown                      -5.420e-01  5.298e-01  -1.023
## D.P.mini                                  1.210e-01  5.823e-01   0.208
## D.T.condition                             1.920e-01  4.658e-01   0.412
## D.npnct11.log                             6.896e-02  2.301e-01   0.300
## .rnorm                                   -4.228e-02  6.351e-02  -0.666
## D.T.ipad                                 -1.095e+00  6.185e-01  -1.770
## D.npnct10.log                             9.986e-01  1.256e+00   0.795
## D.npnct08.log                            -3.774e-01  4.666e-01  -0.809
## D.T.condit                               -1.787e-01  3.151e-01  -0.567
## color.fctrBlack                           7.616e-02  1.719e-01   0.443
## color.fctrGold                            1.123e-01  4.200e-01   0.267
## `color.fctrSpace Gray`                   -3.442e-03  2.324e-01  -0.015
## color.fctrWhite                          -7.702e-02  1.711e-01  -0.450
## D.T.this                                  5.209e-01  6.621e-01   0.787
## D.npnct06.log                            -7.039e-01  5.816e-01  -1.210
## D.T.box                                  -7.553e-01  5.465e-01  -1.382
## D.npnct28.log                            -2.797e+00  6.530e+02  -0.004
## D.T.like                                 -5.570e-01  3.317e-01  -1.679
## D.npnct12.log                            -2.043e-01  4.303e-01  -0.475
## D.npnct09.log                            -8.259e+00  4.889e+02  -0.017
## D.nwrds.unq.log                          -1.151e-01  1.019e-01  -1.129
## D.ndgts.log                               5.554e-01  2.825e-01   1.966
## cellular.fctr1                            1.743e-01  1.531e-01   1.138
## cellular.fctrUnknown                     -6.449e-01  3.242e-01  -1.989
## D.npnct14.log                            -9.562e-01  5.517e-01  -1.733
## `prdline.my.fctriPad 2`                   5.023e-01  2.308e-01   2.177
## `prdline.my.fctriPad 3+`                  9.865e-01  2.489e-01   3.964
## prdline.my.fctriPadAir                    2.382e+00  3.242e-01   7.347
## prdline.my.fctriPadmini                   5.667e-01  2.503e-01   2.264
## `prdline.my.fctriPadmini 2+`              1.620e+00  3.157e-01   5.130
## prdline.my.fctrUnknown                    4.323e-01  3.219e-01   1.343
## D.npnct05.log                            -3.229e+00  1.084e+00  -2.979
## `condition.fctrFor parts or not working` -5.496e-01  2.220e-01  -2.475
## `condition.fctrManufacturer refurbished`  3.925e-01  4.521e-01   0.868
## condition.fctrNew                         3.470e-01  2.261e-01   1.535
## `condition.fctrNew other (see details)`   6.164e-01  3.288e-01   1.875
## `condition.fctrSeller refurbished`       -4.756e-01  2.927e-01  -1.625
## startprice                               -1.063e-02  8.586e-04 -12.380
##                                          Pr(>|z|)    
## (Intercept)                               0.00904 ** 
## biddable                                  < 2e-16 ***
## D.npnct15.log                             0.01670 *  
## D.T.screen                                0.57482    
## D.npnct03.log                             0.71497    
## D.T.used                                  0.73898    
## D.T.excellent                             0.26098    
## D.T.great                                 0.98435    
## D.ratio.sum.TfIdf.nwrds                   0.99339    
## D.T.good                                  0.76212    
## D.npnct01.log                             0.36077    
## D.T.veri                                  0.68047    
## D.T.work                                  0.96578    
## D.T.scratch                               0.45525    
## D.P.air                                   0.15965    
## D.T.use                                   0.21151    
## storage.fctr16                            0.01288 *  
## storage.fctr32                            0.01593 *  
## storage.fctr64                            0.28519    
## storage.fctrUnknown                       0.30629    
## D.P.mini                                  0.83541    
## D.T.condition                             0.68013    
## D.npnct11.log                             0.76442    
## .rnorm                                    0.50560    
## D.T.ipad                                  0.07680 .  
## D.npnct10.log                             0.42664    
## D.npnct08.log                             0.41864    
## D.T.condit                                0.57058    
## color.fctrBlack                           0.65768    
## color.fctrGold                            0.78914    
## `color.fctrSpace Gray`                    0.98818    
## color.fctrWhite                           0.65265    
## D.T.this                                  0.43144    
## D.npnct06.log                             0.22618    
## D.T.box                                   0.16694    
## D.npnct28.log                             0.99658    
## D.T.like                                  0.09315 .  
## D.npnct12.log                             0.63493    
## D.npnct09.log                             0.98652    
## D.nwrds.unq.log                           0.25886    
## D.ndgts.log                               0.04933 *  
## cellular.fctr1                            0.25492    
## cellular.fctrUnknown                      0.04666 *  
## D.npnct14.log                             0.08310 .  
## `prdline.my.fctriPad 2`                   0.02950 *  
## `prdline.my.fctriPad 3+`                 7.38e-05 ***
## prdline.my.fctriPadAir                   2.02e-13 ***
## prdline.my.fctriPadmini                   0.02358 *  
## `prdline.my.fctriPadmini 2+`             2.89e-07 ***
## prdline.my.fctrUnknown                    0.17927    
## D.npnct05.log                             0.00289 ** 
## `condition.fctrFor parts or not working`  0.01332 *  
## `condition.fctrManufacturer refurbished`  0.38533    
## condition.fctrNew                         0.12488    
## `condition.fctrNew other (see details)`   0.06086 .  
## `condition.fctrSeller refurbished`        0.10420    
## startprice                                < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2569.2  on 1860  degrees of freedom
## Residual deviance: 1668.9  on 1804  degrees of freedom
## AIC: 1782.9
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_category_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6321205
## 2        0.1 0.6822277
## 3        0.2 0.7266021
## 4        0.3 0.7606660
## 5        0.4 0.7724687
## 6        0.5 0.7772455
## 7        0.6 0.7802747
## 8        0.7 0.7627346
## 9        0.8 0.6490166
## 10       0.9 0.3660377
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Final.glm.N sold.fctr.predict.Final.glm.Y
## 1         N                           884                           117
## 2         Y                           235                           625
##          Prediction
## Reference   N   Y
##         N 884 117
##         Y 235 625
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.108544e-01   6.158120e-01   7.923083e-01   8.284151e-01   5.378829e-01 
## AccuracyPValue  McnemarPValue 
##  1.212601e-134   4.485543e-10
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](ebayipads_category_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method
## 1 Final.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 biddable, D.npnct15.log, D.T.screen, D.npnct03.log, D.T.used, D.T.excellent, D.T.great, D.ratio.sum.TfIdf.nwrds, D.T.good, D.npnct01.log, D.T.veri, D.T.work, D.T.scratch, D.P.air, D.T.use, storage.fctr, D.P.mini, D.T.condition, D.npnct11.log, .rnorm, D.T.ipad, D.npnct10.log, D.npnct08.log, D.T.condit, color.fctr, D.T.this, D.npnct06.log, D.T.box, D.npnct28.log, D.T.like, D.npnct12.log, D.npnct09.log, D.nwrds.unq.log, D.ndgts.log, cellular.fctr, D.npnct14.log, prdline.my.fctr, D.npnct05.log, condition.fctr, startprice
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.414                 0.206
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8705202                    0.6       0.7802747        0.7856051
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.7923083             0.8284151     0.5669569    1782.935
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02828054      0.05685036
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 137.800 148.226  10.426
## 15 fit.data.training          8          1 148.227      NA      NA
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
##                                          Low.cor.X.glm.importance
## startprice                                           100.00000000
## biddable                                              79.80954040
## prdline.my.fctriPadAir                                55.42496678
## `prdline.my.fctriPadmini 2+`                          24.26241952
## `prdline.my.fctriPad 3+`                              34.41293234
## D.npnct05.log                                         19.72484707
## storage.fctr16                                        12.07759735
## `condition.fctrFor parts or not working`              25.58447029
## storage.fctr32                                        12.35709363
## D.npnct15.log                                         10.28852505
## prdline.my.fctriPadmini                                6.43549260
## `prdline.my.fctriPad 2`                               24.34979172
## cellular.fctrUnknown                                   6.15062015
## D.ndgts.log                                            4.68472975
## `condition.fctrNew other (see details)`               22.28763325
## D.T.ipad                                              27.10904281
## D.npnct14.log                                          2.79495547
## D.T.like                                              21.90219439
## `condition.fctrSeller refurbished`                    25.79969773
## condition.fctrNew                                     23.53196621
## D.P.air                                                1.46490212
## D.T.box                                               20.96837457
## prdline.my.fctrUnknown                                 7.70696581
## D.T.use                                                8.57146597
## D.npnct06.log                                         12.78249366
## cellular.fctr1                                         3.87495114
## D.nwrds.unq.log                                        1.65802150
## D.T.excellent                                          5.15100222
## storage.fctr64                                         5.26115741
## storage.fctrUnknown                                    3.61617086
## D.npnct01.log                                         17.90620310
## `condition.fctrManufacturer refurbished`               5.56050310
## D.npnct08.log                                          5.09152488
## D.npnct10.log                                         13.50159822
## D.T.this                                               2.23894157
## D.T.scratch                                            7.47732189
## .rnorm                                                 5.97562544
## D.T.condit                                             9.43637821
## D.T.screen                                             3.65167582
## D.npnct12.log                                          2.63421316
## color.fctrWhite                                       12.53594991
## color.fctrBlack                                       11.76072506
## D.T.condition                                         10.66373063
## D.T.veri                                               0.45522172
## D.npnct03.log                                          2.47158594
## D.T.used                                               2.99561023
## D.T.good                                               3.26595470
## D.npnct11.log                                          0.89620875
## color.fctrGold                                         8.52393782
## D.P.mini                                               2.39946802
## D.T.work                                              17.39888212
## D.T.great                                             14.23487436
## D.npnct09.log                                          0.08005932
## `color.fctrSpace Gray`                                 3.36548180
## D.ratio.sum.TfIdf.nwrds                                2.31207504
## D.npnct28.log                                          0.00000000
##                                            importance Final.glm.importance
## startprice                               100.00000000         100.00000000
## biddable                                  92.78499517          92.78499517
## prdline.my.fctriPadAir                    59.33503555          59.33503555
## `prdline.my.fctriPadmini 2+`              41.42020502          41.42020502
## `prdline.my.fctriPad 3+`                  31.99474718          31.99474718
## D.npnct05.log                             24.03811999          24.03811999
## storage.fctr16                            20.06274548          20.06274548
## `condition.fctrFor parts or not working`  19.96619994          19.96619994
## storage.fctr32                            19.44289687          19.44289687
## D.npnct15.log                             19.30452996          19.30452996
## prdline.my.fctriPadmini                   18.25906118          18.25906118
## `prdline.my.fctriPad 2`                   17.55420128          17.55420128
## cellular.fctrUnknown                      16.04000563          16.04000563
## D.ndgts.log                               15.84959364          15.84959364
## `condition.fctrNew other (see details)`   15.11244264          15.11244264
## D.T.ipad                                  14.26417190          14.26417190
## D.npnct14.log                             13.96872724          13.96872724
## D.T.like                                  13.53255836          13.53255836
## `condition.fctrSeller refurbished`        13.09480748          13.09480748
## condition.fctrNew                         12.36565999          12.36565999
## D.P.air                                   11.32842947          11.32842947
## D.T.box                                   11.13331355          11.13331355
## prdline.my.fctrUnknown                    10.81747321          10.81747321
## D.T.use                                   10.06122872          10.06122872
## D.npnct06.log                              9.74481508           9.74481508
## cellular.fctr1                             9.16481949           9.16481949
## D.nwrds.unq.log                            9.08897640           9.08897640
## D.T.excellent                              9.04841032           9.04841032
## storage.fctr64                             8.60119865           8.60119865
## storage.fctrUnknown                        8.23203772           8.23203772
## D.npnct01.log                              7.35000460           7.35000460
## `condition.fctrManufacturer refurbished`   6.98017454           6.98017454
## D.npnct08.log                              6.50063952           6.50063952
## D.npnct10.log                              6.38897075           6.38897075
## D.T.this                                   6.32252065           6.32252065
## D.T.scratch                                5.99902416           5.99902416
## .rnorm                                     5.34450319           5.34450319
## D.T.condit                                 4.54855527           4.54855527
## D.T.screen                                 4.49820867           4.49820867
## D.npnct12.log                              3.80200343           3.80200343
## color.fctrWhite                            3.60225678           3.60225678
## color.fctrBlack                            3.54598131           3.54598131
## D.T.condition                              3.29684307           3.29684307
## D.T.veri                                   3.29305690           3.29305690
## D.npnct03.log                              2.91623519           2.91623519
## D.T.used                                   2.65778811           2.65778811
## D.T.good                                   2.41127163           2.41127163
## D.npnct11.log                              2.38693903           2.38693903
## color.fctrGold                             2.12628000           2.12628000
## D.P.mini                                   1.64421907           1.64421907
## D.T.work                                   0.31200594           0.31200594
## D.T.great                                  0.12386561           0.12386561
## D.npnct09.log                              0.10188299           0.10188299
## `color.fctrSpace Gray`                     0.08507511           0.08507511
## D.ratio.sum.TfIdf.nwrds                    0.03229310           0.03229310
## D.npnct28.log                              0.00000000           0.00000000
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 40
```

![](ebayipads_category_files/figure-html/fit.data.training_0-7.png) ![](ebayipads_category_files/figure-html/fit.data.training_0-8.png) ![](ebayipads_category_files/figure-html/fit.data.training_0-9.png) ![](ebayipads_category_files/figure-html/fit.data.training_0-10.png) ![](ebayipads_category_files/figure-html/fit.data.training_0-11.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## 17      10017         Y                     2.302457e-01
## 3       10003         Y                     2.733927e-01
## 15      10015         Y                     2.955193e-01
## 11      10011         Y                     3.354448e-01
## 14      10014         Y                     3.918734e-01
## 1       10001         N                     4.183292e-01
## 2       10002         Y                     9.401699e-01
## 91      10091         Y                     8.659129e-01
## 1397    11397         N                     5.538279e-05
## 4       10004         N                     5.029617e-01
## 135     10135         N                     5.420975e-01
## 193     10193         N                     6.356740e-01
## 127     10127         N                     6.881027e-01
## 120     10120         N                     8.204149e-01
## 182     10182         N                     8.280371e-01
## 103     10103         N                     8.321406e-01
##      sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## 17                             N                                FALSE
## 3                              N                                FALSE
## 15                             N                                FALSE
## 11                             N                                FALSE
## 14                             N                                FALSE
## 1                              N                                 TRUE
## 2                              Y                                 TRUE
## 91                             Y                                 TRUE
## 1397                           N                                 TRUE
## 4                              Y                                FALSE
## 135                            Y                                FALSE
## 193                            Y                                FALSE
## 127                            Y                                FALSE
## 120                            Y                                FALSE
## 182                            Y                                FALSE
## 103                            Y                                FALSE
##      sold.fctr.predict.Final.glm.error .label
## 17                        -0.269754342  10017
## 3                         -0.226607317  10003
## 15                        -0.204480718  10015
## 11                        -0.164555181  10011
## 14                        -0.108126577  10014
## 1                          0.000000000  10001
## 2                          0.000000000  10002
## 91                         0.000000000  10091
## 1397                       0.000000000  11397
## 4                          0.002961668  10004
## 135                        0.042097540  10135
## 193                        0.135673963  10193
## 127                        0.188102680  10127
## 120                        0.320414950  10120
## 182                        0.328037068  10182
## 103                        0.332140564  10103
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## 1705    11705         Y                      0.002580320
## 1359    11359         Y                      0.004205812
## 1804    11804         Y                      0.017442105
## 986     10986         Y                      0.017820161
## 1817    11817         Y                      0.023853167
## 935     10935         Y                      0.029870231
##      sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## 1705                           N                                FALSE
## 1359                           N                                FALSE
## 1804                           N                                FALSE
## 986                            N                                FALSE
## 1817                           N                                FALSE
## 935                            N                                FALSE
##      sold.fctr.predict.Final.glm.error
## 1705                        -0.4974197
## 1359                        -0.4957942
## 1804                        -0.4825579
## 986                         -0.4821798
## 1817                        -0.4761468
## 935                         -0.4701298
##      UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## 1350    11350         Y                        0.1060184
## 1720    11720         Y                        0.1700618
## 1535    11535         Y                        0.3347442
## 1746    11746         N                        0.6543472
## 1644    11644         N                        0.7232232
## 1674    11674         N                        0.7867364
##      sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## 1350                           N                                FALSE
## 1720                           N                                FALSE
## 1535                           N                                FALSE
## 1746                           Y                                FALSE
## 1644                           Y                                FALSE
## 1674                           Y                                FALSE
##      sold.fctr.predict.Final.glm.error
## 1350                        -0.3939816
## 1720                        -0.3299382
## 1535                        -0.1652558
## 1746                         0.1543472
## 1644                         0.2232232
## 1674                         0.2867364
##      UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## 491     10491         N                        0.9121523
## 594     10594         N                        0.9200628
## 1391    11391         N                        0.9223501
## 1250    11250         N                        0.9470651
## 1471    11471         N                        0.9540800
## 1506    11506         N                        0.9717232
##      sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## 491                            Y                                FALSE
## 594                            Y                                FALSE
## 1391                           Y                                FALSE
## 1250                           Y                                FALSE
## 1471                           Y                                FALSE
## 1506                           Y                                FALSE
##      sold.fctr.predict.Final.glm.error
## 491                          0.4121523
## 594                          0.4200628
## 1391                         0.4223501
## 1250                         0.4470651
## 1471                         0.4540800
## 1506                         0.4717232
```

![](ebayipads_category_files/figure-html/fit.data.training_0-12.png) 

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
## [1] "sold.fctr.predict.Final.glm.prob" "sold.fctr.predict.Final.glm"
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

![](ebayipads_category_files/figure-html/fit.data.training_0-13.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn    end elapsed
## 15 fit.data.training          8          1 148.227 153.58   5.354
## 16  predict.data.new          9          0 153.581     NA      NA
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
## Warning in glb_analytics_diag_plots(obs_df = glb_newobs_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 40
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

![](ebayipads_category_files/figure-html/predict.data.new-1.png) 

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

![](ebayipads_category_files/figure-html/predict.data.new-2.png) 

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

![](ebayipads_category_files/figure-html/predict.data.new-3.png) 

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

![](ebayipads_category_files/figure-html/predict.data.new-4.png) 

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

![](ebayipads_category_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## 1862    11862      <NA>                     8.890554e-02
## 1865    11865      <NA>                     7.806373e-01
## 1891    11891      <NA>                     9.479641e-01
## 2625    12625      <NA>                     1.815592e-05
##      sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## 1862                           N                                   NA
## 1865                           Y                                   NA
## 1891                           Y                                   NA
## 2625                           N                                   NA
##      sold.fctr.predict.Final.glm.error .label
## 1862                                 0  11862
## 1865                                 0  11865
## 1891                                 0  11891
## 2625                                 0  12625
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## NA         NA      <NA>                               NA
## NA.1       NA      <NA>                               NA
## NA.2       NA      <NA>                               NA
## NA.3       NA      <NA>                               NA
## NA.4       NA      <NA>                               NA
## NA.5       NA      <NA>                               NA
##      sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## NA                          <NA>                                   NA
## NA.1                        <NA>                                   NA
## NA.2                        <NA>                                   NA
## NA.3                        <NA>                                   NA
## NA.4                        <NA>                                   NA
## NA.5                        <NA>                                   NA
##      sold.fctr.predict.Final.glm.error
## NA                                  NA
## NA.1                                NA
## NA.2                                NA
## NA.3                                NA
## NA.4                                NA
## NA.5                                NA
##        UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## NA.3         NA      <NA>                               NA
## NA.35        NA      <NA>                               NA
## NA.160       NA      <NA>                               NA
## NA.212       NA      <NA>                               NA
## NA.593       NA      <NA>                               NA
## NA.702       NA      <NA>                               NA
##        sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## NA.3                          <NA>                                   NA
## NA.35                         <NA>                                   NA
## NA.160                        <NA>                                   NA
## NA.212                        <NA>                                   NA
## NA.593                        <NA>                                   NA
## NA.702                        <NA>                                   NA
##        sold.fctr.predict.Final.glm.error
## NA.3                                  NA
## NA.35                                 NA
## NA.160                                NA
## NA.212                                NA
## NA.593                                NA
## NA.702                                NA
##        UniqueID sold.fctr sold.fctr.predict.Final.glm.prob
## NA.792       NA      <NA>                               NA
## NA.793       NA      <NA>                               NA
## NA.794       NA      <NA>                               NA
## NA.795       NA      <NA>                               NA
## NA.796       NA      <NA>                               NA
## NA.797       NA      <NA>                               NA
##        sold.fctr.predict.Final.glm sold.fctr.predict.Final.glm.accurate
## NA.792                        <NA>                                   NA
## NA.793                        <NA>                                   NA
## NA.794                        <NA>                                   NA
## NA.795                        <NA>                                   NA
## NA.796                        <NA>                                   NA
## NA.797                        <NA>                                   NA
##        sold.fctr.predict.Final.glm.error
## NA.792                                NA
## NA.793                                NA
## NA.794                                NA
## NA.795                                NA
## NA.796                                NA
## NA.797                                NA
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

![](ebayipads_category_files/figure-html/predict.data.new-6.png) 

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
## [1] "glb_sel_mdl_id: Low.cor.X.glm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.glm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 976  63
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 8              Low.cor.X.glm        0.7909605   0.8382507     0.5784228
## 6              Max.cor.Y.glm        0.7909605   0.8351149     0.5771095
## 9                  All.X.glm        0.7887006   0.8358417     0.5709041
## 7    Interact.High.cor.Y.glm        0.7864407   0.8351868     0.5676667
## 5            Max.cor.Y.rpart        0.7864407   0.7844353     0.5697529
## 11      All.X.no.rnorm.rpart        0.7864407   0.7844353     0.5697529
## 10            All.X.bayesglm        0.7819209   0.8374956     0.5603441
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7774011   0.8257253     0.5503014
## 12         All.X.no.rnorm.rf        0.7728814   0.8498233     0.5457239
## 1          MFO.myMFO_classfr        0.5378531   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5378531   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4621469   0.5214656     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 8     960.7058                    0.5
## 6     959.0160                    0.5
## 9     975.9950                    0.6
## 7     967.0557                    0.5
## 5           NA                    0.7
## 11          NA                    0.7
## 10    979.8148                    0.5
## 4           NA                    0.5
## 12          NA                    0.4
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
## [1] "Low.cor.X.glm OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 391  85
##         Y 100 309
##    prdline.my .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 2      iPad 2    171    154      0.1929825      0.1932203
## 4     iPadAir    152    137      0.1716792      0.1717514
## 7     Unknown     97     87      0.1090226      0.1096045
## 3     iPad 3+    136    123      0.1541353      0.1536723
## 5    iPadmini    126    114      0.1428571      0.1423729
## 6 iPadmini 2+    104     94      0.1177945      0.1175141
## 1      iPad 1     99     89      0.1115288      0.1118644
##   accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 2                 35               136        0.7953216
## 4                 35               117        0.7697368
## 7                 30                67        0.6907216
## 3                 23               113        0.8308824
## 5                 23               103        0.8174603
## 6                 20                84        0.8076923
## 1                 19                80        0.8080808
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
##                                          Low.cor.X.glm.importance
## startprice                                           100.00000000
## biddable                                              79.80954040
## prdline.my.fctriPadAir                                55.42496678
## `prdline.my.fctriPad 3+`                              34.41293234
## D.T.ipad                                              27.10904281
## `condition.fctrSeller refurbished`                    25.79969773
## `condition.fctrFor parts or not working`              25.58447029
## `prdline.my.fctriPad 2`                               24.34979172
## `prdline.my.fctriPadmini 2+`                          24.26241952
## condition.fctrNew                                     23.53196621
## `condition.fctrNew other (see details)`               22.28763325
## D.T.like                                              21.90219439
## D.T.box                                               20.96837457
## D.npnct05.log                                         19.72484707
## D.npnct01.log                                         17.90620310
## D.T.work                                              17.39888212
## D.T.great                                             14.23487436
## D.npnct10.log                                         13.50159822
## D.npnct06.log                                         12.78249366
## color.fctrWhite                                       12.53594991
## storage.fctr32                                        12.35709363
## storage.fctr16                                        12.07759735
## color.fctrBlack                                       11.76072506
## D.T.condition                                         10.66373063
## D.npnct15.log                                         10.28852505
## D.T.condit                                             9.43637821
## D.T.use                                                8.57146597
## color.fctrGold                                         8.52393782
## prdline.my.fctrUnknown                                 7.70696581
## D.T.scratch                                            7.47732189
## prdline.my.fctriPadmini                                6.43549260
## cellular.fctrUnknown                                   6.15062015
## .rnorm                                                 5.97562544
## `condition.fctrManufacturer refurbished`               5.56050310
## storage.fctr64                                         5.26115741
## D.T.excellent                                          5.15100222
## D.npnct08.log                                          5.09152488
## D.ndgts.log                                            4.68472975
## cellular.fctr1                                         3.87495114
## D.T.screen                                             3.65167582
## storage.fctrUnknown                                    3.61617086
## `color.fctrSpace Gray`                                 3.36548180
## D.T.good                                               3.26595470
## D.T.used                                               2.99561023
## D.npnct14.log                                          2.79495547
## D.npnct12.log                                          2.63421316
## D.npnct03.log                                          2.47158594
## D.P.mini                                               2.39946802
## D.ratio.sum.TfIdf.nwrds                                2.31207504
## D.T.this                                               2.23894157
## D.nwrds.unq.log                                        1.65802150
## D.P.air                                                1.46490212
## D.npnct11.log                                          0.89620875
## D.T.veri                                               0.45522172
## D.npnct09.log                                          0.08005932
## D.npnct28.log                                          0.00000000
##                                            importance Final.glm.importance
## startprice                               100.00000000         100.00000000
## biddable                                  92.78499517          92.78499517
## prdline.my.fctriPadAir                    59.33503555          59.33503555
## `prdline.my.fctriPad 3+`                  31.99474718          31.99474718
## D.T.ipad                                  14.26417190          14.26417190
## `condition.fctrSeller refurbished`        13.09480748          13.09480748
## `condition.fctrFor parts or not working`  19.96619994          19.96619994
## `prdline.my.fctriPad 2`                   17.55420128          17.55420128
## `prdline.my.fctriPadmini 2+`              41.42020502          41.42020502
## condition.fctrNew                         12.36565999          12.36565999
## `condition.fctrNew other (see details)`   15.11244264          15.11244264
## D.T.like                                  13.53255836          13.53255836
## D.T.box                                   11.13331355          11.13331355
## D.npnct05.log                             24.03811999          24.03811999
## D.npnct01.log                              7.35000460           7.35000460
## D.T.work                                   0.31200594           0.31200594
## D.T.great                                  0.12386561           0.12386561
## D.npnct10.log                              6.38897075           6.38897075
## D.npnct06.log                              9.74481508           9.74481508
## color.fctrWhite                            3.60225678           3.60225678
## storage.fctr32                            19.44289687          19.44289687
## storage.fctr16                            20.06274548          20.06274548
## color.fctrBlack                            3.54598131           3.54598131
## D.T.condition                              3.29684307           3.29684307
## D.npnct15.log                             19.30452996          19.30452996
## D.T.condit                                 4.54855527           4.54855527
## D.T.use                                   10.06122872          10.06122872
## color.fctrGold                             2.12628000           2.12628000
## prdline.my.fctrUnknown                    10.81747321          10.81747321
## D.T.scratch                                5.99902416           5.99902416
## prdline.my.fctriPadmini                   18.25906118          18.25906118
## cellular.fctrUnknown                      16.04000563          16.04000563
## .rnorm                                     5.34450319           5.34450319
## `condition.fctrManufacturer refurbished`   6.98017454           6.98017454
## storage.fctr64                             8.60119865           8.60119865
## D.T.excellent                              9.04841032           9.04841032
## D.npnct08.log                              6.50063952           6.50063952
## D.ndgts.log                               15.84959364          15.84959364
## cellular.fctr1                             9.16481949           9.16481949
## D.T.screen                                 4.49820867           4.49820867
## storage.fctrUnknown                        8.23203772           8.23203772
## `color.fctrSpace Gray`                     0.08507511           0.08507511
## D.T.good                                   2.41127163           2.41127163
## D.T.used                                   2.65778811           2.65778811
## D.npnct14.log                             13.96872724          13.96872724
## D.npnct12.log                              3.80200343           3.80200343
## D.npnct03.log                              2.91623519           2.91623519
## D.P.mini                                   1.64421907           1.64421907
## D.ratio.sum.TfIdf.nwrds                    0.03229310           0.03229310
## D.T.this                                   6.32252065           6.32252065
## D.nwrds.unq.log                            9.08897640           9.08897640
## D.P.air                                   11.32842947          11.32842947
## D.npnct11.log                              2.38693903           2.38693903
## D.T.veri                                   3.29305690           3.29305690
## D.npnct09.log                              0.10188299           0.10188299
## D.npnct28.log                              0.00000000           0.00000000
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

![](ebayipads_category_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 440 358
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
## 16     predict.data.new          9          0 153.581 159.907   6.326
## 17 display.session.info         10          0 159.908      NA      NA
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
## 11              fit.models          7          1  80.247 117.912  37.665
## 10              fit.models          7          0  50.031  80.247  30.216
## 5         extract.features          3          0  21.663  46.337  24.674
## 12              fit.models          7          2 117.912 132.792  14.880
## 2             inspect.data          2          0   9.198  20.508  11.310
## 14       fit.data.training          8          0 137.800 148.226  10.426
## 16        predict.data.new          9          0 153.581 159.907   6.326
## 15       fit.data.training          8          1 148.227 153.580   5.354
## 13              fit.models          7          3 132.793 137.800   5.007
## 8          select.features          5          0  47.468  48.979   1.511
## 3               scrub.data          2          1  20.508  21.565   1.057
## 9  partition.data.training          6          0  48.980  50.031   1.051
## 6             cluster.data          4          0  46.337  47.324   0.987
## 1              import.data          1          0   8.788   9.198   0.410
## 7      manage.missing.data          4          1  47.324  47.467   0.143
## 4           transform.data          2          2  21.566  21.662   0.096
##    duration
## 11   37.665
## 10   30.216
## 5    24.674
## 12   14.880
## 2    11.310
## 14   10.426
## 16    6.326
## 15    5.353
## 13    5.007
## 8     1.511
## 3     1.057
## 9     1.051
## 6     0.987
## 1     0.410
## 7     0.143
## 4     0.096
## [1] "Total Elapsed Time: 159.907 secs"
```

![](ebayipads_category_files/figure-html/display.session.info-1.png) 

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
