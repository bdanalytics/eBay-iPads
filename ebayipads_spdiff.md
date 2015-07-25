# eBay:iPads:: sold classification:: spdiff
bdanalytics  

**  **    
**Date: (Tue) Jul 28, 2015**    

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

dataclns: -> 
    All.X.no.rnorm.rf: Leaderboard: 0.82211
        newobs_tbl=[N=485, Y=313]; submit_filename=dataclns_Final_rf_submit
        OOB_conf_mtrx=[YN=104, NY=75]=179; max.Accuracy.OOB=0.7977;
            opt.prob.threshold.OOB=0.5
            startprice.log=100.00; biddable=65.85; prdline.my=7.74; D.sum.TfIdf=; 
            D.T.use=2.01; D.T.condition=1.87; D.T.veri=1.62; D.T.ipad=; D.T.like=;
    Low.cor.X.glm: Leaderboard: 0.79264
        newobs_tbl=[N=460, Y=338]; submit_filename=dataclns_Low_cor_X_glm_submit
        OOB_conf_mtrx=[YN=113, NY=74]=187; max.Accuracy.OOB=0.7977;
            opt.prob.threshold.OOB=0.5 -> different from prev run of 0.6
            biddable=100.00; startprice.log=91.85; prdline.my=38.34; D.sum.TfIdf=; 
            D.T.ipad=29.92; D.T.box=27.76; D.T.work=25.79; D.T.use=; D.T.condition=;

txtterms: -> top_n = c(10)
    Low.cor.X.glm: Leaderboard: 0.81448
        newobs_tbl=[N=442, Y=356]; submit_filename=txtterms_Final_glm_submit
        OOB_conf_mtrx=[YN=113, NY=69]=182; max.Accuracy.OOB=0.7943;
            opt.prob.threshold.OOB=0.5
            biddable=100.00; startprice.log=90.11; prdline.my=37.65; D.sum.TfIdf=; 
            D.T.ipad=28.67; D.T.work=24.90; D.T.great=21.44; 
# [1] "D.T.condit"    "D.T.condition" "D.T.good"      "D.T.ipad"      "D.T.new"      
# [6] "D.T.scratch"   "D.T.screen"    "D.T.this"      "D.T.use"       "D.T.work"     
            
    All.X.glm: Leaderboard: 0.81016
        newobs_tbl=[N=445, Y=353]; submit_filename=txtterms_Final_glm_submit
        OOB_conf_mtrx=[YN=108, NY=72]=180; max.Accuracy.OOB=0.7966;
            opt.prob.threshold.OOB=0.5
            biddable=100.00; startprice.log=88.24; prdline.my=33.81; D.sum.TfIdf=; 
            D.T.scratch=25.51; D.T.use=18.97; D.T.good=16.37; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.great"   "D.T.excel"   "D.T.work"    "D.T.ipad"            

    Max.cor.Y.rpart: Leaderboard: 0.79258
        newobs_tbl=[N=439, Y=359]; submit_filename=txtterms_Final_rpart_submit
        OOB_conf_mtrx=[YN=105, NY=76]=181; max.Accuracy.OOB=0.7954802;
            opt.prob.threshold.OOB=0.5
            startprice.log=100; biddable=; prdline.my=; D.sum.TfIdf=; 
            D.T.scratch=; D.T.use=; D.T.good=; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"

    All.X.no.rnorm.rf: Leaderboard: 0.80929
        newobs_tbl=[N=545, Y=253]; submit_filename=txtterms_Final_rf_submit
        OOB_conf_mtrx=[YN=108, NY=61]=169; max.Accuracy.OOB=0.8090395
            opt.prob.threshold.OOB=0.5
            startprice.log=100.00; biddable=78.82; idseq.my=63.43; prdline.my=45.57;
            D.T.use=2.76; D.T.condit=2.35; D.T.scratch=2.00; D.T.good=; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"

txtclstr:
    All.X.no.rnorm.rf: Leaderboard: 0.79363 -> 0.79573
        newobs_tbl=[N=537, Y=261]; submit_filename=txtclstr_Final_rf_submit
        OOB_conf_mtrx=[YN=104, NY=61]=165; max.Accuracy.OOB=0.8135593
            opt.prob.threshold.OOB=0.5
            startprice.log=100.00; biddable=79.99; idseq.my=64.94; 
                prdline.my=4.14; prdline.my.clusterid=1.15; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"            
 
dupobs:
    All.X.no.rnorm.rf: Leaderboard: 0.79295
        newobs_tbl=[N=541, Y=257]; submit_filename=dupobs_Final_rf_submit
        OOB_conf_mtrx=[YN=114, NY=65]=179; max.Accuracy.OOB=0.7977401
            opt.prob.threshold.OOB=0.5
            startprice.log=100.00; biddable=94.49; idseq.my=67.40; 
                prdline.my=4.48; prdline.my.clusterid=1.99; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"            
 
    All.X.no.rnorm.rf: Leaderboard: 0.79652
        newobs_tbl=[N=523, Y=275]; submit_filename=dupobs_Final_rf_submit
        OOB_conf_mtrx=[YN=114, NY=65]=179; max.Accuracy.OOB=0.7977401
            opt.prob.threshold.OOB=0.5
            startprice.log=100.00; biddable=94.24; idseq.my=67.92; 
                prdline.my=4.33; prdline.my.clusterid=2.17; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"
 
csmmdl:
   All.X.no.rnorm.rf: Leaderboard: 0.79396
        newobs_tbl=[N=525, Y=273]; submit_filename=csmmdl_Final_rf_submit
        OOB_conf_mtrx=[YN=111, NY=66]=177; max.Accuracy.OOB=0.8000000
            opt.prob.threshold.OOB=0.5
            startprice.log=100.00; biddable=90.30; idseq.my=67.06; 
                prdline.my=4.40; cellular.fctr=3.57; prdline.my.clusterid=2.08; 

   All.Interact.X.no.rnorm.rf: Leaderboard: 0.77867
        newobs_tbl=[N=564, Y=234]; submit_filename=csmmdl_Final_rf_submit
        OOB_conf_mtrx=[YN=120, NY=53]=173; max.Accuracy.OOB=0.8045198
            opt.prob.threshold.OOB=0.5
            biddable=100.00; startprice.log=93.99; idseq.my=57.30; 
                prdline.my=9.09; cellular.fctr=3.30; prdline.my.clusterid=2.35; 

   All.Interact.X.no.rnorm.rf: Leaderboard: 0.77152
        newobs_tbl=[N=539, Y=259]; submit_filename=csmmdl_Final_rf_submit
        OOB_conf_mtrx=[YN=, NY=]=; max.Accuracy.OOB=0.8011299
            opt.prob.threshold.OOB=0.5
            biddable=100.00; startprice.log=94.93; idseq.my=57.12; 
                prdline.my=9.29; cellular.fctr=3.20; prdline.my.clusterid=2.50; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"
 
        All.X.glmnet: 
            fit_RMSE=???; OOB_RMSE=115.1247; new_RMSE=115.1247; 
            prdline.my.fctr=100.00; condition.fctrNew=88.53; D.npnct09.log=84.34
                biddable=16.48; idseq.my=57.27;
spdiff:                
    All.Interact.X.no.rnorm.rf: Leaderboard: 0.78218
        newobs_tbl=[N=517, Y=281]; submit_filename=spdiff_Final_rf_submit
        OOB_conf_mtrx=[YN=121, NY=38]=159; max.Accuracy.OOB=0.8203390
            opt.prob.threshold.OOB=0.6
            biddable=100.00; startprice.diff=57.53; idseq.my=41.31; 
                prdline.my=11.43; cellular.fctr=2.36; prdline.my.clusterid=1.82; 
 
        All.X.no.rnorm.rf: 
            fit_RMSE=92.19; OOB_RMSE=130.86; new_RMSE=130.86; 
            biddable=100.00; prdline.my.fctr=61.92; idseq.my=57.77;
                condition.fctr=29.53; storage.fctr=11.22; color.fctr=6.69;
                cellular.fctr=6.11
                
    All.X.no.rnorm.rf: Leaderboard: 0.77443
        newobs_tbl=[N=606, Y=192]; submit_filename=spdiff_Final_rf_submit
        OOB_conf_mtrx=[YN=112, NY=28]=140; max.Accuracy.OOB=0.8418079
            opt.prob.threshold.OOB=0.6
            startprice.diff=100.00; biddable=96.53; idseq.my=38.10; 
                prdline.my=3.65; cellular.fctr=2.21; prdline.my.clusterid=0.91; 
 [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"    "D.T.screen" 
 [7] "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"

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
glb_out_pfx <- "spdiff_"
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
# Derive a numeric feature from id var
glb_id_var <- c("UniqueID")
glb_category_var <- c("prdline.my")
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

glb_derive_lst[["idseq.my"]] <- list(
    mapfn=function(UniqueID) { return(UniqueID - 10000) }    
    , args=c("UniqueID"))

glb_derive_lst[["prdline.my"]] <- list(
    mapfn=function(productline) { return(productline) }    
    , args=c("productline"))

glb_derive_lst[["startprice.log"]] <- list(
    mapfn=function(startprice) { return(log(startprice)) }    
    , args=c("startprice"))
# glb_derive_lst[["startprice.log.zval"]] <- list(

glb_derive_lst[["descr.my"]] <- list(
    mapfn=function(description) { mod_raw <- description;
        # Modifications for this exercise only
        # Add dictionary to stemDocument e.g. stickers stemmed to sticker ???
        mod_raw <- gsub("\\.\\.", "\\. ", mod_raw);    
        mod_raw <- gsub("(\\w)(\\*|,|-|/)(\\w)", "\\1\\2 \\3", mod_raw);
        
        mod_raw <- gsub("8\\.25", "825", mod_raw, ignore.case=TRUE);  
        mod_raw <- gsub(" 10\\.SCREEN ", " 10\\. SCREEN ", mod_raw, ignore.case=TRUE); 
        mod_raw <- gsub(" actuuly ", " actual ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" Apple care ", " Applecare ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" ans ", " and ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" bacK!wiped ", " bacK ! wiped ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" backplate", " back plate", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" bend ", " bent ", mod_raw, ignore.case=TRUE);         
        mod_raw <- gsub("Best Buy", "BestBuy", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" black\\.Device ", " black \\. Device ", mod_raw,
                        ignore.case=TRUE);        
        mod_raw <- gsub(" blocks", " blocked", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" carefully ", " careful ", mod_raw, ignore.case=TRUE);        
        
        mod_raw <- gsub(" conditon|condtion|conditions", " condition", mod_raw,
                        ignore.case=TRUE);
        mod_raw <- gsub("(CONDITION|ONLY)\\.(\\w)", "\\1\\. \\2", mod_raw,
                        ignore.case=TRUE);
        mod_raw <- gsub("(condition)(Has)", "\\1\\. \\2", mod_raw);
        
        mod_raw <- gsub(" consist ", " consistent ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" cracksNo ", " cracks No ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" DEFAULTING ", " DEFAULT ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" definitely ", " definite ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" described", " describe", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" desciption", " description", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" devices", " device", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" Digi\\.", " Digitizer\\.", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" display\\.New ", " display\\. New ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" displays", " display", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" drop ", " dropped ", mod_raw, ignore.case=TRUE);         
        mod_raw <- gsub(" effect ", " affect ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" Excellant ", " Excellent ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" excellently", " excellent", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" EUC ", " excellent used condition", mod_raw, ignore.case=TRUE);  
        mod_raw <- gsub(" feels ", " feel ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" fineiCloud ", " fine iCloud ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("^Gentle ", "Gently ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" GREAT\\.SCreen ", " GREAT\\. SCreen ", mod_raw,
                        ignore.case=TRUE);        
        mod_raw <- gsub(" Framing ", " Frame ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("iCL0UD", "iCL0UD", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub("^iPad Black 3rd generation ", "iPad 3 Black ", mod_raw,
                        ignore.case=TRUE);  
        mod_raw <- gsub(" install\\. ", " installed\\. ", mod_raw, ignore.case=TRUE);   
        mod_raw <- gsub("inivisible", "invisible", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" manuals ", " manual ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" book ", " manual ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" mars ", " marks ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" minimum", " minimal", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" MINT\\.wiped ", " MINT\\. wiped ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" NEW\\!(SCREEN|ONE) ", " NEW\\! \\1 ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" new looking$", " looks new", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" newer ", " new ", mod_raw, ignore.case=TRUE);                
        mod_raw <- gsub(" opening", " opened", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" operated", " operational", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" perfectlycord ", " perfectly cord ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" performance", " performs", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" personalized ", " personal ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" products ", " product ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" Keeped ", " Kept ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" knicks ", " nicks ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("^READiPad ", "READ iPad ", mod_raw, ignore.case=TRUE);   
        mod_raw <- gsub(" REFURB\\.", "  REFURBISHED\\.", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" reponding", " respond", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" rotation ", " rotate ", mod_raw, ignore.case=TRUE);   
        mod_raw <- gsub(" Sales ", " Sale ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" scratchs ", " scratches ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" SCREEB ", " SCREEN ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" shipped| Shipment", " ship", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("shrink wrap", "shrinkwrap", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" sides ", " side ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" skinned,", " skin,", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" spec ", " speck ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub("^somescratches ", "some scratches ", mod_raw, ignore.case=TRUE);  
        mod_raw <- gsub(" Sticker ", " Stickers ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("SWAPPA\\.COM", "SWAPPACOM", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" T- Mobile", "  TMobile", mod_raw, ignore.case=TRUE);  
        mod_raw <- gsub(" touchscreen ", " touch screen ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" use\\.Scratches ", " use\\. Scratches ", mod_raw,
                        ignore.case=TRUE);  
        mod_raw <- gsub(" verify ", " verified ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" wear\\.Device ", " wear\\. Device ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" whats ", " what's ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" WiFi\\+4G ", " WiFi \\+ 4G ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" Zaag Invisible Shield", " Zaag InvisibleShield", mod_raw,
                        ignore.case=TRUE);
                                    return(mod_raw) }
    , args=c("description"))

#     mapfn=function(startprice) { return(scale(log(startprice))) }    
#     , args=c("startprice"))
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
# tst <- "descr.my"; args_lst <- NULL; for (arg in glb_derive_lst[[tst]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; print(head(args_lst[[arg]])); print(head(drv_vals <- do.call(glb_derive_lst[[tst]]$mapfn, args_lst))); 
# print(which_ix <- which(args_lst[[arg]] == 0.75)); print(drv_vals[which_ix]); 

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- c("descr.my")   
Sys.setlocale("LC_ALL", "C") # For english
```

```
## [1] "C/C/C/C/C/en_US.UTF-8"
```

```r
glb_txt_munge_filenames_pfx <- "ebay_mytxt_"

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
glb_append_stop_words[["descr.my"]] <- c(NULL
                                        # freq = 1 
                                        ,"511","825","975"
                                        ,"2nd"
                                        ,"a1314","a1430","a1432"
    ,"abused","across","adaptor","add","antenna","anti","anyone", "area","arizona","att"
                                    ,"beginning","bidder","bonus","boot","bound","bruises"
    ,"changed","changing","chrome"
        ,"confidence","considerable","consumer","contents","control","cream"
    ,"date","daughter","decent","defender","defense","degree","depicted"
        ,"disclaimer","distressed","divider"
        ,"dlxnqat9g5wt","done","dont","durable","dust","duty"
                                ,"either","erased","ereader","essentially","every","exact"
                                        ,"faint","film","final","flickers","folding"
                                        ,"generic","genuine","glitter","goes"
                            ,"half","handstand","hdmi","high","higher","hole","hospital"
                                        ,"impact","instead","interior"
                                        ,"jack","july"
                                        ,"keeps","kind","known"
    ,"last","late","let","letters","level","lifting","limited","line","lining","liquid"
        ,"local","long","longer","looping","loss"
                    ,"mb292ll","mc707ll","mc916ll","mc991ll","md789ll","mf432ll","mgye2ll"
                    ,"middle", "mind","mixed"
                                        ,"neither","none","november"
                                        ,"occasional","online","outside"
    ,"paperwork","period","pet","played","plug","poor","portion","pouch","price","provided"
    ,"ranging"
        ,"recently","red","reflected","repeat","required","reserve","residue","result"
        ,"roughly","running"
    ,"said","seconds","seem","semi","send","serious","setup"
        ,"shell","short","size","slice","smoke","smooth"
        ,"softer","software","somewhat","soon"
        ,"sparingly","sparkiling","special","speed"
        ,"stains","standup","status","stopped","strictly","subtle","sustained","swappacom"
    ,"technical","tempered","texture","thank","therefore","think","though"
        ,"toddler","totally","touchy","tried","typical"
                                        ,"university","unknown","untouched","upgrade"
                                        ,"valid","vary","version"
                                        ,"want","website","winning","wrapped"
                                        ,"zaag","zero", "zombie"
                                            )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_filter_txt_terms <- "top" # or "sparse"
glb_top_n <- c(10)
names(glb_top_n) <- glb_txt_vars

glb_sprs_thresholds <- c(0.950) # Generates 10 terms

# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- c("productline", "description", "startprice"
                                  #, "startprice.log", "sold"
                                  ) 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- TRUE
glb_cluster.seed <- 189 # or any integer

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "glmnet", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "glmnet", "rpart", "rf") else  
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

![](ebayipads_spdiff_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 10.081  NA      NA
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
## 948  \211\333\317Used Apple Ipad 16 gig 1st generation in Great working condition and 100% functional.Very little 
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

# For Tableau
write.csv(glb_allobs_df, "data/eBayiPadAll.csv", row.names=FALSE)

#stop(here")
glb_drop_obs <- c(
                11234, #sold=0; 2 other dups(10306, 11503) are sold=1
                11844, #sold=0; 3 other dups(11721, 11738, 11812) are sold=1
                  NULL)
glb_allobs_df <- glb_allobs_df[!glb_allobs_df[, glb_id_var] %in% glb_drop_obs, ]


# Make any data corrections here
glb_allobs_df[glb_allobs_df[, glb_id_var] == 10986, "cellular"] <- "1"
glb_allobs_df[glb_allobs_df[, glb_id_var] == 10986, "carrier"] <- "T-Mobile"

# Check for duplicates by all features
require(gdata)
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```r
#print(names(glb_allobs_df))
dup_allobs_df <- glb_allobs_df[duplicated2(subset(glb_allobs_df, 
                                                  select=-c(UniqueID, sold, .src))), ]
dup_allobs_df <- orderBy(~productline+description+startprice+biddable, dup_allobs_df)
print(sprintf("Found %d duplicates by all features:", nrow(dup_allobs_df)))
```

```
## [1] "Found 304 duplicates by all features:"
```

```r
myprint_df(dup_allobs_df)
```

```
##      description biddable startprice                condition cellular
## 1711                    1       0.99 For parts or not working  Unknown
## 2608                    1       0.99 For parts or not working  Unknown
## 293                     1       5.00                     Used  Unknown
## 478                     1       5.00                     Used  Unknown
## 385                     0      15.00                     Used        0
## 390                     0      15.00                     Used        0
##      carrier   color storage productline sold UniqueID  .src
## 1711 Unknown Unknown      16     Unknown    1    11711 Train
## 2608 Unknown Unknown      16     Unknown   NA    12608  Test
## 293  Unknown   White      16     Unknown    1    10293 Train
## 478  Unknown   White      16     Unknown    1    10478 Train
## 385     None   Black      16     Unknown    0    10385 Train
## 390     None   Black      16     Unknown    0    10390 Train
##      description biddable startprice                condition cellular
## 1956                    1       0.99                     Used        0
## 828                     1     249.97 Manufacturer refurbished        1
## 3                       0     199.99                     Used        0
## 1649                    0     209.00 For parts or not working  Unknown
## 2111                    1     200.00                     Used        0
## 172                     0     269.00                     Used        0
##      carrier      color storage productline sold UniqueID  .src
## 1956    None    Unknown      16      iPad 2   NA    11956  Test
## 828  Unknown      Black      64      iPad 2    0    10828 Train
## 3       None      White      16      iPad 4    1    10003 Train
## 1649 Unknown    Unknown      16    iPad Air    0    11649 Train
## 2111    None Space Gray      64 iPad mini 2   NA    12111  Test
## 172     None    Unknown      32 iPad mini 2    0    10172 Train
##      description biddable startprice condition cellular carrier color
## 8                       0     329.99       New        0    None White
## 660                     0     329.99       New        0    None White
## 319                     0     345.00       New        0    None  Gold
## 1886                    0     345.00       New        0    None  Gold
## 1363                    0     498.88       New        1 Verizon  Gold
## 1394                    0     498.88       New        1 Verizon  Gold
##      storage productline sold UniqueID  .src
## 8         16 iPad mini 3    0    10008 Train
## 660       16 iPad mini 3    0    10660 Train
## 319       16 iPad mini 3    1    10319 Train
## 1886      16 iPad mini 3   NA    11886  Test
## 1363      16 iPad mini 3    0    11363 Train
## 1394      16 iPad mini 3    0    11394 Train
```

```r
# print(dup_allobs_df[, c(glb_id_var, glb_rsp_var_raw, 
#                          "description", "startprice", "biddable")])
# write.csv(dup_allobs_df[, c("UniqueID"), FALSE], "ebayipads_dups.csv", row.names=FALSE)

dupobs_df <- tidyr::unite(dup_allobs_df, "allfeats", -c(sold, UniqueID, .src), sep="#")
# dupobs_df <- dplyr::group_by(dupobs_df, allfeats)
# dupobs_df <- dupobs_df[, "UniqueID", FALSE]
# dupobs_df <- ungroup(dupobs_df)
# 
# dupobs_df$.rownames <- row.names(dupobs_df)
grpobs_df <- data.frame(allfeats=unique(dupobs_df[, "allfeats"]))
grpobs_df$.grpid <- row.names(grpobs_df)
dupobs_df <- merge(dupobs_df, grpobs_df)

# dupobs_tbl <- table(dupobs_df$.grpid)
# print(max(dupobs_tbl))
# print(dupobs_tbl[which.max(dupobs_tbl)])
# print(dupobs_df[dupobs_df$.grpid == names(dupobs_tbl[which.max(dupobs_tbl)]), ])
# print(dupobs_df[dupobs_df$.grpid == 106, ])
# for (grpid in c(9, 17, 31, 36, 53))
#     print(dupobs_df[dupobs_df$.grpid == grpid, ])
dupgrps_df <- as.data.frame(table(dupobs_df$.grpid, dupobs_df$sold, useNA="ifany"))
names(dupgrps_df)[c(1,2)] <- c(".grpid", "sold")
dupgrps_df$.grpid <- as.numeric(as.character(dupgrps_df$.grpid))
dupgrps_df <- tidyr::spread(dupgrps_df, sold, Freq)
names(dupgrps_df)[-1] <- paste("sold", names(dupgrps_df)[-1], sep=".")
dupgrps_df$.freq <- sapply(1:nrow(dupgrps_df), function(row) sum(dupgrps_df[row, -1]))
myprint_df(orderBy(~-.freq, dupgrps_df))
```

```
##     .grpid sold.0 sold.1 sold.NA .freq
## 40      40      0      6       3     9
## 106    106      0      4       1     5
## 9        9      0      1       3     4
## 17      17      0      3       1     4
## 36      36      0      3       1     4
## 53      53      0      2       2     4
##     .grpid sold.0 sold.1 sold.NA .freq
## 10      10      0      2       0     2
## 42      42      0      1       1     2
## 57      57      1      0       1     2
## 66      66      1      0       1     2
## 91      91      0      1       1     2
## 101    101      0      1       1     2
##     .grpid sold.0 sold.1 sold.NA .freq
## 130    130      1      0       1     2
## 131    131      1      1       0     2
## 132    132      0      1       1     2
## 133    133      2      0       0     2
## 134    134      0      1       1     2
## 135    135      2      0       0     2
```

```r
print("sold Conflicts:")
```

```
## [1] "sold Conflicts:"
```

```r
print(subset(dupgrps_df, (sold.0 > 0) & (sold.1 > 0)))
```

```
##     .grpid sold.0 sold.1 sold.NA .freq
## 4        4      1      1       0     2
## 22      22      1      1       0     2
## 23      23      1      1       0     2
## 74      74      1      1       0     2
## 83      83      1      1       0     2
## 84      84      1      1       0     2
## 95      95      1      1       0     2
## 102    102      1      1       0     2
## 109    109      1      1       0     2
## 111    111      1      1       0     2
## 122    122      1      1       0     2
## 131    131      1      1       0     2
```

```r
#dupobs_df[dupobs_df$.grpid == 4, ]
if (nrow(subset(dupgrps_df, (sold.0 > 0) & (sold.1 > 0) & (sold.0 != sold.1))) > 0)
    stop("Duplicate conflicts are resolvable")

print("Test & Train Groups:")
```

```
## [1] "Test & Train Groups:"
```

```r
print(subset(dupgrps_df, (sold.NA > 0)))
```

```
##     .grpid sold.0 sold.1 sold.NA .freq
## 1        1      0      1       1     2
## 5        5      1      0       1     2
## 7        7      0      0       2     2
## 8        8      1      0       1     2
## 9        9      0      1       3     4
## 12      12      0      0       2     2
## 14      14      0      1       1     2
## 15      15      0      0       2     2
## 17      17      0      3       1     4
## 18      18      0      2       1     3
## 19      19      0      2       1     3
## 24      24      0      2       1     3
## 26      26      1      0       1     2
## 28      28      1      0       1     2
## 30      30      0      1       1     2
## 32      32      0      0       2     2
## 33      33      0      1       1     2
## 35      35      0      2       1     3
## 36      36      0      3       1     4
## 37      37      0      0       2     2
## 38      38      0      1       1     2
## 40      40      0      6       3     9
## 41      41      0      0       2     2
## 42      42      0      1       1     2
## 43      43      0      1       1     2
## 44      44      0      2       1     3
## 47      47      0      1       1     2
## 48      48      0      0       2     2
## 49      49      0      1       2     3
## 51      51      0      1       1     2
## 53      53      0      2       2     4
## 54      54      0      1       1     2
## 55      55      1      0       2     3
## 56      56      1      0       1     2
## 57      57      1      0       1     2
## 58      58      0      0       2     2
## 59      59      1      0       1     2
## 60      60      1      0       1     2
## 63      63      0      1       1     2
## 66      66      1      0       1     2
## 67      67      1      0       1     2
## 68      68      0      0       2     2
## 69      69      1      0       1     2
## 73      73      0      1       1     2
## 76      76      0      2       1     3
## 86      86      0      0       2     2
## 87      87      1      0       1     2
## 89      89      1      0       1     2
## 90      90      0      0       2     2
## 91      91      0      1       1     2
## 93      93      0      1       1     2
## 94      94      1      0       1     2
## 99      99      0      1       1     2
## 101    101      0      1       1     2
## 103    103      0      1       1     2
## 104    104      1      0       1     2
## 106    106      0      4       1     5
## 107    107      0      1       1     2
## 108    108      0      1       1     2
## 112    112      1      0       1     2
## 114    114      0      1       1     2
## 115    115      0      1       1     2
## 116    116      1      0       1     2
## 117    117      0      2       1     3
## 118    118      0      1       1     2
## 121    121      1      0       1     2
## 124    124      1      0       1     2
## 128    128      0      1       1     2
## 130    130      1      0       1     2
## 132    132      0      1       1     2
## 134    134      0      1       1     2
```

```r
glb_allobs_df <- merge(glb_allobs_df, dupobs_df[, c(glb_id_var, ".grpid")], 
                       by=glb_id_var, all.x=TRUE)
glb_exclude_vars_as_features <- c(".grpid", glb_exclude_vars_as_features)

spd_allobs_df <- read.csv(paste0(glb_out_pfx, "sp_predict.csv"))
if (nrow(spd_allobs_df) != nrow(glb_allobs_df))
    stop("mismatches between spd_allobs_df & glb_allobs_df")
mrg_allobs_df <- merge(glb_allobs_df, spd_allobs_df)
if (nrow(mrg_allobs_df) != nrow(glb_allobs_df))
    stop("mismatches between mrg_allobs_df & glb_allobs_df")
mrg_allobs_df$startprice.diff <- mrg_allobs_df$startprice -
                                 mrg_allobs_df$startprice.predict.
print(myplot_scatter(mrg_allobs_df, "startprice", "startprice.diff", 
                     colorcol_name = "biddable"))
```

```
## Warning in myplot_scatter(mrg_allobs_df, "startprice", "startprice.diff", :
## converting biddable to class:factor
```

![](ebayipads_spdiff_files/figure-html/import.data-1.png) 

```r
print(myplot_histogram(mrg_allobs_df, "startprice.diff", 
                     fill_col_name = "biddable"))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](ebayipads_spdiff_files/figure-html/import.data-2.png) 

```r
glb_allobs_df <- mrg_allobs_df
glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                  "startprice.log", "startprice.predict.")

#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
# Only for _sp
# print(table(glb_allobs_df$sold, glb_allobs_df$.src, useNA = "ifany"))
# print(table(glb_allobs_df$sold, glb_allobs_df$biddable, glb_allobs_df$.src, 
#             useNA = "ifany"))
# glb_allobs_df$.src <- "Test"
# glb_allobs_df[!is.na(glb_allobs_df$sold) & (glb_allobs_df$sold == 1), ".src"] <- "Train"
# print(table(glb_allobs_df$sold, glb_allobs_df$.src, useNA = "ifany"))
# print(table(glb_allobs_df$sold, glb_allobs_df$biddable, glb_allobs_df$.src, 
#             useNA = "ifany"))
###

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0 10.081 15.291    5.21
## 2 inspect.data          2          0 15.292     NA      NA
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

![](ebayipads_spdiff_files/figure-html/inspect.data-1.png) 

```
##       sold.0 sold.1 sold.NA
## Test      NA     NA     798
## Train    999    860      NA
##          sold.0    sold.1 sold.NA
## Test         NA        NA       1
## Train 0.5373857 0.4626143      NA
## [1] "numeric data missing in : "
## sold 
##  798 
## [1] "numeric data w/ 0s in : "
## biddable     sold 
##     1444      999 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1520           0           0           0           0           0 
## productline      .grpid 
##           0          NA
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
##   sold sold.fctr  .n
## 1    0         N 999
## 2    1         Y 860
## 3   NA      <NA> 798
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

![](ebayipads_spdiff_files/figure-html/inspect.data-2.png) 

```
##       sold.fctr.N sold.fctr.Y sold.fctr.NA
## Test           NA          NA          798
## Train         999         860           NA
##       sold.fctr.N sold.fctr.Y sold.fctr.NA
## Test           NA          NA            1
## Train   0.5373857   0.4626143           NA
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
## The following objects are masked from 'package:gdata':
## 
##     combine, first, last
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

![](ebayipads_spdiff_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: startprice.diff"
```

![](ebayipads_spdiff_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_spdiff_files/figure-html/inspect.data-5.png) 

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
## 2 inspect.data          2          0 15.292 20.094   4.802
## 3   scrub.data          2          1 20.094     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in : "
##      sold sold.fctr 
##       798       798 
## [1] "numeric data w/ 0s in : "
## biddable     sold 
##     1444      999 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1520           0           0           0           0           0 
## productline      .grpid 
##           0          NA
```

```r
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
                        c(glb_id_var, glb_rsp_var, glb_category_var, glb_txt_vars, cols),
                            FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(list(description.contains="mini(?!m)"), perl=TRUE)
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

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

# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "dummy" = "dummy"
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

print(table(glb_allobs_df$cellular, glb_allobs_df$carrier, useNA="ifany"))
```

```
##          
##           AT&T None Other Sprint T-Mobile Unknown Verizon
##   0          0 1593     0      0        0       0       0
##   1        288    0     4     36       28     172     196
##   Unknown    4    4     2      0        0     330       0
```

```r
# glb_allobs_df[(glb_allobs_df$cellular %in% c("Unknown")) & 
#               (glb_allobs_df$carrier %in% c("AT&T", "Other")), 
#               c(glb_id_var, glb_rsp_var_raw, "description", "carrier", "cellular")]
glb_allobs_df[(glb_allobs_df$cellular %in% c("Unknown")) & 
              (glb_allobs_df$carrier %in% c("AT&T", "Other")), 
              "cellular"] <- "1"
# glb_allobs_df[(glb_allobs_df$cellular %in% c("Unknown")) & 
#               (glb_allobs_df$carrier %in% c("None")), 
#               c(glb_id_var, glb_rsp_var_raw, "description", "carrier", "cellular")]
glb_allobs_df[(glb_allobs_df$cellular %in% c("Unknown")) & 
              (glb_allobs_df$carrier %in% c("None")), 
              "cellular"] <- "0"
print(table(glb_allobs_df$cellular, glb_allobs_df$carrier, useNA="ifany"))
```

```
##          
##           AT&T None Other Sprint T-Mobile Unknown Verizon
##   0          0 1597     0      0        0       0       0
##   1        292    0     6     36       28     172     196
##   Unknown    0    0     0      0        0     330       0
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 20.094 20.863   0.769
## 4 transform.data          2          2 20.863     NA      NA
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
## [1] "Creating new feature: idseq.my..."
## [1] "Creating new feature: prdline.my..."
## [1] "Creating new feature: startprice.log..."
## [1] "Creating new feature: descr.my..."
```

```r
#stop(here")
#hex_vctr <- c("\n", "\211", "\235", "\317", "\333")
hex_regex <- paste0(c("\n", "\211", "\235", "\317", "\333"), collapse="|")
for (obs_id in c(10178, 10948, 11514, 11904, 12157, 12210, 12659)) {
#     tmp_str <- unlist(strsplit(glb_allobs_df[row_pos, "descr.my"], ""))
#     glb_allobs_df[row_pos, "descr.my"] <- paste0(tmp_str[!tmp_str %in% hex_vctr],
#                                                          collapse="")
    row_pos <- which(glb_allobs_df$UniqueID == obs_id)
    glb_allobs_df[row_pos, "descr.my"] <- 
        gsub(hex_regex, " ", glb_allobs_df[row_pos, "descr.my"])
}
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 4   transform.data          2          2 20.863 21.414   0.551
## 5 extract.features          3          0 21.414     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 21.422  NA      NA
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
##                                 label step_major step_minor    bgn   end
## 1                extract.features_bgn          1          0 21.422 21.44
## 2 extract.features_factorize.str.vars          2          0 21.440    NA
##   elapsed
## 1   0.018
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##   description     condition      cellular       carrier         color 
## "description"   "condition"    "cellular"     "carrier"       "color" 
##       storage   productline          .src        .grpid    prdline.my 
##     "storage" "productline"        ".src"      ".grpid"  "prdline.my" 
##      descr.my 
##    "descr.my"
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
        #print(rex_str <- glb_txt_map_df[3, "rex_str"])
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
    
    get_DTM_terms <- function(DTM) {
        TfIdf_mtrx <- as.matrix(DTM)
        rownames(TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        TfIdf_vctr <- colSums(TfIdf_mtrx)
        names(TfIdf_vctr) <- dimnames(DTM)[[2]]
        TfIdf_df <- as.data.frame(TfIdf_vctr)
        names(TfIdf_df) <- "TfIdf"
        TfIdf_df$term <- rownames(TfIdf_df)
        TfIdf_df$freq <- colSums(TfIdf_mtrx != 0)
        TfIdf_df$pos <- 1:nrow(TfIdf_df)
        return(TfIdf_df <- orderBy(~ -TfIdf, TfIdf_df))
    }
    get_corpus_terms <- function(txt_corpus) {
        TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                        control=list(weighting=weightTfIdf))
        return(TfIdf_df <- get_DTM_terms(TfIdf_DTM))
    }
    
#stop(here")    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
    # for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        #tolower Not needed as of version 0.6.2 ?
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument, lazy=FALSE)
        txt_corpus <- tm_map(txt_corpus, content_transformer(tolower), lazy=FALSE) #nuppr
        # removePunctuation does not replace with whitespace. Use a custom transformer ???
        txt_corpus <- tm_map(txt_corpus, removePunctuation, lazy=TRUE) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   
        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english")), lazy=TRUE) #nstopwrds
        #print("StoppedWords:"); stopped_words_TfIdf_df <- inspect_terms(txt_corpus)
        #stopped_words_TfIdf_df[grepl("cond", stopped_words_TfIdf_df$term, ignore.case=TRUE), ]
        #txt_X_mtrx <- as.matrix(DocumentTermMatrix(txt_corpus, control=list(weighting=weightTfIdf)))
        #which(txt_X_mtrx[, 211] > 0)
        #glb_allobs_df[which(txt_X_mtrx[, 211] > 0), glb_txt_vars]        
        #txt_X_mtrx[2159, txt_X_mtrx[2159, ] > 0]
        
        # txt_corpus <- tm_map(txt_corpus, stemDocument, "english", lazy=TRUE) #Done below
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_TfIdf_df <- inspect_terms(txt_corpus)
        #stemmed_words_TfIdf_df[grepl("cond", stemmed_words_TfIdf_df$term, ignore.case=TRUE), ]
        #stm_X_mtrx <- as.matrix(DocumentTermMatrix(txt_corpus, control=list(weighting=weightTfIdf)))
        #glb_allobs_df[which((stm_X_mtrx[, 180] > 0) | (stm_X_mtrx[, 181] > 0)), glb_txt_vars]
        #glb_allobs_df[which((stm_X_mtrx[, 181] > 0)), glb_txt_vars]

        # glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
    
#stop(here")        
    glb_post_stop_words_terms_df_lst <- list(); 
    glb_post_stop_words_TfIdf_mtrx_lst <- list();     
    glb_post_stem_words_terms_df_lst <- list(); 
    glb_post_stem_words_TfIdf_mtrx_lst <- list();     
    for (txt_var in glb_txt_vars) {
        print(sprintf("    Top_n stop TfIDf terms for %s:", txt_var))
        # This impacts stemming probably due to lazy parameter
        print(myprint_df(full_TfIdf_df <- get_corpus_terms(glb_corpus_lst[[txt_var]]), 
                   glb_top_n[[txt_var]]))
        glb_post_stop_words_terms_df_lst[[txt_var]] <- full_TfIdf_df
        TfIdf_stop_mtrx <- as.matrix(DocumentTermMatrix(glb_corpus_lst[[txt_var]], 
                                        control=list(weighting=weightTfIdf)))
        rownames(TfIdf_stop_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        glb_post_stop_words_TfIdf_mtrx_lst[[txt_var]] <- TfIdf_stop_mtrx
        
        tmp_allobs_df <- glb_allobs_df[, c(glb_id_var, glb_rsp_var)]
        tmp_allobs_df$terms.n.post.stop <- rowSums(TfIdf_stop_mtrx > 0)
        tmp_allobs_df$terms.n.post.stop.log <- log(1 + tmp_allobs_df$terms.n.post.stop)
        tmp_allobs_df$TfIdf.sum.post.stop <- rowSums(TfIdf_stop_mtrx)        
        
        print(sprintf("    Top_n stem TfIDf terms for %s:", txt_var))        
        glb_corpus_lst[[txt_var]] <- tm_map(glb_corpus_lst[[txt_var]], stemDocument,
                                            "english", lazy=TRUE) #Features ???
        print(myprint_df(full_TfIdf_df <- get_corpus_terms(glb_corpus_lst[[txt_var]]), 
                   glb_top_n[[txt_var]]))
        glb_post_stem_words_terms_df_lst[[txt_var]] <- full_TfIdf_df        
        TfIdf_stem_mtrx <- as.matrix(DocumentTermMatrix(glb_corpus_lst[[txt_var]], 
                                        control=list(weighting=weightTfIdf)))
        rownames(TfIdf_stem_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        glb_post_stem_words_TfIdf_mtrx_lst[[txt_var]] <- TfIdf_stem_mtrx
        
        tmp_allobs_df$terms.n.post.stem <- rowSums(TfIdf_stem_mtrx > 0)
        tmp_allobs_df$terms.n.post.stem.log <- log(1 + tmp_allobs_df$terms.n.post.stem)
        tmp_allobs_df$TfIdf.sum.post.stem <- rowSums(TfIdf_stem_mtrx)
        
        tmp_allobs_df$terms.n.stem.stop.Ratio <- 
            1.0 * tmp_allobs_df$terms.n.post.stem / tmp_allobs_df$terms.n.post.stop
        tmp_allobs_df[is.nan(tmp_allobs_df$terms.n.stem.stop.Ratio), 
                      "terms.n.stem.stop.Ratio"] <- 1.0                
        tmp_allobs_df$TfIdf.sum.stem.stop.Ratio <- 
            1.0 * tmp_allobs_df$TfIdf.sum.post.stem / tmp_allobs_df$TfIdf.sum.post.stop
        tmp_allobs_df[is.nan(tmp_allobs_df$TfIdf.sum.stem.stop.Ratio), 
                      "TfIdf.sum.stem.stop.Ratio"] <- 1.0                
        
        tmp_trnobs_df <- tmp_allobs_df[!is.na(tmp_allobs_df[, glb_rsp_var]), ]
        print(cor(as.matrix(tmp_trnobs_df[, -c(1, 2)]), 
                  as.numeric(tmp_trnobs_df[, glb_rsp_var])))
        
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        tmp_allobs_df <- tmp_allobs_df[, -c(1, 2)]
        names(tmp_allobs_df) <- paste(paste0(txt_var_pfx, "."), names(tmp_allobs_df),
                                      sep="")
        glb_allobs_df <- cbind(glb_allobs_df, tmp_allobs_df)
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
            paste(txt_var_pfx, c("terms.n.post.stop", "terms.n.post.stem")))
    }
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

#stop(here")    
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

    require(reshape2)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_df <- get_DTM_terms(full_TfIdf_DTM)
        full_TfIdf_df <- full_TfIdf_df[, c(2, 1, 3, 4)]
        col_names <- names(full_TfIdf_df)
        col_names[2:length(col_names)] <- 
            paste(col_names[2:length(col_names)], ".full", sep="")
        names(full_TfIdf_df) <- col_names
#         full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
#         rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
#         full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
#         names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
#         full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
#         names(full_TfIdf_df) <- "TfIdf.full"
#         full_TfIdf_df$term <- rownames(full_TfIdf_df)
#         full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
#         full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_df <- get_DTM_terms(sprs_TfIdf_DTM)
        sprs_TfIdf_df <- sprs_TfIdf_df[, c(2, 1, 3, 4)]
        col_names <- names(sprs_TfIdf_df)
        col_names[2:length(col_names)] <- 
            paste(col_names[2:length(col_names)], ".sprs", sep="")
        names(sprs_TfIdf_df) <- col_names
#         sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
#         names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
#         sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
#         names(sprs_TfIdf_df) <- "TfIdf.sprs"
#         sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
#         sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
#         sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
#         glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
#             plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
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
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df            
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        
        txt_full_X_df <- as.data.frame(as.matrix(glb_full_DTM_lst[[txt_var]]))
        terms_full_df <- get_DTM_terms(glb_full_DTM_lst[[txt_var]])        
        colnames(txt_full_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_full_X_df)), sep="")
        rownames(txt_full_X_df) <- rownames(glb_allobs_df) # warning otherwise
        
        if (glb_filter_txt_terms == "sparse") {
            txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
            colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                        make.names(colnames(txt_X_df)), sep="")
            rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
        } else if (glb_filter_txt_terms == "top") {
            txt_X_df <- txt_full_X_df[, terms_full_df$pos[1:glb_top_n[[txt_var]]], FALSE]
        } else stop("glb_filter_txt_terms should be one of c('sparse', 'top') vs. '",
                    glb_filter_txt_terms, "'")    
        
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
## 2 extract.features_factorize.str.vars          2          0 21.440 21.701
## 3       extract.features_process.text          3          0 21.701     NA
##   elapsed
## 2   0.261
## 3      NA
## [1] "Building glb_txt_lst..."
## [1] "running gsub for 10 (of 178): #\\bCentral African Republic\\b#..."
## [1] "running gsub for 20 (of 178): #\\bAlejandro G\\. I&ntilde;&aacute;rritu#..."
## [1] "running gsub for 30 (of 178): #\\bC\\.A\\.A\\.#..."
## [1] "running gsub for 40 (of 178): #\\bCV\\.#..."
## [1] "running gsub for 50 (of 178): #\\bE\\.P\\.A\\.#..."
## [1] "running gsub for 60 (of 178): #\\bG\\.I\\. Joe#..."
## [1] "running gsub for 70 (of 178): #\\bISIS\\.#..."
## [1] "running gsub for 80 (of 178): #\\bJ\\.K\\. Simmons#..."
## [1] "running gsub for 90 (of 178): #\\bM\\. Henri Pol#..."
## [1] "running gsub for 100 (of 178): #\\bN\\.Y\\.S\\.E\\.#..."
## [1] "running gsub for 110 (of 178): #\\bR\\.B\\.S\\.#..."
## [1] "running gsub for 120 (of 178): #\\bSteven A\\. Cohen#..."
## [1] "running gsub for 130 (of 178): #\\bV\\.A\\.#..."
## [1] "running gsub for 140 (of 178): #\\bWall Street#..."
## [1] "running gsub for 150 (of 178): #\\bSaint( |-)((Laurent|Lucia)\\b)+#..."
## [1] "running gsub for 160 (of 178): #\\bSouth( |\\\\.)(America|American|Africa|African|Carolina|Dakota|Korea|Korean|Sudan)\\b#..."
## [1] "running gsub for 170 (of 178): #(\\w)-a-year#..."
## [1] "Remaining OK in descr.my:"
##   pattern .n
## 1      OK  6
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
## [1] "ROKEN SCREEN"
## [1] pattern .n     
## <0 rows> (or 0-length row.names)
## [1] pattern .n     
## <0 rows> (or 0-length row.names)
## [1] "Remaining Acronyms in descr.my:"
## [1] pattern .n     
## <0 rows> (or 0-length row.names)
##        pattern .n
## 1  CONDITION.   8
## 2        ONLY.  6
## 3         GB.   4
## 4       BOX.    2
## 5     CORNER.   2
## 6         ESN.  2
## 7       GOOD.   2
## 8     ICLOUD.   2
## 9       IPADS.  2
## 10    LOCKED.   2
## 11     LOCKS.   2
## 12      ONLY.   2
## 13 SCRATCHES.   2
## 14    TEARS.    2
## 15       USE.   2
## [1] "Remaining #\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+# terms in descr.my: "
##          pattern .n
## 2       New Open  3
## 4  New Condition  2
## 7  New Digitizer  1
## 8     New Opened  1
## 9    New Scratch  1
## 10    New Screen  1
## [1] "    consider cleaning if relevant to problem domain; geography name; .n > 1"
## [1] "Remaining #\\b(N|S|E|W|C)( |\\.)(\\w)+# terms in descr.my: "
##   pattern .n
## 1 C Stock  3
## 2  W blue  1
## [1] "Remaining #\\b(North|South|East|West|Central)( |\\.)(\\w)+# terms in descr.my: "
##                                                    label step_major
## 3                          extract.features_process.text          3
## 4 extract.features_process.text_reporting_compound_terms          3
##   step_minor    bgn    end elapsed
## 3          0 21.701 23.366   1.665
## 4          1 23.366     NA      NA
## [1] "Remaining compound terms in descr.my: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn   end elapsed
## 4          1 23.366 23.37   0.004
## 5          0 23.371    NA      NA
## [1] "Building glb_corpus_lst..."
## [1] "    Top_n stop TfIDf terms for descr.my:"
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
## [1] "Rows: 729; Cols: 4"
##              TfIdf      term freq pos
## condition 208.1205 condition  498 149
## new       125.5866       new  156 433
## used      123.4473      used  240 695
## good      120.9670      good  197 289
## scratches 114.0567 scratches  254 570
## screen    106.6170    screen  210 572
##              TfIdf     term freq pos
## scratch  30.068378  scratch   25 568
## days      6.562908     days    4 184
## taken     5.878172    taken    6 649
## outer     5.557938    outer    5 458
## lot       3.602633      lot    2 386
## greeting  2.075117 greeting    2 294
##                 TfIdf        term freq pos
## 975         1.1375583         975    1  16
## blemish     1.1375583     blemish    1  83
## cables      1.1375583      cables    1 106
## engravement 1.1375583 engravement    1 226
## handling    1.1375583    handling    1 304
## 79in        0.9479652        79in    1  15
##                 TfIdf        term freq pos
## 975         1.1375583         975    1  16
## blemish     1.1375583     blemish    1  83
## cables      1.1375583      cables    1 106
## engravement 1.1375583 engravement    1 226
## handling    1.1375583    handling    1 304
## 79in        0.9479652        79in    1  15
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
## [1] "    Top_n stem TfIDf terms for descr.my:"
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
## [1] "Rows: 588; Cols: 4"
##            TfIdf    term freq pos
## condit  208.1066  condit  496 122
## use     146.5910     use  291 559
## scratch 128.3886 scratch  286 457
## new     125.5866     new  156 346
## good    121.0564    good  197 233
## ipad    107.4871    ipad  232 275
##            TfIdf   term freq pos
## set    17.367419    set   14 469
## purpos  3.372064 purpos    2 418
## first   2.939748  first    2 206
## spent   2.635069  spent    2 503
## oem     2.275117    oem    1 355
## refund  1.421948 refund    1 434
##             TfIdf    term freq pos
## remot   1.2639536   remot    1 437
## ringer  1.2639536  ringer    1 450
## septemb 1.2639536 septemb    1 468
## site    1.2639536    site    1 487
## 975     1.1375583     975    1  16
## 79in    0.9479652    79in    1  15
##             TfIdf    term freq pos
## remot   1.2639536   remot    1 437
## ringer  1.2639536  ringer    1 450
## septemb 1.2639536 septemb    1 468
## site    1.2639536    site    1 487
## 975     1.1375583     975    1  16
## 79in    0.9479652    79in    1  15
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
##                                   [,1]
## terms.n.post.stop         -0.084045051
## terms.n.post.stop.log     -0.065622097
## TfIdf.sum.post.stop       -0.033351783
## terms.n.post.stem         -0.083896783
## terms.n.post.stem.log     -0.065626873
## TfIdf.sum.post.stem       -0.036095697
## terms.n.stem.stop.Ratio    0.016386012
## TfIdf.sum.stem.stop.Ratio -0.008752785
##                           label step_major step_minor    bgn   end elapsed
## 5 extract.features_build.corpus          4          0 23.371 34.25  10.879
## 6  extract.features_extract.DTM          5          0 34.250    NA      NA
## [1] "Extracting TfIDf terms for descr.my..."
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
## 6 extract.features_extract.DTM          5          0 34.250 35.596   1.347
## 7  extract.features_report.DTM          6          0 35.597     NA      NA
## [1] "Reporting TfIDf terms for descr.my..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 2657, terms: 588)>>
## Non-/sparse entries: 8269/1554047
## Sparsity           : 99%
## Maximal term length: 16
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 2657, terms: 8)>>
## Non-/sparse entries: 2069/19187
## Sparsity           : 90%
## Maximal term length: 7
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](ebayipads_spdiff_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_spdiff_files/figure-html/extract.features-2.png) ![](ebayipads_spdiff_files/figure-html/extract.features-3.png) 

```
## Warning in rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df,
## terms_TfIdf_df): object 'full_TfIdf_mtrx' not found
```

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 35.597 37.677    2.08
## 8   extract.features_bind.DTM          7          0 37.677     NA      NA
## [1] "Binding DTM for descr.my..."
##                       label step_major step_minor    bgn    end elapsed
## 8 extract.features_bind.DTM          7          0 37.677 38.115   0.439
## 9 extract.features_bind.DXM          8          0 38.116     NA      NA
## [1] "Binding DXM for descr.my..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_spdiff_files/figure-html/extract.features-4.png) 

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
## 9          iPad Air         iPad Air        0         N 102
## 10           iPad 1           iPad 1        0         N 100
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
## 9            iPad 1           iPad 1       0         N 100
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
## 3       iPad 1           iPad 1 314
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
## 5      iPad 1 314
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

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                        label step_major step_minor    bgn  end elapsed
## 9  extract.features_bind.DXM          8          0 38.116 89.1  50.984
## 10      extract.features_end          9          0 89.100   NA      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                                    label step_major
## 9                              extract.features_bind.DXM          8
## 5                          extract.features_build.corpus          4
## 7                            extract.features_report.DTM          6
## 3                          extract.features_process.text          3
## 6                           extract.features_extract.DTM          5
## 8                              extract.features_bind.DTM          7
## 2                    extract.features_factorize.str.vars          2
## 1                                   extract.features_bgn          1
## 4 extract.features_process.text_reporting_compound_terms          3
##   step_minor    bgn    end elapsed duration
## 9          0 38.116 89.100  50.984   50.984
## 5          0 23.371 34.250  10.879   10.879
## 7          0 35.597 37.677   2.080    2.080
## 3          0 21.701 23.366   1.665    1.665
## 6          0 34.250 35.596   1.347    1.346
## 8          0 37.677 38.115   0.439    0.438
## 2          0 21.440 21.701   0.261    0.261
## 1          0 21.422 21.440   0.018    0.018
## 4          1 23.366 23.370   0.004    0.004
## [1] "Total Elapsed Time: 89.1 secs"
```

![](ebayipads_spdiff_files/figure-html/extract.features-5.png) 

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

![](ebayipads_spdiff_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 21.414 90.455  69.042
## 6     cluster.data          4          0 90.456     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 90.456 91.469   1.013
## 7 manage.missing.data          4          1 91.469     NA      NA
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
##                biddable                    sold          startprice.log 
##                    1444                     999                      31 
##           cellular.fctr     D.terms.n.post.stop D.terms.n.post.stop.log 
##                    1597                    1521                    1521 
##   D.TfIdf.sum.post.stop     D.terms.n.post.stem D.terms.n.post.stem.log 
##                    1521                    1521                    1521 
##   D.TfIdf.sum.post.stem              D.T.condit                 D.T.use 
##                    1521                    2161                    2366 
##             D.T.scratch                 D.T.new                D.T.good 
##                    2371                    2501                    2460 
##                D.T.ipad              D.T.screen               D.T.great 
##                    2425                    2444                    2532 
##                D.T.work               D.T.excel             D.nwrds.log 
##                    2459                    2557                    1520 
##         D.nwrds.unq.log             D.sum.TfIdf D.ratio.sum.TfIdf.nwrds 
##                    1521                    1521                    1521 
##             D.nchrs.log             D.nuppr.log             D.ndgts.log 
##                    1520                    1522                    2426 
##           D.npnct01.log           D.npnct02.log           D.npnct03.log 
##                    2579                    2657                    2614 
##           D.npnct04.log           D.npnct05.log           D.npnct06.log 
##                    2657                    2592                    2554 
##           D.npnct07.log           D.npnct08.log           D.npnct09.log 
##                    2656                    2581                    2641 
##           D.npnct10.log           D.npnct11.log           D.npnct12.log 
##                    2648                    2301                    2537 
##           D.npnct13.log           D.npnct14.log           D.npnct15.log 
##                    1932                    2582                    2637 
##           D.npnct16.log           D.npnct17.log           D.npnct18.log 
##                    2546                    2657                    2656 
##           D.npnct19.log           D.npnct20.log           D.npnct21.log 
##                    2657                    2657                    2657 
##           D.npnct22.log           D.npnct23.log           D.npnct24.log 
##                    2657                    2657                    1520 
##           D.npnct25.log           D.npnct26.log           D.npnct27.log 
##                    2657                    2657                    2657 
##           D.npnct28.log           D.npnct29.log           D.npnct30.log 
##                    2649                    2657                    2657 
##         D.nstopwrds.log                D.P.http                D.P.mini 
##                    1663                    2657                    2623 
##                 D.P.air 
##                    2637 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1520           0           0           0           0           0 
## productline      .grpid  prdline.my    descr.my 
##           0          NA           0        1520
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
##                biddable                    sold          startprice.log 
##                    1444                     999                      31 
##           cellular.fctr     D.terms.n.post.stop D.terms.n.post.stop.log 
##                    1597                    1521                    1521 
##   D.TfIdf.sum.post.stop     D.terms.n.post.stem D.terms.n.post.stem.log 
##                    1521                    1521                    1521 
##   D.TfIdf.sum.post.stem              D.T.condit                 D.T.use 
##                    1521                    2161                    2366 
##             D.T.scratch                 D.T.new                D.T.good 
##                    2371                    2501                    2460 
##                D.T.ipad              D.T.screen               D.T.great 
##                    2425                    2444                    2532 
##                D.T.work               D.T.excel             D.nwrds.log 
##                    2459                    2557                    1520 
##         D.nwrds.unq.log             D.sum.TfIdf D.ratio.sum.TfIdf.nwrds 
##                    1521                    1521                    1521 
##             D.nchrs.log             D.nuppr.log             D.ndgts.log 
##                    1520                    1522                    2426 
##           D.npnct01.log           D.npnct02.log           D.npnct03.log 
##                    2579                    2657                    2614 
##           D.npnct04.log           D.npnct05.log           D.npnct06.log 
##                    2657                    2592                    2554 
##           D.npnct07.log           D.npnct08.log           D.npnct09.log 
##                    2656                    2581                    2641 
##           D.npnct10.log           D.npnct11.log           D.npnct12.log 
##                    2648                    2301                    2537 
##           D.npnct13.log           D.npnct14.log           D.npnct15.log 
##                    1932                    2582                    2637 
##           D.npnct16.log           D.npnct17.log           D.npnct18.log 
##                    2546                    2657                    2656 
##           D.npnct19.log           D.npnct20.log           D.npnct21.log 
##                    2657                    2657                    2657 
##           D.npnct22.log           D.npnct23.log           D.npnct24.log 
##                    2657                    2657                    1520 
##           D.npnct25.log           D.npnct26.log           D.npnct27.log 
##                    2657                    2657                    2657 
##           D.npnct28.log           D.npnct29.log           D.npnct30.log 
##                    2649                    2657                    2657 
##         D.nstopwrds.log                D.P.http                D.P.mini 
##                    1663                    2657                    2623 
##                 D.P.air 
##                    2637 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1520           0           0           0           0           0 
## productline      .grpid  prdline.my    descr.my 
##           0          NA           0        1520
```

## Step `4.1: manage missing data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)
    require(entropy)
    require(tidyr)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
    print("Clustering features: ")
    print(cluster_vars <- grep(paste0("[", 
                                toupper(paste0(substr(glb_txt_vars, 1, 1), collapse="")),
                                      "]\\.[PT]\\."), 
                               names(glb_allobs_df), value=TRUE))
    print(sprintf("glb_allobs_df Entropy: %0.4f", 
        allobs_ent <- entropy(table(glb_allobs_df[, glb_rsp_var]), method="ML")))
    
    category_df <- as.data.frame(table(glb_allobs_df[, glb_category_var], 
                                       glb_allobs_df[, glb_rsp_var]))
    names(category_df)[c(1, 2)] <- c(glb_category_var, glb_rsp_var)
    category_df <- do.call(tidyr::spread, list(category_df, glb_rsp_var, "Freq"))
    tmp.entropy <- sapply(1:nrow(category_df),
                    function(row) entropy(as.numeric(category_df[row, -1]), method="ML"))
    tmp.knt <- sapply(1:nrow(category_df),
                    function(row) sum(as.numeric(category_df[row, -1])))
    category_df$.entropy <- tmp.entropy; category_df$.knt <- tmp.knt
    print(sprintf("glb_allobs_df$%s Entropy: %0.4f (%0.4f pct)", glb_category_var,
        category_ent <- weighted.mean(category_df$.entropy, category_df$.knt),
        100 * category_ent / allobs_ent))
    print(category_df)

    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    
    for (grp in sort(unique(glb_allobs_df[, glb_category_var]))) {
        print(sprintf("Category: %s", grp))
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df[, glb_category_var] == grp, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c(glb_id_var, glb_rsp_var, glb_category_var, glb_txt_vars, cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c(glb_id_var, glb_rsp_var, glb_category_var, glb_txt_vars, cluster_vars)])
    
        set.seed(glb_cluster.seed)
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
#                 100 * # has to be > max(table(glb_allobs_df[, glb_category_var].fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df[, glb_category_var].fctr) == grp)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df[, glb_category_var]==grp,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    cluster_df <- as.data.frame(table(glb_allobs_df[, glb_category_var], 
                                      glb_allobs_df[, ".clusterid"], 
                                      glb_allobs_df[, glb_rsp_var]))
    cluster_df <- subset(cluster_df, Freq > 0)
    names(cluster_df)[c(1, 2, 3)] <- c(glb_category_var, ".clusterid", glb_rsp_var)
#     spread(unite(cluster_df, prdline.my.clusterid, prdline.my, .clusterid),
#            sold.fctr, Freq)
    cluster_df <- do.call(tidyr::unite,
                          list(cluster_df, paste0(glb_category_var, ".clusterid"),
                               grep(glb_category_var, names(cluster_df)),
                               grep(".clusterid", names(cluster_df))))
    cluster_df <- do.call(tidyr::spread, 
                          list(cluster_df, glb_rsp_var, "Freq"))
    tmp.entropy <- sapply(1:nrow(cluster_df),
                    function(row) entropy(as.numeric(cluster_df[row, -1]), method="ML"))
    tmp.knt <- sapply(1:nrow(cluster_df),
                    function(row) sum(as.numeric(cluster_df[row, -1])))
    cluster_df$.entropy <- tmp.entropy; cluster_df$.knt <- tmp.knt
    print(sprintf("glb_allobs_df$%s$.clusterid Entropy: %0.4f (%0.4f pct)",
                  glb_category_var,
        cluster_ent <- weighted.mean(cluster_df$.entropy, cluster_df$.knt),
        100 * cluster_ent / category_ent))
    print(cluster_df)

    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features[paste0(glb_category_var, ".fctr")] <-
        c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}
```

```
## Loading required package: proxy
## 
## Attaching package: 'proxy'
## 
## The following objects are masked from 'package:stats':
## 
##     as.dist, dist
## 
## The following object is masked from 'package:base':
## 
##     as.matrix
## 
## Loading required package: dynamicTreeCut
## Loading required package: entropy
## Loading required package: tidyr
```

```
## [1] "Clustering features: "
##  [1] "D.T.condit"  "D.T.use"     "D.T.scratch" "D.T.new"     "D.T.good"   
##  [6] "D.T.ipad"    "D.T.screen"  "D.T.great"   "D.T.work"    "D.T.excel"  
## [11] "D.P.http"    "D.P.mini"    "D.P.air"    
## [1] "glb_allobs_df Entropy: 0.6903"
## [1] "glb_allobs_df$prdline.my Entropy: 0.6850 (99.2280 pct)"
##    prdline.my   N   Y  .entropy .knt
## 1     Unknown 118  80 0.6746159  198
## 2      iPad 1 100 125 0.6869616  225
## 3      iPad 2 141 147 0.6929302  288
## 4     iPad 3+ 166 145 0.6908657  311
## 5     iPadAir 203 150 0.6818332  353
## 6    iPadmini 146 133 0.6920612  279
## 7 iPadmini 2+ 125  80 0.6688571  205
## [1] "Category: Unknown"
## [1] "max distance(1.0000) pair:"
##     UniqueID sold.fctr prdline.my
## 5      10005         N    Unknown
## 130    10130         Y    Unknown
##                                                                                                descr.my
## 5   Please feel free to buy. All product have been thoroughly inspected, cleaned and tested to be 100% 
## 130                                                                   New - Open Box. Charger included.
##     D.T.condit D.T.use D.T.scratch   D.T.new D.T.good D.T.ipad D.T.screen
## 5            0       0           0 0.0000000        0        0          0
## 130          0       0           0 0.8180361        0        0          0
##     D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 5           0        0         0        0        0       0
## 130         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 1029    11029         N    Unknown
## 1077    11077         N    Unknown
##                                                                                                descr.my
## 1029 A device listed in near mint used cosmetic condition with light blemishes from use. Housing &amp; 
## 1077 A device listed in near mint used cosmetic condition with light blemishes from use. Housing &amp; 
##      D.T.condit   D.T.use D.T.scratch D.T.new D.T.good D.T.ipad D.T.screen
## 1029   0.220126 0.5801286           0       0        0        0          0
## 1077   0.220126 0.5801286           0       0        0        0          0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 1029         0        0         0        0        0       0
## 1077         0        0         0        0        0       0
```

![](ebayipads_spdiff_files/figure-html/cluster.data-1.png) 

```
## [1] "Category: iPad 1"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 9     10009         Y     iPad 1
## 13    10013         Y     iPad 1
##                                                                                            descr.my
## 9                                                                                                  
## 13 GOOD CONDITION. CLEAN ICLOUD. NO LOCKS. CLEAN IMEI. This tablet has been fully tested and works 
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.ipad D.T.screen
## 9    0.000000       0           0       0 0.0000000        0          0
## 13   0.220126       0           0       0 0.3412301        0          0
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 9          0 0.000000         0        0        0       0
## 13         0 0.340566         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 471     10471         N     iPad 1
## 1202    11202         N     iPad 1
##                                                                                                     descr.my
## 471  Used Apple Ipad 64 gig 1st generation in Great working condition and 100% functional SIM card AT&amp;T 
## 1202 Used Apple Ipad 64 gig 1st generation in Great working condition and 100% functional SIM card AT&amp;T 
##      D.T.condit  D.T.use D.T.scratch D.T.new D.T.good  D.T.ipad D.T.screen
## 471   0.1862605 0.245439           0       0        0 0.2705847          0
## 1202  0.1862605 0.245439           0       0        0 0.2705847          0
##      D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 471  0.3392152 0.2881712         0        0        0       0
## 1202 0.3392152 0.2881712         0        0        0       0
```

![](ebayipads_spdiff_files/figure-html/cluster.data-2.png) 

```
## [1] "Category: iPad 2"
## [1] "max distance(1.0000) pair:"
##   UniqueID sold.fctr prdline.my
## 1    10001         N     iPad 2
## 2    10002         Y     iPad 2
##                                                                                               descr.my
## 1                                                        iPad is in 8.5+ out of 10 cosmetic condition!
## 2 Previously used, please read description. May show signs of use such as scratches to the screen and 
##   D.T.condit   D.T.use D.T.scratch D.T.new D.T.good D.T.ipad D.T.screen
## 1  0.8071287 0.0000000   0.0000000       0        0 1.172534  0.0000000
## 2  0.0000000 0.5801286   0.2923374       0        0 0.000000  0.3309884
##   D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 1         0        0         0        0        0       0
## 2         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 132     10132         N     iPad 2
## 2382    12384      <NA>     iPad 2
##                                                                                                      descr.my
## 132      Overall good condition. Some wear from use. Scratches/ scuffs/ nicks/ scrapes on unit housing back, 
## 2382 Device is in GOOD used cosmetic condition with normal scratches &amp; wear, engravement on the housing. 
##      D.T.condit   D.T.use D.T.scratch D.T.new  D.T.good D.T.ipad
## 132   0.2017822 0.2658923   0.2679759       0 0.3127942        0
## 2382  0.2421386 0.3190707   0.3215711       0 0.3753531        0
##      D.T.screen D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 132           0         0        0         0        0        0       0
## 2382          0         0        0         0        0        0       0
```

![](ebayipads_spdiff_files/figure-html/cluster.data-3.png) 

```
## [1] "Category: iPad 3+"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 3     10003         Y    iPad 3+
## 11    10011         Y    iPad 3+
##                                                                                        descr.my
## 3                                                                                              
## 11 good condition, minor wear and tear on body some light scratches on screen. functions great.
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.ipad D.T.screen
## 3    0.000000       0   0.0000000       0 0.0000000        0  0.0000000
## 11   0.220126       0   0.2923374       0 0.3412301        0  0.3309884
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 3  0.0000000        0         0        0        0       0
## 11 0.4008907        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 40      10040         N    iPad 3+
## 1602    11603         Y    iPad 3+
##                                                                                                   descr.my
## 40   Item has been professionally tested and inspected. Tests show that all features work correctly. This 
## 1602                                                                                 Work fine iCloud lock
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.ipad D.T.screen
## 40            0       0           0       0        0        0          0
## 1602          0       0           0       0        0        0          0
##      D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 40           0 0.4162473         0        0        0       0
## 1602         0 0.9365565         0        0        0       0
```

![](ebayipads_spdiff_files/figure-html/cluster.data-4.png) 

```
## [1] "Category: iPadAir"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 16    10016         N    iPadAir
## 33    10033         N    iPadAir
##                                                                                                descr.my
## 16                                                                                                     
## 33 We are selling good quality iPads that have been fully tested by an Apple Certified Technician. The 
##    D.T.condit D.T.use D.T.scratch D.T.new D.T.good  D.T.ipad D.T.screen
## 16          0       0           0       0 0.000000 0.0000000          0
## 33          0       0           0       0 0.417059 0.3908446          0
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 16         0        0         0        0        0       0
## 33         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 44      10044         N    iPadAir
## 1233    11233         Y    iPadAir
##                                                                               descr.my
## 44   Open Box Units Grade A Condition. Units may contain minor cosmetic imperfections.
## 1233                                                                   MINT CONDITION!
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.ipad D.T.screen
## 44     0.220126       0           0       0        0        0          0
## 1233   1.210693       0           0       0        0        0          0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 44           0        0         0        0        0       0
## 1233         0        0         0        0        0       0
```

![](ebayipads_spdiff_files/figure-html/cluster.data-5.png) 

```
## [1] "Category: iPadmini"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 7     10007         Y   iPadmini
## 76    10076         Y   iPadmini
##                                                                                         descr.my
## 7                                                                                               
## 76 Works perfectly, NOT iCloud locked, 1 owner. It is in not in very good  condition, but works 
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.ipad D.T.screen
## 7   0.0000000       0           0       0 0.0000000        0          0
## 76  0.3026733       0           0       0 0.4691913        0          0
##    D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 7          0 0.0000000         0        0        0       0
## 76         0 0.9365565         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 491     10491         N   iPadmini
## 1753    11754         N   iPadmini
##                                                                                               descr.my
## 491                      Cracked screen, flaw is shown in picture, everything is fully functional and 
## 1753 Shows Apple ID locked, password required on activation screen.  Unknown imei and storage space.  
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.ipad D.T.screen
## 491           0       0           0       0        0        0  0.4551091
## 1753          0       0           0       0        0        0  0.4045414
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 491          0        0         0        0        0       0
## 1753         0        0         0        0        0       0
```

![](ebayipads_spdiff_files/figure-html/cluster.data-6.png) 

```
## [1] "Category: iPadmini 2+"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr  prdline.my
## 4     10004         N iPadmini 2+
## 18    10018         N iPadmini 2+
##                                                                                                descr.my
## 4                                                                                                      
## 18 We are selling good quality iPads that have been fully tested by an Apple Certified Technician. The 
##    D.T.condit D.T.use D.T.scratch D.T.new D.T.good  D.T.ipad D.T.screen
## 4           0       0           0       0 0.000000 0.0000000          0
## 18          0       0           0       0 0.417059 0.3908446          0
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 4          0        0         0        0        0       0
## 18         0        0         0        0        0       0
## [1] "min distance(0.0000) pair:"
##   UniqueID sold.fctr  prdline.my descr.my D.T.condit D.T.use D.T.scratch
## 4    10004         N iPadmini 2+                   0       0           0
## 6    10006         Y iPadmini 2+                   0       0           0
##   D.T.new D.T.good D.T.ipad D.T.screen D.T.great D.T.work D.T.excel
## 4       0        0        0          0         0        0         0
## 6       0        0        0          0         0        0         0
##   D.P.http D.P.mini D.P.air
## 4        0        0       0
## 6        0        0       0
```

![](ebayipads_spdiff_files/figure-html/cluster.data-7.png) 

```
## [1] "glb_allobs_df$prdline.my$.clusterid Entropy: 0.6738 (98.3669 pct)"
##    prdline.my.clusterid   N   Y  .entropy .knt
## 1             Unknown_1  77  53 0.6760076  130
## 2             Unknown_2  26  22 0.6896709   48
## 3             Unknown_3  15   5 0.5623351   20
## 4              iPad 1_1  62  88 0.6780488  150
## 5              iPad 1_2  13  16 0.6877868   29
## 6              iPad 1_3  13  13 0.6931472   26
## 7              iPad 1_4  12   8 0.6730117   20
## 8              iPad 2_1  72  94 0.6843392  166
## 9              iPad 2_2  40  29 0.6803854   69
## 10             iPad 2_3  18   5 0.5235863   23
## 11             iPad 2_4   6  11 0.6492484   17
## 12             iPad 2_5   5   8 0.6662784   13
## 13            iPad 3+_1  85 101 0.6894428  186
## 14            iPad 3+_2  41  13 0.5519323   54
## 15            iPad 3+_3  23  16 0.6769517   39
## 16            iPad 3+_4  17  15 0.6911928   32
## 17            iPadAir_1 142 107 0.6832355  249
## 18            iPadAir_2  28  24 0.6901857   52
## 19            iPadAir_3  17  10 0.6591525   27
## 20            iPadAir_4  16   9 0.6534182   25
## 21        iPadmini 2+_1 100  61 0.6635142  161
## 22        iPadmini 2+_2  15  10 0.6730117   25
## 23        iPadmini 2+_3  10   9 0.6917615   19
## 24           iPadmini_1  99  87 0.6910646  186
## 25           iPadmini_2  17  14 0.6884572   31
## 26           iPadmini_3  11  18 0.6637255   29
## 27           iPadmini_4  12  10 0.6890092   22
## 28           iPadmini_5   7   4 0.6554818   11
```

```r
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
## 7 manage.missing.data          4          1 91.469 94.675   3.206
## 8     select.features          5          0 94.676     NA      NA
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
##                                                      id         cor.y
## sold                                               sold  1.0000000000
## biddable                                       biddable  0.5481788380
## startprice.log                           startprice.log -0.4674275376
## startprice                                   startprice -0.4569767211
## startprice.predict.                 startprice.predict. -0.3790187140
## startprice.diff                         startprice.diff -0.3167760893
## UniqueID                                       UniqueID -0.1895466260
## idseq.my                                       idseq.my -0.1895466260
## condition.fctr                           condition.fctr -0.1535490071
## D.npnct05.log                             D.npnct05.log -0.1180558939
## D.terms.n.post.stop                 D.terms.n.post.stop -0.0840450507
## D.terms.n.post.stem                 D.terms.n.post.stem -0.0838967832
## D.npnct14.log                             D.npnct14.log -0.0786203827
## cellular.fctr                             cellular.fctr -0.0739443419
## carrier.fctr                               carrier.fctr -0.0687945647
## D.terms.n.post.stem.log         D.terms.n.post.stem.log -0.0656268732
## D.nwrds.unq.log                         D.nwrds.unq.log -0.0656268732
## D.terms.n.post.stop.log         D.terms.n.post.stop.log -0.0656220969
## D.ndgts.log                                 D.ndgts.log -0.0637072932
## D.npnct09.log                             D.npnct09.log -0.0618253281
## D.nwrds.log                                 D.nwrds.log -0.0590061339
## D.npnct12.log                             D.npnct12.log -0.0568348971
## D.ratio.nstopwrds.nwrds         D.ratio.nstopwrds.nwrds  0.0567372450
## D.nchrs.log                                 D.nchrs.log -0.0565369891
## D.nuppr.log                                 D.nuppr.log -0.0553942172
## D.npnct28.log                             D.npnct28.log -0.0524583244
## D.npnct06.log                             D.npnct06.log -0.0499761958
## D.npnct15.log                             D.npnct15.log  0.0484022793
## D.npnct24.log                             D.npnct24.log -0.0458449965
## D.nstopwrds.log                         D.nstopwrds.log -0.0454507052
## D.npnct16.log                             D.npnct16.log -0.0449403962
## color.fctr                                   color.fctr -0.0433350320
## prdline.my.fctr                         prdline.my.fctr -0.0415814340
## D.npnct08.log                             D.npnct08.log -0.0396513123
## D.T.new                                         D.T.new -0.0384952089
## D.npnct13.log                             D.npnct13.log -0.0373463069
## D.T.condit                                   D.T.condit -0.0368137842
## D.TfIdf.sum.post.stem             D.TfIdf.sum.post.stem -0.0360956970
## D.sum.TfIdf                                 D.sum.TfIdf -0.0360956970
## D.TfIdf.sum.post.stop             D.TfIdf.sum.post.stop -0.0333517826
## .clusterid                                   .clusterid -0.0331547453
## .clusterid.fctr                         .clusterid.fctr -0.0331547453
## D.T.excel                                     D.T.excel  0.0264721403
## D.npnct03.log                             D.npnct03.log  0.0257637868
## D.npnct07.log                             D.npnct07.log  0.0250040676
## D.T.screen                                   D.T.screen  0.0246517455
## D.npnct10.log                             D.npnct10.log -0.0241015016
## D.npnct18.log                             D.npnct18.log -0.0215250231
## D.npnct11.log                             D.npnct11.log -0.0192035548
## D.terms.n.stem.stop.Ratio     D.terms.n.stem.stop.Ratio  0.0163860117
## D.T.ipad                                       D.T.ipad -0.0143370274
## D.T.work                                       D.T.work -0.0128924650
## D.T.use                                         D.T.use  0.0122706569
## D.P.mini                                       D.P.mini -0.0112418293
## storage.fctr                               storage.fctr -0.0110508589
## D.P.air                                         D.P.air -0.0092629952
## D.TfIdf.sum.stem.stop.Ratio D.TfIdf.sum.stem.stop.Ratio -0.0087527853
## D.ratio.sum.TfIdf.nwrds         D.ratio.sum.TfIdf.nwrds  0.0078275033
## D.T.great                                     D.T.great  0.0074871041
## .rnorm                                           .rnorm  0.0067562740
## D.T.scratch                                 D.T.scratch -0.0066323680
## D.npnct01.log                             D.npnct01.log  0.0041255300
## D.T.good                                       D.T.good -0.0005014283
## D.npnct02.log                             D.npnct02.log            NA
## D.npnct04.log                             D.npnct04.log            NA
## D.npnct17.log                             D.npnct17.log            NA
## D.npnct19.log                             D.npnct19.log            NA
## D.npnct20.log                             D.npnct20.log            NA
## D.npnct21.log                             D.npnct21.log            NA
## D.npnct22.log                             D.npnct22.log            NA
## D.npnct23.log                             D.npnct23.log            NA
## D.npnct25.log                             D.npnct25.log            NA
## D.npnct26.log                             D.npnct26.log            NA
## D.npnct27.log                             D.npnct27.log            NA
## D.npnct29.log                             D.npnct29.log            NA
## D.npnct30.log                             D.npnct30.log            NA
## D.P.http                                       D.P.http            NA
##                             exclude.as.feat    cor.y.abs
## sold                                      1 1.0000000000
## biddable                                  0 0.5481788380
## startprice.log                            1 0.4674275376
## startprice                                1 0.4569767211
## startprice.predict.                       1 0.3790187140
## startprice.diff                           0 0.3167760893
## UniqueID                                  1 0.1895466260
## idseq.my                                  0 0.1895466260
## condition.fctr                            0 0.1535490071
## D.npnct05.log                             0 0.1180558939
## D.terms.n.post.stop                       0 0.0840450507
## D.terms.n.post.stem                       0 0.0838967832
## D.npnct14.log                             0 0.0786203827
## cellular.fctr                             0 0.0739443419
## carrier.fctr                              0 0.0687945647
## D.terms.n.post.stem.log                   0 0.0656268732
## D.nwrds.unq.log                           0 0.0656268732
## D.terms.n.post.stop.log                   0 0.0656220969
## D.ndgts.log                               0 0.0637072932
## D.npnct09.log                             0 0.0618253281
## D.nwrds.log                               0 0.0590061339
## D.npnct12.log                             0 0.0568348971
## D.ratio.nstopwrds.nwrds                   0 0.0567372450
## D.nchrs.log                               0 0.0565369891
## D.nuppr.log                               0 0.0553942172
## D.npnct28.log                             0 0.0524583244
## D.npnct06.log                             0 0.0499761958
## D.npnct15.log                             0 0.0484022793
## D.npnct24.log                             0 0.0458449965
## D.nstopwrds.log                           0 0.0454507052
## D.npnct16.log                             0 0.0449403962
## color.fctr                                0 0.0433350320
## prdline.my.fctr                           0 0.0415814340
## D.npnct08.log                             0 0.0396513123
## D.T.new                                   1 0.0384952089
## D.npnct13.log                             0 0.0373463069
## D.T.condit                                1 0.0368137842
## D.TfIdf.sum.post.stem                     0 0.0360956970
## D.sum.TfIdf                               0 0.0360956970
## D.TfIdf.sum.post.stop                     0 0.0333517826
## .clusterid                                1 0.0331547453
## .clusterid.fctr                           0 0.0331547453
## D.T.excel                                 1 0.0264721403
## D.npnct03.log                             0 0.0257637868
## D.npnct07.log                             0 0.0250040676
## D.T.screen                                1 0.0246517455
## D.npnct10.log                             0 0.0241015016
## D.npnct18.log                             0 0.0215250231
## D.npnct11.log                             0 0.0192035548
## D.terms.n.stem.stop.Ratio                 0 0.0163860117
## D.T.ipad                                  1 0.0143370274
## D.T.work                                  1 0.0128924650
## D.T.use                                   1 0.0122706569
## D.P.mini                                  1 0.0112418293
## storage.fctr                              0 0.0110508589
## D.P.air                                   1 0.0092629952
## D.TfIdf.sum.stem.stop.Ratio               0 0.0087527853
## D.ratio.sum.TfIdf.nwrds                   0 0.0078275033
## D.T.great                                 1 0.0074871041
## .rnorm                                    0 0.0067562740
## D.T.scratch                               1 0.0066323680
## D.npnct01.log                             0 0.0041255300
## D.T.good                                  1 0.0005014283
## D.npnct02.log                             0           NA
## D.npnct04.log                             0           NA
## D.npnct17.log                             0           NA
## D.npnct19.log                             0           NA
## D.npnct20.log                             0           NA
## D.npnct21.log                             0           NA
## D.npnct22.log                             0           NA
## D.npnct23.log                             0           NA
## D.npnct25.log                             0           NA
## D.npnct26.log                             0           NA
## D.npnct27.log                             0           NA
## D.npnct29.log                             0           NA
## D.npnct30.log                             0           NA
## D.P.http                                  1           NA
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.sum.TfIdf)=1.0000"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0361"
## [1] "cor(sold.fctr, D.sum.TfIdf)=-0.0361"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.sum.TfIdf as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stem.log)=1.0000"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0656"
## [1] "cor(sold.fctr, D.terms.n.post.stem.log)=-0.0656"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stem.log as highly correlated
## with D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stop.log)=0.9999"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0656"
## [1] "cor(sold.fctr, D.terms.n.post.stop.log)=-0.0656"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stop.log as highly correlated
## with D.nwrds.unq.log
```

```
## [1] "cor(D.nchrs.log, D.nuppr.log)=0.9995"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0565"
## [1] "cor(sold.fctr, D.nuppr.log)=-0.0554"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nuppr.log as highly correlated with
## D.nchrs.log
```

```
## [1] "cor(D.terms.n.post.stem, D.terms.n.post.stop)=0.9991"
## [1] "cor(sold.fctr, D.terms.n.post.stem)=-0.0839"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0840"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stem as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.TfIdf.sum.post.stop)=0.9977"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0361"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stop)=-0.0334"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stop as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nchrs.log, D.nwrds.unq.log)=0.9932"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0565"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0656"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nchrs.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.log, D.nwrds.unq.log)=0.9925"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0590"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0656"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stop)=0.9752"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0656"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0840"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.unq.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.npnct24.log, D.ratio.nstopwrds.nwrds)=-0.9641"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0458"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0567"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct24.log as highly correlated with
## D.ratio.nstopwrds.nwrds
```

```
## [1] "cor(D.npnct06.log, D.npnct16.log)=0.9556"
## [1] "cor(sold.fctr, D.npnct06.log)=-0.0500"
## [1] "cor(sold.fctr, D.npnct16.log)=-0.0449"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct16.log as highly correlated with
## D.npnct06.log
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.ratio.nstopwrds.nwrds)=-0.9254"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0361"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0567"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stem as highly correlated with
## D.ratio.nstopwrds.nwrds
```

```
## [1] "cor(D.nstopwrds.log, D.terms.n.post.stop)=0.8931"
## [1] "cor(sold.fctr, D.nstopwrds.log)=-0.0455"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0840"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nstopwrds.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.ratio.nstopwrds.nwrds, D.terms.n.post.stop)=-0.8677"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0567"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0840"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.ratio.nstopwrds.nwrds as highly correlated
## with D.terms.n.post.stop
```

```
## [1] "cor(carrier.fctr, cellular.fctr)=0.8460"
## [1] "cor(sold.fctr, carrier.fctr)=-0.0688"
## [1] "cor(sold.fctr, cellular.fctr)=-0.0739"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified carrier.fctr as highly correlated with
## cellular.fctr
```

```
## [1] "cor(D.npnct13.log, D.terms.n.post.stop)=0.7381"
## [1] "cor(sold.fctr, D.npnct13.log)=-0.0373"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0840"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct13.log as highly correlated with
## D.terms.n.post.stop
```

```
##                             id         cor.y exclude.as.feat    cor.y.abs
## 72                        sold  1.0000000000               1 1.0000000000
## 65                    biddable  0.5481788380               0 0.5481788380
## 56     D.ratio.nstopwrds.nwrds  0.0567372450               0 0.0567372450
## 36               D.npnct15.log  0.0484022793               0 0.0484022793
## 8                    D.T.excel  0.0264721403               1 0.0264721403
## 24               D.npnct03.log  0.0257637868               0 0.0257637868
## 28               D.npnct07.log  0.0250040676               0 0.0250040676
## 14                  D.T.screen  0.0246517455               1 0.0246517455
## 63   D.terms.n.stem.stop.Ratio  0.0163860117               0 0.0163860117
## 15                     D.T.use  0.0122706569               1 0.0122706569
## 57     D.ratio.sum.TfIdf.nwrds  0.0078275033               0 0.0078275033
## 10                   D.T.great  0.0074871041               1 0.0074871041
## 3                       .rnorm  0.0067562740               0 0.0067562740
## 22               D.npnct01.log  0.0041255300               0 0.0041255300
## 9                     D.T.good -0.0005014283               1 0.0005014283
## 13                 D.T.scratch -0.0066323680               1 0.0066323680
## 19 D.TfIdf.sum.stem.stop.Ratio -0.0087527853               0 0.0087527853
## 4                      D.P.air -0.0092629952               1 0.0092629952
## 77                storage.fctr -0.0110508589               0 0.0110508589
## 6                     D.P.mini -0.0112418293               1 0.0112418293
## 16                    D.T.work -0.0128924650               1 0.0128924650
## 11                    D.T.ipad -0.0143370274               1 0.0143370274
## 32               D.npnct11.log -0.0192035548               0 0.0192035548
## 39               D.npnct18.log -0.0215250231               0 0.0215250231
## 31               D.npnct10.log -0.0241015016               0 0.0241015016
## 1                   .clusterid -0.0331547453               1 0.0331547453
## 2              .clusterid.fctr -0.0331547453               0 0.0331547453
## 18       D.TfIdf.sum.post.stop -0.0333517826               0 0.0333517826
## 17       D.TfIdf.sum.post.stem -0.0360956970               0 0.0360956970
## 58                 D.sum.TfIdf -0.0360956970               0 0.0360956970
## 7                   D.T.condit -0.0368137842               1 0.0368137842
## 34               D.npnct13.log -0.0373463069               0 0.0373463069
## 12                     D.T.new -0.0384952089               1 0.0384952089
## 29               D.npnct08.log -0.0396513123               0 0.0396513123
## 71             prdline.my.fctr -0.0415814340               0 0.0415814340
## 68                  color.fctr -0.0433350320               0 0.0433350320
## 37               D.npnct16.log -0.0449403962               0 0.0449403962
## 52             D.nstopwrds.log -0.0454507052               0 0.0454507052
## 45               D.npnct24.log -0.0458449965               0 0.0458449965
## 27               D.npnct06.log -0.0499761958               0 0.0499761958
## 49               D.npnct28.log -0.0524583244               0 0.0524583244
## 53                 D.nuppr.log -0.0553942172               0 0.0553942172
## 20                 D.nchrs.log -0.0565369891               0 0.0565369891
## 33               D.npnct12.log -0.0568348971               0 0.0568348971
## 54                 D.nwrds.log -0.0590061339               0 0.0590061339
## 30               D.npnct09.log -0.0618253281               0 0.0618253281
## 21                 D.ndgts.log -0.0637072932               0 0.0637072932
## 62     D.terms.n.post.stop.log -0.0656220969               0 0.0656220969
## 55             D.nwrds.unq.log -0.0656268732               0 0.0656268732
## 60     D.terms.n.post.stem.log -0.0656268732               0 0.0656268732
## 66                carrier.fctr -0.0687945647               0 0.0687945647
## 67               cellular.fctr -0.0739443419               0 0.0739443419
## 35               D.npnct14.log -0.0786203827               0 0.0786203827
## 59         D.terms.n.post.stem -0.0838967832               0 0.0838967832
## 61         D.terms.n.post.stop -0.0840450507               0 0.0840450507
## 26               D.npnct05.log -0.1180558939               0 0.1180558939
## 69              condition.fctr -0.1535490071               0 0.1535490071
## 64                    UniqueID -0.1895466260               1 0.1895466260
## 70                    idseq.my -0.1895466260               0 0.1895466260
## 74             startprice.diff -0.3167760893               0 0.3167760893
## 76         startprice.predict. -0.3790187140               1 0.3790187140
## 73                  startprice -0.4569767211               1 0.4569767211
## 75              startprice.log -0.4674275376               1 0.4674275376
## 5                     D.P.http            NA               1           NA
## 23               D.npnct02.log            NA               0           NA
## 25               D.npnct04.log            NA               0           NA
## 38               D.npnct17.log            NA               0           NA
## 40               D.npnct19.log            NA               0           NA
## 41               D.npnct20.log            NA               0           NA
## 42               D.npnct21.log            NA               0           NA
## 43               D.npnct22.log            NA               0           NA
## 44               D.npnct23.log            NA               0           NA
## 46               D.npnct25.log            NA               0           NA
## 47               D.npnct26.log            NA               0           NA
## 48               D.npnct27.log            NA               0           NA
## 50               D.npnct29.log            NA               0           NA
## 51               D.npnct30.log            NA               0           NA
##                 cor.high.X   freqRatio percentUnique zeroVar   nzv
## 72                    <NA>    1.161628    0.10758472   FALSE FALSE
## 65                    <NA>    1.221027    0.10758472   FALSE FALSE
## 56     D.terms.n.post.stop   13.048780    4.24959656   FALSE FALSE
## 36                    <NA>  153.416667    0.16137708   FALSE  TRUE
## 8                     <NA>  128.285714    0.75309306   FALSE  TRUE
## 24                    <NA>   83.227273    0.16137708   FALSE  TRUE
## 28                    <NA> 1858.000000    0.10758472   FALSE  TRUE
## 14                    <NA>   53.343750    0.80688542   FALSE  TRUE
## 63                    <NA>   77.826087    0.48413125   FALSE  TRUE
## 15                    <NA>   44.675676    0.91447015   FALSE  TRUE
## 57                    <NA>   63.000000   34.85745024   FALSE FALSE
## 10                    <NA>  118.400000    0.80688542   FALSE  TRUE
## 3                     <NA>    1.000000  100.00000000   FALSE FALSE
## 22                    <NA>   52.970588    0.32275417   FALSE  TRUE
## 9                     <NA>   45.315789    0.86067778   FALSE  TRUE
## 13                    <NA>   41.400000    0.86067778   FALSE  TRUE
## 19                    <NA>   65.176471   32.92092523   FALSE FALSE
## 4                     <NA>  122.866667    0.16137708   FALSE  TRUE
## 77                    <NA>    2.733138    0.26896181   FALSE FALSE
## 6                     <NA>   91.900000    0.16137708   FALSE  TRUE
## 16                    <NA>   59.241379    0.69930070   FALSE  TRUE
## 11                    <NA>   49.823529    0.80688542   FALSE  TRUE
## 32                    <NA>    9.374269    0.37654653   FALSE FALSE
## 39                    <NA> 1858.000000    0.10758472   FALSE  TRUE
## 31                    <NA>  308.666667    0.16137708   FALSE  TRUE
## 1                     <NA>    3.987013    0.26896181   FALSE FALSE
## 2                     <NA>    3.987013    0.26896181   FALSE FALSE
## 18   D.TfIdf.sum.post.stem   63.000000   34.42711135   FALSE FALSE
## 17 D.ratio.nstopwrds.nwrds   63.000000   34.31952663   FALSE FALSE
## 58   D.TfIdf.sum.post.stem   63.000000   34.31952663   FALSE FALSE
## 7                     <NA>   22.984848    0.91447015   FALSE  TRUE
## 34     D.terms.n.post.stop    5.203065    0.32275417   FALSE FALSE
## 12                    <NA>  103.000000    0.86067778   FALSE  TRUE
## 29                    <NA>   69.576923    0.21516945   FALSE  TRUE
## 71                    <NA>    1.135048    0.37654653   FALSE FALSE
## 68                    <NA>    1.574610    0.26896181   FALSE FALSE
## 37           D.npnct06.log   31.245614    0.16137708   FALSE  TRUE
## 52     D.terms.n.post.stop   13.916667    0.80688542   FALSE FALSE
## 45 D.ratio.nstopwrds.nwrds    1.356147    0.10758472   FALSE FALSE
## 27                    <NA>   33.735849    0.16137708   FALSE  TRUE
## 49                    <NA>  463.250000    0.16137708   FALSE  TRUE
## 53             D.nchrs.log   18.807018    4.41097364   FALSE FALSE
## 20         D.nwrds.unq.log   15.970149    5.70199032   FALSE FALSE
## 33                    <NA>   26.818182    0.21516945   FALSE  TRUE
## 54         D.nwrds.unq.log   12.891566    1.29101668   FALSE FALSE
## 30                    <NA>  308.333333    0.21516945   FALSE  TRUE
## 21                    <NA>   27.031746    0.69930070   FALSE  TRUE
## 62         D.nwrds.unq.log    8.052632    0.80688542   FALSE FALSE
## 55     D.terms.n.post.stop    7.595745    0.80688542   FALSE FALSE
## 60         D.nwrds.unq.log    7.595745    0.80688542   FALSE FALSE
## 66           cellular.fctr    3.195965    0.37654653   FALSE FALSE
## 67                    <NA>    2.112381    0.16137708   FALSE FALSE
## 35                    <NA>   35.333333    0.26896181   FALSE  TRUE
## 59     D.terms.n.post.stop    7.595745    0.80688542   FALSE FALSE
## 61                    <NA>    8.052632    0.80688542   FALSE FALSE
## 26                    <NA>   40.311111    0.10758472   FALSE  TRUE
## 69                    <NA>    4.003460    0.32275417   FALSE FALSE
## 64                    <NA>    1.000000  100.00000000   FALSE FALSE
## 70                    <NA>    1.000000  100.00000000   FALSE FALSE
## 74                    <NA>    1.000000   99.89241528   FALSE FALSE
## 76                    <NA>    1.333333   99.03173749   FALSE FALSE
## 73                    <NA>    2.807692   30.17751479   FALSE FALSE
## 75                    <NA>    2.807692   30.17751479   FALSE FALSE
## 5                     <NA>    0.000000    0.05379236    TRUE  TRUE
## 23                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 25                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 38                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 40                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 41                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 42                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 43                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 44                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 46                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 47                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 48                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 50                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 51                    <NA>    0.000000    0.05379236    TRUE  TRUE
##    myNearZV is.cor.y.abs.low
## 72    FALSE            FALSE
## 65    FALSE            FALSE
## 56    FALSE            FALSE
## 36    FALSE            FALSE
## 8     FALSE            FALSE
## 24    FALSE            FALSE
## 28     TRUE            FALSE
## 14    FALSE            FALSE
## 63    FALSE            FALSE
## 15    FALSE            FALSE
## 57    FALSE            FALSE
## 10    FALSE            FALSE
## 3     FALSE            FALSE
## 22    FALSE             TRUE
## 9     FALSE             TRUE
## 13    FALSE             TRUE
## 19    FALSE            FALSE
## 4     FALSE            FALSE
## 77    FALSE            FALSE
## 6     FALSE            FALSE
## 16    FALSE            FALSE
## 11    FALSE            FALSE
## 32    FALSE            FALSE
## 39     TRUE            FALSE
## 31    FALSE            FALSE
## 1     FALSE            FALSE
## 2     FALSE            FALSE
## 18    FALSE            FALSE
## 17    FALSE            FALSE
## 58    FALSE            FALSE
## 7     FALSE            FALSE
## 34    FALSE            FALSE
## 12    FALSE            FALSE
## 29    FALSE            FALSE
## 71    FALSE            FALSE
## 68    FALSE            FALSE
## 37    FALSE            FALSE
## 52    FALSE            FALSE
## 45    FALSE            FALSE
## 27    FALSE            FALSE
## 49    FALSE            FALSE
## 53    FALSE            FALSE
## 20    FALSE            FALSE
## 33    FALSE            FALSE
## 54    FALSE            FALSE
## 30    FALSE            FALSE
## 21    FALSE            FALSE
## 62    FALSE            FALSE
## 55    FALSE            FALSE
## 60    FALSE            FALSE
## 66    FALSE            FALSE
## 67    FALSE            FALSE
## 35    FALSE            FALSE
## 59    FALSE            FALSE
## 61    FALSE            FALSE
## 26    FALSE            FALSE
## 69    FALSE            FALSE
## 64    FALSE            FALSE
## 70    FALSE            FALSE
## 74    FALSE            FALSE
## 76    FALSE            FALSE
## 73    FALSE            FALSE
## 75    FALSE            FALSE
## 5      TRUE               NA
## 23     TRUE               NA
## 25     TRUE               NA
## 38     TRUE               NA
## 40     TRUE               NA
## 41     TRUE               NA
## 42     TRUE               NA
## 43     TRUE               NA
## 44     TRUE               NA
## 46     TRUE               NA
## 47     TRUE               NA
## 48     TRUE               NA
## 50     TRUE               NA
## 51     TRUE               NA
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
## Warning: Removed 12 rows containing missing values (geom_point).
```

```
## Warning: Removed 12 rows containing missing values (geom_point).
```

```
## Warning: Removed 12 rows containing missing values (geom_point).
```

![](ebayipads_spdiff_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##               id       cor.y exclude.as.feat  cor.y.abs cor.high.X
## 28 D.npnct07.log  0.02500407               0 0.02500407       <NA>
## 39 D.npnct18.log -0.02152502               0 0.02152502       <NA>
## 5       D.P.http          NA               1         NA       <NA>
## 23 D.npnct02.log          NA               0         NA       <NA>
## 25 D.npnct04.log          NA               0         NA       <NA>
## 38 D.npnct17.log          NA               0         NA       <NA>
## 40 D.npnct19.log          NA               0         NA       <NA>
## 41 D.npnct20.log          NA               0         NA       <NA>
## 42 D.npnct21.log          NA               0         NA       <NA>
## 43 D.npnct22.log          NA               0         NA       <NA>
## 44 D.npnct23.log          NA               0         NA       <NA>
## 46 D.npnct25.log          NA               0         NA       <NA>
## 47 D.npnct26.log          NA               0         NA       <NA>
## 48 D.npnct27.log          NA               0         NA       <NA>
## 50 D.npnct29.log          NA               0         NA       <NA>
## 51 D.npnct30.log          NA               0         NA       <NA>
##    freqRatio percentUnique zeroVar  nzv myNearZV is.cor.y.abs.low
## 28      1858    0.10758472   FALSE TRUE     TRUE            FALSE
## 39      1858    0.10758472   FALSE TRUE     TRUE            FALSE
## 5          0    0.05379236    TRUE TRUE     TRUE               NA
## 23         0    0.05379236    TRUE TRUE     TRUE               NA
## 25         0    0.05379236    TRUE TRUE     TRUE               NA
## 38         0    0.05379236    TRUE TRUE     TRUE               NA
## 40         0    0.05379236    TRUE TRUE     TRUE               NA
## 41         0    0.05379236    TRUE TRUE     TRUE               NA
## 42         0    0.05379236    TRUE TRUE     TRUE               NA
## 43         0    0.05379236    TRUE TRUE     TRUE               NA
## 44         0    0.05379236    TRUE TRUE     TRUE               NA
## 46         0    0.05379236    TRUE TRUE     TRUE               NA
## 47         0    0.05379236    TRUE TRUE     TRUE               NA
## 48         0    0.05379236    TRUE TRUE     TRUE               NA
## 50         0    0.05379236    TRUE TRUE     TRUE               NA
## 51         0    0.05379236    TRUE TRUE     TRUE               NA
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
##                biddable                    sold          startprice.log 
##                    1444                     999                      31 
##           cellular.fctr     D.terms.n.post.stop D.terms.n.post.stop.log 
##                    1597                    1521                    1521 
##   D.TfIdf.sum.post.stop     D.terms.n.post.stem D.terms.n.post.stem.log 
##                    1521                    1521                    1521 
##   D.TfIdf.sum.post.stem              D.T.condit                 D.T.use 
##                    1521                    2161                    2366 
##             D.T.scratch                 D.T.new                D.T.good 
##                    2371                    2501                    2460 
##                D.T.ipad              D.T.screen               D.T.great 
##                    2425                    2444                    2532 
##                D.T.work               D.T.excel             D.nwrds.log 
##                    2459                    2557                    1520 
##         D.nwrds.unq.log             D.sum.TfIdf D.ratio.sum.TfIdf.nwrds 
##                    1521                    1521                    1521 
##             D.nchrs.log             D.nuppr.log             D.ndgts.log 
##                    1520                    1522                    2426 
##           D.npnct01.log           D.npnct03.log           D.npnct05.log 
##                    2579                    2614                    2592 
##           D.npnct06.log           D.npnct08.log           D.npnct09.log 
##                    2554                    2581                    2641 
##           D.npnct10.log           D.npnct11.log           D.npnct12.log 
##                    2648                    2301                    2537 
##           D.npnct13.log           D.npnct14.log           D.npnct15.log 
##                    1932                    2582                    2637 
##           D.npnct16.log           D.npnct24.log           D.npnct28.log 
##                    2546                    1520                    2649 
##         D.nstopwrds.log                D.P.mini                 D.P.air 
##                    1663                    2623                    2637 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## description   condition    cellular     carrier       color     storage 
##        1520           0           0           0           0           0 
## productline      .grpid  prdline.my    descr.my 
##           0          NA           0        1520
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 94.676 98.481   3.805
## 9 partition.data.training          6          0 98.481     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    
    set.seed(glb_split_sample.seed)
    
    OOB_size <- nrow(glb_newobs_df) * 1.1
    if (is.null(glb_category_var)) {
        require(caTools)
        split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                              SplitRatio=OOB_size / nrow(glb_trnobs_df))
        glb_OOBobs_df <- glb_trnobs_df[split ,]            
        glb_fitobs_df <- glb_trnobs_df[!split, ] 
    } else {
        sample_vars <- c(glb_rsp_var_raw, glb_category_var)
        rspvar_freq_df <- orderBy(reformulate(glb_rsp_var_raw), 
                                  mycreate_sqlxtab_df(glb_trnobs_df, glb_rsp_var_raw))
        OOB_rspvar_size <- 1.0 * OOB_size * rspvar_freq_df$.n / sum(rspvar_freq_df$.n) 
        newobs_freq_df <- orderBy(reformulate(glb_category_var),
                                  mycreate_sqlxtab_df(glb_newobs_df, glb_category_var))
        trnobs_freq_df <- orderBy(reformulate(glb_category_var),
                                  mycreate_sqlxtab_df(glb_trnobs_df, glb_category_var))
        allobs_freq_df <- merge(newobs_freq_df, trnobs_freq_df, by=glb_category_var,
                                all=TRUE, sort=TRUE, suffixes=c(".Tst", ".Train"))
        allobs_freq_df[is.na(allobs_freq_df)] <- 0
        OOB_strata_size <- ceiling(
            as.vector(matrix(allobs_freq_df$.n.Tst * 1.0 / sum(allobs_freq_df$.n.Tst)) %*%
                      matrix(OOB_rspvar_size, nrow=1)))
        OOB_strata_size[OOB_strata_size == 0] <- 1
        OOB_strata_df <- expand.grid(glb_rsp_var_raw=rspvar_freq_df[, glb_rsp_var_raw],
                                     glb_category_var=allobs_freq_df[, glb_category_var])
        names(OOB_strata_df) <- sample_vars
        OOB_strata_df <- orderBy(reformulate(sample_vars), OOB_strata_df)
        
        trnobs_univ_df <- orderBy(reformulate(sample_vars),
                                       mycreate_sqlxtab_df(glb_trnobs_df, sample_vars))
        trnobs_univ_df <- merge(trnobs_univ_df, OOB_strata_df, all=TRUE)
        tmp_trnobs_df <- orderBy(reformulate(c(glb_rsp_var_raw, glb_category_var)),
                                glb_trnobs_df)
        require(sampling)
        split_strata <- strata(tmp_trnobs_df, 
                               stratanames=c(glb_rsp_var_raw, glb_category_var),
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
if (!is.null(glb_category_var)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_var)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_var)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_var
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}
```

```
##     sold.0 sold.1 sold.NA
##         NA     NA     798
## Fit    524    450      NA
## OOB    475    410      NA
##        sold.0    sold.1 sold.NA
##            NA        NA       1
## Fit 0.5379877 0.4620123      NA
## OOB 0.5367232 0.4632768      NA
##    prdline.my .n.Tst .n.OOB .freqRatio.Tst .freqRatio.OOB
## 3      iPad 2    154    171      0.1929825      0.1932203
## 5     iPadAir    137    151      0.1716792      0.1706215
## 4     iPad 3+    123    136      0.1541353      0.1536723
## 6    iPadmini    114    127      0.1428571      0.1435028
## 7 iPadmini 2+     94    104      0.1177945      0.1175141
## 2      iPad 1     89     99      0.1115288      0.1118644
## 1     Unknown     87     97      0.1090226      0.1096045
```

```r
# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 77 12
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
## 72             sold  1.0000000            TRUE 1.0000000       <NA>
## 64         UniqueID -0.1895466            TRUE 0.1895466       <NA>
## sold.fctr sold.fctr         NA            TRUE        NA       <NA>
##           freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 72         1.161628     0.1075847   FALSE FALSE    FALSE            FALSE
## 64         1.000000   100.0000000   FALSE FALSE    FALSE            FALSE
## sold.fctr        NA            NA      NA    NA       NA               NA
##           interaction.feat rsp_var_raw id_var rsp_var
## 72                    <NA>        TRUE     NA      NA
## 64                    <NA>       FALSE   TRUE      NA
## sold.fctr             <NA>          NA     NA    TRUE
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
##  [1] "D.npnct07.log" "D.npnct18.log" "D.P.http"      "D.npnct02.log"
##  [5] "D.npnct04.log" "D.npnct17.log" "D.npnct19.log" "D.npnct20.log"
##  [9] "D.npnct21.log" "D.npnct22.log" "D.npnct23.log" "D.npnct25.log"
## [13] "D.npnct26.log" "D.npnct27.log" "D.npnct29.log" "D.npnct30.log"
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
## [1] 2657   74
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 1859   73
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 974  73
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 885  73
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 798  73
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
## 9  partition.data.training          6          0 98.481 99.426   0.945
## 10              fit.models          7          0 99.427     NA      NA
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
## 0.5379877 0.4620123 
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
## 1 0.5379877 0.4620123
## 2 0.5379877 0.4620123
## 3 0.5379877 0.4620123
## 4 0.5379877 0.4620123
## 5 0.5379877 0.4620123
## 6 0.5379877 0.4620123
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.MFO.myMFO_classfr.N
## 1         N                                   524
## 2         Y                                   450
##          Prediction
## Reference   N   Y
##         N 524   0
##         Y 450   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.379877e-01   0.000000e+00   5.060896e-01   5.696555e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   5.131410e-01   1.962860e-99 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.5379877 0.4620123
## 2 0.5379877 0.4620123
## 3 0.5379877 0.4620123
## 4 0.5379877 0.4620123
## 5 0.5379877 0.4620123
## 6 0.5379877 0.4620123
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.MFO.myMFO_classfr.N
## 1         N                                   475
## 2         Y                                   410
##          Prediction
## Reference   N   Y
##         N 475   0
##         Y 410   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.367232e-01   0.000000e+00   5.032294e-01   5.699717e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   5.137716e-01   9.975777e-91 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.417                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.5379877
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.5060896             0.5696555             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.5367232
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5032294             0.5699717             0
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

![](ebayipads_spdiff_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.6320225
## 5        0.4 0.6320225
## 6        0.5 0.4747253
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Random.myrandom_classfr.Y
## 1         N                                         524
## 2         Y                                         450
##          Prediction
## Reference   N   Y
##         N   0 524
##         Y   0 450
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   4.620123e-01   0.000000e+00   4.303445e-01   4.939104e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   9.999991e-01  1.552393e-115 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.6332046
## 5        0.4 0.6332046
## 6        0.5 0.4822521
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Random.myrandom_classfr.Y
## 1         N                                         475
## 2         Y                                         410
##          Prediction
## Reference   N   Y
##         N   0 475
##         Y   0 410
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   4.632768e-01   0.000000e+00   4.300283e-01   4.967706e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   9.999948e-01  7.120214e-105 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.282                 0.002   0.5071756
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.4       0.6320225        0.4620123
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.4303445             0.4939104             0   0.5191913
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6332046        0.4632768
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.4300283             0.4967706             0
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
## [1] "    indep_vars: biddable, startprice.diff"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.511 on full training set
```

```
## Loading required package: rpart.plot
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##          CP nsplit rel error
## 1 0.5111111      0         1
## 
## Node number 1: 974 observations
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.5379877 0.4620123) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1         N                                      524
## 2         Y                                      450
##          Prediction
## Reference   N   Y
##         N 524   0
##         Y 450   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.379877e-01   0.000000e+00   5.060896e-01   5.696555e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   5.131410e-01   1.962860e-99 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1         N                                      475
## 2         Y                                      410
##          Prediction
## Reference   N   Y
##         N 475   0
##         Y 410   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.367232e-01   0.000000e+00   5.032294e-01   5.699717e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   5.137716e-01   9.975777e-91 
##               model_id model_method                     feats
## 1 Max.cor.Y.cv.0.rpart        rpart biddable, startprice.diff
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.634                 0.012
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.5379877
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.5060896             0.5696555             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.5367232
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5032294             0.5699717             0
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
## [1] "    indep_vars: biddable, startprice.diff"
## Fitting cp = 0 on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.148888889      1 0.4888889
## 3 0.005333333      2 0.3400000
## 4 0.001481481     11 0.2844444
## 5 0.000000000     14 0.2800000
## 
## Variable importance
##        biddable startprice.diff 
##              51              49 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable        < 0.5       to the left,  improve=144.14990, (0 missing)
##       startprice.diff < 44.18097  to the right, improve= 78.49785, (0 missing)
##   Surrogate splits:
##       startprice.diff < 244.0148  to the left,  agree=0.548, adj=0.022, (0 split)
## 
## Node number 2: 524 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
##   left son=4 (241 obs) right son=5 (283 obs)
##   Primary splits:
##       startprice.diff < 18.6704   to the right, improve=11.698, (0 missing)
## 
## Node number 3: 450 observations,    complexity param=0.1488889
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (89 obs) right son=7 (361 obs)
##   Primary splits:
##       startprice.diff < 84.72197  to the right, improve=88.61445, (0 missing)
## 
## Node number 4: 241 observations
##   predicted class=N  expected loss=0.09543568  P(node) =0.2474333
##     class counts:   218    23
##    probabilities: 0.905 0.095 
## 
## Node number 5: 283 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.3074205  P(node) =0.2905544
##     class counts:   196    87
##    probabilities: 0.693 0.307 
##   left son=10 (128 obs) right son=11 (155 obs)
##   Primary splits:
##       startprice.diff < -16.96598 to the left,  improve=9.605911, (0 missing)
## 
## Node number 6: 89 observations
##   predicted class=N  expected loss=0.1235955  P(node) =0.09137577
##     class counts:    78    11
##    probabilities: 0.876 0.124 
## 
## Node number 7: 361 observations,    complexity param=0.001481481
##   predicted class=Y  expected loss=0.08864266  P(node) =0.3706366
##     class counts:    32   329
##    probabilities: 0.089 0.911 
##   left son=14 (47 obs) right son=15 (314 obs)
##   Primary splits:
##       startprice.diff < 41.70308  to the right, improve=8.057863, (0 missing)
## 
## Node number 10: 128 observations
##   predicted class=N  expected loss=0.1640625  P(node) =0.1314168
##     class counts:   107    21
##    probabilities: 0.836 0.164 
## 
## Node number 11: 155 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.4258065  P(node) =0.1591376
##     class counts:    89    66
##    probabilities: 0.574 0.426 
##   left son=22 (49 obs) right son=23 (106 obs)
##   Primary splits:
##       startprice.diff < 5.201847  to the right, improve=5.807796, (0 missing)
## 
## Node number 14: 47 observations,    complexity param=0.001481481
##   predicted class=Y  expected loss=0.3617021  P(node) =0.04825462
##     class counts:    17    30
##    probabilities: 0.362 0.638 
##   left son=28 (40 obs) right son=29 (7 obs)
##   Primary splits:
##       startprice.diff < 76.69886  to the left,  improve=0.7878419, (0 missing)
## 
## Node number 15: 314 observations
##   predicted class=Y  expected loss=0.0477707  P(node) =0.3223819
##     class counts:    15   299
##    probabilities: 0.048 0.952 
## 
## Node number 22: 49 observations
##   predicted class=N  expected loss=0.2244898  P(node) =0.05030801
##     class counts:    38    11
##    probabilities: 0.776 0.224 
## 
## Node number 23: 106 observations,    complexity param=0.005333333
##   predicted class=Y  expected loss=0.4811321  P(node) =0.1088296
##     class counts:    51    55
##    probabilities: 0.481 0.519 
##   left son=46 (94 obs) right son=47 (12 obs)
##   Primary splits:
##       startprice.diff < 2.233528  to the left,  improve=1.445805, (0 missing)
## 
## Node number 28: 40 observations,    complexity param=0.001481481
##   predicted class=Y  expected loss=0.4  P(node) =0.04106776
##     class counts:    16    24
##    probabilities: 0.400 0.600 
##   left son=56 (10 obs) right son=57 (30 obs)
##   Primary splits:
##       startprice.diff < 66.78261  to the right, improve=1.066667, (0 missing)
## 
## Node number 29: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.007186858
##     class counts:     1     6
##    probabilities: 0.143 0.857 
## 
## Node number 46: 94 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.4893617  P(node) =0.09650924
##     class counts:    48    46
##    probabilities: 0.511 0.489 
##   left son=92 (80 obs) right son=93 (14 obs)
##   Primary splits:
##       startprice.diff < -13.80023 to the right, improve=1.664438, (0 missing)
## 
## Node number 47: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.01232033
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 56: 10 observations
##   predicted class=N  expected loss=0.4  P(node) =0.01026694
##     class counts:     6     4
##    probabilities: 0.600 0.400 
## 
## Node number 57: 30 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.03080082
##     class counts:    10    20
##    probabilities: 0.333 0.667 
## 
## Node number 92: 80 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.45  P(node) =0.08213552
##     class counts:    44    36
##    probabilities: 0.550 0.450 
##   left son=184 (9 obs) right son=185 (71 obs)
##   Primary splits:
##       startprice.diff < 0.8402056 to the right, improve=1.052269, (0 missing)
## 
## Node number 93: 14 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.01437372
##     class counts:     4    10
##    probabilities: 0.286 0.714 
## 
## Node number 184: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.009240246
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 185: 71 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.4788732  P(node) =0.07289528
##     class counts:    37    34
##    probabilities: 0.521 0.479 
##   left son=370 (48 obs) right son=371 (23 obs)
##   Primary splits:
##       startprice.diff < -3.169162 to the left,  improve=2.043504, (0 missing)
## 
## Node number 370: 48 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.3958333  P(node) =0.04928131
##     class counts:    29    19
##    probabilities: 0.604 0.396 
##   left son=740 (19 obs) right son=741 (29 obs)
##   Primary splits:
##       startprice.diff < -7.269784 to the right, improve=1.107154, (0 missing)
## 
## Node number 371: 23 observations
##   predicted class=Y  expected loss=0.3478261  P(node) =0.02361396
##     class counts:     8    15
##    probabilities: 0.348 0.652 
## 
## Node number 740: 19 observations
##   predicted class=N  expected loss=0.2631579  P(node) =0.01950719
##     class counts:    14     5
##    probabilities: 0.737 0.263 
## 
## Node number 741: 29 observations,    complexity param=0.005333333
##   predicted class=N  expected loss=0.4827586  P(node) =0.02977413
##     class counts:    15    14
##    probabilities: 0.517 0.483 
##   left son=1482 (17 obs) right son=1483 (12 obs)
##   Primary splits:
##       startprice.diff < -9.86028  to the left,  improve=2.923935, (0 missing)
## 
## Node number 1482: 17 observations
##   predicted class=N  expected loss=0.2941176  P(node) =0.0174538
##     class counts:    12     5
##    probabilities: 0.706 0.294 
## 
## Node number 1483: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.01232033
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##    1) root 974 450 N (0.53798768 0.46201232)  
##      2) biddable< 0.5 524 110 N (0.79007634 0.20992366)  
##        4) startprice.diff>=18.6704 241  23 N (0.90456432 0.09543568) *
##        5) startprice.diff< 18.6704 283  87 N (0.69257951 0.30742049)  
##         10) startprice.diff< -16.96598 128  21 N (0.83593750 0.16406250) *
##         11) startprice.diff>=-16.96598 155  66 N (0.57419355 0.42580645)  
##           22) startprice.diff>=5.201847 49  11 N (0.77551020 0.22448980) *
##           23) startprice.diff< 5.201847 106  51 Y (0.48113208 0.51886792)  
##             46) startprice.diff< 2.233528 94  46 N (0.51063830 0.48936170)  
##               92) startprice.diff>=-13.80023 80  36 N (0.55000000 0.45000000)  
##                184) startprice.diff>=0.8402056 9   2 N (0.77777778 0.22222222) *
##                185) startprice.diff< 0.8402056 71  34 N (0.52112676 0.47887324)  
##                  370) startprice.diff< -3.169162 48  19 N (0.60416667 0.39583333)  
##                    740) startprice.diff>=-7.269784 19   5 N (0.73684211 0.26315789) *
##                    741) startprice.diff< -7.269784 29  14 N (0.51724138 0.48275862)  
##                     1482) startprice.diff< -9.86028 17   5 N (0.70588235 0.29411765) *
##                     1483) startprice.diff>=-9.86028 12   3 Y (0.25000000 0.75000000) *
##                  371) startprice.diff>=-3.169162 23   8 Y (0.34782609 0.65217391) *
##               93) startprice.diff< -13.80023 14   4 Y (0.28571429 0.71428571) *
##             47) startprice.diff>=2.233528 12   3 Y (0.25000000 0.75000000) *
##      3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##        6) startprice.diff>=84.72197 89  11 N (0.87640449 0.12359551) *
##        7) startprice.diff< 84.72197 361  32 Y (0.08864266 0.91135734)  
##         14) startprice.diff>=41.70308 47  17 Y (0.36170213 0.63829787)  
##           28) startprice.diff< 76.69886 40  16 Y (0.40000000 0.60000000)  
##             56) startprice.diff>=66.78261 10   4 N (0.60000000 0.40000000) *
##             57) startprice.diff< 66.78261 30  10 Y (0.33333333 0.66666667) *
##           29) startprice.diff>=76.69886 7   1 Y (0.14285714 0.85714286) *
##         15) startprice.diff< 41.70308 314  15 Y (0.04777070 0.95222930) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7218935
## 3        0.2 0.8178054
## 4        0.3 0.8532110
## 5        0.4 0.8532110
## 6        0.5 0.8538283
## 7        0.6 0.8538283
## 8        0.7 0.8232386
## 9        0.8 0.7911803
## 10       0.9 0.7827225
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           480
## 2         Y                                            82
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            44
## 2                                           368
##          Prediction
## Reference   N   Y
##         N 480  44
##         Y  82 368
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.706366e-01   7.382100e-01   8.479228e-01   8.910807e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##  1.081623e-109   9.799455e-04 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7220149
## 3        0.2 0.7877412
## 4        0.3 0.7984791
## 5        0.4 0.7984791
## 6        0.5 0.7994859
## 7        0.6 0.7994859
## 8        0.7 0.7732240
## 9        0.8 0.7747489
## 10       0.9 0.7734488
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           418
## 2         Y                                            99
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            57
## 2                                           311
##          Prediction
## Reference   N   Y
##         N 418  57
##         Y  99 311
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.237288e-01   6.430437e-01   7.969937e-01   8.482877e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   3.292368e-72   1.028446e-03 
##                    model_id model_method                     feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart biddable, startprice.diff
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                       0.48                 0.009
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9051018                    0.6       0.8538283        0.8706366
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8479228             0.8910807       0.73821   0.8812811
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7994859        0.8237288
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7969937             0.8482877     0.6430437
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
## [1] "    indep_vars: biddable, startprice.diff"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00533 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-11.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.148888889      1 0.4888889
## 3 0.005333333      2 0.3400000
## 
## Variable importance
##        biddable startprice.diff 
##              61              39 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable        < 0.5      to the left,  improve=144.14990, (0 missing)
##       startprice.diff < 44.18097 to the right, improve= 78.49785, (0 missing)
##   Surrogate splits:
##       startprice.diff < 244.0148 to the left,  agree=0.548, adj=0.022, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.1488889
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (89 obs) right son=7 (361 obs)
##   Primary splits:
##       startprice.diff < 84.72197 to the right, improve=88.61445, (0 missing)
## 
## Node number 6: 89 observations
##   predicted class=N  expected loss=0.1235955  P(node) =0.09137577
##     class counts:    78    11
##    probabilities: 0.876 0.124 
## 
## Node number 7: 361 observations
##   predicted class=Y  expected loss=0.08864266  P(node) =0.3706366
##     class counts:    32   329
##    probabilities: 0.089 0.911 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.53798768 0.46201232)  
##   2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##   3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##     6) startprice.diff>=84.72197 89  11 N (0.87640449 0.12359551) *
##     7) startprice.diff< 84.72197 361  32 Y (0.08864266 0.91135734) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-13.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6576779
## 4        0.3 0.8113440
## 5        0.4 0.8113440
## 6        0.5 0.8113440
## 7        0.6 0.8113440
## 8        0.7 0.8113440
## 9        0.8 0.8113440
## 10       0.9 0.8113440
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-14.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 492
## 2         Y                                 121
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  32
## 2                                 329
##          Prediction
## Reference   N   Y
##         N 492  32
##         Y 121 329
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.429158e-01   6.795322e-01   8.185193e-01   8.652175e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.699210e-90   1.124184e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6515397
## 4        0.3 0.7934783
## 5        0.4 0.7934783
## 6        0.5 0.7934783
## 7        0.6 0.7934783
## 8        0.7 0.7934783
## 9        0.8 0.7934783
## 10       0.9 0.7934783
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-16.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 441
## 2         Y                                 118
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  34
## 2                                 292
##          Prediction
## Reference   N   Y
##         N 441  34
##         Y 118 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.282486e-01   6.497240e-01   8.017531e-01   8.525367e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.168374e-74   1.671294e-11 
##          model_id model_method                     feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart biddable, startprice.diff               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.057                 0.011   0.8435581
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.9        0.811344        0.8367648
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8185193             0.8652175     0.6685851   0.8273068
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.7934783        0.8282486
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8017531             0.8525367      0.649724
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01585287      0.03187391
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
## [1] "    indep_vars: biddable, startprice.diff"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-17.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-18.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-19.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.5037  -0.6915  -0.2142   0.5728   2.4797  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.240824   0.115783  -10.72   <2e-16 ***
## biddable         3.061046   0.187265   16.35   <2e-16 ***
## startprice.diff -0.013661   0.001273  -10.73   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  862.31  on 971  degrees of freedom
## AIC: 868.31
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6863354
## 3        0.2 0.7383260
## 4        0.3 0.7338877
## 5        0.4 0.7569367
## 6        0.5 0.7710012
## 7        0.6 0.7872340
## 8        0.7 0.7906404
## 9        0.8 0.7675963
## 10       0.9 0.2407767
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-22.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               483
## 2         Y                               129
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                41
## 2                               321
##          Prediction
## Reference   N   Y
##         N 483  41
##         Y 129 321
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.254620e-01   6.439814e-01   8.001376e-01   8.488005e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.085595e-79   2.513186e-11 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6916525
## 3        0.2 0.7364341
## 4        0.3 0.7378190
## 5        0.4 0.7564895
## 6        0.5 0.7804878
## 7        0.6 0.7867550
## 8        0.7 0.7896175
## 9        0.8 0.7369985
## 10       0.9 0.2738589
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-24.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               442
## 2         Y                               121
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                33
## 2                               289
##          Prediction
## Reference   N   Y
##         N 442  33
##         Y 121 289
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.259887e-01   6.448747e-01   7.993726e-01   8.504130e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.992247e-73   2.371969e-12 
##        model_id model_method                     feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm biddable, startprice.diff               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.971                 0.013   0.8706955
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.7       0.7906404        0.7977525
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8001376             0.8488005     0.5911469   0.8658999
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.7896175        0.8259887
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7993726              0.850413     0.6448747    868.3141
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009555267      0.01958033
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
## [1] "    indep_vars: biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-25.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-26.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-27.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.5542  -0.6949  -0.2137   0.5631   2.4811  
## 
## Coefficients:
##                                     Estimate Std. Error z value Pr(>|z|)
## (Intercept)                        -1.241030   0.115820 -10.715  < 2e-16
## biddable                            4.453510   1.409049   3.161  0.00157
## startprice.diff                    -0.013687   0.001286 -10.640  < 2e-16
## `biddable:D.terms.n.post.stop`      0.104993   0.288792   0.364  0.71619
## `biddable:D.TfIdf.sum.post.stem`   -0.005976   0.193806  -0.031  0.97540
## `biddable:D.ratio.nstopwrds.nwrds` -1.275311   1.390968  -0.917  0.35922
## `biddable:D.npnct06.log`           -0.117525   0.879725  -0.134  0.89372
## `biddable:D.nchrs.log`              0.157820   0.900923   0.175  0.86094
## `biddable:D.nwrds.unq.log`         -1.042750   2.906364  -0.359  0.71976
## `biddable:cellular.fctr1`           0.013772   0.317458   0.043  0.96540
## `biddable:cellular.fctrUnknown`    -0.905962   0.366723  -2.470  0.01350
##                                       
## (Intercept)                        ***
## biddable                           ** 
## startprice.diff                    ***
## `biddable:D.terms.n.post.stop`        
## `biddable:D.TfIdf.sum.post.stem`      
## `biddable:D.ratio.nstopwrds.nwrds`    
## `biddable:D.npnct06.log`              
## `biddable:D.nchrs.log`                
## `biddable:D.nwrds.unq.log`            
## `biddable:cellular.fctr1`             
## `biddable:cellular.fctrUnknown`    *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  855.46  on 963  degrees of freedom
## AIC: 877.46
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6868687
## 3        0.2 0.7359155
## 4        0.3 0.7343096
## 5        0.4 0.7577778
## 6        0.5 0.7727797
## 7        0.6 0.7867299
## 8        0.7 0.7915633
## 9        0.8 0.7469553
## 10       0.9 0.3357934
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-30.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         487
## 2         Y                                         131
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          37
## 2                                         319
##          Prediction
## Reference   N   Y
##         N 487  37
##         Y 131 319
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.275154e-01   6.478336e-01   8.022954e-01   8.507367e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   6.487999e-81   7.225130e-13 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6934244
## 3        0.2 0.7357212
## 4        0.3 0.7361111
## 5        0.4 0.7530864
## 6        0.5 0.7793548
## 7        0.6 0.7862797
## 8        0.7 0.7868852
## 9        0.8 0.7321429
## 10       0.9 0.3564356
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-32.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         441
## 2         Y                                         122
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          34
## 2                                         288
##          Prediction
## Reference   N   Y
##         N 441  34
##         Y 122 288
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.237288e-01   6.402627e-01   7.969937e-01   8.482877e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   3.292368e-72   3.270658e-12 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                       feats
## 1 biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       1.05                 0.015
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8716794                    0.7       0.7915633        0.7936467
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8022954             0.8507367     0.5830988   0.8652888
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.7868852        0.8237288
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7969937             0.8482877     0.6402627    877.4626
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01031335      0.02147437
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
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":",
                       feat))
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
## [1] "    indep_vars: biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-33.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-34.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-35.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.89910  -0.59239  -0.07592   0.47930   2.70833  
## 
## Coefficients: (7 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                    4.122e+00  5.798e+00
## biddable                                       3.622e+00  2.542e-01
## D.npnct15.log                                  1.492e+00  8.293e-01
## D.npnct03.log                                 -1.122e-01  1.768e+00
## D.terms.n.stem.stop.Ratio                     -3.245e+00  5.161e+00
## D.ratio.sum.TfIdf.nwrds                        1.915e-02  1.599e-01
## .rnorm                                         6.985e-02  9.892e-02
## D.npnct01.log                                  4.453e-01  5.780e-01
## D.TfIdf.sum.stem.stop.Ratio                   -1.885e+00  3.109e+00
## storage.fctr16                                -1.063e+00  5.572e-01
## storage.fctr32                                -1.360e+00  5.893e-01
## storage.fctr64                                -3.708e-01  5.798e-01
## storage.fctrUnknown                           -1.572e+00  7.351e-01
## D.npnct11.log                                 -6.069e-03  3.512e-01
## D.npnct10.log                                 -2.363e+01  1.344e+03
## D.npnct08.log                                  5.628e-01  6.867e-01
## `prdline.my.fctriPad 1`                        1.268e+00  5.920e-01
## `prdline.my.fctriPad 2`                        5.860e-01  5.897e-01
## `prdline.my.fctriPad 3+`                       1.034e+00  5.655e-01
## prdline.my.fctriPadAir                         1.008e+00  5.642e-01
## prdline.my.fctriPadmini                        8.086e-01  5.527e-01
## `prdline.my.fctriPadmini 2+`                   9.390e-01  5.951e-01
## color.fctrBlack                                9.617e-02  2.669e-01
## color.fctrGold                                 1.636e-01  5.539e-01
## `color.fctrSpace Gray`                        -4.397e-01  3.630e-01
## color.fctrWhite                               -3.523e-01  2.634e-01
## D.npnct06.log                                 -2.066e+00  1.275e+00
## D.npnct28.log                                 -2.079e+00  1.720e+03
## D.npnct12.log                                 -1.090e-01  7.129e-01
## D.npnct09.log                                 -8.845e+00  7.670e+02
## D.ndgts.log                                    8.584e-01  4.531e-01
## cellular.fctr1                                 2.113e-01  2.391e-01
## cellular.fctrUnknown                          -4.450e-01  4.803e-01
## D.npnct14.log                                 -2.088e+00  1.034e+00
## D.terms.n.post.stop                           -5.992e-02  4.828e-02
## D.npnct05.log                                 -4.646e+00  1.724e+00
## `condition.fctrFor parts or not working`       5.820e-02  3.851e-01
## `condition.fctrManufacturer refurbished`       7.966e-01  6.620e-01
## condition.fctrNew                             -1.161e-01  3.242e-01
## `condition.fctrNew other (see details)`        8.446e-01  4.988e-01
## `condition.fctrSeller refurbished`            -5.441e-01  4.277e-01
## idseq.my                                      -5.507e-05  2.153e-04
## startprice.diff                               -1.823e-02  1.702e-03
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.400e+00  7.327e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       6.714e-01  8.318e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       3.358e-01  7.168e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -7.029e-01  7.963e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -8.283e-01  7.271e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.379e+00  7.615e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  5.512e-01  9.556e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -1.466e-01  9.062e-01
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -4.315e-02  9.864e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.487e+01  7.639e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      8.067e-02  7.007e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      6.348e-01  9.685e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     9.981e-01  8.703e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  2.058e-01  9.544e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.773e+00  8.934e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       8.574e-01  1.201e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      9.964e-01  8.034e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -4.034e-01  8.753e-01
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -5.871e-01  9.413e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.841e+00  1.538e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -5.363e-01  1.329e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                     0.711  0.47714    
## biddable                                       14.250  < 2e-16 ***
## D.npnct15.log                                   1.799  0.07205 .  
## D.npnct03.log                                  -0.063  0.94941    
## D.terms.n.stem.stop.Ratio                      -0.629  0.52948    
## D.ratio.sum.TfIdf.nwrds                         0.120  0.90469    
## .rnorm                                          0.706  0.48009    
## D.npnct01.log                                   0.770  0.44108    
## D.TfIdf.sum.stem.stop.Ratio                    -0.606  0.54421    
## storage.fctr16                                 -1.907  0.05651 .  
## storage.fctr32                                 -2.308  0.02100 *  
## storage.fctr64                                 -0.640  0.52247    
## storage.fctrUnknown                            -2.139  0.03245 *  
## D.npnct11.log                                  -0.017  0.98621    
## D.npnct10.log                                  -0.018  0.98598    
## D.npnct08.log                                   0.820  0.41247    
## `prdline.my.fctriPad 1`                         2.141  0.03224 *  
## `prdline.my.fctriPad 2`                         0.994  0.32035    
## `prdline.my.fctriPad 3+`                        1.828  0.06748 .  
## prdline.my.fctriPadAir                          1.787  0.07399 .  
## prdline.my.fctriPadmini                         1.463  0.14348    
## `prdline.my.fctriPadmini 2+`                    1.578  0.11461    
## color.fctrBlack                                 0.360  0.71855    
## color.fctrGold                                  0.295  0.76775    
## `color.fctrSpace Gray`                         -1.211  0.22583    
## color.fctrWhite                                -1.337  0.18112    
## D.npnct06.log                                  -1.620  0.10513    
## D.npnct28.log                                  -0.001  0.99904    
## D.npnct12.log                                  -0.153  0.87846    
## D.npnct09.log                                  -0.012  0.99080    
## D.ndgts.log                                     1.894  0.05817 .  
## cellular.fctr1                                  0.884  0.37679    
## cellular.fctrUnknown                           -0.926  0.35419    
## D.npnct14.log                                  -2.021  0.04333 *  
## D.terms.n.post.stop                            -1.241  0.21459    
## D.npnct05.log                                  -2.695  0.00703 ** 
## `condition.fctrFor parts or not working`        0.151  0.87986    
## `condition.fctrManufacturer refurbished`        1.203  0.22885    
## condition.fctrNew                              -0.358  0.72030    
## `condition.fctrNew other (see details)`         1.693  0.09043 .  
## `condition.fctrSeller refurbished`             -1.272  0.20334    
## idseq.my                                       -0.256  0.79814    
## startprice.diff                               -10.706  < 2e-16 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       3.276  0.00105 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        0.807  0.41961    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.468  0.63947    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -0.883  0.37744    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -1.139  0.25468    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.811  0.07007 .  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.577  0.56406    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.162  0.87147    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.044  0.96511    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.019  0.98447    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`       0.115  0.90833    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.655  0.51216    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.147  0.25146    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.216  0.82925    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.985  0.04720 *  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.714  0.47546    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.240  0.21486    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.461  0.64489    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.624  0.53280    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.497  0.01252 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.403  0.68667    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  736.82  on 910  degrees of freedom
## AIC: 864.82
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-36.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7302905
## 3        0.2 0.7699443
## 4        0.3 0.7915408
## 5        0.4 0.8061336
## 6        0.5 0.8123570
## 7        0.6 0.8090692
## 8        0.7 0.7950000
## 9        0.8 0.7483176
## 10       0.9 0.5776398
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               455
## 2         Y                                95
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                69
## 2                               355
##          Prediction
## Reference   N   Y
##         N 455  69
##         Y  95 355
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.316222e-01   6.599017e-01   8.066148e-01   8.546053e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.125901e-83   5.091778e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-38.png) ![](ebayipads_spdiff_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7118959
## 3        0.2 0.7371728
## 4        0.3 0.7525892
## 5        0.4 0.7686659
## 6        0.5 0.7713921
## 7        0.6 0.7745358
## 8        0.7 0.7765668
## 9        0.8 0.7251462
## 10       0.9 0.5860927
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               436
## 2         Y                               125
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                39
## 2                               285
##          Prediction
## Reference   N   Y
##         N 436  39
##         Y 125 285
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.146893e-01   6.219426e-01   7.874927e-01   8.397715e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.806781e-67   3.192972e-11 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                           feats
## 1 biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.386                 0.168
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9063401                    0.5        0.812357        0.7854194
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8066148             0.8546053     0.5673801   0.8499512
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.7765668        0.8146893
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7874927             0.8397715     0.6219426    864.8173
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004787468      0.01254642
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          7          0  99.427 125.587   26.16
## 11 fit.models          7          1 125.587      NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor     bgn end elapsed
## 1 fit.models_1_bgn          1          0 129.488  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here"); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
for (model_id_pfx in c("All.X", "All.Interact.X")) {
#model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                                (exclude.as.feat != 1))[, "id"]
    if (model_id_pfx == "All.Interact.X") {
        interact_vars_vctr <- c(
            #"startprice.log", 
            "startprice.diff",             
            "biddable", "idseq.my")
        indep_vars_vctr <- union(setdiff(indep_vars_vctr, interact_vars_vctr),
                                paste(glb_category_var, interact_vars_vctr, sep=".fctr*"))
    }
    indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
    
    #stop(here")
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
}
```

```
##              label step_major step_minor     bgn     end elapsed
## 1 fit.models_1_bgn          1          0 129.488 129.497   0.009
## 2 fit.models_1_glm          2          0 129.497      NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-1.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-2.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -3.05403  -0.56812  -0.05485   0.46862   2.74437  
## 
## Coefficients: (10 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                   -2.787e+03  3.374e+03
## biddable                                       3.725e+00  2.649e-01
## D.ratio.nstopwrds.nwrds                       -1.434e+01  7.540e+00
## D.npnct15.log                                  8.591e-01  9.183e-01
## D.npnct03.log                                  2.848e-01  2.079e+00
## D.terms.n.stem.stop.Ratio                      2.812e+03  3.374e+03
## D.ratio.sum.TfIdf.nwrds                       -3.338e-01  6.613e-01
## .rnorm                                         4.802e-02  1.025e-01
## D.npnct01.log                                  4.255e-01  6.954e-01
## D.TfIdf.sum.stem.stop.Ratio                   -1.203e+01  1.816e+01
## storage.fctr16                                -9.585e-01  5.717e-01
## storage.fctr32                                -1.288e+00  6.034e-01
## storage.fctr64                                -2.697e-01  5.973e-01
## storage.fctrUnknown                           -1.465e+00  7.493e-01
## D.npnct11.log                                  7.255e-02  4.030e-01
## D.npnct10.log                                 -2.464e+01  1.306e+03
## D.TfIdf.sum.post.stop                         -1.565e+00  2.717e+00
## D.TfIdf.sum.post.stem                          1.785e+00  2.826e+00
## D.sum.TfIdf                                           NA         NA
## D.npnct13.log                                  1.070e-01  4.207e-01
## D.npnct08.log                                  8.084e-01  7.614e-01
## `prdline.my.fctriPad 1`                        1.425e+00  6.075e-01
## `prdline.my.fctriPad 2`                        5.629e-01  6.045e-01
## `prdline.my.fctriPad 3+`                       9.299e-01  5.762e-01
## prdline.my.fctriPadAir                         9.884e-01  5.786e-01
## prdline.my.fctriPadmini                        8.337e-01  5.692e-01
## `prdline.my.fctriPadmini 2+`                   9.887e-01  6.081e-01
## color.fctrBlack                                1.453e-01  2.775e-01
## color.fctrGold                                 2.733e-01  5.775e-01
## `color.fctrSpace Gray`                        -3.453e-01  3.782e-01
## color.fctrWhite                               -2.496e-01  2.739e-01
## D.npnct16.log                                  2.823e+00  1.802e+00
## D.nstopwrds.log                                3.654e+00  2.192e+00
## D.npnct24.log                                 -2.732e+00  6.281e+00
## D.npnct06.log                                 -5.186e+00  2.224e+00
## D.npnct28.log                                 -2.985e+00  1.722e+03
## D.nuppr.log                                    1.105e+00  3.441e+00
## D.nchrs.log                                   -2.005e+00  4.149e+00
## D.npnct12.log                                  3.488e-01  8.201e-01
## D.nwrds.log                                   -2.973e-01  3.411e+00
## D.npnct09.log                                 -8.178e+00  7.891e+02
## D.ndgts.log                                    6.629e-01  5.764e-01
## D.terms.n.post.stop.log                        3.181e+03  3.759e+03
## D.nwrds.unq.log                               -3.185e+03  3.759e+03
## D.terms.n.post.stem.log                               NA         NA
## `carrier.fctrAT&T`                            -6.102e-01  7.651e-01
## carrier.fctrOther                              1.337e+01  1.685e+03
## carrier.fctrSprint                             4.735e-01  1.053e+00
## `carrier.fctrT-Mobile`                        -7.246e-01  1.275e+00
## carrier.fctrUnknown                           -4.050e-01  5.002e-01
## carrier.fctrVerizon                           -4.584e-02  8.272e-01
## cellular.fctr1                                 6.252e-01  7.047e-01
## cellular.fctrUnknown                                  NA         NA
## D.npnct14.log                                 -2.712e+00  1.168e+00
## D.terms.n.post.stem                            2.197e+01  2.095e+01
## D.terms.n.post.stop                           -2.204e+01  2.090e+01
## D.npnct05.log                                 -4.364e+00  1.823e+00
## `condition.fctrFor parts or not working`      -9.262e-02  4.054e-01
## `condition.fctrManufacturer refurbished`       7.724e-01  6.645e-01
## condition.fctrNew                             -8.073e-02  3.367e-01
## `condition.fctrNew other (see details)`        7.400e-01  5.256e-01
## `condition.fctrSeller refurbished`            -5.621e-01  4.557e-01
## idseq.my                                      -4.934e-05  2.205e-04
## startprice.diff                               -1.901e-02  1.797e-03
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.878e+00  7.988e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       2.434e-01  9.002e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`      -1.871e-02  7.816e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.476e+00  9.489e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -9.525e-01  8.066e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     7.483e-01  8.009e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  2.240e-01  1.025e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -2.815e-01  1.031e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -6.803e-01  1.038e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.521e+01  7.273e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -6.070e-03  7.327e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      2.538e-01  1.032e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`     7.844e-01  9.856e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -3.377e-01  1.038e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -2.068e+00  9.714e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       7.010e-01  1.309e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      7.209e-01  8.821e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -8.235e-01  1.097e+00
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -9.700e-01  9.916e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.034e+00  1.546e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -1.042e+00  1.467e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                    -0.826   0.4089    
## biddable                                       14.064   <2e-16 ***
## D.ratio.nstopwrds.nwrds                        -1.901   0.0573 .  
## D.npnct15.log                                   0.936   0.3495    
## D.npnct03.log                                   0.137   0.8911    
## D.terms.n.stem.stop.Ratio                       0.833   0.4046    
## D.ratio.sum.TfIdf.nwrds                        -0.505   0.6138    
## .rnorm                                          0.468   0.6396    
## D.npnct01.log                                   0.612   0.5406    
## D.TfIdf.sum.stem.stop.Ratio                    -0.662   0.5077    
## storage.fctr16                                 -1.677   0.0936 .  
## storage.fctr32                                 -2.135   0.0328 *  
## storage.fctr64                                 -0.451   0.6516    
## storage.fctrUnknown                            -1.956   0.0505 .  
## D.npnct11.log                                   0.180   0.8571    
## D.npnct10.log                                  -0.019   0.9850    
## D.TfIdf.sum.post.stop                          -0.576   0.5644    
## D.TfIdf.sum.post.stem                           0.631   0.5278    
## D.sum.TfIdf                                        NA       NA    
## D.npnct13.log                                   0.254   0.7992    
## D.npnct08.log                                   1.062   0.2884    
## `prdline.my.fctriPad 1`                         2.345   0.0190 *  
## `prdline.my.fctriPad 2`                         0.931   0.3517    
## `prdline.my.fctriPad 3+`                        1.614   0.1065    
## prdline.my.fctriPadAir                          1.708   0.0876 .  
## prdline.my.fctriPadmini                         1.465   0.1430    
## `prdline.my.fctriPadmini 2+`                    1.626   0.1040    
## color.fctrBlack                                 0.524   0.6005    
## color.fctrGold                                  0.473   0.6361    
## `color.fctrSpace Gray`                         -0.913   0.3613    
## color.fctrWhite                                -0.911   0.3621    
## D.npnct16.log                                   1.566   0.1173    
## D.nstopwrds.log                                 1.667   0.0954 .  
## D.npnct24.log                                  -0.435   0.6636    
## D.npnct06.log                                  -2.332   0.0197 *  
## D.npnct28.log                                  -0.002   0.9986    
## D.nuppr.log                                     0.321   0.7481    
## D.nchrs.log                                    -0.483   0.6288    
## D.npnct12.log                                   0.425   0.6706    
## D.nwrds.log                                    -0.087   0.9305    
## D.npnct09.log                                  -0.010   0.9917    
## D.ndgts.log                                     1.150   0.2501    
## D.terms.n.post.stop.log                         0.846   0.3974    
## D.nwrds.unq.log                                -0.847   0.3969    
## D.terms.n.post.stem.log                            NA       NA    
## `carrier.fctrAT&T`                             -0.798   0.4252    
## carrier.fctrOther                               0.008   0.9937    
## carrier.fctrSprint                              0.449   0.6531    
## `carrier.fctrT-Mobile`                         -0.568   0.5699    
## carrier.fctrUnknown                            -0.810   0.4182    
## carrier.fctrVerizon                            -0.055   0.9558    
## cellular.fctr1                                  0.887   0.3750    
## cellular.fctrUnknown                               NA       NA    
## D.npnct14.log                                  -2.322   0.0202 *  
## D.terms.n.post.stem                             1.049   0.2943    
## D.terms.n.post.stop                            -1.055   0.2916    
## D.npnct05.log                                  -2.393   0.0167 *  
## `condition.fctrFor parts or not working`       -0.228   0.8193    
## `condition.fctrManufacturer refurbished`        1.162   0.2451    
## condition.fctrNew                              -0.240   0.8105    
## `condition.fctrNew other (see details)`         1.408   0.1592    
## `condition.fctrSeller refurbished`             -1.234   0.2174    
## idseq.my                                       -0.224   0.8230    
## startprice.diff                               -10.577   <2e-16 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.351   0.0187 *  
## `prdline.my.fctriPad 1:.clusterid.fctr2`        0.270   0.7869    
## `prdline.my.fctriPad 2:.clusterid.fctr2`       -0.024   0.9809    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.555   0.1199    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -1.181   0.2377    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.934   0.3501    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.218   0.8271    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.273   0.7849    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.655   0.5122    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.021   0.9833    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.008   0.9934    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.246   0.8058    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.796   0.4261    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.325   0.7449    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -2.129   0.0332 *  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.536   0.5923    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.817   0.4138    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.751   0.4528    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.978   0.3280    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        1.963   0.0496 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.710   0.4776    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  713.28  on 892  degrees of freedom
## AIC: 877.28
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-4.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7362270
## 3        0.2 0.7809879
## 4        0.3 0.8004053
## 5        0.4 0.8217391
## 6        0.5 0.8213879
## 7        0.6 0.8162291
## 8        0.7 0.7975000
## 9        0.8 0.7463087
## 10       0.9 0.6142209
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           432                            92
## 2         Y                            72                           378
##          Prediction
## Reference   N   Y
##         N 432  92
##         Y  72 378
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.316222e-01   6.623489e-01   8.066148e-01   8.546053e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.125901e-83   1.379016e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-6.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7172285
## 3        0.2 0.7338877
## 4        0.3 0.7488584
## 5        0.4 0.7629362
## 6        0.5 0.7732997
## 7        0.6 0.7769029
## 8        0.7 0.7738420
## 9        0.8 0.7314949
## 10       0.9 0.5963756
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           419                            56
## 2         Y                           114                           296
##          Prediction
## Reference   N   Y
##         N 419  56
##         Y 114 296
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.079096e-01   6.099603e-01   7.803820e-01   8.333692e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.732450e-64   1.232826e-05 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.571                  0.25
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9130577                    0.4       0.8217391        0.7905476
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8066148             0.8546053     0.5768469   0.8492837
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7769029        0.8079096
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1              0.780382             0.8333692     0.6099603    877.2778
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01356908      0.02894366
##                   label step_major step_minor     bgn    end elapsed
## 2      fit.models_1_glm          2          0 129.497 134.97   5.473
## 3 fit.models_1_bayesglm          3          0 134.970     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
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
## 
## Attaching package: 'Matrix'
## 
## The following object is masked from 'package:tidyr':
## 
##     expand
## 
## Loading required package: lme4
## 
## arm (Version 1.8-6, built: 2015-7-7)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/Kaggle_eBay_iPads
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.93999  -0.59559  -0.09853   0.49955   2.58702  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                    5.017e+00  6.711e+00
## biddable                                       3.531e+00  2.439e-01
## D.ratio.nstopwrds.nwrds                       -2.017e+00  2.438e+00
## D.npnct15.log                                  1.113e+00  8.606e-01
## D.npnct03.log                                  9.030e-03  1.806e+00
## D.terms.n.stem.stop.Ratio                     -3.431e+00  5.543e+00
## D.ratio.sum.TfIdf.nwrds                       -3.469e-01  2.983e-01
## .rnorm                                         4.706e-02  9.810e-02
## D.npnct01.log                                  3.597e-01  5.936e-01
## D.TfIdf.sum.stem.stop.Ratio                   -6.399e-01  3.473e+00
## storage.fctr16                                -7.576e-01  4.864e-01
## storage.fctr32                                -1.060e+00  5.163e-01
## storage.fctr64                                -9.463e-02  5.128e-01
## storage.fctrUnknown                           -1.143e+00  6.441e-01
## D.npnct11.log                                  3.760e-02  3.578e-01
## D.npnct10.log                                 -7.700e+00  7.489e+00
## D.TfIdf.sum.post.stop                          2.641e-02  2.897e-01
## D.TfIdf.sum.post.stem                          6.814e-02  3.057e-01
## D.sum.TfIdf                                    6.814e-02  3.057e-01
## D.npnct13.log                                  2.215e-02  3.520e-01
## D.npnct08.log                                  6.169e-01  7.151e-01
## `prdline.my.fctriPad 1`                        1.004e+00  5.071e-01
## `prdline.my.fctriPad 2`                        3.190e-01  5.101e-01
## `prdline.my.fctriPad 3+`                       6.673e-01  4.809e-01
## prdline.my.fctriPadAir                         6.916e-01  4.801e-01
## prdline.my.fctriPadmini                        5.393e-01  4.733e-01
## `prdline.my.fctriPadmini 2+`                   6.522e-01  5.122e-01
## color.fctrBlack                                1.399e-01  2.622e-01
## color.fctrGold                                 2.539e-01  5.306e-01
## `color.fctrSpace Gray`                        -3.804e-01  3.543e-01
## color.fctrWhite                               -2.719e-01  2.582e-01
## D.npnct16.log                                  2.433e+00  1.578e+00
## D.nstopwrds.log                                5.842e-01  6.570e-01
## D.npnct24.log                                  3.539e-01  2.371e+00
## D.npnct06.log                                 -4.576e+00  1.944e+00
## D.npnct28.log                                 -5.536e-02  2.192e+00
## D.nuppr.log                                    3.458e-02  4.811e-01
## D.nchrs.log                                   -3.289e-02  4.804e-01
## D.npnct12.log                                  2.079e-01  7.355e-01
## D.nwrds.log                                   -3.725e-03  7.730e-01
## D.npnct09.log                                 -1.469e+00  5.409e+00
## D.ndgts.log                                    7.901e-01  4.370e-01
## D.terms.n.post.stop.log                       -2.057e-01  1.029e+00
## D.nwrds.unq.log                               -2.073e-01  1.033e+00
## D.terms.n.post.stem.log                       -2.073e-01  1.033e+00
## `carrier.fctrAT&T`                            -3.393e-01  7.754e-01
## carrier.fctrOther                              5.292e-01  1.875e+00
## carrier.fctrSprint                             6.592e-01  9.635e-01
## `carrier.fctrT-Mobile`                        -4.695e-01  1.063e+00
## carrier.fctrUnknown                           -2.394e-01  7.932e-01
## carrier.fctrVerizon                            1.354e-01  8.093e-01
## cellular.fctr1                                 3.757e-01  7.436e-01
## cellular.fctrUnknown                          -2.856e-01  8.741e-01
## D.npnct14.log                                 -2.319e+00  1.011e+00
## D.terms.n.post.stem                           -1.023e-01  2.087e-01
## D.terms.n.post.stop                           -1.360e-01  2.082e-01
## D.npnct05.log                                 -3.766e+00  1.533e+00
## `condition.fctrFor parts or not working`      -6.015e-02  3.805e-01
## `condition.fctrManufacturer refurbished`       7.234e-01  6.199e-01
## condition.fctrNew                             -9.021e-02  3.189e-01
## `condition.fctrNew other (see details)`        6.658e-01  4.839e-01
## `condition.fctrSeller refurbished`            -4.934e-01  4.176e-01
## idseq.my                                      -8.742e-05  2.107e-04
## startprice.diff                               -1.764e-02  1.644e-03
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.545e+00  6.737e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       3.164e-01  7.453e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       1.204e-01  6.629e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.110e+00  7.639e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -8.374e-01  6.801e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     6.803e-01  7.050e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  3.579e-01  8.466e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -4.908e-01  8.139e-01
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -3.187e-01  8.499e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.886e+00  1.559e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -9.173e-02  6.338e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      1.792e-01  8.554e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     7.424e-01  8.071e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -3.665e-01  8.553e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.000e+00  2.500e+00
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.872e+00  8.345e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       4.642e-01  1.030e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      6.549e-01  7.348e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -4.193e-01  7.946e-01
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -6.988e-01  8.143e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.000e+00  2.500e+00
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.000e+00  2.500e+00
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.000e+00  2.500e+00
## `prdline.my.fctriPad 2:.clusterid.fctr5`       2.569e+00  1.199e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.000e+00  2.500e+00
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.000e+00  2.500e+00
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -7.105e-01  1.170e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.000e+00  2.500e+00
##                                               z value Pr(>|z|)    
## (Intercept)                                     0.748   0.4548    
## biddable                                       14.474   <2e-16 ***
## D.ratio.nstopwrds.nwrds                        -0.827   0.4082    
## D.npnct15.log                                   1.293   0.1960    
## D.npnct03.log                                   0.005   0.9960    
## D.terms.n.stem.stop.Ratio                      -0.619   0.5360    
## D.ratio.sum.TfIdf.nwrds                        -1.163   0.2449    
## .rnorm                                          0.480   0.6314    
## D.npnct01.log                                   0.606   0.5445    
## D.TfIdf.sum.stem.stop.Ratio                    -0.184   0.8538    
## storage.fctr16                                 -1.557   0.1194    
## storage.fctr32                                 -2.053   0.0401 *  
## storage.fctr64                                 -0.185   0.8536    
## storage.fctrUnknown                            -1.775   0.0759 .  
## D.npnct11.log                                   0.105   0.9163    
## D.npnct10.log                                  -1.028   0.3038    
## D.TfIdf.sum.post.stop                           0.091   0.9274    
## D.TfIdf.sum.post.stem                           0.223   0.8236    
## D.sum.TfIdf                                     0.223   0.8236    
## D.npnct13.log                                   0.063   0.9498    
## D.npnct08.log                                   0.863   0.3883    
## `prdline.my.fctriPad 1`                         1.980   0.0477 *  
## `prdline.my.fctriPad 2`                         0.625   0.5317    
## `prdline.my.fctriPad 3+`                        1.388   0.1653    
## prdline.my.fctriPadAir                          1.440   0.1498    
## prdline.my.fctriPadmini                         1.140   0.2545    
## `prdline.my.fctriPadmini 2+`                    1.273   0.2029    
## color.fctrBlack                                 0.534   0.5936    
## color.fctrGold                                  0.478   0.6323    
## `color.fctrSpace Gray`                         -1.074   0.2829    
## color.fctrWhite                                -1.053   0.2922    
## D.npnct16.log                                   1.542   0.1232    
## D.nstopwrds.log                                 0.889   0.3739    
## D.npnct24.log                                   0.149   0.8813    
## D.npnct06.log                                  -2.354   0.0186 *  
## D.npnct28.log                                  -0.025   0.9798    
## D.nuppr.log                                     0.072   0.9427    
## D.nchrs.log                                    -0.068   0.9454    
## D.npnct12.log                                   0.283   0.7774    
## D.nwrds.log                                    -0.005   0.9962    
## D.npnct09.log                                  -0.272   0.7860    
## D.ndgts.log                                     1.808   0.0706 .  
## D.terms.n.post.stop.log                        -0.200   0.8416    
## D.nwrds.unq.log                                -0.201   0.8409    
## D.terms.n.post.stem.log                        -0.201   0.8409    
## `carrier.fctrAT&T`                             -0.438   0.6617    
## carrier.fctrOther                               0.282   0.7778    
## carrier.fctrSprint                              0.684   0.4939    
## `carrier.fctrT-Mobile`                         -0.442   0.6587    
## carrier.fctrUnknown                            -0.302   0.7628    
## carrier.fctrVerizon                             0.167   0.8671    
## cellular.fctr1                                  0.505   0.6134    
## cellular.fctrUnknown                           -0.327   0.7439    
## D.npnct14.log                                  -2.294   0.0218 *  
## D.terms.n.post.stem                            -0.490   0.6241    
## D.terms.n.post.stop                            -0.653   0.5137    
## D.npnct05.log                                  -2.457   0.0140 *  
## `condition.fctrFor parts or not working`       -0.158   0.8744    
## `condition.fctrManufacturer refurbished`        1.167   0.2432    
## condition.fctrNew                              -0.283   0.7773    
## `condition.fctrNew other (see details)`         1.376   0.1689    
## `condition.fctrSeller refurbished`             -1.181   0.2374    
## idseq.my                                       -0.415   0.6783    
## startprice.diff                               -10.731   <2e-16 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.294   0.0218 *  
## `prdline.my.fctriPad 1:.clusterid.fctr2`        0.425   0.6712    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.182   0.8558    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.453   0.1462    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -1.231   0.2182    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.965   0.3345    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.423   0.6725    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.603   0.5465    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.375   0.7077    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -1.210   0.2264    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.145   0.8849    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.210   0.8340    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.920   0.3576    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.429   0.6683    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000   1.0000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -2.243   0.0249 *  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.451   0.6522    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.891   0.3728    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.528   0.5978    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.858   0.3908    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000   1.0000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000   1.0000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000   1.0000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.142   0.0322 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000   1.0000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000   1.0000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.607   0.5438    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000   1.0000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  722.59  on 882  degrees of freedom
## AIC: 906.59
## 
## Number of Fisher Scoring iterations: 16
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7295285
## 3        0.2 0.7746090
## 4        0.3 0.7979798
## 5        0.4 0.8129730
## 6        0.5 0.8164196
## 7        0.6 0.8238095
## 8        0.7 0.7979925
## 9        0.8 0.7563249
## 10       0.9 0.5736677
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                480
## 2         Y                                104
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 44
## 2                                346
##          Prediction
## Reference   N   Y
##         N 480  44
##         Y 104 346
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.480493e-01   6.914291e-01   8.239439e-01   8.700277e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   7.437206e-94   1.236046e-06 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7157314
## 3        0.2 0.7414330
## 4        0.3 0.7542857
## 5        0.4 0.7699758
## 6        0.5 0.7741117
## 7        0.6 0.7830688
## 8        0.7 0.7626886
## 9        0.8 0.7288630
## 10       0.9 0.5617597
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                425
## 2         Y                                114
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 50
## 2                                296
##          Prediction
## Reference   N   Y
##         N 425  50
##         Y 114 296
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.146893e-01   6.233456e-01   7.874927e-01   8.397715e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.806781e-67   8.677732e-07 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.587                 0.365
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9103774                    0.6       0.8238095         0.794666
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8239439             0.8700277     0.5859868   0.8576175
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7830688        0.8146893
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7874927             0.8397715     0.6233456    906.5894
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004364881       0.0108705
##                   label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_bayesglm          3          0 134.970 140.831   5.861
## 4   fit.models_1_glmnet          4          0 140.832      NA      NA
## [1] "fitting model: All.X.glmnet"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
```

```
## Loading required package: glmnet
## Loaded glmnet 2.0-2
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-12.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 0.55, lambda = 0.0544 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: lambda
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-13.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-14.png) 

```
##             Length Class      Mode     
## a0            91   -none-     numeric  
## beta        8281   dgCMatrix  S4       
## df            91   -none-     numeric  
## dim            2   -none-     numeric  
## lambda        91   -none-     numeric  
## dev.ratio     91   -none-     numeric  
## nulldev        1   -none-     numeric  
## npasses        1   -none-     numeric  
## jerr           1   -none-     numeric  
## offset         1   -none-     logical  
## classnames     2   -none-     character
## call           5   -none-     call     
## nobs           1   -none-     numeric  
## lambdaOpt      1   -none-     numeric  
## xNames        91   -none-     character
## problemType    1   -none-     character
## tuneValue      2   data.frame list     
## obsLevels      2   -none-     character
## [1] "min lambda > lambdaOpt:"
##                            (Intercept) 
##                          -8.321452e-01 
##                               biddable 
##                           2.005705e+00 
##                    carrier.fctrUnknown 
##                          -6.162401e-02 
##                   cellular.fctrUnknown 
##                          -1.481842e-01 
##                          D.npnct14.log 
##                          -1.836343e-01 
##                          D.npnct05.log 
##                          -4.512526e-01 
##                               idseq.my 
##                          -2.336508e-05 
##                        startprice.diff 
##                          -6.938104e-03 
## prdline.my.fctriPad 2:.clusterid.fctr5 
##                           2.993735e-01 
## [1] "max lambda < lambdaOpt:"
##                                 (Intercept) 
##                                1.086946e+01 
##                                    biddable 
##                                3.672327e+00 
##                     D.ratio.nstopwrds.nwrds 
##                               -6.385701e+00 
##                               D.npnct15.log 
##                                1.013264e+00 
##                               D.npnct03.log 
##                               -8.840403e-02 
##                   D.terms.n.stem.stop.Ratio 
##                               -4.763888e+00 
##                     D.ratio.sum.TfIdf.nwrds 
##                               -3.753543e-01 
##                                      .rnorm 
##                                5.116733e-02 
##                               D.npnct01.log 
##                                3.536558e-01 
##                 D.TfIdf.sum.stem.stop.Ratio 
##                               -9.004306e-01 
##                              storage.fctr16 
##                               -9.708011e-01 
##                              storage.fctr32 
##                               -1.310240e+00 
##                              storage.fctr64 
##                               -2.903630e-01 
##                         storage.fctrUnknown 
##                               -1.416895e+00 
##                               D.npnct11.log 
##                                2.542187e-02 
##                               D.npnct10.log 
##                               -1.057664e+01 
##                       D.TfIdf.sum.post.stem 
##                                8.295661e-02 
##                                 D.sum.TfIdf 
##                                7.594618e-02 
##                               D.npnct13.log 
##                               -6.481685e-03 
##                               D.npnct08.log 
##                                7.075630e-01 
##                       prdline.my.fctriPad 1 
##                                1.338232e+00 
##                       prdline.my.fctriPad 2 
##                                5.277989e-01 
##                      prdline.my.fctriPad 3+ 
##                                9.149489e-01 
##                      prdline.my.fctriPadAir 
##                                9.483600e-01 
##                     prdline.my.fctriPadmini 
##                                7.958867e-01 
##                  prdline.my.fctriPadmini 2+ 
##                                9.295808e-01 
##                             color.fctrBlack 
##                                1.514419e-01 
##                              color.fctrGold 
##                                2.389034e-01 
##                        color.fctrSpace Gray 
##                               -3.992837e-01 
##                             color.fctrWhite 
##                               -2.905883e-01 
##                               D.npnct16.log 
##                                2.754961e+00 
##                             D.nstopwrds.log 
##                                1.647224e+00 
##                               D.npnct24.log 
##                               -9.430424e-01 
##                               D.npnct06.log 
##                               -5.104860e+00 
##                               D.npnct28.log 
##                               -6.207069e-01 
##                                 D.nuppr.log 
##                               -1.351991e-04 
##                                 D.nchrs.log 
##                               -2.562800e-01 
##                               D.npnct12.log 
##                                2.742386e-01 
##                                 D.nwrds.log 
##                               -9.089358e-03 
##                               D.npnct09.log 
##                               -1.637804e+00 
##                                 D.ndgts.log 
##                                8.262264e-01 
##                     D.terms.n.post.stop.log 
##                               -8.293690e-01 
##                             D.nwrds.unq.log 
##                               -5.299870e-01 
##                     D.terms.n.post.stem.log 
##                               -2.821183e-01 
##                            carrier.fctrAT&T 
##                               -1.320782e-01 
##                           carrier.fctrOther 
##                                4.437442e+00 
##                          carrier.fctrSprint 
##                                9.716091e-01 
##                        carrier.fctrT-Mobile 
##                               -3.234630e-01 
##                         carrier.fctrVerizon 
##                                3.735786e-01 
##                              cellular.fctr1 
##                                1.544687e-01 
##                        cellular.fctrUnknown 
##                               -4.092341e-01 
##                               D.npnct14.log 
##                               -2.543977e+00 
##                         D.terms.n.post.stop 
##                               -2.377653e-01 
##                               D.npnct05.log 
##                               -4.528947e+00 
##      condition.fctrFor parts or not working 
##                               -1.156302e-01 
##      condition.fctrManufacturer refurbished 
##                                8.330234e-01 
##                           condition.fctrNew 
##                               -7.711747e-02 
##       condition.fctrNew other (see details) 
##                                7.537808e-01 
##            condition.fctrSeller refurbished 
##                               -5.236321e-01 
##                                    idseq.my 
##                               -6.290456e-05 
##                             startprice.diff 
##                               -1.845350e-02 
##     prdline.my.fctrUnknown:.clusterid.fctr2 
##                                1.754541e+00 
##      prdline.my.fctriPad 1:.clusterid.fctr2 
##                                1.792593e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr2 
##                                7.962901e-02 
##     prdline.my.fctriPad 3+:.clusterid.fctr2 
##                               -1.453073e+00 
##     prdline.my.fctriPadAir:.clusterid.fctr2 
##                               -1.001920e+00 
##    prdline.my.fctriPadmini:.clusterid.fctr2 
##                                7.023608e-01 
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                3.486295e-01 
##     prdline.my.fctrUnknown:.clusterid.fctr3 
##                               -3.826176e-01 
##      prdline.my.fctriPad 1:.clusterid.fctr3 
##                               -5.469441e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr3 
##                               -5.841502e+00 
##     prdline.my.fctriPad 3+:.clusterid.fctr3 
##                               -1.025428e-01 
##     prdline.my.fctriPadAir:.clusterid.fctr3 
##                                1.602461e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr3 
##                                7.811551e-01 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                               -5.094387e-01 
##      prdline.my.fctriPad 1:.clusterid.fctr4 
##                               -2.200274e+00 
##      prdline.my.fctriPad 2:.clusterid.fctr4 
##                                5.902645e-01 
##     prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                7.284231e-01 
##     prdline.my.fctriPadAir:.clusterid.fctr4 
##                               -7.137771e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr4 
##                               -9.480595e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr5 
##                                3.140443e+00 
##    prdline.my.fctriPadmini:.clusterid.fctr5 
##                               -1.133474e+00 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-15.png) 

```
##    threshold    f.score
## 1        0.0 0.63202247
## 2        0.1 0.64841499
## 3        0.2 0.68857590
## 4        0.3 0.74787535
## 5        0.4 0.75496689
## 6        0.5 0.77152698
## 7        0.6 0.80484848
## 8        0.7 0.77631579
## 9        0.8 0.23137255
## 10       0.9 0.00443459
## 11       1.0 0.00000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              481
## 2         Y                              118
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               43
## 2                              332
##          Prediction
## Reference   N   Y
##         N 481  43
##         Y 118 332
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.347023e-01   6.635254e-01   8.098576e-01   8.575034e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.696932e-85   5.476134e-09 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-17.png) 

```
##    threshold     f.score
## 1        0.0 0.633204633
## 2        0.1 0.648648649
## 3        0.2 0.694827586
## 4        0.3 0.734358974
## 5        0.4 0.754017305
## 6        0.5 0.778350515
## 7        0.6 0.796221323
## 8        0.7 0.776671408
## 9        0.8 0.248945148
## 10       0.9 0.009708738
## 11       1.0 0.000000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              439
## 2         Y                              115
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               36
## 2                              295
##          Prediction
## Reference   N   Y
##         N 439  36
##         Y 115 295
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.293785e-01   6.523227e-01   8.029439e-01   8.535980e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   2.796139e-75   2.187709e-10 
##       model_id model_method
## 1 All.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      5.217                 0.961
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8773155                    0.6       0.8048485        0.7977588
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8098576             0.8575034     0.5910839   0.8631682
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7962213        0.8293785
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8029439              0.853598     0.6523227
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01547524      0.03049648
##                 label step_major step_minor     bgn     end elapsed
## 4 fit.models_1_glmnet          4          0 140.832 150.108   9.276
## 5  fit.models_1_rpart          5          0 150.108      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00833 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-19.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-20.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.148888889      1 0.4888889
## 3 0.008333333      2 0.3400000
## 
## Variable importance
##                               biddable 
##                                     49 
##                        startprice.diff 
##                                     30 
##                               idseq.my 
##                                     10 
## condition.fctrFor parts or not working 
##                                      3 
##                  prdline.my.fctriPad 1 
##                                      2 
##                  prdline.my.fctriPad 2 
##                                      2 
##                D.ratio.sum.TfIdf.nwrds 
##                                      2 
##                         color.fctrGold 
##                                      1 
##                D.ratio.nstopwrds.nwrds 
##                                      1 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable          < 0.5       to the left,  improve=144.149900, (0 missing)
##       startprice.diff   < 44.18097  to the right, improve= 78.497850, (0 missing)
##       idseq.my          < 905.5     to the right, improve= 38.912990, (0 missing)
##       condition.fctrNew < 0.5       to the right, improve= 10.867390, (0 missing)
##       D.nwrds.unq.log   < 2.138333  to the right, improve=  9.295085, (0 missing)
##   Surrogate splits:
##       idseq.my                               < 869       to the right, agree=0.636, adj=0.211, (0 split)
##       condition.fctrFor parts or not working < 0.5       to the left,  agree=0.563, adj=0.053, (0 split)
##       prdline.my.fctriPad 1                  < 0.5       to the left,  agree=0.559, adj=0.044, (0 split)
##       prdline.my.fctriPad 2                  < 0.5       to the left,  agree=0.553, adj=0.033, (0 split)
##       D.ratio.sum.TfIdf.nwrds                < 0.9315387 to the left,  agree=0.552, adj=0.031, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.1488889
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (89 obs) right son=7 (361 obs)
##   Primary splits:
##       startprice.diff      < 84.72197  to the right, improve=88.614450, (0 missing)
##       idseq.my             < 670.5     to the right, improve=15.110620, (0 missing)
##       condition.fctrNew    < 0.5       to the right, improve= 3.467222, (0 missing)
##       carrier.fctrUnknown  < 0.5       to the right, improve= 3.372762, (0 missing)
##       cellular.fctrUnknown < 0.5       to the right, improve= 2.987781, (0 missing)
##   Surrogate splits:
##       color.fctrGold                              < 0.5       to the right, agree=0.809, adj=0.034, (0 split)
##       D.ratio.nstopwrds.nwrds                     < 0.1380952 to the left,  agree=0.807, adj=0.022, (0 split)
##       prdline.my.fctriPadmini 2+:.clusterid.fctr3 < 0.5       to the right, agree=0.804, adj=0.011, (0 split)
## 
## Node number 6: 89 observations
##   predicted class=N  expected loss=0.1235955  P(node) =0.09137577
##     class counts:    78    11
##    probabilities: 0.876 0.124 
## 
## Node number 7: 361 observations
##   predicted class=Y  expected loss=0.08864266  P(node) =0.3706366
##     class counts:    32   329
##    probabilities: 0.089 0.911 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.53798768 0.46201232)  
##   2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##   3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##     6) startprice.diff>=84.72197 89  11 N (0.87640449 0.12359551) *
##     7) startprice.diff< 84.72197 361  32 Y (0.08864266 0.91135734) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6576779
## 4        0.3 0.8113440
## 5        0.4 0.8113440
## 6        0.5 0.8113440
## 7        0.6 0.8113440
## 8        0.7 0.8113440
## 9        0.8 0.8113440
## 10       0.9 0.8113440
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-22.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      492
## 2         Y                                      121
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       32
## 2                                      329
##          Prediction
## Reference   N   Y
##         N 492  32
##         Y 121 329
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.429158e-01   6.795322e-01   8.185193e-01   8.652175e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.699210e-90   1.124184e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6515397
## 4        0.3 0.7934783
## 5        0.4 0.7934783
## 6        0.5 0.7934783
## 7        0.6 0.7934783
## 8        0.7 0.7934783
## 9        0.8 0.7934783
## 10       0.9 0.7934783
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      441
## 2         Y                                      118
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       34
## 2                                      292
##          Prediction
## Reference   N   Y
##         N 441  34
##         Y 118 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.282486e-01   6.497240e-01   8.017531e-01   8.525367e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.168374e-74   1.671294e-11 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.673                 0.063
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8435581                    0.9        0.811344        0.8347135
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8185193             0.8652175     0.6642538   0.8273068
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.7934783        0.8282486
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8017531             0.8525367      0.649724
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0167898      0.03471308
##                label step_major step_minor     bgn     end elapsed
## 5 fit.models_1_rpart          5          0 150.108 155.583   5.475
## 6    fit.models_1_rf          6          0 155.584      NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
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
## 
## The following object is masked from 'package:gdata':
## 
##     combine
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-24.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 46 on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-25.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-26.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        974   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           1948   matrix     numeric  
## oob.times        974   -none-     numeric  
## classes            2   -none-     character
## importance        90   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            90   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-27.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8571429
## 3        0.2 0.9513742
## 4        0.3 0.9814613
## 5        0.4 0.9988901
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 0.9595376
## 9        0.8 0.8833747
## 10       0.9 0.8157895
## 11       1.0 0.1670061
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-28.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   524
## 2         Y                                    NA
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                    NA
## 2                                   450
##          Prediction
## Reference   N   Y
##         N 524   0
##         Y   0 450
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.962198e-01   1.000000e+00   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##  5.919016e-263            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-29.png) 

```
##    threshold    f.score
## 1        0.0 0.63320463
## 2        0.1 0.71002710
## 3        0.2 0.76530612
## 4        0.3 0.78923767
## 5        0.4 0.78817734
## 6        0.5 0.80733945
## 7        0.6 0.80868385
## 8        0.7 0.80166436
## 9        0.8 0.77187948
## 10       0.9 0.69266771
## 11       1.0 0.09280742
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   446
## 2         Y                                   112
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                    29
## 2                                   298
##          Prediction
## Reference   N   Y
##         N 446  29
##         Y 112 298
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.406780e-01   6.751279e-01   8.148743e-01   8.641887e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.104950e-81   4.997557e-12 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     15.404                 4.801
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.6               1        0.8418803
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962198                     1     0.6791071   0.8876893
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.8086839         0.840678
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8148743             0.8641887     0.6751279
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01707264      0.03596915
##              label step_major step_minor     bgn     end elapsed
## 6  fit.models_1_rf          6          0 155.584 174.447  18.864
## 7 fit.models_1_glm          7          0 174.448      NA      NA
## [1] "fitting model: All.Interact.X.glm"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-30.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-31.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-32.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-33.png) 

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
## -3.2109  -0.5090  -0.0376   0.4264   2.5624  
## 
## Coefficients: (10 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                   -2.386e+03  3.737e+03
## D.ratio.nstopwrds.nwrds                       -1.459e+01  8.478e+00
## D.npnct15.log                                  5.362e-01  9.757e-01
## D.npnct03.log                                 -3.198e-01  2.299e+00
## D.terms.n.stem.stop.Ratio                      2.406e+03  3.736e+03
## D.ratio.sum.TfIdf.nwrds                       -3.172e-01  7.169e-01
## .rnorm                                         1.717e-02  1.094e-01
## D.npnct01.log                                  2.924e-01  7.696e-01
## D.TfIdf.sum.stem.stop.Ratio                   -6.661e+00  2.076e+01
## storage.fctr16                                -8.690e-01  6.335e-01
## storage.fctr32                                -1.333e+00  6.749e-01
## storage.fctr64                                 7.130e-02  6.602e-01
## storage.fctrUnknown                           -1.509e+00  8.082e-01
## D.npnct11.log                                  3.976e-01  4.343e-01
## D.npnct10.log                                 -2.675e+01  2.110e+03
## D.TfIdf.sum.post.stop                         -7.361e-01  3.092e+00
## D.TfIdf.sum.post.stem                          9.763e-01  3.206e+00
## D.sum.TfIdf                                           NA         NA
## D.npnct13.log                                  3.999e-01  4.653e-01
## D.npnct08.log                                  7.605e-01  8.308e-01
## `prdline.my.fctriPad 1`                        1.545e-01  1.293e+00
## `prdline.my.fctriPad 2`                        1.600e-01  1.337e+00
## `prdline.my.fctriPad 3+`                       1.817e+00  1.185e+00
## prdline.my.fctriPadAir                         1.179e+00  1.177e+00
## prdline.my.fctriPadmini                        1.292e+00  1.181e+00
## `prdline.my.fctriPadmini 2+`                   2.729e-01  1.336e+00
## color.fctrBlack                                2.158e-01  3.054e-01
## color.fctrGold                                 1.702e-02  6.170e-01
## `color.fctrSpace Gray`                        -4.507e-01  3.895e-01
## color.fctrWhite                               -2.766e-01  2.916e-01
## D.npnct16.log                                  4.157e+00  2.106e+00
## D.nstopwrds.log                                4.081e+00  2.471e+00
## D.npnct24.log                                  6.497e-01  7.111e+00
## D.npnct06.log                                 -7.284e+00  2.562e+00
## D.npnct28.log                                 -3.428e+00  2.679e+03
## D.nuppr.log                                    2.139e+00  3.951e+00
## D.nchrs.log                                   -3.853e+00  4.735e+00
## D.npnct12.log                                  1.595e-02  8.749e-01
## D.nwrds.log                                   -2.934e-01  3.633e+00
## D.npnct09.log                                 -8.670e+00  1.027e+03
## D.ndgts.log                                    1.286e+00  6.556e-01
## D.terms.n.post.stop.log                        2.716e+03  4.161e+03
## D.nwrds.unq.log                               -2.720e+03  4.161e+03
## D.terms.n.post.stem.log                               NA         NA
## `carrier.fctrAT&T`                            -1.142e+00  8.462e-01
## carrier.fctrOther                              1.319e+01  2.711e+03
## carrier.fctrSprint                            -1.199e-01  1.123e+00
## `carrier.fctrT-Mobile`                        -1.839e+00  1.356e+00
## carrier.fctrUnknown                           -5.792e-01  5.306e-01
## carrier.fctrVerizon                           -4.572e-01  9.079e-01
## cellular.fctr1                                 1.299e+00  7.822e-01
## cellular.fctrUnknown                                  NA         NA
## D.npnct14.log                                 -2.590e+00  1.255e+00
## D.terms.n.post.stem                            1.863e+01  2.292e+01
## D.terms.n.post.stop                           -1.862e+01  2.286e+01
## D.npnct05.log                                 -5.154e+00  1.951e+00
## `condition.fctrFor parts or not working`       3.579e-02  4.456e-01
## `condition.fctrManufacturer refurbished`       5.735e-01  7.083e-01
## condition.fctrNew                              9.478e-03  3.541e-01
## `condition.fctrNew other (see details)`        8.008e-01  5.590e-01
## `condition.fctrSeller refurbished`            -5.208e-01  5.076e-01
## startprice.diff                               -9.449e-03  3.045e-03
## biddable                                       3.664e+00  7.988e-01
## idseq.my                                       2.886e-04  6.684e-04
## `prdline.my.fctriPad 1:startprice.diff`       -6.907e-02  1.928e-02
## `prdline.my.fctriPad 2:startprice.diff`       -1.071e-02  7.195e-03
## `prdline.my.fctriPad 3+:startprice.diff`      -2.432e-02  6.670e-03
## `prdline.my.fctriPadAir:startprice.diff`      -7.676e-03  4.360e-03
## `prdline.my.fctriPadmini:startprice.diff`     -1.663e-02  6.154e-03
## `prdline.my.fctriPadmini 2+:startprice.diff`  -1.459e-02  6.636e-03
## `prdline.my.fctriPad 1:biddable`               2.168e+00  1.286e+00
## `prdline.my.fctriPad 2:biddable`               1.272e+00  1.165e+00
## `prdline.my.fctriPad 3+:biddable`             -2.110e-01  9.892e-01
## `prdline.my.fctriPadAir:biddable`              5.935e-01  9.813e-01
## `prdline.my.fctriPadmini:biddable`            -5.966e-01  9.521e-01
## `prdline.my.fctriPadmini 2+:biddable`          1.223e-01  1.152e+00
## `prdline.my.fctriPad 1:idseq.my`               7.891e-04  8.842e-04
## `prdline.my.fctriPad 2:idseq.my`              -8.965e-04  9.411e-04
## `prdline.my.fctriPad 3+:idseq.my`             -7.111e-04  7.793e-04
## `prdline.my.fctriPadAir:idseq.my`             -8.596e-04  8.126e-04
## `prdline.my.fctriPadmini:idseq.my`            -4.353e-04  7.720e-04
## `prdline.my.fctriPadmini 2+:idseq.my`          4.430e-04  9.053e-04
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.765e+00  8.568e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`      -1.502e-01  9.820e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       1.732e-01  9.036e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.545e+00  9.816e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -1.030e+00  8.597e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     6.071e-01  8.246e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  5.482e-01  1.112e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -7.968e-01  1.091e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -9.577e-01  1.433e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.554e+01  1.154e+03
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      1.053e-01  7.986e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      2.301e-01  1.082e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`     3.990e-01  9.753e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -8.010e-01  1.098e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -2.205e+00  1.412e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.399e+00  1.465e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      7.748e-01  1.021e+00
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -1.236e+00  1.161e+00
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -6.811e-01  9.914e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.776e+00  1.676e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -5.360e-01  1.505e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                    -0.638 0.523238    
## D.ratio.nstopwrds.nwrds                        -1.721 0.085326 .  
## D.npnct15.log                                   0.550 0.582637    
## D.npnct03.log                                  -0.139 0.889355    
## D.terms.n.stem.stop.Ratio                       0.644 0.519660    
## D.ratio.sum.TfIdf.nwrds                        -0.442 0.658176    
## .rnorm                                          0.157 0.875245    
## D.npnct01.log                                   0.380 0.704025    
## D.TfIdf.sum.stem.stop.Ratio                    -0.321 0.748287    
## storage.fctr16                                 -1.372 0.170146    
## storage.fctr32                                 -1.975 0.048212 *  
## storage.fctr64                                  0.108 0.913992    
## storage.fctrUnknown                            -1.868 0.061806 .  
## D.npnct11.log                                   0.916 0.359873    
## D.npnct10.log                                  -0.013 0.989888    
## D.TfIdf.sum.post.stop                          -0.238 0.811800    
## D.TfIdf.sum.post.stem                           0.304 0.760755    
## D.sum.TfIdf                                        NA       NA    
## D.npnct13.log                                   0.859 0.390083    
## D.npnct08.log                                   0.915 0.359977    
## `prdline.my.fctriPad 1`                         0.120 0.904844    
## `prdline.my.fctriPad 2`                         0.120 0.904698    
## `prdline.my.fctriPad 3+`                        1.534 0.124989    
## prdline.my.fctriPadAir                          1.002 0.316454    
## prdline.my.fctriPadmini                         1.095 0.273721    
## `prdline.my.fctriPadmini 2+`                    0.204 0.838152    
## color.fctrBlack                                 0.707 0.479669    
## color.fctrGold                                  0.028 0.977996    
## `color.fctrSpace Gray`                         -1.157 0.247189    
## color.fctrWhite                                -0.949 0.342860    
## D.npnct16.log                                   1.974 0.048354 *  
## D.nstopwrds.log                                 1.651 0.098648 .  
## D.npnct24.log                                   0.091 0.927204    
## D.npnct06.log                                  -2.842 0.004477 ** 
## D.npnct28.log                                  -0.001 0.998979    
## D.nuppr.log                                     0.541 0.588228    
## D.nchrs.log                                    -0.814 0.415730    
## D.npnct12.log                                   0.018 0.985451    
## D.nwrds.log                                    -0.081 0.935622    
## D.npnct09.log                                  -0.008 0.993268    
## D.ndgts.log                                     1.961 0.049849 *  
## D.terms.n.post.stop.log                         0.653 0.513879    
## D.nwrds.unq.log                                -0.654 0.513272    
## D.terms.n.post.stem.log                            NA       NA    
## `carrier.fctrAT&T`                             -1.349 0.177334    
## carrier.fctrOther                               0.005 0.996119    
## carrier.fctrSprint                             -0.107 0.914964    
## `carrier.fctrT-Mobile`                         -1.356 0.175022    
## carrier.fctrUnknown                            -1.092 0.274998    
## carrier.fctrVerizon                            -0.504 0.614534    
## cellular.fctr1                                  1.661 0.096678 .  
## cellular.fctrUnknown                               NA       NA    
## D.npnct14.log                                  -2.064 0.039029 *  
## D.terms.n.post.stem                             0.813 0.416186    
## D.terms.n.post.stop                            -0.815 0.415264    
## D.npnct05.log                                  -2.641 0.008263 ** 
## `condition.fctrFor parts or not working`        0.080 0.935979    
## `condition.fctrManufacturer refurbished`        0.810 0.418133    
## condition.fctrNew                               0.027 0.978648    
## `condition.fctrNew other (see details)`         1.433 0.151986    
## `condition.fctrSeller refurbished`             -1.026 0.304868    
## startprice.diff                                -3.103 0.001915 ** 
## biddable                                        4.586 4.51e-06 ***
## idseq.my                                        0.432 0.665914    
## `prdline.my.fctriPad 1:startprice.diff`        -3.583 0.000339 ***
## `prdline.my.fctriPad 2:startprice.diff`        -1.488 0.136741    
## `prdline.my.fctriPad 3+:startprice.diff`       -3.647 0.000266 ***
## `prdline.my.fctriPadAir:startprice.diff`       -1.760 0.078345 .  
## `prdline.my.fctriPadmini:startprice.diff`      -2.702 0.006887 ** 
## `prdline.my.fctriPadmini 2+:startprice.diff`   -2.198 0.027943 *  
## `prdline.my.fctriPad 1:biddable`                1.686 0.091781 .  
## `prdline.my.fctriPad 2:biddable`                1.091 0.275110    
## `prdline.my.fctriPad 3+:biddable`              -0.213 0.831112    
## `prdline.my.fctriPadAir:biddable`               0.605 0.545344    
## `prdline.my.fctriPadmini:biddable`             -0.627 0.530861    
## `prdline.my.fctriPadmini 2+:biddable`           0.106 0.915462    
## `prdline.my.fctriPad 1:idseq.my`                0.892 0.372156    
## `prdline.my.fctriPad 2:idseq.my`               -0.953 0.340784    
## `prdline.my.fctriPad 3+:idseq.my`              -0.912 0.361548    
## `prdline.my.fctriPadAir:idseq.my`              -1.058 0.290106    
## `prdline.my.fctriPadmini:idseq.my`             -0.564 0.572845    
## `prdline.my.fctriPadmini 2+:idseq.my`           0.489 0.624638    
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.060 0.039374 *  
## `prdline.my.fctriPad 1:.clusterid.fctr2`       -0.153 0.878405    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.192 0.847964    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.574 0.115406    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -1.198 0.230997    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.736 0.461598    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.493 0.622089    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.730 0.465393    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.668 0.503902    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.013 0.989258    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`       0.132 0.895134    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.213 0.831578    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.409 0.682461    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.729 0.465721    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.562 0.118230    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.954 0.339887    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.759 0.448125    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -1.065 0.286754    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.687 0.492117    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.253 0.024247 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.356 0.721696    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  653.29  on 874  degrees of freedom
## AIC: 853.29
## 
## Number of Fisher Scoring iterations: 16
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-34.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-35.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7610009
## 3        0.2 0.8041825
## 4        0.3 0.8212462
## 5        0.4 0.8396125
## 6        0.5 0.8331442
## 7        0.6 0.8335301
## 8        0.7 0.8099379
## 9        0.8 0.7700394
## 10       0.9 0.6460177
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.glm.N
## 1         N                                    435
## 2         Y                                     60
##   sold.fctr.predict.All.Interact.X.glm.Y
## 1                                     89
## 2                                    390
##          Prediction
## Reference   N   Y
##         N 435  89
##         Y  60 390
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.470226e-01   6.936628e-01   8.228583e-01   8.690663e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.548086e-93   2.179915e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-36.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7247796
## 3        0.2 0.7459459
## 4        0.3 0.7560694
## 5        0.4 0.7733333
## 6        0.5 0.7878788
## 7        0.6 0.7894737
## 8        0.7 0.7711172
## 9        0.8 0.7341040
## 10       0.9 0.6368000
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-38.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.glm.N
## 1         N                                    425
## 2         Y                                    110
##   sold.fctr.predict.All.Interact.X.glm.Y
## 1                                     50
## 2                                    300
##          Prediction
## Reference   N   Y
##         N 425  50
##         Y 110 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.192090e-01   6.327801e-01   7.922402e-01   8.440325e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   8.195907e-70   3.095797e-06 
##             model_id model_method
## 1 All.Interact.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.836                 0.373
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9285072                    0.4       0.8396125        0.7844033
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8228583             0.8690663     0.5662735    0.855923
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7894737         0.819209
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7922402             0.8440325     0.6327801    853.2895
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03020935      0.06241819
##                   label step_major step_minor     bgn    end elapsed
## 7      fit.models_1_glm          7          0 174.448 180.31   5.862
## 8 fit.models_1_bayesglm          8          0 180.311     NA      NA
## [1] "fitting model: All.Interact.X.bayesglm"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -3.05377  -0.54115  -0.08022   0.46910   2.38376  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                    1.856e+00  7.035e+00
## D.ratio.nstopwrds.nwrds                       -1.672e+00  2.446e+00
## D.npnct15.log                                  6.491e-01  9.140e-01
## D.npnct03.log                                 -7.968e-01  1.891e+00
## D.terms.n.stem.stop.Ratio                     -7.348e-01  5.847e+00
## D.ratio.sum.TfIdf.nwrds                       -3.077e-01  3.047e-01
## .rnorm                                         2.644e-02  1.035e-01
## D.npnct01.log                                  2.114e-01  6.478e-01
## D.TfIdf.sum.stem.stop.Ratio                   -4.473e-01  3.662e+00
## storage.fctr16                                -6.947e-01  5.185e-01
## storage.fctr32                                -1.092e+00  5.559e-01
## storage.fctr64                                 1.482e-01  5.468e-01
## storage.fctrUnknown                           -1.161e+00  6.701e-01
## D.npnct11.log                                  1.990e-01  3.768e-01
## D.npnct10.log                                 -8.147e+00  7.369e+00
## D.TfIdf.sum.post.stop                          4.349e-02  2.914e-01
## D.TfIdf.sum.post.stem                          6.711e-02  3.064e-01
## D.sum.TfIdf                                    6.711e-02  3.064e-01
## D.npnct13.log                                  1.670e-01  3.724e-01
## D.npnct08.log                                  4.470e-01  7.670e-01
## `prdline.my.fctriPad 1`                       -7.310e-02  8.246e-01
## `prdline.my.fctriPad 2`                        2.891e-02  8.408e-01
## `prdline.my.fctriPad 3+`                       1.289e+00  7.401e-01
## prdline.my.fctriPadAir                         7.113e-01  7.309e-01
## prdline.my.fctriPadmini                        7.826e-01  7.464e-01
## `prdline.my.fctriPadmini 2+`                  -1.796e-01  8.493e-01
## color.fctrBlack                                1.603e-01  2.806e-01
## color.fctrGold                                 3.446e-03  5.564e-01
## `color.fctrSpace Gray`                        -4.856e-01  3.629e-01
## color.fctrWhite                               -3.166e-01  2.702e-01
## D.npnct16.log                                  3.245e+00  1.813e+00
## D.nstopwrds.log                                5.878e-01  6.650e-01
## D.npnct24.log                                  9.335e-01  2.451e+00
## D.npnct06.log                                 -5.641e+00  2.155e+00
## D.npnct28.log                                 -6.571e-02  2.189e+00
## D.nuppr.log                                    1.100e-03  4.852e-01
## D.nchrs.log                                   -8.520e-02  4.869e-01
## D.npnct12.log                                 -7.261e-02  7.655e-01
## D.nwrds.log                                   -6.485e-02  7.787e-01
## D.npnct09.log                                 -1.468e+00  5.527e+00
## D.ndgts.log                                    1.213e+00  4.751e-01
## D.terms.n.post.stop.log                       -2.345e-01  1.038e+00
## D.nwrds.unq.log                               -2.349e-01  1.041e+00
## D.terms.n.post.stem.log                       -2.349e-01  1.041e+00
## `carrier.fctrAT&T`                            -3.693e-01  7.938e-01
## carrier.fctrOther                              4.414e-01  1.925e+00
## carrier.fctrSprint                             6.126e-01  9.811e-01
## `carrier.fctrT-Mobile`                        -9.110e-01  1.094e+00
## carrier.fctrUnknown                           -5.054e-03  8.158e-01
## carrier.fctrVerizon                            2.202e-01  8.307e-01
## cellular.fctr1                                 5.067e-01  7.570e-01
## cellular.fctrUnknown                          -5.872e-01  9.022e-01
## D.npnct14.log                                 -2.197e+00  1.069e+00
## D.terms.n.post.stem                           -9.215e-02  2.074e-01
## D.terms.n.post.stop                           -1.183e-01  2.064e-01
## D.npnct05.log                                 -4.422e+00  1.613e+00
## `condition.fctrFor parts or not working`       4.222e-02  4.071e-01
## `condition.fctrManufacturer refurbished`       5.571e-01  6.472e-01
## condition.fctrNew                             -2.090e-02  3.324e-01
## `condition.fctrNew other (see details)`        7.391e-01  5.066e-01
## `condition.fctrSeller refurbished`            -4.597e-01  4.602e-01
## startprice.diff                               -8.917e-03  2.522e-03
## biddable                                       3.400e+00  5.238e-01
## idseq.my                                      -4.535e-06  4.538e-04
## `prdline.my.fctriPad 1:startprice.diff`       -5.851e-02  1.635e-02
## `prdline.my.fctriPad 2:startprice.diff`       -9.755e-03  6.450e-03
## `prdline.my.fctriPad 3+:startprice.diff`      -2.229e-02  5.823e-03
## `prdline.my.fctriPadAir:startprice.diff`      -6.944e-03  3.739e-03
## `prdline.my.fctriPadmini:startprice.diff`     -1.584e-02  5.655e-03
## `prdline.my.fctriPadmini 2+:startprice.diff`  -1.247e-02  5.602e-03
## `prdline.my.fctriPad 1:biddable`               1.620e+00  8.937e-01
## `prdline.my.fctriPad 2:biddable`               1.004e+00  8.199e-01
## `prdline.my.fctriPad 3+:biddable`             -1.235e-01  7.072e-01
## `prdline.my.fctriPadAir:biddable`              6.430e-01  7.028e-01
## `prdline.my.fctriPadmini:biddable`            -4.107e-01  6.843e-01
## `prdline.my.fctriPadmini 2+:biddable`          2.419e-01  8.251e-01
## `prdline.my.fctriPad 1:idseq.my`               8.186e-04  6.593e-04
## `prdline.my.fctriPad 2:idseq.my`              -6.048e-04  7.163e-04
## `prdline.my.fctriPad 3+:idseq.my`             -3.408e-04  5.779e-04
## `prdline.my.fctriPadAir:idseq.my`             -5.565e-04  6.021e-04
## `prdline.my.fctriPadmini:idseq.my`            -1.389e-04  5.730e-04
## `prdline.my.fctriPadmini 2+:idseq.my`          6.848e-04  6.698e-04
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.456e+00  6.841e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       7.438e-02  7.928e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       2.084e-01  7.311e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.141e+00  7.946e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -8.994e-01  7.151e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     5.066e-01  7.071e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  6.345e-01  8.829e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -6.230e-01  8.507e-01
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -5.550e-01  1.037e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.506e+00  1.594e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      3.632e-02  6.731e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      2.203e-01  8.791e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     5.059e-01  7.914e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -6.296e-01  8.834e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.000e+00  2.500e+00
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.676e+00  1.109e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`       8.446e-01  1.119e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      5.405e-01  8.151e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -5.591e-01  8.191e-01
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -4.440e-01  8.072e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.000e+00  2.500e+00
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.000e+00  2.500e+00
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.000e+00  2.500e+00
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.068e+00  1.286e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.000e+00  2.500e+00
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.000e+00  2.500e+00
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -3.539e-01  1.181e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.000e+00  2.500e+00
##                                               z value Pr(>|z|)    
## (Intercept)                                     0.264 0.791888    
## D.ratio.nstopwrds.nwrds                        -0.684 0.494191    
## D.npnct15.log                                   0.710 0.477616    
## D.npnct03.log                                  -0.421 0.673449    
## D.terms.n.stem.stop.Ratio                      -0.126 0.899985    
## D.ratio.sum.TfIdf.nwrds                        -1.010 0.312659    
## .rnorm                                          0.255 0.798398    
## D.npnct01.log                                   0.326 0.744226    
## D.TfIdf.sum.stem.stop.Ratio                    -0.122 0.902775    
## storage.fctr16                                 -1.340 0.180268    
## storage.fctr32                                 -1.964 0.049501 *  
## storage.fctr64                                  0.271 0.786335    
## storage.fctrUnknown                            -1.733 0.083112 .  
## D.npnct11.log                                   0.528 0.597272    
## D.npnct10.log                                  -1.106 0.268898    
## D.TfIdf.sum.post.stop                           0.149 0.881357    
## D.TfIdf.sum.post.stem                           0.219 0.826645    
## D.sum.TfIdf                                     0.219 0.826645    
## D.npnct13.log                                   0.448 0.653858    
## D.npnct08.log                                   0.583 0.560079    
## `prdline.my.fctriPad 1`                        -0.089 0.929362    
## `prdline.my.fctriPad 2`                         0.034 0.972571    
## `prdline.my.fctriPad 3+`                        1.742 0.081594 .  
## prdline.my.fctriPadAir                          0.973 0.330468    
## prdline.my.fctriPadmini                         1.049 0.294387    
## `prdline.my.fctriPadmini 2+`                   -0.211 0.832551    
## color.fctrBlack                                 0.571 0.567806    
## color.fctrGold                                  0.006 0.995059    
## `color.fctrSpace Gray`                         -1.338 0.180828    
## color.fctrWhite                                -1.172 0.241305    
## D.npnct16.log                                   1.790 0.073488 .  
## D.nstopwrds.log                                 0.884 0.376742    
## D.npnct24.log                                   0.381 0.703338    
## D.npnct06.log                                  -2.618 0.008839 ** 
## D.npnct28.log                                  -0.030 0.976051    
## D.nuppr.log                                     0.002 0.998191    
## D.nchrs.log                                    -0.175 0.861078    
## D.npnct12.log                                  -0.095 0.924437    
## D.nwrds.log                                    -0.083 0.933627    
## D.npnct09.log                                  -0.266 0.790532    
## D.ndgts.log                                     2.552 0.010710 *  
## D.terms.n.post.stop.log                        -0.226 0.821237    
## D.nwrds.unq.log                                -0.226 0.821532    
## D.terms.n.post.stem.log                        -0.226 0.821532    
## `carrier.fctrAT&T`                             -0.465 0.641785    
## carrier.fctrOther                               0.229 0.818617    
## carrier.fctrSprint                              0.624 0.532373    
## `carrier.fctrT-Mobile`                         -0.833 0.405083    
## carrier.fctrUnknown                            -0.006 0.995057    
## carrier.fctrVerizon                             0.265 0.790942    
## cellular.fctr1                                  0.669 0.503305    
## cellular.fctrUnknown                           -0.651 0.515153    
## D.npnct14.log                                  -2.056 0.039826 *  
## D.terms.n.post.stem                            -0.444 0.656781    
## D.terms.n.post.stop                            -0.573 0.566623    
## D.npnct05.log                                  -2.741 0.006122 ** 
## `condition.fctrFor parts or not working`        0.104 0.917394    
## `condition.fctrManufacturer refurbished`        0.861 0.389381    
## condition.fctrNew                              -0.063 0.949856    
## `condition.fctrNew other (see details)`         1.459 0.144539    
## `condition.fctrSeller refurbished`             -0.999 0.317863    
## startprice.diff                                -3.536 0.000407 ***
## biddable                                        6.492 8.45e-11 ***
## idseq.my                                       -0.010 0.992028    
## `prdline.my.fctriPad 1:startprice.diff`        -3.578 0.000346 ***
## `prdline.my.fctriPad 2:startprice.diff`        -1.512 0.130424    
## `prdline.my.fctriPad 3+:startprice.diff`       -3.828 0.000129 ***
## `prdline.my.fctriPadAir:startprice.diff`       -1.857 0.063292 .  
## `prdline.my.fctriPadmini:startprice.diff`      -2.801 0.005089 ** 
## `prdline.my.fctriPadmini 2+:startprice.diff`   -2.226 0.026009 *  
## `prdline.my.fctriPad 1:biddable`                1.813 0.069807 .  
## `prdline.my.fctriPad 2:biddable`                1.224 0.220924    
## `prdline.my.fctriPad 3+:biddable`              -0.175 0.861339    
## `prdline.my.fctriPadAir:biddable`               0.915 0.360222    
## `prdline.my.fctriPadmini:biddable`             -0.600 0.548408    
## `prdline.my.fctriPadmini 2+:biddable`           0.293 0.769402    
## `prdline.my.fctriPad 1:idseq.my`                1.242 0.214377    
## `prdline.my.fctriPad 2:idseq.my`               -0.844 0.398499    
## `prdline.my.fctriPad 3+:idseq.my`              -0.590 0.555397    
## `prdline.my.fctriPadAir:idseq.my`              -0.924 0.355383    
## `prdline.my.fctriPadmini:idseq.my`             -0.242 0.808430    
## `prdline.my.fctriPadmini 2+:idseq.my`           1.022 0.306628    
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.128 0.033320 *  
## `prdline.my.fctriPad 1:.clusterid.fctr2`        0.094 0.925248    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.285 0.775566    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.437 0.150826    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -1.258 0.208509    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.716 0.473705    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.719 0.472327    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.732 0.463950    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.535 0.592577    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.945 0.344871    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`       0.054 0.956972    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.251 0.802127    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.639 0.522654    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.713 0.476076    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.512 0.130547    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.755 0.450353    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.663 0.507301    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.683 0.494847    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.550 0.582309    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000 1.000000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000 1.000000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.386 0.017042 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.300 0.764378    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000 1.000000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  663.15  on 864  degrees of freedom
## AIC: 883.15
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7506383
## 3        0.2 0.7932011
## 4        0.3 0.8130081
## 5        0.4 0.8304721
## 6        0.5 0.8388571
## 7        0.6 0.8285714
## 8        0.7 0.8119552
## 9        0.8 0.7629139
## 10       0.9 0.6160850
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.bayesglm.N
## 1         N                                         466
## 2         Y                                          83
##   sold.fctr.predict.All.Interact.X.bayesglm.Y
## 1                                          58
## 2                                         367
##          Prediction
## Reference   N   Y
##         N 466  58
##         Y  83 367
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.552361e-01   7.076445e-01   8.315533e-01   8.767468e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.056179e-98   4.326273e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-41.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7265774
## 3        0.2 0.7486744
## 4        0.3 0.7583815
## 5        0.4 0.7729469
## 6        0.5 0.7969543
## 7        0.6 0.7963206
## 8        0.7 0.7879617
## 9        0.8 0.7420290
## 10       0.9 0.5852843
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-42.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.bayesglm.N
## 1         N                                         411
## 2         Y                                          96
##   sold.fctr.predict.All.Interact.X.bayesglm.Y
## 1                                          64
## 2                                         314
##          Prediction
## Reference   N   Y
##         N 411  64
##         Y  96 314
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.192090e-01   6.345052e-01   7.922402e-01   8.440325e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   8.195907e-70   1.425529e-02 
##                  model_id model_method
## 1 All.Interact.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.178                 0.505
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9261281                    0.5       0.8388571        0.8018519
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8315533             0.8767468     0.6010074   0.8673582
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7969543         0.819209
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7922402             0.8440325     0.6345052    883.1471
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01571542       0.0329967
##                   label step_major step_minor     bgn     end elapsed
## 8 fit.models_1_bayesglm          8          0 180.311 185.797   5.487
## 9   fit.models_1_glmnet          9          0 185.798      NA      NA
## [1] "fitting model: All.Interact.X.glmnet"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 1, lambda = 0.00544 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: alpha
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-43.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-44.png) 

```
##             Length Class      Mode     
## a0            84   -none-     numeric  
## beta        9156   dgCMatrix  S4       
## df            84   -none-     numeric  
## dim            2   -none-     numeric  
## lambda        84   -none-     numeric  
## dev.ratio     84   -none-     numeric  
## nulldev        1   -none-     numeric  
## npasses        1   -none-     numeric  
## jerr           1   -none-     numeric  
## offset         1   -none-     logical  
## classnames     2   -none-     character
## call           5   -none-     call     
## nobs           1   -none-     numeric  
## lambdaOpt      1   -none-     numeric  
## xNames       109   -none-     character
## problemType    1   -none-     character
## tuneValue      2   data.frame list     
## obsLevels      2   -none-     character
## [1] "min lambda > lambdaOpt:"
##                                (Intercept) 
##                              -1.071851e+00 
##                              D.npnct15.log 
##                               4.266444e-01 
##                                     .rnorm 
##                               1.214892e-03 
##                              D.npnct01.log 
##                               2.808387e-03 
##                             storage.fctr32 
##                              -2.105252e-01 
##                             storage.fctr64 
##                               5.311066e-01 
##                        storage.fctrUnknown 
##                              -3.327280e-01 
##                              D.npnct10.log 
##                              -2.018629e+00 
##                      D.TfIdf.sum.post.stem 
##                               4.720688e-02 
##                                D.sum.TfIdf 
##                               1.120480e-06 
##                     prdline.my.fctriPad 3+ 
##                               3.417411e-01 
##                            color.fctrBlack 
##                               5.535054e-02 
##                       color.fctrSpace Gray 
##                              -2.672569e-01 
##                            color.fctrWhite 
##                              -1.809478e-01 
##                              D.npnct06.log 
##                              -1.117156e+00 
##                                D.ndgts.log 
##                               2.439055e-01 
##                         carrier.fctrSprint 
##                               6.559722e-01 
##                       carrier.fctrT-Mobile 
##                              -6.159378e-02 
##                        carrier.fctrVerizon 
##                               2.519349e-01 
##                             cellular.fctr1 
##                               3.291090e-02 
##                       cellular.fctrUnknown 
##                              -7.017078e-01 
##                              D.npnct14.log 
##                              -1.066797e+00 
##                        D.terms.n.post.stem 
##                              -1.874865e-02 
##                        D.terms.n.post.stop 
##                              -1.295554e-02 
##                              D.npnct05.log 
##                              -2.338872e+00 
##     condition.fctrManufacturer refurbished 
##                               2.620472e-01 
##                          condition.fctrNew 
##                              -4.809865e-02 
##      condition.fctrNew other (see details) 
##                               5.153368e-01 
##           condition.fctrSeller refurbished 
##                              -2.045123e-01 
##                            startprice.diff 
##                              -9.922903e-03 
##                                   biddable 
##                               3.014829e+00 
##                                   idseq.my 
##                              -6.267334e-05 
##      prdline.my.fctriPad 1:startprice.diff 
##                              -3.474805e-02 
##      prdline.my.fctriPad 2:startprice.diff 
##                              -3.560722e-03 
##     prdline.my.fctriPad 3+:startprice.diff 
##                              -1.286821e-02 
##     prdline.my.fctriPadAir:startprice.diff 
##                              -2.192992e-03 
##    prdline.my.fctriPadmini:startprice.diff 
##                              -1.167589e-02 
## prdline.my.fctriPadmini 2+:startprice.diff 
##                              -5.030936e-03 
##             prdline.my.fctriPad 1:biddable 
##                               6.332994e-01 
##             prdline.my.fctriPad 2:biddable 
##                               3.204299e-01 
##            prdline.my.fctriPadAir:biddable 
##                               3.665535e-01 
##             prdline.my.fctriPad 1:idseq.my 
##                               4.511012e-04 
##             prdline.my.fctriPad 2:idseq.my 
##                              -2.389369e-04 
##            prdline.my.fctriPadAir:idseq.my 
##                              -5.458154e-05 
##        prdline.my.fctriPadmini 2+:idseq.my 
##                               7.910002e-05 
##    prdline.my.fctrUnknown:.clusterid.fctr2 
##                               9.185778e-01 
##     prdline.my.fctriPad 1:.clusterid.fctr2 
##                               1.515635e-01 
##    prdline.my.fctriPad 3+:.clusterid.fctr2 
##                              -3.072119e-01 
##    prdline.my.fctriPadAir:.clusterid.fctr2 
##                              -6.552784e-01 
##   prdline.my.fctriPadmini:.clusterid.fctr2 
##                               5.893832e-01 
##    prdline.my.fctrUnknown:.clusterid.fctr3 
##                              -7.160406e-01 
##     prdline.my.fctriPad 2:.clusterid.fctr3 
##                              -1.709696e+00 
##   prdline.my.fctriPadmini:.clusterid.fctr3 
##                               5.629901e-01 
##     prdline.my.fctriPad 1:.clusterid.fctr4 
##                              -1.005797e+00 
##    prdline.my.fctriPad 3+:.clusterid.fctr4 
##                               3.451929e-01 
##   prdline.my.fctriPadmini:.clusterid.fctr4 
##                              -1.281276e-01 
##     prdline.my.fctriPad 2:.clusterid.fctr5 
##                               2.647768e+00 
## [1] "max lambda < lambdaOpt:"
##                                 (Intercept) 
##                                7.078316e+00 
##                     D.ratio.nstopwrds.nwrds 
##                               -7.168457e+00 
##                               D.npnct15.log 
##                                5.690381e-01 
##                               D.npnct03.log 
##                               -9.108520e-01 
##                   D.terms.n.stem.stop.Ratio 
##                               -8.929569e-02 
##                     D.ratio.sum.TfIdf.nwrds 
##                               -3.361056e-01 
##                                      .rnorm 
##                                2.595797e-02 
##                               D.npnct01.log 
##                                1.643323e-01 
##                 D.TfIdf.sum.stem.stop.Ratio 
##                               -9.313161e-01 
##                              storage.fctr16 
##                               -8.878343e-01 
##                              storage.fctr32 
##                               -1.361324e+00 
##                         storage.fctrUnknown 
##                               -1.451838e+00 
##                               D.npnct11.log 
##                                2.391393e-01 
##                               D.npnct10.log 
##                               -1.094287e+01 
##                       D.TfIdf.sum.post.stem 
##                                1.816461e-01 
##                                 D.sum.TfIdf 
##                                5.320462e-03 
##                               D.npnct13.log 
##                                1.709621e-01 
##                               D.npnct08.log 
##                                5.605765e-01 
##                       prdline.my.fctriPad 2 
##                                1.802617e-02 
##                      prdline.my.fctriPad 3+ 
##                                1.651397e+00 
##                      prdline.my.fctriPadAir 
##                                1.005799e+00 
##                     prdline.my.fctriPadmini 
##                                1.074416e+00 
##                             color.fctrBlack 
##                                1.969397e-01 
##                              color.fctrGold 
##                               -2.622015e-02 
##                        color.fctrSpace Gray 
##                               -5.026492e-01 
##                             color.fctrWhite 
##                               -3.232513e-01 
##                               D.npnct16.log 
##                                3.884682e+00 
##                             D.nstopwrds.log 
##                                1.972564e+00 
##                               D.npnct24.log 
##                                3.156937e-02 
##                               D.npnct06.log 
##                               -6.718634e+00 
##                               D.npnct28.log 
##                               -5.867155e-01 
##                                 D.nchrs.log 
##                               -6.023664e-01 
##                               D.npnct12.log 
##                               -4.897941e-02 
##                               D.npnct09.log 
##                               -1.296182e+00 
##                                 D.ndgts.log 
##                                1.318937e+00 
##                             D.nwrds.unq.log 
##                               -2.161924e+00 
##                     D.terms.n.post.stem.log 
##                               -1.970537e-16 
##                            carrier.fctrAT&T 
##                               -4.735255e-01 
##                           carrier.fctrOther 
##                                3.122673e+00 
##                          carrier.fctrSprint 
##                                5.624587e-01 
##                        carrier.fctrT-Mobile 
##                               -1.219579e+00 
##                         carrier.fctrVerizon 
##                                1.705070e-01 
##                              cellular.fctr1 
##                                6.231476e-01 
##                        cellular.fctrUnknown 
##                               -5.515860e-01 
##                               D.npnct14.log 
##                               -2.490804e+00 
##                         D.terms.n.post.stop 
##                               -1.735829e-01 
##                               D.npnct05.log 
##                               -5.280322e+00 
##      condition.fctrFor parts or not working 
##                                9.580821e-03 
##      condition.fctrManufacturer refurbished 
##                                5.961320e-01 
##       condition.fctrNew other (see details) 
##                                8.042263e-01 
##            condition.fctrSeller refurbished 
##                               -4.887843e-01 
##                             startprice.diff 
##                               -9.174971e-03 
##                                    biddable 
##                                3.544086e+00 
##                                    idseq.my 
##                                1.356103e-04 
##       prdline.my.fctriPad 1:startprice.diff 
##                               -6.541284e-02 
##       prdline.my.fctriPad 2:startprice.diff 
##                               -1.024689e-02 
##      prdline.my.fctriPad 3+:startprice.diff 
##                               -2.416299e-02 
##      prdline.my.fctriPadAir:startprice.diff 
##                               -7.486892e-03 
##     prdline.my.fctriPadmini:startprice.diff 
##                               -1.604006e-02 
##  prdline.my.fctriPadmini 2+:startprice.diff 
##                               -1.372973e-02 
##              prdline.my.fctriPad 1:biddable 
##                                2.157679e+00 
##              prdline.my.fctriPad 2:biddable 
##                                1.240603e+00 
##             prdline.my.fctriPad 3+:biddable 
##                               -1.647133e-01 
##             prdline.my.fctriPadAir:biddable 
##                                6.499318e-01 
##            prdline.my.fctriPadmini:biddable 
##                               -5.227249e-01 
##         prdline.my.fctriPadmini 2+:biddable 
##                                2.589720e-01 
##              prdline.my.fctriPad 1:idseq.my 
##                                8.352692e-04 
##              prdline.my.fctriPad 2:idseq.my 
##                               -7.322703e-04 
##             prdline.my.fctriPad 3+:idseq.my 
##                               -5.510381e-04 
##             prdline.my.fctriPadAir:idseq.my 
##                               -7.348302e-04 
##            prdline.my.fctriPadmini:idseq.my 
##                               -2.759999e-04 
##         prdline.my.fctriPadmini 2+:idseq.my 
##                                6.278549e-04 
##     prdline.my.fctrUnknown:.clusterid.fctr2 
##                                1.613582e+00 
##      prdline.my.fctriPad 1:.clusterid.fctr2 
##                               -1.156028e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr2 
##                                2.678246e-01 
##     prdline.my.fctriPad 3+:.clusterid.fctr2 
##                               -1.527531e+00 
##     prdline.my.fctriPadAir:.clusterid.fctr2 
##                               -1.070915e+00 
##    prdline.my.fctriPadmini:.clusterid.fctr2 
##                                5.122784e-01 
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                7.101637e-01 
##     prdline.my.fctrUnknown:.clusterid.fctr3 
##                               -7.060338e-01 
##      prdline.my.fctriPad 1:.clusterid.fctr3 
##                               -9.898297e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr3 
##                               -4.967315e+00 
##     prdline.my.fctriPad 3+:.clusterid.fctr3 
##                                1.697505e-02 
##     prdline.my.fctriPadAir:.clusterid.fctr3 
##                                1.805793e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr3 
##                                4.263241e-01 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                               -8.796400e-01 
##      prdline.my.fctriPad 1:.clusterid.fctr4 
##                               -2.313679e+00 
##      prdline.my.fctriPad 2:.clusterid.fctr4 
##                                1.290711e+00 
##     prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                6.482265e-01 
##     prdline.my.fctriPadAir:.clusterid.fctr4 
##                               -9.678674e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr4 
##                               -6.430547e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr5 
##                                3.826005e+00 
##    prdline.my.fctriPadmini:.clusterid.fctr5 
##                               -6.192454e-01 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-45.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7148594
## 3        0.2 0.7788899
## 4        0.3 0.7928287
## 5        0.4 0.8131148
## 6        0.5 0.8227115
## 7        0.6 0.8140948
## 8        0.7 0.8025316
## 9        0.8 0.7553763
## 10       0.9 0.5123153
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-46.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.glmnet.N
## 1         N                                       466
## 2         Y                                        95
##   sold.fctr.predict.All.Interact.X.glmnet.Y
## 1                                        58
## 2                                       355
##          Prediction
## Reference   N   Y
##         N 466  58
##         Y  95 355
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.429158e-01   6.821623e-01   8.185193e-01   8.652175e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.699210e-90   3.609347e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-47.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7054611
## 3        0.2 0.7454545
## 4        0.3 0.7581552
## 5        0.4 0.7811366
## 6        0.5 0.7861716
## 7        0.6 0.7930108
## 8        0.7 0.7844228
## 9        0.8 0.7318519
## 10       0.9 0.5098039
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-48.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.glmnet.N
## 1         N                                       436
## 2         Y                                       115
##   sold.fctr.predict.All.Interact.X.glmnet.Y
## 1                                        39
## 2                                       295
##          Prediction
## Reference   N   Y
##         N 436  39
##         Y 115 295
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.259887e-01   6.455950e-01   7.993726e-01   8.504130e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.992247e-73   1.506454e-09 
##                model_id model_method
## 1 All.Interact.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                       6.26                  1.46
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9132994                    0.5       0.8227115        0.8038936
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8185193             0.8652175     0.6039602   0.8728883
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7930108        0.8259887
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7993726              0.850413      0.645595
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02105445      0.04398531
##                  label step_major step_minor     bgn     end elapsed
## 9  fit.models_1_glmnet          9          0 185.798 196.112  10.314
## 10  fit.models_1_rpart         10          0 196.112      NA      NA
## [1] "fitting model: All.Interact.X.no.rnorm.rpart"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00833 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-49.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-50.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.148888889      1 0.4888889
## 3 0.008333333      2 0.3400000
## 
## Variable importance
##                                   biddable 
##                                         34 
##                            startprice.diff 
##                                         21 
##                                   idseq.my 
##                                          7 
##            prdline.my.fctriPad 3+:biddable 
##                                          6 
##            prdline.my.fctriPadAir:biddable 
##                                          6 
##             prdline.my.fctriPad 1:biddable 
##                                          5 
##           prdline.my.fctriPadmini:biddable 
##                                          5 
##     prdline.my.fctriPadAir:startprice.diff 
##                                          5 
##     prdline.my.fctriPad 3+:startprice.diff 
##                                          4 
## prdline.my.fctriPadmini 2+:startprice.diff 
##                                          3 
##    prdline.my.fctriPadmini:startprice.diff 
##                                          3 
##      prdline.my.fctriPad 2:startprice.diff 
##                                          2 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable                       < 0.5      to the left,  improve=144.14990, (0 missing)
##       startprice.diff                < 44.18097 to the right, improve= 78.49785, (0 missing)
##       idseq.my                       < 905.5    to the right, improve= 38.91299, (0 missing)
##       prdline.my.fctriPad 1:biddable < 0.5      to the left,  improve= 25.37925, (0 missing)
##       prdline.my.fctriPad 2:biddable < 0.5      to the left,  improve= 22.83949, (0 missing)
##   Surrogate splits:
##       idseq.my                         < 869      to the right, agree=0.636, adj=0.211, (0 split)
##       prdline.my.fctriPad 3+:biddable  < 0.5      to the left,  agree=0.624, adj=0.187, (0 split)
##       prdline.my.fctriPadAir:biddable  < 0.5      to the left,  agree=0.618, adj=0.173, (0 split)
##       prdline.my.fctriPad 1:biddable   < 0.5      to the left,  agree=0.613, adj=0.162, (0 split)
##       prdline.my.fctriPadmini:biddable < 0.5      to the left,  agree=0.611, adj=0.158, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.1488889
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (89 obs) right son=7 (361 obs)
##   Primary splits:
##       startprice.diff                         < 84.72197 to the right, improve=88.61445, (0 missing)
##       prdline.my.fctriPad 3+:startprice.diff  < 90.53641 to the right, improve=20.17141, (0 missing)
##       idseq.my                                < 670.5    to the right, improve=15.11062, (0 missing)
##       prdline.my.fctriPadmini:startprice.diff < 54.43284 to the right, improve=13.52563, (0 missing)
##       prdline.my.fctriPadAir:startprice.diff  < 102.8707 to the right, improve=11.78578, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir:startprice.diff     < 92.45617 to the right, agree=0.851, adj=0.247, (0 split)
##       prdline.my.fctriPad 3+:startprice.diff     < 90.53641 to the right, agree=0.840, adj=0.191, (0 split)
##       prdline.my.fctriPadmini:startprice.diff    < 88.75737 to the right, agree=0.827, adj=0.124, (0 split)
##       prdline.my.fctriPadmini 2+:startprice.diff < 105.0949 to the right, agree=0.827, adj=0.124, (0 split)
##       prdline.my.fctriPad 2:startprice.diff      < 76.28875 to the right, agree=0.818, adj=0.079, (0 split)
## 
## Node number 6: 89 observations
##   predicted class=N  expected loss=0.1235955  P(node) =0.09137577
##     class counts:    78    11
##    probabilities: 0.876 0.124 
## 
## Node number 7: 361 observations
##   predicted class=Y  expected loss=0.08864266  P(node) =0.3706366
##     class counts:    32   329
##    probabilities: 0.089 0.911 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.53798768 0.46201232)  
##   2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##   3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##     6) startprice.diff>=84.72197 89  11 N (0.87640449 0.12359551) *
##     7) startprice.diff< 84.72197 361  32 Y (0.08864266 0.91135734) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-51.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6576779
## 4        0.3 0.8113440
## 5        0.4 0.8113440
## 6        0.5 0.8113440
## 7        0.6 0.8113440
## 8        0.7 0.8113440
## 9        0.8 0.8113440
## 10       0.9 0.8113440
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-52.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rpart.N
## 1         N                                               492
## 2         Y                                               121
##   sold.fctr.predict.All.Interact.X.no.rnorm.rpart.Y
## 1                                                32
## 2                                               329
##          Prediction
## Reference   N   Y
##         N 492  32
##         Y 121 329
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.429158e-01   6.795322e-01   8.185193e-01   8.652175e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.699210e-90   1.124184e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-53.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6515397
## 4        0.3 0.7934783
## 5        0.4 0.7934783
## 6        0.5 0.7934783
## 7        0.6 0.7934783
## 8        0.7 0.7934783
## 9        0.8 0.7934783
## 10       0.9 0.7934783
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-54.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rpart.N
## 1         N                                               441
## 2         Y                                               118
##   sold.fctr.predict.All.Interact.X.no.rnorm.rpart.Y
## 1                                                34
## 2                                               292
##          Prediction
## Reference   N   Y
##         N 441  34
##         Y 118 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.282486e-01   6.497240e-01   8.017531e-01   8.525367e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.168374e-74   1.671294e-11 
##                        model_id model_method
## 1 All.Interact.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.739                 0.077
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8435581                    0.9        0.811344        0.8347135
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8185193             0.8652175     0.6642538   0.8273068
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.7934783        0.8282486
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8017531             0.8525367      0.649724
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0167898      0.03471308
##                 label step_major step_minor     bgn     end elapsed
## 10 fit.models_1_rpart         10          0 196.112 201.596   5.484
## 11    fit.models_1_rf         11          0 201.596      NA      NA
## [1] "fitting model: All.Interact.X.no.rnorm.rf"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 55 on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-55.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-56.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        974   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           1948   matrix     numeric  
## oob.times        974   -none-     numeric  
## classes            2   -none-     character
## importance       108   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           108   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-57.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8547009
## 3        0.2 0.9564293
## 4        0.3 0.9836066
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 0.9988877
## 8        0.7 0.9547038
## 9        0.8 0.8916256
## 10       0.9 0.8110964
## 11       1.0 0.3240223
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-58.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.N
## 1         N                                            524
## 2         Y                                             NA
##   sold.fctr.predict.All.Interact.X.no.rnorm.rf.Y
## 1                                             NA
## 2                                            450
##          Prediction
## Reference   N   Y
##         N 524   0
##         Y   0 450
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.962198e-01   1.000000e+00   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##  5.919016e-263            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-59.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7174312
## 3        0.2 0.7621951
## 4        0.3 0.7900677
## 5        0.4 0.7980535
## 6        0.5 0.8120104
## 7        0.6 0.8102981
## 8        0.7 0.8027586
## 9        0.8 0.7823613
## 10       0.9 0.7346327
## 11       1.0 0.1890110
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-60.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.N
## 1         N                                            430
## 2         Y                                             99
##   sold.fctr.predict.All.Interact.X.no.rnorm.rf.Y
## 1                                             45
## 2                                            311
##          Prediction
## Reference   N   Y
##         N 430  45
##         Y  99 311
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.372881e-01   6.698360e-01   8.112909e-01   8.610159e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.003894e-79   1.002346e-05 
##                     model_id model_method
## 1 All.Interact.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     18.267                 5.426
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8460019
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962198                     1     0.6873959   0.8910295
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.8120104        0.8372881
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8112909             0.8610159      0.669836
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01330961       0.0282581
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
#stop(here"); sav_models_df <- glb_models_df; glb_models_df <- sav_models_df
model_id <- "csm"; indep_vars_vctr <- c(NULL
    ,"prdline.my.fctr", "prdline.my.fctr:.clusterid.fctr"
    ,"prdline.my.fctr*biddable"
    #,"prdline.my.fctr*startprice.log"
    ,"prdline.my.fctr*startprice.diff"    
    #,"prdline.my.fctr*idseq.my"   
    ,"prdline.my.fctr*condition.fctr"
    ,"prdline.my.fctr*D.terms.n.post.stop"
    #,"prdline.my.fctr*D.terms.n.post.stem"
    ,"prdline.my.fctr*cellular.fctr"    
#    ,"<feat1>:<feat2>"
                                           )
for (method in glb_models_method_vctr) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".",
                                                                     method)]]);               print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: csm.glm"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   639
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-61.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-62.png) 

```
## Warning: not plotting observations with leverage one:
##   639
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-63.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.76431  -0.52933  -0.04817   0.40570   2.57496  
## 
## Coefficients: (11 not defined because of singularities)
##                                                                       Estimate
## (Intercept)                                                         -2.035e+00
## `prdline.my.fctriPad 1`                                              1.046e+00
## `prdline.my.fctriPad 2`                                              2.661e-01
## `prdline.my.fctriPad 3+`                                             1.239e+00
## prdline.my.fctriPadAir                                               8.554e-01
## prdline.my.fctriPadmini                                              1.272e+00
## `prdline.my.fctriPadmini 2+`                                         1.434e-01
## biddable                                                             3.476e+00
## startprice.diff                                                     -8.746e-03
## `condition.fctrFor parts or not working`                             1.440e+00
## `condition.fctrManufacturer refurbished`                            -9.262e+00
## condition.fctrNew                                                   -6.758e-01
## `condition.fctrNew other (see details)`                              5.175e+00
## `condition.fctrSeller refurbished`                                   4.722e-01
## D.terms.n.post.stop                                                  2.414e-02
## cellular.fctr1                                                      -3.107e+00
## cellular.fctrUnknown                                                -1.003e+00
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            1.483e+00
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            -2.709e+00
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             6.468e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                           -3.660e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                           -8.400e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           1.657e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        3.852e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           -1.168e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            -4.385e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            -1.490e+01
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            1.261e+00
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            5.776e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           1.815e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        2.023e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            -5.155e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             1.078e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            2.158e+00
## `prdline.my.fctriPadAir:.clusterid.fctr4`                           -2.270e-01
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           1.178e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             4.610e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           1.237e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                               NA
## `prdline.my.fctriPad 1:biddable`                                     2.092e+00
## `prdline.my.fctriPad 2:biddable`                                     1.063e+00
## `prdline.my.fctriPad 3+:biddable`                                    2.489e-01
## `prdline.my.fctriPadAir:biddable`                                    6.338e-01
## `prdline.my.fctriPadmini:biddable`                                   3.826e-02
## `prdline.my.fctriPadmini 2+:biddable`                                2.768e-01
## `prdline.my.fctriPad 1:startprice.diff`                             -5.542e-02
## `prdline.my.fctriPad 2:startprice.diff`                             -1.179e-02
## `prdline.my.fctriPad 3+:startprice.diff`                            -2.315e-02
## `prdline.my.fctriPadAir:startprice.diff`                            -6.637e-03
## `prdline.my.fctriPadmini:startprice.diff`                           -1.977e-02
## `prdline.my.fctriPadmini 2+:startprice.diff`                        -1.436e-02
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`      -8.422e-01
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`      -3.149e-01
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     -1.248e+00
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     -1.677e+00
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    -2.741e+00
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` -1.698e+01
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      8.534e+00
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      1.042e+01
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     8.778e+00
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`         NA
## `prdline.my.fctriPad 1:condition.fctrNew`                           -1.457e+01
## `prdline.my.fctriPad 2:condition.fctrNew`                                   NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                          -7.033e-01
## `prdline.my.fctriPadAir:condition.fctrNew`                           5.886e-01
## `prdline.my.fctriPadmini:condition.fctrNew`                         -2.294e-01
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       1.846e+00
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       -6.127e+00
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       -1.841e+01
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`      -3.559e+00
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`      -3.472e+00
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     -5.390e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  -6.100e+00
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.413e+00
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            -8.394e-01
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           -1.968e+00
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           -1.827e+00
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          -1.260e+00
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       -1.707e+01
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          3.172e-01
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         -1.071e-01
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        -1.653e-01
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        -8.350e-02
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       -2.173e-01
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    -2.491e-01
## `prdline.my.fctriPad 1:cellular.fctr1`                               2.937e+00
## `prdline.my.fctriPad 2:cellular.fctr1`                               2.226e+00
## `prdline.my.fctriPad 3+:cellular.fctr1`                              4.269e+00
## `prdline.my.fctriPadAir:cellular.fctr1`                              2.851e+00
## `prdline.my.fctriPadmini:cellular.fctr1`                             4.286e+00
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          4.006e+00
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         1.111e-01
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        -2.989e-01
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       -1.248e+00
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        4.163e-01
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       3.140e-02
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    4.144e-01
##                                                                     Std. Error
## (Intercept)                                                          7.560e-01
## `prdline.my.fctriPad 1`                                              9.139e-01
## `prdline.my.fctriPad 2`                                              1.013e+00
## `prdline.my.fctriPad 3+`                                             9.746e-01
## prdline.my.fctriPadAir                                               8.750e-01
## prdline.my.fctriPadmini                                              8.877e-01
## `prdline.my.fctriPadmini 2+`                                         1.112e+00
## biddable                                                             7.883e-01
## startprice.diff                                                      3.046e-03
## `condition.fctrFor parts or not working`                             8.787e-01
## `condition.fctrManufacturer refurbished`                             2.400e+03
## condition.fctrNew                                                    1.002e+00
## `condition.fctrNew other (see details)`                              2.064e+00
## `condition.fctrSeller refurbished`                                   1.201e+00
## D.terms.n.post.stop                                                  9.542e-02
## cellular.fctr1                                                       1.829e+00
## cellular.fctrUnknown                                                 8.432e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            9.415e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             1.816e+00
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             1.095e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            9.718e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            9.535e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           1.011e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        2.047e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            1.004e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             2.189e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             7.721e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            9.377e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            1.021e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           1.239e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        1.571e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             2.185e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             1.313e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            1.054e+00
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            9.756e-01
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           1.195e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             1.714e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           1.654e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                               NA
## `prdline.my.fctriPad 1:biddable`                                     1.412e+00
## `prdline.my.fctriPad 2:biddable`                                     1.128e+00
## `prdline.my.fctriPad 3+:biddable`                                    1.082e+00
## `prdline.my.fctriPadAir:biddable`                                    9.745e-01
## `prdline.my.fctriPadmini:biddable`                                   1.012e+00
## `prdline.my.fctriPadmini 2+:biddable`                                1.220e+00
## `prdline.my.fctriPad 1:startprice.diff`                              2.014e-02
## `prdline.my.fctriPad 2:startprice.diff`                              7.430e-03
## `prdline.my.fctriPad 3+:startprice.diff`                             6.785e-03
## `prdline.my.fctriPadAir:startprice.diff`                             4.352e-03
## `prdline.my.fctriPadmini:startprice.diff`                            6.836e-03
## `prdline.my.fctriPadmini 2+:startprice.diff`                         7.584e-03
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       1.866e+00
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       1.577e+00
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      1.246e+00
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      1.443e+00
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     1.122e+00
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  1.605e+03
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      2.400e+03
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      2.400e+03
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     2.400e+03
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`         NA
## `prdline.my.fctriPad 1:condition.fctrNew`                            9.728e+02
## `prdline.my.fctriPad 2:condition.fctrNew`                                   NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                           1.876e+00
## `prdline.my.fctriPadAir:condition.fctrNew`                           1.168e+00
## `prdline.my.fctriPadmini:condition.fctrNew`                          1.222e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       1.355e+00
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        1.131e+01
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        1.639e+03
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       2.402e+00
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       2.283e+00
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      2.486e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   2.436e+00
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.845e+00
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             1.616e+00
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            1.609e+00
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            1.699e+00
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           1.648e+00
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        1.553e+03
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          2.319e-01
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          1.472e-01
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         1.286e-01
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         1.290e-01
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        1.400e-01
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     2.067e-01
## `prdline.my.fctriPad 1:cellular.fctr1`                               1.995e+00
## `prdline.my.fctriPad 2:cellular.fctr1`                               1.960e+00
## `prdline.my.fctriPad 3+:cellular.fctr1`                              1.951e+00
## `prdline.my.fctriPadAir:cellular.fctr1`                              1.896e+00
## `prdline.my.fctriPadmini:cellular.fctr1`                             1.943e+00
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          2.011e+00
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         1.734e+00
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         1.636e+00
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        1.473e+00
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        1.542e+00
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       1.395e+00
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    1.423e+00
##                                                                     z value
## (Intercept)                                                          -2.692
## `prdline.my.fctriPad 1`                                               1.145
## `prdline.my.fctriPad 2`                                               0.263
## `prdline.my.fctriPad 3+`                                              1.271
## prdline.my.fctriPadAir                                                0.978
## prdline.my.fctriPadmini                                               1.433
## `prdline.my.fctriPadmini 2+`                                          0.129
## biddable                                                              4.410
## startprice.diff                                                      -2.871
## `condition.fctrFor parts or not working`                              1.639
## `condition.fctrManufacturer refurbished`                             -0.004
## condition.fctrNew                                                    -0.674
## `condition.fctrNew other (see details)`                               2.507
## `condition.fctrSeller refurbished`                                    0.393
## D.terms.n.post.stop                                                   0.253
## cellular.fctr1                                                       -1.698
## cellular.fctrUnknown                                                 -1.190
## `prdline.my.fctrUnknown:.clusterid.fctr2`                             1.575
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             -1.492
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              0.591
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            -0.377
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            -0.881
## `prdline.my.fctriPadmini:.clusterid.fctr2`                            1.639
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                         1.882
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            -1.163
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             -2.003
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             -0.019
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             1.345
## `prdline.my.fctriPadAir:.clusterid.fctr3`                             0.566
## `prdline.my.fctriPadmini:.clusterid.fctr3`                            1.465
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                         1.288
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             -2.359
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              0.821
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             2.048
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            -0.233
## `prdline.my.fctriPadmini:.clusterid.fctr4`                            0.986
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                            NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                              2.689
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                            0.748
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                            NA
## `prdline.my.fctriPad 1:biddable`                                      1.482
## `prdline.my.fctriPad 2:biddable`                                      0.942
## `prdline.my.fctriPad 3+:biddable`                                     0.230
## `prdline.my.fctriPadAir:biddable`                                     0.650
## `prdline.my.fctriPadmini:biddable`                                    0.038
## `prdline.my.fctriPadmini 2+:biddable`                                 0.227
## `prdline.my.fctriPad 1:startprice.diff`                              -2.751
## `prdline.my.fctriPad 2:startprice.diff`                              -1.587
## `prdline.my.fctriPad 3+:startprice.diff`                             -3.412
## `prdline.my.fctriPadAir:startprice.diff`                             -1.525
## `prdline.my.fctriPadmini:startprice.diff`                            -2.891
## `prdline.my.fctriPadmini 2+:startprice.diff`                         -1.893
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       -0.451
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       -0.200
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      -1.001
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      -1.162
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     -2.442
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  -0.011
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`           NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`           NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`       0.004
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`       0.004
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`      0.004
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`      NA
## `prdline.my.fctriPad 1:condition.fctrNew`                            -0.015
## `prdline.my.fctriPad 2:condition.fctrNew`                                NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                           -0.375
## `prdline.my.fctriPadAir:condition.fctrNew`                            0.504
## `prdline.my.fctriPadmini:condition.fctrNew`                          -0.188
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                        1.362
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        -0.542
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        -0.011
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       -1.482
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       -1.521
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      -2.168
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   -2.504
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              0.766
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             -0.519
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            -1.223
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            -1.075
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           -0.764
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        -0.011
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                           1.368
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          -0.728
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         -1.286
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         -0.647
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        -1.553
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     -1.205
## `prdline.my.fctriPad 1:cellular.fctr1`                                1.472
## `prdline.my.fctriPad 2:cellular.fctr1`                                1.136
## `prdline.my.fctriPad 3+:cellular.fctr1`                               2.188
## `prdline.my.fctriPadAir:cellular.fctr1`                               1.504
## `prdline.my.fctriPadmini:cellular.fctr1`                              2.205
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                           1.992
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                          0.064
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         -0.183
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        -0.847
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                         0.270
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                        0.023
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                     0.291
##                                                                     Pr(>|z|)
## (Intercept)                                                         0.007113
## `prdline.my.fctriPad 1`                                             0.252327
## `prdline.my.fctriPad 2`                                             0.792761
## `prdline.my.fctriPad 3+`                                            0.203593
## prdline.my.fctriPadAir                                              0.328276
## prdline.my.fctriPadmini                                             0.151795
## `prdline.my.fctriPadmini 2+`                                        0.897343
## biddable                                                            1.03e-05
## startprice.diff                                                     0.004088
## `condition.fctrFor parts or not working`                            0.101160
## `condition.fctrManufacturer refurbished`                            0.996920
## condition.fctrNew                                                   0.500182
## `condition.fctrNew other (see details)`                             0.012163
## `condition.fctrSeller refurbished`                                  0.694049
## D.terms.n.post.stop                                                 0.800242
## cellular.fctr1                                                      0.089444
## cellular.fctrUnknown                                                0.234174
## `prdline.my.fctrUnknown:.clusterid.fctr2`                           0.115163
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            0.135767
## `prdline.my.fctriPad 2:.clusterid.fctr2`                            0.554619
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                           0.706447
## `prdline.my.fctriPadAir:.clusterid.fctr2`                           0.378363
## `prdline.my.fctriPadmini:.clusterid.fctr2`                          0.101253
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                       0.059817
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           0.244730
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            0.045200
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            0.984602
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                           0.178660
## `prdline.my.fctriPadAir:.clusterid.fctr3`                           0.571428
## `prdline.my.fctriPadmini:.clusterid.fctr3`                          0.143021
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                       0.197917
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                 NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            0.018327
## `prdline.my.fctriPad 2:.clusterid.fctr4`                            0.411512
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                           0.040580
## `prdline.my.fctriPadAir:.clusterid.fctr4`                           0.816047
## `prdline.my.fctriPadmini:.clusterid.fctr4`                          0.324192
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                             NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                  NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                            0.007160
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                          0.454692
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                             NA
## `prdline.my.fctriPad 1:biddable`                                    0.138469
## `prdline.my.fctriPad 2:biddable`                                    0.345937
## `prdline.my.fctriPad 3+:biddable`                                   0.817971
## `prdline.my.fctriPadAir:biddable`                                   0.515431
## `prdline.my.fctriPadmini:biddable`                                  0.969838
## `prdline.my.fctriPadmini 2+:biddable`                               0.820546
## `prdline.my.fctriPad 1:startprice.diff`                             0.005940
## `prdline.my.fctriPad 2:startprice.diff`                             0.112538
## `prdline.my.fctriPad 3+:startprice.diff`                            0.000644
## `prdline.my.fctriPadAir:startprice.diff`                            0.127290
## `prdline.my.fctriPadmini:startprice.diff`                           0.003835
## `prdline.my.fctriPadmini 2+:startprice.diff`                        0.058353
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`      0.651767
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`      0.841694
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     0.316727
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     0.245243
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    0.014588
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` 0.991560
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`            NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`            NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     0.997162
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`     0.996534
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    0.997081
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`       NA
## `prdline.my.fctriPad 1:condition.fctrNew`                           0.988053
## `prdline.my.fctriPad 2:condition.fctrNew`                                 NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                          0.707766
## `prdline.my.fctriPadAir:condition.fctrNew`                          0.614162
## `prdline.my.fctriPadmini:condition.fctrNew`                         0.851006
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                      0.173161
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       0.587992
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       0.991039
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`      0.138473
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`      0.128287
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     0.030155
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  0.012278
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`            0.443753
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            0.603479
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           0.221386
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           0.282153
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          0.444620
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       0.991226
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                         0.171232
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         0.466912
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        0.198525
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        0.517338
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       0.120517
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    0.228181
## `prdline.my.fctriPad 1:cellular.fctr1`                              0.141054
## `prdline.my.fctriPad 2:cellular.fctr1`                              0.256041
## `prdline.my.fctriPad 3+:cellular.fctr1`                             0.028653
## `prdline.my.fctriPadAir:cellular.fctr1`                             0.132563
## `prdline.my.fctriPadmini:cellular.fctr1`                            0.027421
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                         0.046347
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                        0.948929
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        0.855006
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       0.397119
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                       0.787098
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                      0.982047
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                   0.770914
##                                                                        
## (Intercept)                                                         ** 
## `prdline.my.fctriPad 1`                                                
## `prdline.my.fctriPad 2`                                                
## `prdline.my.fctriPad 3+`                                               
## prdline.my.fctriPadAir                                                 
## prdline.my.fctriPadmini                                                
## `prdline.my.fctriPadmini 2+`                                           
## biddable                                                            ***
## startprice.diff                                                     ** 
## `condition.fctrFor parts or not working`                               
## `condition.fctrManufacturer refurbished`                               
## condition.fctrNew                                                      
## `condition.fctrNew other (see details)`                             *  
## `condition.fctrSeller refurbished`                                     
## D.terms.n.post.stop                                                    
## cellular.fctr1                                                      .  
## cellular.fctrUnknown                                                   
## `prdline.my.fctrUnknown:.clusterid.fctr2`                              
## `prdline.my.fctriPad 1:.clusterid.fctr2`                               
## `prdline.my.fctriPad 2:.clusterid.fctr2`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                              
## `prdline.my.fctriPadAir:.clusterid.fctr2`                              
## `prdline.my.fctriPadmini:.clusterid.fctr2`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                       .  
## `prdline.my.fctrUnknown:.clusterid.fctr3`                              
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            *  
## `prdline.my.fctriPad 2:.clusterid.fctr3`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                              
## `prdline.my.fctriPadAir:.clusterid.fctr3`                              
## `prdline.my.fctriPadmini:.clusterid.fctr3`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                          
## `prdline.my.fctrUnknown:.clusterid.fctr4`                              
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            *  
## `prdline.my.fctriPad 2:.clusterid.fctr4`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                           *  
## `prdline.my.fctriPadAir:.clusterid.fctr4`                              
## `prdline.my.fctriPadmini:.clusterid.fctr4`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                          
## `prdline.my.fctrUnknown:.clusterid.fctr5`                              
## `prdline.my.fctriPad 1:.clusterid.fctr5`                               
## `prdline.my.fctriPad 2:.clusterid.fctr5`                            ** 
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                              
## `prdline.my.fctriPadAir:.clusterid.fctr5`                              
## `prdline.my.fctriPadmini:.clusterid.fctr5`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                          
## `prdline.my.fctriPad 1:biddable`                                       
## `prdline.my.fctriPad 2:biddable`                                       
## `prdline.my.fctriPad 3+:biddable`                                      
## `prdline.my.fctriPadAir:biddable`                                      
## `prdline.my.fctriPadmini:biddable`                                     
## `prdline.my.fctriPadmini 2+:biddable`                                  
## `prdline.my.fctriPad 1:startprice.diff`                             ** 
## `prdline.my.fctriPad 2:startprice.diff`                                
## `prdline.my.fctriPad 3+:startprice.diff`                            ***
## `prdline.my.fctriPadAir:startprice.diff`                               
## `prdline.my.fctriPadmini:startprice.diff`                           ** 
## `prdline.my.fctriPadmini 2+:startprice.diff`                        .  
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`         
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`         
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`        
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`        
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    *  
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`    
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`         
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`         
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`        
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`        
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`       
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`    
## `prdline.my.fctriPad 1:condition.fctrNew`                              
## `prdline.my.fctriPad 2:condition.fctrNew`                              
## `prdline.my.fctriPad 3+:condition.fctrNew`                             
## `prdline.my.fctriPadAir:condition.fctrNew`                             
## `prdline.my.fctriPadmini:condition.fctrNew`                            
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                         
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`          
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`          
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`         
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`         
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     *  
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  *  
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`               
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`               
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`             
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`          
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                           
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                           
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                          
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                       
## `prdline.my.fctriPad 1:cellular.fctr1`                                 
## `prdline.my.fctriPad 2:cellular.fctr1`                                 
## `prdline.my.fctriPad 3+:cellular.fctr1`                             *  
## `prdline.my.fctriPadAir:cellular.fctr1`                                
## `prdline.my.fctriPadmini:cellular.fctr1`                            *  
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                         *  
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                           
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                           
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                          
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                          
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                         
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  662.45  on 880  degrees of freedom
## AIC: 850.45
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-64.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-65.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7489289
## 3        0.2 0.7973485
## 4        0.3 0.8207739
## 5        0.4 0.8315098
## 6        0.5 0.8293242
## 7        0.6 0.8219833
## 8        0.7 0.8114144
## 9        0.8 0.7720685
## 10       0.9 0.6725146
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.glm.N sold.fctr.predict.csm.glm.Y
## 1         N                         440                          84
## 2         Y                          70                         380
##          Prediction
## Reference   N   Y
##         N 440  84
##         Y  70 380
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.418891e-01   6.826395e-01   8.174354e-01   8.642544e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   7.796162e-90   2.948368e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-66.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-67.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7240705
## 3        0.2 0.7231760
## 4        0.3 0.7459954
## 5        0.4 0.7588739
## 6        0.5 0.7747748
## 7        0.6 0.7708609
## 8        0.7 0.7651934
## 9        0.8 0.7357664
## 10       0.9 0.6215781
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-68.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.glm.N sold.fctr.predict.csm.glm.Y
## 1         N                         409                          66
## 2         Y                         109                         301
##          Prediction
## Reference   N   Y
##         N 409  66
##         Y 109 301
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.022599e-01   5.995009e-01   7.744658e-01   8.280245e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   2.734441e-61   1.498873e-03 
##   model_id model_method
## 1  csm.glm          glm
##                                                                                                                                                                                                             feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.908                 0.306
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9246141                    0.4       0.8315098        0.7884995
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8174354             0.8642544     0.5731278   0.8425315
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7747748        0.8022599
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7744658             0.8280245     0.5995009    850.4544
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02311165      0.04919951
##                                           importance
## biddable                                   100.00000
## `prdline.my.fctriPad 3+:startprice.diff`    77.35573
## `prdline.my.fctriPadmini:startprice.diff`   65.53601
## startprice.diff                             65.07762
## `prdline.my.fctriPad 1:startprice.diff`     62.35076
## `prdline.my.fctriPad 2:.clusterid.fctr5`    60.94899
## [1] "fitting model: csm.bayesglm"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.7170  -0.5760  -0.1076   0.4494   2.5294  
## 
## Coefficients:
##                                                                      Estimate
## (Intercept)                                                         -1.697239
## `prdline.my.fctriPad 1`                                              0.684140
## `prdline.my.fctriPad 2`                                              0.051201
## `prdline.my.fctriPad 3+`                                             1.005200
## prdline.my.fctriPadAir                                               0.559909
## prdline.my.fctriPadmini                                              0.919847
## `prdline.my.fctriPadmini 2+`                                         0.056866
## biddable                                                             3.080858
## startprice.diff                                                     -0.007684
## `condition.fctrFor parts or not working`                             0.694025
## `condition.fctrManufacturer refurbished`                             0.055555
## condition.fctrNew                                                   -0.789406
## `condition.fctrNew other (see details)`                              1.349096
## `condition.fctrSeller refurbished`                                  -0.157919
## D.terms.n.post.stop                                                 -0.007054
## cellular.fctr1                                                      -0.519582
## cellular.fctrUnknown                                                -0.692060
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            1.118960
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            -0.605038
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             0.387043
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                           -0.381145
## `prdline.my.fctriPadAir:.clusterid.fctr2`                           -0.713444
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           1.053034
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        1.712572
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           -0.924431
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            -1.700088
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            -1.903595
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            0.870240
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            0.437486
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           1.137744
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        0.702140
## `prdline.my.fctrUnknown:.clusterid.fctr4`                            0.000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            -2.500196
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             0.674542
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            1.589031
## `prdline.my.fctriPadAir:.clusterid.fctr4`                           -0.170348
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           0.439091
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                        0.000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                            0.000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                             0.000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             3.739868
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            0.000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                            0.000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           0.479564
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                        0.000000
## `prdline.my.fctriPad 1:biddable`                                     1.457478
## `prdline.my.fctriPad 2:biddable`                                     1.199837
## `prdline.my.fctriPad 3+:biddable`                                    0.353597
## `prdline.my.fctriPadAir:biddable`                                    0.879184
## `prdline.my.fctriPadmini:biddable`                                   0.282618
## `prdline.my.fctriPadmini 2+:biddable`                                0.282838
## `prdline.my.fctriPad 1:startprice.diff`                             -0.046162
## `prdline.my.fctriPad 2:startprice.diff`                             -0.011907
## `prdline.my.fctriPad 3+:startprice.diff`                            -0.021792
## `prdline.my.fctriPadAir:startprice.diff`                            -0.006889
## `prdline.my.fctriPadmini:startprice.diff`                           -0.019541
## `prdline.my.fctriPadmini 2+:startprice.diff`                        -0.011557
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       0.851523
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       0.243017
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     -0.460135
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     -0.677646
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    -1.829299
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` -1.358968
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`       0.000000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`       0.000000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     -0.490155
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      0.928555
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    -0.302015
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished` -0.013675
## `prdline.my.fctriPad 1:condition.fctrNew`                           -0.954618
## `prdline.my.fctriPad 2:condition.fctrNew`                            0.000000
## `prdline.my.fctriPad 3+:condition.fctrNew`                          -0.390219
## `prdline.my.fctriPadAir:condition.fctrNew`                           0.641402
## `prdline.my.fctriPadmini:condition.fctrNew`                         -0.039291
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       1.632545
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       -0.272098
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       -0.754397
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       0.205759
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       0.242861
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     -1.048826
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  -1.779094
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.483235
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            -0.100222
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           -0.948700
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           -0.821376
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          -0.434114
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       -1.476240
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          0.128276
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         -0.053472
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        -0.109832
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        -0.058330
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       -0.121570
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    -0.073843
## `prdline.my.fctriPad 1:cellular.fctr1`                               0.419077
## `prdline.my.fctriPad 2:cellular.fctr1`                              -0.269887
## `prdline.my.fctriPad 3+:cellular.fctr1`                              1.396357
## `prdline.my.fctriPadAir:cellular.fctr1`                              0.254954
## `prdline.my.fctriPadmini:cellular.fctr1`                             1.565677
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          1.121080
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                        -0.261280
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        -0.386424
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       -1.237846
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        0.043509
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                      -0.204704
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    0.171147
##                                                                     Std. Error
## (Intercept)                                                           0.481851
## `prdline.my.fctriPad 1`                                               0.636489
## `prdline.my.fctriPad 2`                                               0.698731
## `prdline.my.fctriPad 3+`                                              0.678021
## prdline.my.fctriPadAir                                                0.602916
## prdline.my.fctriPadmini                                               0.614141
## `prdline.my.fctriPadmini 2+`                                          0.744443
## biddable                                                              0.502050
## startprice.diff                                                       0.002443
## `condition.fctrFor parts or not working`                              0.574141
## `condition.fctrManufacturer refurbished`                              1.097392
## condition.fctrNew                                                     0.634270
## `condition.fctrNew other (see details)`                               0.822832
## `condition.fctrSeller refurbished`                                    0.702033
## D.terms.n.post.stop                                                   0.071795
## cellular.fctr1                                                        0.623109
## cellular.fctrUnknown                                                  0.494478
## `prdline.my.fctrUnknown:.clusterid.fctr2`                             0.710742
## `prdline.my.fctriPad 1:.clusterid.fctr2`                              1.056533
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              0.852760
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                             0.799252
## `prdline.my.fctriPadAir:.clusterid.fctr2`                             0.802609
## `prdline.my.fctriPadmini:.clusterid.fctr2`                            0.791639
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                         1.330145
## `prdline.my.fctrUnknown:.clusterid.fctr3`                             0.776345
## `prdline.my.fctriPad 1:.clusterid.fctr3`                              1.193393
## `prdline.my.fctriPad 2:.clusterid.fctr3`                              1.614892
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             0.775188
## `prdline.my.fctriPadAir:.clusterid.fctr3`                             0.857349
## `prdline.my.fctriPadmini:.clusterid.fctr3`                            0.941158
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                         1.093154
## `prdline.my.fctrUnknown:.clusterid.fctr4`                             2.500000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                              1.221426
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              1.019980
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             0.866804
## `prdline.my.fctriPadAir:.clusterid.fctr4`                             0.823779
## `prdline.my.fctriPadmini:.clusterid.fctr4`                            0.941393
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                         2.500000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                             2.500000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                              2.500000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                              1.294451
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                             2.500000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                             2.500000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                            1.218561
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                         2.500000
## `prdline.my.fctriPad 1:biddable`                                      0.869928
## `prdline.my.fctriPad 2:biddable`                                      0.803394
## `prdline.my.fctriPad 3+:biddable`                                     0.745641
## `prdline.my.fctriPadAir:biddable`                                     0.693995
## `prdline.my.fctriPadmini:biddable`                                    0.724271
## `prdline.my.fctriPadmini 2+:biddable`                                 0.826072
## `prdline.my.fctriPad 1:startprice.diff`                               0.015521
## `prdline.my.fctriPad 2:startprice.diff`                               0.006586
## `prdline.my.fctriPad 3+:startprice.diff`                              0.005872
## `prdline.my.fctriPadAir:startprice.diff`                              0.003679
## `prdline.my.fctriPadmini:startprice.diff`                             0.006162
## `prdline.my.fctriPadmini 2+:startprice.diff`                          0.005776
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`        1.160022
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`        1.112469
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`       0.899186
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`       1.048309
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`      0.819605
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`   1.660205
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`        2.500000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`        2.500000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`       1.297445
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`       1.274120
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`      1.286510
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`   2.466752
## `prdline.my.fctriPad 1:condition.fctrNew`                             1.709697
## `prdline.my.fctriPad 2:condition.fctrNew`                             2.500000
## `prdline.my.fctriPad 3+:condition.fctrNew`                            1.273124
## `prdline.my.fctriPadAir:condition.fctrNew`                            0.798404
## `prdline.my.fctriPadmini:condition.fctrNew`                           0.845211
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                        0.903726
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`         2.053091
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`         1.772790
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`        1.161911
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`        1.076915
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`       1.246030
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`    1.223977
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              1.233811
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`              1.051818
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`             1.056615
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`             1.141599
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`            1.082284
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`         1.644347
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                           0.139444
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                           0.114026
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                          0.101317
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                          0.102509
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                         0.106125
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                      0.149909
## `prdline.my.fctriPad 1:cellular.fctr1`                                0.840281
## `prdline.my.fctriPad 2:cellular.fctr1`                                0.830056
## `prdline.my.fctriPad 3+:cellular.fctr1`                               0.809916
## `prdline.my.fctriPadAir:cellular.fctr1`                               0.747653
## `prdline.my.fctriPadmini:cellular.fctr1`                              0.829877
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                           0.906864
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                          1.177299
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                          1.137571
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                         1.042588
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                         1.090862
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                        1.011288
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                     1.018200
##                                                                     z value
## (Intercept)                                                          -3.522
## `prdline.my.fctriPad 1`                                               1.075
## `prdline.my.fctriPad 2`                                               0.073
## `prdline.my.fctriPad 3+`                                              1.483
## prdline.my.fctriPadAir                                                0.929
## prdline.my.fctriPadmini                                               1.498
## `prdline.my.fctriPadmini 2+`                                          0.076
## biddable                                                              6.137
## startprice.diff                                                      -3.146
## `condition.fctrFor parts or not working`                              1.209
## `condition.fctrManufacturer refurbished`                              0.051
## condition.fctrNew                                                    -1.245
## `condition.fctrNew other (see details)`                               1.640
## `condition.fctrSeller refurbished`                                   -0.225
## D.terms.n.post.stop                                                  -0.098
## cellular.fctr1                                                       -0.834
## cellular.fctrUnknown                                                 -1.400
## `prdline.my.fctrUnknown:.clusterid.fctr2`                             1.574
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             -0.573
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              0.454
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            -0.477
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            -0.889
## `prdline.my.fctriPadmini:.clusterid.fctr2`                            1.330
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                         1.288
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            -1.191
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             -1.425
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             -1.179
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             1.123
## `prdline.my.fctriPadAir:.clusterid.fctr3`                             0.510
## `prdline.my.fctriPadmini:.clusterid.fctr3`                            1.209
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                         0.642
## `prdline.my.fctrUnknown:.clusterid.fctr4`                             0.000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             -2.047
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              0.661
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             1.833
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            -0.207
## `prdline.my.fctriPadmini:.clusterid.fctr4`                            0.466
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                         0.000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                              0.000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                              2.889
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                            0.394
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                         0.000
## `prdline.my.fctriPad 1:biddable`                                      1.675
## `prdline.my.fctriPad 2:biddable`                                      1.493
## `prdline.my.fctriPad 3+:biddable`                                     0.474
## `prdline.my.fctriPadAir:biddable`                                     1.267
## `prdline.my.fctriPadmini:biddable`                                    0.390
## `prdline.my.fctriPadmini 2+:biddable`                                 0.342
## `prdline.my.fctriPad 1:startprice.diff`                              -2.974
## `prdline.my.fctriPad 2:startprice.diff`                              -1.808
## `prdline.my.fctriPad 3+:startprice.diff`                             -3.711
## `prdline.my.fctriPadAir:startprice.diff`                             -1.872
## `prdline.my.fctriPadmini:startprice.diff`                            -3.171
## `prdline.my.fctriPadmini 2+:startprice.diff`                         -2.001
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`        0.734
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`        0.218
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      -0.512
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      -0.646
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     -2.232
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  -0.819
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`        0.000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`        0.000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      -0.378
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`       0.729
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     -0.235
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`  -0.006
## `prdline.my.fctriPad 1:condition.fctrNew`                            -0.558
## `prdline.my.fctriPad 2:condition.fctrNew`                             0.000
## `prdline.my.fctriPad 3+:condition.fctrNew`                           -0.307
## `prdline.my.fctriPadAir:condition.fctrNew`                            0.803
## `prdline.my.fctriPadmini:condition.fctrNew`                          -0.046
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                        1.806
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        -0.133
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        -0.426
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`        0.177
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`        0.226
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      -0.842
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   -1.454
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              1.202
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             -0.095
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            -0.898
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            -0.719
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           -0.401
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        -0.898
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                           0.920
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          -0.469
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         -1.084
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         -0.569
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        -1.146
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     -0.493
## `prdline.my.fctriPad 1:cellular.fctr1`                                0.499
## `prdline.my.fctriPad 2:cellular.fctr1`                               -0.325
## `prdline.my.fctriPad 3+:cellular.fctr1`                               1.724
## `prdline.my.fctriPadAir:cellular.fctr1`                               0.341
## `prdline.my.fctriPadmini:cellular.fctr1`                              1.887
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                           1.236
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         -0.222
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         -0.340
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        -1.187
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                         0.040
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       -0.202
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                     0.168
##                                                                     Pr(>|z|)
## (Intercept)                                                         0.000428
## `prdline.my.fctriPad 1`                                             0.282435
## `prdline.my.fctriPad 2`                                             0.941585
## `prdline.my.fctriPad 3+`                                            0.138194
## prdline.my.fctriPadAir                                              0.353061
## prdline.my.fctriPadmini                                             0.134191
## `prdline.my.fctriPadmini 2+`                                        0.939111
## biddable                                                            8.43e-10
## startprice.diff                                                     0.001656
## `condition.fctrFor parts or not working`                            0.226738
## `condition.fctrManufacturer refurbished`                            0.959624
## condition.fctrNew                                                   0.213283
## `condition.fctrNew other (see details)`                             0.101093
## `condition.fctrSeller refurbished`                                  0.822021
## D.terms.n.post.stop                                                 0.921731
## cellular.fctr1                                                      0.404364
## cellular.fctrUnknown                                                0.161639
## `prdline.my.fctrUnknown:.clusterid.fctr2`                           0.115405
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            0.566872
## `prdline.my.fctriPad 2:.clusterid.fctr2`                            0.649921
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                           0.633449
## `prdline.my.fctriPadAir:.clusterid.fctr2`                           0.374053
## `prdline.my.fctriPadmini:.clusterid.fctr2`                          0.183454
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                       0.197917
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           0.233753
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            0.154278
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            0.238488
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                           0.261600
## `prdline.my.fctriPadAir:.clusterid.fctr3`                           0.609857
## `prdline.my.fctriPadmini:.clusterid.fctr3`                          0.226710
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                       0.520674
## `prdline.my.fctrUnknown:.clusterid.fctr4`                           1.000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            0.040663
## `prdline.my.fctriPad 2:.clusterid.fctr4`                            0.508401
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                           0.066772
## `prdline.my.fctriPadAir:.clusterid.fctr4`                           0.836175
## `prdline.my.fctriPadmini:.clusterid.fctr4`                          0.640910
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                       1.000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                           1.000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                            1.000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                            0.003863
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                           1.000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                           1.000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                          0.693914
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                       1.000000
## `prdline.my.fctriPad 1:biddable`                                    0.093855
## `prdline.my.fctriPad 2:biddable`                                    0.135317
## `prdline.my.fctriPad 3+:biddable`                                   0.635344
## `prdline.my.fctriPadAir:biddable`                                   0.205211
## `prdline.my.fctriPadmini:biddable`                                  0.696381
## `prdline.my.fctriPadmini 2+:biddable`                               0.732058
## `prdline.my.fctriPad 1:startprice.diff`                             0.002939
## `prdline.my.fctriPad 2:startprice.diff`                             0.070635
## `prdline.my.fctriPad 3+:startprice.diff`                            0.000206
## `prdline.my.fctriPadAir:startprice.diff`                            0.061156
## `prdline.my.fctriPadmini:startprice.diff`                           0.001519
## `prdline.my.fctriPadmini 2+:startprice.diff`                        0.045391
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`      0.462914
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`      0.827080
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     0.608844
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     0.518009
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    0.025620
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` 0.413041
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`      1.000000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`      1.000000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     0.705590
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`     0.466136
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    0.814399
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished` 0.995577
## `prdline.my.fctriPad 1:condition.fctrNew`                           0.576602
## `prdline.my.fctriPad 2:condition.fctrNew`                           1.000000
## `prdline.my.fctriPad 3+:condition.fctrNew`                          0.759220
## `prdline.my.fctriPadAir:condition.fctrNew`                          0.421770
## `prdline.my.fctriPadmini:condition.fctrNew`                         0.962922
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                      0.070847
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       0.894564
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       0.670442
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`      0.859440
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`      0.821578
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     0.399937
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  0.146075
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`            0.229303
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            0.924089
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           0.369256
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           0.471835
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          0.688340
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       0.369310
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                         0.357621
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         0.639110
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        0.278347
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        0.569338
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       0.251985
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    0.622305
## `prdline.my.fctriPad 1:cellular.fctr1`                              0.617966
## `prdline.my.fctriPad 2:cellular.fctr1`                              0.745073
## `prdline.my.fctriPad 3+:cellular.fctr1`                             0.084694
## `prdline.my.fctriPadAir:cellular.fctr1`                             0.733099
## `prdline.my.fctriPadmini:cellular.fctr1`                            0.059209
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                         0.216378
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                        0.824367
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        0.734088
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       0.235116
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                       0.968185
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                      0.839589
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                   0.866515
##                                                                        
## (Intercept)                                                         ***
## `prdline.my.fctriPad 1`                                                
## `prdline.my.fctriPad 2`                                                
## `prdline.my.fctriPad 3+`                                               
## prdline.my.fctriPadAir                                                 
## prdline.my.fctriPadmini                                                
## `prdline.my.fctriPadmini 2+`                                           
## biddable                                                            ***
## startprice.diff                                                     ** 
## `condition.fctrFor parts or not working`                               
## `condition.fctrManufacturer refurbished`                               
## condition.fctrNew                                                      
## `condition.fctrNew other (see details)`                                
## `condition.fctrSeller refurbished`                                     
## D.terms.n.post.stop                                                    
## cellular.fctr1                                                         
## cellular.fctrUnknown                                                   
## `prdline.my.fctrUnknown:.clusterid.fctr2`                              
## `prdline.my.fctriPad 1:.clusterid.fctr2`                               
## `prdline.my.fctriPad 2:.clusterid.fctr2`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                              
## `prdline.my.fctriPadAir:.clusterid.fctr2`                              
## `prdline.my.fctriPadmini:.clusterid.fctr2`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                          
## `prdline.my.fctrUnknown:.clusterid.fctr3`                              
## `prdline.my.fctriPad 1:.clusterid.fctr3`                               
## `prdline.my.fctriPad 2:.clusterid.fctr3`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                              
## `prdline.my.fctriPadAir:.clusterid.fctr3`                              
## `prdline.my.fctriPadmini:.clusterid.fctr3`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                          
## `prdline.my.fctrUnknown:.clusterid.fctr4`                              
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            *  
## `prdline.my.fctriPad 2:.clusterid.fctr4`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                           .  
## `prdline.my.fctriPadAir:.clusterid.fctr4`                              
## `prdline.my.fctriPadmini:.clusterid.fctr4`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                          
## `prdline.my.fctrUnknown:.clusterid.fctr5`                              
## `prdline.my.fctriPad 1:.clusterid.fctr5`                               
## `prdline.my.fctriPad 2:.clusterid.fctr5`                            ** 
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                              
## `prdline.my.fctriPadAir:.clusterid.fctr5`                              
## `prdline.my.fctriPadmini:.clusterid.fctr5`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                          
## `prdline.my.fctriPad 1:biddable`                                    .  
## `prdline.my.fctriPad 2:biddable`                                       
## `prdline.my.fctriPad 3+:biddable`                                      
## `prdline.my.fctriPadAir:biddable`                                      
## `prdline.my.fctriPadmini:biddable`                                     
## `prdline.my.fctriPadmini 2+:biddable`                                  
## `prdline.my.fctriPad 1:startprice.diff`                             ** 
## `prdline.my.fctriPad 2:startprice.diff`                             .  
## `prdline.my.fctriPad 3+:startprice.diff`                            ***
## `prdline.my.fctriPadAir:startprice.diff`                            .  
## `prdline.my.fctriPadmini:startprice.diff`                           ** 
## `prdline.my.fctriPadmini 2+:startprice.diff`                        *  
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`         
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`         
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`        
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`        
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    *  
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`    
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`         
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`         
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`        
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`        
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`       
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`    
## `prdline.my.fctriPad 1:condition.fctrNew`                              
## `prdline.my.fctriPad 2:condition.fctrNew`                              
## `prdline.my.fctriPad 3+:condition.fctrNew`                             
## `prdline.my.fctriPadAir:condition.fctrNew`                             
## `prdline.my.fctriPadmini:condition.fctrNew`                            
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                      .  
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`          
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`          
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`         
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`         
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`        
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`     
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`               
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`               
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`             
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`          
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                           
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                           
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                          
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                       
## `prdline.my.fctriPad 1:cellular.fctr1`                                 
## `prdline.my.fctriPad 2:cellular.fctr1`                                 
## `prdline.my.fctriPad 3+:cellular.fctr1`                             .  
## `prdline.my.fctriPadAir:cellular.fctr1`                                
## `prdline.my.fctriPadmini:cellular.fctr1`                            .  
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                            
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                           
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                           
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                          
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                          
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                         
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  677.67  on 869  degrees of freedom
## AIC: 887.67
## 
## Number of Fisher Scoring iterations: 16
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-69.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7327227
## 3        0.2 0.7876520
## 4        0.3 0.8134557
## 5        0.4 0.8311404
## 6        0.5 0.8252874
## 7        0.6 0.8210024
## 8        0.7 0.8085106
## 9        0.8 0.7547170
## 10       0.9 0.6345865
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-70.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.bayesglm.N
## 1         N                              441
## 2         Y                               71
##   sold.fctr.predict.csm.bayesglm.Y
## 1                               83
## 2                              379
##          Prediction
## Reference   N   Y
##         N 441  83
##         Y  71 379
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.418891e-01   6.825401e-01   8.174354e-01   8.642544e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   7.796162e-90   3.753992e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-71.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7272727
## 3        0.2 0.7439410
## 4        0.3 0.7659091
## 5        0.4 0.7849332
## 6        0.5 0.7893401
## 7        0.6 0.7973684
## 8        0.7 0.7867036
## 9        0.8 0.7368421
## 10       0.9 0.5966667
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-72.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.bayesglm.N
## 1         N                              428
## 2         Y                              107
##   sold.fctr.predict.csm.bayesglm.Y
## 1                               47
## 2                              303
##          Prediction
## Reference   N   Y
##         N 428  47
##         Y 107 303
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.259887e-01   6.465508e-01   7.993726e-01   8.504130e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.992247e-73   1.990805e-06 
##       model_id model_method
## 1 csm.bayesglm     bayesglm
##                                                                                                                                                                                                             feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.398                 0.451
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9218321                    0.4       0.8311404        0.8059576
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8174354             0.8642544     0.6084395   0.8751682
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7973684        0.8259887
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7993726              0.850413     0.6465508    887.6748
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02013466      0.04297055
##                              N importance
## biddable            100.000000 100.000000
## startprice.diff      74.468483  74.468483
## prdline.my.fctr      34.103141  34.103141
## .clusterid.fctr      12.101557  12.101557
## D.terms.n.post.stop   6.618312   6.618312
## cellular.fctr         5.114660   5.114660
## [1] "fitting model: csm.glmnet"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 0.1, lambda = 0.0544 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: alpha
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: lambda
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-73.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-74.png) 

```
##             Length Class      Mode     
## a0            100  -none-     numeric  
## beta        10400  dgCMatrix  S4       
## df            100  -none-     numeric  
## dim             2  -none-     numeric  
## lambda        100  -none-     numeric  
## dev.ratio     100  -none-     numeric  
## nulldev         1  -none-     numeric  
## npasses         1  -none-     numeric  
## jerr            1  -none-     numeric  
## offset          1  -none-     logical  
## classnames      2  -none-     character
## call            5  -none-     call     
## nobs            1  -none-     numeric  
## lambdaOpt       1  -none-     numeric  
## xNames        104  -none-     character
## problemType     1  -none-     character
## tuneValue       2  data.frame list     
## obsLevels       2  -none-     character
## [1] "min lambda > lambdaOpt:"
##                                                       (Intercept) 
##                                                      -0.785126522 
##                                             prdline.my.fctriPad 1 
##                                                       0.094864050 
##                                            prdline.my.fctriPad 3+ 
##                                                       0.132318552 
##                                                          biddable 
##                                                       1.657612909 
##                                                   startprice.diff 
##                                                      -0.005101683 
##                            condition.fctrFor parts or not working 
##                                                       0.051728598 
##                                                 condition.fctrNew 
##                                                      -0.280672613 
##                             condition.fctrNew other (see details) 
##                                                       0.083501269 
##                                  condition.fctrSeller refurbished 
##                                                      -0.102950458 
##                                               D.terms.n.post.stop 
##                                                      -0.019918996 
##                                              cellular.fctrUnknown 
##                                                      -0.344710052 
##                           prdline.my.fctrUnknown:.clusterid.fctr2 
##                                                       0.333916029 
##                            prdline.my.fctriPad 1:.clusterid.fctr2 
##                                                       0.254154885 
##                           prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                                      -0.189166781 
##                           prdline.my.fctriPadAir:.clusterid.fctr2 
##                                                      -0.293689519 
##                          prdline.my.fctriPadmini:.clusterid.fctr2 
##                                                       0.241456856 
##                           prdline.my.fctrUnknown:.clusterid.fctr3 
##                                                      -0.542249391 
##                            prdline.my.fctriPad 2:.clusterid.fctr3 
##                                                      -1.351549327 
##                          prdline.my.fctriPadmini:.clusterid.fctr3 
##                                                       0.325734603 
##                            prdline.my.fctriPad 1:.clusterid.fctr4 
##                                                      -0.816555954 
##                           prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                                       0.447582131 
##                           prdline.my.fctriPadAir:.clusterid.fctr4 
##                                                      -0.031134475 
##                          prdline.my.fctriPadmini:.clusterid.fctr4 
##                                                      -0.031709207 
##                            prdline.my.fctriPad 2:.clusterid.fctr5 
##                                                       1.624126021 
##                                    prdline.my.fctriPad 1:biddable 
##                                                       0.868744927 
##                                    prdline.my.fctriPad 2:biddable 
##                                                       0.867484185 
##                                   prdline.my.fctriPad 3+:biddable 
##                                                       0.532596792 
##                                   prdline.my.fctriPadAir:biddable 
##                                                       0.871716398 
##                                  prdline.my.fctriPadmini:biddable 
##                                                       0.517827280 
##                               prdline.my.fctriPadmini 2+:biddable 
##                                                       0.357321439 
##                             prdline.my.fctriPad 1:startprice.diff 
##                                                      -0.014585870 
##                             prdline.my.fctriPad 2:startprice.diff 
##                                                      -0.003913650 
##                            prdline.my.fctriPad 3+:startprice.diff 
##                                                      -0.007533428 
##                            prdline.my.fctriPadAir:startprice.diff 
##                                                      -0.002981635 
##                           prdline.my.fctriPadmini:startprice.diff 
##                                                      -0.008857312 
##                        prdline.my.fctriPadmini 2+:startprice.diff 
##                                                      -0.003308179 
##      prdline.my.fctriPad 1:condition.fctrFor parts or not working 
##                                                       1.325865083 
##      prdline.my.fctriPad 2:condition.fctrFor parts or not working 
##                                                       0.150099097 
##    prdline.my.fctriPadmini:condition.fctrFor parts or not working 
##                                                      -0.380551440 
## prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working 
##                                                      -0.887424697 
##     prdline.my.fctriPadAir:condition.fctrManufacturer refurbished 
##                                                       0.393103261 
##                           prdline.my.fctriPad 1:condition.fctrNew 
##                                                      -0.551635876 
##                          prdline.my.fctriPad 3+:condition.fctrNew 
##                                                      -0.148551661 
##                      prdline.my.fctriPadmini 2+:condition.fctrNew 
##                                                       0.064426169 
##       prdline.my.fctriPad 2:condition.fctrNew other (see details) 
##                                                      -0.403291726 
##      prdline.my.fctriPad 3+:condition.fctrNew other (see details) 
##                                                       0.602952762 
##      prdline.my.fctriPadAir:condition.fctrNew other (see details) 
##                                                       0.340466958 
##     prdline.my.fctriPadmini:condition.fctrNew other (see details) 
##                                                      -0.051791987 
##  prdline.my.fctriPadmini 2+:condition.fctrNew other (see details) 
##                                                      -0.592682408 
##            prdline.my.fctriPad 1:condition.fctrSeller refurbished 
##                                                       0.913759705 
##           prdline.my.fctriPad 3+:condition.fctrSeller refurbished 
##                                                      -0.396171064 
##           prdline.my.fctriPadAir:condition.fctrSeller refurbished 
##                                                      -0.229905145 
##          prdline.my.fctriPadmini:condition.fctrSeller refurbished 
##                                                      -0.258933947 
##       prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished 
##                                                      -1.136087129 
##                         prdline.my.fctriPad 1:D.terms.n.post.stop 
##                                                       0.010744828 
##                        prdline.my.fctriPad 3+:D.terms.n.post.stop 
##                                                      -0.022428598 
##                        prdline.my.fctriPadAir:D.terms.n.post.stop 
##                                                      -0.019761785 
##                              prdline.my.fctriPad 2:cellular.fctr1 
##                                                      -0.352872696 
##                             prdline.my.fctriPad 3+:cellular.fctr1 
##                                                       0.171655188 
##                             prdline.my.fctriPadAir:cellular.fctr1 
##                                                      -0.104655936 
##                            prdline.my.fctriPadmini:cellular.fctr1 
##                                                       0.426749577 
##                        prdline.my.fctriPad 2:cellular.fctrUnknown 
##                                                      -0.343388715 
##                       prdline.my.fctriPad 3+:cellular.fctrUnknown 
##                                                      -0.599203821 
##                      prdline.my.fctriPadmini:cellular.fctrUnknown 
##                                                      -0.108462300 
## [1] "max lambda < lambdaOpt:"
##                                                       (Intercept) 
##                                                     -1.908097e+00 
##                                             prdline.my.fctriPad 1 
##                                                      9.232560e-01 
##                                             prdline.my.fctriPad 2 
##                                                      1.448326e-01 
##                                            prdline.my.fctriPad 3+ 
##                                                      1.118741e+00 
##                                            prdline.my.fctriPadAir 
##                                                      7.269006e-01 
##                                           prdline.my.fctriPadmini 
##                                                      1.139041e+00 
##                                        prdline.my.fctriPadmini 2+ 
##                                                      4.467221e-02 
##                                                          biddable 
##                                                      3.220985e+00 
##                                                   startprice.diff 
##                                                     -8.122717e-03 
##                            condition.fctrFor parts or not working 
##                                                      1.279546e+00 
##                            condition.fctrManufacturer refurbished 
##                                                     -5.342958e-05 
##                                                 condition.fctrNew 
##                                                     -7.649789e-01 
##                             condition.fctrNew other (see details) 
##                                                      4.292654e+00 
##                                  condition.fctrSeller refurbished 
##                                                      4.381645e-01 
##                                               D.terms.n.post.stop 
##                                                      1.054294e-02 
##                                                    cellular.fctr1 
##                                                     -2.090343e+00 
##                                              cellular.fctrUnknown 
##                                                     -8.354327e-01 
##                           prdline.my.fctrUnknown:.clusterid.fctr2 
##                                                      1.299903e+00 
##                            prdline.my.fctriPad 1:.clusterid.fctr2 
##                                                     -2.375268e+00 
##                            prdline.my.fctriPad 2:.clusterid.fctr2 
##                                                      6.198774e-01 
##                           prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                                     -3.787361e-01 
##                           prdline.my.fctriPadAir:.clusterid.fctr2 
##                                                     -8.324500e-01 
##                          prdline.my.fctriPadmini:.clusterid.fctr2 
##                                                      1.617222e+00 
##                       prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                                      3.656829e+00 
##                           prdline.my.fctrUnknown:.clusterid.fctr3 
##                                                     -1.066430e+00 
##                            prdline.my.fctriPad 1:.clusterid.fctr3 
##                                                     -3.971714e+00 
##                            prdline.my.fctriPad 2:.clusterid.fctr3 
##                                                     -5.139483e+00 
##                           prdline.my.fctriPad 3+:.clusterid.fctr3 
##                                                      1.215954e+00 
##                           prdline.my.fctriPadAir:.clusterid.fctr3 
##                                                      5.705059e-01 
##                          prdline.my.fctriPadmini:.clusterid.fctr3 
##                                                      1.771634e+00 
##                       prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                                      1.906954e+00 
##                            prdline.my.fctriPad 1:.clusterid.fctr4 
##                                                     -4.760242e+00 
##                            prdline.my.fctriPad 2:.clusterid.fctr4 
##                                                      1.055194e+00 
##                           prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                                      2.105022e+00 
##                           prdline.my.fctriPadAir:.clusterid.fctr4 
##                                                     -2.281940e-01 
##                          prdline.my.fctriPadmini:.clusterid.fctr4 
##                                                      1.125813e+00 
##                            prdline.my.fctriPad 2:.clusterid.fctr5 
##                                                      4.556858e+00 
##                          prdline.my.fctriPadmini:.clusterid.fctr5 
##                                                      1.186111e+00 
##                                    prdline.my.fctriPad 1:biddable 
##                                                      2.166591e+00 
##                                    prdline.my.fctriPad 2:biddable 
##                                                      1.301508e+00 
##                                   prdline.my.fctriPad 3+:biddable 
##                                                      4.684927e-01 
##                                   prdline.my.fctriPadAir:biddable 
##                                                      8.779222e-01 
##                                  prdline.my.fctriPadmini:biddable 
##                                                      2.760250e-01 
##                               prdline.my.fctriPadmini 2+:biddable 
##                                                      4.829626e-01 
##                             prdline.my.fctriPad 1:startprice.diff 
##                                                     -5.325816e-02 
##                             prdline.my.fctriPad 2:startprice.diff 
##                                                     -1.227553e-02 
##                            prdline.my.fctriPad 3+:startprice.diff 
##                                                     -2.327921e-02 
##                            prdline.my.fctriPadAir:startprice.diff 
##                                                     -7.176673e-03 
##                           prdline.my.fctriPadmini:startprice.diff 
##                                                     -2.013394e-02 
##                        prdline.my.fctriPadmini 2+:startprice.diff 
##                                                     -1.442104e-02 
##      prdline.my.fctriPad 1:condition.fctrFor parts or not working 
##                                                     -4.704473e-01 
##      prdline.my.fctriPad 2:condition.fctrFor parts or not working 
##                                                     -1.496551e-01 
##     prdline.my.fctriPad 3+:condition.fctrFor parts or not working 
##                                                     -1.085197e+00 
##     prdline.my.fctriPadAir:condition.fctrFor parts or not working 
##                                                     -1.504022e+00 
##    prdline.my.fctriPadmini:condition.fctrFor parts or not working 
##                                                     -2.564908e+00 
## prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working 
##                                                     -6.463523e+00 
##     prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished 
##                                                     -7.064960e-01 
##     prdline.my.fctriPadAir:condition.fctrManufacturer refurbished 
##                                                      1.159809e+00 
##    prdline.my.fctriPadmini:condition.fctrManufacturer refurbished 
##                                                     -4.682701e-01 
##                           prdline.my.fctriPad 1:condition.fctrNew 
##                                                     -4.939381e+00 
##                          prdline.my.fctriPad 3+:condition.fctrNew 
##                                                     -5.891387e-01 
##                          prdline.my.fctriPadAir:condition.fctrNew 
##                                                      6.771964e-01 
##                         prdline.my.fctriPadmini:condition.fctrNew 
##                                                     -1.263002e-01 
##                      prdline.my.fctriPadmini 2+:condition.fctrNew 
##                                                      1.897538e+00 
##       prdline.my.fctriPad 1:condition.fctrNew other (see details) 
##                                                     -4.851107e+00 
##       prdline.my.fctriPad 2:condition.fctrNew other (see details) 
##                                                     -7.094134e+00 
##      prdline.my.fctriPad 3+:condition.fctrNew other (see details) 
##                                                     -2.659737e+00 
##      prdline.my.fctriPadAir:condition.fctrNew other (see details) 
##                                                     -2.584542e+00 
##     prdline.my.fctriPadmini:condition.fctrNew other (see details) 
##                                                     -4.492406e+00 
##  prdline.my.fctriPadmini 2+:condition.fctrNew other (see details) 
##                                                     -5.199392e+00 
##            prdline.my.fctriPad 1:condition.fctrSeller refurbished 
##                                                      1.446224e+00 
##            prdline.my.fctriPad 2:condition.fctrSeller refurbished 
##                                                     -7.936695e-01 
##           prdline.my.fctriPad 3+:condition.fctrSeller refurbished 
##                                                     -1.910234e+00 
##           prdline.my.fctriPadAir:condition.fctrSeller refurbished 
##                                                     -1.773017e+00 
##          prdline.my.fctriPadmini:condition.fctrSeller refurbished 
##                                                     -1.217800e+00 
##       prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished 
##                                                     -6.713347e+00 
##                         prdline.my.fctriPad 1:D.terms.n.post.stop 
##                                                      2.923420e-01 
##                         prdline.my.fctriPad 2:D.terms.n.post.stop 
##                                                     -9.017609e-02 
##                        prdline.my.fctriPad 3+:D.terms.n.post.stop 
##                                                     -1.478048e-01 
##                        prdline.my.fctriPadAir:D.terms.n.post.stop 
##                                                     -6.942920e-02 
##                       prdline.my.fctriPadmini:D.terms.n.post.stop 
##                                                     -1.980418e-01 
##                    prdline.my.fctriPadmini 2+:D.terms.n.post.stop 
##                                                     -2.208570e-01 
##                              prdline.my.fctriPad 1:cellular.fctr1 
##                                                      1.927842e+00 
##                              prdline.my.fctriPad 2:cellular.fctr1 
##                                                      1.203964e+00 
##                             prdline.my.fctriPad 3+:cellular.fctr1 
##                                                      3.211938e+00 
##                             prdline.my.fctriPadAir:cellular.fctr1 
##                                                      1.830077e+00 
##                            prdline.my.fctriPadmini:cellular.fctr1 
##                                                      3.253492e+00 
##                         prdline.my.fctriPadmini 2+:cellular.fctr1 
##                                                      2.953754e+00 
##                        prdline.my.fctriPad 1:cellular.fctrUnknown 
##                                                     -8.405316e-02 
##                        prdline.my.fctriPad 2:cellular.fctrUnknown 
##                                                     -4.649041e-01 
##                       prdline.my.fctriPad 3+:cellular.fctrUnknown 
##                                                     -1.397003e+00 
##                       prdline.my.fctriPadAir:cellular.fctrUnknown 
##                                                      2.442932e-01 
##                      prdline.my.fctriPadmini:cellular.fctrUnknown 
##                                                     -1.249146e-01 
##                   prdline.my.fctriPadmini 2+:cellular.fctrUnknown 
##                                                      2.555720e-01 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-75.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6671609
## 3        0.2 0.7199341
## 4        0.3 0.7785235
## 5        0.4 0.7986871
## 6        0.5 0.8098016
## 7        0.6 0.8123457
## 8        0.7 0.7635403
## 9        0.8 0.5975232
## 10       0.9 0.1247401
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-76.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.glmnet.N sold.fctr.predict.csm.glmnet.Y
## 1         N                            493                             31
## 2         Y                            121                            329
##          Prediction
## Reference   N   Y
##         N 493  31
##         Y 121 329
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.439425e-01   6.815761e-01   8.196035e-01   8.661802e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.675019e-91   5.242780e-13 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-77.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6710744
## 3        0.2 0.7191217
## 4        0.3 0.7550802
## 5        0.4 0.7711138
## 6        0.5 0.7802341
## 7        0.6 0.7925033
## 8        0.7 0.7334315
## 9        0.8 0.5752961
## 10       0.9 0.1431767
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-78.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.glmnet.N sold.fctr.predict.csm.glmnet.Y
## 1         N                            434                             41
## 2         Y                            114                            296
##          Prediction
## Reference   N   Y
##         N 434  41
##         Y 114 296
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.248588e-01   6.434744e-01   7.981829e-01   8.493505e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   8.130489e-73   7.330192e-09 
##     model_id model_method
## 1 csm.glmnet       glmnet
##                                                                                                                                                                                                             feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      3.011                 0.356
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.908626                    0.6       0.8123457        0.8080089
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8196035             0.8661802     0.6119731   0.8701669
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7925033        0.8248588
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7981829             0.8493505     0.6434744
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01385701      0.03112462
##                                                              importance
## biddable                                                      100.00000
## prdline.my.fctriPad 2:.clusterid.fctr5                         98.96605
## prdline.my.fctriPad 1:condition.fctrFor parts or not working   89.00671
## prdline.my.fctriPad 1:condition.fctrSeller refurbished         75.34252
## prdline.my.fctriPadAir:biddable                                73.91272
## prdline.my.fctriPad 1:biddable                                 73.81033
## [1] "fitting model: csm.rpart"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00833 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-79.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-80.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.148888889      1 0.4888889
## 3 0.008333333      2 0.3400000
## 
## Variable importance
##                                   biddable 
##                                         34 
##                            startprice.diff 
##                                         21 
##            prdline.my.fctriPad 3+:biddable 
##                                          6 
##            prdline.my.fctriPadAir:biddable 
##                                          6 
##             prdline.my.fctriPad 1:biddable 
##                                          6 
##           prdline.my.fctriPadmini:biddable 
##                                          5 
##     prdline.my.fctriPadAir:startprice.diff 
##                                          5 
##             prdline.my.fctriPad 2:biddable 
##                                          5 
##     prdline.my.fctriPad 3+:startprice.diff 
##                                          4 
## prdline.my.fctriPadmini 2+:startprice.diff 
##                                          3 
##    prdline.my.fctriPadmini:startprice.diff 
##                                          3 
##      prdline.my.fctriPad 2:startprice.diff 
##                                          2 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable                        < 0.5      to the left,  improve=144.14990, (0 missing)
##       startprice.diff                 < 44.18097 to the right, improve= 78.49785, (0 missing)
##       prdline.my.fctriPad 1:biddable  < 0.5      to the left,  improve= 25.37925, (0 missing)
##       prdline.my.fctriPad 2:biddable  < 0.5      to the left,  improve= 22.83949, (0 missing)
##       prdline.my.fctriPadAir:biddable < 0.5      to the left,  improve= 16.00553, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPad 3+:biddable  < 0.5      to the left,  agree=0.624, adj=0.187, (0 split)
##       prdline.my.fctriPadAir:biddable  < 0.5      to the left,  agree=0.618, adj=0.173, (0 split)
##       prdline.my.fctriPad 1:biddable   < 0.5      to the left,  agree=0.613, adj=0.162, (0 split)
##       prdline.my.fctriPadmini:biddable < 0.5      to the left,  agree=0.611, adj=0.158, (0 split)
##       prdline.my.fctriPad 2:biddable   < 0.5      to the left,  agree=0.606, adj=0.147, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.1488889
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (89 obs) right son=7 (361 obs)
##   Primary splits:
##       startprice.diff                            < 84.72197 to the right, improve=88.61445, (0 missing)
##       prdline.my.fctriPad 3+:startprice.diff     < 90.53641 to the right, improve=20.17141, (0 missing)
##       prdline.my.fctriPadmini:startprice.diff    < 54.43284 to the right, improve=13.52563, (0 missing)
##       prdline.my.fctriPadAir:startprice.diff     < 102.8707 to the right, improve=11.78578, (0 missing)
##       prdline.my.fctriPadmini 2+:startprice.diff < 138.451  to the right, improve=11.67677, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir:startprice.diff     < 92.45617 to the right, agree=0.851, adj=0.247, (0 split)
##       prdline.my.fctriPad 3+:startprice.diff     < 90.53641 to the right, agree=0.840, adj=0.191, (0 split)
##       prdline.my.fctriPadmini:startprice.diff    < 88.75737 to the right, agree=0.827, adj=0.124, (0 split)
##       prdline.my.fctriPadmini 2+:startprice.diff < 105.0949 to the right, agree=0.827, adj=0.124, (0 split)
##       prdline.my.fctriPad 2:startprice.diff      < 76.28875 to the right, agree=0.818, adj=0.079, (0 split)
## 
## Node number 6: 89 observations
##   predicted class=N  expected loss=0.1235955  P(node) =0.09137577
##     class counts:    78    11
##    probabilities: 0.876 0.124 
## 
## Node number 7: 361 observations
##   predicted class=Y  expected loss=0.08864266  P(node) =0.3706366
##     class counts:    32   329
##    probabilities: 0.089 0.911 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.53798768 0.46201232)  
##   2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##   3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##     6) startprice.diff>=84.72197 89  11 N (0.87640449 0.12359551) *
##     7) startprice.diff< 84.72197 361  32 Y (0.08864266 0.91135734) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-81.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6576779
## 4        0.3 0.8113440
## 5        0.4 0.8113440
## 6        0.5 0.8113440
## 7        0.6 0.8113440
## 8        0.7 0.8113440
## 9        0.8 0.8113440
## 10       0.9 0.8113440
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-82.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.rpart.N sold.fctr.predict.csm.rpart.Y
## 1         N                           492                            32
## 2         Y                           121                           329
##          Prediction
## Reference   N   Y
##         N 492  32
##         Y 121 329
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.429158e-01   6.795322e-01   8.185193e-01   8.652175e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.699210e-90   1.124184e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-83.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6515397
## 4        0.3 0.7934783
## 5        0.4 0.7934783
## 6        0.5 0.7934783
## 7        0.6 0.7934783
## 8        0.7 0.7934783
## 9        0.8 0.7934783
## 10       0.9 0.7934783
## 11       1.0 0.0000000
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-84.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.rpart.N sold.fctr.predict.csm.rpart.Y
## 1         N                           441                            34
## 2         Y                           118                           292
##          Prediction
## Reference   N   Y
##         N 441  34
##         Y 118 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.282486e-01   6.497240e-01   8.017531e-01   8.525367e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.168374e-74   1.671294e-11 
##    model_id model_method
## 1 csm.rpart        rpart
##                                                                                                                                                                                                             feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.792                 0.094
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8435581                    0.9        0.811344         0.841912
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8185193             0.8652175     0.6771481   0.8273068
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.7934783        0.8282486
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8017531             0.8525367      0.649724
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02468137       0.0529597
##                                        importance
## startprice.diff                         100.00000
## biddable                                 86.25929
## prdline.my.fctriPad 1:biddable           15.18694
## prdline.my.fctriPad 2:biddable           13.66715
## prdline.my.fctriPad 3+:startprice.diff   12.07057
## prdline.my.fctriPadAir:biddable           9.57771
## [1] "fitting model: csm.rf"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 53 on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-85.png) ![](ebayipads_spdiff_files/figure-html/fit.models_1-86.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        974   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           1948   matrix     numeric  
## oob.times        974   -none-     numeric  
## classes            2   -none-     character
## importance       104   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           104   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-87.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8628955
## 3        0.2 0.9384776
## 4        0.3 0.9761388
## 5        0.4 0.9966777
## 6        0.5 0.9988901
## 7        0.6 0.9910314
## 8        0.7 0.9374262
## 9        0.8 0.8805970
## 10       0.9 0.8235294
## 11       1.0 0.5123967
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-88.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.rf.N sold.fctr.predict.csm.rf.Y
## 1         N                        523                          1
## 2         Y                         NA                        450
##          Prediction
## Reference   N   Y
##         N 523   1
##         Y   0 450
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.989733e-01   9.979350e-01   9.942930e-01   9.999740e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##  4.956882e-260   1.000000e+00 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-89.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.7381890
## 3        0.2 0.7705443
## 4        0.3 0.7981962
## 5        0.4 0.8080094
## 6        0.5 0.8184080
## 7        0.6 0.8125000
## 8        0.7 0.7896879
## 9        0.8 0.7752809
## 10       0.9 0.7543860
## 11       1.0 0.3922330
```

![](ebayipads_spdiff_files/figure-html/fit.models_1-90.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.rf.N sold.fctr.predict.csm.rf.Y
## 1         N                        410                         65
## 2         Y                         81                        329
##          Prediction
## Reference   N   Y
##         N 410  65
##         Y  81 329
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.350282e-01   6.673789e-01   8.089040e-01   8.588985e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.946649e-78   2.144548e-01 
##   model_id model_method
## 1   csm.rf           rf
##                                                                                                                                                                                                             feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     16.848                 5.689
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9999915                    0.5       0.9988901        0.8305983
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.994293              0.999974     0.6567546   0.8875302
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5        0.818408        0.8350282
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.808904             0.8588985     0.6673789
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02013977      0.04149673
##                                         importance
## startprice.diff                         100.000000
## biddable                                 88.327674
## prdline.my.fctriPadAir:startprice.diff   11.945378
## D.terms.n.post.stop                       9.579481
## prdline.my.fctriPad 3+:startprice.diff    8.795106
## prdline.my.fctriPadmini:startprice.diff   8.522285
```

```r
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
##                                                    model_id
## MFO.myMFO_classfr                         MFO.myMFO_classfr
## Random.myrandom_classfr             Random.myrandom_classfr
## Max.cor.Y.cv.0.rpart                   Max.cor.Y.cv.0.rpart
## Max.cor.Y.cv.0.cp.0.rpart         Max.cor.Y.cv.0.cp.0.rpart
## Max.cor.Y.rpart                             Max.cor.Y.rpart
## Max.cor.Y.glm                                 Max.cor.Y.glm
## Interact.High.cor.Y.glm             Interact.High.cor.Y.glm
## Low.cor.X.glm                                 Low.cor.X.glm
## All.X.glm                                         All.X.glm
## All.X.bayesglm                               All.X.bayesglm
## All.X.glmnet                                   All.X.glmnet
## All.X.no.rnorm.rpart                   All.X.no.rnorm.rpart
## All.X.no.rnorm.rf                         All.X.no.rnorm.rf
## All.Interact.X.glm                       All.Interact.X.glm
## All.Interact.X.bayesglm             All.Interact.X.bayesglm
## All.Interact.X.glmnet                 All.Interact.X.glmnet
## All.Interact.X.no.rnorm.rpart All.Interact.X.no.rnorm.rpart
## All.Interact.X.no.rnorm.rf       All.Interact.X.no.rnorm.rf
## csm.glm                                             csm.glm
## csm.bayesglm                                   csm.bayesglm
## csm.glmnet                                       csm.glmnet
## csm.rpart                                         csm.rpart
## csm.rf                                               csm.rf
##                                   model_method
## MFO.myMFO_classfr                myMFO_classfr
## Random.myrandom_classfr       myrandom_classfr
## Max.cor.Y.cv.0.rpart                     rpart
## Max.cor.Y.cv.0.cp.0.rpart                rpart
## Max.cor.Y.rpart                          rpart
## Max.cor.Y.glm                              glm
## Interact.High.cor.Y.glm                    glm
## Low.cor.X.glm                              glm
## All.X.glm                                  glm
## All.X.bayesglm                        bayesglm
## All.X.glmnet                            glmnet
## All.X.no.rnorm.rpart                     rpart
## All.X.no.rnorm.rf                           rf
## All.Interact.X.glm                         glm
## All.Interact.X.bayesglm               bayesglm
## All.Interact.X.glmnet                   glmnet
## All.Interact.X.no.rnorm.rpart            rpart
## All.Interact.X.no.rnorm.rf                  rf
## csm.glm                                    glm
## csm.bayesglm                          bayesglm
## csm.glmnet                              glmnet
## csm.rpart                                rpart
## csm.rf                                      rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             biddable, startprice.diff
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        biddable, startprice.diff
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.diff
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.diff
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                                                                                                                                biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glm                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glm            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.bayesglm       D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glmnet         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rpart         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rf            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## csm.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.bayesglm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.glmnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rf                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##                               max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                           0                      0.417
## Random.myrandom_classfr                     0                      0.282
## Max.cor.Y.cv.0.rpart                        0                      0.634
## Max.cor.Y.cv.0.cp.0.rpart                   0                      0.480
## Max.cor.Y.rpart                             3                      1.057
## Max.cor.Y.glm                               1                      0.971
## Interact.High.cor.Y.glm                     1                      1.050
## Low.cor.X.glm                               1                      1.386
## All.X.glm                                   1                      1.571
## All.X.bayesglm                              1                      2.587
## All.X.glmnet                                9                      5.217
## All.X.no.rnorm.rpart                        3                      1.673
## All.X.no.rnorm.rf                           3                     15.404
## All.Interact.X.glm                          1                      1.836
## All.Interact.X.bayesglm                     1                      2.178
## All.Interact.X.glmnet                       9                      6.260
## All.Interact.X.no.rnorm.rpart               3                      1.739
## All.Interact.X.no.rnorm.rf                  3                     18.267
## csm.glm                                     1                      1.908
## csm.bayesglm                                1                      2.398
## csm.glmnet                                  9                      3.011
## csm.rpart                                   3                      1.792
## csm.rf                                      3                     16.848
##                               min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                             0.003   0.5000000
## Random.myrandom_classfr                       0.002   0.5071756
## Max.cor.Y.cv.0.rpart                          0.012   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                     0.009   0.9051018
## Max.cor.Y.rpart                               0.011   0.8435581
## Max.cor.Y.glm                                 0.013   0.8706955
## Interact.High.cor.Y.glm                       0.015   0.8716794
## Low.cor.X.glm                                 0.168   0.9063401
## All.X.glm                                     0.250   0.9130577
## All.X.bayesglm                                0.365   0.9103774
## All.X.glmnet                                  0.961   0.8773155
## All.X.no.rnorm.rpart                          0.063   0.8435581
## All.X.no.rnorm.rf                             4.801   1.0000000
## All.Interact.X.glm                            0.373   0.9285072
## All.Interact.X.bayesglm                       0.505   0.9261281
## All.Interact.X.glmnet                         1.460   0.9132994
## All.Interact.X.no.rnorm.rpart                 0.077   0.8435581
## All.Interact.X.no.rnorm.rf                    5.426   1.0000000
## csm.glm                                       0.306   0.9246141
## csm.bayesglm                                  0.451   0.9218321
## csm.glmnet                                    0.356   0.9086260
## csm.rpart                                     0.094   0.8435581
## csm.rf                                        5.689   0.9999915
##                               opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6320225
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.6       0.8538283
## Max.cor.Y.rpart                                  0.9       0.8113440
## Max.cor.Y.glm                                    0.7       0.7906404
## Interact.High.cor.Y.glm                          0.7       0.7915633
## Low.cor.X.glm                                    0.5       0.8123570
## All.X.glm                                        0.4       0.8217391
## All.X.bayesglm                                   0.6       0.8238095
## All.X.glmnet                                     0.6       0.8048485
## All.X.no.rnorm.rpart                             0.9       0.8113440
## All.X.no.rnorm.rf                                0.6       1.0000000
## All.Interact.X.glm                               0.4       0.8396125
## All.Interact.X.bayesglm                          0.5       0.8388571
## All.Interact.X.glmnet                            0.5       0.8227115
## All.Interact.X.no.rnorm.rpart                    0.9       0.8113440
## All.Interact.X.no.rnorm.rf                       0.5       1.0000000
## csm.glm                                          0.4       0.8315098
## csm.bayesglm                                     0.4       0.8311404
## csm.glmnet                                       0.6       0.8123457
## csm.rpart                                        0.9       0.8113440
## csm.rf                                           0.5       0.9988901
##                               max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                    0.5379877             0.5060896
## Random.myrandom_classfr              0.4620123             0.4303445
## Max.cor.Y.cv.0.rpart                 0.5379877             0.5060896
## Max.cor.Y.cv.0.cp.0.rpart            0.8706366             0.8479228
## Max.cor.Y.rpart                      0.8367648             0.8185193
## Max.cor.Y.glm                        0.7977525             0.8001376
## Interact.High.cor.Y.glm              0.7936467             0.8022954
## Low.cor.X.glm                        0.7854194             0.8066148
## All.X.glm                            0.7905476             0.8066148
## All.X.bayesglm                       0.7946660             0.8239439
## All.X.glmnet                         0.7977588             0.8098576
## All.X.no.rnorm.rpart                 0.8347135             0.8185193
## All.X.no.rnorm.rf                    0.8418803             0.9962198
## All.Interact.X.glm                   0.7844033             0.8228583
## All.Interact.X.bayesglm              0.8018519             0.8315533
## All.Interact.X.glmnet                0.8038936             0.8185193
## All.Interact.X.no.rnorm.rpart        0.8347135             0.8185193
## All.Interact.X.no.rnorm.rf           0.8460019             0.9962198
## csm.glm                              0.7884995             0.8174354
## csm.bayesglm                         0.8059576             0.8174354
## csm.glmnet                           0.8080089             0.8196035
## csm.rpart                            0.8419120             0.8185193
## csm.rf                               0.8305983             0.9942930
##                               max.AccuracyUpper.fit max.Kappa.fit
## MFO.myMFO_classfr                         0.5696555     0.0000000
## Random.myrandom_classfr                   0.4939104     0.0000000
## Max.cor.Y.cv.0.rpart                      0.5696555     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.8910807     0.7382100
## Max.cor.Y.rpart                           0.8652175     0.6685851
## Max.cor.Y.glm                             0.8488005     0.5911469
## Interact.High.cor.Y.glm                   0.8507367     0.5830988
## Low.cor.X.glm                             0.8546053     0.5673801
## All.X.glm                                 0.8546053     0.5768469
## All.X.bayesglm                            0.8700277     0.5859868
## All.X.glmnet                              0.8575034     0.5910839
## All.X.no.rnorm.rpart                      0.8652175     0.6642538
## All.X.no.rnorm.rf                         1.0000000     0.6791071
## All.Interact.X.glm                        0.8690663     0.5662735
## All.Interact.X.bayesglm                   0.8767468     0.6010074
## All.Interact.X.glmnet                     0.8652175     0.6039602
## All.Interact.X.no.rnorm.rpart             0.8652175     0.6642538
## All.Interact.X.no.rnorm.rf                1.0000000     0.6873959
## csm.glm                                   0.8642544     0.5731278
## csm.bayesglm                              0.8642544     0.6084395
## csm.glmnet                                0.8661802     0.6119731
## csm.rpart                                 0.8652175     0.6771481
## csm.rf                                    0.9999740     0.6567546
##                               max.auc.OOB opt.prob.threshold.OOB
## MFO.myMFO_classfr               0.5000000                    0.5
## Random.myrandom_classfr         0.5191913                    0.4
## Max.cor.Y.cv.0.rpart            0.5000000                    0.5
## Max.cor.Y.cv.0.cp.0.rpart       0.8812811                    0.6
## Max.cor.Y.rpart                 0.8273068                    0.9
## Max.cor.Y.glm                   0.8658999                    0.7
## Interact.High.cor.Y.glm         0.8652888                    0.7
## Low.cor.X.glm                   0.8499512                    0.7
## All.X.glm                       0.8492837                    0.6
## All.X.bayesglm                  0.8576175                    0.6
## All.X.glmnet                    0.8631682                    0.6
## All.X.no.rnorm.rpart            0.8273068                    0.9
## All.X.no.rnorm.rf               0.8876893                    0.6
## All.Interact.X.glm              0.8559230                    0.6
## All.Interact.X.bayesglm         0.8673582                    0.5
## All.Interact.X.glmnet           0.8728883                    0.6
## All.Interact.X.no.rnorm.rpart   0.8273068                    0.9
## All.Interact.X.no.rnorm.rf      0.8910295                    0.5
## csm.glm                         0.8425315                    0.5
## csm.bayesglm                    0.8751682                    0.6
## csm.glmnet                      0.8701669                    0.6
## csm.rpart                       0.8273068                    0.9
## csm.rf                          0.8875302                    0.5
##                               max.f.score.OOB max.Accuracy.OOB
## MFO.myMFO_classfr                   0.0000000        0.5367232
## Random.myrandom_classfr             0.6332046        0.4632768
## Max.cor.Y.cv.0.rpart                0.0000000        0.5367232
## Max.cor.Y.cv.0.cp.0.rpart           0.7994859        0.8237288
## Max.cor.Y.rpart                     0.7934783        0.8282486
## Max.cor.Y.glm                       0.7896175        0.8259887
## Interact.High.cor.Y.glm             0.7868852        0.8237288
## Low.cor.X.glm                       0.7765668        0.8146893
## All.X.glm                           0.7769029        0.8079096
## All.X.bayesglm                      0.7830688        0.8146893
## All.X.glmnet                        0.7962213        0.8293785
## All.X.no.rnorm.rpart                0.7934783        0.8282486
## All.X.no.rnorm.rf                   0.8086839        0.8406780
## All.Interact.X.glm                  0.7894737        0.8192090
## All.Interact.X.bayesglm             0.7969543        0.8192090
## All.Interact.X.glmnet               0.7930108        0.8259887
## All.Interact.X.no.rnorm.rpart       0.7934783        0.8282486
## All.Interact.X.no.rnorm.rf          0.8120104        0.8372881
## csm.glm                             0.7747748        0.8022599
## csm.bayesglm                        0.7973684        0.8259887
## csm.glmnet                          0.7925033        0.8248588
## csm.rpart                           0.7934783        0.8282486
## csm.rf                              0.8184080        0.8350282
##                               max.AccuracyLower.OOB max.AccuracyUpper.OOB
## MFO.myMFO_classfr                         0.5032294             0.5699717
## Random.myrandom_classfr                   0.4300283             0.4967706
## Max.cor.Y.cv.0.rpart                      0.5032294             0.5699717
## Max.cor.Y.cv.0.cp.0.rpart                 0.7969937             0.8482877
## Max.cor.Y.rpart                           0.8017531             0.8525367
## Max.cor.Y.glm                             0.7993726             0.8504130
## Interact.High.cor.Y.glm                   0.7969937             0.8482877
## Low.cor.X.glm                             0.7874927             0.8397715
## All.X.glm                                 0.7803820             0.8333692
## All.X.bayesglm                            0.7874927             0.8397715
## All.X.glmnet                              0.8029439             0.8535980
## All.X.no.rnorm.rpart                      0.8017531             0.8525367
## All.X.no.rnorm.rf                         0.8148743             0.8641887
## All.Interact.X.glm                        0.7922402             0.8440325
## All.Interact.X.bayesglm                   0.7922402             0.8440325
## All.Interact.X.glmnet                     0.7993726             0.8504130
## All.Interact.X.no.rnorm.rpart             0.8017531             0.8525367
## All.Interact.X.no.rnorm.rf                0.8112909             0.8610159
## csm.glm                                   0.7744658             0.8280245
## csm.bayesglm                              0.7993726             0.8504130
## csm.glmnet                                0.7981829             0.8493505
## csm.rpart                                 0.8017531             0.8525367
## csm.rf                                    0.8089040             0.8588985
##                               max.Kappa.OOB max.AccuracySD.fit
## MFO.myMFO_classfr                 0.0000000                 NA
## Random.myrandom_classfr           0.0000000                 NA
## Max.cor.Y.cv.0.rpart              0.0000000                 NA
## Max.cor.Y.cv.0.cp.0.rpart         0.6430437                 NA
## Max.cor.Y.rpart                   0.6497240        0.015852866
## Max.cor.Y.glm                     0.6448747        0.009555267
## Interact.High.cor.Y.glm           0.6402627        0.010313351
## Low.cor.X.glm                     0.6219426        0.004787468
## All.X.glm                         0.6099603        0.013569077
## All.X.bayesglm                    0.6233456        0.004364881
## All.X.glmnet                      0.6523227        0.015475235
## All.X.no.rnorm.rpart              0.6497240        0.016789805
## All.X.no.rnorm.rf                 0.6751279        0.017072636
## All.Interact.X.glm                0.6327801        0.030209350
## All.Interact.X.bayesglm           0.6345052        0.015715421
## All.Interact.X.glmnet             0.6455950        0.021054446
## All.Interact.X.no.rnorm.rpart     0.6497240        0.016789805
## All.Interact.X.no.rnorm.rf        0.6698360        0.013309611
## csm.glm                           0.5995009        0.023111654
## csm.bayesglm                      0.6465508        0.020134659
## csm.glmnet                        0.6434744        0.013857005
## csm.rpart                         0.6497240        0.024681372
## csm.rf                            0.6673789        0.020139770
##                               max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                          NA          NA
## Random.myrandom_classfr                    NA          NA
## Max.cor.Y.cv.0.rpart                       NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                  NA          NA
## Max.cor.Y.rpart                    0.03187391          NA
## Max.cor.Y.glm                      0.01958033    868.3141
## Interact.High.cor.Y.glm            0.02147437    877.4626
## Low.cor.X.glm                      0.01254642    864.8173
## All.X.glm                          0.02894366    877.2778
## All.X.bayesglm                     0.01087050    906.5894
## All.X.glmnet                       0.03049648          NA
## All.X.no.rnorm.rpart               0.03471308          NA
## All.X.no.rnorm.rf                  0.03596915          NA
## All.Interact.X.glm                 0.06241819    853.2895
## All.Interact.X.bayesglm            0.03299670    883.1471
## All.Interact.X.glmnet              0.04398531          NA
## All.Interact.X.no.rnorm.rpart      0.03471308          NA
## All.Interact.X.no.rnorm.rf         0.02825810          NA
## csm.glm                            0.04919951    850.4544
## csm.bayesglm                       0.04297055    887.6748
## csm.glmnet                         0.03112462          NA
## csm.rpart                          0.05295970          NA
## csm.rf                             0.04149673          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##               label step_major step_minor     bgn     end elapsed
## 11  fit.models_1_rf         11          0 201.596 268.199  66.604
## 12 fit.models_1_end         12          0 268.200      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1 125.587 268.207 142.621
## 12 fit.models          7          2 268.208      NA      NA
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
##                                                    model_id
## MFO.myMFO_classfr                         MFO.myMFO_classfr
## Random.myrandom_classfr             Random.myrandom_classfr
## Max.cor.Y.cv.0.rpart                   Max.cor.Y.cv.0.rpart
## Max.cor.Y.cv.0.cp.0.rpart         Max.cor.Y.cv.0.cp.0.rpart
## Max.cor.Y.rpart                             Max.cor.Y.rpart
## Max.cor.Y.glm                                 Max.cor.Y.glm
## Interact.High.cor.Y.glm             Interact.High.cor.Y.glm
## Low.cor.X.glm                                 Low.cor.X.glm
## All.X.glm                                         All.X.glm
## All.X.bayesglm                               All.X.bayesglm
## All.X.glmnet                                   All.X.glmnet
## All.X.no.rnorm.rpart                   All.X.no.rnorm.rpart
## All.X.no.rnorm.rf                         All.X.no.rnorm.rf
## All.Interact.X.glm                       All.Interact.X.glm
## All.Interact.X.bayesglm             All.Interact.X.bayesglm
## All.Interact.X.glmnet                 All.Interact.X.glmnet
## All.Interact.X.no.rnorm.rpart All.Interact.X.no.rnorm.rpart
## All.Interact.X.no.rnorm.rf       All.Interact.X.no.rnorm.rf
## csm.glm                                             csm.glm
## csm.bayesglm                                   csm.bayesglm
## csm.glmnet                                       csm.glmnet
## csm.rpart                                         csm.rpart
## csm.rf                                               csm.rf
##                                   model_method
## MFO.myMFO_classfr                myMFO_classfr
## Random.myrandom_classfr       myrandom_classfr
## Max.cor.Y.cv.0.rpart                     rpart
## Max.cor.Y.cv.0.cp.0.rpart                rpart
## Max.cor.Y.rpart                          rpart
## Max.cor.Y.glm                              glm
## Interact.High.cor.Y.glm                    glm
## Low.cor.X.glm                              glm
## All.X.glm                                  glm
## All.X.bayesglm                        bayesglm
## All.X.glmnet                            glmnet
## All.X.no.rnorm.rpart                     rpart
## All.X.no.rnorm.rf                           rf
## All.Interact.X.glm                         glm
## All.Interact.X.bayesglm               bayesglm
## All.Interact.X.glmnet                   glmnet
## All.Interact.X.no.rnorm.rpart            rpart
## All.Interact.X.no.rnorm.rf                  rf
## csm.glm                                    glm
## csm.bayesglm                          bayesglm
## csm.glmnet                              glmnet
## csm.rpart                                rpart
## csm.rf                                      rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             biddable, startprice.diff
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        biddable, startprice.diff
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.diff
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.diff
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                                                                                                                                biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glm                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glm            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.bayesglm       D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glmnet         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rpart         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rf            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.diff, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## csm.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.bayesglm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.glmnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rf                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.diff, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##                               max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                           0   0.5000000
## Random.myrandom_classfr                     0   0.5071756
## Max.cor.Y.cv.0.rpart                        0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                   0   0.9051018
## Max.cor.Y.rpart                             3   0.8435581
## Max.cor.Y.glm                               1   0.8706955
## Interact.High.cor.Y.glm                     1   0.8716794
## Low.cor.X.glm                               1   0.9063401
## All.X.glm                                   1   0.9130577
## All.X.bayesglm                              1   0.9103774
## All.X.glmnet                                9   0.8773155
## All.X.no.rnorm.rpart                        3   0.8435581
## All.X.no.rnorm.rf                           3   1.0000000
## All.Interact.X.glm                          1   0.9285072
## All.Interact.X.bayesglm                     1   0.9261281
## All.Interact.X.glmnet                       9   0.9132994
## All.Interact.X.no.rnorm.rpart               3   0.8435581
## All.Interact.X.no.rnorm.rf                  3   1.0000000
## csm.glm                                     1   0.9246141
## csm.bayesglm                                1   0.9218321
## csm.glmnet                                  9   0.9086260
## csm.rpart                                   3   0.8435581
## csm.rf                                      3   0.9999915
##                               opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6320225
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.6       0.8538283
## Max.cor.Y.rpart                                  0.9       0.8113440
## Max.cor.Y.glm                                    0.7       0.7906404
## Interact.High.cor.Y.glm                          0.7       0.7915633
## Low.cor.X.glm                                    0.5       0.8123570
## All.X.glm                                        0.4       0.8217391
## All.X.bayesglm                                   0.6       0.8238095
## All.X.glmnet                                     0.6       0.8048485
## All.X.no.rnorm.rpart                             0.9       0.8113440
## All.X.no.rnorm.rf                                0.6       1.0000000
## All.Interact.X.glm                               0.4       0.8396125
## All.Interact.X.bayesglm                          0.5       0.8388571
## All.Interact.X.glmnet                            0.5       0.8227115
## All.Interact.X.no.rnorm.rpart                    0.9       0.8113440
## All.Interact.X.no.rnorm.rf                       0.5       1.0000000
## csm.glm                                          0.4       0.8315098
## csm.bayesglm                                     0.4       0.8311404
## csm.glmnet                                       0.6       0.8123457
## csm.rpart                                        0.9       0.8113440
## csm.rf                                           0.5       0.9988901
##                               max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                    0.5379877     0.0000000   0.5000000
## Random.myrandom_classfr              0.4620123     0.0000000   0.5191913
## Max.cor.Y.cv.0.rpart                 0.5379877     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart            0.8706366     0.7382100   0.8812811
## Max.cor.Y.rpart                      0.8367648     0.6685851   0.8273068
## Max.cor.Y.glm                        0.7977525     0.5911469   0.8658999
## Interact.High.cor.Y.glm              0.7936467     0.5830988   0.8652888
## Low.cor.X.glm                        0.7854194     0.5673801   0.8499512
## All.X.glm                            0.7905476     0.5768469   0.8492837
## All.X.bayesglm                       0.7946660     0.5859868   0.8576175
## All.X.glmnet                         0.7977588     0.5910839   0.8631682
## All.X.no.rnorm.rpart                 0.8347135     0.6642538   0.8273068
## All.X.no.rnorm.rf                    0.8418803     0.6791071   0.8876893
## All.Interact.X.glm                   0.7844033     0.5662735   0.8559230
## All.Interact.X.bayesglm              0.8018519     0.6010074   0.8673582
## All.Interact.X.glmnet                0.8038936     0.6039602   0.8728883
## All.Interact.X.no.rnorm.rpart        0.8347135     0.6642538   0.8273068
## All.Interact.X.no.rnorm.rf           0.8460019     0.6873959   0.8910295
## csm.glm                              0.7884995     0.5731278   0.8425315
## csm.bayesglm                         0.8059576     0.6084395   0.8751682
## csm.glmnet                           0.8080089     0.6119731   0.8701669
## csm.rpart                            0.8419120     0.6771481   0.8273068
## csm.rf                               0.8305983     0.6567546   0.8875302
##                               opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6332046
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.6       0.7994859
## Max.cor.Y.rpart                                  0.9       0.7934783
## Max.cor.Y.glm                                    0.7       0.7896175
## Interact.High.cor.Y.glm                          0.7       0.7868852
## Low.cor.X.glm                                    0.7       0.7765668
## All.X.glm                                        0.6       0.7769029
## All.X.bayesglm                                   0.6       0.7830688
## All.X.glmnet                                     0.6       0.7962213
## All.X.no.rnorm.rpart                             0.9       0.7934783
## All.X.no.rnorm.rf                                0.6       0.8086839
## All.Interact.X.glm                               0.6       0.7894737
## All.Interact.X.bayesglm                          0.5       0.7969543
## All.Interact.X.glmnet                            0.6       0.7930108
## All.Interact.X.no.rnorm.rpart                    0.9       0.7934783
## All.Interact.X.no.rnorm.rf                       0.5       0.8120104
## csm.glm                                          0.5       0.7747748
## csm.bayesglm                                     0.6       0.7973684
## csm.glmnet                                       0.6       0.7925033
## csm.rpart                                        0.9       0.7934783
## csm.rf                                           0.5       0.8184080
##                               max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                    0.5367232     0.0000000
## Random.myrandom_classfr              0.4632768     0.0000000
## Max.cor.Y.cv.0.rpart                 0.5367232     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart            0.8237288     0.6430437
## Max.cor.Y.rpart                      0.8282486     0.6497240
## Max.cor.Y.glm                        0.8259887     0.6448747
## Interact.High.cor.Y.glm              0.8237288     0.6402627
## Low.cor.X.glm                        0.8146893     0.6219426
## All.X.glm                            0.8079096     0.6099603
## All.X.bayesglm                       0.8146893     0.6233456
## All.X.glmnet                         0.8293785     0.6523227
## All.X.no.rnorm.rpart                 0.8282486     0.6497240
## All.X.no.rnorm.rf                    0.8406780     0.6751279
## All.Interact.X.glm                   0.8192090     0.6327801
## All.Interact.X.bayesglm              0.8192090     0.6345052
## All.Interact.X.glmnet                0.8259887     0.6455950
## All.Interact.X.no.rnorm.rpart        0.8282486     0.6497240
## All.Interact.X.no.rnorm.rf           0.8372881     0.6698360
## csm.glm                              0.8022599     0.5995009
## csm.bayesglm                         0.8259887     0.6465508
## csm.glmnet                           0.8248588     0.6434744
## csm.rpart                            0.8282486     0.6497240
## csm.rf                               0.8350282     0.6673789
##                               inv.elapsedtime.everything
## MFO.myMFO_classfr                             2.39808153
## Random.myrandom_classfr                       3.54609929
## Max.cor.Y.cv.0.rpart                          1.57728707
## Max.cor.Y.cv.0.cp.0.rpart                     2.08333333
## Max.cor.Y.rpart                               0.94607379
## Max.cor.Y.glm                                 1.02986612
## Interact.High.cor.Y.glm                       0.95238095
## Low.cor.X.glm                                 0.72150072
## All.X.glm                                     0.63653724
## All.X.bayesglm                                0.38654813
## All.X.glmnet                                  0.19168104
## All.X.no.rnorm.rpart                          0.59772863
## All.X.no.rnorm.rf                             0.06491820
## All.Interact.X.glm                            0.54466231
## All.Interact.X.bayesglm                       0.45913682
## All.Interact.X.glmnet                         0.15974441
## All.Interact.X.no.rnorm.rpart                 0.57504313
## All.Interact.X.no.rnorm.rf                    0.05474353
## csm.glm                                       0.52410901
## csm.bayesglm                                  0.41701418
## csm.glmnet                                    0.33211558
## csm.rpart                                     0.55803571
## csm.rf                                        0.05935423
##                               inv.elapsedtime.final inv.aic.fit
## MFO.myMFO_classfr                       333.3333333          NA
## Random.myrandom_classfr                 500.0000000          NA
## Max.cor.Y.cv.0.rpart                     83.3333333          NA
## Max.cor.Y.cv.0.cp.0.rpart               111.1111111          NA
## Max.cor.Y.rpart                          90.9090909          NA
## Max.cor.Y.glm                            76.9230769 0.001151657
## Interact.High.cor.Y.glm                  66.6666667 0.001139650
## Low.cor.X.glm                             5.9523810 0.001156314
## All.X.glm                                 4.0000000 0.001139890
## All.X.bayesglm                            2.7397260 0.001103035
## All.X.glmnet                              1.0405827          NA
## All.X.no.rnorm.rpart                     15.8730159          NA
## All.X.no.rnorm.rf                         0.2082899          NA
## All.Interact.X.glm                        2.6809651 0.001171935
## All.Interact.X.bayesglm                   1.9801980 0.001132314
## All.Interact.X.glmnet                     0.6849315          NA
## All.Interact.X.no.rnorm.rpart            12.9870130          NA
## All.Interact.X.no.rnorm.rf                0.1842978          NA
## csm.glm                                   3.2679739 0.001175842
## csm.bayesglm                              2.2172949 0.001126539
## csm.glmnet                                2.8089888          NA
## csm.rpart                                10.6382979          NA
## csm.rf                                    0.1757778          NA
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
## 23. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 5 rows containing missing values (geom_path).
```

```
## Warning: Removed 247 rows containing missing values (geom_point).
```

```
## Warning: Removed 14 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 23. Consider specifying shapes manually if you must have them.
```

![](ebayipads_spdiff_files/figure-html/fit.models_2-1.png) 

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

![](ebayipads_spdiff_files/figure-html/fit.models_2-2.png) 

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
##                         model_id max.Accuracy.OOB max.auc.OOB
## 13             All.X.no.rnorm.rf        0.8406780   0.8876893
## 18    All.Interact.X.no.rnorm.rf        0.8372881   0.8910295
## 23                        csm.rf        0.8350282   0.8875302
## 11                  All.X.glmnet        0.8293785   0.8631682
## 5                Max.cor.Y.rpart        0.8282486   0.8273068
## 12          All.X.no.rnorm.rpart        0.8282486   0.8273068
## 17 All.Interact.X.no.rnorm.rpart        0.8282486   0.8273068
## 22                     csm.rpart        0.8282486   0.8273068
## 20                  csm.bayesglm        0.8259887   0.8751682
## 16         All.Interact.X.glmnet        0.8259887   0.8728883
## 6                  Max.cor.Y.glm        0.8259887   0.8658999
## 21                    csm.glmnet        0.8248588   0.8701669
## 4      Max.cor.Y.cv.0.cp.0.rpart        0.8237288   0.8812811
## 7        Interact.High.cor.Y.glm        0.8237288   0.8652888
## 15       All.Interact.X.bayesglm        0.8192090   0.8673582
## 14            All.Interact.X.glm        0.8192090   0.8559230
## 10                All.X.bayesglm        0.8146893   0.8576175
## 8                  Low.cor.X.glm        0.8146893   0.8499512
## 9                      All.X.glm        0.8079096   0.8492837
## 19                       csm.glm        0.8022599   0.8425315
## 1              MFO.myMFO_classfr        0.5367232   0.5000000
## 3           Max.cor.Y.cv.0.rpart        0.5367232   0.5000000
## 2        Random.myrandom_classfr        0.4632768   0.5191913
##    max.Kappa.OOB min.aic.fit opt.prob.threshold.OOB
## 13     0.6751279          NA                    0.6
## 18     0.6698360          NA                    0.5
## 23     0.6673789          NA                    0.5
## 11     0.6523227          NA                    0.6
## 5      0.6497240          NA                    0.9
## 12     0.6497240          NA                    0.9
## 17     0.6497240          NA                    0.9
## 22     0.6497240          NA                    0.9
## 20     0.6465508    887.6748                    0.6
## 16     0.6455950          NA                    0.6
## 6      0.6448747    868.3141                    0.7
## 21     0.6434744          NA                    0.6
## 4      0.6430437          NA                    0.6
## 7      0.6402627    877.4626                    0.7
## 15     0.6345052    883.1471                    0.5
## 14     0.6327801    853.2895                    0.6
## 10     0.6233456    906.5894                    0.6
## 8      0.6219426    864.8173                    0.7
## 9      0.6099603    877.2778                    0.6
## 19     0.5995009    850.4544                    0.5
## 1      0.0000000          NA                    0.5
## 3      0.0000000          NA                    0.5
## 2      0.0000000          NA                    0.4
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
## 23. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 105 rows containing missing values (geom_point).
```

```
## Warning: Removed 14 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 23. Consider specifying shapes manually if you must have them.
```

![](ebayipads_spdiff_files/figure-html/fit.models_2-3.png) 

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
## [1] "Best model id: All.X.no.rnorm.rf"
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

![](ebayipads_spdiff_files/figure-html/fit.models_2-4.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        974   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           1948   matrix     numeric  
## oob.times        974   -none-     numeric  
## classes            2   -none-     character
## importance        90   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            90   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
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
predct_error_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".err")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                               importance
## startprice.diff                             1.000000e+02
## biddable                                    9.829657e+01
## idseq.my                                    3.855844e+01
## D.ratio.sum.TfIdf.nwrds                     4.710374e+00
## D.TfIdf.sum.stem.stop.Ratio                 4.562101e+00
## storage.fctr64                              3.804173e+00
## prdline.my.fctriPadAir                      3.449252e+00
## D.ratio.nstopwrds.nwrds                     3.419359e+00
## D.TfIdf.sum.post.stop                       3.400298e+00
## D.TfIdf.sum.post.stem                       3.284824e+00
## D.sum.TfIdf                                 3.112166e+00
## color.fctrWhite                             3.046840e+00
## condition.fctrNew                           3.035508e+00
## D.nstopwrds.log                             2.833335e+00
## color.fctrBlack                             2.763335e+00
## D.nchrs.log                                 2.755461e+00
## prdline.my.fctriPadmini                     2.707907e+00
## storage.fctr16                              2.673851e+00
## prdline.my.fctriPad 3+                      2.507217e+00
## storage.fctrUnknown                         2.175406e+00
## carrier.fctrUnknown                         2.165420e+00
## cellular.fctr1                              2.079886e+00
## carrier.fctrVerizon                         2.006626e+00
## condition.fctrNew other (see details)       2.004513e+00
## D.nwrds.log                                 2.002785e+00
## D.nuppr.log                                 1.984142e+00
## prdline.my.fctriPad 1                       1.948124e+00
## prdline.my.fctriPadmini 2+                  1.870453e+00
## cellular.fctrUnknown                        1.772779e+00
## carrier.fctrAT&T                            1.762314e+00
## color.fctrSpace Gray                        1.726675e+00
## storage.fctr32                              1.723320e+00
## color.fctrGold                              1.701581e+00
## condition.fctrFor parts or not working      1.644323e+00
## D.nwrds.unq.log                             1.566584e+00
## prdline.my.fctriPad 2                       1.437702e+00
## D.terms.n.post.stem.log                     1.433424e+00
## D.terms.n.post.stop.log                     1.357900e+00
## D.terms.n.post.stem                         1.341172e+00
## D.terms.n.post.stop                         1.274846e+00
## carrier.fctrSprint                          1.239758e+00
## condition.fctrManufacturer refurbished      1.122589e+00
## D.npnct05.log                               9.921267e-01
## D.npnct13.log                               9.182885e-01
## D.terms.n.stem.stop.Ratio                   8.198376e-01
## prdline.my.fctrUnknown:.clusterid.fctr2     8.044334e-01
## prdline.my.fctriPad 2:.clusterid.fctr5      7.897332e-01
## D.npnct11.log                               7.287454e-01
## D.ndgts.log                                 7.174305e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3     6.328956e-01
## D.npnct08.log                               4.858434e-01
## D.npnct15.log                               4.543304e-01
## D.npnct12.log                               4.286325e-01
## prdline.my.fctriPad 1:.clusterid.fctr4      4.213698e-01
## prdline.my.fctriPadAir:.clusterid.fctr3     4.109719e-01
## D.npnct01.log                               3.942874e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4     3.942077e-01
## prdline.my.fctriPad 1:.clusterid.fctr2      3.808551e-01
## condition.fctrSeller refurbished            3.534679e-01
## prdline.my.fctriPadmini:.clusterid.fctr2    3.318202e-01
## prdline.my.fctriPadmini:.clusterid.fctr3    3.059793e-01
## D.npnct24.log                               2.769820e-01
## D.npnct14.log                               2.220022e-01
## carrier.fctrT-Mobile                        1.809466e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 1.694272e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 1.678059e-01
## prdline.my.fctriPad 2:.clusterid.fctr2      1.560424e-01
## D.npnct16.log                               1.348258e-01
## prdline.my.fctriPadmini:.clusterid.fctr4    1.323776e-01
## prdline.my.fctriPadAir:.clusterid.fctr4     1.291254e-01
## prdline.my.fctriPad 1:.clusterid.fctr3      1.142109e-01
## prdline.my.fctriPadAir:.clusterid.fctr2     9.796609e-02
## prdline.my.fctriPad 2:.clusterid.fctr4      9.518040e-02
## prdline.my.fctrUnknown:.clusterid.fctr3     8.424913e-02
## D.npnct06.log                               6.927794e-02
## prdline.my.fctriPad 2:.clusterid.fctr3      5.249594e-02
## D.npnct03.log                               4.964707e-02
## prdline.my.fctriPad 3+:.clusterid.fctr2     3.901452e-02
## D.npnct10.log                               2.429697e-02
## prdline.my.fctriPadmini:.clusterid.fctr5    1.694124e-02
## carrier.fctrOther                           1.405819e-02
## D.npnct28.log                               4.760632e-03
## D.npnct09.log                               0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4 0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5 0.000000e+00
##                                             All.X.no.rnorm.rf.importance
## startprice.diff                                             1.000000e+02
## biddable                                                    9.829657e+01
## idseq.my                                                    3.855844e+01
## D.ratio.sum.TfIdf.nwrds                                     4.710374e+00
## D.TfIdf.sum.stem.stop.Ratio                                 4.562101e+00
## storage.fctr64                                              3.804173e+00
## prdline.my.fctriPadAir                                      3.449252e+00
## D.ratio.nstopwrds.nwrds                                     3.419359e+00
## D.TfIdf.sum.post.stop                                       3.400298e+00
## D.TfIdf.sum.post.stem                                       3.284824e+00
## D.sum.TfIdf                                                 3.112166e+00
## color.fctrWhite                                             3.046840e+00
## condition.fctrNew                                           3.035508e+00
## D.nstopwrds.log                                             2.833335e+00
## color.fctrBlack                                             2.763335e+00
## D.nchrs.log                                                 2.755461e+00
## prdline.my.fctriPadmini                                     2.707907e+00
## storage.fctr16                                              2.673851e+00
## prdline.my.fctriPad 3+                                      2.507217e+00
## storage.fctrUnknown                                         2.175406e+00
## carrier.fctrUnknown                                         2.165420e+00
## cellular.fctr1                                              2.079886e+00
## carrier.fctrVerizon                                         2.006626e+00
## condition.fctrNew other (see details)                       2.004513e+00
## D.nwrds.log                                                 2.002785e+00
## D.nuppr.log                                                 1.984142e+00
## prdline.my.fctriPad 1                                       1.948124e+00
## prdline.my.fctriPadmini 2+                                  1.870453e+00
## cellular.fctrUnknown                                        1.772779e+00
## carrier.fctrAT&T                                            1.762314e+00
## color.fctrSpace Gray                                        1.726675e+00
## storage.fctr32                                              1.723320e+00
## color.fctrGold                                              1.701581e+00
## condition.fctrFor parts or not working                      1.644323e+00
## D.nwrds.unq.log                                             1.566584e+00
## prdline.my.fctriPad 2                                       1.437702e+00
## D.terms.n.post.stem.log                                     1.433424e+00
## D.terms.n.post.stop.log                                     1.357900e+00
## D.terms.n.post.stem                                         1.341172e+00
## D.terms.n.post.stop                                         1.274846e+00
## carrier.fctrSprint                                          1.239758e+00
## condition.fctrManufacturer refurbished                      1.122589e+00
## D.npnct05.log                                               9.921267e-01
## D.npnct13.log                                               9.182885e-01
## D.terms.n.stem.stop.Ratio                                   8.198376e-01
## prdline.my.fctrUnknown:.clusterid.fctr2                     8.044334e-01
## prdline.my.fctriPad 2:.clusterid.fctr5                      7.897332e-01
## D.npnct11.log                                               7.287454e-01
## D.ndgts.log                                                 7.174305e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                     6.328956e-01
## D.npnct08.log                                               4.858434e-01
## D.npnct15.log                                               4.543304e-01
## D.npnct12.log                                               4.286325e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                      4.213698e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                     4.109719e-01
## D.npnct01.log                                               3.942874e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                     3.942077e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                      3.808551e-01
## condition.fctrSeller refurbished                            3.534679e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                    3.318202e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                    3.059793e-01
## D.npnct24.log                                               2.769820e-01
## D.npnct14.log                                               2.220022e-01
## carrier.fctrT-Mobile                                        1.809466e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                 1.694272e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                 1.678059e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                      1.560424e-01
## D.npnct16.log                                               1.348258e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                    1.323776e-01
## prdline.my.fctriPadAir:.clusterid.fctr4                     1.291254e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                      1.142109e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                     9.796609e-02
## prdline.my.fctriPad 2:.clusterid.fctr4                      9.518040e-02
## prdline.my.fctrUnknown:.clusterid.fctr3                     8.424913e-02
## D.npnct06.log                                               6.927794e-02
## prdline.my.fctriPad 2:.clusterid.fctr3                      5.249594e-02
## D.npnct03.log                                               4.964707e-02
## prdline.my.fctriPad 3+:.clusterid.fctr2                     3.901452e-02
## D.npnct10.log                                               2.429697e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                    1.694124e-02
## carrier.fctrOther                                           1.405819e-02
## D.npnct28.log                                               4.760632e-03
## D.npnct09.log                                               0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                 0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                 0.000000e+00
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 42
```

![](ebayipads_spdiff_files/figure-html/fit.models_2-5.png) ![](ebayipads_spdiff_files/figure-html/fit.models_2-6.png) ![](ebayipads_spdiff_files/figure-html/fit.models_2-7.png) ![](ebayipads_spdiff_files/figure-html/fit.models_2-8.png) ![](ebayipads_spdiff_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 66      10066         N                                    0.490
## 594     10594         N                                    0.474
## 1396    11397         N                                    0.002
## 512     10512         N                                    0.634
## 508     10508         N                                    0.642
## 819     10819         N                                    0.662
## 1103    11103         N                                    0.664
## 1836    11837         N                                    0.764
## 1835    11836         N                                    0.802
## 747     10747         N                                    0.850
## 526     10526         N                                    0.868
## 1768    11769         N                                    0.888
## 1699    11700         N                                    0.946
## 199     10199         N                                    0.996
## 283     10283         N                                    0.998
##      sold.fctr.predict.All.X.no.rnorm.rf
## 66                                     N
## 594                                    N
## 1396                                   N
## 512                                    Y
## 508                                    Y
## 819                                    Y
## 1103                                   Y
## 1836                                   Y
## 1835                                   Y
## 747                                    Y
## 526                                    Y
## 1768                                   Y
## 1699                                   Y
## 199                                    Y
## 283                                    Y
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 66                                           TRUE
## 594                                          TRUE
## 1396                                         TRUE
## 512                                         FALSE
## 508                                         FALSE
## 819                                         FALSE
## 1103                                        FALSE
## 1836                                        FALSE
## 1835                                        FALSE
## 747                                         FALSE
## 526                                         FALSE
## 1768                                        FALSE
## 1699                                        FALSE
## 199                                         FALSE
## 283                                         FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error .label
## 66                                       0.000  10066
## 594                                      0.000  10594
## 1396                                     0.000  11397
## 512                                      0.034  10512
## 508                                      0.042  10508
## 819                                      0.062  10819
## 1103                                     0.064  11103
## 1836                                     0.164  11837
## 1835                                     0.202  11836
## 747                                      0.250  10747
## 526                                      0.268  10526
## 1768                                     0.288  11769
## 1699                                     0.346  11700
## 199                                      0.396  10199
## 283                                      0.398  10283
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 1358    11359         Y                                    0.000
## 1447    11448         Y                                    0.000
## 1696    11697         Y                                    0.008
## 1582    11583         Y                                    0.014
## 1212    11212         Y                                    0.022
## 1420    11421         Y                                    0.026
##      sold.fctr.predict.All.X.no.rnorm.rf
## 1358                                   N
## 1447                                   N
## 1696                                   N
## 1582                                   N
## 1212                                   N
## 1420                                   N
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 1358                                        FALSE
## 1447                                        FALSE
## 1696                                        FALSE
## 1582                                        FALSE
## 1212                                        FALSE
## 1420                                        FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error
## 1358                                    -0.600
## 1447                                    -0.600
## 1696                                    -0.592
## 1582                                    -0.586
## 1212                                    -0.578
## 1420                                    -0.574
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 1704    11705         Y                                    0.114
## 1240    11241         Y                                    0.154
## 961     10961         Y                                    0.362
## 99      10099         Y                                    0.384
## 508     10508         N                                    0.642
## 120     10120         N                                    0.698
##      sold.fctr.predict.All.X.no.rnorm.rf
## 1704                                   N
## 1240                                   N
## 961                                    N
## 99                                     N
## 508                                    Y
## 120                                    Y
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 1704                                        FALSE
## 1240                                        FALSE
## 961                                         FALSE
## 99                                          FALSE
## 508                                         FALSE
## 120                                         FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error
## 1704                                    -0.486
## 1240                                    -0.446
## 961                                     -0.238
## 99                                      -0.216
## 508                                      0.042
## 120                                      0.098
##      UniqueID sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.prob
## 491     10491         N                                    0.976
## 955     10955         N                                    0.994
## 1621    11622         N                                    0.994
## 199     10199         N                                    0.996
## 283     10283         N                                    0.998
## 488     10488         N                                    1.000
##      sold.fctr.predict.All.X.no.rnorm.rf
## 491                                    Y
## 955                                    Y
## 1621                                   Y
## 199                                    Y
## 283                                    Y
## 488                                    Y
##      sold.fctr.predict.All.X.no.rnorm.rf.accurate
## 491                                         FALSE
## 955                                         FALSE
## 1621                                        FALSE
## 199                                         FALSE
## 283                                         FALSE
## 488                                         FALSE
##      sold.fctr.predict.All.X.no.rnorm.rf.error
## 491                                      0.376
## 955                                      0.394
## 1621                                     0.394
## 199                                      0.396
## 283                                      0.398
## 488                                      0.400
```

![](ebayipads_spdiff_files/figure-html/fit.models_2-10.png) 

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
## 12 fit.models          7          2 268.208 298.358   30.15
## 13 fit.models          7          3 298.358      NA      NA
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
## [1] "sold.fctr.predict.All.X.no.rnorm.rf.prob"    
## [2] "sold.fctr.predict.All.X.no.rnorm.rf"         
## [3] "sold.fctr.predict.All.X.no.rnorm.rf.accurate"
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

![](ebayipads_spdiff_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 298.358 303.548    5.19
## 14 fit.data.training          8          0 303.548      NA      NA
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
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_spdiff_files/figure-html/fit.data.training_0-1.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       1859   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           3718   matrix     numeric  
## oob.times       1859   -none-     numeric  
## classes            2   -none-     character
## importance        90   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               1859   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            90   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_spdiff_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold   f.score
## 1        0.0 0.6325855
## 2        0.1 0.8456244
## 3        0.2 0.9513274
## 4        0.3 0.9839817
## 5        0.4 0.9988386
## 6        0.5 1.0000000
## 7        0.6 0.9976690
## 8        0.7 0.9563107
## 9        0.8 0.8802083
## 10       0.9 0.8047255
## 11       1.0 0.1932773
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Final.rf.N sold.fctr.predict.Final.rf.Y
## 1         N                          999                           NA
## 2         Y                           NA                          860
##          Prediction
## Reference   N   Y
##         N 999   0
##         Y   0 860
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      1.0000000      1.0000000      0.9980176      1.0000000      0.5373857 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](ebayipads_spdiff_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     17.792                10.135
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8321703
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9980176                     1      0.659661
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.006940991      0.01364851
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 303.548 323.644  20.096
## 15 fit.data.training          8          1 323.645      NA      NA
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
## glb_fin_mdl_id, : Using default probability threshold: 0.6
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
##                                             All.X.no.rnorm.rf.importance
## startprice.diff                                             1.000000e+02
## biddable                                                    9.829657e+01
## idseq.my                                                    3.855844e+01
## D.TfIdf.sum.stem.stop.Ratio                                 4.562101e+00
## D.ratio.sum.TfIdf.nwrds                                     4.710374e+00
## prdline.my.fctriPadAir                                      3.449252e+00
## D.ratio.nstopwrds.nwrds                                     3.419359e+00
## condition.fctrNew                                           3.035508e+00
## color.fctrWhite                                             3.046840e+00
## prdline.my.fctriPad 1                                       1.948124e+00
## D.TfIdf.sum.post.stop                                       3.400298e+00
## D.nchrs.log                                                 2.755461e+00
## D.sum.TfIdf                                                 3.112166e+00
## D.TfIdf.sum.post.stem                                       3.284824e+00
## prdline.my.fctriPad 3+                                      2.507217e+00
## D.nstopwrds.log                                             2.833335e+00
## storage.fctr64                                              3.804173e+00
## storage.fctr16                                              2.673851e+00
## cellular.fctr1                                              2.079886e+00
## color.fctrBlack                                             2.763335e+00
## prdline.my.fctriPadmini 2+                                  1.870453e+00
## D.nuppr.log                                                 1.984142e+00
## prdline.my.fctriPadmini                                     2.707907e+00
## color.fctrSpace Gray                                        1.726675e+00
## D.nwrds.log                                                 2.002785e+00
## carrier.fctrUnknown                                         2.165420e+00
## storage.fctrUnknown                                         2.175406e+00
## prdline.my.fctriPad 2                                       1.437702e+00
## cellular.fctrUnknown                                        1.772779e+00
## condition.fctrNew other (see details)                       2.004513e+00
## storage.fctr32                                              1.723320e+00
## condition.fctrFor parts or not working                      1.644323e+00
## carrier.fctrVerizon                                         2.006626e+00
## D.terms.n.post.stem.log                                     1.433424e+00
## carrier.fctrAT&T                                            1.762314e+00
## D.terms.n.post.stem                                         1.341172e+00
## D.npnct11.log                                               7.287454e-01
## D.terms.n.post.stop.log                                     1.357900e+00
## D.terms.n.post.stop                                         1.274846e+00
## color.fctrGold                                              1.701581e+00
## D.nwrds.unq.log                                             1.566584e+00
## D.npnct13.log                                               9.182885e-01
## D.ndgts.log                                                 7.174305e-01
## D.npnct15.log                                               4.543304e-01
## condition.fctrManufacturer refurbished                      1.122589e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                      7.897332e-01
## prdline.my.fctrUnknown:.clusterid.fctr2                     8.044334e-01
## carrier.fctrSprint                                          1.239758e+00
## D.terms.n.stem.stop.Ratio                                   8.198376e-01
## condition.fctrSeller refurbished                            3.534679e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                      9.518040e-02
## prdline.my.fctriPad 3+:.clusterid.fctr3                     6.328956e-01
## D.npnct05.log                                               9.921267e-01
## carrier.fctrT-Mobile                                        1.809466e-01
## D.npnct08.log                                               4.858434e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                     8.424913e-02
## prdline.my.fctriPad 1:.clusterid.fctr2                      3.808551e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                    1.323776e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                     3.942077e-01
## D.npnct01.log                                               3.942874e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                     9.796609e-02
## D.npnct16.log                                               1.348258e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                    3.318202e-01
## D.npnct14.log                                               2.220022e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                      1.560424e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                 1.678059e-01
## D.npnct12.log                                               4.286325e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                      1.142109e-01
## D.npnct24.log                                               2.769820e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                    3.059793e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                     3.901452e-02
## D.npnct06.log                                               6.927794e-02
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                 1.694272e-01
## prdline.my.fctriPadAir:.clusterid.fctr4                     1.291254e-01
## D.npnct10.log                                               2.429697e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                    1.694124e-02
## prdline.my.fctriPadAir:.clusterid.fctr3                     4.109719e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                      4.213698e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                      5.249594e-02
## D.npnct03.log                                               4.964707e-02
## carrier.fctrOther                                           1.405819e-02
## D.npnct09.log                                               0.000000e+00
## D.npnct28.log                                               4.760632e-03
## prdline.my.fctrUnknown:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                 0.000000e+00
##                                               importance
## startprice.diff                             1.000000e+02
## biddable                                    9.714977e+01
## idseq.my                                    3.655118e+01
## D.TfIdf.sum.stem.stop.Ratio                 4.111189e+00
## D.ratio.sum.TfIdf.nwrds                     4.029252e+00
## prdline.my.fctriPadAir                      3.604291e+00
## D.ratio.nstopwrds.nwrds                     3.129337e+00
## condition.fctrNew                           3.114490e+00
## color.fctrWhite                             3.083885e+00
## prdline.my.fctriPad 1                       3.047200e+00
## D.TfIdf.sum.post.stop                       2.909248e+00
## D.nchrs.log                                 2.779848e+00
## D.sum.TfIdf                                 2.690770e+00
## D.TfIdf.sum.post.stem                       2.622665e+00
## prdline.my.fctriPad 3+                      2.600492e+00
## D.nstopwrds.log                             2.589084e+00
## storage.fctr64                              2.509445e+00
## storage.fctr16                              2.500840e+00
## cellular.fctr1                              2.436548e+00
## color.fctrBlack                             2.396730e+00
## prdline.my.fctriPadmini 2+                  2.371923e+00
## D.nuppr.log                                 2.315793e+00
## prdline.my.fctriPadmini                     2.212984e+00
## color.fctrSpace Gray                        2.133812e+00
## D.nwrds.log                                 2.028829e+00
## carrier.fctrUnknown                         1.958878e+00
## storage.fctrUnknown                         1.942625e+00
## prdline.my.fctriPad 2                       1.908280e+00
## cellular.fctrUnknown                        1.895589e+00
## condition.fctrNew other (see details)       1.632602e+00
## storage.fctr32                              1.581651e+00
## condition.fctrFor parts or not working      1.493784e+00
## carrier.fctrVerizon                         1.492578e+00
## D.terms.n.post.stem.log                     1.422731e+00
## carrier.fctrAT&T                            1.363287e+00
## D.terms.n.post.stem                         1.360814e+00
## D.npnct11.log                               1.304090e+00
## D.terms.n.post.stop.log                     1.301554e+00
## D.terms.n.post.stop                         1.294931e+00
## color.fctrGold                              1.238760e+00
## D.nwrds.unq.log                             1.236413e+00
## D.npnct13.log                               1.079486e+00
## D.ndgts.log                                 1.076077e+00
## D.npnct15.log                               9.980478e-01
## condition.fctrManufacturer refurbished      7.688005e-01
## prdline.my.fctriPad 2:.clusterid.fctr5      6.998488e-01
## prdline.my.fctrUnknown:.clusterid.fctr2     6.972380e-01
## carrier.fctrSprint                          6.740771e-01
## D.terms.n.stem.stop.Ratio                   6.401453e-01
## condition.fctrSeller refurbished            5.779163e-01
## prdline.my.fctriPad 2:.clusterid.fctr4      5.581525e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3     5.197071e-01
## D.npnct05.log                               5.168400e-01
## carrier.fctrT-Mobile                        5.012177e-01
## D.npnct08.log                               4.044977e-01
## prdline.my.fctrUnknown:.clusterid.fctr3     3.872699e-01
## prdline.my.fctriPad 1:.clusterid.fctr2      3.730643e-01
## prdline.my.fctriPadmini:.clusterid.fctr4    3.558532e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4     3.380535e-01
## D.npnct01.log                               3.231315e-01
## prdline.my.fctriPadAir:.clusterid.fctr2     3.074205e-01
## D.npnct16.log                               2.949843e-01
## prdline.my.fctriPadmini:.clusterid.fctr2    2.902052e-01
## D.npnct14.log                               2.823363e-01
## prdline.my.fctriPad 2:.clusterid.fctr2      2.177715e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 2.104155e-01
## D.npnct12.log                               1.985962e-01
## prdline.my.fctriPad 1:.clusterid.fctr3      1.827763e-01
## D.npnct24.log                               1.773042e-01
## prdline.my.fctriPadmini:.clusterid.fctr3    1.749152e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2     1.727758e-01
## D.npnct06.log                               1.668731e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 1.666519e-01
## prdline.my.fctriPadAir:.clusterid.fctr4     1.637150e-01
## D.npnct10.log                               1.633006e-01
## prdline.my.fctriPadmini:.clusterid.fctr5    1.501172e-01
## prdline.my.fctriPadAir:.clusterid.fctr3     1.453009e-01
## prdline.my.fctriPad 1:.clusterid.fctr4      1.244787e-01
## prdline.my.fctriPad 2:.clusterid.fctr3      7.102360e-02
## D.npnct03.log                               6.886715e-02
## carrier.fctrOther                           6.336752e-02
## D.npnct09.log                               9.683639e-04
## D.npnct28.log                               7.262729e-04
## prdline.my.fctrUnknown:.clusterid.fctr4     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5 0.000000e+00
##                                             Final.rf.importance
## startprice.diff                                    1.000000e+02
## biddable                                           9.714977e+01
## idseq.my                                           3.655118e+01
## D.TfIdf.sum.stem.stop.Ratio                        4.111189e+00
## D.ratio.sum.TfIdf.nwrds                            4.029252e+00
## prdline.my.fctriPadAir                             3.604291e+00
## D.ratio.nstopwrds.nwrds                            3.129337e+00
## condition.fctrNew                                  3.114490e+00
## color.fctrWhite                                    3.083885e+00
## prdline.my.fctriPad 1                              3.047200e+00
## D.TfIdf.sum.post.stop                              2.909248e+00
## D.nchrs.log                                        2.779848e+00
## D.sum.TfIdf                                        2.690770e+00
## D.TfIdf.sum.post.stem                              2.622665e+00
## prdline.my.fctriPad 3+                             2.600492e+00
## D.nstopwrds.log                                    2.589084e+00
## storage.fctr64                                     2.509445e+00
## storage.fctr16                                     2.500840e+00
## cellular.fctr1                                     2.436548e+00
## color.fctrBlack                                    2.396730e+00
## prdline.my.fctriPadmini 2+                         2.371923e+00
## D.nuppr.log                                        2.315793e+00
## prdline.my.fctriPadmini                            2.212984e+00
## color.fctrSpace Gray                               2.133812e+00
## D.nwrds.log                                        2.028829e+00
## carrier.fctrUnknown                                1.958878e+00
## storage.fctrUnknown                                1.942625e+00
## prdline.my.fctriPad 2                              1.908280e+00
## cellular.fctrUnknown                               1.895589e+00
## condition.fctrNew other (see details)              1.632602e+00
## storage.fctr32                                     1.581651e+00
## condition.fctrFor parts or not working             1.493784e+00
## carrier.fctrVerizon                                1.492578e+00
## D.terms.n.post.stem.log                            1.422731e+00
## carrier.fctrAT&T                                   1.363287e+00
## D.terms.n.post.stem                                1.360814e+00
## D.npnct11.log                                      1.304090e+00
## D.terms.n.post.stop.log                            1.301554e+00
## D.terms.n.post.stop                                1.294931e+00
## color.fctrGold                                     1.238760e+00
## D.nwrds.unq.log                                    1.236413e+00
## D.npnct13.log                                      1.079486e+00
## D.ndgts.log                                        1.076077e+00
## D.npnct15.log                                      9.980478e-01
## condition.fctrManufacturer refurbished             7.688005e-01
## prdline.my.fctriPad 2:.clusterid.fctr5             6.998488e-01
## prdline.my.fctrUnknown:.clusterid.fctr2            6.972380e-01
## carrier.fctrSprint                                 6.740771e-01
## D.terms.n.stem.stop.Ratio                          6.401453e-01
## condition.fctrSeller refurbished                   5.779163e-01
## prdline.my.fctriPad 2:.clusterid.fctr4             5.581525e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3            5.197071e-01
## D.npnct05.log                                      5.168400e-01
## carrier.fctrT-Mobile                               5.012177e-01
## D.npnct08.log                                      4.044977e-01
## prdline.my.fctrUnknown:.clusterid.fctr3            3.872699e-01
## prdline.my.fctriPad 1:.clusterid.fctr2             3.730643e-01
## prdline.my.fctriPadmini:.clusterid.fctr4           3.558532e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4            3.380535e-01
## D.npnct01.log                                      3.231315e-01
## prdline.my.fctriPadAir:.clusterid.fctr2            3.074205e-01
## D.npnct16.log                                      2.949843e-01
## prdline.my.fctriPadmini:.clusterid.fctr2           2.902052e-01
## D.npnct14.log                                      2.823363e-01
## prdline.my.fctriPad 2:.clusterid.fctr2             2.177715e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2        2.104155e-01
## D.npnct12.log                                      1.985962e-01
## prdline.my.fctriPad 1:.clusterid.fctr3             1.827763e-01
## D.npnct24.log                                      1.773042e-01
## prdline.my.fctriPadmini:.clusterid.fctr3           1.749152e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2            1.727758e-01
## D.npnct06.log                                      1.668731e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3        1.666519e-01
## prdline.my.fctriPadAir:.clusterid.fctr4            1.637150e-01
## D.npnct10.log                                      1.633006e-01
## prdline.my.fctriPadmini:.clusterid.fctr5           1.501172e-01
## prdline.my.fctriPadAir:.clusterid.fctr3            1.453009e-01
## prdline.my.fctriPad 1:.clusterid.fctr4             1.244787e-01
## prdline.my.fctriPad 2:.clusterid.fctr3             7.102360e-02
## D.npnct03.log                                      6.886715e-02
## carrier.fctrOther                                  6.336752e-02
## D.npnct09.log                                      9.683639e-04
## D.npnct28.log                                      7.262729e-04
## prdline.my.fctrUnknown:.clusterid.fctr4            0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5             0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4        0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5        0.000000e+00
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 42
```

![](ebayipads_spdiff_files/figure-html/fit.data.training_0-4.png) ![](ebayipads_spdiff_files/figure-html/fit.data.training_0-5.png) ![](ebayipads_spdiff_files/figure-html/fit.data.training_0-6.png) ![](ebayipads_spdiff_files/figure-html/fit.data.training_0-7.png) ![](ebayipads_spdiff_files/figure-html/fit.data.training_0-8.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1350    11351         Y                           0.550
## 1447    11448         Y                           0.586
## 332     10332         Y                           0.598
## 837     10837         Y                           0.598
## 1       10001         N                           0.170
## 2       10002         Y                           0.996
## 594     10594         N                           0.112
## 1396    11397         N                           0.000
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1350                          N                               FALSE
## 1447                          N                               FALSE
## 332                           N                               FALSE
## 837                           N                               FALSE
## 1                             N                                TRUE
## 2                             Y                                TRUE
## 594                           N                                TRUE
## 1396                          N                                TRUE
##      sold.fctr.predict.Final.rf.error .label
## 1350                           -0.050  11351
## 1447                           -0.014  11448
## 332                            -0.002  10332
## 837                            -0.002  10837
## 1                               0.000  10001
## 2                               0.000  10002
## 594                             0.000  10594
## 1396                            0.000  11397
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1350    11351         Y                           0.550
## 1447    11448         Y                           0.586
## 332     10332         Y                           0.598
## 837     10837         Y                           0.598
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1350                          N                               FALSE
## 1447                          N                               FALSE
## 332                           N                               FALSE
## 837                           N                               FALSE
##      sold.fctr.predict.Final.rf.error
## 1350                           -0.050
## 1447                           -0.014
## 332                            -0.002
## 837                            -0.002
```

![](ebayipads_spdiff_files/figure-html/fit.data.training_0-9.png) 

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
## [1] "sold.fctr.predict.Final.rf.prob" "sold.fctr.predict.Final.rf"
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

![](ebayipads_spdiff_files/figure-html/fit.data.training_0-10.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 323.645 329.209   5.564
## 16  predict.data.new          9          0 329.209      NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
# sav_newobs_df <- glb_newobs_df

# startprice.pred stuff
# tmp_allobs_df <- glb_get_predictions(glb_allobs_df, mdl_id=glb_fin_mdl_id, 
#                                      rsp_var_out=glb_rsp_var_out,
#     prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
#         glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
#                       "opt.prob.threshold.OOB"], NULL))
# rsp_var_out <- paste0(glb_rsp_var_out, glb_fin_mdl_id)
# tmp_allobs_df <- tmp_allobs_df[, c(glb_id_var, glb_rsp_var, rsp_var_out)]
# names(tmp_allobs_df)[3] <- glb_rsp_var_out
# write.csv(tmp_allobs_df, paste0(glb_out_pfx, "predict.csv"), row.names=FALSE)
##

glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newobs_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.6
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 42
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

![](ebayipads_spdiff_files/figure-html/predict.data.new-1.png) 

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

![](ebayipads_spdiff_files/figure-html/predict.data.new-2.png) 

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

![](ebayipads_spdiff_files/figure-html/predict.data.new-3.png) 

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

![](ebayipads_spdiff_files/figure-html/predict.data.new-4.png) 

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

![](ebayipads_spdiff_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1860    11862      <NA>                           0.306
## 1863    11865      <NA>                           0.682
## 2094    12096      <NA>                           0.414
## 2623    12625      <NA>                           0.412
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1860                          N                                  NA
## 1863                          Y                                  NA
## 2094                          N                                  NA
## 2623                          N                                  NA
##      sold.fctr.predict.Final.rf.error .label
## 1860                                0  11862
## 1863                                0  11865
## 2094                                0  12096
## 2623                                0  12625
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## NA         NA      <NA>                              NA
## NA.1       NA      <NA>                              NA
## NA.2       NA      <NA>                              NA
## NA.3       NA      <NA>                              NA
## NA.4       NA      <NA>                              NA
## NA.5       NA      <NA>                              NA
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## NA                         <NA>                                  NA
## NA.1                       <NA>                                  NA
## NA.2                       <NA>                                  NA
## NA.3                       <NA>                                  NA
## NA.4                       <NA>                                  NA
## NA.5                       <NA>                                  NA
##      sold.fctr.predict.Final.rf.error
## NA                                 NA
## NA.1                               NA
## NA.2                               NA
## NA.3                               NA
## NA.4                               NA
## NA.5                               NA
##        UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## NA.43        NA      <NA>                              NA
## NA.98        NA      <NA>                              NA
## NA.204       NA      <NA>                              NA
## NA.265       NA      <NA>                              NA
## NA.378       NA      <NA>                              NA
## NA.756       NA      <NA>                              NA
##        sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## NA.43                        <NA>                                  NA
## NA.98                        <NA>                                  NA
## NA.204                       <NA>                                  NA
## NA.265                       <NA>                                  NA
## NA.378                       <NA>                                  NA
## NA.756                       <NA>                                  NA
##        sold.fctr.predict.Final.rf.error
## NA.43                                NA
## NA.98                                NA
## NA.204                               NA
## NA.265                               NA
## NA.378                               NA
## NA.756                               NA
##        UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## NA.792       NA      <NA>                              NA
## NA.793       NA      <NA>                              NA
## NA.794       NA      <NA>                              NA
## NA.795       NA      <NA>                              NA
## NA.796       NA      <NA>                              NA
## NA.797       NA      <NA>                              NA
##        sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## NA.792                       <NA>                                  NA
## NA.793                       <NA>                                  NA
## NA.794                       <NA>                                  NA
## NA.795                       <NA>                                  NA
## NA.796                       <NA>                                  NA
## NA.797                       <NA>                                  NA
##        sold.fctr.predict.Final.rf.error
## NA.792                               NA
## NA.793                               NA
## NA.794                               NA
## NA.795                               NA
## NA.796                               NA
## NA.797                               NA
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

![](ebayipads_spdiff_files/figure-html/predict.data.new-6.png) 

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
    
    glb_force_prediction_lst <- list()
    glb_force_prediction_lst[["0"]] <- c(11885, 11907, 11943,
                                         12115, 12253, 12285, 12367, 12388, 12585)
    for (obs_id in glb_force_prediction_lst[["0"]])
        submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] <-
            max(0, submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] - 0.5)
    
    glb_force_prediction_lst[["1"]] <- c(11871, 11875, 11886, 
                                         11913, 11931, 11937, 11967, 11990, 11994, 11999, 
                                         12000, 12002, 12021, 12065, 12072, 
                                         12111, 12114, 12126, 12152, 12172,
                                         12213, 12214, 12233, 12278, 12299, 
                                         12446, 12491, 
                                         12505, 12576, 12608, 12630)
    for (obs_id in glb_force_prediction_lst[["1"]])
        submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] <-
            min(0.9999, submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] + 0.5)
    
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]

if (glb_is_classification) {
    rsp_var_out <- paste0(glb_rsp_var_out, glb_fin_mdl_id)
    tmp_newobs_df <- subset(glb_newobs_df[, c(glb_id_var, ".grpid", rsp_var_out)],
                            !is.na(.grpid))
    tmp_newobs_df <- merge(tmp_newobs_df, dupgrps_df, by=".grpid", all.x=TRUE)
    tmp_newobs_df <- merge(tmp_newobs_df, submit_df, by=glb_id_var, all.x = TRUE)
    tmp_newobs_df$.err <- 
        ((tmp_newobs_df$Probability1 >= 0.5) & (tmp_newobs_df$sold.0 > 0) |
         (tmp_newobs_df$Probability1 <= 0.5) & (tmp_newobs_df$sold.1 > 0))
    tmp_newobs_df <- orderBy(~UniqueID, subset(tmp_newobs_df, .err == TRUE))
    print("Prediction errors in duplicates:")
    print(tmp_newobs_df)
    
    if (nrow(tmp_newobs_df) > 0)
        stop("check Prediction errors in duplicates")
    #print(dupobs_df[dupobs_df$.grpid == 26, ])
    
    if (max(glb_newobs_df[!is.na(glb_newobs_df[, rsp_var_out]) & 
                      (glb_newobs_df[, rsp_var_out] == "Y"), "startprice"]) > 
        max(glb_allobs_df[!is.na(glb_allobs_df[, glb_rsp_var]) & 
                      (glb_allobs_df[, glb_rsp_var] == "Y"), "startprice"]))
        stop("startprice for some +ve predictions > 675")
}
```

```
## [1] "Prediction errors in duplicates:"
## [1] UniqueID                   .grpid                    
## [3] sold.fctr.predict.Final.rf sold.0                    
## [5] sold.1                     sold.NA                   
## [7] .freq                      Probability1              
## [9] .err                      
## <0 rows> (or 0-length row.names)
```

```r
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
for (txt_var in glb_txt_vars) {
    # Print post-stem-words but need post-stop-words for debugging ?
    print(sprintf("    All post-stem-words TfIDf terms for %s:", txt_var))
    myprint_df(glb_post_stem_words_terms_df_lst[[txt_var]])
    TfIdf_mtrx <- glb_post_stem_words_TfIdf_mtrx_lst[[txt_var]]
    print(glb_allobs_df[
        which(TfIdf_mtrx[, tail(glb_post_stem_words_terms_df_lst[[txt_var]], 1)$pos] > 0), 
                        c(glb_id_var, glb_txt_vars)])
    print(nrow(subset(glb_post_stem_words_terms_df_lst[[txt_var]], freq == 1)))
    #print(glb_allobs_df[which(TfIdf_mtrx[, 207] > 0), c(glb_id_var, glb_txt_vars)])
    #unlist(strsplit(glb_allobs_df[2157, "description"], ""))
    #glb_allobs_df[2442, c(glb_id_var, glb_txt_vars)]
    #TfIdf_mtrx[2442, TfIdf_mtrx[2442, ] > 0]  

    print(sprintf("    Top_n post_stem_words TfIDf terms for %s:", txt_var))
    tmp_df <- glb_post_stem_words_terms_df_lst[[txt_var]]
    top_n_vctr <- tmp_df$term[1:glb_top_n[[txt_var]]]
    tmp_freq1_df <- subset(tmp_df, freq == 1)
    tmp_freq1_df$top_n <- grepl(paste0(top_n_vctr, collapse="|"), tmp_freq1_df$term)
    print(subset(tmp_freq1_df, top_n == TRUE))
}
```

```
## [1] "    All post-stem-words TfIDf terms for descr.my:"
##            TfIdf    term freq pos
## condit  208.1066  condit  496 122
## use     146.5910     use  291 559
## scratch 128.3886 scratch  286 457
## new     125.5866     new  156 346
## good    121.0564    good  197 233
## ipad    107.4871    ipad  232 275
##             TfIdf    term freq pos
## box     75.940848     box  110  82
## port     6.121431    port    8 400
## regular  4.394223 regular    5 436
## usag     3.745654    usag    3 557
## besid    2.275117   besid    1  67
## 128      1.263954     128    1   2
##             TfIdf    term freq pos
## remot   1.2639536   remot    1 437
## ringer  1.2639536  ringer    1 450
## septemb 1.2639536 septemb    1 468
## site    1.2639536    site    1 487
## 975     1.1375583     975    1  16
## 79in    0.9479652    79in    1  15
##     UniqueID
## 520    10520
##                                                                                             descr.my
## 520 Apple iPad mini 1st Generation 16GB, Wi- Fi, 7.9in - Space Gray, great condition comes with the 
## [1] 123
## [1] "    Top_n post_stem_words TfIDf terms for descr.my:"
## [1] TfIdf term  freq  pos   top_n
## <0 rows> (or 0-length row.names)
```

```r
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
```

```
## [1] 0.6
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: All.X.no.rnorm.rf"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.rf"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 974  73
```

```r
print(dsp_models_df)
```

```
##                         model_id max.Accuracy.OOB max.auc.OOB
## 13             All.X.no.rnorm.rf        0.8406780   0.8876893
## 18    All.Interact.X.no.rnorm.rf        0.8372881   0.8910295
## 23                        csm.rf        0.8350282   0.8875302
## 11                  All.X.glmnet        0.8293785   0.8631682
## 5                Max.cor.Y.rpart        0.8282486   0.8273068
## 12          All.X.no.rnorm.rpart        0.8282486   0.8273068
## 17 All.Interact.X.no.rnorm.rpart        0.8282486   0.8273068
## 22                     csm.rpart        0.8282486   0.8273068
## 20                  csm.bayesglm        0.8259887   0.8751682
## 16         All.Interact.X.glmnet        0.8259887   0.8728883
## 6                  Max.cor.Y.glm        0.8259887   0.8658999
## 21                    csm.glmnet        0.8248588   0.8701669
## 4      Max.cor.Y.cv.0.cp.0.rpart        0.8237288   0.8812811
## 7        Interact.High.cor.Y.glm        0.8237288   0.8652888
## 15       All.Interact.X.bayesglm        0.8192090   0.8673582
## 14            All.Interact.X.glm        0.8192090   0.8559230
## 10                All.X.bayesglm        0.8146893   0.8576175
## 8                  Low.cor.X.glm        0.8146893   0.8499512
## 9                      All.X.glm        0.8079096   0.8492837
## 19                       csm.glm        0.8022599   0.8425315
## 1              MFO.myMFO_classfr        0.5367232   0.5000000
## 3           Max.cor.Y.cv.0.rpart        0.5367232   0.5000000
## 2        Random.myrandom_classfr        0.4632768   0.5191913
##    max.Kappa.OOB min.aic.fit opt.prob.threshold.OOB
## 13     0.6751279          NA                    0.6
## 18     0.6698360          NA                    0.5
## 23     0.6673789          NA                    0.5
## 11     0.6523227          NA                    0.6
## 5      0.6497240          NA                    0.9
## 12     0.6497240          NA                    0.9
## 17     0.6497240          NA                    0.9
## 22     0.6497240          NA                    0.9
## 20     0.6465508    887.6748                    0.6
## 16     0.6455950          NA                    0.6
## 6      0.6448747    868.3141                    0.7
## 21     0.6434744          NA                    0.6
## 4      0.6430437          NA                    0.6
## 7      0.6402627    877.4626                    0.7
## 15     0.6345052    883.1471                    0.5
## 14     0.6327801    853.2895                    0.6
## 10     0.6233456    906.5894                    0.6
## 8      0.6219426    864.8173                    0.7
## 9      0.6099603    877.2778                    0.6
## 19     0.5995009    850.4544                    0.5
## 1      0.0000000          NA                    0.5
## 3      0.0000000          NA                    0.5
## 2      0.0000000          NA                    0.4
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_var)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_var, glb_rsp_var,
                                           predct_error_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "error.abs.OOB"
        sOOB_ctgry_df <- dplyr::group_by(tmp_OOBobs_df, prdline.my)
        sOOB_ctgry_df <- dplyr::count(sOOB_ctgry_df, 
                                      startprice.OOB.sum = sum(startprice),
                                        err.abs.OOB.sum = sum(error.abs.OOB),
                                        err.abs.OOB.mean = mean(error.abs.OOB))
        names(sOOB_ctgry_df)[4] <- ".n.OOB"
        sOOB_ctgry_df <- dplyr::ungroup(sOOB_ctgry_df)
        #intersect(names(glb_ctgry_df), names(sOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, sOOB_ctgry_df, all=TRUE)
        print(orderBy(~-err.abs.OOB.mean, glb_ctgry_df))
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

    if (!is.null(glb_category_var)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_var, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
        
        print(glb_OOBobs_df[(glb_OOBobs_df$prdline.my == "iPadAir") & 
                            !(glb_OOBobs_df[, predct_accurate_var_name]), 
                            c(glb_id_var, glb_rsp_var_raw,
                              #"description"
                              "biddable", "startprice", "condition"
                              )])
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
## [1] "All.X.no.rnorm.rf OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 446  29
##         Y 112 298
##    prdline.my .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 3      iPad 2    171    154      0.1929825      0.1932203
## 5     iPadAir    151    137      0.1716792      0.1706215
## 1     Unknown     97     87      0.1090226      0.1096045
## 4     iPad 3+    136    123      0.1541353      0.1536723
## 2      iPad 1     99     89      0.1115288      0.1118644
## 7 iPadmini 2+    104     94      0.1177945      0.1175141
## 6    iPadmini    127    114      0.1428571      0.1435028
##   accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 3                 26               145        0.8479532
## 5                 24               127        0.8410596
## 1                 22                75        0.7731959
## 4                 20               116        0.8529412
## 2                 18                81        0.8181818
## 7                 18                86        0.8269231
## 6                 13               114        0.8976378
##      UniqueID sold biddable startprice                condition
## 261     10261    0        1     250.00                     Used
## 738     10738    0        1     350.00                      New
## 19      10019    1        0     375.00                     Used
## 109     10109    1        0     339.99                      New
## 205     10205    1        0     415.00                     Used
## 277     10277    1        0     300.00                     Used
## 577     10577    1        1     279.00                     Used
## 625     10625    1        0     559.99                     Used
## 675     10675    1        0     280.00                     Used
## 1042    11042    1        1     528.00                      New
## 1053    11053    1        1     450.00                     Used
## 1129    11129    1        1     350.00  New other (see details)
## 1200    11200    1        0     379.99                     Used
## 1212    11212    1        0     450.00                      New
## 1225    11225    1        0     499.99                     Used
## 1259    11260    1        1     260.00  New other (see details)
## 1349    11350    1        0     499.00                     Used
## 1353    11354    1        0     300.00                     Used
## 1381    11382    1        0     439.99                      New
## 1496    11497    1        0     320.00 Manufacturer refurbished
## 1521    11522    1        0     419.00                      New
## 1604    11605    1        0     229.00 For parts or not working
## 1790    11791    1        1     349.99                      New
## 1853    11855    1        0     424.99                     Used
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
##                                             All.X.no.rnorm.rf.importance
## startprice.diff                                             1.000000e+02
## biddable                                                    9.829657e+01
## idseq.my                                                    3.855844e+01
## D.ratio.sum.TfIdf.nwrds                                     4.710374e+00
## D.TfIdf.sum.stem.stop.Ratio                                 4.562101e+00
## storage.fctr64                                              3.804173e+00
## prdline.my.fctriPadAir                                      3.449252e+00
## D.ratio.nstopwrds.nwrds                                     3.419359e+00
## D.TfIdf.sum.post.stop                                       3.400298e+00
## D.TfIdf.sum.post.stem                                       3.284824e+00
## D.sum.TfIdf                                                 3.112166e+00
## color.fctrWhite                                             3.046840e+00
## condition.fctrNew                                           3.035508e+00
## D.nstopwrds.log                                             2.833335e+00
## color.fctrBlack                                             2.763335e+00
## D.nchrs.log                                                 2.755461e+00
## prdline.my.fctriPadmini                                     2.707907e+00
## storage.fctr16                                              2.673851e+00
## prdline.my.fctriPad 3+                                      2.507217e+00
## storage.fctrUnknown                                         2.175406e+00
## carrier.fctrUnknown                                         2.165420e+00
## cellular.fctr1                                              2.079886e+00
## carrier.fctrVerizon                                         2.006626e+00
## condition.fctrNew other (see details)                       2.004513e+00
## D.nwrds.log                                                 2.002785e+00
## D.nuppr.log                                                 1.984142e+00
## prdline.my.fctriPad 1                                       1.948124e+00
## prdline.my.fctriPadmini 2+                                  1.870453e+00
## cellular.fctrUnknown                                        1.772779e+00
## carrier.fctrAT&T                                            1.762314e+00
## color.fctrSpace Gray                                        1.726675e+00
## storage.fctr32                                              1.723320e+00
## color.fctrGold                                              1.701581e+00
## condition.fctrFor parts or not working                      1.644323e+00
## D.nwrds.unq.log                                             1.566584e+00
## prdline.my.fctriPad 2                                       1.437702e+00
## D.terms.n.post.stem.log                                     1.433424e+00
## D.terms.n.post.stop.log                                     1.357900e+00
## D.terms.n.post.stem                                         1.341172e+00
## D.terms.n.post.stop                                         1.274846e+00
## carrier.fctrSprint                                          1.239758e+00
## condition.fctrManufacturer refurbished                      1.122589e+00
## D.npnct05.log                                               9.921267e-01
## D.npnct13.log                                               9.182885e-01
## D.terms.n.stem.stop.Ratio                                   8.198376e-01
## prdline.my.fctrUnknown:.clusterid.fctr2                     8.044334e-01
## prdline.my.fctriPad 2:.clusterid.fctr5                      7.897332e-01
## D.npnct11.log                                               7.287454e-01
## D.ndgts.log                                                 7.174305e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                     6.328956e-01
## D.npnct08.log                                               4.858434e-01
## D.npnct15.log                                               4.543304e-01
## D.npnct12.log                                               4.286325e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                      4.213698e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                     4.109719e-01
## D.npnct01.log                                               3.942874e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                     3.942077e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                      3.808551e-01
## condition.fctrSeller refurbished                            3.534679e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                    3.318202e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                    3.059793e-01
## D.npnct24.log                                               2.769820e-01
## D.npnct14.log                                               2.220022e-01
## carrier.fctrT-Mobile                                        1.809466e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                 1.694272e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                 1.678059e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                      1.560424e-01
## D.npnct16.log                                               1.348258e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                    1.323776e-01
## prdline.my.fctriPadAir:.clusterid.fctr4                     1.291254e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                      1.142109e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                     9.796609e-02
## prdline.my.fctriPad 2:.clusterid.fctr4                      9.518040e-02
## prdline.my.fctrUnknown:.clusterid.fctr3                     8.424913e-02
## D.npnct06.log                                               6.927794e-02
## prdline.my.fctriPad 2:.clusterid.fctr3                      5.249594e-02
## D.npnct03.log                                               4.964707e-02
## prdline.my.fctriPad 3+:.clusterid.fctr2                     3.901452e-02
## D.npnct10.log                                               2.429697e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                    1.694124e-02
## carrier.fctrOther                                           1.405819e-02
## D.npnct28.log                                               4.760632e-03
## D.npnct09.log                                               0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                 0.000000e+00
##                                               importance
## startprice.diff                             1.000000e+02
## biddable                                    9.714977e+01
## idseq.my                                    3.655118e+01
## D.ratio.sum.TfIdf.nwrds                     4.029252e+00
## D.TfIdf.sum.stem.stop.Ratio                 4.111189e+00
## storage.fctr64                              2.509445e+00
## prdline.my.fctriPadAir                      3.604291e+00
## D.ratio.nstopwrds.nwrds                     3.129337e+00
## D.TfIdf.sum.post.stop                       2.909248e+00
## D.TfIdf.sum.post.stem                       2.622665e+00
## D.sum.TfIdf                                 2.690770e+00
## color.fctrWhite                             3.083885e+00
## condition.fctrNew                           3.114490e+00
## D.nstopwrds.log                             2.589084e+00
## color.fctrBlack                             2.396730e+00
## D.nchrs.log                                 2.779848e+00
## prdline.my.fctriPadmini                     2.212984e+00
## storage.fctr16                              2.500840e+00
## prdline.my.fctriPad 3+                      2.600492e+00
## storage.fctrUnknown                         1.942625e+00
## carrier.fctrUnknown                         1.958878e+00
## cellular.fctr1                              2.436548e+00
## carrier.fctrVerizon                         1.492578e+00
## condition.fctrNew other (see details)       1.632602e+00
## D.nwrds.log                                 2.028829e+00
## D.nuppr.log                                 2.315793e+00
## prdline.my.fctriPad 1                       3.047200e+00
## prdline.my.fctriPadmini 2+                  2.371923e+00
## cellular.fctrUnknown                        1.895589e+00
## carrier.fctrAT&T                            1.363287e+00
## color.fctrSpace Gray                        2.133812e+00
## storage.fctr32                              1.581651e+00
## color.fctrGold                              1.238760e+00
## condition.fctrFor parts or not working      1.493784e+00
## D.nwrds.unq.log                             1.236413e+00
## prdline.my.fctriPad 2                       1.908280e+00
## D.terms.n.post.stem.log                     1.422731e+00
## D.terms.n.post.stop.log                     1.301554e+00
## D.terms.n.post.stem                         1.360814e+00
## D.terms.n.post.stop                         1.294931e+00
## carrier.fctrSprint                          6.740771e-01
## condition.fctrManufacturer refurbished      7.688005e-01
## D.npnct05.log                               5.168400e-01
## D.npnct13.log                               1.079486e+00
## D.terms.n.stem.stop.Ratio                   6.401453e-01
## prdline.my.fctrUnknown:.clusterid.fctr2     6.972380e-01
## prdline.my.fctriPad 2:.clusterid.fctr5      6.998488e-01
## D.npnct11.log                               1.304090e+00
## D.ndgts.log                                 1.076077e+00
## prdline.my.fctriPad 3+:.clusterid.fctr3     5.197071e-01
## D.npnct08.log                               4.044977e-01
## D.npnct15.log                               9.980478e-01
## D.npnct12.log                               1.985962e-01
## prdline.my.fctriPad 1:.clusterid.fctr4      1.244787e-01
## prdline.my.fctriPadAir:.clusterid.fctr3     1.453009e-01
## D.npnct01.log                               3.231315e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4     3.380535e-01
## prdline.my.fctriPad 1:.clusterid.fctr2      3.730643e-01
## condition.fctrSeller refurbished            5.779163e-01
## prdline.my.fctriPadmini:.clusterid.fctr2    2.902052e-01
## prdline.my.fctriPadmini:.clusterid.fctr3    1.749152e-01
## D.npnct24.log                               1.773042e-01
## D.npnct14.log                               2.823363e-01
## carrier.fctrT-Mobile                        5.012177e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 1.666519e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 2.104155e-01
## prdline.my.fctriPad 2:.clusterid.fctr2      2.177715e-01
## D.npnct16.log                               2.949843e-01
## prdline.my.fctriPadmini:.clusterid.fctr4    3.558532e-01
## prdline.my.fctriPadAir:.clusterid.fctr4     1.637150e-01
## prdline.my.fctriPad 1:.clusterid.fctr3      1.827763e-01
## prdline.my.fctriPadAir:.clusterid.fctr2     3.074205e-01
## prdline.my.fctriPad 2:.clusterid.fctr4      5.581525e-01
## prdline.my.fctrUnknown:.clusterid.fctr3     3.872699e-01
## D.npnct06.log                               1.668731e-01
## prdline.my.fctriPad 2:.clusterid.fctr3      7.102360e-02
## D.npnct03.log                               6.886715e-02
## prdline.my.fctriPad 3+:.clusterid.fctr2     1.727758e-01
## D.npnct10.log                               1.633006e-01
## prdline.my.fctriPadmini:.clusterid.fctr5    1.501172e-01
## carrier.fctrOther                           6.336752e-02
## D.npnct28.log                               7.262729e-04
## D.npnct09.log                               9.683639e-04
## prdline.my.fctrUnknown:.clusterid.fctr4     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5 0.000000e+00
##                                             Final.rf.importance
## startprice.diff                                    1.000000e+02
## biddable                                           9.714977e+01
## idseq.my                                           3.655118e+01
## D.ratio.sum.TfIdf.nwrds                            4.029252e+00
## D.TfIdf.sum.stem.stop.Ratio                        4.111189e+00
## storage.fctr64                                     2.509445e+00
## prdline.my.fctriPadAir                             3.604291e+00
## D.ratio.nstopwrds.nwrds                            3.129337e+00
## D.TfIdf.sum.post.stop                              2.909248e+00
## D.TfIdf.sum.post.stem                              2.622665e+00
## D.sum.TfIdf                                        2.690770e+00
## color.fctrWhite                                    3.083885e+00
## condition.fctrNew                                  3.114490e+00
## D.nstopwrds.log                                    2.589084e+00
## color.fctrBlack                                    2.396730e+00
## D.nchrs.log                                        2.779848e+00
## prdline.my.fctriPadmini                            2.212984e+00
## storage.fctr16                                     2.500840e+00
## prdline.my.fctriPad 3+                             2.600492e+00
## storage.fctrUnknown                                1.942625e+00
## carrier.fctrUnknown                                1.958878e+00
## cellular.fctr1                                     2.436548e+00
## carrier.fctrVerizon                                1.492578e+00
## condition.fctrNew other (see details)              1.632602e+00
## D.nwrds.log                                        2.028829e+00
## D.nuppr.log                                        2.315793e+00
## prdline.my.fctriPad 1                              3.047200e+00
## prdline.my.fctriPadmini 2+                         2.371923e+00
## cellular.fctrUnknown                               1.895589e+00
## carrier.fctrAT&T                                   1.363287e+00
## color.fctrSpace Gray                               2.133812e+00
## storage.fctr32                                     1.581651e+00
## color.fctrGold                                     1.238760e+00
## condition.fctrFor parts or not working             1.493784e+00
## D.nwrds.unq.log                                    1.236413e+00
## prdline.my.fctriPad 2                              1.908280e+00
## D.terms.n.post.stem.log                            1.422731e+00
## D.terms.n.post.stop.log                            1.301554e+00
## D.terms.n.post.stem                                1.360814e+00
## D.terms.n.post.stop                                1.294931e+00
## carrier.fctrSprint                                 6.740771e-01
## condition.fctrManufacturer refurbished             7.688005e-01
## D.npnct05.log                                      5.168400e-01
## D.npnct13.log                                      1.079486e+00
## D.terms.n.stem.stop.Ratio                          6.401453e-01
## prdline.my.fctrUnknown:.clusterid.fctr2            6.972380e-01
## prdline.my.fctriPad 2:.clusterid.fctr5             6.998488e-01
## D.npnct11.log                                      1.304090e+00
## D.ndgts.log                                        1.076077e+00
## prdline.my.fctriPad 3+:.clusterid.fctr3            5.197071e-01
## D.npnct08.log                                      4.044977e-01
## D.npnct15.log                                      9.980478e-01
## D.npnct12.log                                      1.985962e-01
## prdline.my.fctriPad 1:.clusterid.fctr4             1.244787e-01
## prdline.my.fctriPadAir:.clusterid.fctr3            1.453009e-01
## D.npnct01.log                                      3.231315e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4            3.380535e-01
## prdline.my.fctriPad 1:.clusterid.fctr2             3.730643e-01
## condition.fctrSeller refurbished                   5.779163e-01
## prdline.my.fctriPadmini:.clusterid.fctr2           2.902052e-01
## prdline.my.fctriPadmini:.clusterid.fctr3           1.749152e-01
## D.npnct24.log                                      1.773042e-01
## D.npnct14.log                                      2.823363e-01
## carrier.fctrT-Mobile                               5.012177e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3        1.666519e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2        2.104155e-01
## prdline.my.fctriPad 2:.clusterid.fctr2             2.177715e-01
## D.npnct16.log                                      2.949843e-01
## prdline.my.fctriPadmini:.clusterid.fctr4           3.558532e-01
## prdline.my.fctriPadAir:.clusterid.fctr4            1.637150e-01
## prdline.my.fctriPad 1:.clusterid.fctr3             1.827763e-01
## prdline.my.fctriPadAir:.clusterid.fctr2            3.074205e-01
## prdline.my.fctriPad 2:.clusterid.fctr4             5.581525e-01
## prdline.my.fctrUnknown:.clusterid.fctr3            3.872699e-01
## D.npnct06.log                                      1.668731e-01
## prdline.my.fctriPad 2:.clusterid.fctr3             7.102360e-02
## D.npnct03.log                                      6.886715e-02
## prdline.my.fctriPad 3+:.clusterid.fctr2            1.727758e-01
## D.npnct10.log                                      1.633006e-01
## prdline.my.fctriPadmini:.clusterid.fctr5           1.501172e-01
## carrier.fctrOther                                  6.336752e-02
## D.npnct28.log                                      7.262729e-04
## D.npnct09.log                                      9.683639e-04
## prdline.my.fctrUnknown:.clusterid.fctr4            0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5             0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4        0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5        0.000000e+00
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

![](ebayipads_spdiff_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 621 177
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
##                   label step_major step_minor     bgn    end elapsed
## 16     predict.data.new          9          0 329.209 335.35   6.141
## 17 display.session.info         10          0 335.351     NA      NA
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
## 11              fit.models          7          1 125.587 268.207 142.621
## 5         extract.features          3          0  21.414  90.455  69.042
## 12              fit.models          7          2 268.208 298.358  30.150
## 10              fit.models          7          0  99.427 125.587  26.160
## 14       fit.data.training          8          0 303.548 323.644  20.096
## 16        predict.data.new          9          0 329.209 335.350   6.141
## 15       fit.data.training          8          1 323.645 329.209   5.564
## 1              import.data          1          0  10.081  15.291   5.210
## 13              fit.models          7          3 298.358 303.548   5.190
## 2             inspect.data          2          0  15.292  20.094   4.802
## 8          select.features          5          0  94.676  98.481   3.805
## 7      manage.missing.data          4          1  91.469  94.675   3.206
## 6             cluster.data          4          0  90.456  91.469   1.013
## 9  partition.data.training          6          0  98.481  99.426   0.945
## 3               scrub.data          2          1  20.094  20.863   0.769
## 4           transform.data          2          2  20.863  21.414   0.551
##    duration
## 11  142.620
## 5    69.041
## 12   30.150
## 10   26.160
## 14   20.096
## 16    6.141
## 15    5.564
## 1     5.210
## 13    5.190
## 2     4.802
## 8     3.805
## 7     3.206
## 6     1.013
## 9     0.945
## 3     0.769
## 4     0.551
## [1] "Total Elapsed Time: 335.35 secs"
```

![](ebayipads_spdiff_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.1 (2015-06-18)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.4 (Yosemite)
## 
## locale:
## [1] C/en_US.UTF-8/C/C/C/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] randomForest_4.6-10 glmnet_2.0-2        arm_1.8-6          
##  [4] lme4_1.1-8          Matrix_1.2-2        MASS_7.3-43        
##  [7] rpart.plot_1.5.2    rpart_4.1-10        ROCR_1.0-7         
## [10] gplots_2.17.0       sampling_2.7        tidyr_0.2.0        
## [13] entropy_1.2.1       dynamicTreeCut_1.62 proxy_0.4-15       
## [16] tm_0.6-2            NLP_0.1-8           stringr_1.0.0      
## [19] dplyr_0.4.2         plyr_1.8.3          sqldf_0.4-10       
## [22] RSQLite_1.0.0       DBI_0.3.1           gsubfn_0.6-6       
## [25] proto_0.3-10        reshape2_1.4.1      gdata_2.17.0       
## [28] doMC_1.3.3          iterators_1.0.7     foreach_1.4.2      
## [31] doBy_4.5-13         survival_2.38-3     caret_6.0-52       
## [34] ggplot2_1.0.1       lattice_0.20-33    
## 
## loaded via a namespace (and not attached):
##  [1] splines_3.2.1       gtools_3.5.0        assertthat_0.1     
##  [4] stats4_3.2.1        yaml_2.1.13         slam_0.1-32        
##  [7] quantreg_5.11       pROC_1.8            chron_2.3-47       
## [10] digest_0.6.8        RColorBrewer_1.1-2  minqa_1.2.4        
## [13] colorspace_1.2-6    htmltools_0.2.6     lpSolve_5.6.11     
## [16] BradleyTerry2_1.0-6 SparseM_1.6         scales_0.2.5       
## [19] brglm_0.5-9         mgcv_1.8-7          car_2.0-25         
## [22] nnet_7.3-10         lazyeval_0.1.10     pbkrtest_0.4-2     
## [25] magrittr_1.5        evaluate_0.7        nlme_3.1-121       
## [28] class_7.3-13        tools_3.2.1         formatR_1.2        
## [31] munsell_0.4.2       compiler_3.2.1      e1071_1.6-6        
## [34] caTools_1.17.1      nloptr_1.0.4        bitops_1.0-6       
## [37] labeling_0.3        rmarkdown_0.7       gtable_0.1.2       
## [40] codetools_0.2-14    abind_1.4-3         R6_2.1.0           
## [43] knitr_1.10.5        KernSmooth_2.23-15  stringi_0.5-5      
## [46] Rcpp_0.12.0         coda_0.17-1
```
