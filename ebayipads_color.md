# eBay:iPads:: sold classification:: color
bdanalytics  

**  **    
**Date: (Wed) Jul 29, 2015**    

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

 color:
        All.Interact.X.glmnet: 
            fit_RMSE=88.64520; 
            prdline.my.fctr:D.TfIdf.sum.stem.stop.Ratio=100.00;
            prdline.my.fctr:condition.fctr=77.35
            D.TfIdf.sum.stem.stop.Ratio=68.18
            prdline.my.fctr:color.fctr=68.12
            prdline.my.fctr:storage.fctr=63.32
            
    All.X.no.rnorm.rf: Leaderboard: 0.80638
        newobs_tbl=[N=550, Y=248]; submit_filename=color_Final_rf_submit
        OOB_conf_mtrx=[YN=108, NY=54]=162; max.Accuracy.OOB=0.8169492
            opt.prob.threshold.OOB=0.5
            biddable=100.00; startprice.diff=77.90; idseq.my=48.49; 
                D.ratio.sum.TfIdf.nwrds=6.48; storage.fctr=4.74;
                    D.TfIdf.sum.stem.stop.Ratio=4.57; prdline.my=4.32;
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
glb_out_pfx <- "color_"
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
        mod_raw <- gsub(" 128 gb ", " 128gb ", mod_raw, ignore.case=TRUE);  
        mod_raw <- gsub(" actuuly ", " actual ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" Apple care ", " Applecare ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" ans ", " and ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" bacK!wiped ", " bacK ! wiped ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" backplate", " back plate", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("\\bbarley", "barely", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" bend ", " bent ", mod_raw, ignore.case=TRUE);         
        mod_raw <- gsub("Best Buy", "BestBuy", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" black\\.Device ", " black \\. Device ", mod_raw,
                        ignore.case=TRUE);        
        mod_raw <- gsub(" blocks", " blocked", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" brokenCharger ", " broken Charger ", mod_raw, ignore.case=TRUE);
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
        mod_raw <- gsub("\\(gray color", "\\(spacegray color", mod_raw, ignore.case=TRUE); 
        mod_raw <- gsub(" GREAT\\.SCreen ", " GREAT\\. SCreen ", mod_raw,
                        ignore.case=TRUE);        
        mod_raw <- gsub(" Framing ", " Frame ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("iCL0UD", "iCLOUD", mod_raw, ignore.case=TRUE);        
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
        mod_raw <- gsub(" oped ", " opened ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" opening", " opened", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" operated", " operational", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" perfectlycord ", " perfectly cord ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" performance", " performs", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" personalized ", " personal ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" products ", " product ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" Keeped ", " Kept ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" knicks ", " nicks ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("^READiPad ", "READ iPad ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" re- assemble ", " reassemble ", mod_raw, ignore.case=TRUE);
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
        mod_raw <- gsub("\\bspace (grey|gray)", "spacegray", mod_raw, ignore.case=TRUE); 
        mod_raw <- gsub(" spec ", " speck ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub("^somescratches ", "some scratches ", mod_raw, ignore.case=TRUE);  
        mod_raw <- gsub(" Sticker ", " Stickers ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("SWAPPA\\.COM", "SWAPPACOM", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" T- Mobile", "  TMobile", mod_raw, ignore.case=TRUE);  
        mod_raw <- gsub(" touchscreen ", " touch screen ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" UnlockedCracked ", " Unlocked Cracked ", mod_raw,
                        ignore.case=TRUE);         
        mod_raw <- gsub(" uppser ", " upper ", mod_raw, ignore.case=TRUE); 
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
    ,"abused","across","adaptor","add","advised","antenna","anti","anyone","anything"
        ,"applied","area","arizona","att"
    ,"backlight","beetle","beginning","besides","bidder","bonus","boot","bound","bruises"
    ,"capacity","changed","changing","chrome","closely"
        ,"confidence","considerable","consumer","contents","control","cream","cuts"
    ,"daily","date","daughter","decent","defender","defense","degree","depicted"
        ,"disclaimer","distressed","divider"
        ,"dlxnqat9g5wt","done","dont","durable","dust","duty"
        ,"either","emblem","erased","ereader","essentially","every","exact","exhibition"
    ,"facing","faint","february","film","final","flickers","folding","forgot","forwarders"
                                    ,"games","generic","genuine","glitter","goes","grey"
                            ,"half","handstand","hdmi","high","higher","hole","hospital"
                            ,"immaculate","impact","instead","intended","interior","intro"
                                        ,"jack","july"
                                        ,"keeps","kids","kind","known"
    ,"largest","last","late","let","letters","level"
        ,"lifting","limited","line","lining","liquidation"
        ,"local","long","longer","looping","loose","loss"
                    ,"mb292ll","mc707ll","mc916ll","mc991ll","md789ll","mf432ll","mgye2ll"
                    ,"middle", "mind","mixed","mostly"
                                        ,"neither","none","november"
                                        ,"occasional","oem","online","outside"
    ,"paperwork","past","period","pet","photograph","piece","played","plug"
        ,"poor","portfolio","portion","pouch","preinstalled","price","proof","provided"
    ,"ranging","rather"
        ,"real","realized","reassemble","receipt","recently","red"
            ,"reflected","refunds","remote","repeat"
            ,"required","reserve","residue","restarts","result","reviewed"
        ,"ringer","roughly","running"
    ,"said","school"
        ,"seamlessly","seconds","seem","semi","send","september","serious","setup"
        ,"shell","short","site","size","sleeve","slice","smoke","smooth","smudge"
        ,"softer","software","somewhat","soon"
        ,"space","sparingly","sparkiling","special","speed"
        ,"stains","standup","status","stopped","strictly"
        ,"subtle","sustained","swappacom","swivel"
    ,"take","technical","tempered","texture","thank","therefore","think","though"
        ,"toddler","totally","touchy","toys","tried","typical"
                                        ,"university","unknown","untouched","upgrade"
                                        ,"valid","vary","version"
                                        ,"want","website","whole","winning","wrapped"
                                        ,"zaag","zero", "zombie","zoogue"
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
    #glb_model_evl_criteria <- c("min.RMSE.fit", "max.R.sq.fit", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL #"Low.cor.X.glm"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

glb_dsp_cols <- c("sold", ".grpid", "color", "condition", "cellular", "carrier", "storage")

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

![](ebayipads_color_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 14.411  NA      NA
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

# !_sp
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

![](ebayipads_color_files/figure-html/import.data-1.png) 

```r
print(myplot_histogram(mrg_allobs_df, "startprice.diff", 
                     fill_col_name = "biddable"))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](ebayipads_color_files/figure-html/import.data-2.png) 

```r
glb_allobs_df <- mrg_allobs_df
glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                  "startprice.log", "startprice.predict.")
###                                  

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
## 1  import.data          1          0 14.411 26.493  12.082
## 2 inspect.data          2          0 26.493     NA      NA
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

![](ebayipads_color_files/figure-html/inspect.data-1.png) 

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

![](ebayipads_color_files/figure-html/inspect.data-2.png) 

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

![](ebayipads_color_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: startprice.diff"
```

![](ebayipads_color_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_color_files/figure-html/inspect.data-5.png) 

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
## 2 inspect.data          2          0 26.493 39.269  12.776
## 3   scrub.data          2          1 39.269     NA      NA
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
##            label step_major step_minor    bgn   end elapsed
## 3     scrub.data          2          1 39.269 40.04   0.771
## 4 transform.data          2          2 40.041    NA      NA
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
## 4   transform.data          2          2 40.041 40.684   0.643
## 5 extract.features          3          0 40.685     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 40.691  NA      NA
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
## 1                extract.features_bgn          1          0 40.691 40.706
## 2 extract.features_factorize.str.vars          2          0 40.706     NA
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
        txt_X_df[, paste(txt_var_pfx, ".P.black", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("black", glb_allobs_df[, txt_var],
                                               perl=TRUE))    
        txt_X_df[, paste(txt_var_pfx, ".P.white", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("white", glb_allobs_df[, txt_var],
                                               perl=TRUE))    
        txt_X_df[, paste(txt_var_pfx, ".P.gold", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("gold", glb_allobs_df[, txt_var],
                                               perl=TRUE))    
        txt_X_df[, paste(txt_var_pfx, ".P.spacegray", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("spacegray", glb_allobs_df[, txt_var],
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
## 2 extract.features_factorize.str.vars          2          0 40.706 41.029
## 3       extract.features_process.text          3          0 41.030     NA
##   elapsed
## 2   0.324
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
## 3          0 41.030 42.748   1.718
## 4          1 42.748     NA      NA
## [1] "Remaining compound terms in descr.my: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn    end elapsed
## 4          1 42.748 42.753   0.005
## 5          0 42.753     NA      NA
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
## [1] "Rows: 668; Cols: 4"
##              TfIdf      term freq pos
## condition 209.2728 condition  498 137
## new       126.1758       new  156 402
## used      124.5458      used  240 636
## good      121.4973      good  197 268
## scratches 114.5796 scratches  254 521
## screen    107.2911    screen  210 523
##                TfIdf       term freq pos
## scuffs     31.166256     scuffs   39 528
## scratching  7.623387 scratching    9 522
## including   6.588369  including    8 302
## sure        6.363052       sure    6 587
## protected   3.943444  protected    3 478
## ebay        2.305685       ebay    2 206
##                TfIdf        term freq pos
## blemish     1.137558     blemish    1  74
## cables      1.137558      cables    1  96
## engravement 1.137558 engravement    1 211
## handling    1.137558    handling    1 281
## mic         1.137558         mic    1 379
## 79in        1.034144        79in    1  14
##                TfIdf        term freq pos
## blemish     1.137558     blemish    1  74
## cables      1.137558      cables    1  96
## engravement 1.137558 engravement    1 211
## handling    1.137558    handling    1 281
## mic         1.137558         mic    1 379
## 79in        1.034144        79in    1  14
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
## [1] "Rows: 528; Cols: 4"
##            TfIdf    term freq pos
## condit  209.2617  condit  496 111
## use     147.7914     use  291 501
## scratch 129.1467 scratch  286 409
## new     126.1758     new  156 316
## good    121.5866    good  197 213
## ipad    108.6364    ipad  232 249
##              TfIdf     term freq pos
## near     21.703837     near   34 311
## name      3.920486     name    3 310
## <db><cf>  3.602633 <db><cf>    2  17
## pin       3.227959      pin    2 358
## happen    2.593896   happen    2 224
## appli     1.625083    appli    1  43
##                 TfIdf         term freq pos
## marksabsolut 1.421948 marksabsolut    1 292
## often        1.421948        often    1 326
## 360          1.263954          360    1   9
## 975          1.137558          975    1  15
## mic          1.137558          mic    1 298
## 79in         1.034144         79in    1  14
##                 TfIdf         term freq pos
## marksabsolut 1.421948 marksabsolut    1 292
## often        1.421948        often    1 326
## 360          1.263954          360    1   9
## 975          1.137558          975    1  15
## mic          1.137558          mic    1 298
## 79in         1.034144         79in    1  14
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
## terms.n.post.stop         -0.086024184
## terms.n.post.stop.log     -0.066489274
## TfIdf.sum.post.stop       -0.034561568
## terms.n.post.stem         -0.085753719
## terms.n.post.stem.log     -0.066421421
## TfIdf.sum.post.stem       -0.037435134
## terms.n.stem.stop.Ratio    0.020583201
## TfIdf.sum.stem.stop.Ratio -0.009352534
##                           label step_major step_minor    bgn    end
## 5 extract.features_build.corpus          4          0 42.753 53.408
## 6  extract.features_extract.DTM          5          0 53.408     NA
##   elapsed
## 5  10.655
## 6      NA
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
## 6 extract.features_extract.DTM          5          0 53.408 55.493   2.086
## 7  extract.features_report.DTM          6          0 55.494     NA      NA
## [1] "Reporting TfIDf terms for descr.my..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 2657, terms: 528)>>
## Non-/sparse entries: 8212/1394684
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

![](ebayipads_color_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_color_files/figure-html/extract.features-2.png) ![](ebayipads_color_files/figure-html/extract.features-3.png) 

```
## Warning in rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df,
## terms_TfIdf_df): object 'full_TfIdf_mtrx' not found
```

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 55.494 57.239   1.745
## 8   extract.features_bind.DTM          7          0 57.239     NA      NA
## [1] "Binding DTM for descr.my..."
##                       label step_major step_minor    bgn    end elapsed
## 8 extract.features_bind.DTM          7          0 57.239 57.645   0.407
## 9 extract.features_bind.DXM          8          0 57.646     NA      NA
## [1] "Binding DXM for descr.my..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_color_files/figure-html/extract.features-4.png) 

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df

# Use model info provided in description
mydsp_obs(list(description.contains="a[[:digit:]]"), cols=glb_dsp_cols, all=TRUE)
```

```
##      UniqueID sold.fctr prdline.my sold .grpid   color condition cellular
## 618     10618         Y  iPad mini    1   <NA>   Black      Used        0
## 940     10940         N     iPad 3    0   <NA>   Black      Used        1
## 2472    12474      <NA>    Unknown   NA   <NA> Unknown      Used  Unknown
##      carrier storage
## 618     None      16
## 940  Verizon      16
## 2472 Unknown Unknown
##                                                                                                    descr.my
## 618  Nice Apple iPad Mini 16GB Wi- Fi 7.9&#034; spacegray MF432LL/ A A1432 Locked It does work just cannot 
## 940     LIKE NEW (MODEL A1430) + BLUETOOTH KEYBOARD (LATEST MODEL A1314), LEATHER CREAM SMART COVER, BLACK 
## 2472     here we have spacegray apple ipad mini a1432 no charger works great has small nicks nothing major
```

```r
glb_allobs_df[glb_allobs_df$UniqueID == 12474, "prdline.my"] <- "iPad mini"
glb_allobs_df[glb_allobs_df$UniqueID == 12474, "color"] <- "Space Gray"
glb_allobs_df[glb_allobs_df$UniqueID == 12474, "cellular"] <- "0"
glb_allobs_df[glb_allobs_df$UniqueID == 12474, "carrier"] <- "None"

mydsp_obs(list(description.contains="m(.{4})ll"), cols=glb_dsp_cols, all=TRUE)
```

```
##      UniqueID sold.fctr       prdline.my sold .grpid   color
## 617     10617         Y           iPad 2    1   <NA>   White
## 618     10618         Y        iPad mini    1   <NA>   Black
## 992     10992         N           iPad 2    0   <NA>   White
## 1105    11105         N iPad mini Retina    0   <NA>    Gold
## 1359    11360         N           iPad 3    0   <NA> Unknown
## 1360    11361         Y          Unknown    1   <NA> Unknown
## 1365    11366         Y           iPad 1    1   <NA> Unknown
## 2637    12639      <NA>           iPad 2   NA   <NA>   Black
##                     condition cellular carrier storage
## 617                      Used        0    None      64
## 618                      Used        0    None      16
## 992                      Used        0    None      16
## 1105                     Used        0    None      16
## 1359                     Used  Unknown Unknown Unknown
## 1360                     Used  Unknown Unknown Unknown
## 1365                     Used  Unknown Unknown Unknown
## 2637 For parts or not working        0    None      64
##                                                                                                     descr.my
## 617         This a used Apple iPad 2 64GB, Wi- Fi, 9.7in - White (MC991LL/ A) shows signs of wear, has been 
## 618   Nice Apple iPad Mini 16GB Wi- Fi 7.9&#034; spacegray MF432LL/ A A1432 Locked It does work just cannot 
## 992  Up for auction is this APPLE iPad 1st Gen Model MB292LL 16 GB of Memory Storage 9.7&#034; touch screen 
## 1105 Like New Condition Apple iPad Mini 3 MGYE2LL/ A 16GB Wi- Fi Gold Version Tablet/ eReader. Includes USB 
## 1359                  iPad 3 Black 64Gb storage Model Mc707ll/ a iPad is in very nice shape, glass and case 
## 1360   APPLE iPAD AIR 32GB WHITE MD789LL/ B WHITE. This item is Previously Lightly Used, in Good Condition. 
## 1365   Item still in complete working order, minor scratches, normal wear and tear but no damage. screen is 
## 2637  IPAD 2 64GB BLACK MODEL MC916LL/ A WIFI ONLY MODEL.  PICTURE OF IPAD IS ACTUAL UNIT YOU WILL RECEIVE.
```

```r
glb_allobs_df[glb_allobs_df$UniqueID == 11360, "color"] <- "Black"
glb_allobs_df[glb_allobs_df$UniqueID == 11360, "storage"] <- "64"
glb_allobs_df[glb_allobs_df$UniqueID == 11360, "cellular"] <- "0"
glb_allobs_df[glb_allobs_df$UniqueID == 11360, "carrier"] <- "None"

glb_allobs_df[glb_allobs_df$UniqueID == 11361, "prdline.my"] <- "iPad Air"
glb_allobs_df[glb_allobs_df$UniqueID == 11361, "storage"] <- "32"
glb_allobs_df[glb_allobs_df$UniqueID == 11361, "color"] <- "White"
glb_allobs_df[glb_allobs_df$UniqueID == 11361, "cellular"] <- "0"
glb_allobs_df[glb_allobs_df$UniqueID == 11361, "carrier"] <- "None"

# mydsp_obs(list(description.contains="mini(?!m)"), perl=TRUE, cols="D.P.mini", all=TRUE)
# mydsp_obs(list(D.P.mini=1), cols="D.P.mini", all=TRUE)
# mydsp_obs(list(D.P.mini=1, productline="Unknown"), cols="D.P.mini", all=TRUE)

# mydsp_obs(list(description.contains="(?<![fhp])air"), perl=TRUE, all=TRUE)
# mydsp_obs(list(description.contains="air"), perl=FALSE, cols="D.P.air", all=TRUE)
# mydsp_obs(list(D.P.air=1, productline="Unknown"), cols="D.P.air", all=TRUE)

print(mycreate_sqlxtab_df(glb_allobs_df, c("prdline.my", "productline", "D.P.mini",
                                           glb_rsp_var)))
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
## 15          Unknown          Unknown        0         Y  80
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
## 35        iPad mini        iPad mini        1      <NA>   3
## 36      iPad mini 3      iPad mini 3        1      <NA>   3
## 37 iPad mini Retina iPad mini Retina        0         N   3
## 38          Unknown          Unknown        1      <NA>   2
## 39      iPad mini 2      iPad mini 2        1         N   2
## 40      iPad mini 3      iPad mini 3        1         N   2
## 41          Unknown          Unknown        1         N   1
## 42          Unknown          Unknown        1         Y   1
## 43           iPad 5           iPad 5        0         Y   1
## 44         iPad Air          Unknown        0         Y   1
## 45        iPad mini          Unknown        1      <NA>   1
## 46        iPad mini        iPad mini        2         Y   1
## 47      iPad mini 2      iPad mini 2        1         Y   1
## 48 iPad mini Retina iPad mini Retina        1         N   1
```

```r
print(glb_allobs_df[(glb_allobs_df$productline == "Unknown") & 
                    (glb_allobs_df$D.P.mini > 0), 
                    c(glb_id_var, glb_category_var, glb_dsp_cols, glb_txt_vars)])
```

```
##      UniqueID prdline.my sold .grpid      color                condition
## 1172    11172    Unknown    0      8    Unknown                     Used
## 1803    11804    Unknown    1   <NA>      White       Seller refurbished
## 2223    12225    Unknown   NA      8    Unknown                     Used
## 2472    12474  iPad mini   NA   <NA> Space Gray                     Used
## 2623    12625    Unknown   NA   <NA>      White For parts or not working
##      cellular carrier storage
## 1172  Unknown Unknown      16
## 1803        1    AT&T Unknown
## 2223  Unknown Unknown      16
## 2472        0    None Unknown
## 2623  Unknown Unknown Unknown
##                                                                                                    descr.my
## 1172     IPAD mini .  not sure of what generation it can be.  selling as is or best offer. had a crack but 
## 1803    30 Day Warranty.  Refurbished iPad Mini with signs of normal wear including possible scratching on 
## 2223     IPAD mini .  not sure of what generation it can be.  selling as is or best offer. had a crack but 
## 2472     here we have spacegray apple ipad mini a1432 no charger works great has small nicks nothing major 
## 2623 Lot of 10 mixed iPad minis. Colors, models &amp; storage capacity vary between each lot. There may be
```

```r
glb_allobs_df[(glb_allobs_df$D.P.mini == 1) & (glb_allobs_df$productline == "Unknown"),
              "prdline.my"] <- "iPad mini"

print(mycreate_sqlxtab_df(glb_allobs_df, c("prdline.my", "productline", "D.P.air",
                                           glb_rsp_var)))
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
## 26           iPad 3           iPad 3       0      <NA>  55
## 27      iPad mini 2      iPad mini 2       0      <NA>  55
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
## 39          Unknown          Unknown       1      <NA>   1
## 40          Unknown          Unknown       1         N   1
## 41           iPad 3           iPad 3       1         Y   1
## 42           iPad 4           iPad 4       1         N   1
## 43           iPad 5           iPad 5       0         Y   1
## 44         iPad Air          Unknown       1         Y   1
## 45         iPad Air         iPad Air       1      <NA>   1
## 46       iPad Air 2       iPad Air 2       2         N   1
## 47        iPad mini          Unknown       0         N   1
## 48        iPad mini          Unknown       0         Y   1
## 49      iPad mini 2      iPad mini 2       1      <NA>   1
```

```r
print(glb_allobs_df[(glb_allobs_df$productline == "Unknown") & 
                    (glb_allobs_df$D.P.air > 0), 
                    c(glb_id_var, glb_category_var, glb_dsp_cols, glb_txt_vars)])
```

```
##      UniqueID prdline.my sold .grpid      color condition cellular carrier
## 946     10946    Unknown    0   <NA>    Unknown      Used  Unknown Unknown
## 1360    11361   iPad Air    1   <NA>      White      Used        0    None
## 2433    12435    Unknown   NA   <NA> Space Gray      Used  Unknown Unknown
##      storage
## 946  Unknown
## 1360      32
## 2433     128
##                                                                                                   descr.my
## 946     Gently used apple iPad Air, no scratches on screen and almost no visible wear on back of item. No 
## 1360 APPLE iPAD AIR 32GB WHITE MD789LL/ B WHITE. This item is Previously Lightly Used, in Good Condition. 
## 2433    ***128gb***  black/ spacegray iPad Air excellent used condition(no scratches, dents, or blemishes)
```

```r
#glb_allobs_df[glb_allobs_df$UniqueID == 11863, "D.P.air"] <- 0
glb_allobs_df[(glb_allobs_df$D.P.air == 1) & (glb_allobs_df$productline == "Unknown"),
              "prdline.my"] <- "iPad Air"

print(glb_allobs_df[(glb_allobs_df$UniqueID %in% c(11767, 11811, 12156)),
                    c(glb_id_var, "sold",
    "prdline.my", "color", "condition", "cellular", "carrier", "storage", "descr.my")])
```

```
##      UniqueID sold prdline.my   color                condition cellular
## 1766    11767    0    Unknown Unknown For parts or not working  Unknown
## 1810    11811    0    Unknown   Black       Seller refurbished        0
## 2154    12156   NA    Unknown   Black                     Used        0
##      carrier storage
## 1766 Unknown Unknown
## 1810    None Unknown
## 2154    None      32
##                                                                                                 descr.my
## 1766                    Ipad 2 32gb Housing. Some scratches and small dents, but overall good condition.
## 1810 30 Day Warranty.  Refurbished iPad 2 with scratching on screen and wear on back plate.  Comes with 
## 2154  Original IPAD 1st generation - used one owner (myself)Good shape as pictured. Fully functional as
```

```r
glb_allobs_df[glb_allobs_df$UniqueID == 11767, "prdline.my"] <- "iPad 2"
glb_allobs_df[glb_allobs_df$UniqueID == 11767, "storage"] <- "32"
glb_allobs_df[glb_allobs_df$UniqueID == 11811, "prdline.my"] <- "iPad 2"
glb_allobs_df[glb_allobs_df$UniqueID == 12156, "prdline.my"] <- "iPad 1"

# mydsp_obs(list(prdline.my="Unknown"), all=TRUE)

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
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
print(mycreate_sqlxtab_df(subset(glb_allobs_df, color == "Unknown"), 
                        c("color", "D.P.black", "D.P.gold", "D.P.spacegray", "D.P.white")))
```

```
##     color D.P.black D.P.gold D.P.spacegray D.P.white   .n
## 1 Unknown         0        0             0         0 1017
## 2 Unknown         0        0             0         1    4
## 3 Unknown         1        0             0         0    4
## 4 Unknown         0        0             1         0    1
## 5 Unknown         1        0             0         1    1
```

```r
print(glb_allobs_df[(glb_allobs_df$color == "Unknown") & (glb_allobs_df$D.P.black > 0), 
                    c(glb_id_var, "color", "D.P.black", "sold", "prdline.my", "condition",
                      "cellular", "carrier", "storage", "descr.my")])
```

```
##      UniqueID   color D.P.black sold prdline.my condition cellular carrier
## 631     10631 Unknown         1    1     iPad 2      Used        1    AT&T
## 683     10683 Unknown         1    0     iPad 2      Used        0    None
## 858     10858 Unknown         1    1    iPad 3+      Used        0    None
## 1243    11244 Unknown         1    0    Unknown      Used  Unknown Unknown
## 2135    12137 Unknown         1   NA     iPad 1      Used        1    AT&T
##      storage
## 631       16
## 683       32
## 858       16
## 1243 Unknown
## 2135      16
##                                                                                                     descr.my
## 631        Very good condition. Minor bumps and bruises. Only scratches on screen are in non- viewing black 
## 683       Comes with folding black case and is engraved in small letters on the back.  Still works perfectly
## 858                                                 screen cracked. name engraving in the back (blacked out)
## 1243       Ipad is in fair condition. Minor scratches on back. Edge around screen is black instead of white.
## 2135 Device is in AVERAGE used cosmetic condition with heavy scratches and wear. Color is black . Device is
```

```r
glb_allobs_df[glb_allobs_df$UniqueID == 12137, "color"] <- "Black"

print(glb_allobs_df[(glb_allobs_df$color == "Unknown") & (glb_allobs_df$D.P.spacegray > 0),
                    c(glb_id_var, "color", "D.P.spacegray", "prdline.my", "condition",
                      "cellular", "carrier", "storage", "descr.my")])
```

```
##      UniqueID   color D.P.spacegray prdline.my condition cellular carrier
## 2104    12106 Unknown             1    iPadAir      Used        0    None
##      storage
## 2104      16
##                                                                                                            descr.my
## 2104 This is an iPad Air first generation (spacegray color). It&#039;s a used iPad (just like new) as shown in the
```

```r
glb_allobs_df[glb_allobs_df$UniqueID %in% c(12106), "color"] <- "Space Gray"

print(glb_allobs_df[(glb_allobs_df$color == "Unknown") & (glb_allobs_df$D.P.white > 0),
                    c(glb_id_var, "color", "D.P.white", "prdline.my", "condition",
                      "cellular", "carrier", "storage", "descr.my")])
```

```
##      UniqueID   color D.P.white  prdline.my                condition
## 573     10573 Unknown         1 iPadmini 2+                     Used
## 809     10809 Unknown         1     iPad 3+                     Used
## 925     10925 Unknown         1 iPadmini 2+                     Used
## 1243    11244 Unknown         1     Unknown                     Used
## 1734    11735 Unknown         1     iPad 3+ For parts or not working
##      cellular carrier storage
## 573         0    None      16
## 809         0    None      64
## 925         0    None      64
## 1243  Unknown Unknown Unknown
## 1734        1 Verizon      16
##                                                                                                        descr.my
## 573                Like new white iPad mini no scratches always kept in case, sold with keyboard, box and cords
## 809          iPad 3 gen. 64GB, white, wifi- only. Condition = good as new, very minor sign of use. No charger. 
## 925  iPad mini 2/ Retina Display/ Latest Model/ 64GB/ Wi- Fi/ Silver&amp;White . Near Mint Condition excellent 
## 1243          Ipad is in fair condition. Minor scratches on back. Edge around screen is black instead of white.
## 1734             Device is in POOR used cosmetic condition with cracked outer glass. Color is White. Device is
```

```r
glb_allobs_df[glb_allobs_df$UniqueID %in% c(10573, 10809, 10925, 11735), "color"] <-
    "White"

glb_allobs_df$carrier.fctr <- as.factor(glb_allobs_df$carrier)
glb_allobs_df$cellular.fctr <- as.factor(glb_allobs_df$cellular)
glb_allobs_df$color.fctr <- as.factor(glb_allobs_df$color)
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
##                        label step_major step_minor     bgn     end elapsed
## 9  extract.features_bind.DXM          8          0  57.646 123.755  66.109
## 10      extract.features_end          9          0 123.756      NA      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                                    label step_major
## 9                              extract.features_bind.DXM          8
## 5                          extract.features_build.corpus          4
## 6                           extract.features_extract.DTM          5
## 7                            extract.features_report.DTM          6
## 3                          extract.features_process.text          3
## 8                              extract.features_bind.DTM          7
## 2                    extract.features_factorize.str.vars          2
## 1                                   extract.features_bgn          1
## 4 extract.features_process.text_reporting_compound_terms          3
##   step_minor    bgn     end elapsed duration
## 9          0 57.646 123.755  66.109   66.109
## 5          0 42.753  53.408  10.655   10.655
## 6          0 53.408  55.493   2.086    2.085
## 7          0 55.494  57.239   1.745    1.745
## 3          0 41.030  42.748   1.718    1.718
## 8          0 57.239  57.645   0.407    0.406
## 2          0 40.706  41.029   0.324    0.323
## 1          0 40.691  40.706   0.015    0.015
## 4          1 42.748  42.753   0.005    0.005
## [1] "Total Elapsed Time: 123.755 secs"
```

![](ebayipads_color_files/figure-html/extract.features-5.png) 

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

![](ebayipads_color_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 5 extract.features          3          0  40.685 125.107  84.422
## 6     cluster.data          4          0 125.107      NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor     bgn     end elapsed
## 6        cluster.data          4          0 125.107 126.073   0.966
## 7 manage.missing.data          4          1 126.074      NA      NA
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
##                    1600                    1521                    1521 
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
##                    1520                    1522                    2427 
##           D.npnct01.log           D.npnct02.log           D.npnct03.log 
##                    2579                    2657                    2614 
##           D.npnct04.log           D.npnct05.log           D.npnct06.log 
##                    2657                    2592                    2554 
##           D.npnct07.log           D.npnct08.log           D.npnct09.log 
##                    2656                    2581                    2641 
##           D.npnct10.log           D.npnct11.log           D.npnct12.log 
##                    2648                    2301                    2538 
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
##                 D.P.air               D.P.black               D.P.white 
##                    2636                    2640                    2647 
##                D.P.gold           D.P.spacegray 
##                    2655                    2650 
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
##                    1600                    1521                    1521 
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
##                    1520                    1522                    2427 
##           D.npnct01.log           D.npnct02.log           D.npnct03.log 
##                    2579                    2657                    2614 
##           D.npnct04.log           D.npnct05.log           D.npnct06.log 
##                    2657                    2592                    2554 
##           D.npnct07.log           D.npnct08.log           D.npnct09.log 
##                    2656                    2581                    2641 
##           D.npnct10.log           D.npnct11.log           D.npnct12.log 
##                    2648                    2301                    2538 
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
##                 D.P.air               D.P.black               D.P.white 
##                    2636                    2640                    2647 
##                D.P.gold           D.P.spacegray 
##                    2655                    2650 
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
##  [1] "D.T.condit"    "D.T.use"       "D.T.scratch"   "D.T.new"      
##  [5] "D.T.good"      "D.T.ipad"      "D.T.screen"    "D.T.great"    
##  [9] "D.T.work"      "D.T.excel"     "D.P.http"      "D.P.mini"     
## [13] "D.P.air"       "D.P.black"     "D.P.white"     "D.P.gold"     
## [17] "D.P.spacegray"
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
##     D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 5           0        0         0        0        0       0         0
## 130         0        0         0        0        0       0         0
##     D.P.white D.P.gold D.P.spacegray
## 5           0        0             0
## 130         0        0             0
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
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 1029         0        0         0        0        0       0         0
## 1077         0        0         0        0        0       0         0
##      D.P.white D.P.gold D.P.spacegray
## 1029         0        0             0
## 1077         0        0             0
```

![](ebayipads_color_files/figure-html/cluster.data-1.png) 

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
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 9          0 0.000000         0        0        0       0         0
## 13         0 0.340566         0        0        0       0         0
##    D.P.white D.P.gold D.P.spacegray
## 9          0        0             0
## 13         0        0             0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 32      10032         Y     iPad 1
## 1163    11163         N     iPad 1
##                                                                                                descr.my
## 32   In very good  condition, does show sign of use but mostly had a case on at all times. Still has a 
## 1163  Device is in GOOD used cosmetic condition with normal wear, engraving on back. Device is in 100% 
##      D.T.condit   D.T.use D.T.scratch D.T.new  D.T.good D.T.ipad
## 32    0.3026733 0.3988384           0       0 0.4691913        0
## 1163  0.2201260 0.2900643           0       0 0.3412301        0
##      D.T.screen D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 32            0         0        0         0        0        0       0
## 1163          0         0        0         0        0        0       0
##      D.P.black D.P.white D.P.gold D.P.spacegray
## 32           0         0        0             0
## 1163         0         0        0             0
```

![](ebayipads_color_files/figure-html/cluster.data-2.png) 

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
##   D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 1         0        0         0        0        0       0         0
## 2         0        0         0        0        0       0         0
##   D.P.white D.P.gold D.P.spacegray
## 1         0        0             0
## 2         0        0             0
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
##      D.P.black D.P.white D.P.gold D.P.spacegray
## 132          0         0        0             0
## 2382         0         0        0             0
```

![](ebayipads_color_files/figure-html/cluster.data-3.png) 

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
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 3  0.0000000        0         0        0        0       0         0
## 11 0.4008907        0         0        0        0       0         0
##    D.P.white D.P.gold D.P.spacegray
## 3          0        0             0
## 11         0        0             0
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
##      D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 40           0 0.4162473         0        0        0       0         0
## 1602         0 0.9365565         0        0        0       0         0
##      D.P.white D.P.gold D.P.spacegray
## 40           0        0             0
## 1602         0        0             0
```

![](ebayipads_color_files/figure-html/cluster.data-4.png) 

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
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 16         0        0         0        0        0       0         0
## 33         0        0         0        0        0       0         0
##    D.P.white D.P.gold D.P.spacegray
## 16         0        0             0
## 33         0        0             0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 44      10044         N    iPadAir
## 1166    11166         N    iPadAir
##                                                                               descr.my
## 44   Open Box Units Grade A Condition. Units may contain minor cosmetic imperfections.
## 1166                                                  Immaculate Condition. . In a Box
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.ipad D.T.screen
## 44     0.220126       0           0       0        0        0          0
## 1166   1.210693       0           0       0        0        0          0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 44           0        0         0        0        0       0         0
## 1166         0        0         0        0        0       0         0
##      D.P.white D.P.gold D.P.spacegray
## 44           0        0             0
## 1166         0        0             0
```

![](ebayipads_color_files/figure-html/cluster.data-5.png) 

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
##    D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 7          0 0.0000000         0        0        0       0         0
## 76         0 0.9365565         0        0        0       0         0
##    D.P.white D.P.gold D.P.spacegray
## 7          0        0             0
## 76         0        0             0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 491     10491         N   iPadmini
## 2564    12566      <NA>   iPadmini
##                                                                                                    descr.my
## 491                           Cracked screen, flaw is shown in picture, everything is fully functional and 
## 2564 daughter dropped cracked screen.  got some water in the cracks.  screen lights but you can&#039;t see 
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.ipad D.T.screen
## 491           0       0           0       0        0        0  0.4551091
## 2564          0       0           0       0        0        0  0.8090829
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 491          0        0         0        0        0       0         0
## 2564         0        0         0        0        0       0         0
##      D.P.white D.P.gold D.P.spacegray
## 491          0        0             0
## 2564         0        0             0
```

![](ebayipads_color_files/figure-html/cluster.data-6.png) 

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
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air D.P.black
## 4          0        0         0        0        0       0         0
## 18         0        0         0        0        0       0         0
##    D.P.white D.P.gold D.P.spacegray
## 4          0        0             0
## 18         0        0             0
## [1] "min distance(0.0000) pair:"
##   UniqueID sold.fctr  prdline.my descr.my D.T.condit D.T.use D.T.scratch
## 4    10004         N iPadmini 2+                   0       0           0
## 6    10006         Y iPadmini 2+                   0       0           0
##   D.T.new D.T.good D.T.ipad D.T.screen D.T.great D.T.work D.T.excel
## 4       0        0        0          0         0        0         0
## 6       0        0        0          0         0        0         0
##   D.P.http D.P.mini D.P.air D.P.black D.P.white D.P.gold D.P.spacegray
## 4        0        0       0         0         0        0             0
## 6        0        0       0         0         0        0             0
```

![](ebayipads_color_files/figure-html/cluster.data-7.png) 

```
## [1] "glb_allobs_df$prdline.my$.clusterid Entropy: 0.6732 (98.2710 pct)"
##    prdline.my.clusterid   N   Y  .entropy .knt
## 1             Unknown_1  77  53 0.6760076  130
## 2             Unknown_2  27  23 0.6899438   50
## 3             Unknown_3  14   4 0.5297062   18
## 4              iPad 1_1  62  88 0.6780488  150
## 5              iPad 1_2  13  16 0.6877868   29
## 6              iPad 1_3  13  13 0.6931472   26
## 7              iPad 1_4  12   8 0.6730117   20
## 8              iPad 2_1  69  88 0.6858064  157
## 9              iPad 2_2  46  40 0.6907115   86
## 10             iPad 2_3  21  11 0.6434916   32
## 11             iPad 2_4   5   8 0.6662784   13
## 12            iPad 3+_1  77  97 0.6865267  174
## 13            iPad 3+_2  28  17 0.6629658   45
## 14            iPad 3+_3  21   9 0.6108643   30
## 15            iPad 3+_4  11  12 0.6922017   23
## 16            iPad 3+_5  16   8 0.6365142   24
## 17            iPad 3+_6  13   2 0.3926745   15
## 18            iPadAir_1 142 106 0.6825740  248
## 19            iPadAir_2  38  37 0.6930583   75
## 20            iPadAir_3  23   7 0.5432728   30
## 21        iPadmini 2+_1 100  61 0.6635142  161
## 22        iPadmini 2+_2  16   9 0.6534182   25
## 23        iPadmini 2+_3   9  10 0.6917615   19
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
##                 label step_major step_minor     bgn     end elapsed
## 7 manage.missing.data          4          1 126.074 129.394    3.32
## 8     select.features          5          0 129.395      NA      NA
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
## sold                                               sold  1.000000e+00
## biddable                                       biddable  5.481788e-01
## startprice.log                           startprice.log -4.674275e-01
## startprice                                   startprice -4.569767e-01
## startprice.predict.                 startprice.predict. -3.861675e-01
## startprice.diff                         startprice.diff -3.078989e-01
## UniqueID                                       UniqueID -1.895466e-01
## idseq.my                                       idseq.my -1.895466e-01
## condition.fctr                           condition.fctr -1.535490e-01
## D.npnct05.log                             D.npnct05.log -1.180559e-01
## D.terms.n.post.stop                 D.terms.n.post.stop -8.602418e-02
## D.terms.n.post.stem                 D.terms.n.post.stem -8.575372e-02
## D.npnct14.log                             D.npnct14.log -7.862038e-02
## cellular.fctr                             cellular.fctr -7.432974e-02
## D.terms.n.post.stop.log         D.terms.n.post.stop.log -6.648927e-02
## D.terms.n.post.stem.log         D.terms.n.post.stem.log -6.642142e-02
## D.nwrds.unq.log                         D.nwrds.unq.log -6.642142e-02
## .clusterid                                   .clusterid -6.638659e-02
## .clusterid.fctr                         .clusterid.fctr -6.638659e-02
## D.ndgts.log                                 D.ndgts.log -6.286847e-02
## D.npnct09.log                             D.npnct09.log -6.182533e-02
## carrier.fctr                               carrier.fctr -5.990892e-02
## D.npnct12.log                             D.npnct12.log -5.932565e-02
## D.nwrds.log                                 D.nwrds.log -5.903215e-02
## D.ratio.nstopwrds.nwrds         D.ratio.nstopwrds.nwrds  5.811054e-02
## D.nchrs.log                                 D.nchrs.log -5.653921e-02
## D.nuppr.log                                 D.nuppr.log -5.539161e-02
## D.npnct28.log                             D.npnct28.log -5.245832e-02
## D.npnct06.log                             D.npnct06.log -4.997620e-02
## D.npnct15.log                             D.npnct15.log  4.840228e-02
## D.npnct24.log                             D.npnct24.log -4.584500e-02
## D.npnct16.log                             D.npnct16.log -4.494040e-02
## D.nstopwrds.log                         D.nstopwrds.log -4.468510e-02
## prdline.my.fctr                         prdline.my.fctr -4.158143e-02
## D.npnct08.log                             D.npnct08.log -3.965131e-02
## color.fctr                                   color.fctr -3.913729e-02
## D.T.new                                         D.T.new -3.806364e-02
## D.TfIdf.sum.post.stem             D.TfIdf.sum.post.stem -3.743513e-02
## D.sum.TfIdf                                 D.sum.TfIdf -3.743513e-02
## D.npnct13.log                             D.npnct13.log -3.734631e-02
## D.T.condit                                   D.T.condit -3.689700e-02
## D.TfIdf.sum.post.stop             D.TfIdf.sum.post.stop -3.456157e-02
## D.P.gold                                       D.P.gold -3.044917e-02
## D.T.excel                                     D.T.excel  2.672297e-02
## D.npnct03.log                             D.npnct03.log  2.576379e-02
## D.T.screen                                   D.T.screen  2.523744e-02
## D.npnct07.log                             D.npnct07.log  2.500407e-02
## D.npnct10.log                             D.npnct10.log -2.410150e-02
## D.npnct18.log                             D.npnct18.log -2.152502e-02
## D.terms.n.stem.stop.Ratio     D.terms.n.stem.stop.Ratio  2.058320e-02
## D.npnct11.log                             D.npnct11.log -1.920355e-02
## D.P.white                                     D.P.white  1.848988e-02
## D.T.use                                         D.T.use  1.492486e-02
## D.T.work                                       D.T.work -1.263445e-02
## storage.fctr                               storage.fctr -1.167550e-02
## D.T.ipad                                       D.T.ipad -1.165928e-02
## D.P.mini                                       D.P.mini -1.124183e-02
## D.TfIdf.sum.stem.stop.Ratio D.TfIdf.sum.stem.stop.Ratio -9.352534e-03
## D.P.air                                         D.P.air -9.262995e-03
## D.T.great                                     D.T.great  8.157329e-03
## D.ratio.sum.TfIdf.nwrds         D.ratio.sum.TfIdf.nwrds  7.670839e-03
## D.T.scratch                                 D.T.scratch -7.005676e-03
## .rnorm                                           .rnorm  6.756274e-03
## D.npnct01.log                             D.npnct01.log  4.125530e-03
## D.P.spacegray                             D.P.spacegray  3.481857e-03
## D.P.black                                     D.P.black -1.248546e-03
## D.T.good                                       D.T.good -9.510709e-05
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
## sold                                      1 1.000000e+00
## biddable                                  0 5.481788e-01
## startprice.log                            1 4.674275e-01
## startprice                                1 4.569767e-01
## startprice.predict.                       1 3.861675e-01
## startprice.diff                           0 3.078989e-01
## UniqueID                                  1 1.895466e-01
## idseq.my                                  0 1.895466e-01
## condition.fctr                            0 1.535490e-01
## D.npnct05.log                             0 1.180559e-01
## D.terms.n.post.stop                       0 8.602418e-02
## D.terms.n.post.stem                       0 8.575372e-02
## D.npnct14.log                             0 7.862038e-02
## cellular.fctr                             0 7.432974e-02
## D.terms.n.post.stop.log                   0 6.648927e-02
## D.terms.n.post.stem.log                   0 6.642142e-02
## D.nwrds.unq.log                           0 6.642142e-02
## .clusterid                                1 6.638659e-02
## .clusterid.fctr                           0 6.638659e-02
## D.ndgts.log                               0 6.286847e-02
## D.npnct09.log                             0 6.182533e-02
## carrier.fctr                              0 5.990892e-02
## D.npnct12.log                             0 5.932565e-02
## D.nwrds.log                               0 5.903215e-02
## D.ratio.nstopwrds.nwrds                   0 5.811054e-02
## D.nchrs.log                               0 5.653921e-02
## D.nuppr.log                               0 5.539161e-02
## D.npnct28.log                             0 5.245832e-02
## D.npnct06.log                             0 4.997620e-02
## D.npnct15.log                             0 4.840228e-02
## D.npnct24.log                             0 4.584500e-02
## D.npnct16.log                             0 4.494040e-02
## D.nstopwrds.log                           0 4.468510e-02
## prdline.my.fctr                           0 4.158143e-02
## D.npnct08.log                             0 3.965131e-02
## color.fctr                                0 3.913729e-02
## D.T.new                                   1 3.806364e-02
## D.TfIdf.sum.post.stem                     0 3.743513e-02
## D.sum.TfIdf                               0 3.743513e-02
## D.npnct13.log                             0 3.734631e-02
## D.T.condit                                1 3.689700e-02
## D.TfIdf.sum.post.stop                     0 3.456157e-02
## D.P.gold                                  1 3.044917e-02
## D.T.excel                                 1 2.672297e-02
## D.npnct03.log                             0 2.576379e-02
## D.T.screen                                1 2.523744e-02
## D.npnct07.log                             0 2.500407e-02
## D.npnct10.log                             0 2.410150e-02
## D.npnct18.log                             0 2.152502e-02
## D.terms.n.stem.stop.Ratio                 0 2.058320e-02
## D.npnct11.log                             0 1.920355e-02
## D.P.white                                 1 1.848988e-02
## D.T.use                                   1 1.492486e-02
## D.T.work                                  1 1.263445e-02
## storage.fctr                              0 1.167550e-02
## D.T.ipad                                  1 1.165928e-02
## D.P.mini                                  1 1.124183e-02
## D.TfIdf.sum.stem.stop.Ratio               0 9.352534e-03
## D.P.air                                   1 9.262995e-03
## D.T.great                                 1 8.157329e-03
## D.ratio.sum.TfIdf.nwrds                   0 7.670839e-03
## D.T.scratch                               1 7.005676e-03
## .rnorm                                    0 6.756274e-03
## D.npnct01.log                             0 4.125530e-03
## D.P.spacegray                             1 3.481857e-03
## D.P.black                                 1 1.248546e-03
## D.T.good                                  1 9.510709e-05
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
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0374"
## [1] "cor(sold.fctr, D.sum.TfIdf)=-0.0374"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.sum.TfIdf as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stem.log)=1.0000"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0664"
## [1] "cor(sold.fctr, D.terms.n.post.stem.log)=-0.0664"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stem.log as highly correlated
## with D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stop.log)=0.9999"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0664"
## [1] "cor(sold.fctr, D.terms.n.post.stop.log)=-0.0665"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.unq.log as highly correlated with
## D.terms.n.post.stop.log
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
## [1] "cor(sold.fctr, D.terms.n.post.stem)=-0.0858"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0860"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stem as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.TfIdf.sum.post.stop)=0.9976"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0374"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stop)=-0.0346"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stop as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nchrs.log, D.nwrds.log)=0.9929"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0565"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0590"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nchrs.log as highly correlated with
## D.nwrds.log
```

```
## [1] "cor(D.nwrds.log, D.terms.n.post.stop.log)=0.9921"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0590"
## [1] "cor(sold.fctr, D.terms.n.post.stop.log)=-0.0665"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.log as highly correlated with
## D.terms.n.post.stop.log
```

```
## [1] "cor(D.terms.n.post.stop, D.terms.n.post.stop.log)=0.9755"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0860"
## [1] "cor(sold.fctr, D.terms.n.post.stop.log)=-0.0665"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stop.log as highly correlated
## with D.terms.n.post.stop
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.npnct24.log)=0.9648"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0374"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0458"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stem as highly correlated with
## D.npnct24.log
```

```
## [1] "cor(D.npnct24.log, D.ratio.nstopwrds.nwrds)=-0.9620"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0458"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0581"
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
## [1] "cor(D.nstopwrds.log, D.terms.n.post.stop)=0.8885"
## [1] "cor(sold.fctr, D.nstopwrds.log)=-0.0447"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0860"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nstopwrds.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.ratio.nstopwrds.nwrds, D.terms.n.post.stop)=-0.8670"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0581"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0860"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.ratio.nstopwrds.nwrds as highly correlated
## with D.terms.n.post.stop
```

```
## [1] "cor(D.npnct13.log, D.terms.n.post.stop)=0.7357"
## [1] "cor(sold.fctr, D.npnct13.log)=-0.0373"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0860"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct13.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(carrier.fctr, cellular.fctr)=0.7131"
## [1] "cor(sold.fctr, carrier.fctr)=-0.0599"
## [1] "cor(sold.fctr, cellular.fctr)=-0.0743"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified carrier.fctr as highly correlated with
## cellular.fctr
```

```
##                             id         cor.y exclude.as.feat    cor.y.abs
## 76                        sold  1.000000e+00               1 1.000000e+00
## 69                    biddable  5.481788e-01               0 5.481788e-01
## 60     D.ratio.nstopwrds.nwrds  5.811054e-02               0 5.811054e-02
## 40               D.npnct15.log  4.840228e-02               0 4.840228e-02
## 12                   D.T.excel  2.672297e-02               1 2.672297e-02
## 28               D.npnct03.log  2.576379e-02               0 2.576379e-02
## 18                  D.T.screen  2.523744e-02               1 2.523744e-02
## 32               D.npnct07.log  2.500407e-02               0 2.500407e-02
## 67   D.terms.n.stem.stop.Ratio  2.058320e-02               0 2.058320e-02
## 10                   D.P.white  1.848988e-02               1 1.848988e-02
## 19                     D.T.use  1.492486e-02               1 1.492486e-02
## 14                   D.T.great  8.157329e-03               1 8.157329e-03
## 61     D.ratio.sum.TfIdf.nwrds  7.670839e-03               0 7.670839e-03
## 3                       .rnorm  6.756274e-03               0 6.756274e-03
## 26               D.npnct01.log  4.125530e-03               0 4.125530e-03
## 9                D.P.spacegray  3.481857e-03               1 3.481857e-03
## 13                    D.T.good -9.510709e-05               1 9.510709e-05
## 5                    D.P.black -1.248546e-03               1 1.248546e-03
## 17                 D.T.scratch -7.005676e-03               1 7.005676e-03
## 4                      D.P.air -9.262995e-03               1 9.262995e-03
## 23 D.TfIdf.sum.stem.stop.Ratio -9.352534e-03               0 9.352534e-03
## 8                     D.P.mini -1.124183e-02               1 1.124183e-02
## 15                    D.T.ipad -1.165928e-02               1 1.165928e-02
## 81                storage.fctr -1.167550e-02               0 1.167550e-02
## 20                    D.T.work -1.263445e-02               1 1.263445e-02
## 36               D.npnct11.log -1.920355e-02               0 1.920355e-02
## 43               D.npnct18.log -2.152502e-02               0 2.152502e-02
## 35               D.npnct10.log -2.410150e-02               0 2.410150e-02
## 6                     D.P.gold -3.044917e-02               1 3.044917e-02
## 22       D.TfIdf.sum.post.stop -3.456157e-02               0 3.456157e-02
## 11                  D.T.condit -3.689700e-02               1 3.689700e-02
## 38               D.npnct13.log -3.734631e-02               0 3.734631e-02
## 21       D.TfIdf.sum.post.stem -3.743513e-02               0 3.743513e-02
## 62                 D.sum.TfIdf -3.743513e-02               0 3.743513e-02
## 16                     D.T.new -3.806364e-02               1 3.806364e-02
## 72                  color.fctr -3.913729e-02               0 3.913729e-02
## 33               D.npnct08.log -3.965131e-02               0 3.965131e-02
## 75             prdline.my.fctr -4.158143e-02               0 4.158143e-02
## 56             D.nstopwrds.log -4.468510e-02               0 4.468510e-02
## 41               D.npnct16.log -4.494040e-02               0 4.494040e-02
## 49               D.npnct24.log -4.584500e-02               0 4.584500e-02
## 31               D.npnct06.log -4.997620e-02               0 4.997620e-02
## 53               D.npnct28.log -5.245832e-02               0 5.245832e-02
## 57                 D.nuppr.log -5.539161e-02               0 5.539161e-02
## 24                 D.nchrs.log -5.653921e-02               0 5.653921e-02
## 58                 D.nwrds.log -5.903215e-02               0 5.903215e-02
## 37               D.npnct12.log -5.932565e-02               0 5.932565e-02
## 70                carrier.fctr -5.990892e-02               0 5.990892e-02
## 34               D.npnct09.log -6.182533e-02               0 6.182533e-02
## 25                 D.ndgts.log -6.286847e-02               0 6.286847e-02
## 1                   .clusterid -6.638659e-02               1 6.638659e-02
## 2              .clusterid.fctr -6.638659e-02               0 6.638659e-02
## 59             D.nwrds.unq.log -6.642142e-02               0 6.642142e-02
## 64     D.terms.n.post.stem.log -6.642142e-02               0 6.642142e-02
## 66     D.terms.n.post.stop.log -6.648927e-02               0 6.648927e-02
## 71               cellular.fctr -7.432974e-02               0 7.432974e-02
## 39               D.npnct14.log -7.862038e-02               0 7.862038e-02
## 63         D.terms.n.post.stem -8.575372e-02               0 8.575372e-02
## 65         D.terms.n.post.stop -8.602418e-02               0 8.602418e-02
## 30               D.npnct05.log -1.180559e-01               0 1.180559e-01
## 73              condition.fctr -1.535490e-01               0 1.535490e-01
## 68                    UniqueID -1.895466e-01               1 1.895466e-01
## 74                    idseq.my -1.895466e-01               0 1.895466e-01
## 78             startprice.diff -3.078989e-01               0 3.078989e-01
## 80         startprice.predict. -3.861675e-01               1 3.861675e-01
## 77                  startprice -4.569767e-01               1 4.569767e-01
## 79              startprice.log -4.674275e-01               1 4.674275e-01
## 7                     D.P.http            NA               1           NA
## 27               D.npnct02.log            NA               0           NA
## 29               D.npnct04.log            NA               0           NA
## 42               D.npnct17.log            NA               0           NA
## 44               D.npnct19.log            NA               0           NA
## 45               D.npnct20.log            NA               0           NA
## 46               D.npnct21.log            NA               0           NA
## 47               D.npnct22.log            NA               0           NA
## 48               D.npnct23.log            NA               0           NA
## 50               D.npnct25.log            NA               0           NA
## 51               D.npnct26.log            NA               0           NA
## 52               D.npnct27.log            NA               0           NA
## 54               D.npnct29.log            NA               0           NA
## 55               D.npnct30.log            NA               0           NA
##                 cor.high.X   freqRatio percentUnique zeroVar   nzv
## 76                    <NA>    1.161628    0.10758472   FALSE FALSE
## 69                    <NA>    1.221027    0.10758472   FALSE FALSE
## 60     D.terms.n.post.stop   14.078947    4.41097364   FALSE FALSE
## 40                    <NA>  153.416667    0.16137708   FALSE  TRUE
## 12                    <NA>  149.666667    0.75309306   FALSE  TRUE
## 28                    <NA>   83.227273    0.16137708   FALSE  TRUE
## 18                    <NA>   51.727273    0.80688542   FALSE  TRUE
## 32                    <NA> 1858.000000    0.10758472   FALSE  TRUE
## 67                    <NA>   77.869565    0.48413125   FALSE  TRUE
## 10                    <NA>  231.250000    0.16137708   FALSE  TRUE
## 19                    <NA>   48.617647    0.96826251   FALSE  TRUE
## 14                    <NA>  118.400000    0.80688542   FALSE  TRUE
## 61                    <NA>   63.000000   34.85745024   FALSE FALSE
## 3                     <NA>    1.000000  100.00000000   FALSE FALSE
## 26                    <NA>   52.970588    0.32275417   FALSE  TRUE
## 9                     <NA>  463.750000    0.10758472   FALSE  TRUE
## 13                    <NA>   46.540541    0.86067778   FALSE  TRUE
## 5                     <NA>  168.000000    0.10758472   FALSE  TRUE
## 17                    <NA>   47.314286    0.86067778   FALSE  TRUE
## 4                     <NA>  122.866667    0.16137708   FALSE  TRUE
## 23                    <NA>   65.176471   32.92092523   FALSE FALSE
## 8                     <NA>   91.900000    0.16137708   FALSE  TRUE
## 15                    <NA>   56.466667    0.80688542   FALSE  TRUE
## 81                    <NA>    2.725146    0.26896181   FALSE FALSE
## 20                    <NA>   68.720000    0.69930070   FALSE  TRUE
## 36                    <NA>    9.374269    0.37654653   FALSE FALSE
## 43                    <NA> 1858.000000    0.10758472   FALSE  TRUE
## 35                    <NA>  308.666667    0.16137708   FALSE  TRUE
## 6                     <NA>  928.500000    0.10758472   FALSE  TRUE
## 22   D.TfIdf.sum.post.stem   63.000000   34.37331899   FALSE FALSE
## 11                    <NA>   24.868852    0.91447015   FALSE  TRUE
## 38     D.terms.n.post.stop    5.203065    0.32275417   FALSE FALSE
## 21           D.npnct24.log   63.000000   34.31952663   FALSE FALSE
## 62   D.TfIdf.sum.post.stem   63.000000   34.31952663   FALSE FALSE
## 16                    <NA>  125.071429    0.80688542   FALSE  TRUE
## 72                    <NA>    1.544053    0.26896181   FALSE FALSE
## 33                    <NA>   69.576923    0.21516945   FALSE  TRUE
## 75                    <NA>    1.135048    0.37654653   FALSE FALSE
## 56     D.terms.n.post.stop   13.916667    0.80688542   FALSE FALSE
## 41           D.npnct06.log   31.245614    0.16137708   FALSE  TRUE
## 49 D.ratio.nstopwrds.nwrds    1.356147    0.10758472   FALSE FALSE
## 31                    <NA>   33.735849    0.16137708   FALSE  TRUE
## 53                    <NA>  463.250000    0.16137708   FALSE  TRUE
## 57             D.nchrs.log   18.807018    4.41097364   FALSE FALSE
## 24             D.nwrds.log   15.970149    5.70199032   FALSE FALSE
## 58 D.terms.n.post.stop.log   12.738095    1.29101668   FALSE FALSE
## 37                    <NA>   27.246154    0.21516945   FALSE  TRUE
## 70           cellular.fctr    3.220290    0.37654653   FALSE FALSE
## 34                    <NA>  308.333333    0.21516945   FALSE  TRUE
## 25                    <NA>   27.047619    0.69930070   FALSE  TRUE
## 1                     <NA>    3.536657    0.32275417   FALSE FALSE
## 2                     <NA>    3.536657    0.32275417   FALSE FALSE
## 59 D.terms.n.post.stop.log    8.568000    0.80688542   FALSE FALSE
## 64         D.nwrds.unq.log    8.568000    0.80688542   FALSE FALSE
## 66     D.terms.n.post.stop    8.637097    0.80688542   FALSE FALSE
## 71                    <NA>    2.116190    0.16137708   FALSE FALSE
## 39                    <NA>   35.333333    0.26896181   FALSE  TRUE
## 63     D.terms.n.post.stop    8.568000    0.80688542   FALSE FALSE
## 65                    <NA>    8.637097    0.80688542   FALSE FALSE
## 30                    <NA>   40.311111    0.10758472   FALSE  TRUE
## 73                    <NA>    4.003460    0.32275417   FALSE FALSE
## 68                    <NA>    1.000000  100.00000000   FALSE FALSE
## 74                    <NA>    1.000000  100.00000000   FALSE FALSE
## 78                    <NA>    1.000000  100.00000000   FALSE FALSE
## 80                    <NA>    1.000000  100.00000000   FALSE FALSE
## 77                    <NA>    2.807692   30.17751479   FALSE FALSE
## 79                    <NA>    2.807692   30.17751479   FALSE FALSE
## 7                     <NA>    0.000000    0.05379236    TRUE  TRUE
## 27                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 29                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 42                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 44                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 45                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 46                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 47                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 48                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 50                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 51                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 52                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 54                    <NA>    0.000000    0.05379236    TRUE  TRUE
## 55                    <NA>    0.000000    0.05379236    TRUE  TRUE
##    myNearZV is.cor.y.abs.low
## 76    FALSE            FALSE
## 69    FALSE            FALSE
## 60    FALSE            FALSE
## 40    FALSE            FALSE
## 12    FALSE            FALSE
## 28    FALSE            FALSE
## 18    FALSE            FALSE
## 32     TRUE            FALSE
## 67    FALSE            FALSE
## 10    FALSE            FALSE
## 19    FALSE            FALSE
## 14    FALSE            FALSE
## 61    FALSE            FALSE
## 3     FALSE            FALSE
## 26    FALSE             TRUE
## 9     FALSE             TRUE
## 13    FALSE             TRUE
## 5     FALSE             TRUE
## 17    FALSE            FALSE
## 4     FALSE            FALSE
## 23    FALSE            FALSE
## 8     FALSE            FALSE
## 15    FALSE            FALSE
## 81    FALSE            FALSE
## 20    FALSE            FALSE
## 36    FALSE            FALSE
## 43     TRUE            FALSE
## 35    FALSE            FALSE
## 6      TRUE            FALSE
## 22    FALSE            FALSE
## 11    FALSE            FALSE
## 38    FALSE            FALSE
## 21    FALSE            FALSE
## 62    FALSE            FALSE
## 16    FALSE            FALSE
## 72    FALSE            FALSE
## 33    FALSE            FALSE
## 75    FALSE            FALSE
## 56    FALSE            FALSE
## 41    FALSE            FALSE
## 49    FALSE            FALSE
## 31    FALSE            FALSE
## 53    FALSE            FALSE
## 57    FALSE            FALSE
## 24    FALSE            FALSE
## 58    FALSE            FALSE
## 37    FALSE            FALSE
## 70    FALSE            FALSE
## 34    FALSE            FALSE
## 25    FALSE            FALSE
## 1     FALSE            FALSE
## 2     FALSE            FALSE
## 59    FALSE            FALSE
## 64    FALSE            FALSE
## 66    FALSE            FALSE
## 71    FALSE            FALSE
## 39    FALSE            FALSE
## 63    FALSE            FALSE
## 65    FALSE            FALSE
## 30    FALSE            FALSE
## 73    FALSE            FALSE
## 68    FALSE            FALSE
## 74    FALSE            FALSE
## 78    FALSE            FALSE
## 80    FALSE            FALSE
## 77    FALSE            FALSE
## 79    FALSE            FALSE
## 7      TRUE               NA
## 27     TRUE               NA
## 29     TRUE               NA
## 42     TRUE               NA
## 44     TRUE               NA
## 45     TRUE               NA
## 46     TRUE               NA
## 47     TRUE               NA
## 48     TRUE               NA
## 50     TRUE               NA
## 51     TRUE               NA
## 52     TRUE               NA
## 54     TRUE               NA
## 55     TRUE               NA
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

![](ebayipads_color_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##               id       cor.y exclude.as.feat  cor.y.abs cor.high.X
## 32 D.npnct07.log  0.02500407               0 0.02500407       <NA>
## 43 D.npnct18.log -0.02152502               0 0.02152502       <NA>
## 6       D.P.gold -0.03044917               1 0.03044917       <NA>
## 7       D.P.http          NA               1         NA       <NA>
## 27 D.npnct02.log          NA               0         NA       <NA>
## 29 D.npnct04.log          NA               0         NA       <NA>
## 42 D.npnct17.log          NA               0         NA       <NA>
## 44 D.npnct19.log          NA               0         NA       <NA>
## 45 D.npnct20.log          NA               0         NA       <NA>
## 46 D.npnct21.log          NA               0         NA       <NA>
## 47 D.npnct22.log          NA               0         NA       <NA>
## 48 D.npnct23.log          NA               0         NA       <NA>
## 50 D.npnct25.log          NA               0         NA       <NA>
## 51 D.npnct26.log          NA               0         NA       <NA>
## 52 D.npnct27.log          NA               0         NA       <NA>
## 54 D.npnct29.log          NA               0         NA       <NA>
## 55 D.npnct30.log          NA               0         NA       <NA>
##    freqRatio percentUnique zeroVar  nzv myNearZV is.cor.y.abs.low
## 32    1858.0    0.10758472   FALSE TRUE     TRUE            FALSE
## 43    1858.0    0.10758472   FALSE TRUE     TRUE            FALSE
## 6      928.5    0.10758472   FALSE TRUE     TRUE            FALSE
## 7        0.0    0.05379236    TRUE TRUE     TRUE               NA
## 27       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 29       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 42       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 44       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 45       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 46       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 47       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 48       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 50       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 51       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 52       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 54       0.0    0.05379236    TRUE TRUE     TRUE               NA
## 55       0.0    0.05379236    TRUE TRUE     TRUE               NA
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
##                    1600                    1521                    1521 
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
##                    1520                    1522                    2427 
##           D.npnct01.log           D.npnct03.log           D.npnct05.log 
##                    2579                    2614                    2592 
##           D.npnct06.log           D.npnct08.log           D.npnct09.log 
##                    2554                    2581                    2641 
##           D.npnct10.log           D.npnct11.log           D.npnct12.log 
##                    2648                    2301                    2538 
##           D.npnct13.log           D.npnct14.log           D.npnct15.log 
##                    1932                    2582                    2637 
##           D.npnct16.log           D.npnct24.log           D.npnct28.log 
##                    2546                    1520                    2649 
##         D.nstopwrds.log                D.P.mini                 D.P.air 
##                    1663                    2623                    2636 
##               D.P.black               D.P.white           D.P.spacegray 
##                    2640                    2647                    2650 
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
##                     label step_major step_minor     bgn     end elapsed
## 8         select.features          5          0 129.395 133.412   4.017
## 9 partition.data.training          6          0 133.412      NA      NA
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
## [1] 81 12
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
## 76             sold  1.0000000            TRUE 1.0000000       <NA>
## 68         UniqueID -0.1895466            TRUE 0.1895466       <NA>
## sold.fctr sold.fctr         NA            TRUE        NA       <NA>
##           freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 76         1.161628     0.1075847   FALSE FALSE    FALSE            FALSE
## 68         1.000000   100.0000000   FALSE FALSE    FALSE            FALSE
## sold.fctr        NA            NA      NA    NA       NA               NA
##           interaction.feat rsp_var_raw id_var rsp_var
## 76                    <NA>        TRUE     NA      NA
## 68                    <NA>       FALSE   TRUE      NA
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
##  [1] "D.npnct07.log" "D.npnct18.log" "D.P.gold"      "D.P.http"     
##  [5] "D.npnct02.log" "D.npnct04.log" "D.npnct17.log" "D.npnct19.log"
##  [9] "D.npnct20.log" "D.npnct21.log" "D.npnct22.log" "D.npnct23.log"
## [13] "D.npnct25.log" "D.npnct26.log" "D.npnct27.log" "D.npnct29.log"
## [17] "D.npnct30.log"
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
## [1] 2657   77
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 1859   76
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 974  76
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 885  76
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 798  76
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
##                      label step_major step_minor     bgn     end elapsed
## 9  partition.data.training          6          0 133.412 134.301   0.889
## 10              fit.models          7          0 134.301      NA      NA
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
## 1                      0.438                 0.002         0.5
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

![](ebayipads_color_files/figure-html/fit.models_0-1.png) 

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

![](ebayipads_color_files/figure-html/fit.models_0-2.png) 

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

![](ebayipads_color_files/figure-html/fit.models_0-3.png) 

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

![](ebayipads_color_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.253                 0.001   0.5071756
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

![](ebayipads_color_files/figure-html/fit.models_0-5.png) 

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
## 1               0                       0.61                 0.012
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

![](ebayipads_color_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.086666667      1 0.4888889
## 3 0.002962963      2 0.4022222
## 4 0.002222222      5 0.3933333
## 5 0.001481481     18 0.3622222
## 6 0.001111111     21 0.3577778
## 7 0.000000000     23 0.3555556
## 
## Variable importance
##        biddable startprice.diff 
##              59              41 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable        < 0.5       to the left,  improve=144.14990, (0 missing)
##       startprice.diff < 59.64413  to the right, improve= 48.01938, (0 missing)
##   Surrogate splits:
##       startprice.diff < -57.54745 to the right, agree=0.548, adj=0.022, (0 split)
## 
## Node number 2: 524 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
##   left son=4 (246 obs) right son=5 (278 obs)
##   Primary splits:
##       startprice.diff < 25.55147  to the right, improve=8.564878, (0 missing)
## 
## Node number 3: 450 observations,    complexity param=0.08666667
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (135 obs) right son=7 (315 obs)
##   Primary splits:
##       startprice.diff < 68.91842  to the right, improve=61.71429, (0 missing)
## 
## Node number 4: 246 observations
##   predicted class=N  expected loss=0.1138211  P(node) =0.2525667
##     class counts:   218    28
##    probabilities: 0.886 0.114 
## 
## Node number 5: 278 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.294964  P(node) =0.2854209
##     class counts:   196    82
##    probabilities: 0.705 0.295 
##   left son=10 (25 obs) right son=11 (253 obs)
##   Primary splits:
##       startprice.diff < -108.4389 to the left,  improve=3.571512, (0 missing)
## 
## Node number 6: 135 observations,    complexity param=0.002962963
##   predicted class=N  expected loss=0.3555556  P(node) =0.1386037
##     class counts:    87    48
##    probabilities: 0.644 0.356 
##   left son=12 (81 obs) right son=13 (54 obs)
##   Primary splits:
##       startprice.diff < 113.3966  to the right, improve=2.854321, (0 missing)
## 
## Node number 7: 315 observations
##   predicted class=Y  expected loss=0.07301587  P(node) =0.3234086
##     class counts:    23   292
##    probabilities: 0.073 0.927 
## 
## Node number 10: 25 observations
##   predicted class=N  expected loss=0.04  P(node) =0.02566735
##     class counts:    24     1
##    probabilities: 0.960 0.040 
## 
## Node number 11: 253 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.3201581  P(node) =0.2597536
##     class counts:   172    81
##    probabilities: 0.680 0.320 
##   left son=22 (91 obs) right son=23 (162 obs)
##   Primary splits:
##       startprice.diff < -2.694213 to the right, improve=1.747059, (0 missing)
## 
## Node number 12: 81 observations
##   predicted class=N  expected loss=0.2716049  P(node) =0.08316222
##     class counts:    59    22
##    probabilities: 0.728 0.272 
## 
## Node number 13: 54 observations,    complexity param=0.002962963
##   predicted class=N  expected loss=0.4814815  P(node) =0.05544148
##     class counts:    28    26
##    probabilities: 0.519 0.481 
##   left son=26 (20 obs) right son=27 (34 obs)
##   Primary splits:
##       startprice.diff < 95.05288  to the right, improve=0.4217865, (0 missing)
## 
## Node number 22: 91 observations,    complexity param=0.001111111
##   predicted class=N  expected loss=0.2417582  P(node) =0.09342916
##     class counts:    69    22
##    probabilities: 0.758 0.242 
##   left son=44 (45 obs) right son=45 (46 obs)
##   Primary splits:
##       startprice.diff < 10.66358  to the left,  improve=1.323024, (0 missing)
## 
## Node number 23: 162 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.3641975  P(node) =0.1663244
##     class counts:   103    59
##    probabilities: 0.636 0.364 
##   left son=46 (51 obs) right son=47 (111 obs)
##   Primary splits:
##       startprice.diff < -41.27746 to the left,  improve=0.7311037, (0 missing)
## 
## Node number 26: 20 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.4  P(node) =0.02053388
##     class counts:    12     8
##    probabilities: 0.600 0.400 
##   left son=52 (11 obs) right son=53 (9 obs)
##   Primary splits:
##       startprice.diff < 102.2475  to the left,  improve=0.7919192, (0 missing)
## 
## Node number 27: 34 observations,    complexity param=0.002962963
##   predicted class=Y  expected loss=0.4705882  P(node) =0.0349076
##     class counts:    16    18
##    probabilities: 0.471 0.529 
##   left son=54 (8 obs) right son=55 (26 obs)
##   Primary splits:
##       startprice.diff < 75.75335  to the left,  improve=0.4988688, (0 missing)
## 
## Node number 44: 45 observations
##   predicted class=N  expected loss=0.1555556  P(node) =0.04620123
##     class counts:    38     7
##    probabilities: 0.844 0.156 
## 
## Node number 45: 46 observations,    complexity param=0.001111111
##   predicted class=N  expected loss=0.326087  P(node) =0.04722793
##     class counts:    31    15
##    probabilities: 0.674 0.326 
##   left son=90 (39 obs) right son=91 (7 obs)
##   Primary splits:
##       startprice.diff < 12.77658  to the right, improve=0.9939481, (0 missing)
## 
## Node number 46: 51 observations,    complexity param=0.001481481
##   predicted class=N  expected loss=0.2941176  P(node) =0.0523614
##     class counts:    36    15
##    probabilities: 0.706 0.294 
##   left son=92 (7 obs) right son=93 (44 obs)
##   Primary splits:
##       startprice.diff < -45.97369 to the right, improve=1.403743, (0 missing)
## 
## Node number 47: 111 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.3963964  P(node) =0.113963
##     class counts:    67    44
##    probabilities: 0.604 0.396 
##   left son=94 (71 obs) right son=95 (40 obs)
##   Primary splits:
##       startprice.diff < -28.12784 to the right, improve=2.068526, (0 missing)
## 
## Node number 52: 11 observations
##   predicted class=N  expected loss=0.2727273  P(node) =0.01129363
##     class counts:     8     3
##    probabilities: 0.727 0.273 
## 
## Node number 53: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.009240246
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## Node number 54: 8 observations
##   predicted class=N  expected loss=0.375  P(node) =0.008213552
##     class counts:     5     3
##    probabilities: 0.625 0.375 
## 
## Node number 55: 26 observations
##   predicted class=Y  expected loss=0.4230769  P(node) =0.02669405
##     class counts:    11    15
##    probabilities: 0.423 0.577 
## 
## Node number 90: 39 observations
##   predicted class=N  expected loss=0.2820513  P(node) =0.04004107
##     class counts:    28    11
##    probabilities: 0.718 0.282 
## 
## Node number 91: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.007186858
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 92: 7 observations
##   predicted class=N  expected loss=0  P(node) =0.007186858
##     class counts:     7     0
##    probabilities: 1.000 0.000 
## 
## Node number 93: 44 observations,    complexity param=0.001481481
##   predicted class=N  expected loss=0.3409091  P(node) =0.04517454
##     class counts:    29    15
##    probabilities: 0.659 0.341 
##   left son=186 (20 obs) right son=187 (24 obs)
##   Primary splits:
##       startprice.diff < -61.79786 to the right, improve=0.6060606, (0 missing)
## 
## Node number 94: 71 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.3239437  P(node) =0.07289528
##     class counts:    48    23
##    probabilities: 0.676 0.324 
##   left son=188 (10 obs) right son=189 (61 obs)
##   Primary splits:
##       startprice.diff < -25.00492 to the left,  improve=2.442854, (0 missing)
## 
## Node number 95: 40 observations,    complexity param=0.002222222
##   predicted class=Y  expected loss=0.475  P(node) =0.04106776
##     class counts:    19    21
##    probabilities: 0.475 0.525 
##   left son=190 (30 obs) right son=191 (10 obs)
##   Primary splits:
##       startprice.diff < -31.52075 to the left,  improve=0.8166667, (0 missing)
## 
## Node number 186: 20 observations
##   predicted class=N  expected loss=0.25  P(node) =0.02053388
##     class counts:    15     5
##    probabilities: 0.750 0.250 
## 
## Node number 187: 24 observations,    complexity param=0.001481481
##   predicted class=N  expected loss=0.4166667  P(node) =0.02464066
##     class counts:    14    10
##    probabilities: 0.583 0.417 
##   left son=374 (16 obs) right son=375 (8 obs)
##   Primary splits:
##       startprice.diff < -72.02612 to the left,  improve=1.041667, (0 missing)
## 
## Node number 188: 10 observations
##   predicted class=N  expected loss=0  P(node) =0.01026694
##     class counts:    10     0
##    probabilities: 1.000 0.000 
## 
## Node number 189: 61 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.3770492  P(node) =0.06262834
##     class counts:    38    23
##    probabilities: 0.623 0.377 
##   left son=378 (54 obs) right son=379 (7 obs)
##   Primary splits:
##       startprice.diff < -21.36215 to the right, improve=0.5975366, (0 missing)
## 
## Node number 190: 30 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.4666667  P(node) =0.03080082
##     class counts:    16    14
##    probabilities: 0.533 0.467 
##   left son=380 (7 obs) right son=381 (23 obs)
##   Primary splits:
##       startprice.diff < -34.25742 to the right, improve=0.5979296, (0 missing)
## 
## Node number 191: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.01026694
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 374: 16 observations
##   predicted class=N  expected loss=0.3125  P(node) =0.0164271
##     class counts:    11     5
##    probabilities: 0.688 0.312 
## 
## Node number 375: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.008213552
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 378: 54 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.3518519  P(node) =0.05544148
##     class counts:    35    19
##    probabilities: 0.648 0.352 
##   left son=756 (17 obs) right son=757 (37 obs)
##   Primary splits:
##       startprice.diff < -13.74554 to the left,  improve=1.526291, (0 missing)
## 
## Node number 379: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.007186858
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 380: 7 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.007186858
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 381: 23 observations,    complexity param=0.002222222
##   predicted class=Y  expected loss=0.4782609  P(node) =0.02361396
##     class counts:    11    12
##    probabilities: 0.478 0.522 
##   left son=762 (15 obs) right son=763 (8 obs)
##   Primary splits:
##       startprice.diff < -36.76706 to the left,  improve=1.278261, (0 missing)
## 
## Node number 756: 17 observations
##   predicted class=N  expected loss=0.1764706  P(node) =0.0174538
##     class counts:    14     3
##    probabilities: 0.824 0.176 
## 
## Node number 757: 37 observations,    complexity param=0.002222222
##   predicted class=N  expected loss=0.4324324  P(node) =0.03798768
##     class counts:    21    16
##    probabilities: 0.568 0.432 
##   left son=1514 (29 obs) right son=1515 (8 obs)
##   Primary splits:
##       startprice.diff < -11.7343  to the right, improve=2.058714, (0 missing)
## 
## Node number 762: 15 observations
##   predicted class=N  expected loss=0.4  P(node) =0.01540041
##     class counts:     9     6
##    probabilities: 0.600 0.400 
## 
## Node number 763: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.008213552
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 1514: 29 observations
##   predicted class=N  expected loss=0.3448276  P(node) =0.02977413
##     class counts:    19    10
##    probabilities: 0.655 0.345 
## 
## Node number 1515: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.008213552
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##    1) root 974 450 N (0.53798768 0.46201232)  
##      2) biddable< 0.5 524 110 N (0.79007634 0.20992366)  
##        4) startprice.diff>=25.55147 246  28 N (0.88617886 0.11382114) *
##        5) startprice.diff< 25.55147 278  82 N (0.70503597 0.29496403)  
##         10) startprice.diff< -108.4389 25   1 N (0.96000000 0.04000000) *
##         11) startprice.diff>=-108.4389 253  81 N (0.67984190 0.32015810)  
##           22) startprice.diff>=-2.694213 91  22 N (0.75824176 0.24175824)  
##             44) startprice.diff< 10.66358 45   7 N (0.84444444 0.15555556) *
##             45) startprice.diff>=10.66358 46  15 N (0.67391304 0.32608696)  
##               90) startprice.diff>=12.77658 39  11 N (0.71794872 0.28205128) *
##               91) startprice.diff< 12.77658 7   3 Y (0.42857143 0.57142857) *
##           23) startprice.diff< -2.694213 162  59 N (0.63580247 0.36419753)  
##             46) startprice.diff< -41.27746 51  15 N (0.70588235 0.29411765)  
##               92) startprice.diff>=-45.97369 7   0 N (1.00000000 0.00000000) *
##               93) startprice.diff< -45.97369 44  15 N (0.65909091 0.34090909)  
##                186) startprice.diff>=-61.79786 20   5 N (0.75000000 0.25000000) *
##                187) startprice.diff< -61.79786 24  10 N (0.58333333 0.41666667)  
##                  374) startprice.diff< -72.02612 16   5 N (0.68750000 0.31250000) *
##                  375) startprice.diff>=-72.02612 8   3 Y (0.37500000 0.62500000) *
##             47) startprice.diff>=-41.27746 111  44 N (0.60360360 0.39639640)  
##               94) startprice.diff>=-28.12784 71  23 N (0.67605634 0.32394366)  
##                188) startprice.diff< -25.00492 10   0 N (1.00000000 0.00000000) *
##                189) startprice.diff>=-25.00492 61  23 N (0.62295082 0.37704918)  
##                  378) startprice.diff>=-21.36215 54  19 N (0.64814815 0.35185185)  
##                    756) startprice.diff< -13.74554 17   3 N (0.82352941 0.17647059) *
##                    757) startprice.diff>=-13.74554 37  16 N (0.56756757 0.43243243)  
##                     1514) startprice.diff>=-11.7343 29  10 N (0.65517241 0.34482759) *
##                     1515) startprice.diff< -11.7343 8   2 Y (0.25000000 0.75000000) *
##                  379) startprice.diff< -21.36215 7   3 Y (0.42857143 0.57142857) *
##               95) startprice.diff< -28.12784 40  19 Y (0.47500000 0.52500000)  
##                190) startprice.diff< -31.52075 30  14 N (0.53333333 0.46666667)  
##                  380) startprice.diff>=-34.25742 7   2 N (0.71428571 0.28571429) *
##                  381) startprice.diff< -34.25742 23  11 Y (0.47826087 0.52173913)  
##                    762) startprice.diff< -36.76706 15   6 N (0.60000000 0.40000000) *
##                    763) startprice.diff>=-36.76706 8   2 Y (0.25000000 0.75000000) *
##                191) startprice.diff>=-31.52075 10   3 Y (0.30000000 0.70000000) *
##      3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##        6) startprice.diff>=68.91842 135  48 N (0.64444444 0.35555556)  
##         12) startprice.diff>=113.3966 81  22 N (0.72839506 0.27160494) *
##         13) startprice.diff< 113.3966 54  26 N (0.51851852 0.48148148)  
##           26) startprice.diff>=95.05288 20   8 N (0.60000000 0.40000000)  
##             52) startprice.diff< 102.2475 11   3 N (0.72727273 0.27272727) *
##             53) startprice.diff>=102.2475 9   4 Y (0.44444444 0.55555556) *
##           27) startprice.diff< 95.05288 34  16 Y (0.47058824 0.52941176)  
##             54) startprice.diff< 75.75335 8   3 N (0.62500000 0.37500000) *
##             55) startprice.diff>=75.75335 26  11 Y (0.42307692 0.57692308) *
##        7) startprice.diff< 68.91842 315  23 Y (0.07301587 0.92698413) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6497829
## 3        0.2 0.7653631
## 4        0.3 0.8034934
## 5        0.4 0.8111240
## 6        0.5 0.8113208
## 7        0.6 0.7909887
## 8        0.7 0.7784891
## 9        0.8 0.7633987
## 10       0.9 0.7633987
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           470
## 2         Y                                           106
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            54
## 2                                           344
##          Prediction
## Reference   N   Y
##         N 470  54
##         Y 106 344
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.357290e-01   6.668320e-01   8.109392e-01   8.584688e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   6.197689e-86   5.532678e-05 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6254019
## 3        0.2 0.7198321
## 4        0.3 0.7467301
## 5        0.4 0.7471410
## 6        0.5 0.7551546
## 7        0.6 0.7349727
## 8        0.7 0.7357955
## 9        0.8 0.7420290
## 10       0.9 0.7420290
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           402
## 2         Y                                           117
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            73
## 2                                           293
##          Prediction
## Reference   N   Y
##         N 402  73
##         Y 117 293
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.853107e-01   5.650993e-01   7.567658e-01   8.119416e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.847896e-53   1.811288e-03 
##                    model_id model_method                     feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart biddable, startprice.diff
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.488                 0.009
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8855386                    0.5       0.8113208         0.835729
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8109392             0.8584688      0.666832   0.8187625
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7551546        0.7853107
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7567658             0.8119416     0.5650993
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
## Fitting cp = 0.00296 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](ebayipads_color_files/figure-html/fit.models_0-11.png) ![](ebayipads_color_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.086666667      1 0.4888889
## 3 0.002962963      2 0.4022222
## 
## Variable importance
##        biddable startprice.diff 
##              69              31 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable        < 0.5       to the left,  improve=144.14990, (0 missing)
##       startprice.diff < 59.64413  to the right, improve= 48.01938, (0 missing)
##   Surrogate splits:
##       startprice.diff < -57.54745 to the right, agree=0.548, adj=0.022, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.08666667
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (135 obs) right son=7 (315 obs)
##   Primary splits:
##       startprice.diff < 68.91842  to the right, improve=61.71429, (0 missing)
## 
## Node number 6: 135 observations
##   predicted class=N  expected loss=0.3555556  P(node) =0.1386037
##     class counts:    87    48
##    probabilities: 0.644 0.356 
## 
## Node number 7: 315 observations
##   predicted class=Y  expected loss=0.07301587  P(node) =0.3234086
##     class counts:    23   292
##    probabilities: 0.073 0.927 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.53798768 0.46201232)  
##   2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##   3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##     6) startprice.diff>=68.91842 135  48 N (0.64444444 0.35555556) *
##     7) startprice.diff< 68.91842 315  23 Y (0.07301587 0.92698413) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_0-13.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7555556
## 5        0.4 0.7633987
## 6        0.5 0.7633987
## 7        0.6 0.7633987
## 8        0.7 0.7633987
## 9        0.8 0.7633987
## 10       0.9 0.7633987
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-14.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 501
## 2         Y                                 158
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  23
## 2                                 292
##          Prediction
## Reference   N   Y
##         N 501  23
##         Y 158 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.141684e-01   6.180889e-01   7.882906e-01   8.381305e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.530340e-73   2.277382e-23 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_0-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7528231
## 5        0.4 0.7420290
## 6        0.5 0.7420290
## 7        0.6 0.7420290
## 8        0.7 0.7420290
## 9        0.8 0.7420290
## 10       0.9 0.7420290
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-16.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 388
## 2         Y                                 110
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  87
## 2                                 300
##          Prediction
## Reference   N   Y
##         N 388  87
##         Y 110 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.774011e-01   5.506630e-01   7.485294e-01   8.044124e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.956102e-50   1.170130e-01 
##          model_id model_method                     feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart biddable, startprice.diff               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.072                 0.012   0.8243427
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.9       0.7633987        0.7833523
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7882906             0.8381305     0.5585199   0.8129705
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7528231        0.7774011
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7485294             0.8044124      0.550663
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01779075      0.03354379
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

![](ebayipads_color_files/figure-html/fit.models_0-17.png) ![](ebayipads_color_files/figure-html/fit.models_0-18.png) ![](ebayipads_color_files/figure-html/fit.models_0-19.png) ![](ebayipads_color_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4575  -0.7149  -0.3051   0.6402   2.7683  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     -1.1939636  0.1132804 -10.540   <2e-16 ***
## biddable         2.8629786  0.1784163  16.047   <2e-16 ***
## startprice.diff -0.0095589  0.0009883  -9.672   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  913.48  on 971  degrees of freedom
## AIC: 919.48
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_0-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6697178
## 3        0.2 0.7222222
## 4        0.3 0.7387755
## 5        0.4 0.7444934
## 6        0.5 0.7514188
## 7        0.6 0.7639383
## 8        0.7 0.7639594
## 9        0.8 0.6637807
## 10       0.9 0.2373541
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-22.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               487
## 2         Y                               149
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                37
## 2                               301
##          Prediction
## Reference   N   Y
##         N 487  37
##         Y 149 301
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.090349e-01   6.089806e-01   7.829169e-01   8.332690e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.451542e-70   3.988362e-16 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_0-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6705882
## 3        0.2 0.7213740
## 4        0.3 0.7474519
## 5        0.4 0.7601476
## 6        0.5 0.7668790
## 7        0.6 0.7710526
## 8        0.7 0.7580420
## 9        0.8 0.6864275
## 10       0.9 0.2547771
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-24.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               418
## 2         Y                               117
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                57
## 2                               293
##          Prediction
## Reference   N   Y
##         N 418  57
##         Y 117 293
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.033898e-01   6.006483e-01   7.756483e-01   8.290941e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   7.775424e-62   7.720976e-06 
##        model_id model_method                     feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm biddable, startprice.diff               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.004                 0.011   0.8586302
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.7       0.7639594        0.7720798
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7829169              0.833269     0.5402037   0.8633582
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7710526        0.8033898
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7756483             0.8290941     0.6006483    919.4841
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005815317       0.0112495
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
## [1] "    indep_vars: biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.npnct24.log, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:D.terms.n.post.stop.log, biddable:cellular.fctr, biddable:D.nwrds.unq.log"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_color_files/figure-html/fit.models_0-25.png) ![](ebayipads_color_files/figure-html/fit.models_0-26.png) ![](ebayipads_color_files/figure-html/fit.models_0-27.png) ![](ebayipads_color_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3815  -0.7141  -0.3046   0.6147   2.7639  
## 
## Coefficients:
##                                      Estimate Std. Error z value Pr(>|z|)
## (Intercept)                        -1.1938518  0.1132307 -10.544   <2e-16
## biddable                           -0.1443348  3.7603826  -0.038   0.9694
## startprice.diff                    -0.0095134  0.0009984  -9.528   <2e-16
## `biddable:D.terms.n.post.stop`     -0.2126668  0.3174709  -0.670   0.5029
## `biddable:D.TfIdf.sum.post.stem`    0.1714728  0.2046400   0.838   0.4021
## `biddable:D.npnct24.log`           -3.0403645  3.9688018  -0.766   0.4436
## `biddable:D.npnct06.log`            0.2116318  0.9764780   0.217   0.8284
## `biddable:D.ratio.nstopwrds.nwrds`  3.1336058  3.7549448   0.835   0.4040
## `biddable:D.nchrs.log`              1.6977216  1.6433973   1.033   0.3016
## `biddable:D.nwrds.log`             -3.6103019  2.6293833  -1.373   0.1697
## `biddable:D.terms.n.post.stop.log`  9.4879364  8.5423492   1.111   0.2667
## `biddable:cellular.fctr1`          -0.0077050  0.2971903  -0.026   0.9793
## `biddable:cellular.fctrUnknown`    -0.8918723  0.3598666  -2.478   0.0132
## `biddable:D.nwrds.unq.log`         -6.3624298  7.2654801  -0.876   0.3812
##                                       
## (Intercept)                        ***
## biddable                              
## startprice.diff                    ***
## `biddable:D.terms.n.post.stop`        
## `biddable:D.TfIdf.sum.post.stem`      
## `biddable:D.npnct24.log`              
## `biddable:D.npnct06.log`              
## `biddable:D.ratio.nstopwrds.nwrds`    
## `biddable:D.nchrs.log`                
## `biddable:D.nwrds.log`                
## `biddable:D.terms.n.post.stop.log`    
## `biddable:cellular.fctr1`             
## `biddable:cellular.fctrUnknown`    *  
## `biddable:D.nwrds.unq.log`            
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  903.82  on 960  degrees of freedom
## AIC: 931.82
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6692073
## 3        0.2 0.7211121
## 4        0.3 0.7379239
## 5        0.4 0.7439294
## 6        0.5 0.7542857
## 7        0.6 0.7592814
## 8        0.7 0.7615385
## 9        0.8 0.6827881
## 10       0.9 0.3097015
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-30.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         491
## 2         Y                                         153
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          33
## 2                                         297
##          Prediction
## Reference   N   Y
##         N 491  33
##         Y 153 297
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.090349e-01   6.084803e-01   7.829169e-01   8.332690e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.451542e-70   2.649993e-18 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6717045
## 3        0.2 0.7206864
## 4        0.3 0.7491487
## 5        0.4 0.7570900
## 6        0.5 0.7634961
## 7        0.6 0.7665782
## 8        0.7 0.7566064
## 9        0.8 0.6883721
## 10       0.9 0.3108384
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-32.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         420
## 2         Y                                         121
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          55
## 2                                         289
##          Prediction
## Reference   N   Y
##         N 420  55
##         Y 121 289
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.011299e-01   5.956491e-01   7.732835e-01   8.269546e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   9.548473e-61   9.605183e-07 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.npnct24.log, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:D.terms.n.post.stop.log, biddable:cellular.fctr, biddable:D.nwrds.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.047                 0.019
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8580831                    0.7       0.7615385        0.7741374
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7829169              0.833269     0.5436041   0.8615815
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7665782        0.8011299
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7732835             0.8269546     0.5956491    931.8178
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008498508      0.01820715
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
## [1] "    indep_vars: biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, color.fctr, D.npnct08.log, prdline.my.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_color_files/figure-html/fit.models_0-33.png) ![](ebayipads_color_files/figure-html/fit.models_0-34.png) ![](ebayipads_color_files/figure-html/fit.models_0-35.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4418  -0.6549  -0.1206   0.5712   2.7843  
## 
## Coefficients: (14 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                    6.341e+00  5.579e+00
## biddable                                       3.167e+00  2.290e-01
## D.npnct15.log                                  1.820e+00  8.444e-01
## D.npnct03.log                                  8.152e-01  1.618e+00
## D.terms.n.stem.stop.Ratio                     -6.327e+00  4.970e+00
## D.ratio.sum.TfIdf.nwrds                        1.417e-01  1.530e-01
## .rnorm                                         5.538e-02  9.453e-02
## D.npnct01.log                                  4.225e-01  5.459e-01
## D.TfIdf.sum.stem.stop.Ratio                   -1.242e+00  2.937e+00
## storage.fctr16                                -4.534e-01  5.152e-01
## storage.fctr32                                -6.375e-01  5.447e-01
## storage.fctr64                                -4.850e-02  5.365e-01
## storage.fctrUnknown                           -8.709e-01  7.060e-01
## D.npnct11.log                                  1.296e-01  3.429e-01
## D.npnct10.log                                 -2.338e+01  1.317e+03
## color.fctrGold                                 2.765e-02  5.692e-01
## `color.fctrSpace Gray`                        -4.946e-01  3.704e-01
## color.fctrUnknown                             -2.570e-02  2.526e-01
## color.fctrWhite                               -3.653e-01  2.752e-01
## D.npnct08.log                                  4.442e-01  6.743e-01
## `prdline.my.fctriPad 1`                        9.490e-01  5.605e-01
## `prdline.my.fctriPad 2`                        4.461e-01  5.666e-01
## `prdline.my.fctriPad 3+`                       8.958e-01  5.453e-01
## prdline.my.fctriPadAir                         1.222e+00  5.453e-01
## prdline.my.fctriPadmini                        6.066e-01  5.270e-01
## `prdline.my.fctriPadmini 2+`                   9.005e-01  5.764e-01
## D.npnct06.log                                 -2.440e+00  1.169e+00
## D.npnct28.log                                 -3.104e+00  1.723e+03
## D.npnct12.log                                  1.543e-01  6.726e-01
## D.npnct09.log                                 -8.833e+00  7.853e+02
## D.ndgts.log                                    7.185e-01  4.302e-01
## cellular.fctr1                                 1.579e-01  2.217e-01
## cellular.fctrUnknown                          -3.665e-01  4.801e-01
## D.npnct14.log                                 -2.104e+00  1.071e+00
## D.terms.n.post.stop                           -8.192e-02  4.721e-02
## D.npnct05.log                                 -3.242e+00  1.650e+00
## `condition.fctrFor parts or not working`       1.365e-01  3.602e-01
## `condition.fctrManufacturer refurbished`       6.356e-01  6.277e-01
## condition.fctrNew                             -2.826e-01  3.140e-01
## `condition.fctrNew other (see details)`        6.201e-02  4.745e-01
## `condition.fctrSeller refurbished`            -3.296e-01  4.284e-01
## idseq.my                                      -1.667e-04  2.054e-04
## startprice.diff                               -1.151e-02  1.298e-03
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.958e+00  7.051e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       9.845e-01  7.872e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       8.304e-01  6.627e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -3.736e-02  6.866e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -2.976e-01  6.431e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.347e+00  7.246e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2` -4.480e-01  9.284e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -1.029e-01  9.268e-01
## `prdline.my.fctriPad 1:.clusterid.fctr3`       2.594e-01  8.937e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -6.578e-01  1.262e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -1.699e-01  8.631e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -5.099e-01  8.164e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     1.144e+00  8.423e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  3.291e-01  9.275e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.464e+00  8.445e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       3.576e+00  1.430e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      8.138e-01  8.477e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -2.246e-01  8.900e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`     -5.189e-01  8.338e-01
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -6.973e-01  1.234e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`              NA         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`     -1.430e+01  9.056e+02
## `prdline.my.fctriPadAir:.clusterid.fctr6`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`            NA         NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.137  0.25568    
## biddable                                       13.827  < 2e-16 ***
## D.npnct15.log                                   2.155  0.03118 *  
## D.npnct03.log                                   0.504  0.61445    
## D.terms.n.stem.stop.Ratio                      -1.273  0.20301    
## D.ratio.sum.TfIdf.nwrds                         0.926  0.35452    
## .rnorm                                          0.586  0.55799    
## D.npnct01.log                                   0.774  0.43901    
## D.TfIdf.sum.stem.stop.Ratio                    -0.423  0.67241    
## storage.fctr16                                 -0.880  0.37885    
## storage.fctr32                                 -1.170  0.24187    
## storage.fctr64                                 -0.090  0.92797    
## storage.fctrUnknown                            -1.234  0.21733    
## D.npnct11.log                                   0.378  0.70534    
## D.npnct10.log                                  -0.018  0.98584    
## color.fctrGold                                  0.049  0.96126    
## `color.fctrSpace Gray`                         -1.335  0.18178    
## color.fctrUnknown                              -0.102  0.91896    
## color.fctrWhite                                -1.327  0.18445    
## D.npnct08.log                                   0.659  0.51004    
## `prdline.my.fctriPad 1`                         1.693  0.09041 .  
## `prdline.my.fctriPad 2`                         0.787  0.43112    
## `prdline.my.fctriPad 3+`                        1.643  0.10040    
## prdline.my.fctriPadAir                          2.241  0.02504 *  
## prdline.my.fctriPadmini                         1.151  0.24978    
## `prdline.my.fctriPadmini 2+`                    1.562  0.11821    
## D.npnct06.log                                  -2.088  0.03684 *  
## D.npnct28.log                                  -0.002  0.99856    
## D.npnct12.log                                   0.229  0.81852    
## D.npnct09.log                                  -0.011  0.99103    
## D.ndgts.log                                     1.670  0.09491 .  
## cellular.fctr1                                  0.712  0.47626    
## cellular.fctrUnknown                           -0.763  0.44525    
## D.npnct14.log                                  -1.964  0.04952 *  
## D.terms.n.post.stop                            -1.735  0.08268 .  
## D.npnct05.log                                  -1.964  0.04952 *  
## `condition.fctrFor parts or not working`        0.379  0.70475    
## `condition.fctrManufacturer refurbished`        1.013  0.31126    
## condition.fctrNew                              -0.900  0.36812    
## `condition.fctrNew other (see details)`         0.131  0.89602    
## `condition.fctrSeller refurbished`             -0.769  0.44167    
## idseq.my                                       -0.812  0.41707    
## startprice.diff                                -8.869  < 2e-16 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.777  0.00549 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.251  0.21111    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        1.253  0.21020    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -0.054  0.95661    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.463  0.64349    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.859  0.06307 .  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  -0.483  0.62944    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.111  0.91160    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.290  0.77165    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.521  0.60232    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.197  0.84398    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.625  0.53228    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.359  0.17426    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.355  0.72267    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.734  0.08299 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        2.501  0.01240 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.960  0.33707    
## `prdline.my.fctriPadAir:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.252  0.80077    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      -0.622  0.53371    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.565  0.57217    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr6`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr6`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr6`           NA       NA    
## `prdline.my.fctriPad 3+:.clusterid.fctr6`      -0.016  0.98740    
## `prdline.my.fctriPadAir:.clusterid.fctr6`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr6`         NA       NA    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  810.35  on 910  degrees of freedom
## AIC: 938.35
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_0-36.png) ![](ebayipads_color_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7017828
## 3        0.2 0.7534122
## 4        0.3 0.7613412
## 5        0.4 0.7781350
## 6        0.5 0.7977143
## 7        0.6 0.7951807
## 8        0.7 0.7827192
## 9        0.8 0.7019499
## 10       0.9 0.4808013
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               448
## 2         Y                               101
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                76
## 2                               349
##          Prediction
## Reference   N   Y
##         N 448  76
##         Y 101 349
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.182752e-01   6.330005e-01   7.925945e-01   8.420145e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.666328e-75   7.123907e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_0-38.png) ![](ebayipads_color_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6948695
## 3        0.2 0.7241379
## 4        0.3 0.7451869
## 5        0.4 0.7615572
## 6        0.5 0.7733675
## 7        0.6 0.7680000
## 8        0.7 0.7493036
## 9        0.8 0.6687023
## 10       0.9 0.4810127
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               406
## 2         Y                               108
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                69
## 2                               302
##          Prediction
## Reference   N   Y
##         N 406  69
##         Y 108 302
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.000000e-01   5.951959e-01   7.721016e-01   8.258843e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   3.310805e-60   4.286708e-03 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                           feats
## 1 biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, color.fctr, D.npnct08.log, prdline.my.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       1.49                 0.182
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8858185                    0.5       0.7977143         0.771032
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7925945             0.8420145     0.5386776   0.8465571
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7733675              0.8
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7721016             0.8258843     0.5951959    938.3505
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01583674      0.03271193
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          7          0 134.301 161.263  26.962
## 11 fit.models          7          1 161.264      NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor     bgn end elapsed
## 1 fit.models_1_bgn          1          0 165.679  NA      NA
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
        # !_sp
        interact_vars_vctr <- c(
            "idseq.my", "D.ratio.sum.TfIdf.nwrds", "D.TfIdf.sum.stem.stop.Ratio",
            "D.npnct15.log", "D.npnct03.log")
        ###
        # _sp only
#         interact_vars_vctr <- c(
#             "D.nchrs.log", "D.TfIdf.sum.stem.stop.Ratio", "D.npnct16.log", "D.npnct08.log",
#                                 "biddable", "condition.fctr", 
#                                 # "cellular.fctr", "carrier.fctr",
#                                 "color.fctr", "storage.fctr", "idseq.my")
        ###
        indep_vars_vctr <- union(setdiff(indep_vars_vctr, interact_vars_vctr),
                                paste(glb_category_var, interact_vars_vctr, sep=".fctr*"))
        indep_vars_vctr <- union(setdiff(indep_vars_vctr, 
                        c("startprice.diff", "biddable", "cellular.fctr", "carrier.fctr")),
                            c("startprice.diff*biddable", "cellular.fctr*carrier.fctr"))
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
    #           mydsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
    #           mydsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
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
## 1 fit.models_1_bgn          1          0 165.679 165.688   0.009
## 2 fit.models_1_glm          2          0 165.688      NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](ebayipads_color_files/figure-html/fit.models_1-1.png) ![](ebayipads_color_files/figure-html/fit.models_1-2.png) ![](ebayipads_color_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.63237  -0.62729  -0.09729   0.53513   2.86973  
## 
## Coefficients: (17 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                   -3.417e+03  2.382e+03
## biddable                                       3.285e+00  2.399e-01
## D.ratio.nstopwrds.nwrds                       -1.522e+01  7.020e+00
## D.npnct15.log                                  1.485e+00  9.508e-01
## D.npnct03.log                                  1.329e+00  1.950e+00
## D.terms.n.stem.stop.Ratio                      3.435e+03  2.383e+03
## D.ratio.sum.TfIdf.nwrds                        8.880e-03  6.058e-01
## .rnorm                                         4.776e-02  9.762e-02
## D.npnct01.log                                  3.970e-01  6.511e-01
## D.TfIdf.sum.stem.stop.Ratio                   -3.781e+00  1.791e+01
## storage.fctr16                                -5.194e-01  5.209e-01
## storage.fctr32                                -6.846e-01  5.508e-01
## storage.fctr64                                -4.977e-02  5.412e-01
## storage.fctrUnknown                           -1.002e+00  7.154e-01
## D.npnct11.log                                  2.059e-01  3.802e-01
## D.npnct10.log                                 -2.486e+01  1.214e+03
## D.TfIdf.sum.post.stop                         -3.669e-01  2.671e+00
## D.npnct13.log                                  1.931e-01  4.037e-01
## D.TfIdf.sum.post.stem                          6.515e-01  2.783e+00
## D.sum.TfIdf                                           NA         NA
## color.fctrGold                                 6.483e-02  5.831e-01
## `color.fctrSpace Gray`                        -4.747e-01  3.835e-01
## color.fctrUnknown                             -6.167e-02  2.623e-01
## color.fctrWhite                               -3.142e-01  2.843e-01
## D.npnct08.log                                  6.921e-01  7.478e-01
## `prdline.my.fctriPad 1`                        1.017e+00  5.746e-01
## `prdline.my.fctriPad 2`                        3.617e-01  5.799e-01
## `prdline.my.fctriPad 3+`                       8.072e-01  5.568e-01
## prdline.my.fctriPadAir                         1.173e+00  5.584e-01
## prdline.my.fctriPadmini                        5.929e-01  5.430e-01
## `prdline.my.fctriPadmini 2+`                   8.992e-01  5.880e-01
## D.nstopwrds.log                                3.840e+00  2.000e+00
## D.npnct16.log                                  2.863e+00  1.855e+00
## D.npnct24.log                                 -5.010e+00  5.873e+00
## D.npnct06.log                                 -5.705e+00  2.272e+00
## D.npnct28.log                                 -4.265e+00  1.736e+03
## D.nuppr.log                                    2.134e+00  3.144e+00
## D.nchrs.log                                   -2.806e+00  3.687e+00
## D.nwrds.log                                   -2.461e-01  3.126e+00
## D.npnct12.log                                  6.252e-01  7.751e-01
## carrier.fctrNone                               2.240e-01  7.249e-01
## carrier.fctrOther                              1.395e+01  1.624e+03
## carrier.fctrSprint                             1.253e+00  8.097e-01
## `carrier.fctrT-Mobile`                        -7.507e-01  1.035e+00
## carrier.fctrUnknown                           -1.024e-01  5.273e-01
## carrier.fctrVerizon                            5.040e-01  4.853e-01
## D.npnct09.log                                 -7.855e+00  8.278e+02
## D.ndgts.log                                    5.038e-01  5.452e-01
## D.nwrds.unq.log                               -3.887e+03  2.673e+03
## D.terms.n.post.stem.log                               NA         NA
## D.terms.n.post.stop.log                        3.883e+03  2.673e+03
## cellular.fctr1                                 2.771e-01  6.622e-01
## cellular.fctrUnknown                                  NA         NA
## D.npnct14.log                                 -2.544e+00  1.176e+00
## D.terms.n.post.stem                            2.639e+01  1.661e+01
## D.terms.n.post.stop                           -2.642e+01  1.656e+01
## D.npnct05.log                                 -2.640e+00  1.799e+00
## `condition.fctrFor parts or not working`      -2.919e-02  3.814e-01
## `condition.fctrManufacturer refurbished`       5.394e-01  6.406e-01
## condition.fctrNew                             -2.353e-01  3.242e-01
## `condition.fctrNew other (see details)`       -6.686e-02  5.029e-01
## `condition.fctrSeller refurbished`            -3.336e-01  4.441e-01
## idseq.my                                      -1.515e-04  2.109e-04
## startprice.diff                               -1.203e-02  1.366e-03
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.768e+00  7.792e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       7.825e-01  8.591e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       8.548e-01  7.425e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      1.112e-01  7.261e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -1.294e-01  7.044e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.051e+00  7.692e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2` -5.710e-01  1.010e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -1.765e-01  1.041e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -1.338e-01  9.369e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -3.401e-01  1.311e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -1.060e-01  9.774e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -8.818e-01  9.882e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     1.058e+00  9.597e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -8.029e-02  1.006e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.477e+00  9.059e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       3.114e+00  1.432e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      7.984e-01  9.712e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -3.965e-01  9.260e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`     -1.295e+00  9.826e-01
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -1.029e+00  1.377e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`              NA         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`     -1.443e+01  9.181e+02
## `prdline.my.fctriPadAir:.clusterid.fctr6`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`            NA         NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                    -1.435   0.1514    
## biddable                                       13.694   <2e-16 ***
## D.ratio.nstopwrds.nwrds                        -2.168   0.0301 *  
## D.npnct15.log                                   1.562   0.1182    
## D.npnct03.log                                   0.682   0.4954    
## D.terms.n.stem.stop.Ratio                       1.441   0.1495    
## D.ratio.sum.TfIdf.nwrds                         0.015   0.9883    
## .rnorm                                          0.489   0.6247    
## D.npnct01.log                                   0.610   0.5420    
## D.TfIdf.sum.stem.stop.Ratio                    -0.211   0.8328    
## storage.fctr16                                 -0.997   0.3187    
## storage.fctr32                                 -1.243   0.2139    
## storage.fctr64                                 -0.092   0.9267    
## storage.fctrUnknown                            -1.401   0.1612    
## D.npnct11.log                                   0.542   0.5881    
## D.npnct10.log                                  -0.020   0.9837    
## D.TfIdf.sum.post.stop                          -0.137   0.8907    
## D.npnct13.log                                   0.478   0.6324    
## D.TfIdf.sum.post.stem                           0.234   0.8149    
## D.sum.TfIdf                                        NA       NA    
## color.fctrGold                                  0.111   0.9115    
## `color.fctrSpace Gray`                         -1.238   0.2158    
## color.fctrUnknown                              -0.235   0.8141    
## color.fctrWhite                                -1.105   0.2690    
## D.npnct08.log                                   0.926   0.3547    
## `prdline.my.fctriPad 1`                         1.769   0.0768 .  
## `prdline.my.fctriPad 2`                         0.624   0.5328    
## `prdline.my.fctriPad 3+`                        1.450   0.1471    
## prdline.my.fctriPadAir                          2.101   0.0357 *  
## prdline.my.fctriPadmini                         1.092   0.2748    
## `prdline.my.fctriPadmini 2+`                    1.529   0.1262    
## D.nstopwrds.log                                 1.920   0.0549 .  
## D.npnct16.log                                   1.543   0.1227    
## D.npnct24.log                                  -0.853   0.3936    
## D.npnct06.log                                  -2.512   0.0120 *  
## D.npnct28.log                                  -0.002   0.9980    
## D.nuppr.log                                     0.679   0.4974    
## D.nchrs.log                                    -0.761   0.4465    
## D.nwrds.log                                    -0.079   0.9373    
## D.npnct12.log                                   0.807   0.4199    
## carrier.fctrNone                                0.309   0.7573    
## carrier.fctrOther                               0.009   0.9931    
## carrier.fctrSprint                              1.548   0.1217    
## `carrier.fctrT-Mobile`                         -0.725   0.4683    
## carrier.fctrUnknown                            -0.194   0.8460    
## carrier.fctrVerizon                             1.039   0.2990    
## D.npnct09.log                                  -0.009   0.9924    
## D.ndgts.log                                     0.924   0.3554    
## D.nwrds.unq.log                                -1.454   0.1459    
## D.terms.n.post.stem.log                            NA       NA    
## D.terms.n.post.stop.log                         1.453   0.1463    
## cellular.fctr1                                  0.418   0.6756    
## cellular.fctrUnknown                               NA       NA    
## D.npnct14.log                                  -2.163   0.0305 *  
## D.terms.n.post.stem                             1.589   0.1121    
## D.terms.n.post.stop                            -1.596   0.1106    
## D.npnct05.log                                  -1.467   0.1423    
## `condition.fctrFor parts or not working`       -0.077   0.9390    
## `condition.fctrManufacturer refurbished`        0.842   0.3998    
## condition.fctrNew                              -0.726   0.4678    
## `condition.fctrNew other (see details)`        -0.133   0.8942    
## `condition.fctrSeller refurbished`             -0.751   0.4526    
## idseq.my                                       -0.719   0.4724    
## startprice.diff                                -8.811   <2e-16 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.269   0.0233 *  
## `prdline.my.fctriPad 1:.clusterid.fctr2`        0.911   0.3624    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        1.151   0.2497    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`       0.153   0.8782    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.184   0.8542    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.366   0.1720    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  -0.565   0.5718    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.170   0.8653    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.143   0.8864    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.259   0.7953    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.108   0.9136    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.892   0.3722    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.102   0.2704    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.080   0.9364    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.631   0.1030    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        2.175   0.0296 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.822   0.4110    
## `prdline.my.fctriPadAir:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.428   0.6685    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      -1.318   0.1876    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.747   0.4548    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr6`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr6`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr6`           NA       NA    
## `prdline.my.fctriPad 3+:.clusterid.fctr6`      -0.016   0.9875    
## `prdline.my.fctriPadAir:.clusterid.fctr6`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr6`         NA       NA    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  784.39  on 892  degrees of freedom
## AIC: 948.39
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_1-4.png) ![](ebayipads_color_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7130008
## 3        0.2 0.7621671
## 4        0.3 0.7709163
## 5        0.4 0.7978610
## 6        0.5 0.8132875
## 7        0.6 0.8062575
## 8        0.7 0.7888041
## 9        0.8 0.6972222
## 10       0.9 0.5138211
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           456                            68
## 2         Y                            95                           355
##          Prediction
## Reference   N   Y
##         N 456  68
##         Y  95 355
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326489e-01   6.619222e-01   8.076954e-01   8.555717e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   4.994716e-84   4.170246e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_1-6.png) ![](ebayipads_color_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6994536
## 3        0.2 0.7197581
## 4        0.3 0.7449664
## 5        0.4 0.7614458
## 6        0.5 0.7715736
## 7        0.6 0.7710526
## 8        0.7 0.7622951
## 9        0.8 0.6786248
## 10       0.9 0.5061947
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           401                            74
## 2         Y                           106                           304
##          Prediction
## Reference   N   Y
##         N 401  74
##         Y 106 304
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.966102e-01   5.888183e-01   7.685578e-01   8.226715e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.323328e-58   2.085476e-02 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.801                 0.258
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.893698                    0.5       0.8132875         0.767955
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8076954             0.8555717     0.5327262   0.8431528
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7715736        0.7966102
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7685578             0.8226715     0.5888183    948.3929
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01277838       0.0264692
##                   label step_major step_minor     bgn     end elapsed
## 2      fit.models_1_glm          2          0 165.688 171.996   6.308
## 3 fit.models_1_bayesglm          3          0 171.996      NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_color_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.5621  -0.6492  -0.1474   0.5758   2.8296  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                    6.4732613  6.5967327
## biddable                                       3.1415025  0.2236448
## D.ratio.nstopwrds.nwrds                       -2.0016485  2.4122284
## D.npnct15.log                                  1.4920678  0.8898038
## D.npnct03.log                                  0.7287085  1.6791658
## D.terms.n.stem.stop.Ratio                     -5.3051998  5.2920393
## D.ratio.sum.TfIdf.nwrds                       -0.1788281  0.2804948
## .rnorm                                         0.0442853  0.0941655
## D.npnct01.log                                  0.3019465  0.5466944
## D.TfIdf.sum.stem.stop.Ratio                   -0.2632203  3.3867123
## storage.fctr16                                -0.4268016  0.4513403
## storage.fctr32                                -0.6090960  0.4796292
## storage.fctr64                                 0.0215054  0.4745300
## storage.fctrUnknown                           -0.7801934  0.6240624
## D.npnct11.log                                  0.1354597  0.3431836
## D.npnct10.log                                 -7.8702366  7.4401293
## D.TfIdf.sum.post.stop                          0.0622741  0.2937135
## D.npnct13.log                                  0.0796768  0.3410379
## D.TfIdf.sum.post.stem                          0.0817057  0.3101478
## D.sum.TfIdf                                    0.0817057  0.3101478
## color.fctrGold                                 0.0738722  0.5380469
## `color.fctrSpace Gray`                        -0.4843851  0.3591170
## color.fctrUnknown                             -0.0557312  0.2473266
## color.fctrWhite                               -0.3312580  0.2674640
## D.npnct08.log                                  0.4948063  0.7028659
## `prdline.my.fctriPad 1`                        0.7439913  0.4859660
## `prdline.my.fctriPad 2`                        0.2259026  0.4944116
## `prdline.my.fctriPad 3+`                       0.5746895  0.4674085
## prdline.my.fctriPadAir                         0.8999477  0.4688732
## prdline.my.fctriPadmini                        0.3800660  0.4563442
## `prdline.my.fctriPadmini 2+`                   0.6052630  0.4998567
## D.nstopwrds.log                                0.6193376  0.6513100
## D.npnct16.log                                  2.4321111  1.5861158
## D.npnct24.log                                 -0.1571228  2.3237832
## D.npnct06.log                                 -4.8219145  1.9065228
## D.npnct28.log                                 -0.0917201  2.1756298
## D.nuppr.log                                    0.0282537  0.4728189
## D.nchrs.log                                   -0.0773765  0.4760178
## D.nwrds.log                                   -0.0694265  0.7684373
## D.npnct12.log                                  0.4095706  0.6865852
## carrier.fctrNone                               0.0509338  1.1589591
## carrier.fctrOther                              0.5232556  1.8491401
## carrier.fctrSprint                             1.0981275  0.7406424
## `carrier.fctrT-Mobile`                        -0.6858277  0.8748829
## carrier.fctrUnknown                           -0.1980645  0.4785585
## carrier.fctrVerizon                            0.4245266  0.4480254
## D.npnct09.log                                 -1.8039793  5.2114152
## D.ndgts.log                                    0.6500518  0.4167512
## D.nwrds.unq.log                               -0.2693509  1.0356264
## D.terms.n.post.stem.log                       -0.2693509  1.0356264
## D.terms.n.post.stop.log                       -0.2650471  1.0315822
## cellular.fctr1                                 0.1280323  1.1501476
## cellular.fctrUnknown                          -0.1820448  1.1993917
## D.npnct14.log                                 -2.3968587  1.0438074
## D.terms.n.post.stem                           -0.0861699  0.2053404
## D.terms.n.post.stop                           -0.1096726  0.2039429
## D.npnct05.log                                 -2.3408280  1.4309601
## `condition.fctrFor parts or not working`       0.0178713  0.3598336
## `condition.fctrManufacturer refurbished`       0.5587583  0.6008105
## condition.fctrNew                             -0.2185260  0.3092958
## `condition.fctrNew other (see details)`       -0.0413030  0.4603463
## `condition.fctrSeller refurbished`            -0.2859324  0.4135467
## idseq.my                                      -0.0001842  0.0002020
## startprice.diff                               -0.0112824  0.0012677
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.3773335  0.6569271
## `prdline.my.fctriPad 1:.clusterid.fctr2`       0.6830251  0.7189179
## `prdline.my.fctriPad 2:.clusterid.fctr2`       0.6950682  0.6281976
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -0.0494174  0.6273654
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -0.1582895  0.5942248
## `prdline.my.fctriPadmini:.clusterid.fctr2`     0.8263218  0.6677774
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2` -0.3121617  0.8178211
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -0.3238598  0.8390054
## `prdline.my.fctriPad 1:.clusterid.fctr3`       0.0215532  0.7847840
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -0.5628903  1.0369246
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -0.1492070  0.7940398
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -0.6366807  0.7663736
## `prdline.my.fctriPadmini:.clusterid.fctr3`     0.9188031  0.7898413
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -0.1325030  0.8295810
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.4353422  0.7804594
## `prdline.my.fctriPad 2:.clusterid.fctr4`       2.6373335  1.1479779
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      0.6223773  0.7899497
## `prdline.my.fctriPadAir:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -0.3661226  0.7762206
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 3+:.clusterid.fctr5`     -0.9053648  0.8033085
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -0.8143261  1.0989636
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr6`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr6`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr6`       0.0000000  2.5000000
## `prdline.my.fctriPad 3+:.clusterid.fctr6`     -1.1439156  1.5833740
## `prdline.my.fctriPadAir:.clusterid.fctr6`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr6`     0.0000000  2.5000000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`  0.0000000  2.5000000
##                                               z value Pr(>|z|)    
## (Intercept)                                     0.981   0.3265    
## biddable                                       14.047   <2e-16 ***
## D.ratio.nstopwrds.nwrds                        -0.830   0.4067    
## D.npnct15.log                                   1.677   0.0936 .  
## D.npnct03.log                                   0.434   0.6643    
## D.terms.n.stem.stop.Ratio                      -1.002   0.3161    
## D.ratio.sum.TfIdf.nwrds                        -0.638   0.5238    
## .rnorm                                          0.470   0.6381    
## D.npnct01.log                                   0.552   0.5807    
## D.TfIdf.sum.stem.stop.Ratio                    -0.078   0.9380    
## storage.fctr16                                 -0.946   0.3443    
## storage.fctr32                                 -1.270   0.2041    
## storage.fctr64                                  0.045   0.9639    
## storage.fctrUnknown                            -1.250   0.2112    
## D.npnct11.log                                   0.395   0.6931    
## D.npnct10.log                                  -1.058   0.2901    
## D.TfIdf.sum.post.stop                           0.212   0.8321    
## D.npnct13.log                                   0.234   0.8153    
## D.TfIdf.sum.post.stem                           0.263   0.7922    
## D.sum.TfIdf                                     0.263   0.7922    
## color.fctrGold                                  0.137   0.8908    
## `color.fctrSpace Gray`                         -1.349   0.1774    
## color.fctrUnknown                              -0.225   0.8217    
## color.fctrWhite                                -1.239   0.2155    
## D.npnct08.log                                   0.704   0.4814    
## `prdline.my.fctriPad 1`                         1.531   0.1258    
## `prdline.my.fctriPad 2`                         0.457   0.6477    
## `prdline.my.fctriPad 3+`                        1.230   0.2189    
## prdline.my.fctriPadAir                          1.919   0.0549 .  
## prdline.my.fctriPadmini                         0.833   0.4049    
## `prdline.my.fctriPadmini 2+`                    1.211   0.2259    
## D.nstopwrds.log                                 0.951   0.3416    
## D.npnct16.log                                   1.533   0.1252    
## D.npnct24.log                                  -0.068   0.9461    
## D.npnct06.log                                  -2.529   0.0114 *  
## D.npnct28.log                                  -0.042   0.9664    
## D.nuppr.log                                     0.060   0.9524    
## D.nchrs.log                                    -0.163   0.8709    
## D.nwrds.log                                    -0.090   0.9280    
## D.npnct12.log                                   0.597   0.5508    
## carrier.fctrNone                                0.044   0.9649    
## carrier.fctrOther                               0.283   0.7772    
## carrier.fctrSprint                              1.483   0.1382    
## `carrier.fctrT-Mobile`                         -0.784   0.4331    
## carrier.fctrUnknown                            -0.414   0.6790    
## carrier.fctrVerizon                             0.948   0.3434    
## D.npnct09.log                                  -0.346   0.7292    
## D.ndgts.log                                     1.560   0.1188    
## D.nwrds.unq.log                                -0.260   0.7948    
## D.terms.n.post.stem.log                        -0.260   0.7948    
## D.terms.n.post.stop.log                        -0.257   0.7972    
## cellular.fctr1                                  0.111   0.9114    
## cellular.fctrUnknown                           -0.152   0.8794    
## D.npnct14.log                                  -2.296   0.0217 *  
## D.terms.n.post.stem                            -0.420   0.6747    
## D.terms.n.post.stop                            -0.538   0.5907    
## D.npnct05.log                                  -1.636   0.1019    
## `condition.fctrFor parts or not working`        0.050   0.9604    
## `condition.fctrManufacturer refurbished`        0.930   0.3524    
## condition.fctrNew                              -0.707   0.4799    
## `condition.fctrNew other (see details)`        -0.090   0.9285    
## `condition.fctrSeller refurbished`             -0.691   0.4893    
## idseq.my                                       -0.912   0.3616    
## startprice.diff                                -8.900   <2e-16 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.097   0.0360 *  
## `prdline.my.fctriPad 1:.clusterid.fctr2`        0.950   0.3421    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        1.106   0.2685    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -0.079   0.9372    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.266   0.7899    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.237   0.2159    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  -0.382   0.7027    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.386   0.6995    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.027   0.9781    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.543   0.5872    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.188   0.8509    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.831   0.4061    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.163   0.2447    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.160   0.8731    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000   1.0000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.839   0.0659 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        2.297   0.0216 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       0.788   0.4308    
## `prdline.my.fctriPadAir:.clusterid.fctr4`       0.000   1.0000    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.472   0.6372    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000   1.0000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000   1.0000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000   1.0000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        0.000   1.0000    
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      -1.127   0.2597    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000   1.0000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.741   0.4587    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000   1.0000    
## `prdline.my.fctrUnknown:.clusterid.fctr6`       0.000   1.0000    
## `prdline.my.fctriPad 1:.clusterid.fctr6`        0.000   1.0000    
## `prdline.my.fctriPad 2:.clusterid.fctr6`        0.000   1.0000    
## `prdline.my.fctriPad 3+:.clusterid.fctr6`      -0.722   0.4700    
## `prdline.my.fctriPadAir:.clusterid.fctr6`       0.000   1.0000    
## `prdline.my.fctriPadmini:.clusterid.fctr6`      0.000   1.0000    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`   0.000   1.0000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  794.78  on 875  degrees of freedom
## AIC: 992.78
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7052209
## 3        0.2 0.7554745
## 4        0.3 0.7696909
## 5        0.4 0.7859425
## 6        0.5 0.8127128
## 7        0.6 0.8004837
## 8        0.7 0.7783505
## 9        0.8 0.6946779
## 10       0.9 0.4731544
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                451
## 2         Y                                 92
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 73
## 2                                358
##          Prediction
## Reference   N   Y
##         N 451  73
##         Y  92 358
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.305955e-01   6.582049e-01   8.055345e-01   8.536386e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   8.982639e-83   1.611249e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6958855
## 3        0.2 0.7225549
## 4        0.3 0.7452725
## 5        0.4 0.7572816
## 6        0.5 0.7708067
## 7        0.6 0.7669774
## 8        0.7 0.7527778
## 9        0.8 0.6858006
## 10       0.9 0.4688645
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                405
## 2         Y                                109
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 70
## 2                                301
##          Prediction
## Reference   N   Y
##         N 405  70
##         Y 109 301
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.977401e-01   5.906219e-01   7.697388e-01   8.237428e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   3.897512e-59   4.507772e-03 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.602                 0.371
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8912129                    0.5       0.8127128        0.7638683
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8055345             0.8536386      0.524377    0.846285
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7708067        0.7977401
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7697388             0.8237428     0.5906219    992.7758
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.007372855      0.01470821
##                   label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_bayesglm          3          0 171.996 188.908  16.912
## 4   fit.models_1_glmnet          4          0 188.909      NA      NA
## [1] "fitting model: All.X.glmnet"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
```

```
## Loading required package: glmnet
## Loaded glmnet 2.0-2
```

![](ebayipads_color_files/figure-html/fit.models_1-12.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 1, lambda = 0.0544 on full training set
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

![](ebayipads_color_files/figure-html/fit.models_1-13.png) ![](ebayipads_color_files/figure-html/fit.models_1-14.png) 

```
##             Length Class      Mode     
## a0            86   -none-     numeric  
## beta        8428   dgCMatrix  S4       
## df            86   -none-     numeric  
## dim            2   -none-     numeric  
## lambda        86   -none-     numeric  
## dev.ratio     86   -none-     numeric  
## nulldev        1   -none-     numeric  
## npasses        1   -none-     numeric  
## jerr           1   -none-     numeric  
## offset         1   -none-     logical  
## classnames     2   -none-     character
## call           5   -none-     call     
## nobs           1   -none-     numeric  
## lambdaOpt      1   -none-     numeric  
## xNames        98   -none-     character
## problemType    1   -none-     character
## tuneValue      2   data.frame list     
## obsLevels      2   -none-     character
## [1] "min lambda > lambdaOpt:"
##     (Intercept)        biddable startprice.diff 
##    -0.919515342     1.946620085    -0.004400296 
## [1] "max lambda < lambdaOpt:"
##                                 (Intercept) 
##                                1.276539e+01 
##                                    biddable 
##                                3.247199e+00 
##                     D.ratio.nstopwrds.nwrds 
##                               -9.083982e+00 
##                               D.npnct15.log 
##                                1.407742e+00 
##                               D.npnct03.log 
##                                9.912413e-01 
##                   D.terms.n.stem.stop.Ratio 
##                               -4.340034e+00 
##                     D.ratio.sum.TfIdf.nwrds 
##                               -2.183396e-01 
##                                      .rnorm 
##                                5.021253e-02 
##                               D.npnct01.log 
##                                2.364494e-01 
##                 D.TfIdf.sum.stem.stop.Ratio 
##                               -5.113291e-01 
##                              storage.fctr16 
##                               -5.187503e-01 
##                              storage.fctr32 
##                               -7.161513e-01 
##                              storage.fctr64 
##                               -6.394010e-02 
##                         storage.fctrUnknown 
##                               -9.483757e-01 
##                               D.npnct11.log 
##                                1.224066e-01 
##                               D.npnct10.log 
##                               -1.103133e+01 
##                       D.TfIdf.sum.post.stop 
##                                4.139503e-02 
##                               D.npnct13.log 
##                                3.052695e-02 
##                       D.TfIdf.sum.post.stem 
##                                1.855788e-01 
##                                 D.sum.TfIdf 
##                                1.093133e-02 
##                              color.fctrGold 
##                                4.367830e-02 
##                        color.fctrSpace Gray 
##                               -5.181102e-01 
##                           color.fctrUnknown 
##                               -6.875321e-02 
##                             color.fctrWhite 
##                               -3.460863e-01 
##                               D.npnct08.log 
##                                5.029262e-01 
##                       prdline.my.fctriPad 1 
##                                9.566672e-01 
##                       prdline.my.fctriPad 2 
##                                3.352388e-01 
##                      prdline.my.fctriPad 3+ 
##                                7.665349e-01 
##                      prdline.my.fctriPadAir 
##                                1.114513e+00 
##                     prdline.my.fctriPadmini 
##                                5.437103e-01 
##                  prdline.my.fctriPadmini 2+ 
##                                8.284349e-01 
##                             D.nstopwrds.log 
##                                2.295466e+00 
##                               D.npnct16.log 
##                                2.776479e+00 
##                               D.npnct24.log 
##                               -2.040182e+00 
##                               D.npnct06.log 
##                               -5.580554e+00 
##                               D.npnct28.log 
##                               -1.600276e+00 
##                                 D.nchrs.log 
##                               -4.892271e-01 
##                                 D.nwrds.log 
##                               -3.992016e-03 
##                               D.npnct12.log 
##                                5.291397e-01 
##                           carrier.fctrOther 
##                                3.840250e+00 
##                          carrier.fctrSprint 
##                                1.293093e+00 
##                        carrier.fctrT-Mobile 
##                               -7.690768e-01 
##                         carrier.fctrUnknown 
##                               -1.422833e-01 
##                         carrier.fctrVerizon 
##                                4.995221e-01 
##                               D.npnct09.log 
##                               -1.302441e+00 
##                                 D.ndgts.log 
##                                5.801334e-01 
##                             D.nwrds.unq.log 
##                               -2.884429e+00 
##                              cellular.fctr1 
##                                2.795507e-02 
##                        cellular.fctrUnknown 
##                               -2.033592e-01 
##                               D.npnct14.log 
##                               -2.668604e+00 
##                         D.terms.n.post.stop 
##                               -1.283063e-01 
##                               D.npnct05.log 
##                               -2.738838e+00 
##      condition.fctrFor parts or not working 
##                               -3.326092e-02 
##      condition.fctrManufacturer refurbished 
##                                6.221347e-01 
##                           condition.fctrNew 
##                               -2.245634e-01 
##       condition.fctrNew other (see details) 
##                               -4.730393e-02 
##            condition.fctrSeller refurbished 
##                               -2.726138e-01 
##                                    idseq.my 
##                               -1.612032e-04 
##                             startprice.diff 
##                               -1.173299e-02 
##     prdline.my.fctrUnknown:.clusterid.fctr2 
##                                1.579022e+00 
##      prdline.my.fctriPad 1:.clusterid.fctr2 
##                                7.277815e-01 
##      prdline.my.fctriPad 2:.clusterid.fctr2 
##                                8.153899e-01 
##     prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                7.426905e-03 
##     prdline.my.fctriPadAir:.clusterid.fctr2 
##                               -1.764080e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr2 
##                                8.859192e-01 
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                               -4.211580e-01 
##     prdline.my.fctrUnknown:.clusterid.fctr3 
##                               -1.560171e-01 
##      prdline.my.fctriPad 1:.clusterid.fctr3 
##                               -2.946137e-02 
##      prdline.my.fctriPad 2:.clusterid.fctr3 
##                               -7.809401e-01 
##     prdline.my.fctriPad 3+:.clusterid.fctr3 
##                               -8.780368e-02 
##     prdline.my.fctriPadAir:.clusterid.fctr3 
##                               -9.235025e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr3 
##                                1.021235e+00 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                               -1.953260e-01 
##      prdline.my.fctriPad 1:.clusterid.fctr4 
##                               -1.581805e+00 
##      prdline.my.fctriPad 2:.clusterid.fctr4 
##                                3.179580e+00 
##     prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                7.783435e-01 
##    prdline.my.fctriPadmini:.clusterid.fctr4 
##                               -4.596103e-01 
##     prdline.my.fctriPad 3+:.clusterid.fctr5 
##                               -1.218201e+00 
##    prdline.my.fctriPadmini:.clusterid.fctr5 
##                               -1.151030e+00 
##     prdline.my.fctriPad 3+:.clusterid.fctr6 
##                               -4.629991e+00 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-15.png) 

```
##    threshold    f.score
## 1        0.0 0.63202247
## 2        0.1 0.63784550
## 3        0.2 0.66818874
## 4        0.3 0.73653846
## 5        0.4 0.74725275
## 6        0.5 0.76244344
## 7        0.6 0.76666667
## 8        0.7 0.71369295
## 9        0.8 0.15132924
## 10       0.9 0.01324503
## 11       1.0 0.00000000
```

![](ebayipads_color_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              456
## 2         Y                              128
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               68
## 2                              322
##          Prediction
## Reference   N   Y
##         N 456  68
##         Y 128 322
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.987680e-01   5.913520e-01   7.721896e-01   8.235260e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   7.220245e-65   2.505699e-05 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-17.png) 

```
##    threshold     f.score
## 1        0.0 0.633204633
## 2        0.1 0.639498433
## 3        0.2 0.675126904
## 4        0.3 0.746582545
## 5        0.4 0.752157830
## 6        0.5 0.762626263
## 7        0.6 0.775132275
## 8        0.7 0.714285714
## 9        0.8 0.165548098
## 10       0.9 0.009708738
## 11       1.0 0.000000000
```

![](ebayipads_color_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              422
## 2         Y                              117
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               53
## 2                              293
##          Prediction
## Reference   N   Y
##         N 422  53
##         Y 117 293
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.079096e-01   6.095656e-01   7.803820e-01   8.333692e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.732450e-64   1.352502e-06 
##       model_id model_method
## 1 All.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      5.826                 1.167
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8592409                    0.6       0.7666667        0.7864482
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7721896              0.823526     0.5692702   0.8631938
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7751323        0.8079096
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.780382             0.8333692     0.6095656
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004643467      0.01226196
##                 label step_major step_minor     bgn     end elapsed
## 4 fit.models_1_glmnet          4          0 188.909 199.764  10.855
## 5  fit.models_1_rpart          5          0 199.765      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.02 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_color_files/figure-html/fit.models_1-19.png) ![](ebayipads_color_files/figure-html/fit.models_1-20.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##           CP nsplit rel error
## 1 0.51111111      0 1.0000000
## 2 0.08666667      1 0.4888889
## 3 0.02000000      2 0.4022222
## 
## Variable importance
##                                    biddable 
##                                          54 
##                             startprice.diff 
##                                          23 
##                                    idseq.my 
##                                          11 
##      condition.fctrFor parts or not working 
##                                           3 
##                       prdline.my.fctriPad 1 
##                                           2 
##                       prdline.my.fctriPad 2 
##                                           2 
##                     D.ratio.sum.TfIdf.nwrds 
##                                           2 
##                           condition.fctrNew 
##                                           1 
##       condition.fctrNew other (see details) 
##                                           1 
##                  prdline.my.fctriPadmini 2+ 
##                                           1 
##                              color.fctrGold 
##                                           1 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                           1 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable          < 0.5       to the left,  improve=144.14990, (0 missing)
##       startprice.diff   < 59.64413  to the right, improve= 48.01938, (0 missing)
##       idseq.my          < 905.5     to the right, improve= 38.91299, (0 missing)
##       condition.fctrNew < 0.5       to the right, improve= 10.86739, (0 missing)
##       D.nwrds.unq.log   < 2.138333  to the right, improve=  9.59880, (0 missing)
##   Surrogate splits:
##       idseq.my                               < 869       to the right, agree=0.636, adj=0.211, (0 split)
##       condition.fctrFor parts or not working < 0.5       to the left,  agree=0.563, adj=0.053, (0 split)
##       prdline.my.fctriPad 1                  < 0.5       to the left,  agree=0.559, adj=0.044, (0 split)
##       prdline.my.fctriPad 2                  < 0.5       to the left,  agree=0.553, adj=0.033, (0 split)
##       D.ratio.sum.TfIdf.nwrds                < 0.9315387 to the left,  agree=0.551, adj=0.029, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.08666667
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (135 obs) right son=7 (315 obs)
##   Primary splits:
##       startprice.diff      < 68.91842  to the right, improve=61.714290, (0 missing)
##       idseq.my             < 670.5     to the right, improve=15.110620, (0 missing)
##       condition.fctrNew    < 0.5       to the right, improve= 3.467222, (0 missing)
##       carrier.fctrUnknown  < 0.5       to the right, improve= 3.110995, (0 missing)
##       cellular.fctrUnknown < 0.5       to the right, improve= 2.722222, (0 missing)
##   Surrogate splits:
##       condition.fctrNew                           < 0.5       to the right, agree=0.713, adj=0.044, (0 split)
##       prdline.my.fctriPadmini 2+                  < 0.5       to the right, agree=0.709, adj=0.030, (0 split)
##       condition.fctrNew other (see details)       < 0.5       to the right, agree=0.709, adj=0.030, (0 split)
##       color.fctrGold                              < 0.5       to the right, agree=0.707, adj=0.022, (0 split)
##       prdline.my.fctriPadmini 2+:.clusterid.fctr3 < 0.5       to the right, agree=0.707, adj=0.022, (0 split)
## 
## Node number 6: 135 observations
##   predicted class=N  expected loss=0.3555556  P(node) =0.1386037
##     class counts:    87    48
##    probabilities: 0.644 0.356 
## 
## Node number 7: 315 observations
##   predicted class=Y  expected loss=0.07301587  P(node) =0.3234086
##     class counts:    23   292
##    probabilities: 0.073 0.927 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.53798768 0.46201232)  
##   2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##   3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##     6) startprice.diff>=68.91842 135  48 N (0.64444444 0.35555556) *
##     7) startprice.diff< 68.91842 315  23 Y (0.07301587 0.92698413) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7555556
## 5        0.4 0.7633987
## 6        0.5 0.7633987
## 7        0.6 0.7633987
## 8        0.7 0.7633987
## 9        0.8 0.7633987
## 10       0.9 0.7633987
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-22.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      501
## 2         Y                                      158
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       23
## 2                                      292
##          Prediction
## Reference   N   Y
##         N 501  23
##         Y 158 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.141684e-01   6.180889e-01   7.882906e-01   8.381305e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.530340e-73   2.277382e-23 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7528231
## 5        0.4 0.7420290
## 6        0.5 0.7420290
## 7        0.6 0.7420290
## 8        0.7 0.7420290
## 9        0.8 0.7420290
## 10       0.9 0.7420290
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      388
## 2         Y                                      110
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       87
## 2                                      300
##          Prediction
## Reference   N   Y
##         N 388  87
##         Y 110 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.774011e-01   5.506630e-01   7.485294e-01   8.044124e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.956102e-50   1.170130e-01 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.673                 0.069
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8243427                    0.9       0.7633987        0.8018645
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7882906             0.8381305     0.5940341   0.8129705
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7528231        0.7774011
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7485294             0.8044124      0.550663
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01388821      0.02957203
##                label step_major step_minor     bgn     end elapsed
## 5 fit.models_1_rpart          5          0 199.765 205.638   5.873
## 6    fit.models_1_rf          6          0 205.638      NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_color_files/figure-html/fit.models_1-24.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 49 on full training set
```

![](ebayipads_color_files/figure-html/fit.models_1-25.png) ![](ebayipads_color_files/figure-html/fit.models_1-26.png) 

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
## importance        97   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            97   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-27.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8249313
## 3        0.2 0.9433962
## 4        0.3 0.9814613
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 0.9522701
## 9        0.8 0.8650694
## 10       0.9 0.7721692
## 11       1.0 0.1171548
```

![](ebayipads_color_files/figure-html/fit.models_1-28.png) 

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

![](ebayipads_color_files/figure-html/fit.models_1-29.png) 

```
##    threshold    f.score
## 1        0.0 0.63320463
## 2        0.1 0.68221071
## 3        0.2 0.73034826
## 4        0.3 0.76049943
## 5        0.4 0.78378378
## 6        0.5 0.78543563
## 7        0.6 0.77329624
## 8        0.7 0.75644699
## 9        0.8 0.71171171
## 10       0.9 0.65280000
## 11       1.0 0.04295943
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   418
## 2         Y                                   108
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                    57
## 2                                   302
##          Prediction
## Reference   N   Y
##         N 418  57
##         Y 108 302
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.135593e-01   6.218781e-01   7.863067e-01   8.387053e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   6.832889e-67   9.921866e-05 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     16.547                 5.082
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.6               1        0.8018392
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962198                     1      0.597041   0.8637792
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7854356        0.8135593
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7863067             0.8387053     0.6218781
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01439889      0.02854761
##              label step_major step_minor     bgn     end elapsed
## 6  fit.models_1_rf          6          0 205.638 225.897  20.259
## 7 fit.models_1_glm          7          0 225.897      NA      NA
## [1] "fitting model: All.Interact.X.glm"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   62, 672, 951
```

![](ebayipads_color_files/figure-html/fit.models_1-30.png) ![](ebayipads_color_files/figure-html/fit.models_1-31.png) ![](ebayipads_color_files/figure-html/fit.models_1-32.png) 

```
## Warning: not plotting observations with leverage one:
##   62, 672, 951
```

![](ebayipads_color_files/figure-html/fit.models_1-33.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -3.02835  -0.61392  -0.07853   0.44023   2.74561  
## 
## Coefficients: (32 not defined because of singularities)
##                                                            Estimate
## (Intercept)                                              -1.766e+03
## D.ratio.nstopwrds.nwrds                                  -1.737e+01
## D.terms.n.stem.stop.Ratio                                 1.773e+03
## .rnorm                                                    3.075e-02
## D.npnct01.log                                             6.111e-01
## storage.fctr16                                           -3.510e-01
## storage.fctr32                                           -5.069e-01
## storage.fctr64                                            8.672e-02
## storage.fctrUnknown                                      -7.415e-01
## D.npnct11.log                                            -3.206e-02
## D.npnct10.log                                            -2.644e+01
## D.TfIdf.sum.post.stop                                    -7.422e-01
## D.npnct13.log                                             1.445e-01
## D.TfIdf.sum.post.stem                                     8.595e-01
## D.sum.TfIdf                                                      NA
## color.fctrGold                                           -3.116e-01
## `color.fctrSpace Gray`                                   -6.135e-01
## color.fctrUnknown                                        -2.955e-01
## color.fctrWhite                                          -4.178e-01
## D.npnct08.log                                             6.326e-01
## `prdline.my.fctriPad 1`                                   2.995e+01
## `prdline.my.fctriPad 2`                                   2.279e+01
## `prdline.my.fctriPad 3+`                                  1.776e+01
## prdline.my.fctriPadAir                                    9.237e+00
## prdline.my.fctriPadmini                                   1.089e+01
## `prdline.my.fctriPadmini 2+`                              1.613e+01
## D.nstopwrds.log                                           4.470e+00
## D.npnct16.log                                             2.071e+00
## D.npnct24.log                                            -6.124e+00
## D.npnct06.log                                            -5.102e+00
## D.npnct28.log                                            -2.621e+00
## D.nuppr.log                                               1.575e+00
## D.nchrs.log                                              -2.257e+00
## D.nwrds.log                                              -5.270e-01
## D.npnct12.log                                             6.089e-01
## D.npnct09.log                                            -9.740e+00
## D.ndgts.log                                               4.734e-01
## D.nwrds.unq.log                                          -2.044e+03
## D.terms.n.post.stem.log                                          NA
## D.terms.n.post.stop.log                                   2.040e+03
## D.npnct14.log                                            -2.673e+00
## D.terms.n.post.stem                                       1.711e+01
## D.terms.n.post.stop                                      -1.726e+01
## D.npnct05.log                                            -3.244e+00
## `condition.fctrFor parts or not working`                 -2.454e-01
## `condition.fctrManufacturer refurbished`                  3.344e-01
## condition.fctrNew                                        -1.873e-01
## `condition.fctrNew other (see details)`                   9.792e-02
## `condition.fctrSeller refurbished`                       -5.681e-01
## idseq.my                                                  8.131e-04
## D.ratio.sum.TfIdf.nwrds                                   4.714e-01
## D.TfIdf.sum.stem.stop.Ratio                               8.321e+00
## D.npnct15.log                                            -2.104e+01
## D.npnct03.log                                             4.328e-01
## startprice.diff                                          -6.002e-03
## biddable                                                  3.981e+00
## cellular.fctr1                                            2.914e-03
## cellular.fctrUnknown                                     -4.209e-01
## carrier.fctrNone                                                 NA
## carrier.fctrOther                                         1.526e+01
## carrier.fctrSprint                                        1.198e+00
## `carrier.fctrT-Mobile`                                   -1.138e+00
## carrier.fctrUnknown                                       7.745e-02
## carrier.fctrVerizon                                       4.363e-01
## `prdline.my.fctriPad 1:idseq.my`                         -3.989e-04
## `prdline.my.fctriPad 2:idseq.my`                         -1.450e-03
## `prdline.my.fctriPad 3+:idseq.my`                        -6.034e-04
## `prdline.my.fctriPadAir:idseq.my`                        -1.877e-03
## `prdline.my.fctriPadmini:idseq.my`                       -9.396e-04
## `prdline.my.fctriPadmini 2+:idseq.my`                    -4.648e-04
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`          -6.382e-01
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`           1.926e+00
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`         -3.913e-01
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`         -1.427e+00
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`        -1.336e+00
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`     -2.559e+00
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`      -2.852e+01
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`      -2.134e+01
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`     -1.630e+01
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`     -6.101e+00
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`    -9.374e+00
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio` -1.468e+01
## `prdline.my.fctriPad 1:D.npnct15.log`                     4.099e+01
## `prdline.my.fctriPad 2:D.npnct15.log`                     4.322e+01
## `prdline.my.fctriPad 3+:D.npnct15.log`                    1.952e+01
## `prdline.my.fctriPadAir:D.npnct15.log`                    4.387e+01
## `prdline.my.fctriPadmini:D.npnct15.log`                   4.826e+01
## `prdline.my.fctriPadmini 2+:D.npnct15.log`                       NA
## `prdline.my.fctriPad 1:D.npnct03.log`                     2.514e+00
## `prdline.my.fctriPad 2:D.npnct03.log`                     1.242e-01
## `prdline.my.fctriPad 3+:D.npnct03.log`                   -1.944e+01
## `prdline.my.fctriPadAir:D.npnct03.log`                    1.333e+00
## `prdline.my.fctriPadmini:D.npnct03.log`                          NA
## `prdline.my.fctriPadmini 2+:D.npnct03.log`                       NA
## `startprice.diff:biddable`                               -1.412e-02
## `cellular.fctr1:carrier.fctrNone`                                NA
## `cellular.fctrUnknown:carrier.fctrNone`                          NA
## `cellular.fctr1:carrier.fctrOther`                               NA
## `cellular.fctrUnknown:carrier.fctrOther`                         NA
## `cellular.fctr1:carrier.fctrSprint`                              NA
## `cellular.fctrUnknown:carrier.fctrSprint`                        NA
## `cellular.fctr1:carrier.fctrT-Mobile`                            NA
## `cellular.fctrUnknown:carrier.fctrT-Mobile`                      NA
## `cellular.fctr1:carrier.fctrUnknown`                             NA
## `cellular.fctrUnknown:carrier.fctrUnknown`                       NA
## `cellular.fctr1:carrier.fctrVerizon`                             NA
## `cellular.fctrUnknown:carrier.fctrVerizon`                       NA
## `prdline.my.fctrUnknown:.clusterid.fctr2`                 1.964e+00
## `prdline.my.fctriPad 1:.clusterid.fctr2`                 -1.222e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`                 -9.100e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                -2.576e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                 1.064e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                1.536e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`             3.497e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`                -9.273e-01
## `prdline.my.fctriPad 1:.clusterid.fctr3`                 -1.216e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                 -1.963e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                -6.334e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                 1.888e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`                1.470e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`             1.296e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                        NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                 -2.172e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                  1.760e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                 4.479e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`                        NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`               -8.706e-04
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                    NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                        NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                -1.476e+00
## `prdline.my.fctriPadAir:.clusterid.fctr5`                        NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`               -7.159e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                    NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`                        NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`                         NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`                         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                -1.627e+01
## `prdline.my.fctriPadAir:.clusterid.fctr6`                        NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`                       NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                    NA
##                                                          Std. Error
## (Intercept)                                               2.698e+03
## D.ratio.nstopwrds.nwrds                                   8.000e+00
## D.terms.n.stem.stop.Ratio                                 2.700e+03
## .rnorm                                                    1.046e-01
## D.npnct01.log                                             7.102e-01
## storage.fctr16                                            5.347e-01
## storage.fctr32                                            5.707e-01
## storage.fctr64                                            5.477e-01
## storage.fctrUnknown                                       7.446e-01
## D.npnct11.log                                             4.130e-01
## D.npnct10.log                                             2.018e+03
## D.TfIdf.sum.post.stop                                     3.625e+00
## D.npnct13.log                                             4.491e-01
## D.TfIdf.sum.post.stem                                     3.784e+00
## D.sum.TfIdf                                                      NA
## color.fctrGold                                            5.799e-01
## `color.fctrSpace Gray`                                    4.077e-01
## color.fctrUnknown                                         2.863e-01
## color.fctrWhite                                           3.086e-01
## D.npnct08.log                                             7.984e-01
## `prdline.my.fctriPad 1`                                   1.495e+01
## `prdline.my.fctriPad 2`                                   1.811e+01
## `prdline.my.fctriPad 3+`                                  1.126e+01
## prdline.my.fctriPadAir                                    1.504e+01
## prdline.my.fctriPadmini                                   1.249e+01
## `prdline.my.fctriPadmini 2+`                              1.451e+01
## D.nstopwrds.log                                           2.225e+00
## D.npnct16.log                                             1.835e+00
## D.npnct24.log                                             7.168e+00
## D.npnct06.log                                             2.316e+00
## D.npnct28.log                                             2.874e+03
## D.nuppr.log                                               4.751e+00
## D.nchrs.log                                               5.450e+00
## D.nwrds.log                                               3.472e+00
## D.npnct12.log                                             8.256e-01
## D.npnct09.log                                             1.342e+03
## D.ndgts.log                                               6.149e-01
## D.nwrds.unq.log                                           3.024e+03
## D.terms.n.post.stem.log                                          NA
## D.terms.n.post.stop.log                                   3.024e+03
## D.npnct14.log                                             1.327e+00
## D.terms.n.post.stem                                       1.846e+01
## D.terms.n.post.stop                                       1.841e+01
## D.npnct05.log                                             1.887e+00
## `condition.fctrFor parts or not working`                  4.449e-01
## `condition.fctrManufacturer refurbished`                  6.460e-01
## condition.fctrNew                                         3.368e-01
## `condition.fctrNew other (see details)`                   5.103e-01
## `condition.fctrSeller refurbished`                        4.822e-01
## idseq.my                                                  7.051e-04
## D.ratio.sum.TfIdf.nwrds                                   7.365e-01
## D.TfIdf.sum.stem.stop.Ratio                               2.512e+01
## D.npnct15.log                                             5.708e+03
## D.npnct03.log                                             2.619e+00
## startprice.diff                                           1.712e-03
## biddable                                                  3.024e-01
## cellular.fctr1                                            3.301e-01
## cellular.fctrUnknown                                      7.642e-01
## carrier.fctrNone                                                 NA
## carrier.fctrOther                                         2.599e+03
## carrier.fctrSprint                                        7.871e-01
## `carrier.fctrT-Mobile`                                    1.133e+00
## carrier.fctrUnknown                                       5.355e-01
## carrier.fctrVerizon                                       5.132e-01
## `prdline.my.fctriPad 1:idseq.my`                          8.327e-04
## `prdline.my.fctriPad 2:idseq.my`                          9.582e-04
## `prdline.my.fctriPad 3+:idseq.my`                         7.981e-04
## `prdline.my.fctriPadAir:idseq.my`                         8.234e-04
## `prdline.my.fctriPadmini:idseq.my`                        8.062e-04
## `prdline.my.fctriPadmini 2+:idseq.my`                     8.780e-04
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`           7.972e-01
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`           1.442e+00
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`          7.174e-01
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`          8.504e-01
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`         1.166e+00
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`      1.830e+00
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`       1.494e+01
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`       1.805e+01
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`      1.125e+01
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`      1.503e+01
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`     1.248e+01
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio`  1.452e+01
## `prdline.my.fctriPad 1:D.npnct15.log`                     6.230e+03
## `prdline.my.fctriPad 2:D.npnct15.log`                     8.072e+03
## `prdline.my.fctriPad 3+:D.npnct15.log`                    5.708e+03
## `prdline.my.fctriPadAir:D.npnct15.log`                    8.072e+03
## `prdline.my.fctriPadmini:D.npnct15.log`                   6.792e+03
## `prdline.my.fctriPadmini 2+:D.npnct15.log`                       NA
## `prdline.my.fctriPad 1:D.npnct03.log`                     5.983e+00
## `prdline.my.fctriPad 2:D.npnct03.log`                     2.885e+00
## `prdline.my.fctriPad 3+:D.npnct03.log`                    3.917e+03
## `prdline.my.fctriPadAir:D.npnct03.log`                    2.928e+00
## `prdline.my.fctriPadmini:D.npnct03.log`                          NA
## `prdline.my.fctriPadmini 2+:D.npnct03.log`                       NA
## `startprice.diff:biddable`                                2.662e-03
## `cellular.fctr1:carrier.fctrNone`                                NA
## `cellular.fctrUnknown:carrier.fctrNone`                          NA
## `cellular.fctr1:carrier.fctrOther`                               NA
## `cellular.fctrUnknown:carrier.fctrOther`                         NA
## `cellular.fctr1:carrier.fctrSprint`                              NA
## `cellular.fctrUnknown:carrier.fctrSprint`                        NA
## `cellular.fctr1:carrier.fctrT-Mobile`                            NA
## `cellular.fctrUnknown:carrier.fctrT-Mobile`                      NA
## `cellular.fctr1:carrier.fctrUnknown`                             NA
## `cellular.fctrUnknown:carrier.fctrUnknown`                       NA
## `cellular.fctr1:carrier.fctrVerizon`                             NA
## `cellular.fctrUnknown:carrier.fctrVerizon`                       NA
## `prdline.my.fctrUnknown:.clusterid.fctr2`                 9.147e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`                  1.041e+00
## `prdline.my.fctriPad 2:.clusterid.fctr2`                  1.407e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                 8.357e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                 8.549e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                1.099e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`             1.320e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                 1.291e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                  1.223e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                  1.899e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                 1.062e+00
## `prdline.my.fctriPadAir:.clusterid.fctr3`                 1.174e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`                1.262e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`             1.462e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                        NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                  1.211e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                  1.918e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                 1.025e+00
## `prdline.my.fctriPadAir:.clusterid.fctr4`                        NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`                1.169e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                    NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                        NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                 1.080e+00
## `prdline.my.fctriPadAir:.clusterid.fctr5`                        NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                1.615e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                    NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`                        NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`                         NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`                         NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                 1.585e+03
## `prdline.my.fctriPadAir:.clusterid.fctr6`                        NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`                       NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                    NA
##                                                          z value Pr(>|z|)
## (Intercept)                                               -0.655 0.512726
## D.ratio.nstopwrds.nwrds                                   -2.171 0.029904
## D.terms.n.stem.stop.Ratio                                  0.657 0.511389
## .rnorm                                                     0.294 0.768804
## D.npnct01.log                                              0.860 0.389530
## storage.fctr16                                            -0.656 0.511595
## storage.fctr32                                            -0.888 0.374468
## storage.fctr64                                             0.158 0.874182
## storage.fctrUnknown                                       -0.996 0.319309
## D.npnct11.log                                             -0.078 0.938127
## D.npnct10.log                                             -0.013 0.989546
## D.TfIdf.sum.post.stop                                     -0.205 0.837780
## D.npnct13.log                                              0.322 0.747643
## D.TfIdf.sum.post.stem                                      0.227 0.820294
## D.sum.TfIdf                                                   NA       NA
## color.fctrGold                                            -0.537 0.591090
## `color.fctrSpace Gray`                                    -1.505 0.132325
## color.fctrUnknown                                         -1.032 0.302064
## color.fctrWhite                                           -1.354 0.175815
## D.npnct08.log                                              0.792 0.428178
## `prdline.my.fctriPad 1`                                    2.004 0.045090
## `prdline.my.fctriPad 2`                                    1.258 0.208233
## `prdline.my.fctriPad 3+`                                   1.578 0.114634
## prdline.my.fctriPadAir                                     0.614 0.539090
## prdline.my.fctriPadmini                                    0.872 0.383120
## `prdline.my.fctriPadmini 2+`                               1.112 0.266066
## D.nstopwrds.log                                            2.009 0.044569
## D.npnct16.log                                              1.129 0.258895
## D.npnct24.log                                             -0.854 0.392895
## D.npnct06.log                                             -2.203 0.027598
## D.npnct28.log                                             -0.001 0.999272
## D.nuppr.log                                                0.332 0.740263
## D.nchrs.log                                               -0.414 0.678760
## D.nwrds.log                                               -0.152 0.879336
## D.npnct12.log                                              0.738 0.460774
## D.npnct09.log                                             -0.007 0.994211
## D.ndgts.log                                                0.770 0.441319
## D.nwrds.unq.log                                           -0.676 0.499075
## D.terms.n.post.stem.log                                       NA       NA
## D.terms.n.post.stop.log                                    0.675 0.499868
## D.npnct14.log                                             -2.014 0.043998
## D.terms.n.post.stem                                        0.927 0.354160
## D.terms.n.post.stop                                       -0.938 0.348365
## D.npnct05.log                                             -1.719 0.085622
## `condition.fctrFor parts or not working`                  -0.552 0.581264
## `condition.fctrManufacturer refurbished`                   0.518 0.604718
## condition.fctrNew                                         -0.556 0.578103
## `condition.fctrNew other (see details)`                    0.192 0.847830
## `condition.fctrSeller refurbished`                        -1.178 0.238755
## idseq.my                                                   1.153 0.248872
## D.ratio.sum.TfIdf.nwrds                                    0.640 0.522079
## D.TfIdf.sum.stem.stop.Ratio                                0.331 0.740433
## D.npnct15.log                                             -0.004 0.997058
## D.npnct03.log                                              0.165 0.868749
## startprice.diff                                           -3.505 0.000457
## biddable                                                  13.166  < 2e-16
## cellular.fctr1                                             0.009 0.992957
## cellular.fctrUnknown                                      -0.551 0.581806
## carrier.fctrNone                                              NA       NA
## carrier.fctrOther                                          0.006 0.995313
## carrier.fctrSprint                                         1.522 0.128013
## `carrier.fctrT-Mobile`                                    -1.004 0.315230
## carrier.fctrUnknown                                        0.145 0.884989
## carrier.fctrVerizon                                        0.850 0.395308
## `prdline.my.fctriPad 1:idseq.my`                          -0.479 0.631875
## `prdline.my.fctriPad 2:idseq.my`                          -1.513 0.130227
## `prdline.my.fctriPad 3+:idseq.my`                         -0.756 0.449659
## `prdline.my.fctriPadAir:idseq.my`                         -2.280 0.022621
## `prdline.my.fctriPadmini:idseq.my`                        -1.166 0.243814
## `prdline.my.fctriPadmini 2+:idseq.my`                     -0.529 0.596488
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`           -0.801 0.423368
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`            1.336 0.181672
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`          -0.545 0.585435
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`          -1.678 0.093345
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`         -1.146 0.251898
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`      -1.398 0.161984
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`       -1.909 0.056296
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`       -1.182 0.237164
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`      -1.449 0.147284
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`      -0.406 0.684800
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`     -0.751 0.452731
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio`  -1.011 0.312059
## `prdline.my.fctriPad 1:D.npnct15.log`                      0.007 0.994751
## `prdline.my.fctriPad 2:D.npnct15.log`                      0.005 0.995728
## `prdline.my.fctriPad 3+:D.npnct15.log`                     0.003 0.997272
## `prdline.my.fctriPadAir:D.npnct15.log`                     0.005 0.995663
## `prdline.my.fctriPadmini:D.npnct15.log`                    0.007 0.994331
## `prdline.my.fctriPadmini 2+:D.npnct15.log`                    NA       NA
## `prdline.my.fctriPad 1:D.npnct03.log`                      0.420 0.674298
## `prdline.my.fctriPad 2:D.npnct03.log`                      0.043 0.965667
## `prdline.my.fctriPad 3+:D.npnct03.log`                    -0.005 0.996040
## `prdline.my.fctriPadAir:D.npnct03.log`                     0.455 0.648794
## `prdline.my.fctriPadmini:D.npnct03.log`                       NA       NA
## `prdline.my.fctriPadmini 2+:D.npnct03.log`                    NA       NA
## `startprice.diff:biddable`                                -5.303 1.14e-07
## `cellular.fctr1:carrier.fctrNone`                             NA       NA
## `cellular.fctrUnknown:carrier.fctrNone`                       NA       NA
## `cellular.fctr1:carrier.fctrOther`                            NA       NA
## `cellular.fctrUnknown:carrier.fctrOther`                      NA       NA
## `cellular.fctr1:carrier.fctrSprint`                           NA       NA
## `cellular.fctrUnknown:carrier.fctrSprint`                     NA       NA
## `cellular.fctr1:carrier.fctrT-Mobile`                         NA       NA
## `cellular.fctrUnknown:carrier.fctrT-Mobile`                   NA       NA
## `cellular.fctr1:carrier.fctrUnknown`                          NA       NA
## `cellular.fctrUnknown:carrier.fctrUnknown`                    NA       NA
## `cellular.fctr1:carrier.fctrVerizon`                          NA       NA
## `cellular.fctrUnknown:carrier.fctrVerizon`                    NA       NA
## `prdline.my.fctrUnknown:.clusterid.fctr2`                  2.148 0.031750
## `prdline.my.fctriPad 1:.clusterid.fctr2`                  -0.117 0.906500
## `prdline.my.fctriPad 2:.clusterid.fctr2`                  -0.647 0.517754
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                 -0.308 0.757943
## `prdline.my.fctriPadAir:.clusterid.fctr2`                  0.124 0.900966
## `prdline.my.fctriPadmini:.clusterid.fctr2`                 1.398 0.162259
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`              0.265 0.791022
## `prdline.my.fctrUnknown:.clusterid.fctr3`                 -0.718 0.472599
## `prdline.my.fctriPad 1:.clusterid.fctr3`                  -0.994 0.320019
## `prdline.my.fctriPad 2:.clusterid.fctr3`                  -1.033 0.301408
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                 -0.597 0.550841
## `prdline.my.fctriPadAir:.clusterid.fctr3`                  0.161 0.872212
## `prdline.my.fctriPadmini:.clusterid.fctr3`                 1.165 0.244000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`              0.886 0.375655
## `prdline.my.fctrUnknown:.clusterid.fctr4`                     NA       NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                  -1.794 0.072813
## `prdline.my.fctriPad 2:.clusterid.fctr4`                   0.917 0.358935
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                  0.437 0.662164
## `prdline.my.fctriPadAir:.clusterid.fctr4`                     NA       NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`                -0.001 0.999406
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                 NA       NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                     NA       NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                      NA       NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                      NA       NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                 -1.366 0.171922
## `prdline.my.fctriPadAir:.clusterid.fctr5`                     NA       NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                -0.443 0.657530
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                 NA       NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`                     NA       NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`                      NA       NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`                      NA       NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                 -0.010 0.991806
## `prdline.my.fctriPadAir:.clusterid.fctr6`                     NA       NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`                    NA       NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                 NA       NA
##                                                             
## (Intercept)                                                 
## D.ratio.nstopwrds.nwrds                                  *  
## D.terms.n.stem.stop.Ratio                                   
## .rnorm                                                      
## D.npnct01.log                                               
## storage.fctr16                                              
## storage.fctr32                                              
## storage.fctr64                                              
## storage.fctrUnknown                                         
## D.npnct11.log                                               
## D.npnct10.log                                               
## D.TfIdf.sum.post.stop                                       
## D.npnct13.log                                               
## D.TfIdf.sum.post.stem                                       
## D.sum.TfIdf                                                 
## color.fctrGold                                              
## `color.fctrSpace Gray`                                      
## color.fctrUnknown                                           
## color.fctrWhite                                             
## D.npnct08.log                                               
## `prdline.my.fctriPad 1`                                  *  
## `prdline.my.fctriPad 2`                                     
## `prdline.my.fctriPad 3+`                                    
## prdline.my.fctriPadAir                                      
## prdline.my.fctriPadmini                                     
## `prdline.my.fctriPadmini 2+`                                
## D.nstopwrds.log                                          *  
## D.npnct16.log                                               
## D.npnct24.log                                               
## D.npnct06.log                                            *  
## D.npnct28.log                                               
## D.nuppr.log                                                 
## D.nchrs.log                                                 
## D.nwrds.log                                                 
## D.npnct12.log                                               
## D.npnct09.log                                               
## D.ndgts.log                                                 
## D.nwrds.unq.log                                             
## D.terms.n.post.stem.log                                     
## D.terms.n.post.stop.log                                     
## D.npnct14.log                                            *  
## D.terms.n.post.stem                                         
## D.terms.n.post.stop                                         
## D.npnct05.log                                            .  
## `condition.fctrFor parts or not working`                    
## `condition.fctrManufacturer refurbished`                    
## condition.fctrNew                                           
## `condition.fctrNew other (see details)`                     
## `condition.fctrSeller refurbished`                          
## idseq.my                                                    
## D.ratio.sum.TfIdf.nwrds                                     
## D.TfIdf.sum.stem.stop.Ratio                                 
## D.npnct15.log                                               
## D.npnct03.log                                               
## startprice.diff                                          ***
## biddable                                                 ***
## cellular.fctr1                                              
## cellular.fctrUnknown                                        
## carrier.fctrNone                                            
## carrier.fctrOther                                           
## carrier.fctrSprint                                          
## `carrier.fctrT-Mobile`                                      
## carrier.fctrUnknown                                         
## carrier.fctrVerizon                                         
## `prdline.my.fctriPad 1:idseq.my`                            
## `prdline.my.fctriPad 2:idseq.my`                            
## `prdline.my.fctriPad 3+:idseq.my`                           
## `prdline.my.fctriPadAir:idseq.my`                        *  
## `prdline.my.fctriPadmini:idseq.my`                          
## `prdline.my.fctriPadmini 2+:idseq.my`                       
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`             
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`             
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`            
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`         .  
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`           
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`        
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`      .  
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`         
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`        
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`        
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`       
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio`    
## `prdline.my.fctriPad 1:D.npnct15.log`                       
## `prdline.my.fctriPad 2:D.npnct15.log`                       
## `prdline.my.fctriPad 3+:D.npnct15.log`                      
## `prdline.my.fctriPadAir:D.npnct15.log`                      
## `prdline.my.fctriPadmini:D.npnct15.log`                     
## `prdline.my.fctriPadmini 2+:D.npnct15.log`                  
## `prdline.my.fctriPad 1:D.npnct03.log`                       
## `prdline.my.fctriPad 2:D.npnct03.log`                       
## `prdline.my.fctriPad 3+:D.npnct03.log`                      
## `prdline.my.fctriPadAir:D.npnct03.log`                      
## `prdline.my.fctriPadmini:D.npnct03.log`                     
## `prdline.my.fctriPadmini 2+:D.npnct03.log`                  
## `startprice.diff:biddable`                               ***
## `cellular.fctr1:carrier.fctrNone`                           
## `cellular.fctrUnknown:carrier.fctrNone`                     
## `cellular.fctr1:carrier.fctrOther`                          
## `cellular.fctrUnknown:carrier.fctrOther`                    
## `cellular.fctr1:carrier.fctrSprint`                         
## `cellular.fctrUnknown:carrier.fctrSprint`                   
## `cellular.fctr1:carrier.fctrT-Mobile`                       
## `cellular.fctrUnknown:carrier.fctrT-Mobile`                 
## `cellular.fctr1:carrier.fctrUnknown`                        
## `cellular.fctrUnknown:carrier.fctrUnknown`                  
## `cellular.fctr1:carrier.fctrVerizon`                        
## `cellular.fctrUnknown:carrier.fctrVerizon`                  
## `prdline.my.fctrUnknown:.clusterid.fctr2`                *  
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
## `prdline.my.fctriPad 1:.clusterid.fctr4`                 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`                    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                   
## `prdline.my.fctriPadAir:.clusterid.fctr4`                   
## `prdline.my.fctriPadmini:.clusterid.fctr4`                  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`               
## `prdline.my.fctrUnknown:.clusterid.fctr5`                   
## `prdline.my.fctriPad 1:.clusterid.fctr5`                    
## `prdline.my.fctriPad 2:.clusterid.fctr5`                    
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                   
## `prdline.my.fctriPadAir:.clusterid.fctr5`                   
## `prdline.my.fctriPadmini:.clusterid.fctr5`                  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`               
## `prdline.my.fctrUnknown:.clusterid.fctr6`                   
## `prdline.my.fctriPad 1:.clusterid.fctr6`                    
## `prdline.my.fctriPad 2:.clusterid.fctr6`                    
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                   
## `prdline.my.fctriPadAir:.clusterid.fctr6`                   
## `prdline.my.fctriPadmini:.clusterid.fctr6`                  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`               
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  722.93  on 864  degrees of freedom
## AIC: 942.93
## 
## Number of Fisher Scoring iterations: 16
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_1-34.png) ![](ebayipads_color_files/figure-html/fit.models_1-35.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7130154
## 3        0.2 0.7655236
## 4        0.3 0.8033299
## 5        0.4 0.8219485
## 6        0.5 0.8135991
## 7        0.6 0.8199513
## 8        0.7 0.7979670
## 9        0.8 0.7550201
## 10       0.9 0.6359584
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.glm.N
## 1         N                                    448
## 2         Y                                     83
##   sold.fctr.predict.All.Interact.X.glm.Y
## 1                                     76
## 2                                    367
##          Prediction
## Reference   N   Y
##         N 448  76
##         Y  83 367
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.367556e-01   6.712547e-01   8.120210e-01   8.594339e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.413692e-86   6.341948e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_1-36.png) ![](ebayipads_color_files/figure-html/fit.models_1-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6936937
## 3        0.2 0.7303253
## 4        0.3 0.7384259
## 5        0.4 0.7487685
## 6        0.5 0.7573813
## 7        0.6 0.7526316
## 8        0.7 0.7352538
## 9        0.8 0.7146974
## 10       0.9 0.6096774
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-38.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.glm.N
## 1         N                                    401
## 2         Y                                    115
##   sold.fctr.predict.All.Interact.X.glm.Y
## 1                                     74
## 2                                    295
##          Prediction
## Reference   N   Y
##         N 401  74
##         Y 115 295
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.864407e-01   5.676063e-01   7.579436e-01   8.130160e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   5.826387e-54   3.619242e-03 
##             model_id model_method
## 1 All.Interact.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.972                 0.471
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9076718                    0.4       0.8219485        0.7494967
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.812021             0.8594339     0.4949703   0.8344904
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7573813        0.7864407
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7579436              0.813016     0.5676063    942.9274
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01251751      0.02857686
##                   label step_major step_minor     bgn     end elapsed
## 7      fit.models_1_glm          7          0 225.897 231.893   5.996
## 8 fit.models_1_bayesglm          8          0 231.894      NA      NA
## [1] "fitting model: All.Interact.X.bayesglm"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8981  -0.6415  -0.1496   0.4737   2.6704  
## 
## Coefficients:
##                                                            Estimate
## (Intercept)                                               7.111e+00
## D.ratio.nstopwrds.nwrds                                  -1.783e+00
## D.terms.n.stem.stop.Ratio                                -6.355e+00
## .rnorm                                                    3.703e-02
## D.npnct01.log                                             5.371e-01
## storage.fctr16                                           -2.814e-01
## storage.fctr32                                           -4.732e-01
## storage.fctr64                                            1.544e-01
## storage.fctrUnknown                                      -5.737e-01
## D.npnct11.log                                            -2.736e-02
## D.npnct10.log                                            -7.845e+00
## D.TfIdf.sum.post.stop                                     4.921e-02
## D.npnct13.log                                             1.136e-01
## D.TfIdf.sum.post.stem                                     4.375e-02
## D.sum.TfIdf                                               4.375e-02
## color.fctrGold                                           -2.026e-01
## `color.fctrSpace Gray`                                   -5.641e-01
## color.fctrUnknown                                        -2.446e-01
## color.fctrWhite                                          -4.131e-01
## D.npnct08.log                                             4.873e-01
## `prdline.my.fctriPad 1`                                   8.426e-01
## `prdline.my.fctriPad 2`                                   3.475e-01
## `prdline.my.fctriPad 3+`                                  3.730e-01
## prdline.my.fctriPadAir                                    6.439e-01
## prdline.my.fctriPadmini                                   2.667e-01
## `prdline.my.fctriPadmini 2+`                              2.414e-01
## D.nstopwrds.log                                           7.109e-01
## D.npnct16.log                                             2.272e+00
## D.npnct24.log                                            -4.275e-01
## D.npnct06.log                                            -4.614e+00
## D.npnct28.log                                            -5.560e-02
## D.nuppr.log                                              -1.436e-02
## D.nchrs.log                                              -3.958e-02
## D.nwrds.log                                               5.699e-02
## D.npnct12.log                                             3.281e-01
## D.npnct09.log                                            -1.700e+00
## D.ndgts.log                                               6.857e-01
## D.nwrds.unq.log                                          -1.906e-01
## D.terms.n.post.stem.log                                  -1.906e-01
## D.terms.n.post.stop.log                                  -1.874e-01
## D.npnct14.log                                            -2.228e+00
## D.terms.n.post.stem                                      -9.356e-02
## D.terms.n.post.stop                                      -1.261e-01
## D.npnct05.log                                            -2.825e+00
## `condition.fctrFor parts or not working`                 -4.457e-02
## `condition.fctrManufacturer refurbished`                  3.741e-01
## condition.fctrNew                                        -1.669e-01
## `condition.fctrNew other (see details)`                   8.592e-02
## `condition.fctrSeller refurbished`                       -3.657e-01
## idseq.my                                                  2.303e-04
## D.ratio.sum.TfIdf.nwrds                                   3.642e-01
## D.TfIdf.sum.stem.stop.Ratio                              -5.954e-01
## D.npnct15.log                                             3.793e+00
## D.npnct03.log                                            -6.694e-01
## startprice.diff                                          -5.580e-03
## biddable                                                  3.728e+00
## cellular.fctr1                                            1.087e-01
## cellular.fctrUnknown                                     -1.398e-01
## carrier.fctrNone                                          1.083e-02
## carrier.fctrOther                                         4.667e-01
## carrier.fctrSprint                                        4.891e-01
## `carrier.fctrT-Mobile`                                   -4.768e-01
## carrier.fctrUnknown                                      -1.074e-01
## carrier.fctrVerizon                                       1.681e-01
## `prdline.my.fctriPad 1:idseq.my`                          1.399e-04
## `prdline.my.fctriPad 2:idseq.my`                         -8.042e-04
## `prdline.my.fctriPad 3+:idseq.my`                        -1.500e-04
## `prdline.my.fctriPadAir:idseq.my`                        -1.249e-03
## `prdline.my.fctriPadmini:idseq.my`                       -3.985e-04
## `prdline.my.fctriPadmini 2+:idseq.my`                     7.419e-05
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`          -7.461e-01
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`           2.078e+00
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`         -2.760e-01
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`         -1.020e+00
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`        -7.400e-01
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`     -1.810e+00
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`      -7.521e-02
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`       3.870e-01
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`      4.472e-01
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`      1.626e+00
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`     5.434e-01
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio`  4.710e-01
## `prdline.my.fctriPad 1:D.npnct15.log`                     2.428e+00
## `prdline.my.fctriPad 2:D.npnct15.log`                     1.354e-01
## `prdline.my.fctriPad 3+:D.npnct15.log`                   -4.314e+00
## `prdline.my.fctriPadAir:D.npnct15.log`                    1.889e-01
## `prdline.my.fctriPadmini:D.npnct15.log`                   1.275e+00
## `prdline.my.fctriPadmini 2+:D.npnct15.log`               -1.006e+00
## `prdline.my.fctriPad 1:D.npnct03.log`                     2.468e+00
## `prdline.my.fctriPad 2:D.npnct03.log`                     9.611e-01
## `prdline.my.fctriPad 3+:D.npnct03.log`                   -3.062e-01
## `prdline.my.fctriPadAir:D.npnct03.log`                    9.216e-01
## `prdline.my.fctriPadmini:D.npnct03.log`                   7.660e-01
## `prdline.my.fctriPadmini 2+:D.npnct03.log`               -1.006e+00
## `startprice.diff:biddable`                               -1.300e-02
## `cellular.fctr1:carrier.fctrNone`                         0.000e+00
## `cellular.fctrUnknown:carrier.fctrNone`                   0.000e+00
## `cellular.fctr1:carrier.fctrOther`                        4.667e-01
## `cellular.fctrUnknown:carrier.fctrOther`                  0.000e+00
## `cellular.fctr1:carrier.fctrSprint`                       4.891e-01
## `cellular.fctrUnknown:carrier.fctrSprint`                 0.000e+00
## `cellular.fctr1:carrier.fctrT-Mobile`                    -4.768e-01
## `cellular.fctrUnknown:carrier.fctrT-Mobile`               0.000e+00
## `cellular.fctr1:carrier.fctrUnknown`                      1.272e-02
## `cellular.fctrUnknown:carrier.fctrUnknown`               -1.398e-01
## `cellular.fctr1:carrier.fctrVerizon`                      1.681e-01
## `cellular.fctrUnknown:carrier.fctrVerizon`                0.000e+00
## `prdline.my.fctrUnknown:.clusterid.fctr2`                 1.348e+00
## `prdline.my.fctriPad 1:.clusterid.fctr2`                  6.371e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`                 -3.081e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                -6.389e-02
## `prdline.my.fctriPadAir:.clusterid.fctr2`                -1.179e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                9.660e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`             3.715e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`                -1.112e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                 -1.297e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`                 -1.301e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                -3.820e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                -1.871e-02
## `prdline.my.fctriPadmini:.clusterid.fctr3`                9.504e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`             7.684e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`                 0.000e+00
## `prdline.my.fctriPad 1:.clusterid.fctr4`                 -1.553e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                  1.595e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                 5.120e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`                 0.000e+00
## `prdline.my.fctriPadmini:.clusterid.fctr4`               -2.224e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`             0.000e+00
## `prdline.my.fctrUnknown:.clusterid.fctr5`                 0.000e+00
## `prdline.my.fctriPad 1:.clusterid.fctr5`                  0.000e+00
## `prdline.my.fctriPad 2:.clusterid.fctr5`                  0.000e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                -8.268e-01
## `prdline.my.fctriPadAir:.clusterid.fctr5`                 0.000e+00
## `prdline.my.fctriPadmini:.clusterid.fctr5`               -9.056e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`             0.000e+00
## `prdline.my.fctrUnknown:.clusterid.fctr6`                 0.000e+00
## `prdline.my.fctriPad 1:.clusterid.fctr6`                  0.000e+00
## `prdline.my.fctriPad 2:.clusterid.fctr6`                  0.000e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                -1.331e+00
## `prdline.my.fctriPadAir:.clusterid.fctr6`                 0.000e+00
## `prdline.my.fctriPadmini:.clusterid.fctr6`                0.000e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`             0.000e+00
##                                                          Std. Error
## (Intercept)                                               6.841e+00
## D.ratio.nstopwrds.nwrds                                   2.452e+00
## D.terms.n.stem.stop.Ratio                                 5.468e+00
## .rnorm                                                    9.983e-02
## D.npnct01.log                                             5.613e-01
## storage.fctr16                                            4.578e-01
## storage.fctr32                                            4.905e-01
## storage.fctr64                                            4.772e-01
## storage.fctrUnknown                                       6.454e-01
## D.npnct11.log                                             3.566e-01
## D.npnct10.log                                             7.512e+00
## D.TfIdf.sum.post.stop                                     2.935e-01
## D.npnct13.log                                             3.617e-01
## D.TfIdf.sum.post.stem                                     3.084e-01
## D.sum.TfIdf                                               3.084e-01
## color.fctrGold                                            5.303e-01
## `color.fctrSpace Gray`                                    3.776e-01
## color.fctrUnknown                                         2.633e-01
## color.fctrWhite                                           2.853e-01
## D.npnct08.log                                             7.186e-01
## `prdline.my.fctriPad 1`                                   1.784e+00
## `prdline.my.fctriPad 2`                                   1.792e+00
## `prdline.my.fctriPad 3+`                                  1.632e+00
## prdline.my.fctriPadAir                                    1.722e+00
## prdline.my.fctriPadmini                                   1.684e+00
## `prdline.my.fctriPadmini 2+`                              1.803e+00
## D.nstopwrds.log                                           6.657e-01
## D.npnct16.log                                             1.593e+00
## D.npnct24.log                                             2.490e+00
## D.npnct06.log                                             1.936e+00
## D.npnct28.log                                             2.186e+00
## D.nuppr.log                                               4.879e-01
## D.nchrs.log                                               4.799e-01
## D.nwrds.log                                               7.819e-01
## D.npnct12.log                                             7.356e-01
## D.npnct09.log                                             4.975e+00
## D.ndgts.log                                               4.413e-01
## D.nwrds.unq.log                                           1.034e+00
## D.terms.n.post.stem.log                                   1.034e+00
## D.terms.n.post.stop.log                                   1.030e+00
## D.npnct14.log                                             1.134e+00
## D.terms.n.post.stem                                       2.089e-01
## D.terms.n.post.stop                                       2.082e-01
## D.npnct05.log                                             1.492e+00
## `condition.fctrFor parts or not working`                  4.034e-01
## `condition.fctrManufacturer refurbished`                  6.009e-01
## condition.fctrNew                                         3.216e-01
## `condition.fctrNew other (see details)`                   4.592e-01
## `condition.fctrSeller refurbished`                        4.299e-01
## idseq.my                                                  5.160e-04
## D.ratio.sum.TfIdf.nwrds                                   4.764e-01
## D.TfIdf.sum.stem.stop.Ratio                               3.650e+00
## D.npnct15.log                                             3.401e+00
## D.npnct03.log                                             3.212e+00
## startprice.diff                                           1.583e-03
## biddable                                                  2.737e-01
## cellular.fctr1                                            1.314e+00
## cellular.fctrUnknown                                      1.752e+00
## carrier.fctrNone                                          1.314e+00
## carrier.fctrOther                                         2.032e+00
## carrier.fctrSprint                                        1.530e+00
## `carrier.fctrT-Mobile`                                    1.577e+00
## carrier.fctrUnknown                                       1.327e+00
## carrier.fctrVerizon                                       1.472e+00
## `prdline.my.fctriPad 1:idseq.my`                          6.577e-04
## `prdline.my.fctriPad 2:idseq.my`                          7.711e-04
## `prdline.my.fctriPad 3+:idseq.my`                         6.222e-04
## `prdline.my.fctriPadAir:idseq.my`                         6.377e-04
## `prdline.my.fctriPadmini:idseq.my`                        6.322e-04
## `prdline.my.fctriPadmini 2+:idseq.my`                     6.986e-04
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`           5.151e-01
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`           1.259e+00
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`          6.444e-01
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`          7.104e-01
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`         1.005e+00
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`      1.392e+00
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`       1.844e+00
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`       1.858e+00
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`      1.677e+00
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`      1.765e+00
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`     1.741e+00
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio`  1.897e+00
## `prdline.my.fctriPad 1:D.npnct15.log`                     1.023e+01
## `prdline.my.fctriPad 2:D.npnct15.log`                     3.345e+00
## `prdline.my.fctriPad 3+:D.npnct15.log`                    3.760e+00
## `prdline.my.fctriPadAir:D.npnct15.log`                    3.267e+00
## `prdline.my.fctriPadmini:D.npnct15.log`                   2.978e+00
## `prdline.my.fctriPadmini 2+:D.npnct15.log`                3.152e+00
## `prdline.my.fctriPad 1:D.npnct03.log`                     5.026e+00
## `prdline.my.fctriPad 2:D.npnct03.log`                     3.684e+00
## `prdline.my.fctriPad 3+:D.npnct03.log`                    3.129e+00
## `prdline.my.fctriPadAir:D.npnct03.log`                    2.710e+00
## `prdline.my.fctriPadmini:D.npnct03.log`                   3.317e+00
## `prdline.my.fctriPadmini 2+:D.npnct03.log`                3.152e+00
## `startprice.diff:biddable`                                2.472e-03
## `cellular.fctr1:carrier.fctrNone`                         2.500e+00
## `cellular.fctrUnknown:carrier.fctrNone`                   2.500e+00
## `cellular.fctr1:carrier.fctrOther`                        2.032e+00
## `cellular.fctrUnknown:carrier.fctrOther`                  2.500e+00
## `cellular.fctr1:carrier.fctrSprint`                       1.530e+00
## `cellular.fctrUnknown:carrier.fctrSprint`                 2.500e+00
## `cellular.fctr1:carrier.fctrT-Mobile`                     1.577e+00
## `cellular.fctrUnknown:carrier.fctrT-Mobile`               2.500e+00
## `cellular.fctr1:carrier.fctrUnknown`                      1.337e+00
## `cellular.fctrUnknown:carrier.fctrUnknown`                1.752e+00
## `cellular.fctr1:carrier.fctrVerizon`                      1.472e+00
## `cellular.fctrUnknown:carrier.fctrVerizon`                2.500e+00
## `prdline.my.fctrUnknown:.clusterid.fctr2`                 7.145e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`                  7.515e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`                  8.253e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                 6.553e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                 6.361e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                7.779e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`             9.452e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`                 1.015e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                  8.201e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`                  1.150e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                 8.375e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                 9.088e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`                8.744e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`             1.090e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                 2.500e+00
## `prdline.my.fctriPad 1:.clusterid.fctr4`                  8.896e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`                  1.265e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                 8.071e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`                 2.500e+00
## `prdline.my.fctriPadmini:.clusterid.fctr4`                8.756e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`             2.500e+00
## `prdline.my.fctrUnknown:.clusterid.fctr5`                 2.500e+00
## `prdline.my.fctriPad 1:.clusterid.fctr5`                  2.500e+00
## `prdline.my.fctriPad 2:.clusterid.fctr5`                  2.500e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                 8.454e-01
## `prdline.my.fctriPadAir:.clusterid.fctr5`                 2.500e+00
## `prdline.my.fctriPadmini:.clusterid.fctr5`                1.135e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`             2.500e+00
## `prdline.my.fctrUnknown:.clusterid.fctr6`                 2.500e+00
## `prdline.my.fctriPad 1:.clusterid.fctr6`                  2.500e+00
## `prdline.my.fctriPad 2:.clusterid.fctr6`                  2.500e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                 1.561e+00
## `prdline.my.fctriPadAir:.clusterid.fctr6`                 2.500e+00
## `prdline.my.fctriPadmini:.clusterid.fctr6`                2.500e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`             2.500e+00
##                                                          z value Pr(>|z|)
## (Intercept)                                                1.039 0.298582
## D.ratio.nstopwrds.nwrds                                   -0.727 0.467194
## D.terms.n.stem.stop.Ratio                                 -1.162 0.245150
## .rnorm                                                     0.371 0.710729
## D.npnct01.log                                              0.957 0.338657
## storage.fctr16                                            -0.615 0.538789
## storage.fctr32                                            -0.965 0.334764
## storage.fctr64                                             0.324 0.746300
## storage.fctrUnknown                                       -0.889 0.374026
## D.npnct11.log                                             -0.077 0.938838
## D.npnct10.log                                             -1.044 0.296318
## D.TfIdf.sum.post.stop                                      0.168 0.866874
## D.npnct13.log                                              0.314 0.753360
## D.TfIdf.sum.post.stem                                      0.142 0.887197
## D.sum.TfIdf                                                0.142 0.887197
## color.fctrGold                                            -0.382 0.702403
## `color.fctrSpace Gray`                                    -1.494 0.135153
## color.fctrUnknown                                         -0.929 0.353072
## color.fctrWhite                                           -1.448 0.147554
## D.npnct08.log                                              0.678 0.497695
## `prdline.my.fctriPad 1`                                    0.472 0.636739
## `prdline.my.fctriPad 2`                                    0.194 0.846236
## `prdline.my.fctriPad 3+`                                   0.229 0.819191
## prdline.my.fctriPadAir                                     0.374 0.708515
## prdline.my.fctriPadmini                                    0.158 0.874189
## `prdline.my.fctriPadmini 2+`                               0.134 0.893462
## D.nstopwrds.log                                            1.068 0.285611
## D.npnct16.log                                              1.426 0.153746
## D.npnct24.log                                             -0.172 0.863652
## D.npnct06.log                                             -2.383 0.017194
## D.npnct28.log                                             -0.025 0.979705
## D.nuppr.log                                               -0.029 0.976514
## D.nchrs.log                                               -0.082 0.934264
## D.nwrds.log                                                0.073 0.941902
## D.npnct12.log                                              0.446 0.655618
## D.npnct09.log                                             -0.342 0.732553
## D.ndgts.log                                                1.554 0.120255
## D.nwrds.unq.log                                           -0.184 0.853728
## D.terms.n.post.stem.log                                   -0.184 0.853728
## D.terms.n.post.stop.log                                   -0.182 0.855570
## D.npnct14.log                                             -1.964 0.049516
## D.terms.n.post.stem                                       -0.448 0.654296
## D.terms.n.post.stop                                       -0.606 0.544723
## D.npnct05.log                                             -1.894 0.058222
## `condition.fctrFor parts or not working`                  -0.110 0.912030
## `condition.fctrManufacturer refurbished`                   0.623 0.533537
## condition.fctrNew                                         -0.519 0.603773
## `condition.fctrNew other (see details)`                    0.187 0.851577
## `condition.fctrSeller refurbished`                        -0.851 0.394874
## idseq.my                                                   0.446 0.655375
## D.ratio.sum.TfIdf.nwrds                                    0.765 0.444561
## D.TfIdf.sum.stem.stop.Ratio                               -0.163 0.870447
## D.npnct15.log                                              1.115 0.264654
## D.npnct03.log                                             -0.208 0.834919
## startprice.diff                                           -3.524 0.000425
## biddable                                                  13.623  < 2e-16
## cellular.fctr1                                             0.083 0.934058
## cellular.fctrUnknown                                      -0.080 0.936415
## carrier.fctrNone                                           0.008 0.993421
## carrier.fctrOther                                          0.230 0.818299
## carrier.fctrSprint                                         0.320 0.749292
## `carrier.fctrT-Mobile`                                    -0.302 0.762460
## carrier.fctrUnknown                                       -0.081 0.935519
## carrier.fctrVerizon                                        0.114 0.909118
## `prdline.my.fctriPad 1:idseq.my`                           0.213 0.831592
## `prdline.my.fctriPad 2:idseq.my`                          -1.043 0.296941
## `prdline.my.fctriPad 3+:idseq.my`                         -0.241 0.809532
## `prdline.my.fctriPadAir:idseq.my`                         -1.959 0.050113
## `prdline.my.fctriPadmini:idseq.my`                        -0.630 0.528484
## `prdline.my.fctriPadmini 2+:idseq.my`                      0.106 0.915426
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`           -1.448 0.147487
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`            1.650 0.098990
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`          -0.428 0.668447
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`          -1.436 0.151009
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`         -0.736 0.461625
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`      -1.300 0.193666
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`       -0.041 0.967475
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`        0.208 0.835035
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`       0.267 0.789665
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`       0.921 0.356960
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`      0.312 0.755022
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio`   0.248 0.803961
## `prdline.my.fctriPad 1:D.npnct15.log`                      0.237 0.812410
## `prdline.my.fctriPad 2:D.npnct15.log`                      0.040 0.967698
## `prdline.my.fctriPad 3+:D.npnct15.log`                    -1.147 0.251293
## `prdline.my.fctriPadAir:D.npnct15.log`                     0.058 0.953896
## `prdline.my.fctriPadmini:D.npnct15.log`                    0.428 0.668682
## `prdline.my.fctriPadmini 2+:D.npnct15.log`                -0.319 0.749502
## `prdline.my.fctriPad 1:D.npnct03.log`                      0.491 0.623381
## `prdline.my.fctriPad 2:D.npnct03.log`                      0.261 0.794181
## `prdline.my.fctriPad 3+:D.npnct03.log`                    -0.098 0.922041
## `prdline.my.fctriPadAir:D.npnct03.log`                     0.340 0.733743
## `prdline.my.fctriPadmini:D.npnct03.log`                    0.231 0.817345
## `prdline.my.fctriPadmini 2+:D.npnct03.log`                -0.319 0.749502
## `startprice.diff:biddable`                                -5.256 1.47e-07
## `cellular.fctr1:carrier.fctrNone`                          0.000 1.000000
## `cellular.fctrUnknown:carrier.fctrNone`                    0.000 1.000000
## `cellular.fctr1:carrier.fctrOther`                         0.230 0.818299
## `cellular.fctrUnknown:carrier.fctrOther`                   0.000 1.000000
## `cellular.fctr1:carrier.fctrSprint`                        0.320 0.749292
## `cellular.fctrUnknown:carrier.fctrSprint`                  0.000 1.000000
## `cellular.fctr1:carrier.fctrT-Mobile`                     -0.302 0.762460
## `cellular.fctrUnknown:carrier.fctrT-Mobile`                0.000 1.000000
## `cellular.fctr1:carrier.fctrUnknown`                       0.010 0.992408
## `cellular.fctrUnknown:carrier.fctrUnknown`                -0.080 0.936415
## `cellular.fctr1:carrier.fctrVerizon`                       0.114 0.909118
## `cellular.fctrUnknown:carrier.fctrVerizon`                 0.000 1.000000
## `prdline.my.fctrUnknown:.clusterid.fctr2`                  1.887 0.059227
## `prdline.my.fctriPad 1:.clusterid.fctr2`                   0.848 0.396586
## `prdline.my.fctriPad 2:.clusterid.fctr2`                  -0.373 0.708915
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                 -0.097 0.922339
## `prdline.my.fctriPadAir:.clusterid.fctr2`                 -0.185 0.852927
## `prdline.my.fctriPadmini:.clusterid.fctr2`                 1.242 0.214323
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`              0.393 0.694322
## `prdline.my.fctrUnknown:.clusterid.fctr3`                 -1.096 0.273207
## `prdline.my.fctriPad 1:.clusterid.fctr3`                  -0.158 0.874335
## `prdline.my.fctriPad 2:.clusterid.fctr3`                  -1.131 0.257955
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                 -0.456 0.648277
## `prdline.my.fctriPadAir:.clusterid.fctr3`                 -0.021 0.983574
## `prdline.my.fctriPadmini:.clusterid.fctr3`                 1.087 0.277086
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`              0.705 0.480941
## `prdline.my.fctrUnknown:.clusterid.fctr4`                  0.000 1.000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                  -1.745 0.080919
## `prdline.my.fctriPad 2:.clusterid.fctr4`                   1.261 0.207307
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                  0.634 0.525848
## `prdline.my.fctriPadAir:.clusterid.fctr4`                  0.000 1.000000
## `prdline.my.fctriPadmini:.clusterid.fctr4`                -0.254 0.799519
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`              0.000 1.000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                  0.000 1.000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                   0.000 1.000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                   0.000 1.000000
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                 -0.978 0.328097
## `prdline.my.fctriPadAir:.clusterid.fctr5`                  0.000 1.000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                -0.798 0.424915
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`              0.000 1.000000
## `prdline.my.fctrUnknown:.clusterid.fctr6`                  0.000 1.000000
## `prdline.my.fctriPad 1:.clusterid.fctr6`                   0.000 1.000000
## `prdline.my.fctriPad 2:.clusterid.fctr6`                   0.000 1.000000
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                 -0.853 0.393895
## `prdline.my.fctriPadAir:.clusterid.fctr6`                  0.000 1.000000
## `prdline.my.fctriPadmini:.clusterid.fctr6`                 0.000 1.000000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`              0.000 1.000000
##                                                             
## (Intercept)                                                 
## D.ratio.nstopwrds.nwrds                                     
## D.terms.n.stem.stop.Ratio                                   
## .rnorm                                                      
## D.npnct01.log                                               
## storage.fctr16                                              
## storage.fctr32                                              
## storage.fctr64                                              
## storage.fctrUnknown                                         
## D.npnct11.log                                               
## D.npnct10.log                                               
## D.TfIdf.sum.post.stop                                       
## D.npnct13.log                                               
## D.TfIdf.sum.post.stem                                       
## D.sum.TfIdf                                                 
## color.fctrGold                                              
## `color.fctrSpace Gray`                                      
## color.fctrUnknown                                           
## color.fctrWhite                                             
## D.npnct08.log                                               
## `prdline.my.fctriPad 1`                                     
## `prdline.my.fctriPad 2`                                     
## `prdline.my.fctriPad 3+`                                    
## prdline.my.fctriPadAir                                      
## prdline.my.fctriPadmini                                     
## `prdline.my.fctriPadmini 2+`                                
## D.nstopwrds.log                                             
## D.npnct16.log                                               
## D.npnct24.log                                               
## D.npnct06.log                                            *  
## D.npnct28.log                                               
## D.nuppr.log                                                 
## D.nchrs.log                                                 
## D.nwrds.log                                                 
## D.npnct12.log                                               
## D.npnct09.log                                               
## D.ndgts.log                                                 
## D.nwrds.unq.log                                             
## D.terms.n.post.stem.log                                     
## D.terms.n.post.stop.log                                     
## D.npnct14.log                                            *  
## D.terms.n.post.stem                                         
## D.terms.n.post.stop                                         
## D.npnct05.log                                            .  
## `condition.fctrFor parts or not working`                    
## `condition.fctrManufacturer refurbished`                    
## condition.fctrNew                                           
## `condition.fctrNew other (see details)`                     
## `condition.fctrSeller refurbished`                          
## idseq.my                                                    
## D.ratio.sum.TfIdf.nwrds                                     
## D.TfIdf.sum.stem.stop.Ratio                                 
## D.npnct15.log                                               
## D.npnct03.log                                               
## startprice.diff                                          ***
## biddable                                                 ***
## cellular.fctr1                                              
## cellular.fctrUnknown                                        
## carrier.fctrNone                                            
## carrier.fctrOther                                           
## carrier.fctrSprint                                          
## `carrier.fctrT-Mobile`                                      
## carrier.fctrUnknown                                         
## carrier.fctrVerizon                                         
## `prdline.my.fctriPad 1:idseq.my`                            
## `prdline.my.fctriPad 2:idseq.my`                            
## `prdline.my.fctriPad 3+:idseq.my`                           
## `prdline.my.fctriPadAir:idseq.my`                        .  
## `prdline.my.fctriPadmini:idseq.my`                          
## `prdline.my.fctriPadmini 2+:idseq.my`                       
## `prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds`             
## `prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds`          .  
## `prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds`            
## `prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds`            
## `prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds`           
## `prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds`        
## `prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio`         
## `prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio`         
## `prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio`        
## `prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio`        
## `prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio`       
## `prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio`    
## `prdline.my.fctriPad 1:D.npnct15.log`                       
## `prdline.my.fctriPad 2:D.npnct15.log`                       
## `prdline.my.fctriPad 3+:D.npnct15.log`                      
## `prdline.my.fctriPadAir:D.npnct15.log`                      
## `prdline.my.fctriPadmini:D.npnct15.log`                     
## `prdline.my.fctriPadmini 2+:D.npnct15.log`                  
## `prdline.my.fctriPad 1:D.npnct03.log`                       
## `prdline.my.fctriPad 2:D.npnct03.log`                       
## `prdline.my.fctriPad 3+:D.npnct03.log`                      
## `prdline.my.fctriPadAir:D.npnct03.log`                      
## `prdline.my.fctriPadmini:D.npnct03.log`                     
## `prdline.my.fctriPadmini 2+:D.npnct03.log`                  
## `startprice.diff:biddable`                               ***
## `cellular.fctr1:carrier.fctrNone`                           
## `cellular.fctrUnknown:carrier.fctrNone`                     
## `cellular.fctr1:carrier.fctrOther`                          
## `cellular.fctrUnknown:carrier.fctrOther`                    
## `cellular.fctr1:carrier.fctrSprint`                         
## `cellular.fctrUnknown:carrier.fctrSprint`                   
## `cellular.fctr1:carrier.fctrT-Mobile`                       
## `cellular.fctrUnknown:carrier.fctrT-Mobile`                 
## `cellular.fctr1:carrier.fctrUnknown`                        
## `cellular.fctrUnknown:carrier.fctrUnknown`                  
## `cellular.fctr1:carrier.fctrVerizon`                        
## `cellular.fctrUnknown:carrier.fctrVerizon`                  
## `prdline.my.fctrUnknown:.clusterid.fctr2`                .  
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
## `prdline.my.fctriPad 1:.clusterid.fctr4`                 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`                    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                   
## `prdline.my.fctriPadAir:.clusterid.fctr4`                   
## `prdline.my.fctriPadmini:.clusterid.fctr4`                  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`               
## `prdline.my.fctrUnknown:.clusterid.fctr5`                   
## `prdline.my.fctriPad 1:.clusterid.fctr5`                    
## `prdline.my.fctriPad 2:.clusterid.fctr5`                    
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                   
## `prdline.my.fctriPadAir:.clusterid.fctr5`                   
## `prdline.my.fctriPadmini:.clusterid.fctr5`                  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`               
## `prdline.my.fctrUnknown:.clusterid.fctr6`                   
## `prdline.my.fctriPad 1:.clusterid.fctr6`                    
## `prdline.my.fctriPad 2:.clusterid.fctr6`                    
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                   
## `prdline.my.fctriPadAir:.clusterid.fctr6`                   
## `prdline.my.fctriPadmini:.clusterid.fctr6`                  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`               
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.6  on 973  degrees of freedom
## Residual deviance:  739.2  on 832  degrees of freedom
## AIC: 1023.2
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6988906
## 3        0.2 0.7628676
## 4        0.3 0.7971014
## 5        0.4 0.8139013
## 6        0.5 0.8116959
## 7        0.6 0.8186275
## 8        0.7 0.8025478
## 9        0.8 0.7449393
## 10       0.9 0.6057839
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-40.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.bayesglm.N
## 1         N                                         492
## 2         Y                                         116
##   sold.fctr.predict.All.Interact.X.bayesglm.Y
## 1                                          32
## 2                                         334
##          Prediction
## Reference   N   Y
##         N 492  32
##         Y 116 334
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.480493e-01   6.902515e-01   8.239439e-01   8.700277e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   7.437206e-94   8.943352e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-41.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6843034
## 3        0.2 0.7173690
## 4        0.3 0.7456446
## 5        0.4 0.7568238
## 6        0.5 0.7558442
## 7        0.6 0.7573333
## 8        0.7 0.7413555
## 9        0.8 0.7048458
## 10       0.9 0.5963756
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-42.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.bayesglm.N
## 1         N                                         419
## 2         Y                                         126
##   sold.fctr.predict.All.Interact.X.bayesglm.Y
## 1                                          56
## 2                                         284
##          Prediction
## Reference   N   Y
##         N 419  56
##         Y 126 284
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.943503e-01   5.815820e-01   7.661969e-01   8.205280e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.494158e-57   3.143727e-07 
##                  model_id model_method
## 1 All.Interact.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.669                 0.644
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9015225                    0.6       0.8186275          0.78132
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8239439             0.8700277     0.5584387   0.8364519
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7573333        0.7943503
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7661969              0.820528      0.581582    1023.197
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004951066      0.01217108
##                   label step_major step_minor     bgn     end elapsed
## 8 fit.models_1_bayesglm          8          0 231.894 237.855   5.962
## 9   fit.models_1_glmnet          9          0 237.856      NA      NA
## [1] "fitting model: All.Interact.X.glmnet"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_color_files/figure-html/fit.models_1-43.png) ![](ebayipads_color_files/figure-html/fit.models_1-44.png) 

```
##             Length Class      Mode     
## a0            100  -none-     numeric  
## beta        14100  dgCMatrix  S4       
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
## xNames        141  -none-     character
## problemType     1  -none-     character
## tuneValue       2  data.frame list     
## obsLevels       2  -none-     character
## [1] "min lambda > lambdaOpt:"
##                                            (Intercept) 
##                                          -1.698226e-01 
##                              D.terms.n.stem.stop.Ratio 
##                                          -3.659026e-01 
##                                                 .rnorm 
##                                           1.477174e-03 
##                                          D.npnct01.log 
##                                           4.502176e-02 
##                                         storage.fctr32 
##                                          -2.449667e-02 
##                                         storage.fctr64 
##                                           1.708322e-01 
##                                    storage.fctrUnknown 
##                                          -4.900348e-02 
##                                          D.npnct10.log 
##                                          -1.287469e+00 
##                                  D.TfIdf.sum.post.stop 
##                                           2.469677e-03 
##                                  D.TfIdf.sum.post.stem 
##                                           1.070756e-03 
##                                            D.sum.TfIdf 
##                                           7.806662e-04 
##                                   color.fctrSpace Gray 
##                                          -3.439573e-02 
##                                        color.fctrWhite 
##                                          -1.063857e-01 
##                                  prdline.my.fctriPad 1 
##                                           1.099817e-01 
##                                 prdline.my.fctriPadAir 
##                                           2.724451e-02 
##                             prdline.my.fctriPadmini 2+ 
##                                          -7.900776e-03 
##                                          D.npnct06.log 
##                                          -4.962916e-01 
##                                          D.npnct09.log 
##                                          -2.047825e-01 
##                                          D.npnct14.log 
##                                          -7.695653e-01 
##                                    D.terms.n.post.stem 
##                                          -1.480411e-02 
##                                    D.terms.n.post.stop 
##                                          -1.373785e-02 
##                                          D.npnct05.log 
##                                          -1.096370e+00 
##                 condition.fctrFor parts or not working 
##                                           1.846066e-01 
##                                      condition.fctrNew 
##                                          -2.404425e-01 
##                       condition.fctrSeller refurbished 
##                                          -2.866100e-02 
##                                               idseq.my 
##                                          -2.460823e-04 
##                                D.ratio.sum.TfIdf.nwrds 
##                                           9.474587e-03 
##                                          D.npnct15.log 
##                                           1.882427e-01 
##                                          D.npnct03.log 
##                                           1.588289e-01 
##                                        startprice.diff 
##                                          -3.468219e-03 
##                                               biddable 
##                                           2.068567e+00 
##                                   cellular.fctrUnknown 
##                                          -1.157871e-01 
##                                      carrier.fctrOther 
##                                           3.368476e-01 
##                                     carrier.fctrSprint 
##                                           1.726424e-01 
##                                   carrier.fctrT-Mobile 
##                                          -1.152146e-01 
##                                    carrier.fctrUnknown 
##                                          -2.086499e-01 
##                                    carrier.fctrVerizon 
##                                           5.088062e-02 
##                         prdline.my.fctriPad 1:idseq.my 
##                                           8.967734e-05 
##                         prdline.my.fctriPad 2:idseq.my 
##                                          -8.974868e-05 
##                        prdline.my.fctriPadAir:idseq.my 
##                                          -5.115439e-05 
##                       prdline.my.fctriPadmini:idseq.my 
##                                          -4.324019e-06 
##          prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds 
##                                          -2.651698e-03 
##          prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds 
##                                           8.741729e-01 
##         prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds 
##                                           4.650224e-02 
##         prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds 
##                                          -1.009192e-03 
##     prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds 
##                                          -1.673170e-01 
##      prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio 
##                                           6.246440e-02 
##     prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio 
##                                           4.218641e-02 
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio 
##                                          -2.870275e-03 
##                    prdline.my.fctriPad 1:D.npnct15.log 
##                                           1.634943e+00 
##                    prdline.my.fctriPad 2:D.npnct15.log 
##                                           8.948154e-01 
##                   prdline.my.fctriPad 3+:D.npnct15.log 
##                                          -2.201596e-01 
##                   prdline.my.fctriPadAir:D.npnct15.log 
##                                           1.098672e+00 
##                  prdline.my.fctriPadmini:D.npnct15.log 
##                                           2.521239e+00 
##               prdline.my.fctriPadmini 2+:D.npnct15.log 
##                                          -5.135175e-02 
##                    prdline.my.fctriPad 1:D.npnct03.log 
##                                           1.611671e-01 
##                   prdline.my.fctriPad 3+:D.npnct03.log 
##                                          -6.311387e-02 
##                  prdline.my.fctriPadmini:D.npnct03.log 
##                                           3.290473e-01 
##               prdline.my.fctriPadmini 2+:D.npnct03.log 
##                                          -4.915287e-02 
##                               startprice.diff:biddable 
##                                          -4.892813e-03 
##                       cellular.fctr1:carrier.fctrOther 
##                                           3.349700e-01 
##                      cellular.fctr1:carrier.fctrSprint 
##                                           1.723496e-01 
##                    cellular.fctr1:carrier.fctrT-Mobile 
##                                          -1.145955e-01 
##               cellular.fctrUnknown:carrier.fctrUnknown 
##                                          -1.156738e-01 
##                     cellular.fctr1:carrier.fctrVerizon 
##                                           5.060152e-02 
##                prdline.my.fctrUnknown:.clusterid.fctr2 
##                                           4.866790e-01 
##                 prdline.my.fctriPad 1:.clusterid.fctr2 
##                                           5.078194e-01 
##               prdline.my.fctriPadmini:.clusterid.fctr2 
##                                           3.541130e-01 
##                prdline.my.fctrUnknown:.clusterid.fctr3 
##                                          -4.110006e-01 
##                 prdline.my.fctriPad 2:.clusterid.fctr3 
##                                          -5.529884e-01 
##                prdline.my.fctriPad 3+:.clusterid.fctr3 
##                                          -3.477201e-03 
##                prdline.my.fctriPadAir:.clusterid.fctr3 
##                                          -7.637534e-02 
##               prdline.my.fctriPadmini:.clusterid.fctr3 
##                                           2.714153e-01 
##                 prdline.my.fctriPad 1:.clusterid.fctr4 
##                                          -7.104578e-01 
##                 prdline.my.fctriPad 2:.clusterid.fctr4 
##                                           1.240770e+00 
##                prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                           7.759840e-02 
##               prdline.my.fctriPadmini:.clusterid.fctr4 
##                                          -9.085333e-02 
##               prdline.my.fctriPadmini:.clusterid.fctr5 
##                                          -2.503130e-01 
##                prdline.my.fctriPad 3+:.clusterid.fctr6 
##                                          -8.572935e-01 
## [1] "max lambda < lambdaOpt:"
##                                            (Intercept) 
##                                           9.4092544214 
##                                D.ratio.nstopwrds.nwrds 
##                                          -4.0623637935 
##                              D.terms.n.stem.stop.Ratio 
##                                          -6.9855363620 
##                                                 .rnorm 
##                                           0.0369428979 
##                                          D.npnct01.log 
##                                           0.6165217699 
##                                         storage.fctr16 
##                                          -0.3542440642 
##                                         storage.fctr32 
##                                          -0.5531000946 
##                                         storage.fctr64 
##                                           0.0792727873 
##                                    storage.fctrUnknown 
##                                          -0.7057754038 
##                                          D.npnct11.log 
##                                          -0.0557062969 
##                                          D.npnct10.log 
##                                          -9.9557016772 
##                                  D.TfIdf.sum.post.stop 
##                                           0.0567305353 
##                                          D.npnct13.log 
##                                           0.1090122594 
##                                  D.TfIdf.sum.post.stem 
##                                           0.0321755950 
##                                            D.sum.TfIdf 
##                                           0.0347095394 
##                                         color.fctrGold 
##                                          -0.2853923526 
##                                   color.fctrSpace Gray 
##                                          -0.6085422591 
##                                      color.fctrUnknown 
##                                          -0.2829967500 
##                                        color.fctrWhite 
##                                          -0.4615165875 
##                                          D.npnct08.log 
##                                           0.5458798483 
##                                  prdline.my.fctriPad 1 
##                                           2.8250473222 
##                                  prdline.my.fctriPad 2 
##                                           1.1482785616 
##                                 prdline.my.fctriPad 3+ 
##                                           1.0968460885 
##                                 prdline.my.fctriPadAir 
##                                           0.6047513818 
##                                prdline.my.fctriPadmini 
##                                           0.2100029069 
##                             prdline.my.fctriPadmini 2+ 
##                                           0.6112190326 
##                                        D.nstopwrds.log 
##                                           1.3081823019 
##                                          D.npnct16.log 
##                                           2.4833324904 
##                                          D.npnct24.log 
##                                          -1.3003786797 
##                                          D.npnct06.log 
##                                          -5.0154566355 
##                                          D.npnct28.log 
##                                          -1.0203867837 
##                                            D.nuppr.log 
##                                          -0.0828883945 
##                                            D.nchrs.log 
##                                          -0.1278375544 
##                                          D.npnct12.log 
##                                           0.4686035342 
##                                          D.npnct09.log 
##                                          -1.9516237091 
##                                            D.ndgts.log 
##                                           0.7259517312 
##                                        D.nwrds.unq.log 
##                                          -0.3336914340 
##                                D.terms.n.post.stem.log 
##                                          -0.2629347929 
##                                D.terms.n.post.stop.log 
##                                          -0.2085973096 
##                                          D.npnct14.log 
##                                          -2.3774003067 
##                                    D.terms.n.post.stem 
##                                          -0.0932817113 
##                                    D.terms.n.post.stop 
##                                          -0.1638536562 
##                                          D.npnct05.log 
##                                          -3.5140577393 
##                 condition.fctrFor parts or not working 
##                                          -0.0938135303 
##                 condition.fctrManufacturer refurbished 
##                                           0.4287285422 
##                                      condition.fctrNew 
##                                          -0.1515775865 
##                  condition.fctrNew other (see details) 
##                                           0.0581933230 
##                       condition.fctrSeller refurbished 
##                                          -0.3999074103 
##                                               idseq.my 
##                                           0.0005300156 
##                                D.ratio.sum.TfIdf.nwrds 
##                                           0.4934353587 
##                            D.TfIdf.sum.stem.stop.Ratio 
##                                          -0.3128306550 
##                                          D.npnct15.log 
##                                           1.4870458929 
##                                        startprice.diff 
##                                          -0.0056598648 
##                                               biddable 
##                                           3.8431012027 
##                                         cellular.fctr1 
##                                           0.0530357729 
##                                   cellular.fctrUnknown 
##                                          -0.1643065763 
##                                      carrier.fctrOther 
##                                           2.3514697637 
##                                     carrier.fctrSprint 
##                                           0.5549957289 
##                                   carrier.fctrT-Mobile 
##                                          -0.5656733259 
##                                    carrier.fctrUnknown 
##                                          -0.0431941180 
##                                    carrier.fctrVerizon 
##                                           0.1876390074 
##                         prdline.my.fctriPad 1:idseq.my 
##                                          -0.0001349832 
##                         prdline.my.fctriPad 2:idseq.my 
##                                          -0.0011147109 
##                        prdline.my.fctriPad 3+:idseq.my 
##                                          -0.0004146841 
##                        prdline.my.fctriPadAir:idseq.my 
##                                          -0.0016035128 
##                       prdline.my.fctriPadmini:idseq.my 
##                                          -0.0007005278 
##                    prdline.my.fctriPadmini 2+:idseq.my 
##                                          -0.0001921516 
##          prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds 
##                                          -0.8695237648 
##          prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds 
##                                           2.0958276654 
##         prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds 
##                                          -0.3714903075 
##         prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds 
##                                          -1.2486314268 
##        prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds 
##                                          -1.1477991985 
##     prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds 
##                                          -2.4047377301 
##      prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio 
##                                          -1.6431981838 
##     prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio 
##                                           0.1354198500 
##     prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio 
##                                           2.1602136537 
##    prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio 
##                                           1.0094472987 
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio 
##                                           0.4933747123 
##                    prdline.my.fctriPad 1:D.npnct15.log 
##                                           5.1248217418 
##                    prdline.my.fctriPad 2:D.npnct15.log 
##                                           5.3119048613 
##                   prdline.my.fctriPad 3+:D.npnct15.log 
##                                          -2.1223652039 
##                   prdline.my.fctriPadAir:D.npnct15.log 
##                                           6.0437108665 
##                  prdline.my.fctriPadmini:D.npnct15.log 
##                                           9.2491556328 
##               prdline.my.fctriPadmini 2+:D.npnct15.log 
##                                          -3.5694815589 
##                    prdline.my.fctriPad 1:D.npnct03.log 
##                                           1.7626931778 
##                    prdline.my.fctriPad 2:D.npnct03.log 
##                                          -0.0321411566 
##                   prdline.my.fctriPad 3+:D.npnct03.log 
##                                          -4.5461616868 
##                   prdline.my.fctriPadAir:D.npnct03.log 
##                                           0.7658065373 
##                  prdline.my.fctriPadmini:D.npnct03.log 
##                                          -0.1920338520 
##               prdline.my.fctriPadmini 2+:D.npnct03.log 
##                                          -3.5639761657 
##                               startprice.diff:biddable 
##                                          -0.0135087297 
##                       cellular.fctr1:carrier.fctrOther 
##                                           2.3413483794 
##                      cellular.fctr1:carrier.fctrSprint 
##                                           0.5404536870 
##                    cellular.fctr1:carrier.fctrT-Mobile 
##                                          -0.5745205114 
##               cellular.fctrUnknown:carrier.fctrUnknown 
##                                          -0.1782125384 
##                     cellular.fctr1:carrier.fctrVerizon 
##                                           0.1837629663 
##                prdline.my.fctrUnknown:.clusterid.fctr2 
##                                           1.4030982888 
##                 prdline.my.fctriPad 1:.clusterid.fctr2 
##                                           0.5912425706 
##                 prdline.my.fctriPad 2:.clusterid.fctr2 
##                                          -0.4327392038 
##                prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                          -0.1511747252 
##                prdline.my.fctriPadAir:.clusterid.fctr2 
##                                          -0.1007359331 
##               prdline.my.fctriPadmini:.clusterid.fctr2 
##                                           1.2460841882 
##            prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                           0.7467090013 
##                prdline.my.fctrUnknown:.clusterid.fctr3 
##                                          -1.5174303047 
##                 prdline.my.fctriPad 1:.clusterid.fctr3 
##                                          -0.3015557736 
##                 prdline.my.fctriPad 2:.clusterid.fctr3 
##                                          -1.8668231434 
##                prdline.my.fctriPad 3+:.clusterid.fctr3 
##                                          -0.5019116947 
##               prdline.my.fctriPadmini:.clusterid.fctr3 
##                                           1.2576710189 
##            prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                           1.2096547646 
##                 prdline.my.fctriPad 1:.clusterid.fctr4 
##                                          -1.8692599083 
##                 prdline.my.fctriPad 2:.clusterid.fctr4 
##                                           2.0744185449 
##                prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                           0.5675130539 
##               prdline.my.fctriPadmini:.clusterid.fctr4 
##                                          -0.1329778477 
##                prdline.my.fctriPad 3+:.clusterid.fctr5 
##                                          -1.1112420133 
##               prdline.my.fctriPadmini:.clusterid.fctr5 
##                                          -1.1371925380 
##                prdline.my.fctriPad 3+:.clusterid.fctr6 
##                                          -4.8416180291 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-45.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6507592
## 3        0.2 0.6973789
## 4        0.3 0.7535954
## 5        0.4 0.7890110
## 6        0.5 0.7961859
## 7        0.6 0.7965044
## 8        0.7 0.7364130
## 9        0.8 0.5509554
## 10       0.9 0.1132075
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-46.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.glmnet.N
## 1         N                                       492
## 2         Y                                       131
##   sold.fctr.predict.All.Interact.X.glmnet.Y
## 1                                        32
## 2                                       319
##          Prediction
## Reference   N   Y
##         N 492  32
##         Y 131 319
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326489e-01   6.580424e-01   8.076954e-01   8.555717e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   4.994716e-84   1.641941e-14 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-47.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6449563
## 3        0.2 0.6910995
## 4        0.3 0.7378238
## 5        0.4 0.7606318
## 6        0.5 0.7630208
## 7        0.6 0.7578659
## 8        0.7 0.7184751
## 9        0.8 0.5291005
## 10       0.9 0.1013825
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-48.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.glmnet.N
## 1         N                                       410
## 2         Y                                       117
##   sold.fctr.predict.All.Interact.X.glmnet.Y
## 1                                        65
## 2                                       293
##          Prediction
## Reference   N   Y
##         N 410  65
##         Y 117 293
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.943503e-01   5.828499e-01   7.661969e-01   8.205280e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.494158e-57   1.565945e-04 
##                model_id model_method
## 1 All.Interact.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      9.088                 0.736
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8845377                    0.6       0.7965044        0.7977651
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8076954             0.8555717     0.5903552   0.8510347
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7630208        0.7943503
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7661969              0.820528     0.5828499
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02063411      0.04268017
##                  label step_major step_minor     bgn    end elapsed
## 9  fit.models_1_glmnet          9          0 237.856 250.88  13.024
## 10  fit.models_1_rpart         10          0 250.881     NA      NA
## [1] "fitting model: All.Interact.X.no.rnorm.rpart"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.02 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_color_files/figure-html/fit.models_1-49.png) ![](ebayipads_color_files/figure-html/fit.models_1-50.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##           CP nsplit rel error
## 1 0.51111111      0 1.0000000
## 2 0.08666667      1 0.4888889
## 3 0.02000000      2 0.4022222
## 
## Variable importance
##                           startprice.diff:biddable 
##                                                 36 
##                                           biddable 
##                                                 34 
##                                    startprice.diff 
##                                                 15 
##                                           idseq.my 
##                                                  7 
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio 
##                                                  2 
##                     prdline.my.fctriPad 1:idseq.my 
##                                                  2 
##             condition.fctrFor parts or not working 
##                                                  2 
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio 
##                                                  1 
##                    prdline.my.fctriPadAir:idseq.my 
##                                                  1 
##                                  condition.fctrNew 
##                                                  1 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable                        < 0.5        to the left,  improve=144.14990, (0 missing)
##       startprice.diff:biddable        < -1.57273   to the right, improve= 99.97262, (0 missing)
##       startprice.diff                 < 59.64413   to the right, improve= 48.01938, (0 missing)
##       idseq.my                        < 905.5      to the right, improve= 38.91299, (0 missing)
##       prdline.my.fctriPadAir:idseq.my < 869        to the right, improve= 12.82037, (0 missing)
##   Surrogate splits:
##       startprice.diff:biddable                           < 0.06166769 to the left,  agree=0.829, adj=0.629, (0 split)
##       idseq.my                                           < 869        to the right, agree=0.636, adj=0.211, (0 split)
##       prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio < 0.9972378  to the left,  agree=0.566, adj=0.060, (0 split)
##       prdline.my.fctriPad 1:idseq.my                     < 71         to the left,  agree=0.564, adj=0.056, (0 split)
##       condition.fctrFor parts or not working             < 0.5        to the left,  agree=0.563, adj=0.053, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.08666667
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (135 obs) right son=7 (315 obs)
##   Primary splits:
##       startprice.diff                     < 68.91842   to the right, improve=61.714290, (0 missing)
##       startprice.diff:biddable            < 68.91842   to the right, improve=61.714290, (0 missing)
##       idseq.my                            < 670.5      to the right, improve=15.110620, (0 missing)
##       prdline.my.fctriPadmini 2+:idseq.my < 666.5      to the right, improve= 4.191636, (0 missing)
##       condition.fctrNew                   < 0.5        to the right, improve= 3.467222, (0 missing)
##   Surrogate splits:
##       startprice.diff:biddable                           < 68.91842   to the right, agree=1.000, adj=1.000, (0 split)
##       prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio < 0.9845277  to the right, agree=0.718, adj=0.059, (0 split)
##       prdline.my.fctriPadAir:idseq.my                    < 741        to the right, agree=0.716, adj=0.052, (0 split)
##       condition.fctrNew                                  < 0.5        to the right, agree=0.713, adj=0.044, (0 split)
##       prdline.my.fctriPadmini 2+                         < 0.5        to the right, agree=0.709, adj=0.030, (0 split)
## 
## Node number 6: 135 observations
##   predicted class=N  expected loss=0.3555556  P(node) =0.1386037
##     class counts:    87    48
##    probabilities: 0.644 0.356 
## 
## Node number 7: 315 observations
##   predicted class=Y  expected loss=0.07301587  P(node) =0.3234086
##     class counts:    23   292
##    probabilities: 0.073 0.927 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.53798768 0.46201232)  
##   2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##   3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##     6) startprice.diff>=68.91842 135  48 N (0.64444444 0.35555556) *
##     7) startprice.diff< 68.91842 315  23 Y (0.07301587 0.92698413) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-51.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7555556
## 5        0.4 0.7633987
## 6        0.5 0.7633987
## 7        0.6 0.7633987
## 8        0.7 0.7633987
## 9        0.8 0.7633987
## 10       0.9 0.7633987
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-52.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rpart.N
## 1         N                                               501
## 2         Y                                               158
##   sold.fctr.predict.All.Interact.X.no.rnorm.rpart.Y
## 1                                                23
## 2                                               292
##          Prediction
## Reference   N   Y
##         N 501  23
##         Y 158 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.141684e-01   6.180889e-01   7.882906e-01   8.381305e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.530340e-73   2.277382e-23 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-53.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7528231
## 5        0.4 0.7420290
## 6        0.5 0.7420290
## 7        0.6 0.7420290
## 8        0.7 0.7420290
## 9        0.8 0.7420290
## 10       0.9 0.7420290
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-54.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rpart.N
## 1         N                                               388
## 2         Y                                               110
##   sold.fctr.predict.All.Interact.X.no.rnorm.rpart.Y
## 1                                                87
## 2                                               300
##          Prediction
## Reference   N   Y
##         N 388  87
##         Y 110 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.774011e-01   5.506630e-01   7.485294e-01   8.044124e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.956102e-50   1.170130e-01 
##                        model_id model_method
## 1 All.Interact.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.854                 0.093
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8243427                    0.9       0.7633987        0.8018645
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7882906             0.8381305     0.5940341   0.8129705
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7528231        0.7774011
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7485294             0.8044124      0.550663
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01388821      0.02957203
##                 label step_major step_minor     bgn     end elapsed
## 10 fit.models_1_rpart         10          0 250.881 256.412   5.531
## 11    fit.models_1_rf         11          0 256.413      NA      NA
## [1] "fitting model: All.Interact.X.no.rnorm.rf"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 71 on full training set
```

![](ebayipads_color_files/figure-html/fit.models_1-55.png) ![](ebayipads_color_files/figure-html/fit.models_1-56.png) 

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
## importance       140   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           140   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-57.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8234218
## 3        0.2 0.9394572
## 4        0.3 0.9846827
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 0.9988877
## 8        0.7 0.9510490
## 9        0.8 0.8607595
## 10       0.9 0.7738420
## 11       1.0 0.1963928
```

![](ebayipads_color_files/figure-html/fit.models_1-58.png) 

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

![](ebayipads_color_files/figure-html/fit.models_1-59.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6843501
## 3        0.2 0.7243781
## 4        0.3 0.7433628
## 5        0.4 0.7647768
## 6        0.5 0.7727856
## 7        0.6 0.7747253
## 8        0.7 0.7590188
## 9        0.8 0.7329377
## 10       0.9 0.6635071
## 11       1.0 0.1232877
```

![](ebayipads_color_files/figure-html/fit.models_1-60.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.N
## 1         N                                            439
## 2         Y                                            128
##   sold.fctr.predict.All.Interact.X.no.rnorm.rf.Y
## 1                                             36
## 2                                            282
##          Prediction
## Reference   N   Y
##         N 439  36
##         Y 128 282
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.146893e-01   6.215582e-01   7.874927e-01   8.397715e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.806781e-67   1.195356e-12 
##                     model_id model_method
## 1 All.Interact.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     20.893                 7.009
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.7823457
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962198                     1     0.5584003   0.8559487
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7747253        0.8146893
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7874927             0.8397715     0.6215582
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.006148854      0.01320333
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
# !_sp
model_id <- "csm"; indep_vars_vctr <- c(NULL
    ,"prdline.my.fctr", "prdline.my.fctr:.clusterid.fctr"
    ,"prdline.my.fctr*biddable"
    #,"prdline.my.fctr*startprice.log"
    #,"prdline.my.fctr*startprice.diff"    
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
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   639
```

![](ebayipads_color_files/figure-html/fit.models_1-61.png) ![](ebayipads_color_files/figure-html/fit.models_1-62.png) 

```
## Warning: not plotting observations with leverage one:
##   639
```

![](ebayipads_color_files/figure-html/fit.models_1-63.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.7858  -0.6818  -0.2582   0.6508   2.4690  
## 
## Coefficients: (17 not defined because of singularities)
##                                                                       Estimate
## (Intercept)                                                         -1.900e+00
## `prdline.my.fctriPad 1`                                              8.143e-01
## `prdline.my.fctriPad 2`                                              3.003e-01
## `prdline.my.fctriPad 3+`                                             1.536e+00
## prdline.my.fctriPadAir                                               7.191e-01
## prdline.my.fctriPadmini                                              9.831e-01
## `prdline.my.fctriPadmini 2+`                                         8.573e-01
## biddable                                                             2.399e+00
## `condition.fctrFor parts or not working`                             1.060e+00
## `condition.fctrManufacturer refurbished`                             1.088e+14
## condition.fctrNew                                                   -1.031e+00
## `condition.fctrNew other (see details)`                              2.833e+00
## `condition.fctrSeller refurbished`                                   5.202e-01
## D.terms.n.post.stop                                                  2.404e-02
## cellular.fctr1                                                      -2.454e+00
## cellular.fctrUnknown                                                -4.524e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            1.131e+00
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            -5.541e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             1.618e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            2.816e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            3.566e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           1.171e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        1.949e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           -1.127e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            -2.100e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            -1.001e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            3.771e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                           -5.482e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           9.583e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        3.432e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            -4.021e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             3.907e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            1.041e+00
## `prdline.my.fctriPadAir:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`                          -3.766e-02
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            6.110e-02
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                          -9.449e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`                                    NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                           -4.504e+15
## `prdline.my.fctriPadAir:.clusterid.fctr6`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`                                  NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                               NA
## `prdline.my.fctriPad 1:biddable`                                     1.431e+00
## `prdline.my.fctriPad 2:biddable`                                     1.145e+00
## `prdline.my.fctriPad 3+:biddable`                                   -4.215e-01
## `prdline.my.fctriPadAir:biddable`                                    6.005e-01
## `prdline.my.fctriPadmini:biddable`                                   2.015e-01
## `prdline.my.fctriPadmini 2+:biddable`                               -5.837e-01
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       1.297e+00
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       5.342e-02
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     -1.096e+00
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     -4.000e-01
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    -2.106e+00
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` -1.199e+01
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     -1.088e+14
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`     -1.088e+14
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    -1.088e+14
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished` -1.088e+14
## `prdline.my.fctriPad 1:condition.fctrNew`                           -4.504e+15
## `prdline.my.fctriPad 2:condition.fctrNew`                                   NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                          -7.190e-01
## `prdline.my.fctriPadAir:condition.fctrNew`                           6.458e-02
## `prdline.my.fctriPadmini:condition.fctrNew`                          5.439e-01
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       6.983e-01
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       -3.778e+00
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       -4.504e+15
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`      -2.341e+00
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`      -1.724e+00
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     -4.405e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  -5.190e+00
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.968e+00
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            -6.535e-01
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           -1.357e+00
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           -2.834e-01
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          -2.047e+00
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       -4.504e+15
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          1.124e-01
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         -1.512e-01
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        -2.082e-01
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        -1.458e-01
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       -8.592e-02
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    -1.662e-01
## `prdline.my.fctriPad 1:cellular.fctr1`                               2.200e+00
## `prdline.my.fctriPad 2:cellular.fctr1`                               1.831e+00
## `prdline.my.fctriPad 3+:cellular.fctr1`                              2.267e+00
## `prdline.my.fctriPadAir:cellular.fctr1`                              2.213e+00
## `prdline.my.fctriPadmini:cellular.fctr1`                             2.708e+00
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          2.751e+00
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                        -3.392e-01
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        -9.981e-01
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       -4.377e-01
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        6.079e-01
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       4.677e-01
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    1.768e-01
##                                                                     Std. Error
## (Intercept)                                                          7.613e-01
## `prdline.my.fctriPad 1`                                              8.939e-01
## `prdline.my.fctriPad 2`                                              9.953e-01
## `prdline.my.fctriPad 3+`                                             8.824e-01
## prdline.my.fctriPadAir                                               8.557e-01
## prdline.my.fctriPadmini                                              8.621e-01
## `prdline.my.fctriPadmini 2+`                                         9.646e-01
## biddable                                                             6.113e-01
## `condition.fctrFor parts or not working`                             7.707e-01
## `condition.fctrManufacturer refurbished`                             1.578e+14
## condition.fctrNew                                                    9.190e-01
## `condition.fctrNew other (see details)`                              1.504e+00
## `condition.fctrSeller refurbished`                                   1.076e+00
## D.terms.n.post.stop                                                  8.799e-02
## cellular.fctr1                                                       1.452e+00
## cellular.fctrUnknown                                                 7.158e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            8.018e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             1.409e+00
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             1.251e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            7.299e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            8.508e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           8.508e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        1.637e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            9.530e-01
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             1.511e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             1.784e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            8.376e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            8.448e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           1.049e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        1.278e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             1.429e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             1.889e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            8.384e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           1.097e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            8.368e-01
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           1.125e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`                                    NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                            2.740e+07
## `prdline.my.fctriPadAir:.clusterid.fctr6`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`                                  NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                               NA
## `prdline.my.fctriPad 1:biddable`                                     9.144e-01
## `prdline.my.fctriPad 2:biddable`                                     8.818e-01
## `prdline.my.fctriPad 3+:biddable`                                    7.481e-01
## `prdline.my.fctriPadAir:biddable`                                    7.331e-01
## `prdline.my.fctriPadmini:biddable`                                   7.754e-01
## `prdline.my.fctriPadmini 2+:biddable`                                8.069e-01
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       1.642e+00
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       1.370e+00
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      9.934e-01
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      1.467e+00
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     9.623e-01
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  2.995e+00
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      1.578e+14
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      1.578e+14
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     1.578e+14
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`  1.578e+14
## `prdline.my.fctriPad 1:condition.fctrNew`                            3.875e+07
## `prdline.my.fctriPad 2:condition.fctrNew`                                   NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                           1.542e+00
## `prdline.my.fctriPadAir:condition.fctrNew`                           1.039e+00
## `prdline.my.fctriPadmini:condition.fctrNew`                          1.125e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       1.108e+00
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        2.996e+00
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        4.745e+07
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       1.745e+00
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       1.721e+00
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      2.065e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   1.965e+00
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.634e+00
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             1.476e+00
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            1.387e+00
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            1.466e+00
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           1.445e+00
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        4.745e+07
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          1.778e-01
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          1.663e-01
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         1.176e-01
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         1.253e-01
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        1.244e-01
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     1.830e-01
## `prdline.my.fctriPad 1:cellular.fctr1`                               1.588e+00
## `prdline.my.fctriPad 2:cellular.fctr1`                               1.578e+00
## `prdline.my.fctriPad 3+:cellular.fctr1`                              1.512e+00
## `prdline.my.fctriPadAir:cellular.fctr1`                              1.511e+00
## `prdline.my.fctriPadmini:cellular.fctr1`                             1.541e+00
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          1.589e+00
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         1.555e+00
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         1.592e+00
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        1.161e+00
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        1.488e+00
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       1.215e+00
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    1.095e+00
##                                                                        z value
## (Intercept)                                                         -2.496e+00
## `prdline.my.fctriPad 1`                                              9.110e-01
## `prdline.my.fctriPad 2`                                              3.020e-01
## `prdline.my.fctriPad 3+`                                             1.740e+00
## prdline.my.fctriPadAir                                               8.400e-01
## prdline.my.fctriPadmini                                              1.140e+00
## `prdline.my.fctriPadmini 2+`                                         8.890e-01
## biddable                                                             3.925e+00
## `condition.fctrFor parts or not working`                             1.375e+00
## `condition.fctrManufacturer refurbished`                             6.900e-01
## condition.fctrNew                                                   -1.122e+00
## `condition.fctrNew other (see details)`                              1.883e+00
## `condition.fctrSeller refurbished`                                   4.840e-01
## D.terms.n.post.stop                                                  2.730e-01
## cellular.fctr1                                                      -1.690e+00
## cellular.fctrUnknown                                                -6.320e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            1.411e+00
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            -3.930e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             1.293e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            3.860e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            4.190e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           1.377e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        1.191e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           -1.183e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            -1.389e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            -5.600e-02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            4.500e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                           -6.490e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           9.130e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        2.690e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            -2.813e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             2.068e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            1.242e+00
## `prdline.my.fctriPadAir:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`                          -3.400e-02
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            7.300e-02
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                          -8.400e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`                                    NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                           -1.644e+08
## `prdline.my.fctriPadAir:.clusterid.fctr6`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`                                  NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                               NA
## `prdline.my.fctriPad 1:biddable`                                     1.565e+00
## `prdline.my.fctriPad 2:biddable`                                     1.299e+00
## `prdline.my.fctriPad 3+:biddable`                                   -5.630e-01
## `prdline.my.fctriPadAir:biddable`                                    8.190e-01
## `prdline.my.fctriPadmini:biddable`                                   2.600e-01
## `prdline.my.fctriPadmini 2+:biddable`                               -7.230e-01
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       7.900e-01
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       3.900e-02
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     -1.103e+00
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     -2.730e-01
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    -2.189e+00
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` -4.004e+00
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     -6.900e-01
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`     -6.900e-01
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    -6.900e-01
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished` -6.900e-01
## `prdline.my.fctriPad 1:condition.fctrNew`                           -1.162e+08
## `prdline.my.fctriPad 2:condition.fctrNew`                                   NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                          -4.660e-01
## `prdline.my.fctriPadAir:condition.fctrNew`                           6.200e-02
## `prdline.my.fctriPadmini:condition.fctrNew`                          4.840e-01
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       6.300e-01
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       -1.261e+00
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       -9.491e+07
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`      -1.342e+00
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`      -1.002e+00
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     -2.133e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  -2.641e+00
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.205e+00
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            -4.430e-01
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           -9.790e-01
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           -1.930e-01
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          -1.416e+00
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       -9.491e+07
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          6.320e-01
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         -9.090e-01
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        -1.771e+00
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        -1.164e+00
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       -6.910e-01
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    -9.080e-01
## `prdline.my.fctriPad 1:cellular.fctr1`                               1.385e+00
## `prdline.my.fctriPad 2:cellular.fctr1`                               1.161e+00
## `prdline.my.fctriPad 3+:cellular.fctr1`                              1.499e+00
## `prdline.my.fctriPadAir:cellular.fctr1`                              1.465e+00
## `prdline.my.fctriPadmini:cellular.fctr1`                             1.758e+00
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          1.731e+00
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                        -2.180e-01
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        -6.270e-01
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       -3.770e-01
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        4.080e-01
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       3.850e-01
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    1.610e-01
##                                                                     Pr(>|z|)
## (Intercept)                                                          0.01255
## `prdline.my.fctriPad 1`                                              0.36235
## `prdline.my.fctriPad 2`                                              0.76289
## `prdline.my.fctriPad 3+`                                             0.08183
## prdline.my.fctriPadAir                                               0.40066
## prdline.my.fctriPadmini                                              0.25415
## `prdline.my.fctriPadmini 2+`                                         0.37412
## biddable                                                            8.66e-05
## `condition.fctrFor parts or not working`                             0.16919
## `condition.fctrManufacturer refurbished`                             0.49039
## condition.fctrNew                                                    0.26172
## `condition.fctrNew other (see details)`                              0.05970
## `condition.fctrSeller refurbished`                                   0.62872
## D.terms.n.post.stop                                                  0.78472
## cellular.fctr1                                                       0.09099
## cellular.fctrUnknown                                                 0.52740
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            0.15826
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             0.69414
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             0.19613
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            0.69960
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            0.67511
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           0.16866
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        0.23360
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            0.23689
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             0.16471
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             0.95523
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            0.65254
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            0.51643
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           0.36105
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        0.78826
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                 NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             0.00491
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             0.03862
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            0.21436
## `prdline.my.fctriPadAir:.clusterid.fctr4`                                 NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           0.97263
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                             NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                  NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                                  NA
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            0.94180
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           0.40098
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                             NA
## `prdline.my.fctrUnknown:.clusterid.fctr6`                                 NA
## `prdline.my.fctriPad 1:.clusterid.fctr6`                                  NA
## `prdline.my.fctriPad 2:.clusterid.fctr6`                                  NA
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                            < 2e-16
## `prdline.my.fctriPadAir:.clusterid.fctr6`                                 NA
## `prdline.my.fctriPadmini:.clusterid.fctr6`                                NA
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                             NA
## `prdline.my.fctriPad 1:biddable`                                     0.11759
## `prdline.my.fctriPad 2:biddable`                                     0.19394
## `prdline.my.fctriPad 3+:biddable`                                    0.57312
## `prdline.my.fctriPadAir:biddable`                                    0.41272
## `prdline.my.fctriPadmini:biddable`                                   0.79492
## `prdline.my.fctriPadmini 2+:biddable`                                0.46948
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       0.42955
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       0.96889
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      0.27009
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      0.78506
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     0.02861
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` 6.23e-05
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`            NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`            NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      0.49039
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      0.49039
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     0.49039
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`  0.49039
## `prdline.my.fctriPad 1:condition.fctrNew`                            < 2e-16
## `prdline.my.fctriPad 2:condition.fctrNew`                                 NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                           0.64103
## `prdline.my.fctriPadAir:condition.fctrNew`                           0.95046
## `prdline.my.fctriPadmini:condition.fctrNew`                          0.62869
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       0.52849
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        0.20724
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        < 2e-16
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       0.17975
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       0.31652
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      0.03290
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   0.00827
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             0.22825
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             0.65797
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            0.32773
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            0.84665
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           0.15674
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        < 2e-16
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          0.52720
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          0.36314
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         0.07661
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         0.24462
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        0.48960
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     0.36391
## `prdline.my.fctriPad 1:cellular.fctr1`                               0.16593
## `prdline.my.fctriPad 2:cellular.fctr1`                               0.24582
## `prdline.my.fctriPad 3+:cellular.fctr1`                              0.13391
## `prdline.my.fctriPadAir:cellular.fctr1`                              0.14289
## `prdline.my.fctriPadmini:cellular.fctr1`                             0.07878
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          0.08347
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         0.82735
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         0.53060
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        0.70625
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        0.68296
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       0.70019
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    0.87178
##                                                                        
## (Intercept)                                                         *  
## `prdline.my.fctriPad 1`                                                
## `prdline.my.fctriPad 2`                                                
## `prdline.my.fctriPad 3+`                                            .  
## prdline.my.fctriPadAir                                                 
## prdline.my.fctriPadmini                                                
## `prdline.my.fctriPadmini 2+`                                           
## biddable                                                            ***
## `condition.fctrFor parts or not working`                               
## `condition.fctrManufacturer refurbished`                               
## condition.fctrNew                                                      
## `condition.fctrNew other (see details)`                             .  
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
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                          
## `prdline.my.fctrUnknown:.clusterid.fctr3`                              
## `prdline.my.fctriPad 1:.clusterid.fctr3`                               
## `prdline.my.fctriPad 2:.clusterid.fctr3`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                              
## `prdline.my.fctriPadAir:.clusterid.fctr3`                              
## `prdline.my.fctriPadmini:.clusterid.fctr3`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                          
## `prdline.my.fctrUnknown:.clusterid.fctr4`                              
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            ** 
## `prdline.my.fctriPad 2:.clusterid.fctr4`                            *  
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                              
## `prdline.my.fctriPadAir:.clusterid.fctr4`                              
## `prdline.my.fctriPadmini:.clusterid.fctr4`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                          
## `prdline.my.fctrUnknown:.clusterid.fctr5`                              
## `prdline.my.fctriPad 1:.clusterid.fctr5`                               
## `prdline.my.fctriPad 2:.clusterid.fctr5`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                              
## `prdline.my.fctriPadAir:.clusterid.fctr5`                              
## `prdline.my.fctriPadmini:.clusterid.fctr5`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                          
## `prdline.my.fctrUnknown:.clusterid.fctr6`                              
## `prdline.my.fctriPad 1:.clusterid.fctr6`                               
## `prdline.my.fctriPad 2:.clusterid.fctr6`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                           ***
## `prdline.my.fctriPadAir:.clusterid.fctr6`                              
## `prdline.my.fctriPadmini:.clusterid.fctr6`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                          
## `prdline.my.fctriPad 1:biddable`                                       
## `prdline.my.fctriPad 2:biddable`                                       
## `prdline.my.fctriPad 3+:biddable`                                      
## `prdline.my.fctriPadAir:biddable`                                      
## `prdline.my.fctriPadmini:biddable`                                     
## `prdline.my.fctriPadmini 2+:biddable`                                  
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`         
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`         
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`        
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`        
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    *  
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` ***
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`         
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`         
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`        
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`        
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`       
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`    
## `prdline.my.fctriPad 1:condition.fctrNew`                           ***
## `prdline.my.fctriPad 2:condition.fctrNew`                              
## `prdline.my.fctriPad 3+:condition.fctrNew`                             
## `prdline.my.fctriPadAir:condition.fctrNew`                             
## `prdline.my.fctriPadmini:condition.fctrNew`                            
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                         
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`          
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       ***
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`         
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`         
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     *  
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  ** 
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`               
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`               
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`             
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       ***
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        .  
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                           
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                          
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                       
## `prdline.my.fctriPad 1:cellular.fctr1`                                 
## `prdline.my.fctriPad 2:cellular.fctr1`                                 
## `prdline.my.fctriPad 3+:cellular.fctr1`                                
## `prdline.my.fctriPadAir:cellular.fctr1`                                
## `prdline.my.fctriPadmini:cellular.fctr1`                            .  
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                         .  
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
## Residual deviance:  885.99  on 886  degrees of freedom
## AIC: 1062
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_1-64.png) ![](ebayipads_color_files/figure-html/fit.models_1-65.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6815385
## 3        0.2 0.7357331
## 4        0.3 0.7770961
## 5        0.4 0.7768240
## 6        0.5 0.7752809
## 7        0.6 0.7623529
## 8        0.7 0.7310167
## 9        0.8 0.6447932
## 10       0.9 0.2953271
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.glm.N sold.fctr.predict.csm.glm.Y
## 1         N                         376                         148
## 2         Y                          70                         380
##          Prediction
## Reference   N   Y
##         N 376 148
##         Y  70 380
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.761807e-01   5.552064e-01   7.486774e-01   8.020028e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   8.551925e-54   1.837200e-07 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_color_files/figure-html/fit.models_1-66.png) ![](ebayipads_color_files/figure-html/fit.models_1-67.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6465517
## 3        0.2 0.6802326
## 4        0.3 0.7013575
## 5        0.4 0.7148014
## 6        0.5 0.7094340
## 7        0.6 0.7032680
## 8        0.7 0.6373938
## 9        0.8 0.5665635
## 10       0.9 0.2545455
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-68.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.glm.N sold.fctr.predict.csm.glm.Y
## 1         N                         351                         124
## 2         Y                         113                         297
##          Prediction
## Reference   N   Y
##         N 351 124
##         Y 113 297
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.322034e-01   4.624886e-01   7.017229e-01   7.611290e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   5.368860e-33   5.159701e-01 
##   model_id model_method
## 1  csm.glm          glm
##                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.892                 0.434
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8645971                    0.3       0.7770961        0.7310098
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7486774             0.8020028     0.4584371   0.7577818
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7148014        0.7322034
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7017229              0.761129     0.4624886    1061.995
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01871693      0.04314557
##                                                                       importance
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                           1.000000e+02
## `prdline.my.fctriPad 1:condition.fctrNew`                           7.070959e+01
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       5.773499e+01
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       5.773496e+01
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` 2.414836e-06
## biddable                                                            2.367024e-06
## [1] "fitting model: csm.bayesglm"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.5095  -0.7012  -0.3496   0.6620   2.4254  
## 
## Coefficients:
##                                                                      Estimate
## (Intercept)                                                         -1.508512
## `prdline.my.fctriPad 1`                                              0.463383
## `prdline.my.fctriPad 2`                                              0.060657
## `prdline.my.fctriPad 3+`                                             1.074380
## prdline.my.fctriPadAir                                               0.377256
## prdline.my.fctriPadmini                                              0.604061
## `prdline.my.fctriPadmini 2+`                                         0.393566
## biddable                                                             2.056725
## `condition.fctrFor parts or not working`                             0.615225
## `condition.fctrManufacturer refurbished`                            -0.076083
## condition.fctrNew                                                   -1.125439
## `condition.fctrNew other (see details)`                              0.517524
## `condition.fctrSeller refurbished`                                   0.015575
## D.terms.n.post.stop                                                 -0.003836
## cellular.fctr1                                                      -0.701578
## cellular.fctrUnknown                                                -0.311681
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            0.944240
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            -0.018028
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             1.017765
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            0.228491
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            0.268898
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           0.940611
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        1.057369
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           -0.894932
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            -1.219411
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            -0.556292
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            0.297080
## `prdline.my.fctriPadAir:.clusterid.fctr3`                           -0.480489
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           0.784650
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                       -0.136721
## `prdline.my.fctrUnknown:.clusterid.fctr4`                            0.000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            -2.938293
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             3.105465
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            0.904838
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            0.000000
## `prdline.my.fctriPadmini:.clusterid.fctr4`                          -0.244613
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                        0.000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                            0.000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                             0.000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             0.000000
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            0.012033
## `prdline.my.fctriPadAir:.clusterid.fctr5`                            0.000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                          -0.821816
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                        0.000000
## `prdline.my.fctrUnknown:.clusterid.fctr6`                            0.000000
## `prdline.my.fctriPad 1:.clusterid.fctr6`                             0.000000
## `prdline.my.fctriPad 2:.clusterid.fctr6`                             0.000000
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                           -1.413633
## `prdline.my.fctriPadAir:.clusterid.fctr6`                            0.000000
## `prdline.my.fctriPadmini:.clusterid.fctr6`                           0.000000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                        0.000000
## `prdline.my.fctriPad 1:biddable`                                     1.394635
## `prdline.my.fctriPad 2:biddable`                                     1.312908
## `prdline.my.fctriPad 3+:biddable`                                   -0.043244
## `prdline.my.fctriPadAir:biddable`                                    0.877863
## `prdline.my.fctriPadmini:biddable`                                   0.414435
## `prdline.my.fctriPadmini 2+:biddable`                               -0.209169
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       1.625643
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       0.257825
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     -0.591202
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     -0.186294
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    -1.440430
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` -1.258249
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`       0.000000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`       0.000000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     -0.210818
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      1.377732
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    -0.453904
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished` -0.674377
## `prdline.my.fctriPad 1:condition.fctrNew`                           -1.955158
## `prdline.my.fctriPad 2:condition.fctrNew`                            0.000000
## `prdline.my.fctriPad 3+:condition.fctrNew`                          -0.444166
## `prdline.my.fctriPadAir:condition.fctrNew`                           0.162938
## `prdline.my.fctriPadmini:condition.fctrNew`                          0.625985
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       0.842047
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       -0.626391
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       -0.995009
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       0.010610
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       0.457803
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     -1.505134
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  -2.260569
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.854995
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            -0.093928
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           -0.714342
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            0.150427
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          -1.257000
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       -2.038421
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          0.078413
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         -0.065103
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        -0.167531
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        -0.107755
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       -0.044040
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    -0.072363
## `prdline.my.fctriPad 1:cellular.fctr1`                               0.405270
## `prdline.my.fctriPad 2:cellular.fctr1`                               0.042205
## `prdline.my.fctriPad 3+:cellular.fctr1`                              0.540558
## `prdline.my.fctriPadAir:cellular.fctr1`                              0.422384
## `prdline.my.fctriPadmini:cellular.fctr1`                             0.908276
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          0.890094
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                        -0.376446
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        -0.790415
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       -0.484768
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        0.239034
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       0.208358
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    0.029532
##                                                                     Std. Error
## (Intercept)                                                           0.460297
## `prdline.my.fctriPad 1`                                               0.600087
## `prdline.my.fctriPad 2`                                               0.661639
## `prdline.my.fctriPad 3+`                                              0.595670
## prdline.my.fctriPadAir                                                0.564969
## prdline.my.fctriPadmini                                               0.568108
## `prdline.my.fctriPadmini 2+`                                          0.664848
## biddable                                                              0.420601
## `condition.fctrFor parts or not working`                              0.540775
## `condition.fctrManufacturer refurbished`                              1.042265
## condition.fctrNew                                                     0.605326
## `condition.fctrNew other (see details)`                               0.746275
## `condition.fctrSeller refurbished`                                    0.667977
## D.terms.n.post.stop                                                   0.068715
## cellular.fctr1                                                        0.603735
## cellular.fctrUnknown                                                  0.459164
## `prdline.my.fctrUnknown:.clusterid.fctr2`                             0.651548
## `prdline.my.fctriPad 1:.clusterid.fctr2`                              0.958051
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              0.860839
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                             0.634082
## `prdline.my.fctriPadAir:.clusterid.fctr2`                             0.727854
## `prdline.my.fctriPadmini:.clusterid.fctr2`                            0.705941
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                         1.179868
## `prdline.my.fctrUnknown:.clusterid.fctr3`                             0.787226
## `prdline.my.fctriPad 1:.clusterid.fctr3`                              1.011018
## `prdline.my.fctriPad 2:.clusterid.fctr3`                              1.196935
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             0.726823
## `prdline.my.fctriPadAir:.clusterid.fctr3`                             0.727995
## `prdline.my.fctriPadmini:.clusterid.fctr3`                            0.852444
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                         0.973812
## `prdline.my.fctrUnknown:.clusterid.fctr4`                             2.500000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                              1.003950
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              1.195250
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             0.731258
## `prdline.my.fctriPadAir:.clusterid.fctr4`                             2.500000
## `prdline.my.fctriPadmini:.clusterid.fctr4`                            0.879112
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                         2.500000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                             2.500000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                              2.500000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                              2.500000
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                             0.726767
## `prdline.my.fctriPadAir:.clusterid.fctr5`                             2.500000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                            0.927142
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                         2.500000
## `prdline.my.fctrUnknown:.clusterid.fctr6`                             2.500000
## `prdline.my.fctriPad 1:.clusterid.fctr6`                              2.500000
## `prdline.my.fctriPad 2:.clusterid.fctr6`                              2.500000
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                             1.536630
## `prdline.my.fctriPadAir:.clusterid.fctr6`                             2.500000
## `prdline.my.fctriPadmini:.clusterid.fctr6`                            2.500000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                         2.500000
## `prdline.my.fctriPad 1:biddable`                                      0.674992
## `prdline.my.fctriPad 2:biddable`                                      0.665157
## `prdline.my.fctriPad 3+:biddable`                                     0.565566
## `prdline.my.fctriPadAir:biddable`                                     0.557964
## `prdline.my.fctriPadmini:biddable`                                    0.582688
## `prdline.my.fctriPadmini 2+:biddable`                                 0.619813
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`        1.139607
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`        1.015844
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`       0.760329
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`       0.983406
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`      0.731036
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`   1.625974
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`        2.500000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`        2.500000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`       1.215864
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`       1.271817
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`      1.211499
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`   1.831739
## `prdline.my.fctriPad 1:condition.fctrNew`                             1.689442
## `prdline.my.fctriPad 2:condition.fctrNew`                             2.500000
## `prdline.my.fctriPad 3+:condition.fctrNew`                            1.109556
## `prdline.my.fctriPadAir:condition.fctrNew`                            0.728326
## `prdline.my.fctriPadmini:condition.fctrNew`                           0.805481
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                        0.783553
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`         1.590404
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`         1.685738
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`        0.998661
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`        0.981804
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`       1.245644
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`    1.199084
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              1.126210
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`              0.992255
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`             0.960704
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`             1.015081
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`            1.008867
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`         1.675167
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                           0.125658
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                           0.118527
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                          0.095145
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                          0.101723
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                         0.099149
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                      0.139056
## `prdline.my.fctriPad 1:cellular.fctr1`                                0.776112
## `prdline.my.fctriPad 2:cellular.fctr1`                                0.776217
## `prdline.my.fctriPad 3+:cellular.fctr1`                               0.699777
## `prdline.my.fctriPadAir:cellular.fctr1`                               0.698589
## `prdline.my.fctriPadmini:cellular.fctr1`                              0.739887
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                           0.805923
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                          1.120308
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                          1.152360
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                         0.889867
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                         1.129040
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                        0.920104
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                     0.840295
##                                                                     z value
## (Intercept)                                                          -3.277
## `prdline.my.fctriPad 1`                                               0.772
## `prdline.my.fctriPad 2`                                               0.092
## `prdline.my.fctriPad 3+`                                              1.804
## prdline.my.fctriPadAir                                                0.668
## prdline.my.fctriPadmini                                               1.063
## `prdline.my.fctriPadmini 2+`                                          0.592
## biddable                                                              4.890
## `condition.fctrFor parts or not working`                              1.138
## `condition.fctrManufacturer refurbished`                             -0.073
## condition.fctrNew                                                    -1.859
## `condition.fctrNew other (see details)`                               0.693
## `condition.fctrSeller refurbished`                                    0.023
## D.terms.n.post.stop                                                  -0.056
## cellular.fctr1                                                       -1.162
## cellular.fctrUnknown                                                 -0.679
## `prdline.my.fctrUnknown:.clusterid.fctr2`                             1.449
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             -0.019
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              1.182
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                             0.360
## `prdline.my.fctriPadAir:.clusterid.fctr2`                             0.369
## `prdline.my.fctriPadmini:.clusterid.fctr2`                            1.332
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                         0.896
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            -1.137
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             -1.206
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             -0.465
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             0.409
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            -0.660
## `prdline.my.fctriPadmini:.clusterid.fctr3`                            0.920
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        -0.140
## `prdline.my.fctrUnknown:.clusterid.fctr4`                             0.000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             -2.927
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              2.598
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             1.237
## `prdline.my.fctriPadAir:.clusterid.fctr4`                             0.000
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           -0.278
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                         0.000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                              0.000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                              0.000
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                             0.017
## `prdline.my.fctriPadAir:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           -0.886
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                         0.000
## `prdline.my.fctrUnknown:.clusterid.fctr6`                             0.000
## `prdline.my.fctriPad 1:.clusterid.fctr6`                              0.000
## `prdline.my.fctriPad 2:.clusterid.fctr6`                              0.000
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                            -0.920
## `prdline.my.fctriPadAir:.clusterid.fctr6`                             0.000
## `prdline.my.fctriPadmini:.clusterid.fctr6`                            0.000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                         0.000
## `prdline.my.fctriPad 1:biddable`                                      2.066
## `prdline.my.fctriPad 2:biddable`                                      1.974
## `prdline.my.fctriPad 3+:biddable`                                    -0.076
## `prdline.my.fctriPadAir:biddable`                                     1.573
## `prdline.my.fctriPadmini:biddable`                                    0.711
## `prdline.my.fctriPadmini 2+:biddable`                                -0.337
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`        1.426
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`        0.254
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      -0.778
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      -0.189
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     -1.970
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  -0.774
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`        0.000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`        0.000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      -0.173
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`       1.083
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     -0.375
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`  -0.368
## `prdline.my.fctriPad 1:condition.fctrNew`                            -1.157
## `prdline.my.fctriPad 2:condition.fctrNew`                             0.000
## `prdline.my.fctriPad 3+:condition.fctrNew`                           -0.400
## `prdline.my.fctriPadAir:condition.fctrNew`                            0.224
## `prdline.my.fctriPadmini:condition.fctrNew`                           0.777
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                        1.075
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        -0.394
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        -0.590
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`        0.011
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`        0.466
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      -1.208
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   -1.885
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              1.647
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             -0.095
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            -0.744
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`             0.148
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           -1.246
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        -1.217
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                           0.624
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          -0.549
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         -1.761
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         -1.059
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        -0.444
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     -0.520
## `prdline.my.fctriPad 1:cellular.fctr1`                                0.522
## `prdline.my.fctriPad 2:cellular.fctr1`                                0.054
## `prdline.my.fctriPad 3+:cellular.fctr1`                               0.772
## `prdline.my.fctriPadAir:cellular.fctr1`                               0.605
## `prdline.my.fctriPadmini:cellular.fctr1`                              1.228
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                           1.104
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         -0.336
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         -0.686
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        -0.545
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                         0.212
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                        0.226
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                     0.035
##                                                                     Pr(>|z|)
## (Intercept)                                                          0.00105
## `prdline.my.fctriPad 1`                                              0.44000
## `prdline.my.fctriPad 2`                                              0.92695
## `prdline.my.fctriPad 3+`                                             0.07129
## prdline.my.fctriPadAir                                               0.50430
## prdline.my.fctriPadmini                                              0.28765
## `prdline.my.fctriPadmini 2+`                                         0.55388
## biddable                                                            1.01e-06
## `condition.fctrFor parts or not working`                             0.25526
## `condition.fctrManufacturer refurbished`                             0.94181
## condition.fctrNew                                                    0.06299
## `condition.fctrNew other (see details)`                              0.48801
## `condition.fctrSeller refurbished`                                   0.98140
## D.terms.n.post.stop                                                  0.95548
## cellular.fctr1                                                       0.24521
## cellular.fctrUnknown                                                 0.49726
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            0.14727
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             0.98499
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             0.23709
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            0.71859
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            0.71180
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           0.18272
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        0.37016
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            0.25561
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             0.22777
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             0.64210
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            0.68273
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            0.50924
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           0.35733
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        0.88835
## `prdline.my.fctrUnknown:.clusterid.fctr4`                            1.00000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             0.00343
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             0.00937
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            0.21595
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            1.00000
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           0.78082
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                        1.00000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                            1.00000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                             1.00000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             1.00000
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            0.98679
## `prdline.my.fctriPadAir:.clusterid.fctr5`                            1.00000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           0.37540
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                        1.00000
## `prdline.my.fctrUnknown:.clusterid.fctr6`                            1.00000
## `prdline.my.fctriPad 1:.clusterid.fctr6`                             1.00000
## `prdline.my.fctriPad 2:.clusterid.fctr6`                             1.00000
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                            0.35760
## `prdline.my.fctriPadAir:.clusterid.fctr6`                            1.00000
## `prdline.my.fctriPadmini:.clusterid.fctr6`                           1.00000
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                        1.00000
## `prdline.my.fctriPad 1:biddable`                                     0.03881
## `prdline.my.fctriPad 2:biddable`                                     0.04840
## `prdline.my.fctriPad 3+:biddable`                                    0.93905
## `prdline.my.fctriPadAir:biddable`                                    0.11564
## `prdline.my.fctriPadmini:biddable`                                   0.47693
## `prdline.my.fctriPadmini 2+:biddable`                                0.73576
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       0.15373
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       0.79965
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      0.43683
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      0.84975
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     0.04879
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  0.43902
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`       1.00000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`       1.00000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      0.86235
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      0.27868
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     0.70791
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`  0.71275
## `prdline.my.fctriPad 1:condition.fctrNew`                            0.24716
## `prdline.my.fctriPad 2:condition.fctrNew`                            1.00000
## `prdline.my.fctriPad 3+:condition.fctrNew`                           0.68893
## `prdline.my.fctriPadAir:condition.fctrNew`                           0.82298
## `prdline.my.fctriPadmini:condition.fctrNew`                          0.43707
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       0.28253
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        0.69369
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        0.55502
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       0.99152
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       0.64101
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      0.22692
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   0.05940
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             0.09953
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             0.92458
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            0.45714
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            0.88219
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           0.21278
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        0.22366
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          0.53261
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          0.58283
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         0.07827
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         0.28946
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        0.65691
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     0.60279
## `prdline.my.fctriPad 1:cellular.fctr1`                               0.60155
## `prdline.my.fctriPad 2:cellular.fctr1`                               0.95664
## `prdline.my.fctriPad 3+:cellular.fctr1`                              0.43983
## `prdline.my.fctriPadAir:cellular.fctr1`                              0.54543
## `prdline.my.fctriPadmini:cellular.fctr1`                             0.21960
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          0.26940
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         0.73686
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         0.49277
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        0.58592
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        0.83233
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       0.82085
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    0.97196
##                                                                        
## (Intercept)                                                         ** 
## `prdline.my.fctriPad 1`                                                
## `prdline.my.fctriPad 2`                                                
## `prdline.my.fctriPad 3+`                                            .  
## prdline.my.fctriPadAir                                                 
## prdline.my.fctriPadmini                                                
## `prdline.my.fctriPadmini 2+`                                           
## biddable                                                            ***
## `condition.fctrFor parts or not working`                               
## `condition.fctrManufacturer refurbished`                               
## condition.fctrNew                                                   .  
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
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            ** 
## `prdline.my.fctriPad 2:.clusterid.fctr4`                            ** 
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                              
## `prdline.my.fctriPadAir:.clusterid.fctr4`                              
## `prdline.my.fctriPadmini:.clusterid.fctr4`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                          
## `prdline.my.fctrUnknown:.clusterid.fctr5`                              
## `prdline.my.fctriPad 1:.clusterid.fctr5`                               
## `prdline.my.fctriPad 2:.clusterid.fctr5`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                              
## `prdline.my.fctriPadAir:.clusterid.fctr5`                              
## `prdline.my.fctriPadmini:.clusterid.fctr5`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                          
## `prdline.my.fctrUnknown:.clusterid.fctr6`                              
## `prdline.my.fctriPad 1:.clusterid.fctr6`                               
## `prdline.my.fctriPad 2:.clusterid.fctr6`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr6`                              
## `prdline.my.fctriPadAir:.clusterid.fctr6`                              
## `prdline.my.fctriPadmini:.clusterid.fctr6`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr6`                          
## `prdline.my.fctriPad 1:biddable`                                    *  
## `prdline.my.fctriPad 2:biddable`                                    *  
## `prdline.my.fctriPad 3+:biddable`                                      
## `prdline.my.fctriPadAir:biddable`                                      
## `prdline.my.fctriPadmini:biddable`                                     
## `prdline.my.fctriPadmini 2+:biddable`                                  
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
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`        
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  .  
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`            .  
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`               
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`              
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`             
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`          
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                            
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        .  
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                           
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                          
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                       
## `prdline.my.fctriPad 1:cellular.fctr1`                                 
## `prdline.my.fctriPad 2:cellular.fctr1`                                 
## `prdline.my.fctriPad 3+:cellular.fctr1`                                
## `prdline.my.fctriPadAir:cellular.fctr1`                                
## `prdline.my.fctriPadmini:cellular.fctr1`                               
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
## Residual deviance:  896.21  on 869  degrees of freedom
## AIC: 1106.2
## 
## Number of Fisher Scoring iterations: 16
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-69.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6732075
## 3        0.2 0.7394808
## 4        0.3 0.7742594
## 5        0.4 0.7789934
## 6        0.5 0.7683616
## 7        0.6 0.7544484
## 8        0.7 0.7244094
## 9        0.8 0.6160584
## 10       0.9 0.2509653
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-70.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.bayesglm.N
## 1         N                              416
## 2         Y                               94
##   sold.fctr.predict.csm.bayesglm.Y
## 1                              108
## 2                              356
##          Prediction
## Reference   N   Y
##         N 416 108
##         Y  94 356
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.926078e-01   5.837219e-01   7.657655e-01   8.176677e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.015605e-61   3.603613e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-71.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6583748
## 3        0.2 0.6894531
## 4        0.3 0.7144482
## 5        0.4 0.7310513
## 6        0.5 0.7263556
## 7        0.6 0.7043364
## 8        0.7 0.6209913
## 9        0.8 0.5655608
## 10       0.9 0.1802575
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-72.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.bayesglm.N
## 1         N                              366
## 2         Y                              111
##   sold.fctr.predict.csm.bayesglm.Y
## 1                              109
## 2                              299
##          Prediction
## Reference   N   Y
##         N 366 109
##         Y 111 299
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.514124e-01   4.999615e-01   7.215645e-01   7.795759e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.010199e-39   9.462474e-01 
##       model_id model_method
## 1 csm.bayesglm     bayesglm
##                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.058                 0.424
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.862676                    0.4       0.7789934        0.7576923
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7657655             0.8176677     0.5127898   0.7749705
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7310513        0.7514124
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7215645             0.7795759     0.4999615    1106.207
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008104349      0.01872738
##                              N importance
## biddable            100.000000 100.000000
## prdline.my.fctr      34.103141  34.103141
## .clusterid.fctr      10.507475  10.507475
## D.terms.n.post.stop   6.216049   6.216049
## cellular.fctr         5.046058   5.046058
## condition.fctr        0.000000   0.000000
## [1] "fitting model: csm.glmnet"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 0.55, lambda = 0.0544 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: lambda
```

![](ebayipads_color_files/figure-html/fit.models_1-73.png) ![](ebayipads_color_files/figure-html/fit.models_1-74.png) 

```
##             Length Class      Mode     
## a0            95   -none-     numeric  
## beta        9880   dgCMatrix  S4       
## df            95   -none-     numeric  
## dim            2   -none-     numeric  
## lambda        95   -none-     numeric  
## dev.ratio     95   -none-     numeric  
## nulldev        1   -none-     numeric  
## npasses        1   -none-     numeric  
## jerr           1   -none-     numeric  
## offset         1   -none-     logical  
## classnames     2   -none-     character
## call           5   -none-     call     
## nobs           1   -none-     numeric  
## lambdaOpt      1   -none-     numeric  
## xNames       104   -none-     character
## problemType    1   -none-     character
## tuneValue      2   data.frame list     
## obsLevels      2   -none-     character
## [1] "min lambda > lambdaOpt:"
##                                                  (Intercept) 
##                                                 -1.007559311 
##                                                     biddable 
##                                                  1.779236846 
##                                            condition.fctrNew 
##                                                 -0.166509715 
##                                         cellular.fctrUnknown 
##                                                 -0.048814838 
##                       prdline.my.fctriPad 2:.clusterid.fctr4 
##                                                  0.348938535 
##                               prdline.my.fctriPad 1:biddable 
##                                                  0.283880312 
##                               prdline.my.fctriPad 2:biddable 
##                                                  0.258093898 
## prdline.my.fctriPad 1:condition.fctrFor parts or not working 
##                                                  0.598206083 
##                   prdline.my.fctriPad 3+:D.terms.n.post.stop 
##                                                 -0.005815295 
## [1] "max lambda < lambdaOpt:"
##                                                       (Intercept) 
##                                                       -1.84845980 
##                                             prdline.my.fctriPad 1 
##                                                        0.76424893 
##                                             prdline.my.fctriPad 2 
##                                                        0.24371339 
##                                            prdline.my.fctriPad 3+ 
##                                                        1.49877412 
##                                            prdline.my.fctriPadAir 
##                                                        0.68168898 
##                                           prdline.my.fctriPadmini 
##                                                        0.94481896 
##                                        prdline.my.fctriPadmini 2+ 
##                                                        0.84960693 
##                                                          biddable 
##                                                        2.31363756 
##                            condition.fctrFor parts or not working 
##                                                        1.05296785 
##                                                 condition.fctrNew 
##                                                       -1.05318838 
##                             condition.fctrNew other (see details) 
##                                                        2.58918213 
##                                  condition.fctrSeller refurbished 
##                                                        0.52474944 
##                                               D.terms.n.post.stop 
##                                                        0.01926081 
##                                                    cellular.fctr1 
##                                                       -2.11025050 
##                                              cellular.fctrUnknown 
##                                                       -0.40150057 
##                           prdline.my.fctrUnknown:.clusterid.fctr2 
##                                                        1.08105059 
##                            prdline.my.fctriPad 1:.clusterid.fctr2 
##                                                       -0.59766521 
##                            prdline.my.fctriPad 2:.clusterid.fctr2 
##                                                        1.57803269 
##                           prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                                        0.26715744 
##                           prdline.my.fctriPadAir:.clusterid.fctr2 
##                                                        0.37320221 
##                          prdline.my.fctriPadmini:.clusterid.fctr2 
##                                                        1.14938203 
##                       prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                                        1.83740653 
##                           prdline.my.fctrUnknown:.clusterid.fctr3 
##                                                       -1.11129890 
##                            prdline.my.fctriPad 1:.clusterid.fctr3 
##                                                       -2.05930359 
##                            prdline.my.fctriPad 2:.clusterid.fctr3 
##                                                       -0.14138251 
##                           prdline.my.fctriPad 3+:.clusterid.fctr3 
##                                                        0.35479063 
##                           prdline.my.fctriPadAir:.clusterid.fctr3 
##                                                       -0.52529553 
##                          prdline.my.fctriPadmini:.clusterid.fctr3 
##                                                        0.96421664 
##                       prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                                        0.06399376 
##                            prdline.my.fctriPad 1:.clusterid.fctr4 
##                                                       -3.98588144 
##                            prdline.my.fctriPad 2:.clusterid.fctr4 
##                                                        4.06349538 
##                           prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                                        1.01897070 
##                          prdline.my.fctriPadmini:.clusterid.fctr4 
##                                                       -0.05059401 
##                           prdline.my.fctriPad 3+:.clusterid.fctr5 
##                                                        0.04042165 
##                          prdline.my.fctriPadmini:.clusterid.fctr5 
##                                                       -0.94233411 
##                           prdline.my.fctriPad 3+:.clusterid.fctr6 
##                                                       -5.51692804 
##                                    prdline.my.fctriPad 1:biddable 
##                                                        1.53085043 
##                                    prdline.my.fctriPad 2:biddable 
##                                                        1.23456311 
##                                   prdline.my.fctriPad 3+:biddable 
##                                                       -0.33006392 
##                                   prdline.my.fctriPadAir:biddable 
##                                                        0.67716427 
##                                  prdline.my.fctriPadmini:biddable 
##                                                        0.26579309 
##                               prdline.my.fctriPadmini 2+:biddable 
##                                                       -0.49404830 
##      prdline.my.fctriPad 1:condition.fctrFor parts or not working 
##                                                        1.31505108 
##      prdline.my.fctriPad 2:condition.fctrFor parts or not working 
##                                                        0.04720898 
##     prdline.my.fctriPad 3+:condition.fctrFor parts or not working 
##                                                       -1.10049778 
##     prdline.my.fctriPadAir:condition.fctrFor parts or not working 
##                                                       -0.65164355 
##    prdline.my.fctriPadmini:condition.fctrFor parts or not working 
##                                                       -2.04524195 
## prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working 
##                                                       -6.49351983 
##     prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished 
##                                                       -0.39362636 
##     prdline.my.fctriPadAir:condition.fctrManufacturer refurbished 
##                                                        1.65721588 
##    prdline.my.fctriPadmini:condition.fctrManufacturer refurbished 
##                                                       -0.71681748 
## prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished 
##                                                       -5.43641398 
##                           prdline.my.fctriPad 1:condition.fctrNew 
##                                                       -7.20520477 
##                          prdline.my.fctriPad 3+:condition.fctrNew 
##                                                       -0.69792998 
##                          prdline.my.fctriPadAir:condition.fctrNew 
##                                                        0.09328170 
##                         prdline.my.fctriPadmini:condition.fctrNew 
##                                                        0.55232416 
##                      prdline.my.fctriPadmini 2+:condition.fctrNew 
##                                                        0.66313960 
##       prdline.my.fctriPad 1:condition.fctrNew other (see details) 
##                                                       -3.51737875 
##       prdline.my.fctriPad 2:condition.fctrNew other (see details) 
##                                                       -7.36641521 
##      prdline.my.fctriPad 3+:condition.fctrNew other (see details) 
##                                                       -2.08783863 
##      prdline.my.fctriPadAir:condition.fctrNew other (see details) 
##                                                       -1.47077618 
##     prdline.my.fctriPadmini:condition.fctrNew other (see details) 
##                                                       -4.16372137 
##  prdline.my.fctriPadmini 2+:condition.fctrNew other (see details) 
##                                                       -4.98085945 
##            prdline.my.fctriPad 1:condition.fctrSeller refurbished 
##                                                        1.94195941 
##            prdline.my.fctriPad 2:condition.fctrSeller refurbished 
##                                                       -0.64355020 
##           prdline.my.fctriPad 3+:condition.fctrSeller refurbished 
##                                                       -1.35985695 
##           prdline.my.fctriPadAir:condition.fctrSeller refurbished 
##                                                       -0.26033958 
##          prdline.my.fctriPadmini:condition.fctrSeller refurbished 
##                                                       -2.04937581 
##       prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished 
##                                                       -7.81696445 
##                         prdline.my.fctriPad 1:D.terms.n.post.stop 
##                                                        0.11340381 
##                         prdline.my.fctriPad 2:D.terms.n.post.stop 
##                                                       -0.14075275 
##                        prdline.my.fctriPad 3+:D.terms.n.post.stop 
##                                                       -0.20061215 
##                        prdline.my.fctriPadAir:D.terms.n.post.stop 
##                                                       -0.14406935 
##                       prdline.my.fctriPadmini:D.terms.n.post.stop 
##                                                       -0.08016961 
##                    prdline.my.fctriPadmini 2+:D.terms.n.post.stop 
##                                                       -0.15298258 
##                              prdline.my.fctriPad 1:cellular.fctr1 
##                                                        1.85407557 
##                              prdline.my.fctriPad 2:cellular.fctr1 
##                                                        1.48191584 
##                             prdline.my.fctriPad 3+:cellular.fctr1 
##                                                        1.92386040 
##                             prdline.my.fctriPadAir:cellular.fctr1 
##                                                        1.84032411 
##                            prdline.my.fctriPadmini:cellular.fctr1 
##                                                        2.35197829 
##                         prdline.my.fctriPadmini 2+:cellular.fctr1 
##                                                        2.45019432 
##                        prdline.my.fctriPad 1:cellular.fctrUnknown 
##                                                       -0.39042088 
##                        prdline.my.fctriPad 2:cellular.fctrUnknown 
##                                                       -1.03539067 
##                       prdline.my.fctriPad 3+:cellular.fctrUnknown 
##                                                       -0.48259476 
##                       prdline.my.fctriPadAir:cellular.fctrUnknown 
##                                                        0.46061587 
##                      prdline.my.fctriPadmini:cellular.fctrUnknown 
##                                                        0.40735463 
##                   prdline.my.fctriPadmini 2+:cellular.fctrUnknown 
##                                                        0.12416427 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-75.png) 

```
##    threshold    f.score
## 1        0.0 0.63202247
## 2        0.1 0.63202247
## 3        0.2 0.63202247
## 4        0.3 0.76619100
## 5        0.4 0.76158940
## 6        0.5 0.75555556
## 7        0.6 0.75555556
## 8        0.7 0.40747029
## 9        0.8 0.01762115
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](ebayipads_color_files/figure-html/fit.models_1-76.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.glmnet.N sold.fctr.predict.csm.glmnet.Y
## 1         N                            412                            112
## 2         Y                            101                            349
##          Prediction
## Reference   N   Y
##         N 412 112
##         Y 101 349
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.813142e-01   5.608471e-01   7.540110e-01   8.069046e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.367724e-56   4.932248e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-77.png) 

```
##    threshold    f.score
## 1        0.0 0.63320463
## 2        0.1 0.63320463
## 3        0.2 0.63320463
## 4        0.3 0.75434243
## 5        0.4 0.75561097
## 6        0.5 0.75282309
## 7        0.6 0.75282309
## 8        0.7 0.34404537
## 9        0.8 0.03333333
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](ebayipads_color_files/figure-html/fit.models_1-78.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.glmnet.N sold.fctr.predict.csm.glmnet.Y
## 1         N                            386                             89
## 2         Y                            107                            303
##          Prediction
## Reference   N   Y
##         N 386  89
##         Y 107 303
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.785311e-01   5.533181e-01   7.497051e-01   8.054889e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.636469e-50   2.246386e-01 
##     model_id model_method
## 1 csm.glmnet       glmnet
##                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      2.726                 0.334
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8162405                    0.3        0.766191        0.7741374
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.754011             0.8069046     0.5456204    0.781638
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4        0.755611        0.7785311
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7497051             0.8054889     0.5533181
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008498508      0.01784782
##                                                              importance
## biddable                                                      100.00000
## prdline.my.fctriPad 1:condition.fctrFor parts or not working   44.76341
## prdline.my.fctriPad 2:.clusterid.fctr4                         32.34231
## prdline.my.fctriPad 1:biddable                                 24.60104
## prdline.my.fctriPad 2:biddable                                 23.41508
## prdline.my.fctriPad 1:condition.fctrSeller refurbished         10.87551
## [1] "fitting model: csm.rpart"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00333 on full training set
```

![](ebayipads_color_files/figure-html/fit.models_1-79.png) ![](ebayipads_color_files/figure-html/fit.models_1-80.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.003333333      1 0.4888889
## 
## Variable importance
##                         biddable  prdline.my.fctriPad 3+:biddable 
##                               55                               10 
##  prdline.my.fctriPadAir:biddable   prdline.my.fctriPad 1:biddable 
##                                9                                9 
## prdline.my.fctriPadmini:biddable   prdline.my.fctriPad 2:biddable 
##                                9                                8 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable                        < 0.5 to the left,  improve=144.14990, (0 missing)
##       prdline.my.fctriPad 1:biddable  < 0.5 to the left,  improve= 25.37925, (0 missing)
##       prdline.my.fctriPad 2:biddable  < 0.5 to the left,  improve= 22.83949, (0 missing)
##       prdline.my.fctriPadAir:biddable < 0.5 to the left,  improve= 16.00553, (0 missing)
##       prdline.my.fctriPad 3+:biddable < 0.5 to the left,  improve= 14.01384, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPad 3+:biddable  < 0.5 to the left,  agree=0.624, adj=0.187, (0 split)
##       prdline.my.fctriPadAir:biddable  < 0.5 to the left,  agree=0.618, adj=0.173, (0 split)
##       prdline.my.fctriPad 1:biddable   < 0.5 to the left,  agree=0.613, adj=0.162, (0 split)
##       prdline.my.fctriPadmini:biddable < 0.5 to the left,  agree=0.611, adj=0.158, (0 split)
##       prdline.my.fctriPad 2:biddable   < 0.5 to the left,  agree=0.606, adj=0.147, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 974 450 N (0.5379877 0.4620123)  
##   2) biddable< 0.5 524 110 N (0.7900763 0.2099237) *
##   3) biddable>=0.5 450 110 Y (0.2444444 0.7555556) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.models_1-81.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7555556
## 5        0.4 0.7555556
## 6        0.5 0.7555556
## 7        0.6 0.7555556
## 8        0.7 0.7555556
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-82.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.rpart.N sold.fctr.predict.csm.rpart.Y
## 1         N                           414                           110
## 2         Y                           110                           340
##          Prediction
## Reference   N   Y
##         N 414 110
##         Y 110 340
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.741273e-01   5.456319e-01   7.465456e-01   8.000405e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   7.516456e-53   1.000000e+00 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-83.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7528231
## 5        0.4 0.7528231
## 6        0.5 0.7528231
## 7        0.6 0.7528231
## 8        0.7 0.7528231
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](ebayipads_color_files/figure-html/fit.models_1-84.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.rpart.N sold.fctr.predict.csm.rpart.Y
## 1         N                           388                            87
## 2         Y                           110                           300
##          Prediction
## Reference   N   Y
##         N 388  87
##         Y 110 300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.774011e-01   5.506630e-01   7.485294e-01   8.044124e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.956102e-50   1.170130e-01 
##    model_id model_method
## 1 csm.rpart        rpart
##                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.698                 0.075
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7728159                    0.7       0.7555556        0.7669389
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7465456             0.8000405     0.5300413   0.7742747
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.7528231        0.7774011
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7485294             0.8044124      0.550663
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002017353     0.007393625
##                                 importance
## biddable                        100.000000
## prdline.my.fctriPad 1:biddable   17.606151
## prdline.my.fctriPad 2:biddable   15.844266
## prdline.my.fctriPadAir:biddable  11.103395
## prdline.my.fctriPad 3+:biddable   9.721717
## `prdline.my.fctriPad 1`           0.000000
## [1] "fitting model: csm.rf"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 53 on full training set
```

![](ebayipads_color_files/figure-html/fit.models_1-85.png) ![](ebayipads_color_files/figure-html/fit.models_1-86.png) 

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

![](ebayipads_color_files/figure-html/fit.models_1-87.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7834275
## 3        0.2 0.8244734
## 4        0.3 0.8550420
## 5        0.4 0.8758170
## 6        0.5 0.8757127
## 7        0.6 0.8667439
## 8        0.7 0.8256659
## 9        0.8 0.7779204
## 10       0.9 0.6961326
## 11       1.0 0.3824561
```

![](ebayipads_color_files/figure-html/fit.models_1-88.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.rf.N sold.fctr.predict.csm.rf.Y
## 1         N                        458                         66
## 2         Y                         48                        402
##          Prediction
## Reference   N   Y
##         N 458  66
##         Y  48 402
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.829569e-01   7.652178e-01   8.610882e-01   9.024774e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##  4.046280e-119   1.113407e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_color_files/figure-html/fit.models_1-89.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6957404
## 3        0.2 0.7063922
## 4        0.3 0.7129630
## 5        0.4 0.7024390
## 6        0.5 0.6991037
## 7        0.6 0.6905710
## 8        0.7 0.6583679
## 9        0.8 0.6171761
## 10       0.9 0.5651491
## 11       1.0 0.2510288
```

![](ebayipads_color_files/figure-html/fit.models_1-90.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.rf.N sold.fctr.predict.csm.rf.Y
## 1         N                        329                        146
## 2         Y                        102                        308
##          Prediction
## Reference   N   Y
##         N 329 146
##         Y 102 308
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.197740e-01   4.406158e-01   6.889217e-01   7.491552e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.973065e-29   6.323781e-03 
##   model_id model_method
## 1   csm.rf           rf
##                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     22.131                 7.953
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.907888                    0.4        0.875817         0.728968
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8610882             0.9024774     0.4528104   0.7665417
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3        0.712963         0.719774
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.6889217             0.7491552     0.4406158
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0167801       0.0311865
##                                            importance
## biddable                                   100.000000
## D.terms.n.post.stop                         22.608839
## prdline.my.fctriPad 3+:D.terms.n.post.stop   8.017530
## cellular.fctrUnknown                         6.252631
## prdline.my.fctriPad 1:biddable               5.977706
## cellular.fctr1                               5.750988
```

```r
###

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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           biddable, startprice.diff
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      biddable, startprice.diff
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                biddable, startprice.diff
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.diff
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.npnct24.log, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:D.terms.n.post.stop.log, biddable:cellular.fctr, biddable:D.nwrds.unq.log
## Low.cor.X.glm                                                                                                                                                                                                                                                                                                                                                                                              biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, color.fctr, D.npnct08.log, prdline.my.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glm                                                                                                   biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm                                                                                              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet                                                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart                                                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                                                                                                   biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glm            D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.bayesglm       D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glmnet         D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rpart         D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rf            D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## csm.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.bayesglm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.glmnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rf                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##                               max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                           0                      0.438
## Random.myrandom_classfr                     0                      0.253
## Max.cor.Y.cv.0.rpart                        0                      0.610
## Max.cor.Y.cv.0.cp.0.rpart                   0                      0.488
## Max.cor.Y.rpart                             3                      1.072
## Max.cor.Y.glm                               1                      1.004
## Interact.High.cor.Y.glm                     1                      1.047
## Low.cor.X.glm                               1                      1.490
## All.X.glm                                   1                      1.801
## All.X.bayesglm                              1                      3.602
## All.X.glmnet                                9                      5.826
## All.X.no.rnorm.rpart                        3                      1.673
## All.X.no.rnorm.rf                           3                     16.547
## All.Interact.X.glm                          1                      1.972
## All.Interact.X.bayesglm                     1                      2.669
## All.Interact.X.glmnet                       9                      9.088
## All.Interact.X.no.rnorm.rpart               3                      1.854
## All.Interact.X.no.rnorm.rf                  3                     20.893
## csm.glm                                     1                      1.892
## csm.bayesglm                                1                      2.058
## csm.glmnet                                  9                      2.726
## csm.rpart                                   3                      1.698
## csm.rf                                      3                     22.131
##                               min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                             0.002   0.5000000
## Random.myrandom_classfr                       0.001   0.5071756
## Max.cor.Y.cv.0.rpart                          0.012   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                     0.009   0.8855386
## Max.cor.Y.rpart                               0.012   0.8243427
## Max.cor.Y.glm                                 0.011   0.8586302
## Interact.High.cor.Y.glm                       0.019   0.8580831
## Low.cor.X.glm                                 0.182   0.8858185
## All.X.glm                                     0.258   0.8936980
## All.X.bayesglm                                0.371   0.8912129
## All.X.glmnet                                  1.167   0.8592409
## All.X.no.rnorm.rpart                          0.069   0.8243427
## All.X.no.rnorm.rf                             5.082   1.0000000
## All.Interact.X.glm                            0.471   0.9076718
## All.Interact.X.bayesglm                       0.644   0.9015225
## All.Interact.X.glmnet                         0.736   0.8845377
## All.Interact.X.no.rnorm.rpart                 0.093   0.8243427
## All.Interact.X.no.rnorm.rf                    7.009   1.0000000
## csm.glm                                       0.434   0.8645971
## csm.bayesglm                                  0.424   0.8626760
## csm.glmnet                                    0.334   0.8162405
## csm.rpart                                     0.075   0.7728159
## csm.rf                                        7.953   0.9078880
##                               opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6320225
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.5       0.8113208
## Max.cor.Y.rpart                                  0.9       0.7633987
## Max.cor.Y.glm                                    0.7       0.7639594
## Interact.High.cor.Y.glm                          0.7       0.7615385
## Low.cor.X.glm                                    0.5       0.7977143
## All.X.glm                                        0.5       0.8132875
## All.X.bayesglm                                   0.5       0.8127128
## All.X.glmnet                                     0.6       0.7666667
## All.X.no.rnorm.rpart                             0.9       0.7633987
## All.X.no.rnorm.rf                                0.6       1.0000000
## All.Interact.X.glm                               0.4       0.8219485
## All.Interact.X.bayesglm                          0.6       0.8186275
## All.Interact.X.glmnet                            0.6       0.7965044
## All.Interact.X.no.rnorm.rpart                    0.9       0.7633987
## All.Interact.X.no.rnorm.rf                       0.5       1.0000000
## csm.glm                                          0.3       0.7770961
## csm.bayesglm                                     0.4       0.7789934
## csm.glmnet                                       0.3       0.7661910
## csm.rpart                                        0.7       0.7555556
## csm.rf                                           0.4       0.8758170
##                               max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                    0.5379877             0.5060896
## Random.myrandom_classfr              0.4620123             0.4303445
## Max.cor.Y.cv.0.rpart                 0.5379877             0.5060896
## Max.cor.Y.cv.0.cp.0.rpart            0.8357290             0.8109392
## Max.cor.Y.rpart                      0.7833523             0.7882906
## Max.cor.Y.glm                        0.7720798             0.7829169
## Interact.High.cor.Y.glm              0.7741374             0.7829169
## Low.cor.X.glm                        0.7710320             0.7925945
## All.X.glm                            0.7679550             0.8076954
## All.X.bayesglm                       0.7638683             0.8055345
## All.X.glmnet                         0.7864482             0.7721896
## All.X.no.rnorm.rpart                 0.8018645             0.7882906
## All.X.no.rnorm.rf                    0.8018392             0.9962198
## All.Interact.X.glm                   0.7494967             0.8120210
## All.Interact.X.bayesglm              0.7813200             0.8239439
## All.Interact.X.glmnet                0.7977651             0.8076954
## All.Interact.X.no.rnorm.rpart        0.8018645             0.7882906
## All.Interact.X.no.rnorm.rf           0.7823457             0.9962198
## csm.glm                              0.7310098             0.7486774
## csm.bayesglm                         0.7576923             0.7657655
## csm.glmnet                           0.7741374             0.7540110
## csm.rpart                            0.7669389             0.7465456
## csm.rf                               0.7289680             0.8610882
##                               max.AccuracyUpper.fit max.Kappa.fit
## MFO.myMFO_classfr                         0.5696555     0.0000000
## Random.myrandom_classfr                   0.4939104     0.0000000
## Max.cor.Y.cv.0.rpart                      0.5696555     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.8584688     0.6668320
## Max.cor.Y.rpart                           0.8381305     0.5585199
## Max.cor.Y.glm                             0.8332690     0.5402037
## Interact.High.cor.Y.glm                   0.8332690     0.5436041
## Low.cor.X.glm                             0.8420145     0.5386776
## All.X.glm                                 0.8555717     0.5327262
## All.X.bayesglm                            0.8536386     0.5243770
## All.X.glmnet                              0.8235260     0.5692702
## All.X.no.rnorm.rpart                      0.8381305     0.5940341
## All.X.no.rnorm.rf                         1.0000000     0.5970410
## All.Interact.X.glm                        0.8594339     0.4949703
## All.Interact.X.bayesglm                   0.8700277     0.5584387
## All.Interact.X.glmnet                     0.8555717     0.5903552
## All.Interact.X.no.rnorm.rpart             0.8381305     0.5940341
## All.Interact.X.no.rnorm.rf                1.0000000     0.5584003
## csm.glm                                   0.8020028     0.4584371
## csm.bayesglm                              0.8176677     0.5127898
## csm.glmnet                                0.8069046     0.5456204
## csm.rpart                                 0.8000405     0.5300413
## csm.rf                                    0.9024774     0.4528104
##                               max.auc.OOB opt.prob.threshold.OOB
## MFO.myMFO_classfr               0.5000000                    0.5
## Random.myrandom_classfr         0.5191913                    0.4
## Max.cor.Y.cv.0.rpart            0.5000000                    0.5
## Max.cor.Y.cv.0.cp.0.rpart       0.8187625                    0.5
## Max.cor.Y.rpart                 0.8129705                    0.3
## Max.cor.Y.glm                   0.8633582                    0.6
## Interact.High.cor.Y.glm         0.8615815                    0.6
## Low.cor.X.glm                   0.8465571                    0.5
## All.X.glm                       0.8431528                    0.5
## All.X.bayesglm                  0.8462850                    0.5
## All.X.glmnet                    0.8631938                    0.6
## All.X.no.rnorm.rpart            0.8129705                    0.3
## All.X.no.rnorm.rf               0.8637792                    0.5
## All.Interact.X.glm              0.8344904                    0.5
## All.Interact.X.bayesglm         0.8364519                    0.6
## All.Interact.X.glmnet           0.8510347                    0.5
## All.Interact.X.no.rnorm.rpart   0.8129705                    0.3
## All.Interact.X.no.rnorm.rf      0.8559487                    0.6
## csm.glm                         0.7577818                    0.4
## csm.bayesglm                    0.7749705                    0.4
## csm.glmnet                      0.7816380                    0.4
## csm.rpart                       0.7742747                    0.7
## csm.rf                          0.7665417                    0.3
##                               max.f.score.OOB max.Accuracy.OOB
## MFO.myMFO_classfr                   0.0000000        0.5367232
## Random.myrandom_classfr             0.6332046        0.4632768
## Max.cor.Y.cv.0.rpart                0.0000000        0.5367232
## Max.cor.Y.cv.0.cp.0.rpart           0.7551546        0.7853107
## Max.cor.Y.rpart                     0.7528231        0.7774011
## Max.cor.Y.glm                       0.7710526        0.8033898
## Interact.High.cor.Y.glm             0.7665782        0.8011299
## Low.cor.X.glm                       0.7733675        0.8000000
## All.X.glm                           0.7715736        0.7966102
## All.X.bayesglm                      0.7708067        0.7977401
## All.X.glmnet                        0.7751323        0.8079096
## All.X.no.rnorm.rpart                0.7528231        0.7774011
## All.X.no.rnorm.rf                   0.7854356        0.8135593
## All.Interact.X.glm                  0.7573813        0.7864407
## All.Interact.X.bayesglm             0.7573333        0.7943503
## All.Interact.X.glmnet               0.7630208        0.7943503
## All.Interact.X.no.rnorm.rpart       0.7528231        0.7774011
## All.Interact.X.no.rnorm.rf          0.7747253        0.8146893
## csm.glm                             0.7148014        0.7322034
## csm.bayesglm                        0.7310513        0.7514124
## csm.glmnet                          0.7556110        0.7785311
## csm.rpart                           0.7528231        0.7774011
## csm.rf                              0.7129630        0.7197740
##                               max.AccuracyLower.OOB max.AccuracyUpper.OOB
## MFO.myMFO_classfr                         0.5032294             0.5699717
## Random.myrandom_classfr                   0.4300283             0.4967706
## Max.cor.Y.cv.0.rpart                      0.5032294             0.5699717
## Max.cor.Y.cv.0.cp.0.rpart                 0.7567658             0.8119416
## Max.cor.Y.rpart                           0.7485294             0.8044124
## Max.cor.Y.glm                             0.7756483             0.8290941
## Interact.High.cor.Y.glm                   0.7732835             0.8269546
## Low.cor.X.glm                             0.7721016             0.8258843
## All.X.glm                                 0.7685578             0.8226715
## All.X.bayesglm                            0.7697388             0.8237428
## All.X.glmnet                              0.7803820             0.8333692
## All.X.no.rnorm.rpart                      0.7485294             0.8044124
## All.X.no.rnorm.rf                         0.7863067             0.8387053
## All.Interact.X.glm                        0.7579436             0.8130160
## All.Interact.X.bayesglm                   0.7661969             0.8205280
## All.Interact.X.glmnet                     0.7661969             0.8205280
## All.Interact.X.no.rnorm.rpart             0.7485294             0.8044124
## All.Interact.X.no.rnorm.rf                0.7874927             0.8397715
## csm.glm                                   0.7017229             0.7611290
## csm.bayesglm                              0.7215645             0.7795759
## csm.glmnet                                0.7497051             0.8054889
## csm.rpart                                 0.7485294             0.8044124
## csm.rf                                    0.6889217             0.7491552
##                               max.Kappa.OOB max.AccuracySD.fit
## MFO.myMFO_classfr                 0.0000000                 NA
## Random.myrandom_classfr           0.0000000                 NA
## Max.cor.Y.cv.0.rpart              0.0000000                 NA
## Max.cor.Y.cv.0.cp.0.rpart         0.5650993                 NA
## Max.cor.Y.rpart                   0.5506630        0.017790747
## Max.cor.Y.glm                     0.6006483        0.005815317
## Interact.High.cor.Y.glm           0.5956491        0.008498508
## Low.cor.X.glm                     0.5951959        0.015836744
## All.X.glm                         0.5888183        0.012778378
## All.X.bayesglm                    0.5906219        0.007372855
## All.X.glmnet                      0.6095656        0.004643467
## All.X.no.rnorm.rpart              0.5506630        0.013888207
## All.X.no.rnorm.rf                 0.6218781        0.014398891
## All.Interact.X.glm                0.5676063        0.012517513
## All.Interact.X.bayesglm           0.5815820        0.004951066
## All.Interact.X.glmnet             0.5828499        0.020634110
## All.Interact.X.no.rnorm.rpart     0.5506630        0.013888207
## All.Interact.X.no.rnorm.rf        0.6215582        0.006148854
## csm.glm                           0.4624886        0.018716928
## csm.bayesglm                      0.4999615        0.008104349
## csm.glmnet                        0.5533181        0.008498508
## csm.rpart                         0.5506630        0.002017353
## csm.rf                            0.4406158        0.016780098
##                               max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                          NA          NA
## Random.myrandom_classfr                    NA          NA
## Max.cor.Y.cv.0.rpart                       NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                  NA          NA
## Max.cor.Y.rpart                   0.033543790          NA
## Max.cor.Y.glm                     0.011249498    919.4841
## Interact.High.cor.Y.glm           0.018207150    931.8178
## Low.cor.X.glm                     0.032711928    938.3505
## All.X.glm                         0.026469200    948.3929
## All.X.bayesglm                    0.014708213    992.7758
## All.X.glmnet                      0.012261960          NA
## All.X.no.rnorm.rpart              0.029572029          NA
## All.X.no.rnorm.rf                 0.028547614          NA
## All.Interact.X.glm                0.028576855    942.9274
## All.Interact.X.bayesglm           0.012171076   1023.1966
## All.Interact.X.glmnet             0.042680167          NA
## All.Interact.X.no.rnorm.rpart     0.029572029          NA
## All.Interact.X.no.rnorm.rf        0.013203334          NA
## csm.glm                           0.043145565   1061.9948
## csm.bayesglm                      0.018727383   1106.2071
## csm.glmnet                        0.017847823          NA
## csm.rpart                         0.007393625          NA
## csm.rf                            0.031186496          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##               label step_major step_minor     bgn     end elapsed
## 11  fit.models_1_rf         11          0 256.413 327.355  70.942
## 12 fit.models_1_end         12          0 327.356      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1 161.264 327.363 166.099
## 12 fit.models          7          2 327.364      NA      NA
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           biddable, startprice.diff
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      biddable, startprice.diff
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                biddable, startprice.diff
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.diff
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        biddable, startprice.diff, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.npnct24.log, biddable:D.npnct06.log, biddable:D.ratio.nstopwrds.nwrds, biddable:D.nchrs.log, biddable:D.nwrds.log, biddable:D.terms.n.post.stop.log, biddable:cellular.fctr, biddable:D.nwrds.unq.log
## Low.cor.X.glm                                                                                                                                                                                                                                                                                                                                                                                              biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, color.fctr, D.npnct08.log, prdline.my.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glm                                                                                                   biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm                                                                                              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet                                                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart                                                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                                                                                                   biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, carrier.fctr, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.diff, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glm            D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.bayesglm       D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glmnet         D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, .rnorm, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rpart         D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rf            D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
## csm.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.bayesglm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.glmnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rf                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##                               max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                           0   0.5000000
## Random.myrandom_classfr                     0   0.5071756
## Max.cor.Y.cv.0.rpart                        0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                   0   0.8855386
## Max.cor.Y.rpart                             3   0.8243427
## Max.cor.Y.glm                               1   0.8586302
## Interact.High.cor.Y.glm                     1   0.8580831
## Low.cor.X.glm                               1   0.8858185
## All.X.glm                                   1   0.8936980
## All.X.bayesglm                              1   0.8912129
## All.X.glmnet                                9   0.8592409
## All.X.no.rnorm.rpart                        3   0.8243427
## All.X.no.rnorm.rf                           3   1.0000000
## All.Interact.X.glm                          1   0.9076718
## All.Interact.X.bayesglm                     1   0.9015225
## All.Interact.X.glmnet                       9   0.8845377
## All.Interact.X.no.rnorm.rpart               3   0.8243427
## All.Interact.X.no.rnorm.rf                  3   1.0000000
## csm.glm                                     1   0.8645971
## csm.bayesglm                                1   0.8626760
## csm.glmnet                                  9   0.8162405
## csm.rpart                                   3   0.7728159
## csm.rf                                      3   0.9078880
##                               opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6320225
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.5       0.8113208
## Max.cor.Y.rpart                                  0.9       0.7633987
## Max.cor.Y.glm                                    0.7       0.7639594
## Interact.High.cor.Y.glm                          0.7       0.7615385
## Low.cor.X.glm                                    0.5       0.7977143
## All.X.glm                                        0.5       0.8132875
## All.X.bayesglm                                   0.5       0.8127128
## All.X.glmnet                                     0.6       0.7666667
## All.X.no.rnorm.rpart                             0.9       0.7633987
## All.X.no.rnorm.rf                                0.6       1.0000000
## All.Interact.X.glm                               0.4       0.8219485
## All.Interact.X.bayesglm                          0.6       0.8186275
## All.Interact.X.glmnet                            0.6       0.7965044
## All.Interact.X.no.rnorm.rpart                    0.9       0.7633987
## All.Interact.X.no.rnorm.rf                       0.5       1.0000000
## csm.glm                                          0.3       0.7770961
## csm.bayesglm                                     0.4       0.7789934
## csm.glmnet                                       0.3       0.7661910
## csm.rpart                                        0.7       0.7555556
## csm.rf                                           0.4       0.8758170
##                               max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                    0.5379877     0.0000000   0.5000000
## Random.myrandom_classfr              0.4620123     0.0000000   0.5191913
## Max.cor.Y.cv.0.rpart                 0.5379877     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart            0.8357290     0.6668320   0.8187625
## Max.cor.Y.rpart                      0.7833523     0.5585199   0.8129705
## Max.cor.Y.glm                        0.7720798     0.5402037   0.8633582
## Interact.High.cor.Y.glm              0.7741374     0.5436041   0.8615815
## Low.cor.X.glm                        0.7710320     0.5386776   0.8465571
## All.X.glm                            0.7679550     0.5327262   0.8431528
## All.X.bayesglm                       0.7638683     0.5243770   0.8462850
## All.X.glmnet                         0.7864482     0.5692702   0.8631938
## All.X.no.rnorm.rpart                 0.8018645     0.5940341   0.8129705
## All.X.no.rnorm.rf                    0.8018392     0.5970410   0.8637792
## All.Interact.X.glm                   0.7494967     0.4949703   0.8344904
## All.Interact.X.bayesglm              0.7813200     0.5584387   0.8364519
## All.Interact.X.glmnet                0.7977651     0.5903552   0.8510347
## All.Interact.X.no.rnorm.rpart        0.8018645     0.5940341   0.8129705
## All.Interact.X.no.rnorm.rf           0.7823457     0.5584003   0.8559487
## csm.glm                              0.7310098     0.4584371   0.7577818
## csm.bayesglm                         0.7576923     0.5127898   0.7749705
## csm.glmnet                           0.7741374     0.5456204   0.7816380
## csm.rpart                            0.7669389     0.5300413   0.7742747
## csm.rf                               0.7289680     0.4528104   0.7665417
##                               opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6332046
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.5       0.7551546
## Max.cor.Y.rpart                                  0.3       0.7528231
## Max.cor.Y.glm                                    0.6       0.7710526
## Interact.High.cor.Y.glm                          0.6       0.7665782
## Low.cor.X.glm                                    0.5       0.7733675
## All.X.glm                                        0.5       0.7715736
## All.X.bayesglm                                   0.5       0.7708067
## All.X.glmnet                                     0.6       0.7751323
## All.X.no.rnorm.rpart                             0.3       0.7528231
## All.X.no.rnorm.rf                                0.5       0.7854356
## All.Interact.X.glm                               0.5       0.7573813
## All.Interact.X.bayesglm                          0.6       0.7573333
## All.Interact.X.glmnet                            0.5       0.7630208
## All.Interact.X.no.rnorm.rpart                    0.3       0.7528231
## All.Interact.X.no.rnorm.rf                       0.6       0.7747253
## csm.glm                                          0.4       0.7148014
## csm.bayesglm                                     0.4       0.7310513
## csm.glmnet                                       0.4       0.7556110
## csm.rpart                                        0.7       0.7528231
## csm.rf                                           0.3       0.7129630
##                               max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                    0.5367232     0.0000000
## Random.myrandom_classfr              0.4632768     0.0000000
## Max.cor.Y.cv.0.rpart                 0.5367232     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart            0.7853107     0.5650993
## Max.cor.Y.rpart                      0.7774011     0.5506630
## Max.cor.Y.glm                        0.8033898     0.6006483
## Interact.High.cor.Y.glm              0.8011299     0.5956491
## Low.cor.X.glm                        0.8000000     0.5951959
## All.X.glm                            0.7966102     0.5888183
## All.X.bayesglm                       0.7977401     0.5906219
## All.X.glmnet                         0.8079096     0.6095656
## All.X.no.rnorm.rpart                 0.7774011     0.5506630
## All.X.no.rnorm.rf                    0.8135593     0.6218781
## All.Interact.X.glm                   0.7864407     0.5676063
## All.Interact.X.bayesglm              0.7943503     0.5815820
## All.Interact.X.glmnet                0.7943503     0.5828499
## All.Interact.X.no.rnorm.rpart        0.7774011     0.5506630
## All.Interact.X.no.rnorm.rf           0.8146893     0.6215582
## csm.glm                              0.7322034     0.4624886
## csm.bayesglm                         0.7514124     0.4999615
## csm.glmnet                           0.7785311     0.5533181
## csm.rpart                            0.7774011     0.5506630
## csm.rf                               0.7197740     0.4406158
##                               inv.elapsedtime.everything
## MFO.myMFO_classfr                             2.28310502
## Random.myrandom_classfr                       3.95256917
## Max.cor.Y.cv.0.rpart                          1.63934426
## Max.cor.Y.cv.0.cp.0.rpart                     2.04918033
## Max.cor.Y.rpart                               0.93283582
## Max.cor.Y.glm                                 0.99601594
## Interact.High.cor.Y.glm                       0.95510984
## Low.cor.X.glm                                 0.67114094
## All.X.glm                                     0.55524708
## All.X.bayesglm                                0.27762354
## All.X.glmnet                                  0.17164435
## All.X.no.rnorm.rpart                          0.59772863
## All.X.no.rnorm.rf                             0.06043392
## All.Interact.X.glm                            0.50709939
## All.Interact.X.bayesglm                       0.37467216
## All.Interact.X.glmnet                         0.11003521
## All.Interact.X.no.rnorm.rpart                 0.53937433
## All.Interact.X.no.rnorm.rf                    0.04786292
## csm.glm                                       0.52854123
## csm.bayesglm                                  0.48590865
## csm.glmnet                                    0.36683786
## csm.rpart                                     0.58892815
## csm.rf                                        0.04518549
##                               inv.elapsedtime.final  inv.aic.fit
## MFO.myMFO_classfr                       500.0000000           NA
## Random.myrandom_classfr                1000.0000000           NA
## Max.cor.Y.cv.0.rpart                     83.3333333           NA
## Max.cor.Y.cv.0.cp.0.rpart               111.1111111           NA
## Max.cor.Y.rpart                          83.3333333           NA
## Max.cor.Y.glm                            90.9090909 0.0010875664
## Interact.High.cor.Y.glm                  52.6315789 0.0010731712
## Low.cor.X.glm                             5.4945055 0.0010656999
## All.X.glm                                 3.8759690 0.0010544153
## All.X.bayesglm                            2.6954178 0.0010072767
## All.X.glmnet                              0.8568980           NA
## All.X.no.rnorm.rpart                     14.4927536           NA
## All.X.no.rnorm.rf                         0.1967729           NA
## All.Interact.X.glm                        2.1231423 0.0010605270
## All.Interact.X.bayesglm                   1.5527950 0.0009773293
## All.Interact.X.glmnet                     1.3586957           NA
## All.Interact.X.no.rnorm.rpart            10.7526882           NA
## All.Interact.X.no.rnorm.rf                0.1426737           NA
## csm.glm                                   2.3041475 0.0009416242
## csm.bayesglm                              2.3584906 0.0009039899
## csm.glmnet                                2.9940120           NA
## csm.rpart                                13.3333333           NA
## csm.rf                                    0.1257387           NA
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

![](ebayipads_color_files/figure-html/fit.models_2-1.png) 

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

![](ebayipads_color_files/figure-html/fit.models_2-2.png) 

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
## 18    All.Interact.X.no.rnorm.rf        0.8146893   0.8559487
## 13             All.X.no.rnorm.rf        0.8135593   0.8637792
## 11                  All.X.glmnet        0.8079096   0.8631938
## 6                  Max.cor.Y.glm        0.8033898   0.8633582
## 7        Interact.High.cor.Y.glm        0.8011299   0.8615815
## 8                  Low.cor.X.glm        0.8000000   0.8465571
## 10                All.X.bayesglm        0.7977401   0.8462850
## 9                      All.X.glm        0.7966102   0.8431528
## 16         All.Interact.X.glmnet        0.7943503   0.8510347
## 15       All.Interact.X.bayesglm        0.7943503   0.8364519
## 14            All.Interact.X.glm        0.7864407   0.8344904
## 4      Max.cor.Y.cv.0.cp.0.rpart        0.7853107   0.8187625
## 21                    csm.glmnet        0.7785311   0.7816380
## 5                Max.cor.Y.rpart        0.7774011   0.8129705
## 12          All.X.no.rnorm.rpart        0.7774011   0.8129705
## 17 All.Interact.X.no.rnorm.rpart        0.7774011   0.8129705
## 22                     csm.rpart        0.7774011   0.7742747
## 20                  csm.bayesglm        0.7514124   0.7749705
## 19                       csm.glm        0.7322034   0.7577818
## 23                        csm.rf        0.7197740   0.7665417
## 1              MFO.myMFO_classfr        0.5367232   0.5000000
## 3           Max.cor.Y.cv.0.rpart        0.5367232   0.5000000
## 2        Random.myrandom_classfr        0.4632768   0.5191913
##    max.Kappa.OOB min.aic.fit opt.prob.threshold.OOB
## 18     0.6215582          NA                    0.6
## 13     0.6218781          NA                    0.5
## 11     0.6095656          NA                    0.6
## 6      0.6006483    919.4841                    0.6
## 7      0.5956491    931.8178                    0.6
## 8      0.5951959    938.3505                    0.5
## 10     0.5906219    992.7758                    0.5
## 9      0.5888183    948.3929                    0.5
## 16     0.5828499          NA                    0.5
## 15     0.5815820   1023.1966                    0.6
## 14     0.5676063    942.9274                    0.5
## 4      0.5650993          NA                    0.5
## 21     0.5533181          NA                    0.4
## 5      0.5506630          NA                    0.3
## 12     0.5506630          NA                    0.3
## 17     0.5506630          NA                    0.3
## 22     0.5506630          NA                    0.7
## 20     0.4999615   1106.2071                    0.4
## 19     0.4624886   1061.9948                    0.4
## 23     0.4406158          NA                    0.3
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

![](ebayipads_color_files/figure-html/fit.models_2-3.png) 

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
## [1] "Best model id: All.Interact.X.no.rnorm.rf"
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

![](ebayipads_color_files/figure-html/fit.models_2-4.png) 

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
## importance       140   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           140   -none-     character
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
##                                                          importance
## biddable                                               1.000000e+02
## startprice.diff:biddable                               7.295709e+01
## startprice.diff                                        6.287101e+01
## idseq.my                                               4.557866e+01
## prdline.my.fctriPadmini:idseq.my                       8.119212e+00
## prdline.my.fctriPadAir:idseq.my                        8.102069e+00
## D.ratio.sum.TfIdf.nwrds                                6.787692e+00
## prdline.my.fctriPad 3+:idseq.my                        6.463037e+00
## prdline.my.fctriPadmini 2+:idseq.my                    5.541186e+00
## prdline.my.fctriPad 1:idseq.my                         5.448197e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio     4.744887e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds          4.653410e+00
## D.TfIdf.sum.stem.stop.Ratio                            4.583426e+00
## storage.fctr64                                         4.562079e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio     4.372017e+00
## color.fctrWhite                                        3.982307e+00
## D.ratio.nstopwrds.nwrds                                3.747225e+00
## storage.fctrUnknown                                    3.656495e+00
## D.nstopwrds.log                                        3.501850e+00
## storage.fctr16                                         3.457889e+00
## D.sum.TfIdf                                            3.363877e+00
## color.fctrUnknown                                      3.345165e+00
## D.TfIdf.sum.post.stem                                  3.183039e+00
## cellular.fctr1                                         3.009368e+00
## D.TfIdf.sum.post.stop                                  2.954056e+00
## prdline.my.fctriPad 2:idseq.my                         2.914295e+00
## condition.fctrNew                                      2.772069e+00
## D.nchrs.log                                            2.742885e+00
## D.nwrds.log                                            2.697258e+00
## D.nuppr.log                                            2.691101e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio    2.637843e+00
## carrier.fctrNone                                       2.620120e+00
## carrier.fctrUnknown                                    2.136666e+00
## color.fctrSpace Gray                                   2.049710e+00
## condition.fctrNew other (see details)                  1.824646e+00
## storage.fctr32                                         1.763924e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                1.756634e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio      1.669334e+00
## D.terms.n.post.stem.log                                1.590060e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds          1.578635e+00
## D.terms.n.post.stem                                    1.567921e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio 1.567397e+00
## D.nwrds.unq.log                                        1.539712e+00
## carrier.fctrVerizon                                    1.526833e+00
## cellular.fctr1:carrier.fctrVerizon                     1.485664e+00
## D.npnct13.log                                          1.427327e+00
## color.fctrGold                                         1.423172e+00
## D.terms.n.post.stop.log                                1.421139e+00
## cellular.fctrUnknown                                   1.374536e+00
## cellular.fctrUnknown:carrier.fctrUnknown               1.361053e+00
## prdline.my.fctriPadAir                                 1.359004e+00
## condition.fctrManufacturer refurbished                 1.353844e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds         1.334240e+00
## D.terms.n.stem.stop.Ratio                              1.299448e+00
## condition.fctrFor parts or not working                 1.289770e+00
## D.terms.n.post.stop                                    1.279013e+00
## prdline.my.fctriPadmini                                1.185258e+00
## condition.fctrSeller refurbished                       1.141570e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds        1.119166e+00
## cellular.fctr1:carrier.fctrUnknown                     1.115997e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio      1.085548e+00
## prdline.my.fctriPad 3+                                 1.074790e+00
## D.npnct11.log                                          1.028460e+00
## prdline.my.fctriPadmini:.clusterid.fctr3               9.742803e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds     9.263539e-01
## D.ndgts.log                                            9.199387e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                 9.085270e-01
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds         8.016364e-01
## D.npnct12.log                                          7.340608e-01
## D.npnct08.log                                          7.259117e-01
## prdline.my.fctriPadmini 2+                             7.232466e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                6.341148e-01
## cellular.fctr1:carrier.fctrSprint                      6.253784e-01
## D.npnct01.log                                          5.556204e-01
## carrier.fctrSprint                                     5.354769e-01
## prdline.my.fctriPad 1                                  5.323525e-01
## prdline.my.fctriPad 2                                  5.238119e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                4.086118e-01
## carrier.fctrT-Mobile                                   4.073964e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                 4.042061e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                 3.962993e-01
## D.npnct24.log                                          3.935604e-01
## cellular.fctr1:carrier.fctrT-Mobile                    3.814115e-01
## D.npnct15.log                                          3.807822e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                 3.531661e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                3.283063e-01
## prdline.my.fctriPadAir:D.npnct03.log                   3.097530e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2            2.966641e-01
## prdline.my.fctriPadmini:.clusterid.fctr2               2.849642e-01
## D.npnct05.log                                          2.411519e-01
## prdline.my.fctriPadmini:D.npnct15.log                  2.393818e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                2.354775e-01
## D.npnct14.log                                          2.344787e-01
## D.npnct16.log                                          2.156363e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                 2.005415e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3            1.694852e-01
## D.npnct03.log                                          1.471806e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                1.381536e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                1.184789e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                9.551081e-02
## prdline.my.fctriPadmini:D.npnct03.log                  8.979465e-02
## D.npnct06.log                                          6.541619e-02
## prdline.my.fctriPadmini:.clusterid.fctr5               6.254349e-02
## prdline.my.fctriPadmini:.clusterid.fctr4               5.965600e-02
## prdline.my.fctriPad 1:.clusterid.fctr3                 4.521448e-02
## prdline.my.fctriPad 3+:D.npnct15.log                   4.097444e-02
## prdline.my.fctriPad 1:D.npnct15.log                    3.470296e-02
## prdline.my.fctriPad 2:D.npnct15.log                    1.968935e-02
## prdline.my.fctriPad 3+:.clusterid.fctr6                1.838658e-02
## cellular.fctr1:carrier.fctrOther                       1.831673e-02
## D.npnct10.log                                          1.457302e-02
## D.npnct09.log                                          7.709772e-03
## carrier.fctrOther                                      6.459850e-03
## prdline.my.fctriPadmini 2+:D.npnct15.log               5.391554e-03
## prdline.my.fctriPadAir:D.npnct15.log                   3.647228e-03
## prdline.my.fctriPad 3+:D.npnct03.log                   3.044642e-03
## prdline.my.fctriPad 2:D.npnct03.log                    2.537202e-03
## D.npnct28.log                                          0.000000e+00
## prdline.my.fctriPad 1:D.npnct03.log                    0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log               0.000000e+00
## cellular.fctr1:carrier.fctrNone                        0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                  0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                 0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile              0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon               0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4            0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5            0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                 0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                 0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr6               0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6            0.000000e+00
##                                                        All.Interact.X.no.rnorm.rf.importance
## biddable                                                                        1.000000e+02
## startprice.diff:biddable                                                        7.295709e+01
## startprice.diff                                                                 6.287101e+01
## idseq.my                                                                        4.557866e+01
## prdline.my.fctriPadmini:idseq.my                                                8.119212e+00
## prdline.my.fctriPadAir:idseq.my                                                 8.102069e+00
## D.ratio.sum.TfIdf.nwrds                                                         6.787692e+00
## prdline.my.fctriPad 3+:idseq.my                                                 6.463037e+00
## prdline.my.fctriPadmini 2+:idseq.my                                             5.541186e+00
## prdline.my.fctriPad 1:idseq.my                                                  5.448197e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio                              4.744887e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds                                   4.653410e+00
## D.TfIdf.sum.stem.stop.Ratio                                                     4.583426e+00
## storage.fctr64                                                                  4.562079e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio                              4.372017e+00
## color.fctrWhite                                                                 3.982307e+00
## D.ratio.nstopwrds.nwrds                                                         3.747225e+00
## storage.fctrUnknown                                                             3.656495e+00
## D.nstopwrds.log                                                                 3.501850e+00
## storage.fctr16                                                                  3.457889e+00
## D.sum.TfIdf                                                                     3.363877e+00
## color.fctrUnknown                                                               3.345165e+00
## D.TfIdf.sum.post.stem                                                           3.183039e+00
## cellular.fctr1                                                                  3.009368e+00
## D.TfIdf.sum.post.stop                                                           2.954056e+00
## prdline.my.fctriPad 2:idseq.my                                                  2.914295e+00
## condition.fctrNew                                                               2.772069e+00
## D.nchrs.log                                                                     2.742885e+00
## D.nwrds.log                                                                     2.697258e+00
## D.nuppr.log                                                                     2.691101e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio                             2.637843e+00
## carrier.fctrNone                                                                2.620120e+00
## carrier.fctrUnknown                                                             2.136666e+00
## color.fctrSpace Gray                                                            2.049710e+00
## condition.fctrNew other (see details)                                           1.824646e+00
## storage.fctr32                                                                  1.763924e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                                         1.756634e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio                               1.669334e+00
## D.terms.n.post.stem.log                                                         1.590060e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds                                   1.578635e+00
## D.terms.n.post.stem                                                             1.567921e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio                          1.567397e+00
## D.nwrds.unq.log                                                                 1.539712e+00
## carrier.fctrVerizon                                                             1.526833e+00
## cellular.fctr1:carrier.fctrVerizon                                              1.485664e+00
## D.npnct13.log                                                                   1.427327e+00
## color.fctrGold                                                                  1.423172e+00
## D.terms.n.post.stop.log                                                         1.421139e+00
## cellular.fctrUnknown                                                            1.374536e+00
## cellular.fctrUnknown:carrier.fctrUnknown                                        1.361053e+00
## prdline.my.fctriPadAir                                                          1.359004e+00
## condition.fctrManufacturer refurbished                                          1.353844e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds                                  1.334240e+00
## D.terms.n.stem.stop.Ratio                                                       1.299448e+00
## condition.fctrFor parts or not working                                          1.289770e+00
## D.terms.n.post.stop                                                             1.279013e+00
## prdline.my.fctriPadmini                                                         1.185258e+00
## condition.fctrSeller refurbished                                                1.141570e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds                                 1.119166e+00
## cellular.fctr1:carrier.fctrUnknown                                              1.115997e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio                               1.085548e+00
## prdline.my.fctriPad 3+                                                          1.074790e+00
## D.npnct11.log                                                                   1.028460e+00
## prdline.my.fctriPadmini:.clusterid.fctr3                                        9.742803e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds                              9.263539e-01
## D.ndgts.log                                                                     9.199387e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                                          9.085270e-01
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds                                  8.016364e-01
## D.npnct12.log                                                                   7.340608e-01
## D.npnct08.log                                                                   7.259117e-01
## prdline.my.fctriPadmini 2+                                                      7.232466e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                                         6.341148e-01
## cellular.fctr1:carrier.fctrSprint                                               6.253784e-01
## D.npnct01.log                                                                   5.556204e-01
## carrier.fctrSprint                                                              5.354769e-01
## prdline.my.fctriPad 1                                                           5.323525e-01
## prdline.my.fctriPad 2                                                           5.238119e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                                         4.086118e-01
## carrier.fctrT-Mobile                                                            4.073964e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                                          4.042061e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                                          3.962993e-01
## D.npnct24.log                                                                   3.935604e-01
## cellular.fctr1:carrier.fctrT-Mobile                                             3.814115e-01
## D.npnct15.log                                                                   3.807822e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                                          3.531661e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                                         3.283063e-01
## prdline.my.fctriPadAir:D.npnct03.log                                            3.097530e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                                     2.966641e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                                        2.849642e-01
## D.npnct05.log                                                                   2.411519e-01
## prdline.my.fctriPadmini:D.npnct15.log                                           2.393818e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                                         2.354775e-01
## D.npnct14.log                                                                   2.344787e-01
## D.npnct16.log                                                                   2.156363e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                                          2.005415e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                                     1.694852e-01
## D.npnct03.log                                                                   1.471806e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                                         1.381536e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                                         1.184789e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                                         9.551081e-02
## prdline.my.fctriPadmini:D.npnct03.log                                           8.979465e-02
## D.npnct06.log                                                                   6.541619e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                                        6.254349e-02
## prdline.my.fctriPadmini:.clusterid.fctr4                                        5.965600e-02
## prdline.my.fctriPad 1:.clusterid.fctr3                                          4.521448e-02
## prdline.my.fctriPad 3+:D.npnct15.log                                            4.097444e-02
## prdline.my.fctriPad 1:D.npnct15.log                                             3.470296e-02
## prdline.my.fctriPad 2:D.npnct15.log                                             1.968935e-02
## prdline.my.fctriPad 3+:.clusterid.fctr6                                         1.838658e-02
## cellular.fctr1:carrier.fctrOther                                                1.831673e-02
## D.npnct10.log                                                                   1.457302e-02
## D.npnct09.log                                                                   7.709772e-03
## carrier.fctrOther                                                               6.459850e-03
## prdline.my.fctriPadmini 2+:D.npnct15.log                                        5.391554e-03
## prdline.my.fctriPadAir:D.npnct15.log                                            3.647228e-03
## prdline.my.fctriPad 3+:D.npnct03.log                                            3.044642e-03
## prdline.my.fctriPad 2:D.npnct03.log                                             2.537202e-03
## D.npnct28.log                                                                   0.000000e+00
## prdline.my.fctriPad 1:D.npnct03.log                                             0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log                                        0.000000e+00
## cellular.fctr1:carrier.fctrNone                                                 0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                                           0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                                          0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                                         0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile                                       0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon                                        0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                                         0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                                         0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                                     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                                         0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                                          0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                                          0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                                         0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                                     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                                         0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                                          0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                                          0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                                         0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr6                                        0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6                                     0.000000e+00
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 49
```

![](ebayipads_color_files/figure-html/fit.models_2-5.png) ![](ebayipads_color_files/figure-html/fit.models_2-6.png) ![](ebayipads_color_files/figure-html/fit.models_2-7.png) ![](ebayipads_color_files/figure-html/fit.models_2-8.png) ![](ebayipads_color_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 66      10066         N                                             0.076
## 594     10594         N                                             0.528
## 1396    11397         N                                             0.004
## 1802    11803         N                                             0.614
## 1755    11756         N                                             0.614
## 1509    11510         N                                             0.630
## 747     10747         N                                             0.644
## 1385    11386         N                                             0.654
## 127     10127         N                                             0.660
## 409     10409         N                                             0.668
## 851     10851         N                                             0.730
## 1835    11836         N                                             0.810
## 1699    11700         N                                             0.946
## 199     10199         N                                             0.956
## 1768    11769         N                                             0.982
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 66                                              N
## 594                                             N
## 1396                                            N
## 1802                                            Y
## 1755                                            Y
## 1509                                            Y
## 747                                             Y
## 1385                                            Y
## 127                                             Y
## 409                                             Y
## 851                                             Y
## 1835                                            Y
## 1699                                            Y
## 199                                             Y
## 1768                                            Y
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 66                                                    TRUE
## 594                                                   TRUE
## 1396                                                  TRUE
## 1802                                                 FALSE
## 1755                                                 FALSE
## 1509                                                 FALSE
## 747                                                  FALSE
## 1385                                                 FALSE
## 127                                                  FALSE
## 409                                                  FALSE
## 851                                                  FALSE
## 1835                                                 FALSE
## 1699                                                 FALSE
## 199                                                  FALSE
## 1768                                                 FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error .label
## 66                                                0.000  10066
## 594                                               0.000  10594
## 1396                                              0.000  11397
## 1802                                              0.014  11803
## 1755                                              0.014  11756
## 1509                                              0.030  11510
## 747                                               0.044  10747
## 1385                                              0.054  11386
## 127                                               0.060  10127
## 409                                               0.068  10409
## 851                                               0.130  10851
## 1835                                              0.210  11836
## 1699                                              0.346  11700
## 199                                               0.356  10199
## 1768                                              0.382  11769
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 1447    11448         Y                                             0.000
## 1358    11359         Y                                             0.002
## 1582    11583         Y                                             0.014
## 1420    11421         Y                                             0.022
## 1186    11186         Y                                             0.030
## 1381    11382         Y                                             0.034
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 1447                                            N
## 1358                                            N
## 1582                                            N
## 1420                                            N
## 1186                                            N
## 1381                                            N
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 1447                                                 FALSE
## 1358                                                 FALSE
## 1582                                                 FALSE
## 1420                                                 FALSE
## 1186                                                 FALSE
## 1381                                                 FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error
## 1447                                             -0.600
## 1358                                             -0.598
## 1582                                             -0.586
## 1420                                             -0.578
## 1186                                             -0.570
## 1381                                             -0.566
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 394     10394         Y                                             0.120
## 105     10105         Y                                             0.230
## 1855    11857         Y                                             0.242
## 1529    11530         Y                                             0.510
## 528     10528         N                                             0.668
## 1621    11622         N                                             0.892
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 394                                             N
## 105                                             N
## 1855                                            N
## 1529                                            N
## 528                                             Y
## 1621                                            Y
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 394                                                  FALSE
## 105                                                  FALSE
## 1855                                                 FALSE
## 1529                                                 FALSE
## 528                                                  FALSE
## 1621                                                 FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error
## 394                                              -0.480
## 105                                              -0.370
## 1855                                             -0.358
## 1529                                             -0.090
## 528                                               0.068
## 1621                                              0.292
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 491     10491         N                                             0.976
## 1768    11769         N                                             0.982
## 283     10283         N                                             0.982
## 413     10413         N                                             0.984
## 241     10241         N                                             0.986
## 488     10488         N                                             1.000
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 491                                             Y
## 1768                                            Y
## 283                                             Y
## 413                                             Y
## 241                                             Y
## 488                                             Y
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 491                                                  FALSE
## 1768                                                 FALSE
## 283                                                  FALSE
## 413                                                  FALSE
## 241                                                  FALSE
## 488                                                  FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error
## 491                                               0.376
## 1768                                              0.382
## 283                                               0.382
## 413                                               0.384
## 241                                               0.386
## 488                                               0.400
```

![](ebayipads_color_files/figure-html/fit.models_2-10.png) 

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
##         label step_major step_minor     bgn    end elapsed
## 12 fit.models          7          2 327.364 356.67  29.306
## 13 fit.models          7          3 356.671     NA      NA
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
## [1] "sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob"    
## [2] "sold.fctr.predict.All.Interact.X.no.rnorm.rf"         
## [3] "sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate"
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

![](ebayipads_color_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 356.671 361.584   4.913
## 14 fit.data.training          8          0 361.584      NA      NA
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
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_color_files/figure-html/fit.data.training_0-1.png) 

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
## importance       140   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               1859   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           140   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_color_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold   f.score
## 1        0.0 0.6325855
## 2        0.1 0.8329298
## 3        0.2 0.9419496
## 4        0.3 0.9822958
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 0.9988359
## 8        0.7 0.9505796
## 9        0.8 0.8601723
## 10       0.9 0.7766714
## 11       1.0 0.2413088
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

![](ebayipads_color_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 D.ratio.nstopwrds.nwrds, D.terms.n.stem.stop.Ratio, D.npnct01.log, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.npnct13.log, D.TfIdf.sum.post.stem, D.sum.TfIdf, color.fctr, D.npnct08.log, prdline.my.fctr, D.nstopwrds.log, D.npnct16.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.nwrds.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, D.terms.n.post.stem.log, D.terms.n.post.stop.log, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*idseq.my, prdline.my.fctr*D.ratio.sum.TfIdf.nwrds, prdline.my.fctr*D.TfIdf.sum.stem.stop.Ratio, prdline.my.fctr*D.npnct15.log, prdline.my.fctr*D.npnct03.log, startprice.diff*biddable, cellular.fctr*carrier.fctr, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     28.696                16.942
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8068798
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9980176                     1     0.6082762
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009354327       0.0185016
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 361.584 392.647  31.063
## 15 fit.data.training          8          1 392.648      NA      NA
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
##                                                        All.Interact.X.no.rnorm.rf.importance
## biddable                                                                        1.000000e+02
## startprice.diff                                                                 6.287101e+01
## startprice.diff:biddable                                                        7.295709e+01
## idseq.my                                                                        4.557866e+01
## prdline.my.fctriPadAir:idseq.my                                                 8.102069e+00
## prdline.my.fctriPadmini:idseq.my                                                8.119212e+00
## prdline.my.fctriPadmini 2+:idseq.my                                             5.541186e+00
## prdline.my.fctriPad 3+:idseq.my                                                 6.463037e+00
## D.ratio.sum.TfIdf.nwrds                                                         6.787692e+00
## prdline.my.fctriPad 2:idseq.my                                                  2.914295e+00
## D.TfIdf.sum.stem.stop.Ratio                                                     4.583426e+00
## prdline.my.fctriPad 1:idseq.my                                                  5.448197e+00
## D.ratio.nstopwrds.nwrds                                                         3.747225e+00
## color.fctrWhite                                                                 3.982307e+00
## D.nstopwrds.log                                                                 3.501850e+00
## color.fctrUnknown                                                               3.345165e+00
## storage.fctr64                                                                  4.562079e+00
## storage.fctr16                                                                  3.457889e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio                              4.372017e+00
## D.nchrs.log                                                                     2.742885e+00
## D.TfIdf.sum.post.stop                                                           2.954056e+00
## D.TfIdf.sum.post.stem                                                           3.183039e+00
## D.sum.TfIdf                                                                     3.363877e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio                              4.744887e+00
## condition.fctrNew                                                               2.772069e+00
## D.nuppr.log                                                                     2.691101e+00
## storage.fctrUnknown                                                             3.656495e+00
## D.nwrds.log                                                                     2.697258e+00
## cellular.fctr1                                                                  3.009368e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio                               1.669334e+00
## carrier.fctrNone                                                                2.620120e+00
## color.fctrSpace Gray                                                            2.049710e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds                                   1.578635e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio                               1.085548e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio                          1.567397e+00
## D.npnct11.log                                                                   1.028460e+00
## carrier.fctrUnknown                                                             2.136666e+00
## storage.fctr32                                                                  1.763924e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio                             2.637843e+00
## D.npnct13.log                                                                   1.427327e+00
## D.nwrds.unq.log                                                                 1.539712e+00
## prdline.my.fctriPadAir                                                          1.359004e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds                                  1.334240e+00
## D.terms.n.post.stem.log                                                         1.590060e+00
## D.terms.n.post.stem                                                             1.567921e+00
## D.terms.n.post.stop                                                             1.279013e+00
## cellular.fctrUnknown:carrier.fctrUnknown                                        1.361053e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds                                   4.653410e+00
## D.terms.n.post.stop.log                                                         1.421139e+00
## condition.fctrSeller refurbished                                                1.141570e+00
## D.npnct15.log                                                                   3.807822e-01
## cellular.fctrUnknown                                                            1.374536e+00
## prdline.my.fctriPad 2:.clusterid.fctr4                                          9.085270e-01
## condition.fctrManufacturer refurbished                                          1.353844e+00
## carrier.fctrVerizon                                                             1.526833e+00
## condition.fctrFor parts or not working                                          1.289770e+00
## condition.fctrNew other (see details)                                           1.824646e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds                                 1.119166e+00
## D.ndgts.log                                                                     9.199387e-01
## cellular.fctr1:carrier.fctrVerizon                                              1.485664e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                                         1.756634e+00
## prdline.my.fctriPad 3+                                                          1.074790e+00
## cellular.fctr1:carrier.fctrUnknown                                              1.115997e+00
## prdline.my.fctriPadmini 2+                                                      7.232466e-01
## color.fctrGold                                                                  1.423172e+00
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds                                  8.016364e-01
## prdline.my.fctriPadmini                                                         1.185258e+00
## prdline.my.fctrUnknown:.clusterid.fctr3                                         6.341148e-01
## D.terms.n.stem.stop.Ratio                                                       1.299448e+00
## prdline.my.fctriPad 1                                                           5.323525e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds                              9.263539e-01
## D.npnct08.log                                                                   7.259117e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                                        9.742803e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                                        5.965600e-02
## cellular.fctr1:carrier.fctrT-Mobile                                             3.814115e-01
## prdline.my.fctriPad 3+:.clusterid.fctr6                                         1.838658e-02
## prdline.my.fctriPad 2                                                           5.238119e-01
## carrier.fctrSprint                                                              5.354769e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                                          3.531661e-01
## cellular.fctr1:carrier.fctrSprint                                               6.253784e-01
## carrier.fctrT-Mobile                                                            4.073964e-01
## D.npnct14.log                                                                   2.344787e-01
## D.npnct16.log                                                                   2.156363e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                                         3.283063e-01
## D.npnct01.log                                                                   5.556204e-01
## prdline.my.fctriPad 1:D.npnct15.log                                             3.470296e-02
## prdline.my.fctriPadAir:.clusterid.fctr2                                         9.551081e-02
## prdline.my.fctriPad 3+:.clusterid.fctr4                                         4.086118e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                                          3.962993e-01
## D.npnct24.log                                                                   3.935604e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                                        2.849642e-01
## D.npnct12.log                                                                   7.340608e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                                         1.184789e-01
## D.npnct06.log                                                                   6.541619e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                                        6.254349e-02
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                                     1.694852e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                                         2.354775e-01
## D.npnct05.log                                                                   2.411519e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                                     2.966641e-01
## prdline.my.fctriPadmini:D.npnct15.log                                           2.393818e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                                         1.381536e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                                          2.005415e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                                          4.042061e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                                          4.521448e-02
## prdline.my.fctriPad 2:D.npnct03.log                                             2.537202e-03
## prdline.my.fctriPadAir:D.npnct03.log                                            3.097530e-01
## D.npnct10.log                                                                   1.457302e-02
## D.npnct03.log                                                                   1.471806e-01
## prdline.my.fctriPad 3+:D.npnct15.log                                            4.097444e-02
## prdline.my.fctriPadmini:D.npnct03.log                                           8.979465e-02
## prdline.my.fctriPad 2:D.npnct15.log                                             1.968935e-02
## cellular.fctr1:carrier.fctrOther                                                1.831673e-02
## D.npnct09.log                                                                   7.709772e-03
## prdline.my.fctriPad 1:D.npnct03.log                                             0.000000e+00
## carrier.fctrOther                                                               6.459850e-03
## D.npnct28.log                                                                   0.000000e+00
## prdline.my.fctriPad 3+:D.npnct03.log                                            3.044642e-03
## cellular.fctr1:carrier.fctrNone                                                 0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                                           0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                                          0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                                         0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile                                       0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon                                        0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                                         0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                                         0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                                         0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                                          0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                                          0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                                          0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                                          0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                                         0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                                         0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                                         0.000000e+00
## prdline.my.fctriPadAir:D.npnct15.log                                            3.647228e-03
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6                                     0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log                                        0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct15.log                                        5.391554e-03
## prdline.my.fctriPadmini:.clusterid.fctr6                                        0.000000e+00
##                                                          importance
## biddable                                               1.000000e+02
## startprice.diff                                        6.960528e+01
## startprice.diff:biddable                               6.693838e+01
## idseq.my                                               4.204923e+01
## prdline.my.fctriPadAir:idseq.my                        7.671632e+00
## prdline.my.fctriPadmini:idseq.my                       6.375032e+00
## prdline.my.fctriPadmini 2+:idseq.my                    5.683911e+00
## prdline.my.fctriPad 3+:idseq.my                        5.627514e+00
## D.ratio.sum.TfIdf.nwrds                                5.029416e+00
## prdline.my.fctriPad 2:idseq.my                         4.774435e+00
## D.TfIdf.sum.stem.stop.Ratio                            4.764236e+00
## prdline.my.fctriPad 1:idseq.my                         4.228074e+00
## D.ratio.nstopwrds.nwrds                                4.055811e+00
## color.fctrWhite                                        3.424959e+00
## D.nstopwrds.log                                        3.420231e+00
## color.fctrUnknown                                      3.323058e+00
## storage.fctr64                                         3.115700e+00
## storage.fctr16                                         3.087964e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio     3.049947e+00
## D.nchrs.log                                            3.032897e+00
## D.TfIdf.sum.post.stop                                  3.015868e+00
## D.TfIdf.sum.post.stem                                  3.012195e+00
## D.sum.TfIdf                                            2.969096e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio     2.893811e+00
## condition.fctrNew                                      2.850164e+00
## D.nuppr.log                                            2.703828e+00
## storage.fctrUnknown                                    2.611807e+00
## D.nwrds.log                                            2.576687e+00
## cellular.fctr1                                         2.449276e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio      2.363800e+00
## carrier.fctrNone                                       2.339635e+00
## color.fctrSpace Gray                                   2.323702e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds          2.153269e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio      2.007878e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio 1.983204e+00
## D.npnct11.log                                          1.773518e+00
## carrier.fctrUnknown                                    1.749041e+00
## storage.fctr32                                         1.709455e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio    1.661616e+00
## D.npnct13.log                                          1.641072e+00
## D.nwrds.unq.log                                        1.623470e+00
## prdline.my.fctriPadAir                                 1.601391e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds         1.566376e+00
## D.terms.n.post.stem.log                                1.498671e+00
## D.terms.n.post.stem                                    1.495246e+00
## D.terms.n.post.stop                                    1.351248e+00
## cellular.fctrUnknown:carrier.fctrUnknown               1.329742e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds          1.328155e+00
## D.terms.n.post.stop.log                                1.327629e+00
## condition.fctrSeller refurbished                       1.323465e+00
## D.npnct15.log                                          1.299244e+00
## cellular.fctrUnknown                                   1.272499e+00
## prdline.my.fctriPad 2:.clusterid.fctr4                 1.254470e+00
## condition.fctrManufacturer refurbished                 1.240694e+00
## carrier.fctrVerizon                                    1.234290e+00
## condition.fctrFor parts or not working                 1.211497e+00
## condition.fctrNew other (see details)                  1.209819e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds        1.185455e+00
## D.ndgts.log                                            1.184364e+00
## cellular.fctr1:carrier.fctrVerizon                     1.125683e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                1.107954e+00
## prdline.my.fctriPad 3+                                 1.091447e+00
## cellular.fctr1:carrier.fctrUnknown                     1.064053e+00
## prdline.my.fctriPadmini 2+                             9.675146e-01
## color.fctrGold                                         9.190781e-01
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds         9.080687e-01
## prdline.my.fctriPadmini                                9.052589e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                8.941081e-01
## D.terms.n.stem.stop.Ratio                              8.127660e-01
## prdline.my.fctriPad 1                                  8.040097e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds     7.693623e-01
## D.npnct08.log                                          7.021449e-01
## prdline.my.fctriPadmini:.clusterid.fctr3               6.830103e-01
## prdline.my.fctriPadmini:.clusterid.fctr4               6.337059e-01
## cellular.fctr1:carrier.fctrT-Mobile                    6.321343e-01
## prdline.my.fctriPad 3+:.clusterid.fctr6                6.319869e-01
## prdline.my.fctriPad 2                                  6.203999e-01
## carrier.fctrSprint                                     5.885889e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                 5.527526e-01
## cellular.fctr1:carrier.fctrSprint                      5.144275e-01
## carrier.fctrT-Mobile                                   5.016182e-01
## D.npnct14.log                                          4.907743e-01
## D.npnct16.log                                          4.716153e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                3.477216e-01
## D.npnct01.log                                          3.384943e-01
## prdline.my.fctriPad 1:D.npnct15.log                    3.105568e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                3.071686e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                2.845234e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                 2.636628e-01
## D.npnct24.log                                          2.612274e-01
## prdline.my.fctriPadmini:.clusterid.fctr2               2.600623e-01
## D.npnct12.log                                          2.228584e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                2.147635e-01
## D.npnct06.log                                          2.138099e-01
## prdline.my.fctriPadmini:.clusterid.fctr5               2.114139e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3            2.068904e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                1.995621e-01
## D.npnct05.log                                          1.858545e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2            1.781904e-01
## prdline.my.fctriPadmini:D.npnct15.log                  1.370333e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                1.317783e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                 1.163280e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                 9.575558e-02
## prdline.my.fctriPad 1:.clusterid.fctr3                 9.357905e-02
## prdline.my.fctriPad 2:D.npnct03.log                    9.355105e-02
## prdline.my.fctriPadAir:D.npnct03.log                   9.102826e-02
## D.npnct10.log                                          8.532980e-02
## D.npnct03.log                                          5.802949e-02
## prdline.my.fctriPad 3+:D.npnct15.log                   4.802268e-02
## prdline.my.fctriPadmini:D.npnct03.log                  3.516015e-02
## prdline.my.fctriPad 2:D.npnct15.log                    1.197634e-02
## cellular.fctr1:carrier.fctrOther                       8.801221e-03
## D.npnct09.log                                          6.856611e-03
## prdline.my.fctriPad 1:D.npnct03.log                    3.536624e-03
## carrier.fctrOther                                      2.314881e-03
## D.npnct28.log                                          1.736161e-03
## prdline.my.fctriPad 3+:D.npnct03.log                   9.645339e-04
## cellular.fctr1:carrier.fctrNone                        0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                  0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                 0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile              0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon               0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                 0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                 0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                0.000000e+00
## prdline.my.fctriPadAir:D.npnct15.log                   0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4            0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6            0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log               0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct15.log               0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr6               0.000000e+00
##                                                        Final.rf.importance
## biddable                                                      1.000000e+02
## startprice.diff                                               6.960528e+01
## startprice.diff:biddable                                      6.693838e+01
## idseq.my                                                      4.204923e+01
## prdline.my.fctriPadAir:idseq.my                               7.671632e+00
## prdline.my.fctriPadmini:idseq.my                              6.375032e+00
## prdline.my.fctriPadmini 2+:idseq.my                           5.683911e+00
## prdline.my.fctriPad 3+:idseq.my                               5.627514e+00
## D.ratio.sum.TfIdf.nwrds                                       5.029416e+00
## prdline.my.fctriPad 2:idseq.my                                4.774435e+00
## D.TfIdf.sum.stem.stop.Ratio                                   4.764236e+00
## prdline.my.fctriPad 1:idseq.my                                4.228074e+00
## D.ratio.nstopwrds.nwrds                                       4.055811e+00
## color.fctrWhite                                               3.424959e+00
## D.nstopwrds.log                                               3.420231e+00
## color.fctrUnknown                                             3.323058e+00
## storage.fctr64                                                3.115700e+00
## storage.fctr16                                                3.087964e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio            3.049947e+00
## D.nchrs.log                                                   3.032897e+00
## D.TfIdf.sum.post.stop                                         3.015868e+00
## D.TfIdf.sum.post.stem                                         3.012195e+00
## D.sum.TfIdf                                                   2.969096e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio            2.893811e+00
## condition.fctrNew                                             2.850164e+00
## D.nuppr.log                                                   2.703828e+00
## storage.fctrUnknown                                           2.611807e+00
## D.nwrds.log                                                   2.576687e+00
## cellular.fctr1                                                2.449276e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio             2.363800e+00
## carrier.fctrNone                                              2.339635e+00
## color.fctrSpace Gray                                          2.323702e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds                 2.153269e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio             2.007878e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio        1.983204e+00
## D.npnct11.log                                                 1.773518e+00
## carrier.fctrUnknown                                           1.749041e+00
## storage.fctr32                                                1.709455e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio           1.661616e+00
## D.npnct13.log                                                 1.641072e+00
## D.nwrds.unq.log                                               1.623470e+00
## prdline.my.fctriPadAir                                        1.601391e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds                1.566376e+00
## D.terms.n.post.stem.log                                       1.498671e+00
## D.terms.n.post.stem                                           1.495246e+00
## D.terms.n.post.stop                                           1.351248e+00
## cellular.fctrUnknown:carrier.fctrUnknown                      1.329742e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds                 1.328155e+00
## D.terms.n.post.stop.log                                       1.327629e+00
## condition.fctrSeller refurbished                              1.323465e+00
## D.npnct15.log                                                 1.299244e+00
## cellular.fctrUnknown                                          1.272499e+00
## prdline.my.fctriPad 2:.clusterid.fctr4                        1.254470e+00
## condition.fctrManufacturer refurbished                        1.240694e+00
## carrier.fctrVerizon                                           1.234290e+00
## condition.fctrFor parts or not working                        1.211497e+00
## condition.fctrNew other (see details)                         1.209819e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds               1.185455e+00
## D.ndgts.log                                                   1.184364e+00
## cellular.fctr1:carrier.fctrVerizon                            1.125683e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                       1.107954e+00
## prdline.my.fctriPad 3+                                        1.091447e+00
## cellular.fctr1:carrier.fctrUnknown                            1.064053e+00
## prdline.my.fctriPadmini 2+                                    9.675146e-01
## color.fctrGold                                                9.190781e-01
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds                9.080687e-01
## prdline.my.fctriPadmini                                       9.052589e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                       8.941081e-01
## D.terms.n.stem.stop.Ratio                                     8.127660e-01
## prdline.my.fctriPad 1                                         8.040097e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds            7.693623e-01
## D.npnct08.log                                                 7.021449e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                      6.830103e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                      6.337059e-01
## cellular.fctr1:carrier.fctrT-Mobile                           6.321343e-01
## prdline.my.fctriPad 3+:.clusterid.fctr6                       6.319869e-01
## prdline.my.fctriPad 2                                         6.203999e-01
## carrier.fctrSprint                                            5.885889e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                        5.527526e-01
## cellular.fctr1:carrier.fctrSprint                             5.144275e-01
## carrier.fctrT-Mobile                                          5.016182e-01
## D.npnct14.log                                                 4.907743e-01
## D.npnct16.log                                                 4.716153e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                       3.477216e-01
## D.npnct01.log                                                 3.384943e-01
## prdline.my.fctriPad 1:D.npnct15.log                           3.105568e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                       3.071686e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                       2.845234e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                        2.636628e-01
## D.npnct24.log                                                 2.612274e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                      2.600623e-01
## D.npnct12.log                                                 2.228584e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                       2.147635e-01
## D.npnct06.log                                                 2.138099e-01
## prdline.my.fctriPadmini:.clusterid.fctr5                      2.114139e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                   2.068904e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                       1.995621e-01
## D.npnct05.log                                                 1.858545e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                   1.781904e-01
## prdline.my.fctriPadmini:D.npnct15.log                         1.370333e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                       1.317783e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                        1.163280e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                        9.575558e-02
## prdline.my.fctriPad 1:.clusterid.fctr3                        9.357905e-02
## prdline.my.fctriPad 2:D.npnct03.log                           9.355105e-02
## prdline.my.fctriPadAir:D.npnct03.log                          9.102826e-02
## D.npnct10.log                                                 8.532980e-02
## D.npnct03.log                                                 5.802949e-02
## prdline.my.fctriPad 3+:D.npnct15.log                          4.802268e-02
## prdline.my.fctriPadmini:D.npnct03.log                         3.516015e-02
## prdline.my.fctriPad 2:D.npnct15.log                           1.197634e-02
## cellular.fctr1:carrier.fctrOther                              8.801221e-03
## D.npnct09.log                                                 6.856611e-03
## prdline.my.fctriPad 1:D.npnct03.log                           3.536624e-03
## carrier.fctrOther                                             2.314881e-03
## D.npnct28.log                                                 1.736161e-03
## prdline.my.fctriPad 3+:D.npnct03.log                          9.645339e-04
## cellular.fctr1:carrier.fctrNone                               0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                         0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                        0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                       0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile                     0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon                      0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                       0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                       0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                       0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                        0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                        0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                        0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                        0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                       0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                       0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                       0.000000e+00
## prdline.my.fctriPadAir:D.npnct15.log                          0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                   0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                   0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6                   0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log                      0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct15.log                      0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr6                      0.000000e+00
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 49
```

![](ebayipads_color_files/figure-html/fit.data.training_0-4.png) ![](ebayipads_color_files/figure-html/fit.data.training_0-5.png) ![](ebayipads_color_files/figure-html/fit.data.training_0-6.png) ![](ebayipads_color_files/figure-html/fit.data.training_0-7.png) ![](ebayipads_color_files/figure-html/fit.data.training_0-8.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1358    11359         Y                           0.594
## 991     10991         Y                           0.596
## 1       10001         N                           0.246
## 2       10002         Y                           0.998
## 594     10594         N                           0.152
## 1396    11397         N                           0.004
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1358                          N                               FALSE
## 991                           N                               FALSE
## 1                             N                                TRUE
## 2                             Y                                TRUE
## 594                           N                                TRUE
## 1396                          N                                TRUE
##      sold.fctr.predict.Final.rf.error .label
## 1358                           -0.006  11359
## 991                            -0.004  10991
## 1                               0.000  10001
## 2                               0.000  10002
## 594                             0.000  10594
## 1396                            0.000  11397
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1358    11359         Y                           0.594
## 991     10991         Y                           0.596
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1358                          N                               FALSE
## 991                           N                               FALSE
##      sold.fctr.predict.Final.rf.error
## 1358                           -0.006
## 991                            -0.004
```

![](ebayipads_color_files/figure-html/fit.data.training_0-9.png) 

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

![](ebayipads_color_files/figure-html/fit.data.training_0-10.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 392.648 398.125   5.477
## 16  predict.data.new          9          0 398.126      NA      NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 49
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

![](ebayipads_color_files/figure-html/predict.data.new-1.png) 

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

![](ebayipads_color_files/figure-html/predict.data.new-2.png) 

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

![](ebayipads_color_files/figure-html/predict.data.new-3.png) 

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

![](ebayipads_color_files/figure-html/predict.data.new-4.png) 

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

![](ebayipads_color_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1860    11862      <NA>                           0.310
## 1863    11865      <NA>                           0.532
## 2094    12096      <NA>                           0.468
## 2623    12625      <NA>                           0.562
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1860                          N                                  NA
## 1863                          N                                  NA
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
## NA.35        NA      <NA>                              NA
## NA.137       NA      <NA>                              NA
## NA.141       NA      <NA>                              NA
## NA.431       NA      <NA>                              NA
## NA.644       NA      <NA>                              NA
## NA.666       NA      <NA>                              NA
##        sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## NA.35                        <NA>                                  NA
## NA.137                       <NA>                                  NA
## NA.141                       <NA>                                  NA
## NA.431                       <NA>                                  NA
## NA.644                       <NA>                                  NA
## NA.666                       <NA>                                  NA
##        sold.fctr.predict.Final.rf.error
## NA.35                                NA
## NA.137                               NA
## NA.141                               NA
## NA.431                               NA
## NA.644                               NA
## NA.666                               NA
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

![](ebayipads_color_files/figure-html/predict.data.new-6.png) 

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
                                         12050, 12115, 12253, 12285, 12367, 12388, 12585)
    for (obs_id in glb_force_prediction_lst[["0"]]) {
        if (is.na(glb_allobs_df[glb_allobs_df[, glb_id_var] == obs_id, ".grpid"]))
            stop(".grpid is NA")
        submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] <-
            max(0, submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] - 0.5)
    }    
    
    glb_force_prediction_lst[["1"]] <- c(11871, 11875, 11886, 
                                    11913, 11931, 11937, 11967, 11990, 11991, 11994, 11999,
                                         12000, 12002, 12021, 12065, 12072, 
                                         12111, 12114, 12126, 12152, 12172,
                                         12213, 12214, 12233, 12278, 12299, 
                                         12446, 12491, 
                                         12505, 12576, 12608, 12630)
    for (obs_id in glb_force_prediction_lst[["1"]]) {
        if (is.na(glb_allobs_df[glb_allobs_df[, glb_id_var] == obs_id, ".grpid"]))
            stop(".grpid is NA")
        submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] <-
            min(0.9999, submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] + 0.5)
    }    
    
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
## condit  209.2617  condit  496 111
## use     147.7914     use  291 501
## scratch 129.1467 scratch  286 409
## new     126.1758     new  156 316
## good    121.5866    good  197 213
## ipad    108.6364    ipad  232 249
##               TfIdf      term freq pos
## origin    42.799283    origin   56 332
## came       4.107736      came    4  85
## necessari  2.305685 necessari    2 312
## swipe      2.275117     swipe    1 465
## titl       2.075117      titl    2 481
## buyer      1.625083     buyer    1  83
##                 TfIdf         term freq pos
## marksabsolut 1.421948 marksabsolut    1 292
## often        1.421948        often    1 326
## 360          1.263954          360    1   9
## 975          1.137558          975    1  15
## mic          1.137558          mic    1 298
## 79in         1.034144         79in    1  14
##     UniqueID
## 520    10520
##                                                                                            descr.my
## 520 Apple iPad mini 1st Generation 16GB, Wi- Fi, 7.9in - spacegray, great condition comes with the 
## [1] 65
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
## [1] "glb_sel_mdl_id: All.Interact.X.no.rnorm.rf"
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
## [1] 974  76
```

```r
print(dsp_models_df)
```

```
##                         model_id max.Accuracy.OOB max.auc.OOB
## 18    All.Interact.X.no.rnorm.rf        0.8146893   0.8559487
## 13             All.X.no.rnorm.rf        0.8135593   0.8637792
## 11                  All.X.glmnet        0.8079096   0.8631938
## 6                  Max.cor.Y.glm        0.8033898   0.8633582
## 7        Interact.High.cor.Y.glm        0.8011299   0.8615815
## 8                  Low.cor.X.glm        0.8000000   0.8465571
## 10                All.X.bayesglm        0.7977401   0.8462850
## 9                      All.X.glm        0.7966102   0.8431528
## 16         All.Interact.X.glmnet        0.7943503   0.8510347
## 15       All.Interact.X.bayesglm        0.7943503   0.8364519
## 14            All.Interact.X.glm        0.7864407   0.8344904
## 4      Max.cor.Y.cv.0.cp.0.rpart        0.7853107   0.8187625
## 21                    csm.glmnet        0.7785311   0.7816380
## 5                Max.cor.Y.rpart        0.7774011   0.8129705
## 12          All.X.no.rnorm.rpart        0.7774011   0.8129705
## 17 All.Interact.X.no.rnorm.rpart        0.7774011   0.8129705
## 22                     csm.rpart        0.7774011   0.7742747
## 20                  csm.bayesglm        0.7514124   0.7749705
## 19                       csm.glm        0.7322034   0.7577818
## 23                        csm.rf        0.7197740   0.7665417
## 1              MFO.myMFO_classfr        0.5367232   0.5000000
## 3           Max.cor.Y.cv.0.rpart        0.5367232   0.5000000
## 2        Random.myrandom_classfr        0.4632768   0.5191913
##    max.Kappa.OOB min.aic.fit opt.prob.threshold.OOB
## 18     0.6215582          NA                    0.6
## 13     0.6218781          NA                    0.5
## 11     0.6095656          NA                    0.6
## 6      0.6006483    919.4841                    0.6
## 7      0.5956491    931.8178                    0.6
## 8      0.5951959    938.3505                    0.5
## 10     0.5906219    992.7758                    0.5
## 9      0.5888183    948.3929                    0.5
## 16     0.5828499          NA                    0.5
## 15     0.5815820   1023.1966                    0.6
## 14     0.5676063    942.9274                    0.5
## 4      0.5650993          NA                    0.5
## 21     0.5533181          NA                    0.4
## 5      0.5506630          NA                    0.3
## 12     0.5506630          NA                    0.3
## 17     0.5506630          NA                    0.3
## 22     0.5506630          NA                    0.7
## 20     0.4999615   1106.2071                    0.4
## 19     0.4624886   1061.9948                    0.4
## 23     0.4406158          NA                    0.3
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
## [1] "All.Interact.X.no.rnorm.rf OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 439  36
##         Y 128 282
##    prdline.my .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 3      iPad 2    171    154      0.1929825      0.1932203
## 5     iPadAir    151    137      0.1716792      0.1706215
## 1     Unknown     97     87      0.1090226      0.1096045
## 4     iPad 3+    136    123      0.1541353      0.1536723
## 2      iPad 1     99     89      0.1115288      0.1118644
## 6    iPadmini    127    114      0.1428571      0.1435028
## 7 iPadmini 2+    104     94      0.1177945      0.1175141
##   accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 3                 29               142        0.8304094
## 5                 29               122        0.8079470
## 1                 23                74        0.7628866
## 4                 22               114        0.8382353
## 2                 21                78        0.7878788
## 6                 21               106        0.8346457
## 7                 19                85        0.8173077
##      UniqueID sold biddable startprice                condition
## 246     10246    0        1     400.00                     Used
## 261     10261    0        1     250.00                     Used
## 738     10738    0        1     350.00                      New
## 1320    11321    0        1     600.00                      New
## 19      10019    1        0     375.00                     Used
## 109     10109    1        0     339.99                      New
## 205     10205    1        0     415.00                     Used
## 277     10277    1        0     300.00                     Used
## 342     10342    1        1     295.00                     Used
## 467     10467    1        1     249.98                     Used
## 572     10572    1        1     185.00                     Used
## 577     10577    1        1     279.00                     Used
## 625     10625    1        0     559.99                     Used
## 675     10675    1        0     280.00                     Used
## 976     10976    1        1     350.00                     Used
## 1129    11129    1        1     350.00  New other (see details)
## 1200    11200    1        0     379.99                     Used
## 1212    11212    1        0     450.00                      New
## 1225    11225    1        0     499.99                     Used
## 1259    11260    1        1     260.00  New other (see details)
## 1294    11295    1        0     425.00                      New
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
##                                                        All.Interact.X.no.rnorm.rf.importance
## biddable                                                                        1.000000e+02
## startprice.diff:biddable                                                        7.295709e+01
## startprice.diff                                                                 6.287101e+01
## idseq.my                                                                        4.557866e+01
## prdline.my.fctriPadmini:idseq.my                                                8.119212e+00
## prdline.my.fctriPadAir:idseq.my                                                 8.102069e+00
## D.ratio.sum.TfIdf.nwrds                                                         6.787692e+00
## prdline.my.fctriPad 3+:idseq.my                                                 6.463037e+00
## prdline.my.fctriPadmini 2+:idseq.my                                             5.541186e+00
## prdline.my.fctriPad 1:idseq.my                                                  5.448197e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio                              4.744887e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds                                   4.653410e+00
## D.TfIdf.sum.stem.stop.Ratio                                                     4.583426e+00
## storage.fctr64                                                                  4.562079e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio                              4.372017e+00
## color.fctrWhite                                                                 3.982307e+00
## D.ratio.nstopwrds.nwrds                                                         3.747225e+00
## storage.fctrUnknown                                                             3.656495e+00
## D.nstopwrds.log                                                                 3.501850e+00
## storage.fctr16                                                                  3.457889e+00
## D.sum.TfIdf                                                                     3.363877e+00
## color.fctrUnknown                                                               3.345165e+00
## D.TfIdf.sum.post.stem                                                           3.183039e+00
## cellular.fctr1                                                                  3.009368e+00
## D.TfIdf.sum.post.stop                                                           2.954056e+00
## prdline.my.fctriPad 2:idseq.my                                                  2.914295e+00
## condition.fctrNew                                                               2.772069e+00
## D.nchrs.log                                                                     2.742885e+00
## D.nwrds.log                                                                     2.697258e+00
## D.nuppr.log                                                                     2.691101e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio                             2.637843e+00
## carrier.fctrNone                                                                2.620120e+00
## carrier.fctrUnknown                                                             2.136666e+00
## color.fctrSpace Gray                                                            2.049710e+00
## condition.fctrNew other (see details)                                           1.824646e+00
## storage.fctr32                                                                  1.763924e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                                         1.756634e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio                               1.669334e+00
## D.terms.n.post.stem.log                                                         1.590060e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds                                   1.578635e+00
## D.terms.n.post.stem                                                             1.567921e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio                          1.567397e+00
## D.nwrds.unq.log                                                                 1.539712e+00
## carrier.fctrVerizon                                                             1.526833e+00
## cellular.fctr1:carrier.fctrVerizon                                              1.485664e+00
## D.npnct13.log                                                                   1.427327e+00
## color.fctrGold                                                                  1.423172e+00
## D.terms.n.post.stop.log                                                         1.421139e+00
## cellular.fctrUnknown                                                            1.374536e+00
## cellular.fctrUnknown:carrier.fctrUnknown                                        1.361053e+00
## prdline.my.fctriPadAir                                                          1.359004e+00
## condition.fctrManufacturer refurbished                                          1.353844e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds                                  1.334240e+00
## D.terms.n.stem.stop.Ratio                                                       1.299448e+00
## condition.fctrFor parts or not working                                          1.289770e+00
## D.terms.n.post.stop                                                             1.279013e+00
## prdline.my.fctriPadmini                                                         1.185258e+00
## condition.fctrSeller refurbished                                                1.141570e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds                                 1.119166e+00
## cellular.fctr1:carrier.fctrUnknown                                              1.115997e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio                               1.085548e+00
## prdline.my.fctriPad 3+                                                          1.074790e+00
## D.npnct11.log                                                                   1.028460e+00
## prdline.my.fctriPadmini:.clusterid.fctr3                                        9.742803e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds                              9.263539e-01
## D.ndgts.log                                                                     9.199387e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                                          9.085270e-01
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds                                  8.016364e-01
## D.npnct12.log                                                                   7.340608e-01
## D.npnct08.log                                                                   7.259117e-01
## prdline.my.fctriPadmini 2+                                                      7.232466e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                                         6.341148e-01
## cellular.fctr1:carrier.fctrSprint                                               6.253784e-01
## D.npnct01.log                                                                   5.556204e-01
## carrier.fctrSprint                                                              5.354769e-01
## prdline.my.fctriPad 1                                                           5.323525e-01
## prdline.my.fctriPad 2                                                           5.238119e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                                         4.086118e-01
## carrier.fctrT-Mobile                                                            4.073964e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                                          4.042061e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                                          3.962993e-01
## D.npnct24.log                                                                   3.935604e-01
## cellular.fctr1:carrier.fctrT-Mobile                                             3.814115e-01
## D.npnct15.log                                                                   3.807822e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                                          3.531661e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                                         3.283063e-01
## prdline.my.fctriPadAir:D.npnct03.log                                            3.097530e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                                     2.966641e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                                        2.849642e-01
## D.npnct05.log                                                                   2.411519e-01
## prdline.my.fctriPadmini:D.npnct15.log                                           2.393818e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                                         2.354775e-01
## D.npnct14.log                                                                   2.344787e-01
## D.npnct16.log                                                                   2.156363e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                                          2.005415e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                                     1.694852e-01
## D.npnct03.log                                                                   1.471806e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                                         1.381536e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                                         1.184789e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                                         9.551081e-02
## prdline.my.fctriPadmini:D.npnct03.log                                           8.979465e-02
## D.npnct06.log                                                                   6.541619e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                                        6.254349e-02
## prdline.my.fctriPadmini:.clusterid.fctr4                                        5.965600e-02
## prdline.my.fctriPad 1:.clusterid.fctr3                                          4.521448e-02
## prdline.my.fctriPad 3+:D.npnct15.log                                            4.097444e-02
## prdline.my.fctriPad 1:D.npnct15.log                                             3.470296e-02
## prdline.my.fctriPad 2:D.npnct15.log                                             1.968935e-02
## prdline.my.fctriPad 3+:.clusterid.fctr6                                         1.838658e-02
## cellular.fctr1:carrier.fctrOther                                                1.831673e-02
## D.npnct10.log                                                                   1.457302e-02
## D.npnct09.log                                                                   7.709772e-03
## carrier.fctrOther                                                               6.459850e-03
## prdline.my.fctriPadmini 2+:D.npnct15.log                                        5.391554e-03
## prdline.my.fctriPadAir:D.npnct15.log                                            3.647228e-03
## prdline.my.fctriPad 3+:D.npnct03.log                                            3.044642e-03
## prdline.my.fctriPad 2:D.npnct03.log                                             2.537202e-03
## prdline.my.fctriPad 1:D.npnct03.log                                             0.000000e+00
## D.npnct28.log                                                                   0.000000e+00
## cellular.fctr1:carrier.fctrNone                                                 0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                                           0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                                          0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                                         0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile                                       0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon                                        0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                                         0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                                         0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                                         0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                                          0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                                          0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                                          0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                                          0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                                         0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                                         0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                                         0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                                     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6                                     0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log                                        0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr6                                        0.000000e+00
##                                                          importance
## biddable                                               1.000000e+02
## startprice.diff:biddable                               6.693838e+01
## startprice.diff                                        6.960528e+01
## idseq.my                                               4.204923e+01
## prdline.my.fctriPadmini:idseq.my                       6.375032e+00
## prdline.my.fctriPadAir:idseq.my                        7.671632e+00
## D.ratio.sum.TfIdf.nwrds                                5.029416e+00
## prdline.my.fctriPad 3+:idseq.my                        5.627514e+00
## prdline.my.fctriPadmini 2+:idseq.my                    5.683911e+00
## prdline.my.fctriPad 1:idseq.my                         4.228074e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio     2.893811e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds          1.328155e+00
## D.TfIdf.sum.stem.stop.Ratio                            4.764236e+00
## storage.fctr64                                         3.115700e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio     3.049947e+00
## color.fctrWhite                                        3.424959e+00
## D.ratio.nstopwrds.nwrds                                4.055811e+00
## storage.fctrUnknown                                    2.611807e+00
## D.nstopwrds.log                                        3.420231e+00
## storage.fctr16                                         3.087964e+00
## D.sum.TfIdf                                            2.969096e+00
## color.fctrUnknown                                      3.323058e+00
## D.TfIdf.sum.post.stem                                  3.012195e+00
## cellular.fctr1                                         2.449276e+00
## D.TfIdf.sum.post.stop                                  3.015868e+00
## prdline.my.fctriPad 2:idseq.my                         4.774435e+00
## condition.fctrNew                                      2.850164e+00
## D.nchrs.log                                            3.032897e+00
## D.nwrds.log                                            2.576687e+00
## D.nuppr.log                                            2.703828e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio    1.661616e+00
## carrier.fctrNone                                       2.339635e+00
## carrier.fctrUnknown                                    1.749041e+00
## color.fctrSpace Gray                                   2.323702e+00
## condition.fctrNew other (see details)                  1.209819e+00
## storage.fctr32                                         1.709455e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                1.107954e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio      2.363800e+00
## D.terms.n.post.stem.log                                1.498671e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds          2.153269e+00
## D.terms.n.post.stem                                    1.495246e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio 1.983204e+00
## D.nwrds.unq.log                                        1.623470e+00
## carrier.fctrVerizon                                    1.234290e+00
## cellular.fctr1:carrier.fctrVerizon                     1.125683e+00
## D.npnct13.log                                          1.641072e+00
## color.fctrGold                                         9.190781e-01
## D.terms.n.post.stop.log                                1.327629e+00
## cellular.fctrUnknown                                   1.272499e+00
## cellular.fctrUnknown:carrier.fctrUnknown               1.329742e+00
## prdline.my.fctriPadAir                                 1.601391e+00
## condition.fctrManufacturer refurbished                 1.240694e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds         1.566376e+00
## D.terms.n.stem.stop.Ratio                              8.127660e-01
## condition.fctrFor parts or not working                 1.211497e+00
## D.terms.n.post.stop                                    1.351248e+00
## prdline.my.fctriPadmini                                9.052589e-01
## condition.fctrSeller refurbished                       1.323465e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds        1.185455e+00
## cellular.fctr1:carrier.fctrUnknown                     1.064053e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio      2.007878e+00
## prdline.my.fctriPad 3+                                 1.091447e+00
## D.npnct11.log                                          1.773518e+00
## prdline.my.fctriPadmini:.clusterid.fctr3               6.830103e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds     7.693623e-01
## D.ndgts.log                                            1.184364e+00
## prdline.my.fctriPad 2:.clusterid.fctr4                 1.254470e+00
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds         9.080687e-01
## D.npnct12.log                                          2.228584e-01
## D.npnct08.log                                          7.021449e-01
## prdline.my.fctriPadmini 2+                             9.675146e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                8.941081e-01
## cellular.fctr1:carrier.fctrSprint                      5.144275e-01
## D.npnct01.log                                          3.384943e-01
## carrier.fctrSprint                                     5.885889e-01
## prdline.my.fctriPad 1                                  8.040097e-01
## prdline.my.fctriPad 2                                  6.203999e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                2.845234e-01
## carrier.fctrT-Mobile                                   5.016182e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                 9.575558e-02
## prdline.my.fctriPad 1:.clusterid.fctr2                 2.636628e-01
## D.npnct24.log                                          2.612274e-01
## cellular.fctr1:carrier.fctrT-Mobile                    6.321343e-01
## D.npnct15.log                                          1.299244e+00
## prdline.my.fctriPad 2:.clusterid.fctr2                 5.527526e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                3.477216e-01
## prdline.my.fctriPadAir:D.npnct03.log                   9.102826e-02
## prdline.my.fctriPadmini 2+:.clusterid.fctr2            1.781904e-01
## prdline.my.fctriPadmini:.clusterid.fctr2               2.600623e-01
## D.npnct05.log                                          1.858545e-01
## prdline.my.fctriPadmini:D.npnct15.log                  1.370333e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                1.995621e-01
## D.npnct14.log                                          4.907743e-01
## D.npnct16.log                                          4.716153e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                 1.163280e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3            2.068904e-01
## D.npnct03.log                                          5.802949e-02
## prdline.my.fctriPadAir:.clusterid.fctr3                1.317783e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                2.147635e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                3.071686e-01
## prdline.my.fctriPadmini:D.npnct03.log                  3.516015e-02
## D.npnct06.log                                          2.138099e-01
## prdline.my.fctriPadmini:.clusterid.fctr5               2.114139e-01
## prdline.my.fctriPadmini:.clusterid.fctr4               6.337059e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                 9.357905e-02
## prdline.my.fctriPad 3+:D.npnct15.log                   4.802268e-02
## prdline.my.fctriPad 1:D.npnct15.log                    3.105568e-01
## prdline.my.fctriPad 2:D.npnct15.log                    1.197634e-02
## prdline.my.fctriPad 3+:.clusterid.fctr6                6.319869e-01
## cellular.fctr1:carrier.fctrOther                       8.801221e-03
## D.npnct10.log                                          8.532980e-02
## D.npnct09.log                                          6.856611e-03
## carrier.fctrOther                                      2.314881e-03
## prdline.my.fctriPadmini 2+:D.npnct15.log               0.000000e+00
## prdline.my.fctriPadAir:D.npnct15.log                   0.000000e+00
## prdline.my.fctriPad 3+:D.npnct03.log                   9.645339e-04
## prdline.my.fctriPad 2:D.npnct03.log                    9.355105e-02
## prdline.my.fctriPad 1:D.npnct03.log                    3.536624e-03
## D.npnct28.log                                          1.736161e-03
## cellular.fctr1:carrier.fctrNone                        0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                  0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                 0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile              0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon               0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                 0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                 0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                 0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4            0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5            0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6            0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log               0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr6               0.000000e+00
##                                                        Final.rf.importance
## biddable                                                      1.000000e+02
## startprice.diff:biddable                                      6.693838e+01
## startprice.diff                                               6.960528e+01
## idseq.my                                                      4.204923e+01
## prdline.my.fctriPadmini:idseq.my                              6.375032e+00
## prdline.my.fctriPadAir:idseq.my                               7.671632e+00
## D.ratio.sum.TfIdf.nwrds                                       5.029416e+00
## prdline.my.fctriPad 3+:idseq.my                               5.627514e+00
## prdline.my.fctriPadmini 2+:idseq.my                           5.683911e+00
## prdline.my.fctriPad 1:idseq.my                                4.228074e+00
## prdline.my.fctriPad 3+:D.TfIdf.sum.stem.stop.Ratio            2.893811e+00
## prdline.my.fctriPad 2:D.ratio.sum.TfIdf.nwrds                 1.328155e+00
## D.TfIdf.sum.stem.stop.Ratio                                   4.764236e+00
## storage.fctr64                                                3.115700e+00
## prdline.my.fctriPadAir:D.TfIdf.sum.stem.stop.Ratio            3.049947e+00
## color.fctrWhite                                               3.424959e+00
## D.ratio.nstopwrds.nwrds                                       4.055811e+00
## storage.fctrUnknown                                           2.611807e+00
## D.nstopwrds.log                                               3.420231e+00
## storage.fctr16                                                3.087964e+00
## D.sum.TfIdf                                                   2.969096e+00
## color.fctrUnknown                                             3.323058e+00
## D.TfIdf.sum.post.stem                                         3.012195e+00
## cellular.fctr1                                                2.449276e+00
## D.TfIdf.sum.post.stop                                         3.015868e+00
## prdline.my.fctriPad 2:idseq.my                                4.774435e+00
## condition.fctrNew                                             2.850164e+00
## D.nchrs.log                                                   3.032897e+00
## D.nwrds.log                                                   2.576687e+00
## D.nuppr.log                                                   2.703828e+00
## prdline.my.fctriPadmini:D.TfIdf.sum.stem.stop.Ratio           1.661616e+00
## carrier.fctrNone                                              2.339635e+00
## carrier.fctrUnknown                                           1.749041e+00
## color.fctrSpace Gray                                          2.323702e+00
## condition.fctrNew other (see details)                         1.209819e+00
## storage.fctr32                                                1.709455e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                       1.107954e+00
## prdline.my.fctriPad 1:D.TfIdf.sum.stem.stop.Ratio             2.363800e+00
## D.terms.n.post.stem.log                                       1.498671e+00
## prdline.my.fctriPad 1:D.ratio.sum.TfIdf.nwrds                 2.153269e+00
## D.terms.n.post.stem                                           1.495246e+00
## prdline.my.fctriPadmini 2+:D.TfIdf.sum.stem.stop.Ratio        1.983204e+00
## D.nwrds.unq.log                                               1.623470e+00
## carrier.fctrVerizon                                           1.234290e+00
## cellular.fctr1:carrier.fctrVerizon                            1.125683e+00
## D.npnct13.log                                                 1.641072e+00
## color.fctrGold                                                9.190781e-01
## D.terms.n.post.stop.log                                       1.327629e+00
## cellular.fctrUnknown                                          1.272499e+00
## cellular.fctrUnknown:carrier.fctrUnknown                      1.329742e+00
## prdline.my.fctriPadAir                                        1.601391e+00
## condition.fctrManufacturer refurbished                        1.240694e+00
## prdline.my.fctriPad 3+:D.ratio.sum.TfIdf.nwrds                1.566376e+00
## D.terms.n.stem.stop.Ratio                                     8.127660e-01
## condition.fctrFor parts or not working                        1.211497e+00
## D.terms.n.post.stop                                           1.351248e+00
## prdline.my.fctriPadmini                                       9.052589e-01
## condition.fctrSeller refurbished                              1.323465e+00
## prdline.my.fctriPadmini:D.ratio.sum.TfIdf.nwrds               1.185455e+00
## cellular.fctr1:carrier.fctrUnknown                            1.064053e+00
## prdline.my.fctriPad 2:D.TfIdf.sum.stem.stop.Ratio             2.007878e+00
## prdline.my.fctriPad 3+                                        1.091447e+00
## D.npnct11.log                                                 1.773518e+00
## prdline.my.fctriPadmini:.clusterid.fctr3                      6.830103e-01
## prdline.my.fctriPadmini 2+:D.ratio.sum.TfIdf.nwrds            7.693623e-01
## D.ndgts.log                                                   1.184364e+00
## prdline.my.fctriPad 2:.clusterid.fctr4                        1.254470e+00
## prdline.my.fctriPadAir:D.ratio.sum.TfIdf.nwrds                9.080687e-01
## D.npnct12.log                                                 2.228584e-01
## D.npnct08.log                                                 7.021449e-01
## prdline.my.fctriPadmini 2+                                    9.675146e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                       8.941081e-01
## cellular.fctr1:carrier.fctrSprint                             5.144275e-01
## D.npnct01.log                                                 3.384943e-01
## carrier.fctrSprint                                            5.885889e-01
## prdline.my.fctriPad 1                                         8.040097e-01
## prdline.my.fctriPad 2                                         6.203999e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                       2.845234e-01
## carrier.fctrT-Mobile                                          5.016182e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                        9.575558e-02
## prdline.my.fctriPad 1:.clusterid.fctr2                        2.636628e-01
## D.npnct24.log                                                 2.612274e-01
## cellular.fctr1:carrier.fctrT-Mobile                           6.321343e-01
## D.npnct15.log                                                 1.299244e+00
## prdline.my.fctriPad 2:.clusterid.fctr2                        5.527526e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                       3.477216e-01
## prdline.my.fctriPadAir:D.npnct03.log                          9.102826e-02
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                   1.781904e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                      2.600623e-01
## D.npnct05.log                                                 1.858545e-01
## prdline.my.fctriPadmini:D.npnct15.log                         1.370333e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                       1.995621e-01
## D.npnct14.log                                                 4.907743e-01
## D.npnct16.log                                                 4.716153e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                        1.163280e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                   2.068904e-01
## D.npnct03.log                                                 5.802949e-02
## prdline.my.fctriPadAir:.clusterid.fctr3                       1.317783e-01
## prdline.my.fctriPad 3+:.clusterid.fctr5                       2.147635e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                       3.071686e-01
## prdline.my.fctriPadmini:D.npnct03.log                         3.516015e-02
## D.npnct06.log                                                 2.138099e-01
## prdline.my.fctriPadmini:.clusterid.fctr5                      2.114139e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                      6.337059e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                        9.357905e-02
## prdline.my.fctriPad 3+:D.npnct15.log                          4.802268e-02
## prdline.my.fctriPad 1:D.npnct15.log                           3.105568e-01
## prdline.my.fctriPad 2:D.npnct15.log                           1.197634e-02
## prdline.my.fctriPad 3+:.clusterid.fctr6                       6.319869e-01
## cellular.fctr1:carrier.fctrOther                              8.801221e-03
## D.npnct10.log                                                 8.532980e-02
## D.npnct09.log                                                 6.856611e-03
## carrier.fctrOther                                             2.314881e-03
## prdline.my.fctriPadmini 2+:D.npnct15.log                      0.000000e+00
## prdline.my.fctriPadAir:D.npnct15.log                          0.000000e+00
## prdline.my.fctriPad 3+:D.npnct03.log                          9.645339e-04
## prdline.my.fctriPad 2:D.npnct03.log                           9.355105e-02
## prdline.my.fctriPad 1:D.npnct03.log                           3.536624e-03
## D.npnct28.log                                                 1.736161e-03
## cellular.fctr1:carrier.fctrNone                               0.000000e+00
## cellular.fctrUnknown:carrier.fctrNone                         0.000000e+00
## cellular.fctrUnknown:carrier.fctrOther                        0.000000e+00
## cellular.fctrUnknown:carrier.fctrSprint                       0.000000e+00
## cellular.fctrUnknown:carrier.fctrT-Mobile                     0.000000e+00
## cellular.fctrUnknown:carrier.fctrVerizon                      0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr4                       0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                       0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr6                       0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                        0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr6                        0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                        0.000000e+00
## prdline.my.fctriPad 2:.clusterid.fctr6                        0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr4                       0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                       0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr6                       0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                   0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                   0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr6                   0.000000e+00
## prdline.my.fctriPadmini 2+:D.npnct03.log                      0.000000e+00
## prdline.my.fctriPadmini:.clusterid.fctr6                      0.000000e+00
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

![](ebayipads_color_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 630 168
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
## 16     predict.data.new          9          0 398.126 404.414   6.288
## 17 display.session.info         10          0 404.414      NA      NA
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
## 11              fit.models          7          1 161.264 327.363 166.099
## 5         extract.features          3          0  40.685 125.107  84.422
## 14       fit.data.training          8          0 361.584 392.647  31.063
## 12              fit.models          7          2 327.364 356.670  29.306
## 10              fit.models          7          0 134.301 161.263  26.962
## 2             inspect.data          2          0  26.493  39.269  12.776
## 1              import.data          1          0  14.411  26.493  12.082
## 16        predict.data.new          9          0 398.126 404.414   6.288
## 15       fit.data.training          8          1 392.648 398.125   5.477
## 13              fit.models          7          3 356.671 361.584   4.913
## 8          select.features          5          0 129.395 133.412   4.017
## 7      manage.missing.data          4          1 126.074 129.394   3.320
## 6             cluster.data          4          0 125.107 126.073   0.966
## 9  partition.data.training          6          0 133.412 134.301   0.889
## 3               scrub.data          2          1  39.269  40.040   0.771
## 4           transform.data          2          2  40.041  40.684   0.643
##    duration
## 11  166.099
## 5    84.422
## 14   31.063
## 12   29.306
## 10   26.962
## 2    12.776
## 1    12.082
## 16    6.288
## 15    5.477
## 13    4.913
## 8     4.017
## 7     3.320
## 6     0.966
## 9     0.889
## 3     0.771
## 4     0.643
## [1] "Total Elapsed Time: 404.414 secs"
```

![](ebayipads_color_files/figure-html/display.session.info-1.png) 

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
