# eBay:iPads:: sold classification:: csmmdl
bdanalytics  

**  **    
**Date: (Sun) Jul 26, 2015**    

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
glb_out_pfx <- "csmmdl_"
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
                                ,"either","erased","ereader","essentially","every", "exact"
                                        ,"film","final","flickers","folding"
                                        ,"generic","genuine","glitter","goes"
                                    ,"handstand","hdmi","high","higher","hole","hospital"
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
        ,"shell","short","size","slice","smoke"
        ,"softer","software","somewhat","soon"
        ,"sparingly","sparkiling","special","speed"
        ,"stains","standup","status","stopped","subtle","sustained","swappacom"
                            ,"technical","texture","therefore","though","totally","touchy"
                                        ,"university","unknown","untouched","upgrade"
                                        ,"valid","vary","version"
                                        ,"want","website","winning","wrapped"
                                        ,"zaag","zombie"
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
glb_exclude_vars_as_features <- c("productline", "startprice", "description") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- TRUE

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

![](ebayipads_csmmdl_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 11.308  NA      NA
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

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0 11.308 15.312   4.004
## 2 inspect.data          2          0 15.312     NA      NA
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

![](ebayipads_csmmdl_files/figure-html/inspect.data-1.png) 

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

![](ebayipads_csmmdl_files/figure-html/inspect.data-2.png) 

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

![](ebayipads_csmmdl_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_csmmdl_files/figure-html/inspect.data-4.png) 

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
## 2 inspect.data          2          0 15.312 19.122    3.81
## 3   scrub.data          2          1 19.123     NA      NA
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
## 3     scrub.data          2          1 19.123 19.836   0.713
## 4 transform.data          2          2 19.837     NA      NA
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
## 4   transform.data          2          2 19.837 20.381   0.544
## 5 extract.features          3          0 20.381     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 20.387  NA      NA
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
## 1                extract.features_bgn          1          0 20.387 20.401
## 2 extract.features_factorize.str.vars          2          0 20.402     NA
##   elapsed
## 1   0.014
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
##                                 label step_major step_minor    bgn   end
## 2 extract.features_factorize.str.vars          2          0 20.402 20.68
## 3       extract.features_process.text          3          0 20.681    NA
##   elapsed
## 2   0.279
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
## 3          0 20.681 22.396   1.715
## 4          1 22.397     NA      NA
## [1] "Remaining compound terms in descr.my: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn    end elapsed
## 4          1 22.397 22.401   0.004
## 5          0 22.401     NA      NA
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
## [1] "Rows: 741; Cols: 4"
##              TfIdf      term freq pos
## condition 207.8887 condition  498 149
## new       125.5866       new  156 436
## used      123.3991      used  240 706
## good      120.8627      good  197 290
## scratches 113.8942 scratches  254 573
## screen    106.4789    screen  210 575
##                 TfIdf        term freq pos
## description 29.523233 description   33 201
## pristine     6.488452    pristine    5 513
## sell         5.766727        sell    4 585
## pics         5.461987        pics    4 487
## backside     3.458528    backside    2  67
## bright       1.980793      bright    2  95
##                 TfIdf        term freq pos
## 975         1.1375583         975    1  16
## blemish     1.1375583     blemish    1  83
## cables      1.1375583      cables    1 106
## engravement 1.1375583 engravement    1 226
## handling    1.1375583    handling    1 306
## 79in        0.9479652        79in    1  15
##                 TfIdf        term freq pos
## 975         1.1375583         975    1  16
## blemish     1.1375583     blemish    1  83
## cables      1.1375583      cables    1 106
## engravement 1.1375583 engravement    1 226
## handling    1.1375583    handling    1 306
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
## [1] "Rows: 600; Cols: 4"
##            TfIdf    term freq pos
## condit  207.8742  condit  496 122
## use     146.4897     use  291 570
## scratch 128.1897 scratch  286 460
## new     125.5866     new  156 349
## good    120.9521    good  197 234
## screen  107.3402  screen  213 461
##                   TfIdf          term freq pos
## look          17.038461          look   21 309
## wrap           3.248920          wrap    2 595
## now            2.882106           now    2 357
## silver         2.459066        silver    3 486
## mayb           2.190401          mayb    2 325
## unlockedcrack  1.421948 unlockedcrack    1 559
##             TfIdf    term freq pos
## think   1.2639536   think    1 537
## toddler 1.2639536 toddler    1 546
## tri     1.2639536     tri    1 551
## typic   1.2639536   typic    1 555
## 975     1.1375583     975    1  16
## 79in    0.9479652    79in    1  15
##             TfIdf    term freq pos
## think   1.2639536   think    1 537
## toddler 1.2639536 toddler    1 546
## tri     1.2639536     tri    1 551
## typic   1.2639536   typic    1 555
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
## terms.n.post.stop         -0.084070071
## terms.n.post.stop.log     -0.065637762
## TfIdf.sum.post.stop       -0.033322152
## terms.n.post.stem         -0.083920992
## terms.n.post.stem.log     -0.065642430
## TfIdf.sum.post.stem       -0.036079650
## terms.n.stem.stop.Ratio    0.016386012
## TfIdf.sum.stem.stop.Ratio -0.009279696
##                           label step_major step_minor    bgn    end
## 5 extract.features_build.corpus          4          0 22.401 33.259
## 6  extract.features_extract.DTM          5          0 33.259     NA
##   elapsed
## 5  10.858
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
## 6 extract.features_extract.DTM          5          0 33.259 34.633   1.375
## 7  extract.features_report.DTM          6          0 34.634     NA      NA
## [1] "Reporting TfIDf terms for descr.my..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 2657, terms: 600)>>
## Non-/sparse entries: 8280/1585920
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

![](ebayipads_csmmdl_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_csmmdl_files/figure-html/extract.features-2.png) ![](ebayipads_csmmdl_files/figure-html/extract.features-3.png) 

```
## Warning in rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df,
## terms_TfIdf_df): object 'full_TfIdf_mtrx' not found
```

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 34.634 36.791   2.157
## 8   extract.features_bind.DTM          7          0 36.792     NA      NA
## [1] "Binding DTM for descr.my..."
##                       label step_major step_minor    bgn    end elapsed
## 8 extract.features_bind.DTM          7          0 36.792 37.245   0.454
## 9 extract.features_bind.DXM          8          0 37.246     NA      NA
## [1] "Binding DXM for descr.my..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_csmmdl_files/figure-html/extract.features-4.png) 

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
##                        label step_major step_minor    bgn    end elapsed
## 9  extract.features_bind.DXM          8          0 37.246 86.124  48.878
## 10      extract.features_end          9          0 86.124     NA      NA
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
## 9          0 37.246 86.124  48.878   48.878
## 5          0 22.401 33.259  10.858   10.858
## 7          0 34.634 36.791   2.157    2.157
## 3          0 20.681 22.396   1.715    1.715
## 6          0 33.259 34.633   1.375    1.374
## 8          0 36.792 37.245   0.454    0.453
## 2          0 20.402 20.680   0.279    0.278
## 1          0 20.387 20.401   0.014    0.014
## 4          1 22.397 22.401   0.004    0.004
## [1] "Total Elapsed Time: 86.124 secs"
```

![](ebayipads_csmmdl_files/figure-html/extract.features-5.png) 

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

![](ebayipads_csmmdl_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 20.381 87.484  67.103
## 6     cluster.data          4          0 87.484     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 87.484 88.527   1.043
## 7 manage.missing.data          4          1 88.527     NA      NA
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
##              D.T.screen                D.T.ipad               D.T.great 
##                    2444                    2425                    2532 
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
##              D.T.screen                D.T.ipad               D.T.great 
##                    2444                    2425                    2532 
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
##  [6] "D.T.screen"  "D.T.ipad"    "D.T.great"   "D.T.work"    "D.T.excel"  
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
##     D.T.condit D.T.use D.T.scratch   D.T.new D.T.good D.T.screen D.T.ipad
## 5            0       0           0 0.0000000        0          0        0
## 130          0       0           0 0.8180361        0          0        0
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
##      D.T.condit   D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 1029   0.220126 0.5801286           0       0        0          0        0
## 1077   0.220126 0.5801286           0       0        0          0        0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 1029         0        0         0        0        0       0
## 1077         0        0         0        0        0       0
```

![](ebayipads_csmmdl_files/figure-html/cluster.data-1.png) 

```
## [1] "Category: iPad 1"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 9     10009         Y     iPad 1
## 13    10013         Y     iPad 1
##                                                                                            descr.my
## 9                                                                                                  
## 13 GOOD CONDITION. CLEAN ICLOUD. NO LOCKS. CLEAN IMEI. This tablet has been fully tested and works 
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.screen D.T.ipad
## 9    0.000000       0           0       0 0.0000000          0        0
## 13   0.220126       0           0       0 0.3412301          0        0
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
##      D.T.condit  D.T.use D.T.scratch D.T.new D.T.good D.T.screen  D.T.ipad
## 471   0.1862605 0.245439           0       0        0          0 0.2705847
## 1202  0.1862605 0.245439           0       0        0          0 0.2705847
##      D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 471  0.3392152 0.2881712         0        0        0       0
## 1202 0.3392152 0.2881712         0        0        0       0
```

![](ebayipads_csmmdl_files/figure-html/cluster.data-2.png) 

```
## [1] "Category: iPad 2"
## [1] "max distance(1.0000) pair:"
##   UniqueID sold.fctr prdline.my
## 1    10001         N     iPad 2
## 2    10002         Y     iPad 2
##                                                                                               descr.my
## 1                                                        iPad is in 8.5+ out of 10 cosmetic condition!
## 2 Previously used, please read description. May show signs of use such as scratches to the screen and 
##   D.T.condit   D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 1  0.8071287 0.0000000   0.0000000       0        0  0.0000000 1.172534
## 2  0.0000000 0.5801286   0.2923374       0        0  0.3309884 0.000000
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
##      D.T.condit   D.T.use D.T.scratch D.T.new  D.T.good D.T.screen
## 132   0.2017822 0.2658923   0.2679759       0 0.3127942          0
## 2382  0.2421386 0.3190707   0.3215711       0 0.3753531          0
##      D.T.ipad D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 132         0         0        0         0        0        0       0
## 2382        0         0        0         0        0        0       0
```

![](ebayipads_csmmdl_files/figure-html/cluster.data-3.png) 

```
## [1] "Category: iPad 3+"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 3     10003         Y    iPad 3+
## 11    10011         Y    iPad 3+
##                                                                                        descr.my
## 3                                                                                              
## 11 good condition, minor wear and tear on body some light scratches on screen. functions great.
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.screen D.T.ipad
## 3    0.000000       0   0.0000000       0 0.0000000  0.0000000        0
## 11   0.220126       0   0.2923374       0 0.3412301  0.3309884        0
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
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 40            0       0           0       0        0          0        0
## 1602          0       0           0       0        0          0        0
##      D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 40           0 0.4162473         0        0        0       0
## 1602         0 0.9365565         0        0        0       0
```

![](ebayipads_csmmdl_files/figure-html/cluster.data-4.png) 

```
## [1] "Category: iPadAir"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 16    10016         N    iPadAir
## 33    10033         N    iPadAir
##                                                                                                descr.my
## 16                                                                                                     
## 33 We are selling good quality iPads that have been fully tested by an Apple Certified Technician. The 
##    D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen  D.T.ipad
## 16          0       0           0       0 0.000000          0 0.0000000
## 33          0       0           0       0 0.417059          0 0.3908446
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
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 44     0.220126       0           0       0        0          0        0
## 1233   1.210693       0           0       0        0          0        0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 44           0        0         0        0        0       0
## 1233         0        0         0        0        0       0
```

![](ebayipads_csmmdl_files/figure-html/cluster.data-5.png) 

```
## [1] "Category: iPadmini"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr prdline.my
## 7     10007         Y   iPadmini
## 76    10076         Y   iPadmini
##                                                                                         descr.my
## 7                                                                                               
## 76 Works perfectly, NOT iCloud locked, 1 owner. It is in not in very good  condition, but works 
##    D.T.condit D.T.use D.T.scratch D.T.new  D.T.good D.T.screen D.T.ipad
## 7   0.0000000       0           0       0 0.0000000          0        0
## 76  0.3026733       0           0       0 0.4691913          0        0
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
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 491           0       0           0       0        0  0.4551091        0
## 1753          0       0           0       0        0  0.4045414        0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 491          0        0         0        0        0       0
## 1753         0        0         0        0        0       0
```

![](ebayipads_csmmdl_files/figure-html/cluster.data-6.png) 

```
## [1] "Category: iPadmini 2+"
## [1] "max distance(1.0000) pair:"
##    UniqueID sold.fctr  prdline.my
## 4     10004         N iPadmini 2+
## 18    10018         N iPadmini 2+
##                                                                                                descr.my
## 4                                                                                                      
## 18 We are selling good quality iPads that have been fully tested by an Apple Certified Technician. The 
##    D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen  D.T.ipad
## 4           0       0           0       0 0.000000          0 0.0000000
## 18          0       0           0       0 0.417059          0 0.3908446
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 4          0        0         0        0        0       0
## 18         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr  prdline.my
## 750     10750         Y iPadmini 2+
## 2289    12291      <NA> iPadmini 2+
##                                                                                                  descr.my
## 750                                                                      Slight crack on very top screen.
## 2289 Has a cracked screen and has always been in an otter box.I had removed case to clean and my toddler 
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 750           0       0           0       0        0  0.9102182        0
## 2289          0       0           0       0        0  0.4045414        0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 750          0        0         0        0        0       0
## 2289         0        0         0        0        0       0
```

![](ebayipads_csmmdl_files/figure-html/cluster.data-7.png) 

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
## 7 manage.missing.data          4          1 88.527 91.971   3.445
## 8     select.features          5          0 91.972     NA      NA
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
## UniqueID                                       UniqueID -0.1895466260
## idseq.my                                       idseq.my -0.1895466260
## condition.fctr                           condition.fctr -0.1535490071
## D.npnct05.log                             D.npnct05.log -0.1180558939
## D.terms.n.post.stop                 D.terms.n.post.stop -0.0840700705
## D.terms.n.post.stem                 D.terms.n.post.stem -0.0839209919
## D.npnct14.log                             D.npnct14.log -0.0786203827
## cellular.fctr                             cellular.fctr -0.0739443419
## carrier.fctr                               carrier.fctr -0.0687945647
## D.terms.n.post.stem.log         D.terms.n.post.stem.log -0.0656424297
## D.nwrds.unq.log                         D.nwrds.unq.log -0.0656424297
## D.terms.n.post.stop.log         D.terms.n.post.stop.log -0.0656377617
## D.ndgts.log                                 D.ndgts.log -0.0637072932
## D.npnct09.log                             D.npnct09.log -0.0618253281
## D.nwrds.log                                 D.nwrds.log -0.0590061339
## D.npnct12.log                             D.npnct12.log -0.0568348971
## D.nchrs.log                                 D.nchrs.log -0.0565369891
## D.ratio.nstopwrds.nwrds         D.ratio.nstopwrds.nwrds  0.0565347637
## D.nuppr.log                                 D.nuppr.log -0.0553942172
## D.npnct28.log                             D.npnct28.log -0.0524583244
## D.npnct06.log                             D.npnct06.log -0.0499761958
## D.npnct15.log                             D.npnct15.log  0.0484022793
## D.npnct24.log                             D.npnct24.log -0.0458449965
## D.nstopwrds.log                         D.nstopwrds.log -0.0456417635
## D.npnct16.log                             D.npnct16.log -0.0449403962
## color.fctr                                   color.fctr -0.0433350320
## prdline.my.fctr                         prdline.my.fctr -0.0415814340
## D.npnct08.log                             D.npnct08.log -0.0396513123
## D.T.new                                         D.T.new -0.0384952089
## D.npnct13.log                             D.npnct13.log -0.0373463069
## D.T.condit                                   D.T.condit -0.0372461578
## D.TfIdf.sum.post.stem             D.TfIdf.sum.post.stem -0.0360796497
## D.sum.TfIdf                                 D.sum.TfIdf -0.0360796497
## D.TfIdf.sum.post.stop             D.TfIdf.sum.post.stop -0.0333221522
## .clusterid                                   .clusterid -0.0331547453
## .clusterid.fctr                         .clusterid.fctr -0.0331547453
## D.T.excel                                     D.T.excel  0.0264721403
## D.npnct03.log                             D.npnct03.log  0.0257637868
## D.npnct07.log                             D.npnct07.log  0.0250040676
## D.T.screen                                   D.T.screen  0.0249545788
## D.npnct10.log                             D.npnct10.log -0.0241015016
## D.npnct18.log                             D.npnct18.log -0.0215250231
## D.npnct11.log                             D.npnct11.log -0.0192035548
## D.terms.n.stem.stop.Ratio     D.terms.n.stem.stop.Ratio  0.0163860117
## D.T.ipad                                       D.T.ipad -0.0144429726
## D.T.work                                       D.T.work -0.0124402416
## D.T.use                                         D.T.use  0.0121001107
## D.P.mini                                       D.P.mini -0.0112418293
## storage.fctr                               storage.fctr -0.0110508589
## D.TfIdf.sum.stem.stop.Ratio D.TfIdf.sum.stem.stop.Ratio -0.0092796960
## D.P.air                                         D.P.air -0.0092629952
## D.ratio.sum.TfIdf.nwrds         D.ratio.sum.TfIdf.nwrds  0.0078076980
## D.T.great                                     D.T.great  0.0076325420
## D.T.scratch                                 D.T.scratch -0.0069105878
## .rnorm                                           .rnorm  0.0067562740
## D.npnct01.log                             D.npnct01.log  0.0041255300
## D.T.good                                       D.T.good -0.0003806625
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
## startprice.log                            0 0.4674275376
## startprice                                1 0.4569767211
## UniqueID                                  1 0.1895466260
## idseq.my                                  0 0.1895466260
## condition.fctr                            0 0.1535490071
## D.npnct05.log                             0 0.1180558939
## D.terms.n.post.stop                       0 0.0840700705
## D.terms.n.post.stem                       0 0.0839209919
## D.npnct14.log                             0 0.0786203827
## cellular.fctr                             0 0.0739443419
## carrier.fctr                              0 0.0687945647
## D.terms.n.post.stem.log                   0 0.0656424297
## D.nwrds.unq.log                           0 0.0656424297
## D.terms.n.post.stop.log                   0 0.0656377617
## D.ndgts.log                               0 0.0637072932
## D.npnct09.log                             0 0.0618253281
## D.nwrds.log                               0 0.0590061339
## D.npnct12.log                             0 0.0568348971
## D.nchrs.log                               0 0.0565369891
## D.ratio.nstopwrds.nwrds                   0 0.0565347637
## D.nuppr.log                               0 0.0553942172
## D.npnct28.log                             0 0.0524583244
## D.npnct06.log                             0 0.0499761958
## D.npnct15.log                             0 0.0484022793
## D.npnct24.log                             0 0.0458449965
## D.nstopwrds.log                           0 0.0456417635
## D.npnct16.log                             0 0.0449403962
## color.fctr                                0 0.0433350320
## prdline.my.fctr                           0 0.0415814340
## D.npnct08.log                             0 0.0396513123
## D.T.new                                   1 0.0384952089
## D.npnct13.log                             0 0.0373463069
## D.T.condit                                1 0.0372461578
## D.TfIdf.sum.post.stem                     0 0.0360796497
## D.sum.TfIdf                               0 0.0360796497
## D.TfIdf.sum.post.stop                     0 0.0333221522
## .clusterid                                1 0.0331547453
## .clusterid.fctr                           0 0.0331547453
## D.T.excel                                 1 0.0264721403
## D.npnct03.log                             0 0.0257637868
## D.npnct07.log                             0 0.0250040676
## D.T.screen                                1 0.0249545788
## D.npnct10.log                             0 0.0241015016
## D.npnct18.log                             0 0.0215250231
## D.npnct11.log                             0 0.0192035548
## D.terms.n.stem.stop.Ratio                 0 0.0163860117
## D.T.ipad                                  1 0.0144429726
## D.T.work                                  1 0.0124402416
## D.T.use                                   1 0.0121001107
## D.P.mini                                  1 0.0112418293
## storage.fctr                              0 0.0110508589
## D.TfIdf.sum.stem.stop.Ratio               0 0.0092796960
## D.P.air                                   1 0.0092629952
## D.ratio.sum.TfIdf.nwrds                   0 0.0078076980
## D.T.great                                 1 0.0076325420
## D.T.scratch                               1 0.0069105878
## .rnorm                                    0 0.0067562740
## D.npnct01.log                             0 0.0041255300
## D.T.good                                  1 0.0003806625
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
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0841"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stem as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.TfIdf.sum.post.stop)=0.9977"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0361"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stop)=-0.0333"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stop as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nchrs.log, D.nwrds.unq.log)=0.9933"
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
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stop)=0.9753"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0656"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0841"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.unq.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.npnct24.log, D.ratio.nstopwrds.nwrds)=-0.9643"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0458"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0565"
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
## [1] "cor(D.TfIdf.sum.post.stem, D.ratio.nstopwrds.nwrds)=-0.9256"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0361"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0565"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stem as highly correlated with
## D.ratio.nstopwrds.nwrds
```

```
## [1] "cor(D.nstopwrds.log, D.terms.n.post.stop)=0.8935"
## [1] "cor(sold.fctr, D.nstopwrds.log)=-0.0456"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0841"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nstopwrds.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.ratio.nstopwrds.nwrds, D.terms.n.post.stop)=-0.8678"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0565"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0841"
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
## [1] "cor(D.npnct13.log, D.terms.n.post.stop)=0.7383"
## [1] "cor(sold.fctr, D.npnct13.log)=-0.0373"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0841"
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
## 56     D.ratio.nstopwrds.nwrds  0.0565347637               0 0.0565347637
## 36               D.npnct15.log  0.0484022793               0 0.0484022793
## 8                    D.T.excel  0.0264721403               1 0.0264721403
## 24               D.npnct03.log  0.0257637868               0 0.0257637868
## 28               D.npnct07.log  0.0250040676               0 0.0250040676
## 14                  D.T.screen  0.0249545788               1 0.0249545788
## 63   D.terms.n.stem.stop.Ratio  0.0163860117               0 0.0163860117
## 15                     D.T.use  0.0121001107               1 0.0121001107
## 57     D.ratio.sum.TfIdf.nwrds  0.0078076980               0 0.0078076980
## 10                   D.T.great  0.0076325420               1 0.0076325420
## 3                       .rnorm  0.0067562740               0 0.0067562740
## 22               D.npnct01.log  0.0041255300               0 0.0041255300
## 9                     D.T.good -0.0003806625               1 0.0003806625
## 13                 D.T.scratch -0.0069105878               1 0.0069105878
## 4                      D.P.air -0.0092629952               1 0.0092629952
## 19 D.TfIdf.sum.stem.stop.Ratio -0.0092796960               0 0.0092796960
## 75                storage.fctr -0.0110508589               0 0.0110508589
## 6                     D.P.mini -0.0112418293               1 0.0112418293
## 16                    D.T.work -0.0124402416               1 0.0124402416
## 11                    D.T.ipad -0.0144429726               1 0.0144429726
## 32               D.npnct11.log -0.0192035548               0 0.0192035548
## 39               D.npnct18.log -0.0215250231               0 0.0215250231
## 31               D.npnct10.log -0.0241015016               0 0.0241015016
## 1                   .clusterid -0.0331547453               1 0.0331547453
## 2              .clusterid.fctr -0.0331547453               0 0.0331547453
## 18       D.TfIdf.sum.post.stop -0.0333221522               0 0.0333221522
## 17       D.TfIdf.sum.post.stem -0.0360796497               0 0.0360796497
## 58                 D.sum.TfIdf -0.0360796497               0 0.0360796497
## 7                   D.T.condit -0.0372461578               1 0.0372461578
## 34               D.npnct13.log -0.0373463069               0 0.0373463069
## 12                     D.T.new -0.0384952089               1 0.0384952089
## 29               D.npnct08.log -0.0396513123               0 0.0396513123
## 71             prdline.my.fctr -0.0415814340               0 0.0415814340
## 68                  color.fctr -0.0433350320               0 0.0433350320
## 37               D.npnct16.log -0.0449403962               0 0.0449403962
## 52             D.nstopwrds.log -0.0456417635               0 0.0456417635
## 45               D.npnct24.log -0.0458449965               0 0.0458449965
## 27               D.npnct06.log -0.0499761958               0 0.0499761958
## 49               D.npnct28.log -0.0524583244               0 0.0524583244
## 53                 D.nuppr.log -0.0553942172               0 0.0553942172
## 20                 D.nchrs.log -0.0565369891               0 0.0565369891
## 33               D.npnct12.log -0.0568348971               0 0.0568348971
## 54                 D.nwrds.log -0.0590061339               0 0.0590061339
## 30               D.npnct09.log -0.0618253281               0 0.0618253281
## 21                 D.ndgts.log -0.0637072932               0 0.0637072932
## 62     D.terms.n.post.stop.log -0.0656377617               0 0.0656377617
## 55             D.nwrds.unq.log -0.0656424297               0 0.0656424297
## 60     D.terms.n.post.stem.log -0.0656424297               0 0.0656424297
## 66                carrier.fctr -0.0687945647               0 0.0687945647
## 67               cellular.fctr -0.0739443419               0 0.0739443419
## 35               D.npnct14.log -0.0786203827               0 0.0786203827
## 59         D.terms.n.post.stem -0.0839209919               0 0.0839209919
## 61         D.terms.n.post.stop -0.0840700705               0 0.0840700705
## 26               D.npnct05.log -0.1180558939               0 0.1180558939
## 69              condition.fctr -0.1535490071               0 0.1535490071
## 64                    UniqueID -0.1895466260               1 0.1895466260
## 70                    idseq.my -0.1895466260               0 0.1895466260
## 73                  startprice -0.4569767211               1 0.4569767211
## 74              startprice.log -0.4674275376               0 0.4674275376
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
## 56     D.terms.n.post.stop   13.209877    4.24959656   FALSE FALSE
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
## 9                     <NA>   44.153846    0.86067778   FALSE  TRUE
## 13                    <NA>   40.390244    0.86067778   FALSE  TRUE
## 4                     <NA>  122.866667    0.16137708   FALSE  TRUE
## 19                    <NA>   65.176471   32.92092523   FALSE FALSE
## 75                    <NA>    2.733138    0.26896181   FALSE FALSE
## 6                     <NA>   91.900000    0.16137708   FALSE  TRUE
## 16                    <NA>   57.266667    0.69930070   FALSE  TRUE
## 11                    <NA>   48.400000    0.80688542   FALSE  TRUE
## 32                    <NA>    9.374269    0.37654653   FALSE FALSE
## 39                    <NA> 1858.000000    0.10758472   FALSE  TRUE
## 31                    <NA>  308.666667    0.16137708   FALSE  TRUE
## 1                     <NA>    3.987013    0.26896181   FALSE FALSE
## 2                     <NA>    3.987013    0.26896181   FALSE FALSE
## 18   D.TfIdf.sum.post.stem   63.000000   34.42711135   FALSE FALSE
## 17 D.ratio.nstopwrds.nwrds   63.000000   34.31952663   FALSE FALSE
## 58   D.TfIdf.sum.post.stem   63.000000   34.31952663   FALSE FALSE
## 7                     <NA>   22.641791    0.91447015   FALSE  TRUE
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
## 62         D.nwrds.unq.log    7.992537    0.80688542   FALSE FALSE
## 55     D.terms.n.post.stop    7.542254    0.80688542   FALSE FALSE
## 60         D.nwrds.unq.log    7.542254    0.80688542   FALSE FALSE
## 66           cellular.fctr    3.195965    0.37654653   FALSE FALSE
## 67                    <NA>    2.112381    0.16137708   FALSE FALSE
## 35                    <NA>   35.333333    0.26896181   FALSE  TRUE
## 59     D.terms.n.post.stop    7.542254    0.80688542   FALSE FALSE
## 61                    <NA>    7.992537    0.80688542   FALSE FALSE
## 26                    <NA>   40.311111    0.10758472   FALSE  TRUE
## 69                    <NA>    4.003460    0.32275417   FALSE FALSE
## 64                    <NA>    1.000000  100.00000000   FALSE FALSE
## 70                    <NA>    1.000000  100.00000000   FALSE FALSE
## 73                    <NA>    2.807692   30.17751479   FALSE FALSE
## 74                    <NA>    2.807692   30.17751479   FALSE FALSE
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
## 13    FALSE            FALSE
## 4     FALSE            FALSE
## 19    FALSE            FALSE
## 75    FALSE            FALSE
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
## 73    FALSE            FALSE
## 74    FALSE            FALSE
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
## Warning: Removed 10 rows containing missing values (geom_point).
```

```
## Warning: Removed 10 rows containing missing values (geom_point).
```

```
## Warning: Removed 10 rows containing missing values (geom_point).
```

![](ebayipads_csmmdl_files/figure-html/select.features-1.png) 

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
##              D.T.screen                D.T.ipad               D.T.great 
##                    2444                    2425                    2532 
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
## 8         select.features          5          0 91.972 95.802    3.83
## 9 partition.data.training          6          0 95.802     NA      NA
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
## [1] 75 12
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
## [1] 2657   72
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 1859   71
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 974  71
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 885  71
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 798  71
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
## 9  partition.data.training          6          0 95.802 96.688   0.887
## 10              fit.models          7          0 96.689     NA      NA
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
## 1                      0.405                 0.002         0.5
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

![](ebayipads_csmmdl_files/figure-html/fit.models_0-1.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_0-2.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_0-3.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.265                 0.001   0.5071756
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
## [1] "    indep_vars: biddable, startprice.log"
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

![](ebayipads_csmmdl_files/figure-html/fit.models_0-5.png) 

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
##               model_id model_method                    feats
## 1 Max.cor.Y.cv.0.rpart        rpart biddable, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.631                 0.013
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
## [1] "    indep_vars: biddable, startprice.log"
## Fitting cp = 0 on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##              CP nsplit rel error
## 1  0.5111111111      0 1.0000000
## 2  0.0144444444      1 0.4888889
## 3  0.0133333333      3 0.4600000
## 4  0.0100000000      4 0.4466667
## 5  0.0055555556      6 0.4266667
## 6  0.0029629630     10 0.4044444
## 7  0.0008888889     13 0.3955556
## 8  0.0004444444     18 0.3911111
## 9  0.0003174603     23 0.3888889
## 10 0.0000000000     30 0.3866667
## 
## Variable importance
## startprice.log       biddable 
##             50             50 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable       < 0.5      to the left,  improve=144.1499, (0 missing)
##       startprice.log < 4.610145 to the right, improve=117.2126, (0 missing)
##   Surrogate splits:
##       startprice.log < 5.030417 to the right, agree=0.751, adj=0.46, (0 split)
## 
## Node number 2: 524 observations,    complexity param=0.002962963
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
##   left son=4 (276 obs) right son=5 (248 obs)
##   Primary splits:
##       startprice.log < 5.527188 to the right, improve=4.393115, (0 missing)
## 
## Node number 3: 450 observations,    complexity param=0.01444444
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (162 obs) right son=7 (288 obs)
##   Primary splits:
##       startprice.log < 4.863673 to the right, improve=38.02778, (0 missing)
## 
## Node number 4: 276 observations,    complexity param=0.0004444444
##   predicted class=N  expected loss=0.1485507  P(node) =0.2833676
##     class counts:   235    41
##    probabilities: 0.851 0.149 
##   left son=8 (119 obs) right son=9 (157 obs)
##   Primary splits:
##       startprice.log < 5.972491 to the right, improve=0.9523844, (0 missing)
## 
## Node number 5: 248 observations,    complexity param=0.002962963
##   predicted class=N  expected loss=0.2782258  P(node) =0.2546201
##     class counts:   179    69
##    probabilities: 0.722 0.278 
##   left son=10 (9 obs) right son=11 (239 obs)
##   Primary splits:
##       startprice.log < 3.365867 to the left,  improve=1.445843, (0 missing)
## 
## Node number 6: 162 observations,    complexity param=0.01444444
##   predicted class=N  expected loss=0.4814815  P(node) =0.1663244
##     class counts:    84    78
##    probabilities: 0.519 0.481 
##   left son=12 (27 obs) right son=13 (135 obs)
##   Primary splits:
##       startprice.log < 6.024113 to the right, improve=3.2, (0 missing)
## 
## Node number 7: 288 observations
##   predicted class=Y  expected loss=0.09027778  P(node) =0.2956879
##     class counts:    26   262
##    probabilities: 0.090 0.910 
## 
## Node number 8: 119 observations
##   predicted class=N  expected loss=0.1008403  P(node) =0.1221766
##     class counts:   107    12
##    probabilities: 0.899 0.101 
## 
## Node number 9: 157 observations,    complexity param=0.0004444444
##   predicted class=N  expected loss=0.1847134  P(node) =0.161191
##     class counts:   128    29
##    probabilities: 0.815 0.185 
##   left son=18 (147 obs) right son=19 (10 obs)
##   Primary splits:
##       startprice.log < 5.940158 to the left,  improve=2.123359, (0 missing)
## 
## Node number 10: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.009240246
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 11: 239 observations,    complexity param=0.002962963
##   predicted class=N  expected loss=0.2887029  P(node) =0.2453799
##     class counts:   170    69
##    probabilities: 0.711 0.289 
##   left son=22 (225 obs) right son=23 (14 obs)
##   Primary splits:
##       startprice.log < 4.248424 to the right, improve=3.730424, (0 missing)
## 
## Node number 12: 27 observations
##   predicted class=N  expected loss=0.2592593  P(node) =0.02772074
##     class counts:    20     7
##    probabilities: 0.741 0.259 
## 
## Node number 13: 135 observations,    complexity param=0.01333333
##   predicted class=Y  expected loss=0.4740741  P(node) =0.1386037
##     class counts:    64    71
##    probabilities: 0.474 0.526 
##   left son=26 (10 obs) right son=27 (125 obs)
##   Primary splits:
##       startprice.log < 5.010535 to the left,  improve=2.294519, (0 missing)
## 
## Node number 18: 147 observations,    complexity param=0.0004444444
##   predicted class=N  expected loss=0.1632653  P(node) =0.150924
##     class counts:   123    24
##    probabilities: 0.837 0.163 
##   left son=36 (39 obs) right son=37 (108 obs)
##   Primary splits:
##       startprice.log < 5.669864 to the left,  improve=1.331356, (0 missing)
## 
## Node number 19: 10 observations
##   predicted class=N  expected loss=0.5  P(node) =0.01026694
##     class counts:     5     5
##    probabilities: 0.500 0.500 
## 
## Node number 22: 225 observations,    complexity param=0.0008888889
##   predicted class=N  expected loss=0.2666667  P(node) =0.2310062
##     class counts:   165    60
##    probabilities: 0.733 0.267 
##   left son=44 (209 obs) right son=45 (16 obs)
##   Primary splits:
##       startprice.log < 5.521381 to the left,  improve=1.005383, (0 missing)
## 
## Node number 23: 14 observations
##   predicted class=Y  expected loss=0.3571429  P(node) =0.01437372
##     class counts:     5     9
##    probabilities: 0.357 0.643 
## 
## Node number 26: 10 observations
##   predicted class=N  expected loss=0.2  P(node) =0.01026694
##     class counts:     8     2
##    probabilities: 0.800 0.200 
## 
## Node number 27: 125 observations,    complexity param=0.01
##   predicted class=Y  expected loss=0.448  P(node) =0.1283368
##     class counts:    56    69
##    probabilities: 0.448 0.552 
##   left son=54 (98 obs) right son=55 (27 obs)
##   Primary splits:
##       startprice.log < 5.176086 to the right, improve=1.585149, (0 missing)
## 
## Node number 36: 39 observations
##   predicted class=N  expected loss=0.05128205  P(node) =0.04004107
##     class counts:    37     2
##    probabilities: 0.949 0.051 
## 
## Node number 37: 108 observations,    complexity param=0.0004444444
##   predicted class=N  expected loss=0.2037037  P(node) =0.110883
##     class counts:    86    22
##    probabilities: 0.796 0.204 
##   left son=74 (67 obs) right son=75 (41 obs)
##   Primary splits:
##       startprice.log < 5.768305 to the right, improve=1.046502, (0 missing)
## 
## Node number 44: 209 observations,    complexity param=0.0008888889
##   predicted class=N  expected loss=0.2535885  P(node) =0.2145791
##     class counts:   156    53
##    probabilities: 0.746 0.254 
##   left son=88 (185 obs) right son=89 (24 obs)
##   Primary splits:
##       startprice.log < 4.499754 to the right, improve=0.799347, (0 missing)
## 
## Node number 45: 16 observations
##   predicted class=N  expected loss=0.4375  P(node) =0.0164271
##     class counts:     9     7
##    probabilities: 0.562 0.438 
## 
## Node number 54: 98 observations,    complexity param=0.01
##   predicted class=Y  expected loss=0.4897959  P(node) =0.100616
##     class counts:    48    50
##    probabilities: 0.490 0.510 
##   left son=108 (67 obs) right son=109 (31 obs)
##   Primary splits:
##       startprice.log < 5.700393 to the left,  improve=2.535682, (0 missing)
## 
## Node number 55: 27 observations
##   predicted class=Y  expected loss=0.2962963  P(node) =0.02772074
##     class counts:     8    19
##    probabilities: 0.296 0.704 
## 
## Node number 74: 67 observations
##   predicted class=N  expected loss=0.1492537  P(node) =0.0687885
##     class counts:    57    10
##    probabilities: 0.851 0.149 
## 
## Node number 75: 41 observations,    complexity param=0.0004444444
##   predicted class=N  expected loss=0.2926829  P(node) =0.04209446
##     class counts:    29    12
##    probabilities: 0.707 0.293 
##   left son=150 (30 obs) right son=151 (11 obs)
##   Primary splits:
##       startprice.log < 5.720131 to the left,  improve=1.921064, (0 missing)
## 
## Node number 88: 185 observations,    complexity param=0.0008888889
##   predicted class=N  expected loss=0.2378378  P(node) =0.1899384
##     class counts:   141    44
##    probabilities: 0.762 0.238 
##   left son=176 (34 obs) right son=177 (151 obs)
##   Primary splits:
##       startprice.log < 4.828114 to the left,  improve=1.864583, (0 missing)
## 
## Node number 89: 24 observations
##   predicted class=N  expected loss=0.375  P(node) =0.02464066
##     class counts:    15     9
##    probabilities: 0.625 0.375 
## 
## Node number 108: 67 observations,    complexity param=0.005555556
##   predicted class=N  expected loss=0.4328358  P(node) =0.0687885
##     class counts:    38    29
##    probabilities: 0.567 0.433 
##   left son=216 (9 obs) right son=217 (58 obs)
##   Primary splits:
##       startprice.log < 5.549046 to the right, improve=0.9223423, (0 missing)
## 
## Node number 109: 31 observations
##   predicted class=Y  expected loss=0.3225806  P(node) =0.03182752
##     class counts:    10    21
##    probabilities: 0.323 0.677 
## 
## Node number 150: 30 observations
##   predicted class=N  expected loss=0.2  P(node) =0.03080082
##     class counts:    24     6
##    probabilities: 0.800 0.200 
## 
## Node number 151: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.01129363
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 176: 34 observations
##   predicted class=N  expected loss=0.08823529  P(node) =0.0349076
##     class counts:    31     3
##    probabilities: 0.912 0.088 
## 
## Node number 177: 151 observations,    complexity param=0.0008888889
##   predicted class=N  expected loss=0.2715232  P(node) =0.1550308
##     class counts:   110    41
##    probabilities: 0.728 0.272 
##   left son=354 (130 obs) right son=355 (21 obs)
##   Primary splits:
##       startprice.log < 5.010602 to the right, improve=0.5841836, (0 missing)
## 
## Node number 216: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.009240246
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 217: 58 observations,    complexity param=0.005555556
##   predicted class=N  expected loss=0.4655172  P(node) =0.05954825
##     class counts:    31    27
##    probabilities: 0.534 0.466 
##   left son=434 (45 obs) right son=435 (13 obs)
##   Primary splits:
##       startprice.log < 5.501177 to the left,  improve=1.723607, (0 missing)
## 
## Node number 354: 130 observations,    complexity param=0.0003174603
##   predicted class=N  expected loss=0.2538462  P(node) =0.1334702
##     class counts:    97    33
##    probabilities: 0.746 0.254 
##   left son=708 (14 obs) right son=709 (116 obs)
##   Primary splits:
##       startprice.log < 5.074986 to the left,  improve=0.3865479, (0 missing)
## 
## Node number 355: 21 observations,    complexity param=0.0008888889
##   predicted class=N  expected loss=0.3809524  P(node) =0.02156057
##     class counts:    13     8
##    probabilities: 0.619 0.381 
##   left son=710 (13 obs) right son=711 (8 obs)
##   Primary splits:
##       startprice.log < 4.961339 to the left,  improve=1.539377, (0 missing)
## 
## Node number 434: 45 observations,    complexity param=0.005555556
##   predicted class=N  expected loss=0.4  P(node) =0.04620123
##     class counts:    27    18
##    probabilities: 0.600 0.400 
##   left son=868 (18 obs) right son=869 (27 obs)
##   Primary splits:
##       startprice.log < 5.322712 to the right, improve=1.896296, (0 missing)
## 
## Node number 435: 13 observations
##   predicted class=Y  expected loss=0.3076923  P(node) =0.01334702
##     class counts:     4     9
##    probabilities: 0.308 0.692 
## 
## Node number 708: 14 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.01437372
##     class counts:    12     2
##    probabilities: 0.857 0.143 
## 
## Node number 709: 116 observations,    complexity param=0.0003174603
##   predicted class=N  expected loss=0.2672414  P(node) =0.1190965
##     class counts:    85    31
##    probabilities: 0.733 0.267 
##   left son=1418 (27 obs) right son=1419 (89 obs)
##   Primary splits:
##       startprice.log < 5.393605 to the right, improve=0.4738976, (0 missing)
## 
## Node number 710: 13 observations
##   predicted class=N  expected loss=0.2307692  P(node) =0.01334702
##     class counts:    10     3
##    probabilities: 0.769 0.231 
## 
## Node number 711: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.008213552
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 868: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.01848049
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 869: 27 observations,    complexity param=0.005555556
##   predicted class=Y  expected loss=0.4814815  P(node) =0.02772074
##     class counts:    13    14
##    probabilities: 0.481 0.519 
##   left son=1738 (8 obs) right son=1739 (19 obs)
##   Primary splits:
##       startprice.log < 5.206629 to the left,  improve=1.639376, (0 missing)
## 
## Node number 1418: 27 observations
##   predicted class=N  expected loss=0.1851852  P(node) =0.02772074
##     class counts:    22     5
##    probabilities: 0.815 0.185 
## 
## Node number 1419: 89 observations,    complexity param=0.0003174603
##   predicted class=N  expected loss=0.2921348  P(node) =0.09137577
##     class counts:    63    26
##    probabilities: 0.708 0.292 
##   left son=2838 (80 obs) right son=2839 (9 obs)
##   Primary splits:
##       startprice.log < 5.347084 to the left,  improve=0.4645443, (0 missing)
## 
## Node number 1738: 8 observations
##   predicted class=N  expected loss=0.25  P(node) =0.008213552
##     class counts:     6     2
##    probabilities: 0.750 0.250 
## 
## Node number 1739: 19 observations
##   predicted class=Y  expected loss=0.3684211  P(node) =0.01950719
##     class counts:     7    12
##    probabilities: 0.368 0.632 
## 
## Node number 2838: 80 observations,    complexity param=0.0003174603
##   predicted class=N  expected loss=0.275  P(node) =0.08213552
##     class counts:    58    22
##    probabilities: 0.725 0.275 
##   left son=5676 (8 obs) right son=5677 (72 obs)
##   Primary splits:
##       startprice.log < 5.339912 to the right, improve=0.4, (0 missing)
## 
## Node number 2839: 9 observations
##   predicted class=N  expected loss=0.4444444  P(node) =0.009240246
##     class counts:     5     4
##    probabilities: 0.556 0.444 
## 
## Node number 5676: 8 observations
##   predicted class=N  expected loss=0.125  P(node) =0.008213552
##     class counts:     7     1
##    probabilities: 0.875 0.125 
## 
## Node number 5677: 72 observations,    complexity param=0.0003174603
##   predicted class=N  expected loss=0.2916667  P(node) =0.07392197
##     class counts:    51    21
##    probabilities: 0.708 0.292 
##   left son=11354 (65 obs) right son=11355 (7 obs)
##   Primary splits:
##       startprice.log < 5.09056  to the right, improve=0.2906593, (0 missing)
## 
## Node number 11354: 65 observations,    complexity param=0.0003174603
##   predicted class=N  expected loss=0.2769231  P(node) =0.06673511
##     class counts:    47    18
##    probabilities: 0.723 0.277 
##   left son=22708 (14 obs) right son=22709 (51 obs)
##   Primary splits:
##       startprice.log < 5.192818 to the left,  improve=0.6414135, (0 missing)
## 
## Node number 11355: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.007186858
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 22708: 14 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.01437372
##     class counts:    12     2
##    probabilities: 0.857 0.143 
## 
## Node number 22709: 51 observations,    complexity param=0.0003174603
##   predicted class=N  expected loss=0.3137255  P(node) =0.0523614
##     class counts:    35    16
##    probabilities: 0.686 0.314 
##   left son=45418 (42 obs) right son=45419 (9 obs)
##   Primary splits:
##       startprice.log < 5.227067 to the right, improve=1.278245, (0 missing)
## 
## Node number 45418: 42 observations
##   predicted class=N  expected loss=0.2619048  P(node) =0.04312115
##     class counts:    31    11
##    probabilities: 0.738 0.262 
## 
## Node number 45419: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.009240246
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##     1) root 974 450 N (0.53798768 0.46201232)  
##       2) biddable< 0.5 524 110 N (0.79007634 0.20992366)  
##         4) startprice.log>=5.527188 276  41 N (0.85144928 0.14855072)  
##           8) startprice.log>=5.972491 119  12 N (0.89915966 0.10084034) *
##           9) startprice.log< 5.972491 157  29 N (0.81528662 0.18471338)  
##            18) startprice.log< 5.940158 147  24 N (0.83673469 0.16326531)  
##              36) startprice.log< 5.669864 39   2 N (0.94871795 0.05128205) *
##              37) startprice.log>=5.669864 108  22 N (0.79629630 0.20370370)  
##                74) startprice.log>=5.768305 67  10 N (0.85074627 0.14925373) *
##                75) startprice.log< 5.768305 41  12 N (0.70731707 0.29268293)  
##                 150) startprice.log< 5.720131 30   6 N (0.80000000 0.20000000) *
##                 151) startprice.log>=5.720131 11   5 Y (0.45454545 0.54545455) *
##            19) startprice.log>=5.940158 10   5 N (0.50000000 0.50000000) *
##         5) startprice.log< 5.527188 248  69 N (0.72177419 0.27822581)  
##          10) startprice.log< 3.365867 9   0 N (1.00000000 0.00000000) *
##          11) startprice.log>=3.365867 239  69 N (0.71129707 0.28870293)  
##            22) startprice.log>=4.248424 225  60 N (0.73333333 0.26666667)  
##              44) startprice.log< 5.521381 209  53 N (0.74641148 0.25358852)  
##                88) startprice.log>=4.499754 185  44 N (0.76216216 0.23783784)  
##                 176) startprice.log< 4.828114 34   3 N (0.91176471 0.08823529) *
##                 177) startprice.log>=4.828114 151  41 N (0.72847682 0.27152318)  
##                   354) startprice.log>=5.010602 130  33 N (0.74615385 0.25384615)  
##                     708) startprice.log< 5.074986 14   2 N (0.85714286 0.14285714) *
##                     709) startprice.log>=5.074986 116  31 N (0.73275862 0.26724138)  
##                      1418) startprice.log>=5.393605 27   5 N (0.81481481 0.18518519) *
##                      1419) startprice.log< 5.393605 89  26 N (0.70786517 0.29213483)  
##                        2838) startprice.log< 5.347084 80  22 N (0.72500000 0.27500000)  
##                          5676) startprice.log>=5.339912 8   1 N (0.87500000 0.12500000) *
##                          5677) startprice.log< 5.339912 72  21 N (0.70833333 0.29166667)  
##                           11354) startprice.log>=5.09056 65  18 N (0.72307692 0.27692308)  
##                             22708) startprice.log< 5.192818 14   2 N (0.85714286 0.14285714) *
##                             22709) startprice.log>=5.192818 51  16 N (0.68627451 0.31372549)  
##                               45418) startprice.log>=5.227067 42  11 N (0.73809524 0.26190476) *
##                               45419) startprice.log< 5.227067 9   4 Y (0.44444444 0.55555556) *
##                           11355) startprice.log< 5.09056 7   3 N (0.57142857 0.42857143) *
##                        2839) startprice.log>=5.347084 9   4 N (0.55555556 0.44444444) *
##                   355) startprice.log< 5.010602 21   8 N (0.61904762 0.38095238)  
##                     710) startprice.log< 4.961339 13   3 N (0.76923077 0.23076923) *
##                     711) startprice.log>=4.961339 8   3 Y (0.37500000 0.62500000) *
##                89) startprice.log< 4.499754 24   9 N (0.62500000 0.37500000) *
##              45) startprice.log>=5.521381 16   7 N (0.56250000 0.43750000) *
##            23) startprice.log< 4.248424 14   5 Y (0.35714286 0.64285714) *
##       3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##         6) startprice.log>=4.863673 162  78 N (0.51851852 0.48148148)  
##          12) startprice.log>=6.024113 27   7 N (0.74074074 0.25925926) *
##          13) startprice.log< 6.024113 135  64 Y (0.47407407 0.52592593)  
##            26) startprice.log< 5.010535 10   2 N (0.80000000 0.20000000) *
##            27) startprice.log>=5.010535 125  56 Y (0.44800000 0.55200000)  
##              54) startprice.log>=5.176086 98  48 Y (0.48979592 0.51020408)  
##               108) startprice.log< 5.700393 67  29 N (0.56716418 0.43283582)  
##                 216) startprice.log>=5.549046 9   2 N (0.77777778 0.22222222) *
##                 217) startprice.log< 5.549046 58  27 N (0.53448276 0.46551724)  
##                   434) startprice.log< 5.501177 45  18 N (0.60000000 0.40000000)  
##                     868) startprice.log>=5.322712 18   4 N (0.77777778 0.22222222) *
##                     869) startprice.log< 5.322712 27  13 Y (0.48148148 0.51851852)  
##                      1738) startprice.log< 5.206629 8   2 N (0.75000000 0.25000000) *
##                      1739) startprice.log>=5.206629 19   7 Y (0.36842105 0.63157895) *
##                   435) startprice.log>=5.501177 13   4 Y (0.30769231 0.69230769) *
##               109) startprice.log>=5.700393 31  10 Y (0.32258065 0.67741935) *
##              55) startprice.log< 5.176086 27   8 Y (0.29629630 0.70370370) *
##         7) startprice.log< 4.863673 288  26 Y (0.09027778 0.90972222) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6631893
## 3        0.2 0.7657573
## 4        0.3 0.8034188
## 5        0.4 0.8048246
## 6        0.5 0.8022727
## 7        0.6 0.7929412
## 8        0.7 0.7346405
## 9        0.8 0.7100271
## 10       0.9 0.7100271
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           429
## 2         Y                                            83
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            95
## 2                                           367
##          Prediction
## Reference   N   Y
##         N 429  95
##         Y  83 367
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.172485e-01   6.330658e-01   7.915181e-01   8.410439e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   6.422756e-75   4.096641e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6457990
## 3        0.2 0.7080745
## 4        0.3 0.7142857
## 5        0.4 0.7191539
## 6        0.5 0.7314702
## 7        0.6 0.7439490
## 8        0.7 0.7070707
## 9        0.8 0.6818874
## 10       0.9 0.6818874
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1         N                                           392
## 2         Y                                           118
##   sold.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                            83
## 2                                           292
##          Prediction
## Reference   N   Y
##         N 392  83
##         Y 118 292
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.728814e-01   5.406159e-01   7.438293e-01   8.001036e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   3.908036e-48   1.647699e-02 
##                    model_id model_method                    feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart biddable, startprice.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.516                  0.01
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8781425                    0.4       0.8048246        0.8172485
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7915181             0.8410439     0.6330658   0.8075096
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6        0.743949        0.7728814
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7438293             0.8001036     0.5406159
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
## [1] "    indep_vars: biddable, startprice.log"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0133 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-11.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##           CP nsplit rel error
## 1 0.51111111      0 1.0000000
## 2 0.01444444      1 0.4888889
## 3 0.01333333      3 0.4600000
## 
## Variable importance
##       biddable startprice.log 
##             57             43 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable       < 0.5      to the left,  improve=144.1499, (0 missing)
##       startprice.log < 4.610145 to the right, improve=117.2126, (0 missing)
##   Surrogate splits:
##       startprice.log < 5.030417 to the right, agree=0.751, adj=0.46, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.01444444
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (162 obs) right son=7 (288 obs)
##   Primary splits:
##       startprice.log < 4.863673 to the right, improve=38.02778, (0 missing)
## 
## Node number 6: 162 observations,    complexity param=0.01444444
##   predicted class=N  expected loss=0.4814815  P(node) =0.1663244
##     class counts:    84    78
##    probabilities: 0.519 0.481 
##   left son=12 (27 obs) right son=13 (135 obs)
##   Primary splits:
##       startprice.log < 6.024113 to the right, improve=3.2, (0 missing)
## 
## Node number 7: 288 observations
##   predicted class=Y  expected loss=0.09027778  P(node) =0.2956879
##     class counts:    26   262
##    probabilities: 0.090 0.910 
## 
## Node number 12: 27 observations
##   predicted class=N  expected loss=0.2592593  P(node) =0.02772074
##     class counts:    20     7
##    probabilities: 0.741 0.259 
## 
## Node number 13: 135 observations
##   predicted class=Y  expected loss=0.4740741  P(node) =0.1386037
##     class counts:    64    71
##    probabilities: 0.474 0.526 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 974 450 N (0.53798768 0.46201232)  
##    2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##    3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##      6) startprice.log>=4.863673 162  78 N (0.51851852 0.48148148)  
##       12) startprice.log>=6.024113 27   7 N (0.74074074 0.25925926) *
##       13) startprice.log< 6.024113 135  64 Y (0.47407407 0.52592593) *
##      7) startprice.log< 4.863673 288  26 Y (0.09027778 0.90972222) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-13.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7628866
## 5        0.4 0.7628866
## 6        0.5 0.7628866
## 7        0.6 0.7100271
## 8        0.7 0.7100271
## 9        0.8 0.7100271
## 10       0.9 0.7100271
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-14.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 434
## 2         Y                                 117
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  90
## 2                                 333
##          Prediction
## Reference   N   Y
##         N 434  90
##         Y 117 333
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.874743e-01   5.706620e-01   7.604190e-01   8.127790e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.596136e-59   7.074280e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7582697
## 5        0.4 0.7582697
## 6        0.5 0.7582697
## 7        0.6 0.6818874
## 8        0.7 0.6818874
## 9        0.8 0.6818874
## 10       0.9 0.6818874
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-16.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.rpart.N
## 1         N                                 397
## 2         Y                                 112
##   sold.fctr.predict.Max.cor.Y.rpart.Y
## 1                                  78
## 2                                 298
##          Prediction
## Reference   N   Y
##         N 397  78
##         Y 112 298
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.853107e-01   5.658292e-01   7.567658e-01   8.119416e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.847896e-53   1.666249e-02 
##          model_id model_method                    feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart biddable, startprice.log               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.046                 0.012   0.8172434
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5       0.7628866        0.7731117
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.760419              0.812779      0.537726   0.8080205
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7582697        0.7853107
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7567658             0.8119416     0.5658292
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009511979       0.0250436
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
## [1] "    indep_vars: biddable, startprice.log"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-17.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-18.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-19.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8846  -0.7020  -0.5158   0.7747   2.1091  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     2.36119    0.47914   4.928 8.31e-07 ***
## biddable        1.77676    0.16636  10.681  < 2e-16 ***
## startprice.log -0.68628    0.08788  -7.809 5.75e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.6  on 973  degrees of freedom
## Residual deviance:  937.2  on 971  degrees of freedom
## AIC: 943.2
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6338028
## 3        0.2 0.7118644
## 4        0.3 0.7466391
## 5        0.4 0.7538126
## 6        0.5 0.7570621
## 7        0.6 0.7274939
## 8        0.7 0.6949384
## 9        0.8 0.5460317
## 10       0.9 0.3907638
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-22.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               424
## 2         Y                               115
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                               100
## 2                               335
##          Prediction
## Reference   N   Y
##         N 424 100
##         Y 115 335
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.792608e-01   5.549108e-01   7.518769e-01   8.049446e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.140215e-55   3.396829e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6361521
## 3        0.2 0.7043062
## 4        0.3 0.7508691
## 5        0.4 0.7604938
## 6        0.5 0.7597484
## 7        0.6 0.7415426
## 8        0.7 0.6798780
## 9        0.8 0.5150977
## 10       0.9 0.3913894
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-24.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Max.cor.Y.glm.N
## 1         N                               383
## 2         Y                               102
##   sold.fctr.predict.Max.cor.Y.glm.Y
## 1                                92
## 2                               308
##          Prediction
## Reference   N   Y
##         N 383  92
##         Y 102 308
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.807910e-01   5.584673e-01   7.520575e-01   8.076410e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.749595e-51   5.181742e-01 
##        model_id model_method                    feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm biddable, startprice.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.041                 0.015    0.840229
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5       0.7570621        0.7803039
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7518769             0.8049446     0.5567446   0.8402773
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7604938         0.780791
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7520575              0.807641     0.5584673    943.1953
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0141875      0.02978891
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
## [1] "    indep_vars: biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-25.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-26.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-27.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.9796  -0.7021  -0.5105   0.7397   2.1206  
## 
## Coefficients:
##                                    Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         2.48002    0.48772   5.085 3.68e-07
## biddable                            2.46193    1.37600   1.789  0.07358
## startprice.log                     -0.70868    0.08957  -7.912 2.54e-15
## `biddable:D.terms.n.post.stop`      0.13854    0.27844   0.498  0.61878
## `biddable:D.TfIdf.sum.post.stem`    0.08666    0.20408   0.425  0.67111
## `biddable:D.ratio.nstopwrds.nwrds` -0.40778    1.36637  -0.298  0.76537
## `biddable:D.npnct06.log`           -0.45102    0.81335  -0.555  0.57922
## `biddable:D.nchrs.log`              0.03157    0.82551   0.038  0.96950
## `biddable:D.nwrds.unq.log`         -1.06127    2.74119  -0.387  0.69864
## `biddable:cellular.fctr1`          -0.11424    0.29015  -0.394  0.69379
## `biddable:cellular.fctrUnknown`    -1.15217    0.36332  -3.171  0.00152
##                                       
## (Intercept)                        ***
## biddable                           .  
## startprice.log                     ***
## `biddable:D.terms.n.post.stop`        
## `biddable:D.TfIdf.sum.post.stem`      
## `biddable:D.ratio.nstopwrds.nwrds`    
## `biddable:D.npnct06.log`              
## `biddable:D.nchrs.log`                
## `biddable:D.nwrds.unq.log`            
## `biddable:cellular.fctr1`             
## `biddable:cellular.fctrUnknown`    ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  924.96  on 963  degrees of freedom
## AIC: 946.96
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6346968
## 3        0.2 0.7131367
## 4        0.3 0.7484407
## 5        0.4 0.7571744
## 6        0.5 0.7525656
## 7        0.6 0.7457213
## 8        0.7 0.7063599
## 9        0.8 0.5597484
## 10       0.9 0.4112478
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-30.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         411
## 2         Y                                         107
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                         113
## 2                                         343
##          Prediction
## Reference   N   Y
##         N 411 113
##         Y 107 343
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.741273e-01   5.460593e-01   7.465456e-01   8.000405e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   7.516456e-53   7.360416e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6371406
## 3        0.2 0.7063340
## 4        0.3 0.7508691
## 5        0.4 0.7636816
## 6        0.5 0.7496823
## 7        0.6 0.7392473
## 8        0.7 0.6943620
## 9        0.8 0.5316901
## 10       0.9 0.3906250
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-32.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         388
## 2         Y                                         103
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                          87
## 2                                         307
##          Prediction
## Reference   N   Y
##         N 388  87
##         Y 103 307
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.853107e-01   5.671369e-01   7.567658e-01   8.119416e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.847896e-53   2.765005e-01 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                      feats
## 1 biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.016                 0.017
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8427735                    0.4       0.7571744        0.7669516
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7465456             0.8000405     0.5288692   0.8396662
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7636816        0.7853107
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7567658             0.8119416     0.5671369    946.9576
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01672188      0.03716387
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
## [1] "    indep_vars: biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-33.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-34.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-35.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4306  -0.6785  -0.1977   0.6285   2.4349  
## 
## Coefficients: (7 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                    1.165e+01  5.567e+00
## biddable                                       1.750e+00  2.007e-01
## D.npnct15.log                                  1.980e+00  8.570e-01
## D.npnct03.log                                  6.237e-01  1.618e+00
## D.terms.n.stem.stop.Ratio                     -7.485e+00  4.922e+00
## D.ratio.sum.TfIdf.nwrds                       -4.495e-02  1.575e-01
## .rnorm                                         5.748e-03  9.212e-02
## D.npnct01.log                                 -6.052e-03  5.577e-01
## D.TfIdf.sum.stem.stop.Ratio                   -8.525e-01  2.962e+00
## storage.fctr16                                 1.418e-01  4.515e-01
## storage.fctr32                                 2.269e-02  4.823e-01
## storage.fctr64                                 6.899e-01  4.693e-01
## storage.fctrUnknown                            4.702e-02  6.425e-01
## D.npnct11.log                                 -1.171e-01  3.537e-01
## D.npnct10.log                                 -2.335e+01  1.380e+03
## D.npnct08.log                                  6.290e-01  6.497e-01
## `prdline.my.fctriPad 1`                        8.839e-01  5.727e-01
## `prdline.my.fctriPad 2`                        8.771e-01  5.870e-01
## `prdline.my.fctriPad 3+`                       1.374e+00  5.666e-01
## prdline.my.fctriPadAir                         2.023e+00  5.803e-01
## prdline.my.fctriPadmini                        1.154e+00  5.496e-01
## `prdline.my.fctriPadmini 2+`                   1.584e+00  5.985e-01
## color.fctrBlack                                1.339e-01  2.535e-01
## color.fctrGold                                -3.726e-01  5.008e-01
## `color.fctrSpace Gray`                        -1.843e-01  3.261e-01
## color.fctrWhite                               -9.023e-02  2.504e-01
## D.npnct06.log                                 -2.061e+00  1.137e+00
## D.npnct28.log                                 -1.256e+00  1.737e+03
## D.npnct12.log                                  8.174e-02  7.309e-01
## D.npnct09.log                                 -9.840e+00  8.153e+02
## D.ndgts.log                                    6.201e-01  4.488e-01
## cellular.fctr1                                 1.120e-01  2.158e-01
## cellular.fctrUnknown                          -4.067e-01  4.663e-01
## D.npnct14.log                                 -1.541e+00  1.048e+00
## D.terms.n.post.stop                           -9.710e-02  4.784e-02
## D.npnct05.log                                 -4.041e+00  1.821e+00
## `condition.fctrFor parts or not working`      -2.718e-01  3.903e-01
## `condition.fctrManufacturer refurbished`       4.079e-01  5.965e-01
## condition.fctrNew                             -3.123e-01  3.003e-01
## `condition.fctrNew other (see details)`        2.761e-01  4.496e-01
## `condition.fctrSeller refurbished`            -9.544e-01  4.794e-01
## idseq.my                                      -4.620e-04  2.038e-04
## startprice.log                                -1.019e+00  1.453e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.773e+00  7.502e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.393e+00  8.122e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       8.716e-01  7.359e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -7.366e-01  7.309e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -6.105e-01  7.082e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.563e+00  7.526e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  4.678e-01  8.752e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`      1.925e-01  1.054e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`       6.972e-01  9.122e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.496e+01  7.766e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -2.114e-01  6.640e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      6.829e-01  9.248e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     1.148e+00  8.419e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  4.507e-01  9.262e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.553e+00  9.315e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.176e+00  1.296e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      1.090e+00  7.734e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -1.208e-01  8.386e-01
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -3.443e-01  9.826e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.823e+00  1.426e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -4.236e-01  1.372e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                     2.092 0.036439 *  
## biddable                                        8.722  < 2e-16 ***
## D.npnct15.log                                   2.310 0.020873 *  
## D.npnct03.log                                   0.385 0.699953    
## D.terms.n.stem.stop.Ratio                      -1.521 0.128346    
## D.ratio.sum.TfIdf.nwrds                        -0.285 0.775356    
## .rnorm                                          0.062 0.950253    
## D.npnct01.log                                  -0.011 0.991340    
## D.TfIdf.sum.stem.stop.Ratio                    -0.288 0.773513    
## storage.fctr16                                  0.314 0.753526    
## storage.fctr32                                  0.047 0.962474    
## storage.fctr64                                  1.470 0.141535    
## storage.fctrUnknown                             0.073 0.941666    
## D.npnct11.log                                  -0.331 0.740595    
## D.npnct10.log                                  -0.017 0.986500    
## D.npnct08.log                                   0.968 0.332969    
## `prdline.my.fctriPad 1`                         1.543 0.122768    
## `prdline.my.fctriPad 2`                         1.494 0.135104    
## `prdline.my.fctriPad 3+`                        2.426 0.015280 *  
## prdline.my.fctriPadAir                          3.487 0.000489 ***
## prdline.my.fctriPadmini                         2.100 0.035736 *  
## `prdline.my.fctriPadmini 2+`                    2.647 0.008117 ** 
## color.fctrBlack                                 0.528 0.597302    
## color.fctrGold                                 -0.744 0.456943    
## `color.fctrSpace Gray`                         -0.565 0.572018    
## color.fctrWhite                                -0.360 0.718614    
## D.npnct06.log                                  -1.813 0.069867 .  
## D.npnct28.log                                  -0.001 0.999423    
## D.npnct12.log                                   0.112 0.910958    
## D.npnct09.log                                  -0.012 0.990371    
## D.ndgts.log                                     1.382 0.167029    
## cellular.fctr1                                  0.519 0.603544    
## cellular.fctrUnknown                           -0.872 0.383043    
## D.npnct14.log                                  -1.471 0.141396    
## D.terms.n.post.stop                            -2.030 0.042384 *  
## D.npnct05.log                                  -2.219 0.026493 *  
## `condition.fctrFor parts or not working`       -0.696 0.486224    
## `condition.fctrManufacturer refurbished`        0.684 0.494111    
## condition.fctrNew                              -1.040 0.298478    
## `condition.fctrNew other (see details)`         0.614 0.539121    
## `condition.fctrSeller refurbished`             -1.991 0.046498 *  
## idseq.my                                       -2.267 0.023384 *  
## startprice.log                                 -7.012 2.34e-12 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       3.696 0.000219 ***
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.716 0.086234 .  
## `prdline.my.fctriPad 2:.clusterid.fctr2`        1.185 0.236214    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.008 0.313567    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.862 0.388655    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      2.077 0.037831 *  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.535 0.592981    
## `prdline.my.fctrUnknown:.clusterid.fctr3`       0.183 0.855148    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.764 0.444731    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.019 0.984633    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.318 0.750238    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.738 0.460266    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.364 0.172596    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.487 0.626566    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.667 0.095570 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.908 0.364091    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.409 0.158701    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.144 0.885448    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.350 0.726060    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.682 0.007325 ** 
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.309 0.757541    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  807.71  on 910  degrees of freedom
## AIC: 935.71
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-36.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6938776
## 3        0.2 0.7457323
## 4        0.3 0.7829615
## 5        0.4 0.7863436
## 6        0.5 0.7962963
## 7        0.6 0.7897934
## 8        0.7 0.7532468
## 9        0.8 0.6550725
## 10       0.9 0.4966887
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               454
## 2         Y                               106
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                70
## 2                               344
##          Prediction
## Reference   N   Y
##         N 454  70
##         Y 106 344
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.193018e-01   6.344405e-01   7.936712e-01   8.429848e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   4.293519e-76   8.334145e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-38.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6649215
## 3        0.2 0.7113821
## 4        0.3 0.7394767
## 5        0.4 0.7481663
## 6        0.5 0.7595908
## 7        0.6 0.7486486
## 8        0.7 0.7249284
## 9        0.8 0.6391097
## 10       0.9 0.4836364
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               400
## 2         Y                               113
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                75
## 2                               297
##          Prediction
## Reference   N   Y
##         N 400  75
##         Y 113 297
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.875706e-01   5.701108e-01   7.591217e-01   8.140900e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.824793e-54   6.965225e-03 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.434                 0.199
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8858227                    0.5       0.7962963        0.7566825
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7936712             0.8429848     0.5097158   0.8280616
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7595908        0.7875706
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7591217               0.81409     0.5701108    935.7122
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.008872342      0.02173136
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn    end elapsed
## 10 fit.models          7          0  96.689 123.33  26.641
## 11 fit.models          7          1 123.330     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor     bgn end elapsed
## 1 fit.models_1_bgn          1          0 127.706  NA      NA
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
        interact_vars_vctr <- c("startprice.log", "biddable", "idseq.my")
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
## 1 fit.models_1_bgn          1          0 127.706 127.717   0.011
## 2 fit.models_1_glm          2          0 127.718      NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-1.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-2.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.5482  -0.6591  -0.1605   0.5979   2.4380  
## 
## Coefficients: (10 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                   -1.586e+03  4.174e+03
## biddable                                       1.777e+00  2.067e-01
## D.ratio.nstopwrds.nwrds                       -1.704e+01  7.363e+00
## D.npnct15.log                                  1.380e+00  9.560e-01
## D.npnct03.log                                  1.170e+00  1.985e+00
## D.terms.n.stem.stop.Ratio                      1.613e+03  4.174e+03
## D.ratio.sum.TfIdf.nwrds                       -9.227e-02  6.818e-01
## .rnorm                                         7.826e-03  9.426e-02
## D.npnct01.log                                  6.120e-02  6.460e-01
## D.TfIdf.sum.stem.stop.Ratio                   -6.193e+00  1.784e+01
## storage.fctr16                                 1.091e-01  4.599e-01
## storage.fctr32                                -3.668e-02  4.950e-01
## storage.fctr64                                 7.012e-01  4.794e-01
## storage.fctrUnknown                           -1.147e-02  6.508e-01
## D.npnct11.log                                 -7.162e-02  4.024e-01
## D.npnct10.log                                 -2.504e+01  1.253e+03
## D.TfIdf.sum.post.stop                         -6.818e-01  2.651e+00
## D.TfIdf.sum.post.stem                          9.407e-01  2.758e+00
## D.sum.TfIdf                                           NA         NA
## D.npnct13.log                                  9.777e-02  4.193e-01
## D.npnct08.log                                  9.173e-01  7.304e-01
## `prdline.my.fctriPad 1`                        9.516e-01  5.821e-01
## `prdline.my.fctriPad 2`                        8.356e-01  5.958e-01
## `prdline.my.fctriPad 3+`                       1.305e+00  5.739e-01
## prdline.my.fctriPadAir                         2.041e+00  5.886e-01
## prdline.my.fctriPadmini                        1.162e+00  5.592e-01
## `prdline.my.fctriPadmini 2+`                   1.590e+00  6.055e-01
## color.fctrBlack                                1.711e-01  2.613e-01
## color.fctrGold                                -4.189e-01  5.378e-01
## `color.fctrSpace Gray`                        -1.179e-01  3.358e-01
## color.fctrWhite                                4.443e-03  2.612e-01
## D.npnct16.log                                  2.398e+00  1.894e+00
## D.nstopwrds.log                                4.167e+00  2.126e+00
## D.npnct24.log                                 -3.744e+00  6.165e+00
## D.npnct06.log                                 -4.899e+00  2.301e+00
## D.npnct28.log                                 -2.342e+00  1.740e+03
## D.nuppr.log                                    2.183e+00  3.434e+00
## D.nchrs.log                                   -4.354e+00  4.132e+00
## D.npnct12.log                                  7.453e-01  8.175e-01
## D.nwrds.log                                    1.579e+00  3.338e+00
## D.npnct09.log                                 -8.852e+00  8.172e+02
## D.ndgts.log                                    3.201e-01  5.764e-01
## D.terms.n.post.stop.log                        1.835e+03  4.656e+03
## D.nwrds.unq.log                               -1.840e+03  4.657e+03
## D.terms.n.post.stem.log                               NA         NA
## `carrier.fctrAT&T`                            -3.653e-02  6.968e-01
## carrier.fctrOther                              1.571e+01  1.600e+03
## carrier.fctrSprint                             7.199e-01  9.340e-01
## `carrier.fctrT-Mobile`                        -5.417e-01  1.105e+00
## carrier.fctrUnknown                           -3.729e-01  4.814e-01
## carrier.fctrVerizon                            2.783e-01  7.495e-01
## cellular.fctr1                                 1.208e-01  6.334e-01
## cellular.fctrUnknown                                  NA         NA
## D.npnct14.log                                 -2.302e+00  1.306e+00
## D.terms.n.post.stem                            1.365e+01  2.664e+01
## D.terms.n.post.stop                           -1.370e+01  2.660e+01
## D.npnct05.log                                 -3.531e+00  1.979e+00
## `condition.fctrFor parts or not working`      -3.814e-01  4.080e-01
## `condition.fctrManufacturer refurbished`       3.945e-01  6.087e-01
## condition.fctrNew                             -2.514e-01  3.104e-01
## `condition.fctrNew other (see details)`        1.480e-01  4.746e-01
## `condition.fctrSeller refurbished`            -9.603e-01  4.957e-01
## idseq.my                                      -4.826e-04  2.081e-04
## startprice.log                                -1.044e+00  1.464e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.524e+00  8.155e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.093e+00  8.667e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       7.934e-01  7.974e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.386e+00  8.947e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -6.394e-01  7.633e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.211e+00  7.968e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  3.503e-01  9.416e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`      5.928e-02  1.165e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`       3.413e-01  9.633e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.489e+01  7.462e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -4.536e-02  6.928e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      1.410e-01  1.070e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`     7.982e-01  9.315e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  1.050e-01  1.005e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.693e+00  9.835e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.336e+00  1.471e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      1.156e+00  8.760e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -4.681e-01  1.045e+00
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -7.242e-01  1.026e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.437e+00  1.459e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -7.510e-01  1.434e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                    -0.380 0.703942    
## biddable                                        8.594  < 2e-16 ***
## D.ratio.nstopwrds.nwrds                        -2.314 0.020678 *  
## D.npnct15.log                                   1.444 0.148748    
## D.npnct03.log                                   0.590 0.555405    
## D.terms.n.stem.stop.Ratio                       0.386 0.699236    
## D.ratio.sum.TfIdf.nwrds                        -0.135 0.892351    
## .rnorm                                          0.083 0.933835    
## D.npnct01.log                                   0.095 0.924519    
## D.TfIdf.sum.stem.stop.Ratio                    -0.347 0.728417    
## storage.fctr16                                  0.237 0.812466    
## storage.fctr32                                 -0.074 0.940927    
## storage.fctr64                                  1.463 0.143583    
## storage.fctrUnknown                            -0.018 0.985944    
## D.npnct11.log                                  -0.178 0.858735    
## D.npnct10.log                                  -0.020 0.984062    
## D.TfIdf.sum.post.stop                          -0.257 0.797057    
## D.TfIdf.sum.post.stem                           0.341 0.733023    
## D.sum.TfIdf                                        NA       NA    
## D.npnct13.log                                   0.233 0.815626    
## D.npnct08.log                                   1.256 0.209136    
## `prdline.my.fctriPad 1`                         1.635 0.102069    
## `prdline.my.fctriPad 2`                         1.403 0.160726    
## `prdline.my.fctriPad 3+`                        2.274 0.022952 *  
## prdline.my.fctriPadAir                          3.467 0.000526 ***
## prdline.my.fctriPadmini                         2.078 0.037666 *  
## `prdline.my.fctriPadmini 2+`                    2.625 0.008659 ** 
## color.fctrBlack                                 0.655 0.512510    
## color.fctrGold                                 -0.779 0.436026    
## `color.fctrSpace Gray`                         -0.351 0.725491    
## color.fctrWhite                                 0.017 0.986430    
## D.npnct16.log                                   1.266 0.205422    
## D.nstopwrds.log                                 1.960 0.049980 *  
## D.npnct24.log                                  -0.607 0.543689    
## D.npnct06.log                                  -2.130 0.033196 *  
## D.npnct28.log                                  -0.001 0.998926    
## D.nuppr.log                                     0.636 0.525015    
## D.nchrs.log                                    -1.054 0.292087    
## D.npnct12.log                                   0.912 0.361936    
## D.nwrds.log                                     0.473 0.636169    
## D.npnct09.log                                  -0.011 0.991358    
## D.ndgts.log                                     0.555 0.578680    
## D.terms.n.post.stop.log                         0.394 0.693508    
## D.nwrds.unq.log                                -0.395 0.692751    
## D.terms.n.post.stem.log                            NA       NA    
## `carrier.fctrAT&T`                             -0.052 0.958194    
## carrier.fctrOther                               0.010 0.992166    
## carrier.fctrSprint                              0.771 0.440858    
## `carrier.fctrT-Mobile`                         -0.490 0.624132    
## carrier.fctrUnknown                            -0.775 0.438576    
## carrier.fctrVerizon                             0.371 0.710362    
## cellular.fctr1                                  0.191 0.848743    
## cellular.fctrUnknown                               NA       NA    
## D.npnct14.log                                  -1.763 0.077968 .  
## D.terms.n.post.stem                             0.512 0.608479    
## D.terms.n.post.stop                            -0.515 0.606679    
## D.npnct05.log                                  -1.784 0.074431 .  
## `condition.fctrFor parts or not working`       -0.935 0.349937    
## `condition.fctrManufacturer refurbished`        0.648 0.516873    
## condition.fctrNew                              -0.810 0.418008    
## `condition.fctrNew other (see details)`         0.312 0.755189    
## `condition.fctrSeller refurbished`             -1.937 0.052718 .  
## idseq.my                                       -2.319 0.020376 *  
## startprice.log                                 -7.130    1e-12 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       3.095 0.001969 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.261 0.207152    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.995 0.319764    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.550 0.121241    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.838 0.402191    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.520 0.128614    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.372 0.709865    
## `prdline.my.fctrUnknown:.clusterid.fctr3`       0.051 0.959429    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.354 0.723107    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.020 0.984074    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.065 0.947804    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.132 0.895201    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.857 0.391499    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.104 0.916804    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.721 0.085257 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.908 0.363875    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.320 0.186910    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.448 0.654328    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.706 0.480351    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.356 0.018493 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.524 0.600478    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  784.09  on 892  degrees of freedom
## AIC: 948.09
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-4.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7030879
## 3        0.2 0.7606143
## 4        0.3 0.7824311
## 5        0.4 0.7986942
## 6        0.5 0.8045977
## 7        0.6 0.7893462
## 8        0.7 0.7583548
## 9        0.8 0.6657102
## 10       0.9 0.5090312
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           454                            70
## 2         Y                           100                           350
##          Prediction
## Reference   N   Y
##         N 454  70
##         Y 100 350
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.254620e-01   6.472368e-01   8.001376e-01   8.488005e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.085595e-79   2.613509e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-6.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6642921
## 3        0.2 0.7054108
## 4        0.3 0.7303371
## 5        0.4 0.7424426
## 6        0.5 0.7588832
## 7        0.6 0.7573333
## 8        0.7 0.7326733
## 9        0.8 0.6562986
## 10       0.9 0.5098743
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           396                            79
## 2         Y                           111                           299
##          Prediction
## Reference   N   Y
##         N 396  79
##         Y 111 299
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.853107e-01   5.659749e-01   7.567658e-01   8.119416e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.847896e-53   2.451400e-02 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.681                 0.243
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8949406                    0.5       0.8045977        0.7535992
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8001376             0.8488005     0.5028958   0.8270244
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7588832        0.7853107
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7567658             0.8119416     0.5659749    948.0891
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005787427      0.01185225
##                   label step_major step_minor     bgn     end elapsed
## 2      fit.models_1_glm          2          0 127.718 133.494   5.777
## 3 fit.models_1_bayesglm          3          0 133.495      NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4010  -0.6785  -0.2331   0.6316   2.3555  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                   11.0311411  6.5585705
## biddable                                       1.7414094  0.1974861
## D.ratio.nstopwrds.nwrds                       -1.9828450  2.4501714
## D.npnct15.log                                  1.5869715  0.8953370
## D.npnct03.log                                  0.3591209  1.6651191
## D.terms.n.stem.stop.Ratio                     -5.4445224  5.3218866
## D.ratio.sum.TfIdf.nwrds                       -0.3074912  0.2831652
## .rnorm                                         0.0086422  0.0912756
## D.npnct01.log                                  0.0334377  0.5489651
## D.TfIdf.sum.stem.stop.Ratio                   -0.1885622  3.3933482
## storage.fctr16                                 0.0091580  0.4065053
## storage.fctr32                                -0.1142542  0.4381689
## storage.fctr64                                 0.5763969  0.4275731
## storage.fctrUnknown                           -0.0348479  0.5760677
## D.npnct11.log                                 -0.0443944  0.3561767
## D.npnct10.log                                 -7.5887606  7.5284643
## D.TfIdf.sum.post.stop                          0.0545979  0.2903251
## D.TfIdf.sum.post.stem                          0.0779651  0.3063190
## D.sum.TfIdf                                    0.0779651  0.3063190
## D.npnct13.log                                 -0.0240404  0.3505908
## D.npnct08.log                                  0.6682931  0.6724956
## `prdline.my.fctriPad 1`                        0.5804059  0.4906332
## `prdline.my.fctriPad 2`                        0.5281587  0.4992357
## `prdline.my.fctriPad 3+`                       0.9170369  0.4729375
## prdline.my.fctriPadAir                         1.5289727  0.4814266
## prdline.my.fctriPadmini                        0.7871567  0.4621386
## `prdline.my.fctriPadmini 2+`                   1.1322692  0.5046642
## color.fctrBlack                                0.1638875  0.2487292
## color.fctrGold                                -0.3676075  0.4932730
## `color.fctrSpace Gray`                        -0.1323352  0.3193463
## color.fctrWhite                               -0.0577061  0.2457914
## D.npnct16.log                                  2.0220994  1.6011905
## D.nstopwrds.log                                0.7798052  0.6655717
## D.npnct24.log                                  0.1257415  2.3421011
## D.npnct06.log                                 -3.9284325  1.8983532
## D.npnct28.log                                 -0.0492641  2.1893966
## D.nuppr.log                                   -0.1057463  0.4856763
## D.nchrs.log                                   -0.1878907  0.4960200
## D.npnct12.log                                  0.4847740  0.7272638
## D.nwrds.log                                    0.0756928  0.7748920
## D.npnct09.log                                 -1.8658809  4.7751499
## D.ndgts.log                                    0.5422791  0.4365564
## D.terms.n.post.stop.log                       -0.2569497  1.0312207
## D.nwrds.unq.log                               -0.2613852  1.0353966
## D.terms.n.post.stem.log                       -0.2613852  1.0353966
## `carrier.fctrAT&T`                            -0.1521372  0.7572642
## carrier.fctrOther                              1.5117772  1.7033346
## carrier.fctrSprint                             0.5516919  0.9133922
## `carrier.fctrT-Mobile`                        -0.5966969  0.9869142
## carrier.fctrUnknown                           -0.5619853  0.7622889
## carrier.fctrVerizon                            0.1357106  0.7848237
## cellular.fctr1                                 0.2450217  0.7262713
## cellular.fctrUnknown                           0.0777980  0.8467742
## D.npnct14.log                                 -1.8838140  1.0756989
## D.terms.n.post.stem                           -0.0795792  0.2029531
## D.terms.n.post.stop                           -0.0959494  0.2015820
## D.npnct05.log                                 -3.1465233  1.5285662
## `condition.fctrFor parts or not working`      -0.2910290  0.3831395
## `condition.fctrManufacturer refurbished`       0.3962399  0.5698175
## condition.fctrNew                             -0.2479958  0.2965302
## `condition.fctrNew other (see details)`        0.1572930  0.4395561
## `condition.fctrSeller refurbished`            -0.8312531  0.4556949
## idseq.my                                      -0.0005000  0.0002001
## startprice.log                                -0.9460797  0.1331350
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.0218316  0.6793151
## `prdline.my.fctriPad 1:.clusterid.fctr2`       0.9220626  0.7320739
## `prdline.my.fctriPad 2:.clusterid.fctr2`       0.6454325  0.6686278
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.0322534  0.7114773
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -0.6100807  0.6491246
## `prdline.my.fctriPadmini:.clusterid.fctr2`     0.9703992  0.6916948
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  0.3805342  0.7822512
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -0.2861650  0.8998692
## `prdline.my.fctriPad 1:.clusterid.fctr3`       0.3406214  0.7879464
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.7665352  1.5578172
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -0.2713470  0.6011896
## `prdline.my.fctriPadAir:.clusterid.fctr3`      0.2682154  0.8561322
## `prdline.my.fctriPadmini:.clusterid.fctr3`     0.8051485  0.7657861
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  0.0152238  0.8267652
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.5869731  0.8337580
## `prdline.my.fctriPad 2:.clusterid.fctr4`       0.8237771  1.1380529
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      0.8521876  0.7290154
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -0.1361042  0.7645312
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -0.5067265  0.8367824
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`       2.8276764  1.1448696
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -0.6693741  1.1079492
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.0000000  2.5000000
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.682  0.09258 .  
## biddable                                        8.818  < 2e-16 ***
## D.ratio.nstopwrds.nwrds                        -0.809  0.41836    
## D.npnct15.log                                   1.772  0.07631 .  
## D.npnct03.log                                   0.216  0.82924    
## D.terms.n.stem.stop.Ratio                      -1.023  0.30629    
## D.ratio.sum.TfIdf.nwrds                        -1.086  0.27752    
## .rnorm                                          0.095  0.92457    
## D.npnct01.log                                   0.061  0.95143    
## D.TfIdf.sum.stem.stop.Ratio                    -0.056  0.95569    
## storage.fctr16                                  0.023  0.98203    
## storage.fctr32                                 -0.261  0.79428    
## storage.fctr64                                  1.348  0.17764    
## storage.fctrUnknown                            -0.060  0.95176    
## D.npnct11.log                                  -0.125  0.90081    
## D.npnct10.log                                  -1.008  0.31345    
## D.TfIdf.sum.post.stop                           0.188  0.85083    
## D.TfIdf.sum.post.stem                           0.255  0.79909    
## D.sum.TfIdf                                     0.255  0.79909    
## D.npnct13.log                                  -0.069  0.94533    
## D.npnct08.log                                   0.994  0.32034    
## `prdline.my.fctriPad 1`                         1.183  0.23682    
## `prdline.my.fctriPad 2`                         1.058  0.29009    
## `prdline.my.fctriPad 3+`                        1.939  0.05250 .  
## prdline.my.fctriPadAir                          3.176  0.00149 ** 
## prdline.my.fctriPadmini                         1.703  0.08851 .  
## `prdline.my.fctriPadmini 2+`                    2.244  0.02486 *  
## color.fctrBlack                                 0.659  0.50996    
## color.fctrGold                                 -0.745  0.45613    
## `color.fctrSpace Gray`                         -0.414  0.67859    
## color.fctrWhite                                -0.235  0.81438    
## D.npnct16.log                                   1.263  0.20664    
## D.nstopwrds.log                                 1.172  0.24134    
## D.npnct24.log                                   0.054  0.95718    
## D.npnct06.log                                  -2.069  0.03851 *  
## D.npnct28.log                                  -0.023  0.98205    
## D.nuppr.log                                    -0.218  0.82764    
## D.nchrs.log                                    -0.379  0.70484    
## D.npnct12.log                                   0.667  0.50505    
## D.nwrds.log                                     0.098  0.92219    
## D.npnct09.log                                  -0.391  0.69598    
## D.ndgts.log                                     1.242  0.21417    
## D.terms.n.post.stop.log                        -0.249  0.80323    
## D.nwrds.unq.log                                -0.252  0.80069    
## D.terms.n.post.stem.log                        -0.252  0.80069    
## `carrier.fctrAT&T`                             -0.201  0.84077    
## carrier.fctrOther                               0.888  0.37479    
## carrier.fctrSprint                              0.604  0.54584    
## `carrier.fctrT-Mobile`                         -0.605  0.54544    
## carrier.fctrUnknown                            -0.737  0.46098    
## carrier.fctrVerizon                             0.173  0.86272    
## cellular.fctr1                                  0.337  0.73584    
## cellular.fctrUnknown                            0.092  0.92680    
## D.npnct14.log                                  -1.751  0.07990 .  
## D.terms.n.post.stem                            -0.392  0.69498    
## D.terms.n.post.stop                            -0.476  0.63409    
## D.npnct05.log                                  -2.058  0.03954 *  
## `condition.fctrFor parts or not working`       -0.760  0.44750    
## `condition.fctrManufacturer refurbished`        0.695  0.48682    
## condition.fctrNew                              -0.836  0.40297    
## `condition.fctrNew other (see details)`         0.358  0.72046    
## `condition.fctrSeller refurbished`             -1.824  0.06813 .  
## idseq.my                                       -2.498  0.01247 *  
## startprice.log                                 -7.106 1.19e-12 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.976  0.00292 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.260  0.20784    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.965  0.33439    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.451  0.14682    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.940  0.34729    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.403  0.16064    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.486  0.62664    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.318  0.75048    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.432  0.66553    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -1.134  0.25680    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.451  0.65174    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.313  0.75406    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.051  0.29307    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.018  0.98531    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000  1.00000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.903  0.05699 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.724  0.46916    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.169  0.24242    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.178  0.85870    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.606  0.54480    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000  1.00000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000  1.00000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.470  0.01352 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.604  0.54574    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000  1.00000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  794.45  on 882  degrees of freedom
## AIC: 978.45
## 
## Number of Fisher Scoring iterations: 16
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6953125
## 3        0.2 0.7538048
## 4        0.3 0.7797980
## 5        0.4 0.7903930
## 6        0.5 0.7972350
## 7        0.6 0.7931873
## 8        0.7 0.7552083
## 9        0.8 0.6481752
## 10       0.9 0.4824121
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                452
## 2         Y                                104
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 72
## 2                                346
##          Prediction
## Reference   N   Y
##         N 452  72
##         Y 104 346
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.193018e-01   6.346711e-01   7.936712e-01   8.429848e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   4.293519e-76   1.945412e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6701389
## 3        0.2 0.7088353
## 4        0.3 0.7371938
## 5        0.4 0.7460510
## 6        0.5 0.7603093
## 7        0.6 0.7573333
## 8        0.7 0.7277937
## 9        0.8 0.6410256
## 10       0.9 0.4686347
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                404
## 2         Y                                115
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 71
## 2                                295
##          Prediction
## Reference   N   Y
##         N 404  71
##         Y 115 295
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.898305e-01   5.742551e-01   7.614789e-01   8.162373e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.754179e-55   1.616500e-03 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.972                 0.435
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8909075                    0.5        0.797235        0.7659259
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7936712             0.8429848     0.5277105   0.8348447
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7603093        0.7898305
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7614789             0.8162373     0.5742551     978.447
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01380423      0.02560403
##                   label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_bayesglm          3          0 133.495 139.752   6.257
## 4   fit.models_1_glmnet          4          0 139.753      NA      NA
## [1] "fitting model: All.X.glmnet"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
```

```
## Loading required package: glmnet
## Loaded glmnet 2.0-2
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-12.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-13.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-14.png) 

```
##             Length Class      Mode     
## a0            87   -none-     numeric  
## beta        7917   dgCMatrix  S4       
## df            87   -none-     numeric  
## dim            2   -none-     numeric  
## lambda        87   -none-     numeric  
## dev.ratio     87   -none-     numeric  
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
##                           0.9957629065 
##                               biddable 
##                           1.3948301397 
##                    carrier.fctrUnknown 
##                          -0.2043714335 
##                          D.npnct14.log 
##                          -0.0788332928 
##                          D.npnct05.log 
##                          -0.3314289297 
##                               idseq.my 
##                          -0.0002096538 
##                         startprice.log 
##                          -0.3321137323 
## prdline.my.fctriPad 2:.clusterid.fctr5 
##                           0.2023437418 
## [1] "max lambda < lambdaOpt:"
##                                 (Intercept) 
##                               16.6541051741 
##                                    biddable 
##                                1.7789337712 
##                     D.ratio.nstopwrds.nwrds 
##                               -6.3919019210 
##                               D.npnct15.log 
##                                1.5632208461 
##                               D.npnct03.log 
##                                0.4480532636 
##                   D.terms.n.stem.stop.Ratio 
##                               -6.3372855936 
##                     D.ratio.sum.TfIdf.nwrds 
##                               -0.3580121165 
##                                      .rnorm 
##                                0.0085698586 
##                               D.npnct01.log 
##                               -0.0316336275 
##                 D.TfIdf.sum.stem.stop.Ratio 
##                               -0.5630643660 
##                              storage.fctr16 
##                                0.0225406124 
##                              storage.fctr32 
##                               -0.1027134620 
##                              storage.fctr64 
##                                0.6123620920 
##                         storage.fctrUnknown 
##                               -0.0345960603 
##                               D.npnct11.log 
##                               -0.0813500563 
##                               D.npnct10.log 
##                               -9.9175070525 
##                       D.TfIdf.sum.post.stop 
##                                0.0166367507 
##                       D.TfIdf.sum.post.stem 
##                                0.1212018941 
##                                 D.sum.TfIdf 
##                                0.1004772669 
##                               D.npnct13.log 
##                               -0.0279608847 
##                               D.npnct08.log 
##                                0.7327938687 
##                       prdline.my.fctriPad 1 
##                                0.8676076066 
##                       prdline.my.fctriPad 2 
##                                0.7769130719 
##                      prdline.my.fctriPad 3+ 
##                                1.2482267346 
##                      prdline.my.fctriPadAir 
##                                1.9208396843 
##                     prdline.my.fctriPadmini 
##                                1.0909105030 
##                  prdline.my.fctriPadmini 2+ 
##                                1.4902062959 
##                             color.fctrBlack 
##                                0.1707359730 
##                              color.fctrGold 
##                               -0.4617859901 
##                        color.fctrSpace Gray 
##                               -0.1537531352 
##                             color.fctrWhite 
##                               -0.0546524187 
##                               D.npnct16.log 
##                                2.2482010368 
##                             D.nstopwrds.log 
##                                1.9124833306 
##                               D.npnct24.log 
##                               -1.1771598619 
##                               D.npnct06.log 
##                               -4.4247554874 
##                               D.npnct28.log 
##                               -0.0292359239 
##                                 D.nuppr.log 
##                               -0.0980854541 
##                                 D.nchrs.log 
##                               -0.5422852304 
##                               D.npnct12.log 
##                                0.5933390457 
##                               D.npnct09.log 
##                               -2.0953473852 
##                                 D.ndgts.log 
##                                0.5546916576 
##                     D.terms.n.post.stop.log 
##                               -0.9150842222 
##                             D.nwrds.unq.log 
##                               -0.5703360925 
##                     D.terms.n.post.stem.log 
##                               -0.3327368590 
##                            carrier.fctrAT&T 
##                                0.0115382912 
##                           carrier.fctrOther 
##                                6.0804368988 
##                          carrier.fctrSprint 
##                                0.7831117459 
##                        carrier.fctrT-Mobile 
##                               -0.5542944945 
##                         carrier.fctrUnknown 
##                               -0.3874656197 
##                         carrier.fctrVerizon 
##                                0.2992537418 
##                              cellular.fctr1 
##                                0.0833125125 
##                               D.npnct14.log 
##                               -2.1166052865 
##                         D.terms.n.post.stem 
##                               -0.0223327476 
##                         D.terms.n.post.stop 
##                               -0.1392141956 
##                               D.npnct05.log 
##                               -3.7696511442 
##      condition.fctrFor parts or not working 
##                               -0.3562214676 
##      condition.fctrManufacturer refurbished 
##                                0.4341711624 
##                           condition.fctrNew 
##                               -0.2398485787 
##       condition.fctrNew other (see details) 
##                                0.1709714410 
##            condition.fctrSeller refurbished 
##                               -0.9302216931 
##                                    idseq.my 
##                               -0.0004900226 
##                              startprice.log 
##                               -1.0026070020 
##     prdline.my.fctrUnknown:.clusterid.fctr2 
##                                2.4385983342 
##      prdline.my.fctriPad 1:.clusterid.fctr2 
##                                1.1027805473 
##      prdline.my.fctriPad 2:.clusterid.fctr2 
##                                0.8410527425 
##     prdline.my.fctriPad 3+:.clusterid.fctr2 
##                               -1.2182309463 
##     prdline.my.fctriPadAir:.clusterid.fctr2 
##                               -0.5946454575 
##    prdline.my.fctriPadmini:.clusterid.fctr2 
##                                1.2056922729 
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                0.5262069477 
##     prdline.my.fctrUnknown:.clusterid.fctr3 
##                                0.0151510723 
##      prdline.my.fctriPad 1:.clusterid.fctr3 
##                                0.4557379141 
##      prdline.my.fctriPad 2:.clusterid.fctr3 
##                               -5.1858567770 
##     prdline.my.fctriPad 3+:.clusterid.fctr3 
##                               -0.1867544571 
##     prdline.my.fctriPadAir:.clusterid.fctr3 
##                                0.2431597610 
##    prdline.my.fctriPadmini:.clusterid.fctr3 
##                                0.9655991392 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                0.0868086614 
##      prdline.my.fctriPad 1:.clusterid.fctr4 
##                               -1.7171642638 
##      prdline.my.fctriPad 2:.clusterid.fctr4 
##                                1.3129408948 
##     prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                1.1094427014 
##     prdline.my.fctriPadAir:.clusterid.fctr4 
##                               -0.1115173819 
##    prdline.my.fctriPadmini:.clusterid.fctr4 
##                               -0.5780967105 
##      prdline.my.fctriPad 2:.clusterid.fctr5 
##                                3.5096181151 
##    prdline.my.fctriPadmini:.clusterid.fctr5 
##                               -0.7746154814 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6577381
## 4        0.3 0.7492386
## 5        0.4 0.7497244
## 6        0.5 0.7550562
## 7        0.6 0.7605985
## 8        0.7 0.5772231
## 9        0.8 0.3799283
## 10       0.9 0.2421875
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              477
## 2         Y                              145
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               47
## 2                              305
##          Prediction
## Reference   N   Y
##         N 477  47
##         Y 145 305
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.028747e-01   5.972676e-01   7.764773e-01   8.274264e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   5.065979e-67   2.552837e-12 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6524939
## 4        0.3 0.7508380
## 5        0.4 0.7546700
## 6        0.5 0.7522236
## 7        0.6 0.7565337
## 8        0.7 0.5601375
## 9        0.8 0.3592814
## 10       0.9 0.2056893
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              433
## 2         Y                              135
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               42
## 2                              275
##          Prediction
## Reference   N   Y
##         N 433  42
##         Y 135 275
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.000000e-01   5.914905e-01   7.721016e-01   8.258843e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   3.310805e-60   4.673901e-12 
##       model_id model_method
## 1 All.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      6.678                 0.828
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8538083                    0.6       0.7605985        0.7772206
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7764773             0.8274264     0.5509902   0.8455918
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7565337              0.8
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7721016             0.8258843     0.5914905
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0135659      0.02568244
##                 label step_major step_minor     bgn     end elapsed
## 4 fit.models_1_glmnet          4          0 139.753 150.676  10.923
## 5  fit.models_1_rpart          5          0 150.677      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0111 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-19.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-20.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##           CP nsplit rel error
## 1 0.51111111      0 1.0000000
## 2 0.04333333      1 0.4888889
## 3 0.01111111      3 0.4022222
## 
## Variable importance
##                               biddable 
##                                     41 
##                         startprice.log 
##                                     30 
##                               idseq.my 
##                                     13 
## condition.fctrFor parts or not working 
##                                      2 
##                      condition.fctrNew 
##                                      2 
##                 prdline.my.fctriPadAir 
##                                      2 
##                  prdline.my.fctriPad 1 
##                                      2 
##                  prdline.my.fctriPad 2 
##                                      1 
##                   color.fctrSpace Gray 
##                                      1 
##             prdline.my.fctriPadmini 2+ 
##                                      1 
##                D.ratio.nstopwrds.nwrds 
##                                      1 
##                D.ratio.sum.TfIdf.nwrds 
##                                      1 
##                  D.TfIdf.sum.post.stem 
##                                      1 
##                  D.TfIdf.sum.post.stop 
##                                      1 
##                            D.sum.TfIdf 
##                                      1 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable          < 0.5       to the left,  improve=144.149900, (0 missing)
##       startprice.log    < 4.610145  to the right, improve=117.212600, (0 missing)
##       idseq.my          < 905.5     to the right, improve= 38.912990, (0 missing)
##       condition.fctrNew < 0.5       to the right, improve= 10.867390, (0 missing)
##       D.nwrds.unq.log   < 2.138333  to the right, improve=  9.197126, (0 missing)
##   Surrogate splits:
##       startprice.log                         < 5.030417  to the right, agree=0.751, adj=0.460, (0 split)
##       idseq.my                               < 869       to the right, agree=0.636, adj=0.211, (0 split)
##       condition.fctrFor parts or not working < 0.5       to the left,  agree=0.563, adj=0.053, (0 split)
##       prdline.my.fctriPad 1                  < 0.5       to the left,  agree=0.559, adj=0.044, (0 split)
##       prdline.my.fctriPad 2                  < 0.5       to the left,  agree=0.553, adj=0.033, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.04333333
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (162 obs) right son=7 (288 obs)
##   Primary splits:
##       startprice.log       < 4.863673  to the right, improve=38.027780, (0 missing)
##       idseq.my             < 670.5     to the right, improve=15.110620, (0 missing)
##       condition.fctrNew    < 0.5       to the right, improve= 3.467222, (0 missing)
##       carrier.fctrUnknown  < 0.5       to the right, improve= 3.372762, (0 missing)
##       cellular.fctrUnknown < 0.5       to the right, improve= 2.987781, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir     < 0.5       to the right, agree=0.707, adj=0.185, (0 split)
##       condition.fctrNew          < 0.5       to the right, agree=0.707, adj=0.185, (0 split)
##       prdline.my.fctriPadmini 2+ < 0.5       to the right, agree=0.676, adj=0.099, (0 split)
##       color.fctrSpace Gray       < 0.5       to the right, agree=0.676, adj=0.099, (0 split)
##       color.fctrGold             < 0.5       to the right, agree=0.656, adj=0.043, (0 split)
## 
## Node number 6: 162 observations,    complexity param=0.04333333
##   predicted class=N  expected loss=0.4814815  P(node) =0.1663244
##     class counts:    84    78
##    probabilities: 0.519 0.481 
##   left son=12 (69 obs) right son=13 (93 obs)
##   Primary splits:
##       idseq.my                    < 906.5     to the right, improve=16.765470, (0 missing)
##       prdline.my.fctriPadAir      < 0.5       to the left,  improve= 5.555556, (0 missing)
##       D.ratio.sum.TfIdf.nwrds     < 0.2885561 to the right, improve= 5.081543, (0 missing)
##       D.ratio.nstopwrds.nwrds     < 0.4       to the left,  improve= 4.204106, (0 missing)
##       D.TfIdf.sum.stem.stop.Ratio < 0.9194525 to the left,  improve= 3.901876, (0 missing)
##   Surrogate splits:
##       D.ratio.nstopwrds.nwrds < 0.4292763 to the left,  agree=0.648, adj=0.174, (0 split)
##       D.ratio.sum.TfIdf.nwrds < 0.2688815 to the right, agree=0.642, adj=0.159, (0 split)
##       D.TfIdf.sum.post.stop   < 1.734202  to the right, agree=0.636, adj=0.145, (0 split)
##       D.TfIdf.sum.post.stem   < 1.710971  to the right, agree=0.636, adj=0.145, (0 split)
##       D.sum.TfIdf             < 1.710971  to the right, agree=0.636, adj=0.145, (0 split)
## 
## Node number 7: 288 observations
##   predicted class=Y  expected loss=0.09027778  P(node) =0.2956879
##     class counts:    26   262
##    probabilities: 0.090 0.910 
## 
## Node number 12: 69 observations
##   predicted class=N  expected loss=0.2173913  P(node) =0.07084189
##     class counts:    54    15
##    probabilities: 0.783 0.217 
## 
## Node number 13: 93 observations
##   predicted class=Y  expected loss=0.3225806  P(node) =0.09548255
##     class counts:    30    63
##    probabilities: 0.323 0.677 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 974 450 N (0.53798768 0.46201232)  
##    2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##    3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##      6) startprice.log>=4.863673 162  78 N (0.51851852 0.48148148)  
##       12) idseq.my>=906.5 69  15 N (0.78260870 0.21739130) *
##       13) idseq.my< 906.5 93  30 Y (0.32258065 0.67741935) *
##      7) startprice.log< 4.863673 288  26 Y (0.09027778 0.90972222) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-21.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7821901
## 5        0.4 0.7821901
## 6        0.5 0.7821901
## 7        0.6 0.7821901
## 8        0.7 0.7100271
## 9        0.8 0.7100271
## 10       0.9 0.7100271
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-22.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      468
## 2         Y                                      125
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       56
## 2                                      325
##          Prediction
## Reference   N   Y
##         N 468  56
##         Y 125 325
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.141684e-01   6.220873e-01   7.882906e-01   8.381305e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.530340e-73   4.317455e-07 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-23.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7600000
## 5        0.4 0.7600000
## 6        0.5 0.7600000
## 7        0.6 0.7600000
## 8        0.7 0.6818874
## 9        0.8 0.6818874
## 10       0.9 0.6818874
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rpart.N
## 1         N                                      420
## 2         Y                                      125
##   sold.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                       55
## 2                                      285
##          Prediction
## Reference   N   Y
##         N 420  55
##         Y 125 285
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.966102e-01   5.861800e-01   7.685578e-01   8.226715e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.323328e-58   2.704485e-07 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.656                 0.066
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8214419                    0.6       0.7821901        0.8069991
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7882906             0.8381305     0.6066762   0.8103723
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6            0.76        0.7966102
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7685578             0.8226715       0.58618
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0166685      0.03448591
##                label step_major step_minor     bgn     end elapsed
## 5 fit.models_1_rpart          5          0 150.677 156.198   5.522
## 6    fit.models_1_rf          6          0 156.199      NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-24.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 46 on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-25.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-26.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-27.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8256881
## 3        0.2 0.9433962
## 4        0.3 0.9814613
## 5        0.4 0.9988901
## 6        0.5 1.0000000
## 7        0.6 0.9988877
## 8        0.7 0.9473684
## 9        0.8 0.8535032
## 10       0.9 0.7482615
## 11       1.0 0.1012658
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-28.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-29.png) 

```
##    threshold    f.score
## 1        0.0 0.63320463
## 2        0.1 0.66379310
## 3        0.2 0.70280736
## 4        0.3 0.74861573
## 5        0.4 0.76513317
## 6        0.5 0.76240209
## 7        0.6 0.75414365
## 8        0.7 0.74498567
## 9        0.8 0.68912711
## 10       0.9 0.59567388
## 11       1.0 0.07494145
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   375
## 2         Y                                    94
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                   100
## 2                                   316
##          Prediction
## Reference   N   Y
##         N 375 100
##         Y  94 316
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.807910e-01   5.596450e-01   7.520575e-01   8.076410e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.749595e-51   7.196107e-01 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     16.938                 5.077
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8018487
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962198                     1     0.5975601   0.8397895
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7651332         0.780791
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7520575              0.807641      0.559645
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01385658      0.03037514
##              label step_major step_minor     bgn     end elapsed
## 6  fit.models_1_rf          6          0 156.199 176.698  20.499
## 7 fit.models_1_glm          7          0 176.698      NA      NA
## [1] "fitting model: All.Interact.X.glm"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-30.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-31.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-32.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-33.png) 

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
## -3.3261  -0.6031  -0.0576   0.5218   2.8743  
## 
## Coefficients: (10 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                   -2.064e+03  4.303e+03
## D.ratio.nstopwrds.nwrds                       -1.703e+01  7.927e+00
## D.npnct15.log                                  5.999e-01  1.272e+00
## D.npnct03.log                                  2.333e+00  2.425e+00
## D.terms.n.stem.stop.Ratio                      2.085e+03  4.303e+03
## D.ratio.sum.TfIdf.nwrds                       -1.490e-01  6.948e-01
## .rnorm                                        -2.623e-02  1.020e-01
## D.npnct01.log                                 -1.385e-01  7.330e-01
## D.TfIdf.sum.stem.stop.Ratio                   -2.745e+00  1.980e+01
## storage.fctr16                                -3.385e-01  5.429e-01
## storage.fctr32                                -5.040e-01  5.842e-01
## storage.fctr64                                 6.835e-01  5.428e-01
## storage.fctrUnknown                           -6.678e-01  7.461e-01
## D.npnct11.log                                  1.511e-01  4.309e-01
## D.npnct10.log                                 -2.549e+01  1.215e+03
## D.TfIdf.sum.post.stop                         -5.829e-02  2.921e+00
## D.TfIdf.sum.post.stem                          3.317e-01  3.039e+00
## D.sum.TfIdf                                           NA         NA
## D.npnct13.log                                  3.133e-01  4.554e-01
## D.npnct08.log                                  8.724e-01  7.986e-01
## `prdline.my.fctriPad 1`                        3.289e+01  8.394e+00
## `prdline.my.fctriPad 2`                        9.938e-01  2.313e+00
## `prdline.my.fctriPad 3+`                       7.505e+00  3.245e+00
## prdline.my.fctriPadAir                         1.434e+01  4.651e+00
## prdline.my.fctriPadmini                        3.175e+00  2.291e+00
## `prdline.my.fctriPadmini 2+`                   1.269e+01  5.136e+00
## color.fctrBlack                                1.812e-01  2.880e-01
## color.fctrGold                                -2.405e-01  5.733e-01
## `color.fctrSpace Gray`                        -1.722e-01  3.530e-01
## color.fctrWhite                               -3.232e-02  2.759e-01
## D.npnct16.log                                  2.505e+00  1.910e+00
## D.nstopwrds.log                                4.065e+00  2.259e+00
## D.npnct24.log                                 -1.564e+00  6.601e+00
## D.npnct06.log                                 -6.976e+00  2.523e+00
## D.npnct28.log                                 -3.312e+00  1.707e+03
## D.nuppr.log                                    1.093e+00  3.517e+00
## D.nchrs.log                                   -3.222e+00  4.228e+00
## D.npnct12.log                                  5.094e-01  8.690e-01
## D.nwrds.log                                    1.881e+00  3.511e+00
## D.npnct09.log                                 -7.883e+00  7.735e+02
## D.ndgts.log                                    4.922e-01  6.312e-01
## D.terms.n.post.stop.log                        2.371e+03  4.796e+03
## D.nwrds.unq.log                               -2.378e+03  4.797e+03
## D.terms.n.post.stem.log                               NA         NA
## `carrier.fctrAT&T`                            -4.716e-01  7.552e-01
## carrier.fctrOther                              1.475e+01  1.549e+03
## carrier.fctrSprint                             3.984e-01  9.780e-01
## `carrier.fctrT-Mobile`                        -1.272e+00  1.260e+00
## carrier.fctrUnknown                           -3.349e-01  5.146e-01
## carrier.fctrVerizon                           -8.284e-02  7.990e-01
## cellular.fctr1                                 5.958e-01  6.837e-01
## cellular.fctrUnknown                                  NA         NA
## D.npnct14.log                                 -2.268e+00  1.351e+00
## D.terms.n.post.stem                            1.782e+01  2.695e+01
## D.terms.n.post.stop                           -1.758e+01  2.690e+01
## D.npnct05.log                                 -3.522e+00  2.045e+00
## `condition.fctrFor parts or not working`      -8.282e-01  4.600e-01
## `condition.fctrManufacturer refurbished`      -1.019e-02  6.338e-01
## condition.fctrNew                             -4.062e-02  3.402e-01
## `condition.fctrNew other (see details)`        2.772e-01  5.016e-01
## `condition.fctrSeller refurbished`            -1.286e+00  5.350e-01
## startprice.log                                -7.559e-01  1.876e-01
## biddable                                       2.351e+00  6.586e-01
## idseq.my                                       4.728e-04  6.956e-04
## `prdline.my.fctriPad 1:startprice.log`        -7.185e+00  1.892e+00
## `prdline.my.fctriPad 2:startprice.log`         6.857e-02  4.111e-01
## `prdline.my.fctriPad 3+:startprice.log`       -8.912e-01  5.565e-01
## `prdline.my.fctriPadAir:startprice.log`       -1.821e+00  7.543e-01
## `prdline.my.fctriPadmini:startprice.log`      -1.852e-01  3.837e-01
## `prdline.my.fctriPadmini 2+:startprice.log`   -1.854e+00  8.830e-01
## `prdline.my.fctriPad 1:biddable`              -1.004e-01  1.193e+00
## `prdline.my.fctriPad 2:biddable`               5.680e-01  9.662e-01
## `prdline.my.fctriPad 3+:biddable`             -1.528e+00  8.458e-01
## `prdline.my.fctriPadAir:biddable`             -5.113e-01  8.360e-01
## `prdline.my.fctriPadmini:biddable`            -8.788e-01  8.403e-01
## `prdline.my.fctriPadmini 2+:biddable`         -1.469e+00  9.147e-01
## `prdline.my.fctriPad 1:idseq.my`               6.055e-05  9.566e-04
## `prdline.my.fctriPad 2:idseq.my`              -9.453e-04  9.093e-04
## `prdline.my.fctriPad 3+:idseq.my`             -8.674e-04  7.851e-04
## `prdline.my.fctriPadAir:idseq.my`             -1.932e-03  8.274e-04
## `prdline.my.fctriPadmini:idseq.my`            -6.992e-04  7.777e-04
## `prdline.my.fctriPadmini 2+:idseq.my`         -3.822e-04  8.842e-04
## `prdline.my.fctrUnknown:.clusterid.fctr2`      3.018e+00  9.064e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       2.451e+00  1.108e+00
## `prdline.my.fctriPad 2:.clusterid.fctr2`       1.419e+00  8.861e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.271e+00  8.747e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -7.717e-01  9.195e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.533e+00  8.211e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  6.379e-01  9.873e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`      1.876e-01  1.129e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -4.693e-01  1.719e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.458e+01  7.264e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      2.832e-01  7.162e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`      2.418e-01  1.206e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`     8.892e-01  9.012e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  3.113e-02  9.942e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -2.388e+00  1.634e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`       2.176e+00  1.554e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      1.328e+00  9.182e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -7.331e-01  1.156e+00
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -2.454e-01  1.004e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       4.472e+00  1.571e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -1.108e-01  1.418e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                    -0.480 0.631452    
## D.ratio.nstopwrds.nwrds                        -2.149 0.031647 *  
## D.npnct15.log                                   0.472 0.637273    
## D.npnct03.log                                   0.962 0.336034    
## D.terms.n.stem.stop.Ratio                       0.485 0.627981    
## D.ratio.sum.TfIdf.nwrds                        -0.214 0.830245    
## .rnorm                                         -0.257 0.797132    
## D.npnct01.log                                  -0.189 0.850105    
## D.TfIdf.sum.stem.stop.Ratio                    -0.139 0.889752    
## storage.fctr16                                 -0.624 0.532892    
## storage.fctr32                                 -0.863 0.388253    
## storage.fctr64                                  1.259 0.207955    
## storage.fctrUnknown                            -0.895 0.370726    
## D.npnct11.log                                   0.351 0.725869    
## D.npnct10.log                                  -0.021 0.983258    
## D.TfIdf.sum.post.stop                          -0.020 0.984078    
## D.TfIdf.sum.post.stem                           0.109 0.913077    
## D.sum.TfIdf                                        NA       NA    
## D.npnct13.log                                   0.688 0.491491    
## D.npnct08.log                                   1.092 0.274669    
## `prdline.my.fctriPad 1`                         3.919 8.91e-05 ***
## `prdline.my.fctriPad 2`                         0.430 0.667448    
## `prdline.my.fctriPad 3+`                        2.313 0.020744 *  
## prdline.my.fctriPadAir                          3.084 0.002046 ** 
## prdline.my.fctriPadmini                         1.386 0.165792    
## `prdline.my.fctriPadmini 2+`                    2.472 0.013453 *  
## color.fctrBlack                                 0.629 0.529278    
## color.fctrGold                                 -0.419 0.674910    
## `color.fctrSpace Gray`                         -0.488 0.625750    
## color.fctrWhite                                -0.117 0.906749    
## D.npnct16.log                                   1.311 0.189697    
## D.nstopwrds.log                                 1.799 0.071964 .  
## D.npnct24.log                                  -0.237 0.812747    
## D.npnct06.log                                  -2.765 0.005689 ** 
## D.npnct28.log                                  -0.002 0.998452    
## D.nuppr.log                                     0.311 0.755906    
## D.nchrs.log                                    -0.762 0.445996    
## D.npnct12.log                                   0.586 0.557747    
## D.nwrds.log                                     0.536 0.592055    
## D.npnct09.log                                  -0.010 0.991869    
## D.ndgts.log                                     0.780 0.435543    
## D.terms.n.post.stop.log                         0.494 0.621114    
## D.nwrds.unq.log                                -0.496 0.620054    
## D.terms.n.post.stem.log                            NA       NA    
## `carrier.fctrAT&T`                             -0.625 0.532282    
## carrier.fctrOther                               0.010 0.992402    
## carrier.fctrSprint                              0.407 0.683731    
## `carrier.fctrT-Mobile`                         -1.009 0.312898    
## carrier.fctrUnknown                            -0.651 0.515143    
## carrier.fctrVerizon                            -0.104 0.917417    
## cellular.fctr1                                  0.871 0.383506    
## cellular.fctrUnknown                               NA       NA    
## D.npnct14.log                                  -1.678 0.093350 .  
## D.terms.n.post.stem                             0.661 0.508551    
## D.terms.n.post.stop                            -0.654 0.513315    
## D.npnct05.log                                  -1.722 0.085029 .  
## `condition.fctrFor parts or not working`       -1.800 0.071794 .  
## `condition.fctrManufacturer refurbished`       -0.016 0.987179    
## condition.fctrNew                              -0.119 0.904946    
## `condition.fctrNew other (see details)`         0.553 0.580492    
## `condition.fctrSeller refurbished`             -2.404 0.016223 *  
## startprice.log                                 -4.028 5.62e-05 ***
## biddable                                        3.570 0.000357 ***
## idseq.my                                        0.680 0.496719    
## `prdline.my.fctriPad 1:startprice.log`         -3.798 0.000146 ***
## `prdline.my.fctriPad 2:startprice.log`          0.167 0.867536    
## `prdline.my.fctriPad 3+:startprice.log`        -1.601 0.109286    
## `prdline.my.fctriPadAir:startprice.log`        -2.415 0.015752 *  
## `prdline.my.fctriPadmini:startprice.log`       -0.483 0.629314    
## `prdline.my.fctriPadmini 2+:startprice.log`    -2.100 0.035737 *  
## `prdline.my.fctriPad 1:biddable`               -0.084 0.932923    
## `prdline.my.fctriPad 2:biddable`                0.588 0.556619    
## `prdline.my.fctriPad 3+:biddable`              -1.806 0.070887 .  
## `prdline.my.fctriPadAir:biddable`              -0.612 0.540815    
## `prdline.my.fctriPadmini:biddable`             -1.046 0.295683    
## `prdline.my.fctriPadmini 2+:biddable`          -1.606 0.108199    
## `prdline.my.fctriPad 1:idseq.my`                0.063 0.949535    
## `prdline.my.fctriPad 2:idseq.my`               -1.040 0.298546    
## `prdline.my.fctriPad 3+:idseq.my`              -1.105 0.269255    
## `prdline.my.fctriPadAir:idseq.my`              -2.335 0.019539 *  
## `prdline.my.fctriPadmini:idseq.my`             -0.899 0.368601    
## `prdline.my.fctriPadmini 2+:idseq.my`          -0.432 0.665535    
## `prdline.my.fctrUnknown:.clusterid.fctr2`       3.330 0.000869 ***
## `prdline.my.fctriPad 1:.clusterid.fctr2`        2.212 0.026999 *  
## `prdline.my.fctriPad 2:.clusterid.fctr2`        1.601 0.109277    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.453 0.146140    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.839 0.401285    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.867 0.061844 .  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.646 0.518261    
## `prdline.my.fctrUnknown:.clusterid.fctr3`       0.166 0.868028    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.273 0.784895    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.020 0.983982    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`       0.395 0.692547    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.200 0.841102    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.987 0.323778    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.031 0.975019    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.461 0.143911    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        1.400 0.161657    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.446 0.148200    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.634 0.526022    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.244 0.806920    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.847 0.004412 ** 
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.078 0.937698    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  718.51  on 874  degrees of freedom
## AIC: 918.51
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-34.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-35.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7304204
## 3        0.2 0.7775735
## 4        0.3 0.8125633
## 5        0.4 0.8069040
## 6        0.5 0.8111240
## 7        0.6 0.8068044
## 8        0.7 0.7722008
## 9        0.8 0.7011173
## 10       0.9 0.5900621
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.glm.N
## 1         N                                    388
## 2         Y                                     49
##   sold.fctr.predict.All.Interact.X.glm.Y
## 1                                    136
## 2                                    401
##          Prediction
## Reference   N   Y
##         N 388 136
##         Y  49 401
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.100616e-01   6.230634e-01   7.839911e-01   8.342419e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   6.712911e-71   2.567896e-10 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-36.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6771800
## 3        0.2 0.7010309
## 4        0.3 0.7387387
## 5        0.4 0.7530266
## 6        0.5 0.7531807
## 7        0.6 0.7533512
## 8        0.7 0.7277856
## 9        0.8 0.6533128
## 10       0.9 0.5646259
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.glm.N
## 1         N                                    420
## 2         Y                                    129
##   sold.fctr.predict.All.Interact.X.glm.Y
## 1                                     55
## 2                                    281
##          Prediction
## Reference   N   Y
##         N 420  55
##         Y 129 281
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.920904e-01   5.766981e-01   7.638373e-01   8.183833e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.641218e-56   7.381379e-08 
##             model_id model_method
## 1 All.Interact.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.823                 0.317
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9129304                    0.3       0.8125633        0.7628332
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7839911             0.8342419     0.5212224   0.8209653
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7533512        0.7920904
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7638373             0.8183833     0.5766981     918.512
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009240419      0.02092041
##                   label step_major step_minor     bgn     end elapsed
## 7      fit.models_1_glm          7          0 176.698 182.619   5.921
## 8 fit.models_1_bayesglm          8          0 182.620      NA      NA
## [1] "fitting model: All.Interact.X.bayesglm"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: fitted probabilities numerically 0 or 1 occurred
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-38.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.1681  -0.6455  -0.1805   0.5657   2.4370  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                   11.4160028  6.8564931
## D.ratio.nstopwrds.nwrds                       -1.4615005  2.4236806
## D.npnct15.log                                  0.8810397  1.1200157
## D.npnct03.log                                  0.3009048  1.8757457
## D.terms.n.stem.stop.Ratio                     -6.1229826  5.6580039
## D.ratio.sum.TfIdf.nwrds                       -0.1917736  0.2903543
## .rnorm                                        -0.0053031  0.0953296
## D.npnct01.log                                  0.0821483  0.5907614
## D.TfIdf.sum.stem.stop.Ratio                   -1.1931189  3.4823422
## storage.fctr16                                 0.0449391  0.4234369
## storage.fctr32                                -0.1134139  0.4592044
## storage.fctr64                                 0.7544855  0.4460079
## storage.fctrUnknown                           -0.1123469  0.5984244
## D.npnct11.log                                  0.0547307  0.3710939
## D.npnct10.log                                 -7.7053932  7.5227315
## D.TfIdf.sum.post.stop                          0.0704590  0.2921986
## D.TfIdf.sum.post.stem                          0.0809672  0.3075117
## D.sum.TfIdf                                    0.0809672  0.3075117
## D.npnct13.log                                  0.0732039  0.3611185
## D.npnct08.log                                  0.6689499  0.7106025
## `prdline.my.fctriPad 1`                       19.9095349  5.0425315
## `prdline.my.fctriPad 2`                       -0.1241446  1.3832706
## `prdline.my.fctriPad 3+`                       2.0824767  1.4958799
## prdline.my.fctriPadAir                         3.8203862  1.8332071
## prdline.my.fctriPadmini                        0.5398018  1.2866008
## `prdline.my.fctriPadmini 2+`                   1.7843743  1.8007107
## color.fctrBlack                                0.1333037  0.2648097
## color.fctrGold                                -0.4044513  0.5074949
## `color.fctrSpace Gray`                        -0.1920629  0.3267771
## color.fctrWhite                               -0.0686907  0.2519487
## D.npnct16.log                                  2.2566336  1.6365749
## D.nstopwrds.log                                0.7636137  0.6611988
## D.npnct24.log                                  0.1691439  2.3584630
## D.npnct06.log                                 -4.8379162  2.0478141
## D.npnct28.log                                 -0.0603249  2.1831116
## D.nuppr.log                                   -0.1357811  0.4896836
## D.nchrs.log                                   -0.1994893  0.4990073
## D.npnct12.log                                  0.3389582  0.7595436
## D.nwrds.log                                    0.0710666  0.7788859
## D.npnct09.log                                 -1.6193509  5.0536860
## D.ndgts.log                                    0.8022079  0.4514172
## D.terms.n.post.stop.log                       -0.3097954  1.0429820
## D.nwrds.unq.log                               -0.3154225  1.0474254
## D.terms.n.post.stem.log                       -0.3154225  1.0474254
## `carrier.fctrAT&T`                            -0.2246992  0.7658708
## carrier.fctrOther                              1.4627408  1.7087224
## carrier.fctrSprint                             0.5949891  0.9228651
## `carrier.fctrT-Mobile`                        -0.7025610  1.0145765
## carrier.fctrUnknown                           -0.3813854  0.7676878
## carrier.fctrVerizon                            0.0704048  0.7900070
## cellular.fctr1                                 0.3186039  0.7301277
## cellular.fctrUnknown                          -0.0315473  0.8584801
## D.npnct14.log                                 -1.7974322  1.1038822
## D.terms.n.post.stem                           -0.0646803  0.2017786
## D.terms.n.post.stop                           -0.0801956  0.2002565
## D.npnct05.log                                 -3.2690994  1.5503382
## `condition.fctrFor parts or not working`      -0.4441089  0.4092075
## `condition.fctrManufacturer refurbished`       0.0939231  0.5789419
## condition.fctrNew                             -0.2646782  0.3091064
## `condition.fctrNew other (see details)`        0.1310776  0.4544229
## `condition.fctrSeller refurbished`            -1.0710989  0.4812480
## startprice.log                                -0.8397811  0.1600317
## biddable                                       1.7818741  0.4673126
## idseq.my                                      -0.0002495  0.0004979
## `prdline.my.fctriPad 1:startprice.log`        -4.5202441  1.1334567
## `prdline.my.fctriPad 2:startprice.log`         0.1308938  0.2750417
## `prdline.my.fctriPad 3+:startprice.log`       -0.1308992  0.2635449
## `prdline.my.fctriPadAir:startprice.log`       -0.2687174  0.2960929
## `prdline.my.fctriPadmini:startprice.log`       0.1060963  0.2354003
## `prdline.my.fctriPadmini 2+:startprice.log`   -0.1596780  0.3231862
## `prdline.my.fctriPad 1:biddable`               0.3433701  0.8404069
## `prdline.my.fctriPad 2:biddable`               0.6923817  0.7124843
## `prdline.my.fctriPad 3+:biddable`             -0.6315484  0.6156128
## `prdline.my.fctriPadAir:biddable`              0.3167855  0.6198930
## `prdline.my.fctriPadmini:biddable`            -0.2813675  0.6250466
## `prdline.my.fctriPadmini 2+:biddable`         -0.3712768  0.6770605
## `prdline.my.fctriPad 1:idseq.my`               0.0006633  0.0007435
## `prdline.my.fctriPad 2:idseq.my`              -0.0003876  0.0007211
## `prdline.my.fctriPad 3+:idseq.my`             -0.0001093  0.0005940
## `prdline.my.fctriPadAir:idseq.my`             -0.0010684  0.0006309
## `prdline.my.fctriPadmini:idseq.my`            -0.0001036  0.0005900
## `prdline.my.fctriPadmini 2+:idseq.my`          0.0003223  0.0006942
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.2969342  0.7287689
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.6660189  0.8488957
## `prdline.my.fctriPad 2:.clusterid.fctr2`       0.9375722  0.7125718
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -0.9809342  0.6937563
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -0.6483767  0.7072040
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.0851904  0.6880762
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  0.6257916  0.7875072
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -0.1500191  0.8974657
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -0.2887400  1.1022503
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.6528251  1.6013332
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -0.1438975  0.5944892
## `prdline.my.fctriPadAir:.clusterid.fctr3`      0.4939557  0.9043813
## `prdline.my.fctriPadmini:.clusterid.fctr3`     0.8072263  0.7427838
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -0.0345448  0.8137595
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.8518015  1.1331508
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.3193676  1.1927078
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      0.8404951  0.7356587
## `prdline.my.fctriPadAir:.clusterid.fctr4`     -0.0662072  0.7799415
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -0.2386697  0.8123364
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.3344642  1.2117830
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -0.4603829  1.0853913
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.0000000  2.5000000
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.665 0.095914 .  
## D.ratio.nstopwrds.nwrds                        -0.603 0.546503    
## D.npnct15.log                                   0.787 0.431498    
## D.npnct03.log                                   0.160 0.872551    
## D.terms.n.stem.stop.Ratio                      -1.082 0.279172    
## D.ratio.sum.TfIdf.nwrds                        -0.660 0.508945    
## .rnorm                                         -0.056 0.955637    
## D.npnct01.log                                   0.139 0.889407    
## D.TfIdf.sum.stem.stop.Ratio                    -0.343 0.731885    
## storage.fctr16                                  0.106 0.915480    
## storage.fctr32                                 -0.247 0.804924    
## storage.fctr64                                  1.692 0.090714 .  
## storage.fctrUnknown                            -0.188 0.851082    
## D.npnct11.log                                   0.147 0.882749    
## D.npnct10.log                                  -1.024 0.305702    
## D.TfIdf.sum.post.stop                           0.241 0.809451    
## D.TfIdf.sum.post.stem                           0.263 0.792321    
## D.sum.TfIdf                                     0.263 0.792321    
## D.npnct13.log                                   0.203 0.839358    
## D.npnct08.log                                   0.941 0.346508    
## `prdline.my.fctriPad 1`                         3.948 7.87e-05 ***
## `prdline.my.fctriPad 2`                        -0.090 0.928488    
## `prdline.my.fctriPad 3+`                        1.392 0.163880    
## prdline.my.fctriPadAir                          2.084 0.037161 *  
## prdline.my.fctriPadmini                         0.420 0.674809    
## `prdline.my.fctriPadmini 2+`                    0.991 0.321721    
## color.fctrBlack                                 0.503 0.614687    
## color.fctrGold                                 -0.797 0.425476    
## `color.fctrSpace Gray`                         -0.588 0.556701    
## color.fctrWhite                                -0.273 0.785132    
## D.npnct16.log                                   1.379 0.167933    
## D.nstopwrds.log                                 1.155 0.248134    
## D.npnct24.log                                   0.072 0.942826    
## D.npnct06.log                                  -2.362 0.018153 *  
## D.npnct28.log                                  -0.028 0.977955    
## D.nuppr.log                                    -0.277 0.781563    
## D.nchrs.log                                    -0.400 0.689324    
## D.npnct12.log                                   0.446 0.655405    
## D.nwrds.log                                     0.091 0.927301    
## D.npnct09.log                                  -0.320 0.748643    
## D.ndgts.log                                     1.777 0.075554 .  
## D.terms.n.post.stop.log                        -0.297 0.766445    
## D.nwrds.unq.log                                -0.301 0.763307    
## D.terms.n.post.stem.log                        -0.301 0.763307    
## `carrier.fctrAT&T`                             -0.293 0.769224    
## carrier.fctrOther                               0.856 0.391974    
## carrier.fctrSprint                              0.645 0.519109    
## `carrier.fctrT-Mobile`                         -0.692 0.488644    
## carrier.fctrUnknown                            -0.497 0.619332    
## carrier.fctrVerizon                             0.089 0.928987    
## cellular.fctr1                                  0.436 0.662570    
## cellular.fctrUnknown                           -0.037 0.970686    
## D.npnct14.log                                  -1.628 0.103465    
## D.terms.n.post.stem                            -0.321 0.748551    
## D.terms.n.post.stop                            -0.400 0.688814    
## D.npnct05.log                                  -2.109 0.034976 *  
## `condition.fctrFor parts or not working`       -1.085 0.277793    
## `condition.fctrManufacturer refurbished`        0.162 0.871123    
## condition.fctrNew                              -0.856 0.391849    
## `condition.fctrNew other (see details)`         0.288 0.773003    
## `condition.fctrSeller refurbished`             -2.226 0.026036 *  
## startprice.log                                 -5.248 1.54e-07 ***
## biddable                                        3.813 0.000137 ***
## idseq.my                                       -0.501 0.616311    
## `prdline.my.fctriPad 1:startprice.log`         -3.988 6.66e-05 ***
## `prdline.my.fctriPad 2:startprice.log`          0.476 0.634142    
## `prdline.my.fctriPad 3+:startprice.log`        -0.497 0.619410    
## `prdline.my.fctriPadAir:startprice.log`        -0.908 0.364119    
## `prdline.my.fctriPadmini:startprice.log`        0.451 0.652202    
## `prdline.my.fctriPadmini 2+:startprice.log`    -0.494 0.621254    
## `prdline.my.fctriPad 1:biddable`                0.409 0.682851    
## `prdline.my.fctriPad 2:biddable`                0.972 0.331157    
## `prdline.my.fctriPad 3+:biddable`              -1.026 0.304945    
## `prdline.my.fctriPadAir:biddable`               0.511 0.609328    
## `prdline.my.fctriPadmini:biddable`             -0.450 0.652599    
## `prdline.my.fctriPadmini 2+:biddable`          -0.548 0.583441    
## `prdline.my.fctriPad 1:idseq.my`                0.892 0.372332    
## `prdline.my.fctriPad 2:idseq.my`               -0.537 0.590962    
## `prdline.my.fctriPad 3+:idseq.my`              -0.184 0.854064    
## `prdline.my.fctriPadAir:idseq.my`              -1.693 0.090368 .  
## `prdline.my.fctriPadmini:idseq.my`             -0.176 0.860608    
## `prdline.my.fctriPadmini 2+:idseq.my`           0.464 0.642479    
## `prdline.my.fctrUnknown:.clusterid.fctr2`       3.152 0.001623 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.963 0.049696 *  
## `prdline.my.fctriPad 2:.clusterid.fctr2`        1.316 0.188255    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.414 0.157378    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.917 0.359238    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.577 0.114764    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.795 0.426818    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.167 0.867245    
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.262 0.793356    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -1.032 0.301999    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.242 0.808740    
## `prdline.my.fctriPadAir:.clusterid.fctr3`       0.546 0.584942    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.087 0.277144    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.042 0.966139    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.634 0.102216    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        1.106 0.268642    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.143 0.253243    
## `prdline.my.fctriPadAir:.clusterid.fctr4`      -0.085 0.932351    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.294 0.768906    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000 1.000000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000 1.000000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.752 0.005929 ** 
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.424 0.671447    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000 1.000000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  741.09  on 864  degrees of freedom
## AIC: 961.09
## 
## Number of Fisher Scoring iterations: 28
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7065391
## 3        0.2 0.7619910
## 4        0.3 0.7887888
## 5        0.4 0.7978142
## 6        0.5 0.8088065
## 7        0.6 0.7985348
## 8        0.7 0.7542707
## 9        0.8 0.6781609
## 10       0.9 0.5480769
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.bayesglm.N
## 1         N                                         460
## 2         Y                                         101
##   sold.fctr.predict.All.Interact.X.bayesglm.Y
## 1                                          64
## 2                                         349
##          Prediction
## Reference   N   Y
##         N 460  64
##         Y 101 349
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.305955e-01   6.572339e-01   8.055345e-01   8.536386e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   8.982639e-83   5.069310e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-41.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6779964
## 3        0.2 0.7076923
## 4        0.3 0.7393258
## 5        0.4 0.7548544
## 6        0.5 0.7602041
## 7        0.6 0.7553191
## 8        0.7 0.7175793
## 9        0.8 0.6250000
## 10       0.9 0.5053763
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-42.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.bayesglm.N
## 1         N                                         399
## 2         Y                                         112
##   sold.fctr.predict.All.Interact.X.bayesglm.Y
## 1                                          76
## 2                                         298
##          Prediction
## Reference   N   Y
##         N 399  76
##         Y 112 298
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.875706e-01   5.702552e-01   7.591217e-01   8.140900e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.824793e-54   1.069107e-02 
##                  model_id model_method
## 1 All.Interact.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.625                 0.836
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9047371                    0.5       0.8088065        0.7638715
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8055345             0.8536386     0.5235453   0.8287856
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7602041        0.7875706
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7591217               0.81409     0.5702552    961.0875
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01427511      0.02703693
##                   label step_major step_minor     bgn     end elapsed
## 8 fit.models_1_bayesglm          8          0 182.620 189.591   6.972
## 9   fit.models_1_glmnet          9          0 189.592      NA      NA
## [1] "fitting model: All.Interact.X.glmnet"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-43.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-44.png) 

```
##             Length Class      Mode     
## a0            100  -none-     numeric  
## beta        10900  dgCMatrix  S4       
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
## xNames        109  -none-     character
## problemType     1  -none-     character
## tuneValue       2  data.frame list     
## obsLevels       2  -none-     character
## [1] "min lambda > lambdaOpt:"
##                              (Intercept) 
##                             1.922341e+00 
##                            D.npnct15.log 
##                             7.862417e-01 
##                            D.npnct03.log 
##                             2.001823e-02 
##                D.terms.n.stem.stop.Ratio 
##                            -5.979974e-01 
##                           storage.fctr32 
##                            -1.491588e-02 
##                           storage.fctr64 
##                             2.350952e-01 
##                            D.npnct10.log 
##                            -1.133313e+00 
##                    D.TfIdf.sum.post.stop 
##                             1.718529e-03 
##                    D.TfIdf.sum.post.stem 
##                             1.452436e-03 
##                              D.sum.TfIdf 
##                             1.171898e-03 
##                          color.fctrBlack 
##                             1.032258e-01 
##                           color.fctrGold 
##                            -2.026707e-01 
##                          color.fctrWhite 
##                            -4.461748e-02 
##                            D.npnct06.log 
##                            -4.471173e-01 
##                            D.npnct09.log 
##                            -2.667066e-01 
##                        carrier.fctrOther 
##                             1.165426e+00 
##                       carrier.fctrSprint 
##                             2.960722e-01 
##                     carrier.fctrT-Mobile 
##                            -1.019309e-01 
##                      carrier.fctrUnknown 
##                            -3.157595e-01 
##                      carrier.fctrVerizon 
##                             7.854717e-02 
##                     cellular.fctrUnknown 
##                            -1.031327e-01 
##                            D.npnct14.log 
##                            -6.796386e-01 
##                      D.terms.n.post.stem 
##                            -1.225351e-02 
##                      D.terms.n.post.stop 
##                            -1.119689e-02 
##                            D.npnct05.log 
##                            -1.181667e+00 
##   condition.fctrFor parts or not working 
##                             7.362661e-02 
##                        condition.fctrNew 
##                            -2.689575e-01 
##         condition.fctrSeller refurbished 
##                            -2.257223e-01 
##                           startprice.log 
##                            -3.530793e-01 
##                                 biddable 
##                             1.124531e+00 
##                                 idseq.my 
##                            -3.419209e-04 
##     prdline.my.fctriPad 1:startprice.log 
##                            -2.752600e-02 
##     prdline.my.fctriPad 2:startprice.log 
##                            -2.305005e-03 
##           prdline.my.fctriPad 1:biddable 
##                             6.239926e-01 
##           prdline.my.fctriPad 2:biddable 
##                             6.287647e-01 
##          prdline.my.fctriPad 3+:biddable 
##                             2.382667e-01 
##          prdline.my.fctriPadAir:biddable 
##                             7.501826e-01 
##         prdline.my.fctriPadmini:biddable 
##                             2.230129e-01 
##      prdline.my.fctriPadmini 2+:biddable 
##                             3.844352e-02 
##           prdline.my.fctriPad 1:idseq.my 
##                             1.117435e-04 
##           prdline.my.fctriPad 2:idseq.my 
##                            -7.551322e-05 
##          prdline.my.fctriPadAir:idseq.my 
##                            -1.207837e-04 
##  prdline.my.fctrUnknown:.clusterid.fctr2 
##                             5.476277e-01 
##   prdline.my.fctriPad 1:.clusterid.fctr2 
##                             4.958703e-01 
##   prdline.my.fctriPad 2:.clusterid.fctr2 
##                             1.739743e-02 
##  prdline.my.fctriPad 3+:.clusterid.fctr2 
##                            -4.765229e-01 
##  prdline.my.fctriPadAir:.clusterid.fctr2 
##                            -2.671013e-01 
## prdline.my.fctriPadmini:.clusterid.fctr2 
##                             3.465505e-01 
##  prdline.my.fctrUnknown:.clusterid.fctr3 
##                            -3.480426e-01 
##   prdline.my.fctriPad 2:.clusterid.fctr3 
##                            -1.172322e+00 
##  prdline.my.fctriPad 3+:.clusterid.fctr3 
##                            -1.880635e-01 
##  prdline.my.fctriPadAir:.clusterid.fctr3 
##                             7.265175e-02 
## prdline.my.fctriPadmini:.clusterid.fctr3 
##                             3.201298e-01 
##   prdline.my.fctriPad 1:.clusterid.fctr4 
##                            -9.694715e-01 
##  prdline.my.fctriPad 3+:.clusterid.fctr4 
##                             1.730416e-01 
## prdline.my.fctriPadmini:.clusterid.fctr4 
##                            -1.636539e-01 
##   prdline.my.fctriPad 2:.clusterid.fctr5 
##                             1.620821e+00 
## prdline.my.fctriPadmini:.clusterid.fctr5 
##                            -4.305136e-01 
## [1] "max lambda < lambdaOpt:"
##                                 (Intercept) 
##                               12.9183130205 
##                     D.ratio.nstopwrds.nwrds 
##                               -3.5488457251 
##                               D.npnct15.log 
##                                1.0519023730 
##                               D.npnct03.log 
##                               -0.0848606076 
##                   D.terms.n.stem.stop.Ratio 
##                               -5.8190598760 
##                     D.ratio.sum.TfIdf.nwrds 
##                               -0.2997789433 
##                                      .rnorm 
##                                0.0041276974 
##                               D.npnct01.log 
##                               -0.0574219948 
##                 D.TfIdf.sum.stem.stop.Ratio 
##                               -1.0385183481 
##                              storage.fctr16 
##                               -0.0308852731 
##                              storage.fctr32 
##                               -0.1905685884 
##                              storage.fctr64 
##                                0.7135481151 
##                         storage.fctrUnknown 
##                               -0.2450131396 
##                               D.npnct11.log 
##                                0.0248062182 
##                               D.npnct10.log 
##                               -9.8132967478 
##                       D.TfIdf.sum.post.stop 
##                                0.0718430078 
##                       D.TfIdf.sum.post.stem 
##                                0.0980636631 
##                                 D.sum.TfIdf 
##                                0.0956238161 
##                               D.npnct13.log 
##                                0.1080942456 
##                               D.npnct08.log 
##                                0.7900021664 
##                       prdline.my.fctriPad 1 
##                               11.4478193291 
##                       prdline.my.fctriPad 2 
##                               -0.4640143015 
##                      prdline.my.fctriPad 3+ 
##                                3.6057127754 
##                      prdline.my.fctriPadAir 
##                                5.5347323655 
##                     prdline.my.fctriPadmini 
##                                0.8828814948 
##                  prdline.my.fctriPadmini 2+ 
##                                4.7096547440 
##                             color.fctrBlack 
##                                0.2056363241 
##                              color.fctrGold 
##                               -0.4099785640 
##                        color.fctrSpace Gray 
##                               -0.1842620266 
##                             color.fctrWhite 
##                               -0.0626131308 
##                               D.npnct16.log 
##                                2.6806573979 
##                             D.nstopwrds.log 
##                                1.3306852024 
##                               D.npnct24.log 
##                               -0.1324705465 
##                               D.npnct06.log 
##                               -5.2003618939 
##                               D.npnct28.log 
##                               -1.1267882113 
##                                 D.nuppr.log 
##                               -0.2063651812 
##                                 D.nchrs.log 
##                               -0.3700700381 
##                               D.npnct12.log 
##                                0.4709391433 
##                                 D.nwrds.log 
##                                0.0936215077 
##                               D.npnct09.log 
##                               -1.7444575313 
##                                 D.ndgts.log 
##                                0.8971030868 
##                     D.terms.n.post.stop.log 
##                               -0.4138337885 
##                             D.nwrds.unq.log 
##                               -0.6119928348 
##                     D.terms.n.post.stem.log 
##                               -0.5349791159 
##                           carrier.fctrOther 
##                                6.2426626188 
##                          carrier.fctrSprint 
##                                0.9218636580 
##                        carrier.fctrT-Mobile 
##                               -0.7042752425 
##                         carrier.fctrUnknown 
##                               -0.1044461972 
##                         carrier.fctrVerizon 
##                                0.3443040607 
##                              cellular.fctr1 
##                                0.0905294118 
##                        cellular.fctrUnknown 
##                               -0.2802290948 
##                               D.npnct14.log 
##                               -2.0452645732 
##                         D.terms.n.post.stem 
##                               -0.0453022599 
##                         D.terms.n.post.stop 
##                               -0.0946528694 
##                               D.npnct05.log 
##                               -4.0251071860 
##      condition.fctrFor parts or not working 
##                               -0.4137358111 
##      condition.fctrManufacturer refurbished 
##                                0.1012467137 
##                           condition.fctrNew 
##                               -0.1916569120 
##       condition.fctrNew other (see details) 
##                                0.1609507180 
##            condition.fctrSeller refurbished 
##                               -1.1402400595 
##                              startprice.log 
##                               -0.8904718572 
##                                    biddable 
##                                2.0206785857 
##                                    idseq.my 
##                               -0.0001131449 
##        prdline.my.fctriPad 1:startprice.log 
##                               -2.5620849354 
##        prdline.my.fctriPad 2:startprice.log 
##                                0.2323664928 
##       prdline.my.fctriPad 3+:startprice.log 
##                               -0.3072772838 
##       prdline.my.fctriPadAir:startprice.log 
##                               -0.4594275687 
##      prdline.my.fctriPadmini:startprice.log 
##                                0.1237547980 
##   prdline.my.fctriPadmini 2+:startprice.log 
##                               -0.5893007927 
##              prdline.my.fctriPad 1:biddable 
##                                0.5265641559 
##              prdline.my.fctriPad 2:biddable 
##                                0.7554559230 
##             prdline.my.fctriPad 3+:biddable 
##                               -1.0356799085 
##             prdline.my.fctriPadAir:biddable 
##                                0.0344832178 
##            prdline.my.fctriPadmini:biddable 
##                               -0.5349897429 
##         prdline.my.fctriPadmini 2+:biddable 
##                               -0.7508192024 
##              prdline.my.fctriPad 1:idseq.my 
##                                0.0006174027 
##              prdline.my.fctriPad 2:idseq.my 
##                               -0.0005271803 
##             prdline.my.fctriPad 3+:idseq.my 
##                               -0.0003311883 
##             prdline.my.fctriPadAir:idseq.my 
##                               -0.0013145442 
##            prdline.my.fctriPadmini:idseq.my 
##                               -0.0002635633 
##         prdline.my.fctriPadmini 2+:idseq.my 
##                                0.0001951094 
##     prdline.my.fctrUnknown:.clusterid.fctr2 
##                                2.7407064047 
##      prdline.my.fctriPad 1:.clusterid.fctr2 
##                                1.6558111199 
##      prdline.my.fctriPad 2:.clusterid.fctr2 
##                                1.3177101085 
##     prdline.my.fctriPad 3+:.clusterid.fctr2 
##                               -1.1419689290 
##     prdline.my.fctriPadAir:.clusterid.fctr2 
##                               -0.6110229770 
##    prdline.my.fctriPadmini:.clusterid.fctr2 
##                                1.3653277333 
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                0.8989868679 
##     prdline.my.fctrUnknown:.clusterid.fctr3 
##                                0.0209356652 
##      prdline.my.fctriPad 1:.clusterid.fctr3 
##                                0.0554857848 
##      prdline.my.fctriPad 2:.clusterid.fctr3 
##                               -4.9389286671 
##     prdline.my.fctriPad 3+:.clusterid.fctr3 
##                               -0.0314840735 
##     prdline.my.fctriPadAir:.clusterid.fctr3 
##                                0.5703470211 
##    prdline.my.fctriPadmini:.clusterid.fctr3 
##                                1.0073901555 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                0.0413844893 
##      prdline.my.fctriPad 1:.clusterid.fctr4 
##                               -2.2693483536 
##      prdline.my.fctriPad 2:.clusterid.fctr4 
##                                2.1442148680 
##     prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                1.1291242083 
##     prdline.my.fctriPadAir:.clusterid.fctr4 
##                               -0.0374413357 
##    prdline.my.fctriPadmini:.clusterid.fctr4 
##                               -0.2049249607 
##      prdline.my.fctriPad 2:.clusterid.fctr5 
##                                4.2172204486 
##    prdline.my.fctriPadmini:.clusterid.fctr5 
##                               -0.4236191753 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-45.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6460876
## 3        0.2 0.7066557
## 4        0.3 0.7543860
## 5        0.4 0.7778982
## 6        0.5 0.7786790
## 7        0.6 0.7741935
## 8        0.7 0.6900421
## 9        0.8 0.4966887
## 10       0.9 0.2819048
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-46.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.glmnet.N
## 1         N                                       447
## 2         Y                                       114
##   sold.fctr.predict.All.Interact.X.glmnet.Y
## 1                                        77
## 2                                       336
##          Prediction
## Reference   N   Y
##         N 447  77
##         Y 114 336
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.039014e-01   6.032223e-01   7.775499e-01   8.284008e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.442797e-67   9.190951e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-47.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6439873
## 3        0.2 0.6817360
## 4        0.3 0.7305644
## 5        0.4 0.7637699
## 6        0.5 0.7545220
## 7        0.6 0.7339700
## 8        0.7 0.6320755
## 9        0.8 0.4518519
## 10       0.9 0.2473348
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-48.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.glmnet.N
## 1         N                                       380
## 2         Y                                        98
##   sold.fctr.predict.All.Interact.X.glmnet.Y
## 1                                        95
## 2                                       312
##          Prediction
## Reference   N   Y
##         N 380  95
##         Y  98 312
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.819209e-01   5.612566e-01   7.532341e-01   8.087166e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   5.664722e-52   8.855296e-01 
##                model_id model_method
## 1 All.Interact.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      8.001                 0.976
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8764504                    0.5        0.778679        0.7823425
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7775499             0.8284008     0.5598826   0.8362259
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7637699        0.7819209
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7532341             0.8087166     0.5612566
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.003377418      0.01071358
##                  label step_major step_minor     bgn     end elapsed
## 9  fit.models_1_glmnet          9          0 189.592 201.716  12.124
## 10  fit.models_1_rpart         10          0 201.717      NA      NA
## [1] "fitting model: All.Interact.X.no.rnorm.rpart"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0111 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-49.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-50.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##           CP nsplit rel error
## 1 0.51111111      0 1.0000000
## 2 0.04333333      1 0.4888889
## 3 0.01111111      3 0.4022222
## 
## Variable importance
##                              biddable 
##                                    34 
##                        startprice.log 
##                                    24 
##                              idseq.my 
##                                    11 
##       prdline.my.fctriPadAir:biddable 
##                                     7 
##       prdline.my.fctriPad 3+:biddable 
##                                     6 
##        prdline.my.fctriPad 1:biddable 
##                                     5 
## prdline.my.fctriPadAir:startprice.log 
##                                     3 
## prdline.my.fctriPad 3+:startprice.log 
##                                     2 
##                     condition.fctrNew 
##                                     2 
##                prdline.my.fctriPadAir 
##                                     2 
##       prdline.my.fctriPadAir:idseq.my 
##                                     1 
##               D.ratio.nstopwrds.nwrds 
##                                     1 
##               D.ratio.sum.TfIdf.nwrds 
##                                     1 
##                 D.TfIdf.sum.post.stem 
##                                     1 
##                 D.TfIdf.sum.post.stop 
##                                     1 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable                       < 0.5       to the left,  improve=144.14990, (0 missing)
##       startprice.log                 < 4.610145  to the right, improve=117.21260, (0 missing)
##       idseq.my                       < 905.5     to the right, improve= 38.91299, (0 missing)
##       prdline.my.fctriPad 1:biddable < 0.5       to the left,  improve= 25.37925, (0 missing)
##       prdline.my.fctriPad 2:biddable < 0.5       to the left,  improve= 22.83949, (0 missing)
##   Surrogate splits:
##       startprice.log                  < 5.030417  to the right, agree=0.751, adj=0.460, (0 split)
##       idseq.my                        < 869       to the right, agree=0.636, adj=0.211, (0 split)
##       prdline.my.fctriPad 3+:biddable < 0.5       to the left,  agree=0.624, adj=0.187, (0 split)
##       prdline.my.fctriPadAir:biddable < 0.5       to the left,  agree=0.618, adj=0.173, (0 split)
##       prdline.my.fctriPad 1:biddable  < 0.5       to the left,  agree=0.613, adj=0.162, (0 split)
## 
## Node number 2: 524 observations
##   predicted class=N  expected loss=0.2099237  P(node) =0.5379877
##     class counts:   414   110
##    probabilities: 0.790 0.210 
## 
## Node number 3: 450 observations,    complexity param=0.04333333
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (162 obs) right son=7 (288 obs)
##   Primary splits:
##       startprice.log                            < 4.863673  to the right, improve=38.027780, (0 missing)
##       idseq.my                                  < 670.5     to the right, improve=15.110620, (0 missing)
##       prdline.my.fctriPadmini 2+:startprice.log < 6.024113  to the right, improve= 8.118385, (0 missing)
##       prdline.my.fctriPad 3+:startprice.log     < 5.23369   to the right, improve= 8.059102, (0 missing)
##       prdline.my.fctriPadmini:startprice.log    < 5.176086  to the right, improve= 7.628118, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir:startprice.log < 4.79078   to the right, agree=0.760, adj=0.333, (0 split)
##       prdline.my.fctriPad 3+:startprice.log < 4.935157  to the right, agree=0.716, adj=0.210, (0 split)
##       prdline.my.fctriPadAir                < 0.5       to the right, agree=0.707, adj=0.185, (0 split)
##       condition.fctrNew                     < 0.5       to the right, agree=0.707, adj=0.185, (0 split)
##       prdline.my.fctriPadAir:biddable       < 0.5       to the right, agree=0.707, adj=0.185, (0 split)
## 
## Node number 6: 162 observations,    complexity param=0.04333333
##   predicted class=N  expected loss=0.4814815  P(node) =0.1663244
##     class counts:    84    78
##    probabilities: 0.519 0.481 
##   left son=12 (69 obs) right son=13 (93 obs)
##   Primary splits:
##       idseq.my                              < 906.5     to the right, improve=16.765470, (0 missing)
##       prdline.my.fctriPadAir:startprice.log < 4.993479  to the left,  improve= 6.161522, (0 missing)
##       prdline.my.fctriPadAir                < 0.5       to the left,  improve= 5.555556, (0 missing)
##       prdline.my.fctriPadAir:biddable       < 0.5       to the left,  improve= 5.555556, (0 missing)
##       prdline.my.fctriPadAir:idseq.my       < 46        to the left,  improve= 5.555556, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir:idseq.my < 873       to the right, agree=0.691, adj=0.275, (0 split)
##       D.ratio.nstopwrds.nwrds         < 0.4292763 to the left,  agree=0.648, adj=0.174, (0 split)
##       D.ratio.sum.TfIdf.nwrds         < 0.2688815 to the right, agree=0.642, adj=0.159, (0 split)
##       D.TfIdf.sum.post.stop           < 1.734202  to the right, agree=0.636, adj=0.145, (0 split)
##       D.TfIdf.sum.post.stem           < 1.710971  to the right, agree=0.636, adj=0.145, (0 split)
## 
## Node number 7: 288 observations
##   predicted class=Y  expected loss=0.09027778  P(node) =0.2956879
##     class counts:    26   262
##    probabilities: 0.090 0.910 
## 
## Node number 12: 69 observations
##   predicted class=N  expected loss=0.2173913  P(node) =0.07084189
##     class counts:    54    15
##    probabilities: 0.783 0.217 
## 
## Node number 13: 93 observations
##   predicted class=Y  expected loss=0.3225806  P(node) =0.09548255
##     class counts:    30    63
##    probabilities: 0.323 0.677 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 974 450 N (0.53798768 0.46201232)  
##    2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##    3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##      6) startprice.log>=4.863673 162  78 N (0.51851852 0.48148148)  
##       12) idseq.my>=906.5 69  15 N (0.78260870 0.21739130) *
##       13) idseq.my< 906.5 93  30 Y (0.32258065 0.67741935) *
##      7) startprice.log< 4.863673 288  26 Y (0.09027778 0.90972222) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-51.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7821901
## 5        0.4 0.7821901
## 6        0.5 0.7821901
## 7        0.6 0.7821901
## 8        0.7 0.7100271
## 9        0.8 0.7100271
## 10       0.9 0.7100271
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-52.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rpart.N
## 1         N                                               468
## 2         Y                                               125
##   sold.fctr.predict.All.Interact.X.no.rnorm.rpart.Y
## 1                                                56
## 2                                               325
##          Prediction
## Reference   N   Y
##         N 468  56
##         Y 125 325
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.141684e-01   6.220873e-01   7.882906e-01   8.381305e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.530340e-73   4.317455e-07 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-53.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7600000
## 5        0.4 0.7600000
## 6        0.5 0.7600000
## 7        0.6 0.7600000
## 8        0.7 0.6818874
## 9        0.8 0.6818874
## 10       0.9 0.6818874
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-54.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rpart.N
## 1         N                                               420
## 2         Y                                               125
##   sold.fctr.predict.All.Interact.X.no.rnorm.rpart.Y
## 1                                                55
## 2                                               285
##          Prediction
## Reference   N   Y
##         N 420  55
##         Y 125 285
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.966102e-01   5.861800e-01   7.685578e-01   8.226715e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.323328e-58   2.704485e-07 
##                        model_id model_method
## 1 All.Interact.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.749                 0.073
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8214419                    0.6       0.7821901        0.8069991
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7882906             0.8381305     0.6066762   0.8103723
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6            0.76        0.7966102
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7685578             0.8226715       0.58618
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0166685      0.03448591
##                 label step_major step_minor     bgn     end elapsed
## 10 fit.models_1_rpart         10          0 201.717 207.246   5.529
## 11    fit.models_1_rf         11          0 207.247      NA      NA
## [1] "fitting model: All.Interact.X.no.rnorm.rf"
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 55 on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-55.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-56.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-57.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8211679
## 3        0.2 0.9433962
## 4        0.3 0.9857612
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 0.9988877
## 8        0.7 0.9510490
## 9        0.8 0.8549618
## 10       0.9 0.7569061
## 11       1.0 0.2142857
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-58.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-59.png) 

```
##    threshold    f.score
## 1        0.0 0.63320463
## 2        0.1 0.65692175
## 3        0.2 0.70541872
## 4        0.3 0.74496644
## 5        0.4 0.76390606
## 6        0.5 0.76964048
## 7        0.6 0.75795297
## 8        0.7 0.75287356
## 9        0.8 0.72321429
## 10       0.9 0.65610143
## 11       1.0 0.09722222
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-60.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.N
## 1         N                                            423
## 2         Y                                            121
##   sold.fctr.predict.All.Interact.X.no.rnorm.rf.Y
## 1                                             52
## 2                                            289
##          Prediction
## Reference   N   Y
##         N 423  52
##         Y 121 289
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.045198e-01   6.023402e-01   7.768312e-01   8.301634e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   2.195267e-62   2.341638e-07 
##                     model_id model_method
## 1 All.Interact.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     18.239                 5.749
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.7977398
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962198                     1     0.5894791    0.838095
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7696405        0.8045198
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7768312             0.8301634     0.6023402
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01084154       0.0245984
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
    ,"prdline.my.fctr*startprice.log"
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
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-61.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-62.png) 

```
## Warning: not plotting observations with leverage one:
##   639
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-63.png) 

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
## -2.5488  -0.6225  -0.1060   0.5281   3.4394  
## 
## Coefficients: (11 not defined because of singularities)
##                                                                       Estimate
## (Intercept)                                                          1.121e+00
## `prdline.my.fctriPad 1`                                              3.892e+01
## `prdline.my.fctriPad 2`                                              1.541e-02
## `prdline.my.fctriPad 3+`                                             7.391e+00
## prdline.my.fctriPadAir                                               1.224e+01
## prdline.my.fctriPadmini                                              5.285e+00
## `prdline.my.fctriPadmini 2+`                                         2.076e+01
## biddable                                                             2.080e+00
## startprice.log                                                      -6.333e-01
## `condition.fctrFor parts or not working`                             8.201e-01
## `condition.fctrManufacturer refurbished`                            -1.168e+01
## condition.fctrNew                                                   -9.567e-01
## `condition.fctrNew other (see details)`                              3.281e+00
## `condition.fctrSeller refurbished`                                   2.818e-01
## D.terms.n.post.stop                                                 -7.451e-02
## cellular.fctr1                                                      -2.359e+00
## cellular.fctrUnknown                                                -5.987e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            2.229e+00
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            -3.515e+00
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             7.191e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                           -7.235e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                           -9.003e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           2.367e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        3.222e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           -3.224e-01
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            -7.532e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            -1.565e+01
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            2.472e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            7.428e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           2.774e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        1.312e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            -7.429e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             1.314e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            1.277e+00
## `prdline.my.fctriPadAir:.clusterid.fctr4`                           -4.473e-01
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           1.468e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             3.577e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           9.077e-02
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                               NA
## `prdline.my.fctriPad 1:biddable`                                    -2.223e-01
## `prdline.my.fctriPad 2:biddable`                                     8.803e-01
## `prdline.my.fctriPad 3+:biddable`                                   -1.086e+00
## `prdline.my.fctriPadAir:biddable`                                   -1.272e-01
## `prdline.my.fctriPadmini:biddable`                                  -6.331e-01
## `prdline.my.fctriPadmini 2+:biddable`                               -1.270e+00
## `prdline.my.fctriPad 1:startprice.log`                              -8.537e+00
## `prdline.my.fctriPad 2:startprice.log`                               1.226e-01
## `prdline.my.fctriPad 3+:startprice.log`                             -1.021e+00
## `prdline.my.fctriPadAir:startprice.log`                             -1.797e+00
## `prdline.my.fctriPadmini:startprice.log`                            -7.509e-01
## `prdline.my.fctriPadmini 2+:startprice.log`                         -3.579e+00
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`      -6.443e+00
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`      -9.115e-03
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     -2.157e+00
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     -2.600e+00
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    -3.103e+00
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` -1.767e+01
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      1.075e+01
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      1.295e+01
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     1.125e+01
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`         NA
## `prdline.my.fctriPad 1:condition.fctrNew`                           -6.331e+00
## `prdline.my.fctriPad 2:condition.fctrNew`                                   NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                          -1.101e+00
## `prdline.my.fctriPadAir:condition.fctrNew`                           8.092e-01
## `prdline.my.fctriPadmini:condition.fctrNew`                          9.109e-01
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       2.556e+00
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       -2.216e+01
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       -1.776e+01
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`      -2.168e+00
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`      -1.752e+00
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     -4.073e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  -4.444e+00
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.613e+00
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            -3.912e-01
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           -2.187e+00
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           -6.807e+00
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          -1.473e+00
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       -1.792e+01
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          6.501e-01
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          2.779e-02
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        -6.399e-02
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        -3.191e-02
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       -1.967e-01
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    -1.221e-01
## `prdline.my.fctriPad 1:cellular.fctr1`                               2.949e+00
## `prdline.my.fctriPad 2:cellular.fctr1`                               1.465e+00
## `prdline.my.fctriPad 3+:cellular.fctr1`                              2.796e+00
## `prdline.my.fctriPadAir:cellular.fctr1`                              2.221e+00
## `prdline.my.fctriPadmini:cellular.fctr1`                             2.971e+00
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          3.708e+00
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         1.955e+00
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        -9.307e-01
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       -1.926e+00
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                       -4.148e-01
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       7.711e-01
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    6.006e-01
##                                                                     Std. Error
## (Intercept)                                                          1.163e+00
## `prdline.my.fctriPad 1`                                              1.238e+01
## `prdline.my.fctriPad 2`                                              2.037e+00
## `prdline.my.fctriPad 3+`                                             3.018e+00
## prdline.my.fctriPadAir                                               4.458e+00
## prdline.my.fctriPadmini                                              2.552e+00
## `prdline.my.fctriPadmini 2+`                                         7.648e+00
## biddable                                                             6.599e-01
## startprice.log                                                       2.092e-01
## `condition.fctrFor parts or not working`                             8.326e-01
## `condition.fctrManufacturer refurbished`                             2.400e+03
## condition.fctrNew                                                    9.570e-01
## `condition.fctrNew other (see details)`                              1.632e+00
## `condition.fctrSeller refurbished`                                   1.355e+00
## D.terms.n.post.stop                                                  1.161e-01
## cellular.fctr1                                                       1.598e+00
## cellular.fctrUnknown                                                 7.961e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            1.087e+00
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             2.433e+00
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             1.074e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            7.869e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            9.778e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           1.128e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        1.783e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            1.136e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             3.619e+00
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             7.960e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            7.611e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            1.068e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           1.432e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        1.459e+00
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             3.207e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             1.377e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            8.456e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            1.004e+00
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           1.408e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                               NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                    NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             1.505e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                   NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           1.413e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                               NA
## `prdline.my.fctriPad 1:biddable`                                     1.219e+00
## `prdline.my.fctriPad 2:biddable`                                     9.498e-01
## `prdline.my.fctriPad 3+:biddable`                                    8.306e-01
## `prdline.my.fctriPadAir:biddable`                                    8.033e-01
## `prdline.my.fctriPadmini:biddable`                                   8.606e-01
## `prdline.my.fctriPadmini 2+:biddable`                                9.500e-01
## `prdline.my.fctriPad 1:startprice.log`                               2.801e+00
## `prdline.my.fctriPad 2:startprice.log`                               3.736e-01
## `prdline.my.fctriPad 3+:startprice.log`                              5.493e-01
## `prdline.my.fctriPadAir:startprice.log`                              7.534e-01
## `prdline.my.fctriPadmini:startprice.log`                             4.766e-01
## `prdline.my.fctriPadmini 2+:startprice.log`                          1.406e+00
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       3.041e+00
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       1.393e+00
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      1.187e+00
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      1.589e+00
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     1.125e+00
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  1.587e+03
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`              NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      2.400e+03
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      2.400e+03
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     2.400e+03
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`         NA
## `prdline.my.fctriPad 1:condition.fctrNew`                            7.849e+02
## `prdline.my.fctriPad 2:condition.fctrNew`                                   NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                           1.965e+00
## `prdline.my.fctriPadAir:condition.fctrNew`                           1.101e+00
## `prdline.my.fctriPadmini:condition.fctrNew`                          1.162e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       1.397e+00
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        1.270e+05
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        1.670e+03
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       1.875e+00
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       1.858e+00
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      2.114e+00
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   2.088e+00
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             2.557e+00
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             1.694e+00
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            1.787e+00
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            1.548e+01
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           1.765e+00
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        1.620e+03
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          3.308e-01
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          1.598e-01
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         1.386e-01
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         1.459e-01
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        1.752e-01
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     2.030e-01
## `prdline.my.fctriPad 1:cellular.fctr1`                               1.876e+00
## `prdline.my.fctriPad 2:cellular.fctr1`                               1.721e+00
## `prdline.my.fctriPad 3+:cellular.fctr1`                              1.667e+00
## `prdline.my.fctriPadAir:cellular.fctr1`                              1.664e+00
## `prdline.my.fctriPadmini:cellular.fctr1`                             1.691e+00
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          1.804e+00
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         2.270e+00
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         1.584e+00
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        1.717e+00
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        1.880e+00
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       1.243e+00
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    1.322e+00
##                                                                     z value
## (Intercept)                                                           0.964
## `prdline.my.fctriPad 1`                                               3.143
## `prdline.my.fctriPad 2`                                               0.008
## `prdline.my.fctriPad 3+`                                              2.449
## prdline.my.fctriPadAir                                                2.746
## prdline.my.fctriPadmini                                               2.071
## `prdline.my.fctriPadmini 2+`                                          2.715
## biddable                                                              3.153
## startprice.log                                                       -3.027
## `condition.fctrFor parts or not working`                              0.985
## `condition.fctrManufacturer refurbished`                             -0.005
## condition.fctrNew                                                    -1.000
## `condition.fctrNew other (see details)`                               2.010
## `condition.fctrSeller refurbished`                                    0.208
## D.terms.n.post.stop                                                  -0.642
## cellular.fctr1                                                       -1.476
## cellular.fctrUnknown                                                 -0.752
## `prdline.my.fctrUnknown:.clusterid.fctr2`                             2.051
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             -1.445
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              0.670
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            -0.919
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            -0.921
## `prdline.my.fctriPadmini:.clusterid.fctr2`                            2.098
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                         1.807
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            -0.284
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             -2.082
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             -0.020
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             0.325
## `prdline.my.fctriPadAir:.clusterid.fctr3`                             0.696
## `prdline.my.fctriPadmini:.clusterid.fctr3`                            1.937
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                         0.899
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             -2.317
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              0.954
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             1.510
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            -0.445
## `prdline.my.fctriPadmini:.clusterid.fctr4`                            1.043
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                            NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                              2.377
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                            0.064
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                            NA
## `prdline.my.fctriPad 1:biddable`                                     -0.182
## `prdline.my.fctriPad 2:biddable`                                      0.927
## `prdline.my.fctriPad 3+:biddable`                                    -1.307
## `prdline.my.fctriPadAir:biddable`                                    -0.158
## `prdline.my.fctriPadmini:biddable`                                   -0.736
## `prdline.my.fctriPadmini 2+:biddable`                                -1.337
## `prdline.my.fctriPad 1:startprice.log`                               -3.048
## `prdline.my.fctriPad 2:startprice.log`                                0.328
## `prdline.my.fctriPad 3+:startprice.log`                              -1.858
## `prdline.my.fctriPadAir:startprice.log`                              -2.385
## `prdline.my.fctriPadmini:startprice.log`                             -1.576
## `prdline.my.fctriPadmini 2+:startprice.log`                          -2.545
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       -2.119
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       -0.007
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      -1.818
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      -1.636
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     -2.758
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  -0.011
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`           NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`           NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`       0.004
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`       0.005
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`      0.005
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`      NA
## `prdline.my.fctriPad 1:condition.fctrNew`                            -0.008
## `prdline.my.fctriPad 2:condition.fctrNew`                                NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                           -0.561
## `prdline.my.fctriPadAir:condition.fctrNew`                            0.735
## `prdline.my.fctriPadmini:condition.fctrNew`                           0.784
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                        1.830
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`         0.000
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        -0.011
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       -1.156
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       -0.943
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      -1.927
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   -2.129
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              0.631
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             -0.231
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            -1.224
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            -0.440
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           -0.835
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        -0.011
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                           1.965
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                           0.174
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         -0.462
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         -0.219
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        -1.123
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     -0.602
## `prdline.my.fctriPad 1:cellular.fctr1`                                1.572
## `prdline.my.fctriPad 2:cellular.fctr1`                                0.851
## `prdline.my.fctriPad 3+:cellular.fctr1`                               1.677
## `prdline.my.fctriPadAir:cellular.fctr1`                               1.335
## `prdline.my.fctriPadmini:cellular.fctr1`                              1.757
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                           2.056
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                          0.861
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         -0.588
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        -1.121
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        -0.221
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                        0.620
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                     0.454
##                                                                     Pr(>|z|)
## (Intercept)                                                          0.33518
## `prdline.my.fctriPad 1`                                              0.00167
## `prdline.my.fctriPad 2`                                              0.99396
## `prdline.my.fctriPad 3+`                                             0.01433
## prdline.my.fctriPadAir                                               0.00603
## prdline.my.fctriPadmini                                              0.03839
## `prdline.my.fctriPadmini 2+`                                         0.00663
## biddable                                                             0.00162
## startprice.log                                                       0.00247
## `condition.fctrFor parts or not working`                             0.32461
## `condition.fctrManufacturer refurbished`                             0.99612
## condition.fctrNew                                                    0.31746
## `condition.fctrNew other (see details)`                              0.04441
## `condition.fctrSeller refurbished`                                   0.83524
## D.terms.n.post.stop                                                  0.52110
## cellular.fctr1                                                       0.13997
## cellular.fctrUnknown                                                 0.45205
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            0.04031
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             0.14846
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             0.50305
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            0.35788
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            0.35715
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           0.03586
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        0.07083
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            0.77654
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             0.03739
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             0.98432
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            0.74533
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            0.48671
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           0.05275
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        0.36842
## `prdline.my.fctrUnknown:.clusterid.fctr4`                                 NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             0.02052
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             0.33993
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            0.13110
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            0.65605
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           0.29702
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                             NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`                                  NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             0.01746
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`                                 NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           0.94879
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                             NA
## `prdline.my.fctriPad 1:biddable`                                     0.85532
## `prdline.my.fctriPad 2:biddable`                                     0.35403
## `prdline.my.fctriPad 3+:biddable`                                    0.19108
## `prdline.my.fctriPadAir:biddable`                                    0.87414
## `prdline.my.fctriPadmini:biddable`                                   0.46192
## `prdline.my.fctriPadmini 2+:biddable`                                0.18138
## `prdline.my.fctriPad 1:startprice.log`                               0.00230
## `prdline.my.fctriPad 2:startprice.log`                               0.74279
## `prdline.my.fctriPad 3+:startprice.log`                              0.06311
## `prdline.my.fctriPadAir:startprice.log`                              0.01707
## `prdline.my.fctriPadmini:startprice.log`                             0.11512
## `prdline.my.fctriPadmini 2+:startprice.log`                          0.01092
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       0.03408
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       0.99478
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      0.06904
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      0.10175
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     0.00582
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  0.99112
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`            NA
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`            NA
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      0.99642
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      0.99569
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     0.99626
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`       NA
## `prdline.my.fctriPad 1:condition.fctrNew`                            0.99356
## `prdline.my.fctriPad 2:condition.fctrNew`                                 NA
## `prdline.my.fctriPad 3+:condition.fctrNew`                           0.57506
## `prdline.my.fctriPadAir:condition.fctrNew`                           0.46217
## `prdline.my.fctriPadmini:condition.fctrNew`                          0.43295
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       0.06732
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        0.99986
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        0.99152
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       0.24758
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       0.34560
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      0.05398
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   0.03329
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             0.52806
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             0.81740
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            0.22087
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            0.66008
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           0.40383
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        0.99117
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          0.04938
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          0.86194
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         0.64421
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         0.82683
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        0.26155
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     0.54737
## `prdline.my.fctriPad 1:cellular.fctr1`                               0.11598
## `prdline.my.fctriPad 2:cellular.fctr1`                               0.39461
## `prdline.my.fctriPad 3+:cellular.fctr1`                              0.09347
## `prdline.my.fctriPadAir:cellular.fctr1`                              0.18193
## `prdline.my.fctriPadmini:cellular.fctr1`                             0.07899
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          0.03976
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         0.38910
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         0.55675
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        0.26215
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        0.82534
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       0.53494
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    0.64967
##                                                                       
## (Intercept)                                                           
## `prdline.my.fctriPad 1`                                             **
## `prdline.my.fctriPad 2`                                               
## `prdline.my.fctriPad 3+`                                            * 
## prdline.my.fctriPadAir                                              **
## prdline.my.fctriPadmini                                             * 
## `prdline.my.fctriPadmini 2+`                                        **
## biddable                                                            **
## startprice.log                                                      **
## `condition.fctrFor parts or not working`                              
## `condition.fctrManufacturer refurbished`                              
## condition.fctrNew                                                     
## `condition.fctrNew other (see details)`                             * 
## `condition.fctrSeller refurbished`                                    
## D.terms.n.post.stop                                                   
## cellular.fctr1                                                        
## cellular.fctrUnknown                                                  
## `prdline.my.fctrUnknown:.clusterid.fctr2`                           * 
## `prdline.my.fctriPad 1:.clusterid.fctr2`                              
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                             
## `prdline.my.fctriPadAir:.clusterid.fctr2`                             
## `prdline.my.fctriPadmini:.clusterid.fctr2`                          * 
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                       . 
## `prdline.my.fctrUnknown:.clusterid.fctr3`                             
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            * 
## `prdline.my.fctriPad 2:.clusterid.fctr3`                              
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             
## `prdline.my.fctriPadAir:.clusterid.fctr3`                             
## `prdline.my.fctriPadmini:.clusterid.fctr3`                          . 
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                         
## `prdline.my.fctrUnknown:.clusterid.fctr4`                             
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            * 
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             
## `prdline.my.fctriPadAir:.clusterid.fctr4`                             
## `prdline.my.fctriPadmini:.clusterid.fctr4`                            
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                         
## `prdline.my.fctrUnknown:.clusterid.fctr5`                             
## `prdline.my.fctriPad 1:.clusterid.fctr5`                              
## `prdline.my.fctriPad 2:.clusterid.fctr5`                            * 
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
## `prdline.my.fctriPad 1:startprice.log`                              **
## `prdline.my.fctriPad 2:startprice.log`                                
## `prdline.my.fctriPad 3+:startprice.log`                             . 
## `prdline.my.fctriPadAir:startprice.log`                             * 
## `prdline.my.fctriPadmini:startprice.log`                              
## `prdline.my.fctriPadmini 2+:startprice.log`                         * 
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`      * 
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`        
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     . 
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`       
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    **
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
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     . 
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  * 
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`              
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`             
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`             
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`            
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`         
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                         * 
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
## Residual deviance:  725.06  on 880  degrees of freedom
## AIC: 913.06
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-64.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-65.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7090465
## 3        0.2 0.7734807
## 4        0.3 0.8085977
## 5        0.4 0.8236583
## 6        0.5 0.8116279
## 7        0.6 0.8063725
## 8        0.7 0.7855297
## 9        0.8 0.7252747
## 10       0.9 0.5887850
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.glm.N sold.fctr.predict.csm.glm.Y
## 1         N                         437                          87
## 2         Y                          74                         376
##          Prediction
## Reference   N   Y
##         N 437  87
##         Y  74 376
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.347023e-01   6.681621e-01   8.098576e-01   8.575034e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.696932e-85   3.442850e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-66.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-67.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6739328
## 3        0.2 0.7001045
## 4        0.3 0.7373272
## 5        0.4 0.7377451
## 6        0.5 0.7480720
## 7        0.6 0.7479893
## 8        0.7 0.7359551
## 9        0.8 0.6796992
## 10       0.9 0.5795645
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.glm.N sold.fctr.predict.csm.glm.Y
## 1         N                         398                          77
## 2         Y                         119                         291
##          Prediction
## Reference   N   Y
##         N 398  77
##         Y 119 291
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.785311e-01   5.515164e-01   7.497051e-01   8.054889e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.636469e-50   3.405236e-03 
##   model_id model_method
## 1  csm.glm          glm
##                                                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.823                  0.32
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9084139                    0.4       0.8236583        0.7761728
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8098576             0.8575034     0.5480081   0.8167343
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5        0.748072        0.7785311
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7497051             0.8054889     0.5515164    913.0612
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.02106992      0.04477307
##                                                                  importance
## biddable                                                          100.00000
## `prdline.my.fctriPad 1`                                            99.70423
## `prdline.my.fctriPad 1:startprice.log`                             96.68720
## startprice.log                                                     96.01400
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`   87.47363
## prdline.my.fctriPadAir                                             87.10941
## [1] "fitting model: csm.bayesglm"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: fitted probabilities numerically 0 or 1 occurred
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-68.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8195  -0.6694  -0.2460   0.5901   2.6247  
## 
## Coefficients:
##                                                                     Estimate
## (Intercept)                                                          2.69596
## `prdline.my.fctriPad 1`                                             17.54183
## `prdline.my.fctriPad 2`                                             -0.56432
## `prdline.my.fctriPad 3+`                                             2.42704
## prdline.my.fctriPadAir                                               2.87333
## prdline.my.fctriPadmini                                              1.61589
## `prdline.my.fctriPadmini 2+`                                         2.37613
## biddable                                                             1.65029
## startprice.log                                                      -0.86884
## `condition.fctrFor parts or not working`                             0.05569
## `condition.fctrManufacturer refurbished`                            -0.07468
## condition.fctrNew                                                   -1.09635
## `condition.fctrNew other (see details)`                              0.73135
## `condition.fctrSeller refurbished`                                  -0.56160
## D.terms.n.post.stop                                                 -0.07378
## cellular.fctr1                                                      -0.45556
## cellular.fctrUnknown                                                -0.52371
## `prdline.my.fctrUnknown:.clusterid.fctr2`                            1.90128
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            -0.07691
## `prdline.my.fctriPad 2:.clusterid.fctr2`                             0.43547
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                           -0.55200
## `prdline.my.fctriPadAir:.clusterid.fctr2`                           -0.57277
## `prdline.my.fctriPadmini:.clusterid.fctr2`                           1.45280
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                        1.05164
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           -0.38856
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            -2.00607
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            -2.28198
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                            0.19975
## `prdline.my.fctriPadAir:.clusterid.fctr3`                            0.60887
## `prdline.my.fctriPadmini:.clusterid.fctr3`                           1.59864
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                        0.14362
## `prdline.my.fctrUnknown:.clusterid.fctr4`                            0.00000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            -2.80753
## `prdline.my.fctriPad 2:.clusterid.fctr4`                             0.81658
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                            1.10549
## `prdline.my.fctriPadAir:.clusterid.fctr4`                           -0.27320
## `prdline.my.fctriPadmini:.clusterid.fctr4`                           0.38172
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                        0.00000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                            0.00000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                             0.00000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                             2.90587
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                            0.00000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                            0.00000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                          -0.39417
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                        0.00000
## `prdline.my.fctriPad 1:biddable`                                     0.27186
## `prdline.my.fctriPad 2:biddable`                                     0.96066
## `prdline.my.fctriPad 3+:biddable`                                   -0.41014
## `prdline.my.fctriPadAir:biddable`                                    0.56616
## `prdline.my.fctriPadmini:biddable`                                  -0.06592
## `prdline.my.fctriPadmini 2+:biddable`                               -0.28169
## `prdline.my.fctriPad 1:startprice.log`                              -3.85110
## `prdline.my.fctriPad 2:startprice.log`                               0.19614
## `prdline.my.fctriPad 3+:startprice.log`                             -0.17504
## `prdline.my.fctriPadAir:startprice.log`                             -0.24977
## `prdline.my.fctriPadmini:startprice.log`                            -0.11251
## `prdline.my.fctriPadmini 2+:startprice.log`                         -0.27368
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`      -0.78902
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`       0.40768
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     -0.91955
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     -0.62404
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    -1.82462
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` -1.15292
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`       0.00000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`       0.00000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     -0.55088
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`      1.17319
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    -0.28249
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished` -0.35336
## `prdline.my.fctriPad 1:condition.fctrNew`                           -0.03983
## `prdline.my.fctriPad 2:condition.fctrNew`                            0.00000
## `prdline.my.fctriPad 3+:condition.fctrNew`                          -0.50832
## `prdline.my.fctriPadAir:condition.fctrNew`                           0.55121
## `prdline.my.fctriPadmini:condition.fctrNew`                          0.88450
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                       1.38874
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       -0.27261
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       -0.99603
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`       0.23278
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`       0.37809
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     -1.13761
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  -1.86838
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`             1.39636
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`             0.37400
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           -0.99697
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           -0.99784
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          -0.51657
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       -1.53099
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                          0.21001
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                          0.04113
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        -0.06350
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        -0.02975
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       -0.08281
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                     0.01381
## `prdline.my.fctriPad 1:cellular.fctr1`                               0.49332
## `prdline.my.fctriPad 2:cellular.fctr1`                              -0.33799
## `prdline.my.fctriPad 3+:cellular.fctr1`                              0.75489
## `prdline.my.fctriPadAir:cellular.fctr1`                              0.20931
## `prdline.my.fctriPadmini:cellular.fctr1`                             0.91460
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                          0.86484
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                         0.31448
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        -0.70679
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       -1.10384
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                        0.07425
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                       0.51155
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                    0.18186
##                                                                     Std. Error
## (Intercept)                                                            0.85731
## `prdline.my.fctriPad 1`                                                4.88980
## `prdline.my.fctriPad 2`                                                1.35431
## `prdline.my.fctriPad 3+`                                               1.47976
## prdline.my.fctriPadAir                                                 1.69894
## prdline.my.fctriPadmini                                                1.36897
## `prdline.my.fctriPadmini 2+`                                           1.94227
## biddable                                                               0.46243
## startprice.log                                                         0.16875
## `condition.fctrFor parts or not working`                               0.58753
## `condition.fctrManufacturer refurbished`                               1.05933
## condition.fctrNew                                                      0.64367
## `condition.fctrNew other (see details)`                                0.79573
## `condition.fctrSeller refurbished`                                     0.77748
## D.terms.n.post.stop                                                    0.08724
## cellular.fctr1                                                         0.62160
## cellular.fctrUnknown                                                   0.51358
## `prdline.my.fctrUnknown:.clusterid.fctr2`                              0.82618
## `prdline.my.fctriPad 1:.clusterid.fctr2`                               1.09479
## `prdline.my.fctriPad 2:.clusterid.fctr2`                               0.84132
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                              0.67292
## `prdline.my.fctriPadAir:.clusterid.fctr2`                              0.78323
## `prdline.my.fctriPadmini:.clusterid.fctr2`                             0.80632
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                          1.17797
## `prdline.my.fctrUnknown:.clusterid.fctr3`                              0.90768
## `prdline.my.fctriPad 1:.clusterid.fctr3`                               1.30973
## `prdline.my.fctriPad 2:.clusterid.fctr3`                               1.66691
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                              0.66297
## `prdline.my.fctriPadAir:.clusterid.fctr3`                              0.85522
## `prdline.my.fctriPadmini:.clusterid.fctr3`                             0.97074
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                          0.97759
## `prdline.my.fctrUnknown:.clusterid.fctr4`                              2.50000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                               1.27765
## `prdline.my.fctriPad 2:.clusterid.fctr4`                               1.07241
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                              0.74183
## `prdline.my.fctriPadAir:.clusterid.fctr4`                              0.82264
## `prdline.my.fctriPadmini:.clusterid.fctr4`                             0.99726
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                          2.50000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                              2.50000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                               2.50000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                               1.16402
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                              2.50000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                              2.50000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                             1.03750
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                          2.50000
## `prdline.my.fctriPad 1:biddable`                                       0.80346
## `prdline.my.fctriPad 2:biddable`                                       0.70447
## `prdline.my.fctriPad 3+:biddable`                                      0.61975
## `prdline.my.fctriPadAir:biddable`                                      0.60637
## `prdline.my.fctriPadmini:biddable`                                     0.64099
## `prdline.my.fctriPadmini 2+:biddable`                                  0.66896
## `prdline.my.fctriPad 1:startprice.log`                                 1.09447
## `prdline.my.fctriPad 2:startprice.log`                                 0.27293
## `prdline.my.fctriPad 3+:startprice.log`                                0.27366
## `prdline.my.fctriPadAir:startprice.log`                                0.28784
## `prdline.my.fctriPadmini:startprice.log`                               0.26160
## `prdline.my.fctriPadmini 2+:startprice.log`                            0.35678
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`         1.34946
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`         1.02189
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`        0.86662
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`        1.06642
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`       0.82505
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`    1.64968
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`         2.50000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`         2.50000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`        1.26736
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`        1.25276
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`       1.22064
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`    1.99958
## `prdline.my.fctriPad 1:condition.fctrNew`                              2.40789
## `prdline.my.fctriPad 2:condition.fctrNew`                              2.50000
## `prdline.my.fctriPad 3+:condition.fctrNew`                             1.20645
## `prdline.my.fctriPadAir:condition.fctrNew`                             0.77268
## `prdline.my.fctriPadmini:condition.fctrNew`                            0.83983
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                         0.87796
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`          2.06552
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`          1.69700
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`         1.03824
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`         1.01826
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`        1.21484
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`     1.19130
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`               1.39647
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`               1.07029
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`              1.12839
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`              1.58722
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`             1.09680
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`          1.66678
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                            0.15321
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                            0.12281
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                           0.10832
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                           0.11295
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                          0.12149
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                       0.14677
## `prdline.my.fctriPad 1:cellular.fctr1`                                 0.86209
## `prdline.my.fctriPad 2:cellular.fctr1`                                 0.80848
## `prdline.my.fctriPad 3+:cellular.fctr1`                                0.73111
## `prdline.my.fctriPadAir:cellular.fctr1`                                0.72785
## `prdline.my.fctriPadmini:cellular.fctr1`                               0.76729
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                            0.84023
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                           1.19025
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                           1.13833
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                          1.04730
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                          1.19949
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                         0.94282
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                      0.92312
##                                                                     z value
## (Intercept)                                                           3.145
## `prdline.my.fctriPad 1`                                               3.587
## `prdline.my.fctriPad 2`                                              -0.417
## `prdline.my.fctriPad 3+`                                              1.640
## prdline.my.fctriPadAir                                                1.691
## prdline.my.fctriPadmini                                               1.180
## `prdline.my.fctriPadmini 2+`                                          1.223
## biddable                                                              3.569
## startprice.log                                                       -5.149
## `condition.fctrFor parts or not working`                              0.095
## `condition.fctrManufacturer refurbished`                             -0.071
## condition.fctrNew                                                    -1.703
## `condition.fctrNew other (see details)`                               0.919
## `condition.fctrSeller refurbished`                                   -0.722
## D.terms.n.post.stop                                                  -0.846
## cellular.fctr1                                                       -0.733
## cellular.fctrUnknown                                                 -1.020
## `prdline.my.fctrUnknown:.clusterid.fctr2`                             2.301
## `prdline.my.fctriPad 1:.clusterid.fctr2`                             -0.070
## `prdline.my.fctriPad 2:.clusterid.fctr2`                              0.518
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                            -0.820
## `prdline.my.fctriPadAir:.clusterid.fctr2`                            -0.731
## `prdline.my.fctriPadmini:.clusterid.fctr2`                            1.802
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                         0.893
## `prdline.my.fctrUnknown:.clusterid.fctr3`                            -0.428
## `prdline.my.fctriPad 1:.clusterid.fctr3`                             -1.532
## `prdline.my.fctriPad 2:.clusterid.fctr3`                             -1.369
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                             0.301
## `prdline.my.fctriPadAir:.clusterid.fctr3`                             0.712
## `prdline.my.fctriPadmini:.clusterid.fctr3`                            1.647
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                         0.147
## `prdline.my.fctrUnknown:.clusterid.fctr4`                             0.000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                             -2.197
## `prdline.my.fctriPad 2:.clusterid.fctr4`                              0.761
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                             1.490
## `prdline.my.fctriPadAir:.clusterid.fctr4`                            -0.332
## `prdline.my.fctriPadmini:.clusterid.fctr4`                            0.383
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                         0.000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                              0.000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                              2.496
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                             0.000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                           -0.380
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                         0.000
## `prdline.my.fctriPad 1:biddable`                                      0.338
## `prdline.my.fctriPad 2:biddable`                                      1.364
## `prdline.my.fctriPad 3+:biddable`                                    -0.662
## `prdline.my.fctriPadAir:biddable`                                     0.934
## `prdline.my.fctriPadmini:biddable`                                   -0.103
## `prdline.my.fctriPadmini 2+:biddable`                                -0.421
## `prdline.my.fctriPad 1:startprice.log`                               -3.519
## `prdline.my.fctriPad 2:startprice.log`                                0.719
## `prdline.my.fctriPad 3+:startprice.log`                              -0.640
## `prdline.my.fctriPadAir:startprice.log`                              -0.868
## `prdline.my.fctriPadmini:startprice.log`                             -0.430
## `prdline.my.fctriPadmini 2+:startprice.log`                          -0.767
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`       -0.585
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`        0.399
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`      -1.061
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`      -0.585
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`     -2.212
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working`  -0.699
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`        0.000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`        0.000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`      -0.435
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`       0.936
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`     -0.231
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished`  -0.177
## `prdline.my.fctriPad 1:condition.fctrNew`                            -0.017
## `prdline.my.fctriPad 2:condition.fctrNew`                             0.000
## `prdline.my.fctriPad 3+:condition.fctrNew`                           -0.421
## `prdline.my.fctriPadAir:condition.fctrNew`                            0.713
## `prdline.my.fctriPadmini:condition.fctrNew`                           1.053
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                        1.582
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`        -0.132
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`        -0.587
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`        0.224
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`        0.371
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`      -0.936
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`   -1.568
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`              1.000
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`              0.349
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`            -0.884
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`            -0.629
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`           -0.471
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`        -0.919
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                           1.371
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                           0.335
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                         -0.586
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                         -0.263
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                        -0.682
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                      0.094
## `prdline.my.fctriPad 1:cellular.fctr1`                                0.572
## `prdline.my.fctriPad 2:cellular.fctr1`                               -0.418
## `prdline.my.fctriPad 3+:cellular.fctr1`                               1.033
## `prdline.my.fctriPadAir:cellular.fctr1`                               0.288
## `prdline.my.fctriPadmini:cellular.fctr1`                              1.192
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                           1.029
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                          0.264
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                         -0.621
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                        -1.054
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                         0.062
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                        0.543
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                     0.197
##                                                                     Pr(>|z|)
## (Intercept)                                                         0.001663
## `prdline.my.fctriPad 1`                                             0.000334
## `prdline.my.fctriPad 2`                                             0.676907
## `prdline.my.fctriPad 3+`                                            0.100972
## prdline.my.fctriPadAir                                              0.090790
## prdline.my.fctriPadmini                                             0.237853
## `prdline.my.fctriPadmini 2+`                                        0.221187
## biddable                                                            0.000359
## startprice.log                                                      2.62e-07
## `condition.fctrFor parts or not working`                            0.924482
## `condition.fctrManufacturer refurbished`                            0.943795
## condition.fctrNew                                                   0.088513
## `condition.fctrNew other (see details)`                             0.358044
## `condition.fctrSeller refurbished`                                  0.470085
## D.terms.n.post.stop                                                 0.397717
## cellular.fctr1                                                      0.463632
## cellular.fctrUnknown                                                0.307856
## `prdline.my.fctrUnknown:.clusterid.fctr2`                           0.021375
## `prdline.my.fctriPad 1:.clusterid.fctr2`                            0.943992
## `prdline.my.fctriPad 2:.clusterid.fctr2`                            0.604733
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                           0.412042
## `prdline.my.fctriPadAir:.clusterid.fctr2`                           0.464599
## `prdline.my.fctriPadmini:.clusterid.fctr2`                          0.071580
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                       0.371990
## `prdline.my.fctrUnknown:.clusterid.fctr3`                           0.668596
## `prdline.my.fctriPad 1:.clusterid.fctr3`                            0.125604
## `prdline.my.fctriPad 2:.clusterid.fctr3`                            0.171004
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                           0.763186
## `prdline.my.fctriPadAir:.clusterid.fctr3`                           0.476500
## `prdline.my.fctriPadmini:.clusterid.fctr3`                          0.099594
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                       0.883198
## `prdline.my.fctrUnknown:.clusterid.fctr4`                           1.000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            0.027991
## `prdline.my.fctriPad 2:.clusterid.fctr4`                            0.446391
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                           0.136167
## `prdline.my.fctriPadAir:.clusterid.fctr4`                           0.739809
## `prdline.my.fctriPadmini:.clusterid.fctr4`                          0.701894
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                       1.000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`                           1.000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`                            1.000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`                            0.012546
## `prdline.my.fctriPad 3+:.clusterid.fctr5`                           1.000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`                           1.000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`                          0.704005
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`                       1.000000
## `prdline.my.fctriPad 1:biddable`                                    0.735089
## `prdline.my.fctriPad 2:biddable`                                    0.172677
## `prdline.my.fctriPad 3+:biddable`                                   0.508113
## `prdline.my.fctriPadAir:biddable`                                   0.350466
## `prdline.my.fctriPadmini:biddable`                                  0.918085
## `prdline.my.fctriPadmini 2+:biddable`                               0.673692
## `prdline.my.fctriPad 1:startprice.log`                              0.000434
## `prdline.my.fctriPad 2:startprice.log`                              0.472353
## `prdline.my.fctriPad 3+:startprice.log`                             0.522404
## `prdline.my.fctriPadAir:startprice.log`                             0.385542
## `prdline.my.fctriPadmini:startprice.log`                            0.667135
## `prdline.my.fctriPadmini 2+:startprice.log`                         0.443036
## `prdline.my.fctriPad 1:condition.fctrFor parts or not working`      0.558757
## `prdline.my.fctriPad 2:condition.fctrFor parts or not working`      0.689933
## `prdline.my.fctriPad 3+:condition.fctrFor parts or not working`     0.288654
## `prdline.my.fctriPadAir:condition.fctrFor parts or not working`     0.558433
## `prdline.my.fctriPadmini:condition.fctrFor parts or not working`    0.026999
## `prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working` 0.484631
## `prdline.my.fctriPad 1:condition.fctrManufacturer refurbished`      1.000000
## `prdline.my.fctriPad 2:condition.fctrManufacturer refurbished`      1.000000
## `prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished`     0.663805
## `prdline.my.fctriPadAir:condition.fctrManufacturer refurbished`     0.349024
## `prdline.my.fctriPadmini:condition.fctrManufacturer refurbished`    0.816981
## `prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished` 0.859732
## `prdline.my.fctriPad 1:condition.fctrNew`                           0.986804
## `prdline.my.fctriPad 2:condition.fctrNew`                           1.000000
## `prdline.my.fctriPad 3+:condition.fctrNew`                          0.673507
## `prdline.my.fctriPadAir:condition.fctrNew`                          0.475616
## `prdline.my.fctriPadmini:condition.fctrNew`                         0.292251
## `prdline.my.fctriPadmini 2+:condition.fctrNew`                      0.113698
## `prdline.my.fctriPad 1:condition.fctrNew other (see details)`       0.894998
## `prdline.my.fctriPad 2:condition.fctrNew other (see details)`       0.557246
## `prdline.my.fctriPad 3+:condition.fctrNew other (see details)`      0.822594
## `prdline.my.fctriPadAir:condition.fctrNew other (see details)`      0.710407
## `prdline.my.fctriPadmini:condition.fctrNew other (see details)`     0.349051
## `prdline.my.fctriPadmini 2+:condition.fctrNew other (see details)`  0.116797
## `prdline.my.fctriPad 1:condition.fctrSeller refurbished`            0.317345
## `prdline.my.fctriPad 2:condition.fctrSeller refurbished`            0.726759
## `prdline.my.fctriPad 3+:condition.fctrSeller refurbished`           0.376946
## `prdline.my.fctriPadAir:condition.fctrSeller refurbished`           0.529566
## `prdline.my.fctriPadmini:condition.fctrSeller refurbished`          0.637653
## `prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished`       0.358343
## `prdline.my.fctriPad 1:D.terms.n.post.stop`                         0.170442
## `prdline.my.fctriPad 2:D.terms.n.post.stop`                         0.737715
## `prdline.my.fctriPad 3+:D.terms.n.post.stop`                        0.557732
## `prdline.my.fctriPadAir:D.terms.n.post.stop`                        0.792264
## `prdline.my.fctriPadmini:D.terms.n.post.stop`                       0.495470
## `prdline.my.fctriPadmini 2+:D.terms.n.post.stop`                    0.925022
## `prdline.my.fctriPad 1:cellular.fctr1`                              0.567162
## `prdline.my.fctriPad 2:cellular.fctr1`                              0.675910
## `prdline.my.fctriPad 3+:cellular.fctr1`                             0.301827
## `prdline.my.fctriPadAir:cellular.fctr1`                             0.773672
## `prdline.my.fctriPadmini:cellular.fctr1`                            0.233263
## `prdline.my.fctriPadmini 2+:cellular.fctr1`                         0.303343
## `prdline.my.fctriPad 1:cellular.fctrUnknown`                        0.791614
## `prdline.my.fctriPad 2:cellular.fctrUnknown`                        0.534663
## `prdline.my.fctriPad 3+:cellular.fctrUnknown`                       0.291885
## `prdline.my.fctriPadAir:cellular.fctrUnknown`                       0.950644
## `prdline.my.fctriPadmini:cellular.fctrUnknown`                      0.587423
## `prdline.my.fctriPadmini 2+:cellular.fctrUnknown`                   0.843823
##                                                                        
## (Intercept)                                                         ** 
## `prdline.my.fctriPad 1`                                             ***
## `prdline.my.fctriPad 2`                                                
## `prdline.my.fctriPad 3+`                                               
## prdline.my.fctriPadAir                                              .  
## prdline.my.fctriPadmini                                                
## `prdline.my.fctriPadmini 2+`                                           
## biddable                                                            ***
## startprice.log                                                      ***
## `condition.fctrFor parts or not working`                               
## `condition.fctrManufacturer refurbished`                               
## condition.fctrNew                                                   .  
## `condition.fctrNew other (see details)`                                
## `condition.fctrSeller refurbished`                                     
## D.terms.n.post.stop                                                    
## cellular.fctr1                                                         
## cellular.fctrUnknown                                                   
## `prdline.my.fctrUnknown:.clusterid.fctr2`                           *  
## `prdline.my.fctriPad 1:.clusterid.fctr2`                               
## `prdline.my.fctriPad 2:.clusterid.fctr2`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr2`                              
## `prdline.my.fctriPadAir:.clusterid.fctr2`                              
## `prdline.my.fctriPadmini:.clusterid.fctr2`                          .  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`                          
## `prdline.my.fctrUnknown:.clusterid.fctr3`                              
## `prdline.my.fctriPad 1:.clusterid.fctr3`                               
## `prdline.my.fctriPad 2:.clusterid.fctr3`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr3`                              
## `prdline.my.fctriPadAir:.clusterid.fctr3`                              
## `prdline.my.fctriPadmini:.clusterid.fctr3`                          .  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`                          
## `prdline.my.fctrUnknown:.clusterid.fctr4`                              
## `prdline.my.fctriPad 1:.clusterid.fctr4`                            *  
## `prdline.my.fctriPad 2:.clusterid.fctr4`                               
## `prdline.my.fctriPad 3+:.clusterid.fctr4`                              
## `prdline.my.fctriPadAir:.clusterid.fctr4`                              
## `prdline.my.fctriPadmini:.clusterid.fctr4`                             
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`                          
## `prdline.my.fctrUnknown:.clusterid.fctr5`                              
## `prdline.my.fctriPad 1:.clusterid.fctr5`                               
## `prdline.my.fctriPad 2:.clusterid.fctr5`                            *  
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
## `prdline.my.fctriPad 1:startprice.log`                              ***
## `prdline.my.fctriPad 2:startprice.log`                                 
## `prdline.my.fctriPad 3+:startprice.log`                                
## `prdline.my.fctriPadAir:startprice.log`                                
## `prdline.my.fctriPadmini:startprice.log`                               
## `prdline.my.fctriPadmini 2+:startprice.log`                            
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
## Residual deviance:  757.81  on 869  degrees of freedom
## AIC: 967.81
## 
## Number of Fisher Scoring iterations: 33
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-69.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6879875
## 3        0.2 0.7493261
## 4        0.3 0.7954779
## 5        0.4 0.8021858
## 6        0.5 0.8060748
## 7        0.6 0.7955390
## 8        0.7 0.7509987
## 9        0.8 0.6884780
## 10       0.9 0.5276873
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-70.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.bayesglm.N
## 1         N                              463
## 2         Y                              105
##   sold.fctr.predict.csm.bayesglm.Y
## 1                               61
## 2                              345
##          Prediction
## Reference   N   Y
##         N 463  61
##         Y 105 345
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.295688e-01   6.547751e-01   8.044545e-01   8.526716e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   3.767982e-82   8.455177e-04 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-71.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6747615
## 3        0.2 0.7062374
## 4        0.3 0.7346939
## 5        0.4 0.7478685
## 6        0.5 0.7525773
## 7        0.6 0.7570470
## 8        0.7 0.7238913
## 9        0.8 0.6388443
## 10       0.9 0.5017921
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-72.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.bayesglm.N
## 1         N                              422
## 2         Y                              128
##   sold.fctr.predict.csm.bayesglm.Y
## 1                               53
## 2                              282
##          Prediction
## Reference   N   Y
##         N 422  53
##         Y 128 282
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.954802e-01   5.835294e-01   7.673772e-01   8.215999e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.462035e-58   3.789807e-08 
##       model_id model_method
## 1 csm.bayesglm     bayesglm
##                                                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.908                 0.852
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8986853                    0.5       0.8060748        0.7741247
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8044545             0.8526716     0.5443023   0.8334377
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6        0.757047        0.7954802
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7673772             0.8215999     0.5835294    967.8072
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002182198     0.005351267
##                              N importance
## startprice.log      100.000000 100.000000
## biddable             94.720022  94.720022
## prdline.my.fctr      32.302503  32.302503
## .clusterid.fctr      11.462598  11.462598
## D.terms.n.post.stop   6.242874   6.242874
## cellular.fctr         4.844607   4.844607
## [1] "fitting model: csm.glmnet"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 1, lambda = 0.00544 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: alpha
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-73.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-74.png) 

```
##             Length Class      Mode     
## a0            89   -none-     numeric  
## beta        9256   dgCMatrix  S4       
## df            89   -none-     numeric  
## dim            2   -none-     numeric  
## lambda        89   -none-     numeric  
## dev.ratio     89   -none-     numeric  
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
##                                                       (Intercept) 
##                                                       2.848504687 
##                                            prdline.my.fctriPad 3+ 
##                                                       0.189351927 
##                                            prdline.my.fctriPadAir 
##                                                       0.088370786 
##                                           prdline.my.fctriPadmini 
##                                                       0.002838343 
##                                                          biddable 
##                                                       1.547825609 
##                                                    startprice.log 
##                                                      -0.730345460 
##                                                 condition.fctrNew 
##                                                      -0.385177218 
##                                  condition.fctrSeller refurbished 
##                                                      -0.172303623 
##                                               D.terms.n.post.stop 
##                                                      -0.037387670 
##                                              cellular.fctrUnknown 
##                                                      -0.871603875 
##                           prdline.my.fctrUnknown:.clusterid.fctr2 
##                                                       0.902632143 
##                            prdline.my.fctriPad 1:.clusterid.fctr2 
##                                                       0.568678396 
##                           prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                                      -0.364341797 
##                           prdline.my.fctriPadAir:.clusterid.fctr2 
##                                                      -0.440287912 
##                          prdline.my.fctriPadmini:.clusterid.fctr2 
##                                                       0.625294489 
##                       prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                                       0.481795395 
##                           prdline.my.fctrUnknown:.clusterid.fctr3 
##                                                      -0.870653991 
##                            prdline.my.fctriPad 2:.clusterid.fctr3 
##                                                      -1.959659830 
##                           prdline.my.fctriPadAir:.clusterid.fctr3 
##                                                       0.086477019 
##                          prdline.my.fctriPadmini:.clusterid.fctr3 
##                                                       0.715910778 
##                            prdline.my.fctriPad 1:.clusterid.fctr4 
##                                                      -1.670445683 
##                            prdline.my.fctriPad 2:.clusterid.fctr4 
##                                                       0.094319774 
##                           prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                                       0.719184358 
##                            prdline.my.fctriPad 2:.clusterid.fctr5 
##                                                       2.367110785 
##                          prdline.my.fctriPadmini:.clusterid.fctr5 
##                                                      -0.479616575 
##                                    prdline.my.fctriPad 1:biddable 
##                                                       0.721023741 
##                                    prdline.my.fctriPad 2:biddable 
##                                                       0.496494172 
##                                   prdline.my.fctriPadAir:biddable 
##                                                       0.686241705 
##                              prdline.my.fctriPad 1:startprice.log 
##                                                      -0.076018145 
##      prdline.my.fctriPad 1:condition.fctrFor parts or not working 
##                                                       1.648562952 
##     prdline.my.fctriPad 3+:condition.fctrFor parts or not working 
##                                                      -0.391601148 
##    prdline.my.fctriPadmini:condition.fctrFor parts or not working 
##                                                      -1.146633724 
## prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working 
##                                                      -0.923790188 
##     prdline.my.fctriPadAir:condition.fctrManufacturer refurbished 
##                                                       0.984570352 
##                           prdline.my.fctriPad 1:condition.fctrNew 
##                                                      -1.114956375 
##                          prdline.my.fctriPad 3+:condition.fctrNew 
##                                                      -0.501319019 
##                         prdline.my.fctriPadmini:condition.fctrNew 
##                                                       0.189869265 
##                      prdline.my.fctriPadmini 2+:condition.fctrNew 
##                                                       0.388695582 
##       prdline.my.fctriPad 2:condition.fctrNew other (see details) 
##                                                      -0.549015553 
##      prdline.my.fctriPad 3+:condition.fctrNew other (see details) 
##                                                       0.788782876 
##      prdline.my.fctriPadAir:condition.fctrNew other (see details) 
##                                                       0.771855898 
##     prdline.my.fctriPadmini:condition.fctrNew other (see details) 
##                                                      -0.361942670 
##  prdline.my.fctriPadmini 2+:condition.fctrNew other (see details) 
##                                                      -0.901732503 
##            prdline.my.fctriPad 1:condition.fctrSeller refurbished 
##                                                       1.019786033 
##           prdline.my.fctriPad 3+:condition.fctrSeller refurbished 
##                                                      -0.834573827 
##           prdline.my.fctriPadAir:condition.fctrSeller refurbished 
##                                                      -0.313626167 
##          prdline.my.fctriPadmini:condition.fctrSeller refurbished 
##                                                      -0.512140284 
##       prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished 
##                                                      -1.846784098 
##                         prdline.my.fctriPad 1:D.terms.n.post.stop 
##                                                       0.004391011 
##                        prdline.my.fctriPad 3+:D.terms.n.post.stop 
##                                                      -0.040371331 
##                        prdline.my.fctriPadAir:D.terms.n.post.stop 
##                                                      -0.005268821 
##                       prdline.my.fctriPadmini:D.terms.n.post.stop 
##                                                      -0.010333943 
##                              prdline.my.fctriPad 2:cellular.fctr1 
##                                                      -0.488628382 
##                             prdline.my.fctriPad 3+:cellular.fctr1 
##                                                       0.162254018 
##                            prdline.my.fctriPadmini:cellular.fctr1 
##                                                       0.332513947 
##                         prdline.my.fctriPadmini 2+:cellular.fctr1 
##                                                       0.097971458 
##                       prdline.my.fctriPad 3+:cellular.fctrUnknown 
##                                                      -0.382720281 
##                      prdline.my.fctriPadmini:cellular.fctrUnknown 
##                                                       0.357739071 
##                   prdline.my.fctriPadmini 2+:cellular.fctrUnknown 
##                                                       0.068659403 
## [1] "max lambda < lambdaOpt:"
##                                                       (Intercept) 
##                                                       1.432858332 
##                                             prdline.my.fctriPad 1 
##                                                      29.867450298 
##                                             prdline.my.fctriPad 2 
##                                                      -0.152687173 
##                                            prdline.my.fctriPad 3+ 
##                                                       6.492223574 
##                                            prdline.my.fctriPadAir 
##                                                      10.244442353 
##                                           prdline.my.fctriPadmini 
##                                                       4.622933958 
##                                        prdline.my.fctriPadmini 2+ 
##                                                      16.516619817 
##                                                          biddable 
##                                                       1.992948749 
##                                                    startprice.log 
##                                                      -0.688382078 
##                            condition.fctrFor parts or not working 
##                                                       0.763039507 
##                                                 condition.fctrNew 
##                                                      -0.999948455 
##                             condition.fctrNew other (see details) 
##                                                       3.056431395 
##                                  condition.fctrSeller refurbished 
##                                                       0.174447875 
##                                               D.terms.n.post.stop 
##                                                      -0.084263218 
##                                                    cellular.fctr1 
##                                                      -2.003805334 
##                                              cellular.fctrUnknown 
##                                                      -0.570578216 
##                           prdline.my.fctrUnknown:.clusterid.fctr2 
##                                                       2.259939340 
##                            prdline.my.fctriPad 1:.clusterid.fctr2 
##                                                      -2.864829168 
##                            prdline.my.fctriPad 2:.clusterid.fctr2 
##                                                       0.704716958 
##                           prdline.my.fctriPad 3+:.clusterid.fctr2 
##                                                      -0.707210722 
##                           prdline.my.fctriPadAir:.clusterid.fctr2 
##                                                      -0.843692906 
##                          prdline.my.fctriPadmini:.clusterid.fctr2 
##                                                       2.281663963 
##                       prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                                       2.791698774 
##                           prdline.my.fctrUnknown:.clusterid.fctr3 
##                                                      -0.264690999 
##                            prdline.my.fctriPad 1:.clusterid.fctr3 
##                                                      -6.236512436 
##                            prdline.my.fctriPad 2:.clusterid.fctr3 
##                                                      -6.314726183 
##                           prdline.my.fctriPad 3+:.clusterid.fctr3 
##                                                       0.234153042 
##                           prdline.my.fctriPadAir:.clusterid.fctr3 
##                                                       0.738115293 
##                          prdline.my.fctriPadmini:.clusterid.fctr3 
##                                                       2.656724015 
##                       prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                                       1.055860040 
##                            prdline.my.fctriPad 1:.clusterid.fctr4 
##                                                      -6.408983977 
##                            prdline.my.fctriPad 2:.clusterid.fctr4 
##                                                       1.298426406 
##                           prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                                       1.262327023 
##                           prdline.my.fctriPadAir:.clusterid.fctr4 
##                                                      -0.419898001 
##                          prdline.my.fctriPadmini:.clusterid.fctr4 
##                                                       1.362484083 
##                            prdline.my.fctriPad 2:.clusterid.fctr5 
##                                                       3.563063102 
##                          prdline.my.fctriPadmini:.clusterid.fctr5 
##                                                       0.003057804 
##                                    prdline.my.fctriPad 2:biddable 
##                                                       0.933037249 
##                                   prdline.my.fctriPad 3+:biddable 
##                                                      -0.953446480 
##                                   prdline.my.fctriPadAir:biddable 
##                                                       0.008199769 
##                                  prdline.my.fctriPadmini:biddable 
##                                                      -0.507839196 
##                               prdline.my.fctriPadmini 2+:biddable 
##                                                      -1.064738110 
##                              prdline.my.fctriPad 1:startprice.log 
##                                                      -6.506069411 
##                              prdline.my.fctriPad 2:startprice.log 
##                                                       0.153126306 
##                             prdline.my.fctriPad 3+:startprice.log 
##                                                      -0.859612651 
##                             prdline.my.fctriPadAir:startprice.log 
##                                                      -1.458172886 
##                            prdline.my.fctriPadmini:startprice.log 
##                                                      -0.629426563 
##                         prdline.my.fctriPadmini 2+:startprice.log 
##                                                      -2.800900682 
##      prdline.my.fctriPad 1:condition.fctrFor parts or not working 
##                                                      -4.872820448 
##      prdline.my.fctriPad 2:condition.fctrFor parts or not working 
##                                                       0.016178274 
##     prdline.my.fctriPad 3+:condition.fctrFor parts or not working 
##                                                      -2.039058620 
##     prdline.my.fctriPadAir:condition.fctrFor parts or not working 
##                                                      -2.320701242 
##    prdline.my.fctriPadmini:condition.fctrFor parts or not working 
##                                                      -2.986563035 
## prdline.my.fctriPadmini 2+:condition.fctrFor parts or not working 
##                                                      -7.204673336 
##     prdline.my.fctriPad 3+:condition.fctrManufacturer refurbished 
##                                                      -0.901649608 
##     prdline.my.fctriPadAir:condition.fctrManufacturer refurbished 
##                                                       1.261147532 
##    prdline.my.fctriPadmini:condition.fctrManufacturer refurbished 
##                                                      -0.433323571 
## prdline.my.fctriPadmini 2+:condition.fctrManufacturer refurbished 
##                                                      -1.801978902 
##                          prdline.my.fctriPad 3+:condition.fctrNew 
##                                                      -0.994598321 
##                          prdline.my.fctriPadAir:condition.fctrNew 
##                                                       0.773865759 
##                         prdline.my.fctriPadmini:condition.fctrNew 
##                                                       0.931003056 
##                      prdline.my.fctriPadmini 2+:condition.fctrNew 
##                                                       2.285733337 
##       prdline.my.fctriPad 1:condition.fctrNew other (see details) 
##                                                      -4.322236148 
##       prdline.my.fctriPad 2:condition.fctrNew other (see details) 
##                                                      -7.349271332 
##      prdline.my.fctriPad 3+:condition.fctrNew other (see details) 
##                                                      -1.958137969 
##      prdline.my.fctriPadAir:condition.fctrNew other (see details) 
##                                                      -1.603547800 
##     prdline.my.fctriPadmini:condition.fctrNew other (see details) 
##                                                      -3.856482243 
##  prdline.my.fctriPadmini 2+:condition.fctrNew other (see details) 
##                                                      -4.297795984 
##            prdline.my.fctriPad 1:condition.fctrSeller refurbished 
##                                                       1.412104142 
##            prdline.my.fctriPad 2:condition.fctrSeller refurbished 
##                                                      -0.279934091 
##           prdline.my.fctriPad 3+:condition.fctrSeller refurbished 
##                                                      -2.068651747 
##           prdline.my.fctriPadAir:condition.fctrSeller refurbished 
##                                                      -5.092285355 
##          prdline.my.fctriPadmini:condition.fctrSeller refurbished 
##                                                      -1.367503333 
##       prdline.my.fctriPadmini 2+:condition.fctrSeller refurbished 
##                                                      -7.505407362 
##                         prdline.my.fctriPad 1:D.terms.n.post.stop 
##                                                       0.561419790 
##                         prdline.my.fctriPad 2:D.terms.n.post.stop 
##                                                       0.036795622 
##                        prdline.my.fctriPad 3+:D.terms.n.post.stop 
##                                                      -0.053610191 
##                        prdline.my.fctriPadAir:D.terms.n.post.stop 
##                                                      -0.020230321 
##                       prdline.my.fctriPadmini:D.terms.n.post.stop 
##                                                      -0.174360343 
##                    prdline.my.fctriPadmini 2+:D.terms.n.post.stop 
##                                                      -0.083730109 
##                              prdline.my.fctriPad 1:cellular.fctr1 
##                                                       2.357135425 
##                              prdline.my.fctriPad 2:cellular.fctr1 
##                                                       1.106562272 
##                             prdline.my.fctriPad 3+:cellular.fctr1 
##                                                       2.420911797 
##                             prdline.my.fctriPadAir:cellular.fctr1 
##                                                       1.840342247 
##                            prdline.my.fctriPadmini:cellular.fctr1 
##                                                       2.595336499 
##                         prdline.my.fctriPadmini 2+:cellular.fctr1 
##                                                       3.148364088 
##                        prdline.my.fctriPad 1:cellular.fctrUnknown 
##                                                       1.504694159 
##                        prdline.my.fctriPad 2:cellular.fctrUnknown 
##                                                      -0.953340255 
##                       prdline.my.fctriPad 3+:cellular.fctrUnknown 
##                                                      -1.827813715 
##                       prdline.my.fctriPadAir:cellular.fctrUnknown 
##                                                      -0.279804606 
##                      prdline.my.fctriPadmini:cellular.fctrUnknown 
##                                                       0.736707775 
##                   prdline.my.fctriPadmini 2+:cellular.fctrUnknown 
##                                                       0.503675307 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-75.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6597786
## 3        0.2 0.7450980
## 4        0.3 0.7862209
## 5        0.4 0.7818383
## 6        0.5 0.7883721
## 7        0.6 0.7823961
## 8        0.7 0.7316422
## 9        0.8 0.6055046
## 10       0.9 0.4195804
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-76.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.glmnet.N sold.fctr.predict.csm.glmnet.Y
## 1         N                            453                             71
## 2         Y                            111                            339
##          Prediction
## Reference   N   Y
##         N 453  71
##         Y 111 339
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.131417e-01   6.217395e-01   7.872153e-01   8.371587e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.324224e-72   3.841741e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-77.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6529943
## 3        0.2 0.7000983
## 4        0.3 0.7366021
## 5        0.4 0.7521578
## 6        0.5 0.7633588
## 7        0.6 0.7682119
## 8        0.7 0.6960352
## 9        0.8 0.5950413
## 10       0.9 0.4168260
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-78.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.glmnet.N sold.fctr.predict.csm.glmnet.Y
## 1         N                            420                             55
## 2         Y                            120                            290
##          Prediction
## Reference   N   Y
##         N 420  55
##         Y 120 290
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.022599e-01   5.980144e-01   7.744658e-01   8.280245e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   2.734441e-61   1.311883e-06 
##     model_id model_method
## 1 csm.glmnet       glmnet
##                                                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                       8.63                 2.512
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8842006                    0.5       0.7883721        0.7782431
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7872153             0.8371587     0.5524382   0.8337946
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7682119        0.8022599
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7744658             0.8280245     0.5980144
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01855776       0.0408953
##                                                               importance
## prdline.my.fctriPad 2:.clusterid.fctr5                         100.00000
## prdline.my.fctriPad 1:condition.fctrFor parts or not working    83.30323
## biddable                                                        80.97557
## prdline.my.fctriPad 1:condition.fctrSeller refurbished          68.86457
## prdline.my.fctriPadAir:condition.fctrManufacturer refurbished   68.02039
## prdline.my.fctrUnknown:.clusterid.fctr2                         66.18564
## [1] "fitting model: csm.rpart"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00833 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-79.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-80.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 974 
## 
##            CP nsplit rel error
## 1 0.511111111      0 1.0000000
## 2 0.027777778      1 0.4888889
## 3 0.008333333      3 0.4333333
## 
## Variable importance
##                                 biddable 
##                                       35 
##                           startprice.log 
##                                       25 
##          prdline.my.fctriPadAir:biddable 
##                                        9 
##          prdline.my.fctriPad 3+:biddable 
##                                        7 
##           prdline.my.fctriPad 1:biddable 
##                                        6 
##         prdline.my.fctriPadmini:biddable 
##                                        6 
##    prdline.my.fctriPadAir:startprice.log 
##                                        5 
##                   prdline.my.fctriPadAir 
##                                        3 
##    prdline.my.fctriPad 3+:startprice.log 
##                                        2 
##                        condition.fctrNew 
##                                        2 
## prdline.my.fctriPadAir:condition.fctrNew 
##                                        1 
## 
## Node number 1: 974 observations,    complexity param=0.5111111
##   predicted class=N  expected loss=0.4620123  P(node) =1
##     class counts:   524   450
##    probabilities: 0.538 0.462 
##   left son=2 (524 obs) right son=3 (450 obs)
##   Primary splits:
##       biddable                              < 0.5      to the left,  improve=144.14990, (0 missing)
##       startprice.log                        < 4.610145 to the right, improve=117.21260, (0 missing)
##       prdline.my.fctriPad 1:biddable        < 0.5      to the left,  improve= 25.37925, (0 missing)
##       prdline.my.fctriPad 2:biddable        < 0.5      to the left,  improve= 22.83949, (0 missing)
##       prdline.my.fctriPadAir:startprice.log < 5.768305 to the right, improve= 17.62175, (0 missing)
##   Surrogate splits:
##       startprice.log                   < 5.030417 to the right, agree=0.751, adj=0.460, (0 split)
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
## Node number 3: 450 observations,    complexity param=0.02777778
##   predicted class=Y  expected loss=0.2444444  P(node) =0.4620123
##     class counts:   110   340
##    probabilities: 0.244 0.756 
##   left son=6 (162 obs) right son=7 (288 obs)
##   Primary splits:
##       startprice.log                            < 4.863673 to the right, improve=38.027780, (0 missing)
##       prdline.my.fctriPadmini 2+:startprice.log < 6.024113 to the right, improve= 8.118385, (0 missing)
##       prdline.my.fctriPad 3+:startprice.log     < 5.23369  to the right, improve= 8.059102, (0 missing)
##       prdline.my.fctriPadmini:startprice.log    < 5.176086 to the right, improve= 7.628118, (0 missing)
##       prdline.my.fctriPad 1:startprice.log      < 4.467899 to the right, improve= 6.476747, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir:startprice.log < 4.79078  to the right, agree=0.760, adj=0.333, (0 split)
##       prdline.my.fctriPad 3+:startprice.log < 4.935157 to the right, agree=0.716, adj=0.210, (0 split)
##       prdline.my.fctriPadAir                < 0.5      to the right, agree=0.707, adj=0.185, (0 split)
##       condition.fctrNew                     < 0.5      to the right, agree=0.707, adj=0.185, (0 split)
##       prdline.my.fctriPadAir:biddable       < 0.5      to the right, agree=0.707, adj=0.185, (0 split)
## 
## Node number 6: 162 observations,    complexity param=0.02777778
##   predicted class=N  expected loss=0.4814815  P(node) =0.1663244
##     class counts:    84    78
##    probabilities: 0.519 0.481 
##   left son=12 (109 obs) right son=13 (53 obs)
##   Primary splits:
##       prdline.my.fctriPadAir:startprice.log     < 4.993479 to the left,  improve=6.161522, (0 missing)
##       prdline.my.fctriPadAir                    < 0.5      to the left,  improve=5.555556, (0 missing)
##       prdline.my.fctriPadAir:biddable           < 0.5      to the left,  improve=5.555556, (0 missing)
##       D.terms.n.post.stop                       < 0.5      to the right, improve=3.555556, (0 missing)
##       prdline.my.fctriPadmini 2+:startprice.log < 6.024113 to the right, improve=3.392115, (0 missing)
##   Surrogate splits:
##       prdline.my.fctriPadAir                     < 0.5      to the left,  agree=0.994, adj=0.981, (0 split)
##       prdline.my.fctriPadAir:biddable            < 0.5      to the left,  agree=0.994, adj=0.981, (0 split)
##       prdline.my.fctriPadAir:condition.fctrNew   < 0.5      to the left,  agree=0.784, adj=0.340, (0 split)
##       prdline.my.fctriPadAir:D.terms.n.post.stop < 1        to the left,  agree=0.765, adj=0.283, (0 split)
##       prdline.my.fctriPadAir:cellular.fctr1      < 0.5      to the left,  agree=0.753, adj=0.245, (0 split)
## 
## Node number 7: 288 observations
##   predicted class=Y  expected loss=0.09027778  P(node) =0.2956879
##     class counts:    26   262
##    probabilities: 0.090 0.910 
## 
## Node number 12: 109 observations
##   predicted class=N  expected loss=0.3853211  P(node) =0.1119097
##     class counts:    67    42
##    probabilities: 0.615 0.385 
## 
## Node number 13: 53 observations
##   predicted class=Y  expected loss=0.3207547  P(node) =0.05441478
##     class counts:    17    36
##    probabilities: 0.321 0.679 
## 
## n= 974 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 974 450 N (0.53798768 0.46201232)  
##    2) biddable< 0.5 524 110 N (0.79007634 0.20992366) *
##    3) biddable>=0.5 450 110 Y (0.24444444 0.75555556)  
##      6) startprice.log>=4.863673 162  78 N (0.51851852 0.48148148)  
##       12) prdline.my.fctriPadAir:startprice.log< 4.993479 109  42 N (0.61467890 0.38532110) *
##       13) prdline.my.fctriPadAir:startprice.log>=4.993479 53  17 Y (0.32075472 0.67924528) *
##      7) startprice.log< 4.863673 288  26 Y (0.09027778 0.90972222) *
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-81.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6320225
## 3        0.2 0.6320225
## 4        0.3 0.7555556
## 5        0.4 0.7534766
## 6        0.5 0.7534766
## 7        0.6 0.7534766
## 8        0.7 0.7100271
## 9        0.8 0.7100271
## 10       0.9 0.7100271
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-82.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-83.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6332046
## 3        0.2 0.6332046
## 4        0.3 0.7528231
## 5        0.4 0.7233429
## 6        0.5 0.7233429
## 7        0.6 0.7233429
## 8        0.7 0.6818874
## 9        0.8 0.6818874
## 10       0.9 0.6818874
## 11       1.0 0.0000000
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-84.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
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
##                                                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.765                 0.075
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8187829                    0.3       0.7555556        0.7946787
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7465456             0.8000405     0.5816711   0.8090783
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7528231        0.7774011
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7485294             0.8044124      0.550663
##   max.AccuracySD.fit max.KappaSD.fit
## 1            0.01547      0.03231746
##                                           importance
## startprice.log                            100.000000
## biddable                                   92.855906
## prdline.my.fctriPad 1:biddable             16.348351
## prdline.my.fctriPadAir:startprice.log      15.320282
## prdline.my.fctriPad 2:biddable             14.712336
## prdline.my.fctriPadmini 2+:startprice.log   7.414628
## [1] "fitting model: csm.rf"
## [1] "    indep_vars: prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 104 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-85.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_1-86.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_1-87.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8372093
## 3        0.2 0.9268795
## 4        0.3 0.9667025
## 5        0.4 0.9878988
## 6        0.5 0.9877642
## 7        0.6 0.9866071
## 8        0.7 0.9188544
## 9        0.8 0.8520408
## 10       0.9 0.7821380
## 11       1.0 0.4771574
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-88.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.csm.rf.N sold.fctr.predict.csm.rf.Y
## 1         N                        514                         10
## 2         Y                          1                        449
##          Prediction
## Reference   N   Y
##         N 514  10
##         Y   1 449
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.887064e-01   9.773136e-01   9.798826e-01   9.943492e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##  1.991248e-238   1.586133e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-89.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6905660
## 3        0.2 0.7246073
## 4        0.3 0.7482993
## 5        0.4 0.7435294
## 6        0.5 0.7568922
## 7        0.6 0.7463479
## 8        0.7 0.7398031
## 9        0.8 0.6994048
## 10       0.9 0.6800000
## 11       1.0 0.4130019
```

![](ebayipads_csmmdl_files/figure-html/fit.models_1-90.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.csm.rf.N sold.fctr.predict.csm.rf.Y
## 1         N                        389                         86
## 2         Y                        108                        302
##          Prediction
## Reference   N   Y
##         N 389  86
##         Y 108 302
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.807910e-01   5.575798e-01   7.520575e-01   8.076410e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.749595e-51   1.316282e-01 
##   model_id model_method
## 1   csm.rf           rf
##                                                                                                                                                                                                            feats
## 1 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     20.796                 8.175
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.998503                    0.4       0.9878988        0.7915828
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9798826             0.9943492     0.5791514   0.8329551
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7568922         0.780791
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7520575              0.807641     0.5575798
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009337702      0.02083542
##                                        importance
## biddable                               100.000000
## startprice.log                          87.231371
## prdline.my.fctriPadAir:startprice.log   15.574956
## D.terms.n.post.stop                     14.332489
## prdline.my.fctriPadmini:startprice.log  10.832101
## prdline.my.fctriPad 3+:startprice.log    9.489204
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             biddable, startprice.log
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        biddable, startprice.log
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.log
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.log
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                                                                                                                                biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glm                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glm            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.bayesglm       D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glmnet         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rpart         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rf            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## csm.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.bayesglm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.glmnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rf                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##                               max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                           0                      0.405
## Random.myrandom_classfr                     0                      0.265
## Max.cor.Y.cv.0.rpart                        0                      0.631
## Max.cor.Y.cv.0.cp.0.rpart                   0                      0.516
## Max.cor.Y.rpart                             3                      1.046
## Max.cor.Y.glm                               1                      1.041
## Interact.High.cor.Y.glm                     1                      1.016
## Low.cor.X.glm                               1                      1.434
## All.X.glm                                   1                      1.681
## All.X.bayesglm                              1                      2.972
## All.X.glmnet                                9                      6.678
## All.X.no.rnorm.rpart                        3                      1.656
## All.X.no.rnorm.rf                           3                     16.938
## All.Interact.X.glm                          1                      1.823
## All.Interact.X.bayesglm                     1                      3.625
## All.Interact.X.glmnet                       9                      8.001
## All.Interact.X.no.rnorm.rpart               3                      1.749
## All.Interact.X.no.rnorm.rf                  3                     18.239
## csm.glm                                     1                      1.823
## csm.bayesglm                                1                      3.908
## csm.glmnet                                  9                      8.630
## csm.rpart                                   3                      1.765
## csm.rf                                      3                     20.796
##                               min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                             0.002   0.5000000
## Random.myrandom_classfr                       0.001   0.5071756
## Max.cor.Y.cv.0.rpart                          0.013   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                     0.010   0.8781425
## Max.cor.Y.rpart                               0.012   0.8172434
## Max.cor.Y.glm                                 0.015   0.8402290
## Interact.High.cor.Y.glm                       0.017   0.8427735
## Low.cor.X.glm                                 0.199   0.8858227
## All.X.glm                                     0.243   0.8949406
## All.X.bayesglm                                0.435   0.8909075
## All.X.glmnet                                  0.828   0.8538083
## All.X.no.rnorm.rpart                          0.066   0.8214419
## All.X.no.rnorm.rf                             5.077   1.0000000
## All.Interact.X.glm                            0.317   0.9129304
## All.Interact.X.bayesglm                       0.836   0.9047371
## All.Interact.X.glmnet                         0.976   0.8764504
## All.Interact.X.no.rnorm.rpart                 0.073   0.8214419
## All.Interact.X.no.rnorm.rf                    5.749   1.0000000
## csm.glm                                       0.320   0.9084139
## csm.bayesglm                                  0.852   0.8986853
## csm.glmnet                                    2.512   0.8842006
## csm.rpart                                     0.075   0.8187829
## csm.rf                                        8.175   0.9985030
##                               opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6320225
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.4       0.8048246
## Max.cor.Y.rpart                                  0.5       0.7628866
## Max.cor.Y.glm                                    0.5       0.7570621
## Interact.High.cor.Y.glm                          0.4       0.7571744
## Low.cor.X.glm                                    0.5       0.7962963
## All.X.glm                                        0.5       0.8045977
## All.X.bayesglm                                   0.5       0.7972350
## All.X.glmnet                                     0.6       0.7605985
## All.X.no.rnorm.rpart                             0.6       0.7821901
## All.X.no.rnorm.rf                                0.5       1.0000000
## All.Interact.X.glm                               0.3       0.8125633
## All.Interact.X.bayesglm                          0.5       0.8088065
## All.Interact.X.glmnet                            0.5       0.7786790
## All.Interact.X.no.rnorm.rpart                    0.6       0.7821901
## All.Interact.X.no.rnorm.rf                       0.5       1.0000000
## csm.glm                                          0.4       0.8236583
## csm.bayesglm                                     0.5       0.8060748
## csm.glmnet                                       0.5       0.7883721
## csm.rpart                                        0.3       0.7555556
## csm.rf                                           0.4       0.9878988
##                               max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                    0.5379877             0.5060896
## Random.myrandom_classfr              0.4620123             0.4303445
## Max.cor.Y.cv.0.rpart                 0.5379877             0.5060896
## Max.cor.Y.cv.0.cp.0.rpart            0.8172485             0.7915181
## Max.cor.Y.rpart                      0.7731117             0.7604190
## Max.cor.Y.glm                        0.7803039             0.7518769
## Interact.High.cor.Y.glm              0.7669516             0.7465456
## Low.cor.X.glm                        0.7566825             0.7936712
## All.X.glm                            0.7535992             0.8001376
## All.X.bayesglm                       0.7659259             0.7936712
## All.X.glmnet                         0.7772206             0.7764773
## All.X.no.rnorm.rpart                 0.8069991             0.7882906
## All.X.no.rnorm.rf                    0.8018487             0.9962198
## All.Interact.X.glm                   0.7628332             0.7839911
## All.Interact.X.bayesglm              0.7638715             0.8055345
## All.Interact.X.glmnet                0.7823425             0.7775499
## All.Interact.X.no.rnorm.rpart        0.8069991             0.7882906
## All.Interact.X.no.rnorm.rf           0.7977398             0.9962198
## csm.glm                              0.7761728             0.8098576
## csm.bayesglm                         0.7741247             0.8044545
## csm.glmnet                           0.7782431             0.7872153
## csm.rpart                            0.7946787             0.7465456
## csm.rf                               0.7915828             0.9798826
##                               max.AccuracyUpper.fit max.Kappa.fit
## MFO.myMFO_classfr                         0.5696555     0.0000000
## Random.myrandom_classfr                   0.4939104     0.0000000
## Max.cor.Y.cv.0.rpart                      0.5696555     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.8410439     0.6330658
## Max.cor.Y.rpart                           0.8127790     0.5377260
## Max.cor.Y.glm                             0.8049446     0.5567446
## Interact.High.cor.Y.glm                   0.8000405     0.5288692
## Low.cor.X.glm                             0.8429848     0.5097158
## All.X.glm                                 0.8488005     0.5028958
## All.X.bayesglm                            0.8429848     0.5277105
## All.X.glmnet                              0.8274264     0.5509902
## All.X.no.rnorm.rpart                      0.8381305     0.6066762
## All.X.no.rnorm.rf                         1.0000000     0.5975601
## All.Interact.X.glm                        0.8342419     0.5212224
## All.Interact.X.bayesglm                   0.8536386     0.5235453
## All.Interact.X.glmnet                     0.8284008     0.5598826
## All.Interact.X.no.rnorm.rpart             0.8381305     0.6066762
## All.Interact.X.no.rnorm.rf                1.0000000     0.5894791
## csm.glm                                   0.8575034     0.5480081
## csm.bayesglm                              0.8526716     0.5443023
## csm.glmnet                                0.8371587     0.5524382
## csm.rpart                                 0.8000405     0.5816711
## csm.rf                                    0.9943492     0.5791514
##                               max.auc.OOB opt.prob.threshold.OOB
## MFO.myMFO_classfr               0.5000000                    0.5
## Random.myrandom_classfr         0.5191913                    0.4
## Max.cor.Y.cv.0.rpart            0.5000000                    0.5
## Max.cor.Y.cv.0.cp.0.rpart       0.8075096                    0.6
## Max.cor.Y.rpart                 0.8080205                    0.5
## Max.cor.Y.glm                   0.8402773                    0.4
## Interact.High.cor.Y.glm         0.8396662                    0.4
## Low.cor.X.glm                   0.8280616                    0.5
## All.X.glm                       0.8270244                    0.5
## All.X.bayesglm                  0.8348447                    0.5
## All.X.glmnet                    0.8455918                    0.6
## All.X.no.rnorm.rpart            0.8103723                    0.6
## All.X.no.rnorm.rf               0.8397895                    0.4
## All.Interact.X.glm              0.8209653                    0.6
## All.Interact.X.bayesglm         0.8287856                    0.5
## All.Interact.X.glmnet           0.8362259                    0.4
## All.Interact.X.no.rnorm.rpart   0.8103723                    0.6
## All.Interact.X.no.rnorm.rf      0.8380950                    0.5
## csm.glm                         0.8167343                    0.5
## csm.bayesglm                    0.8334377                    0.6
## csm.glmnet                      0.8337946                    0.6
## csm.rpart                       0.8090783                    0.3
## csm.rf                          0.8329551                    0.5
##                               max.f.score.OOB max.Accuracy.OOB
## MFO.myMFO_classfr                   0.0000000        0.5367232
## Random.myrandom_classfr             0.6332046        0.4632768
## Max.cor.Y.cv.0.rpart                0.0000000        0.5367232
## Max.cor.Y.cv.0.cp.0.rpart           0.7439490        0.7728814
## Max.cor.Y.rpart                     0.7582697        0.7853107
## Max.cor.Y.glm                       0.7604938        0.7807910
## Interact.High.cor.Y.glm             0.7636816        0.7853107
## Low.cor.X.glm                       0.7595908        0.7875706
## All.X.glm                           0.7588832        0.7853107
## All.X.bayesglm                      0.7603093        0.7898305
## All.X.glmnet                        0.7565337        0.8000000
## All.X.no.rnorm.rpart                0.7600000        0.7966102
## All.X.no.rnorm.rf                   0.7651332        0.7807910
## All.Interact.X.glm                  0.7533512        0.7920904
## All.Interact.X.bayesglm             0.7602041        0.7875706
## All.Interact.X.glmnet               0.7637699        0.7819209
## All.Interact.X.no.rnorm.rpart       0.7600000        0.7966102
## All.Interact.X.no.rnorm.rf          0.7696405        0.8045198
## csm.glm                             0.7480720        0.7785311
## csm.bayesglm                        0.7570470        0.7954802
## csm.glmnet                          0.7682119        0.8022599
## csm.rpart                           0.7528231        0.7774011
## csm.rf                              0.7568922        0.7807910
##                               max.AccuracyLower.OOB max.AccuracyUpper.OOB
## MFO.myMFO_classfr                         0.5032294             0.5699717
## Random.myrandom_classfr                   0.4300283             0.4967706
## Max.cor.Y.cv.0.rpart                      0.5032294             0.5699717
## Max.cor.Y.cv.0.cp.0.rpart                 0.7438293             0.8001036
## Max.cor.Y.rpart                           0.7567658             0.8119416
## Max.cor.Y.glm                             0.7520575             0.8076410
## Interact.High.cor.Y.glm                   0.7567658             0.8119416
## Low.cor.X.glm                             0.7591217             0.8140900
## All.X.glm                                 0.7567658             0.8119416
## All.X.bayesglm                            0.7614789             0.8162373
## All.X.glmnet                              0.7721016             0.8258843
## All.X.no.rnorm.rpart                      0.7685578             0.8226715
## All.X.no.rnorm.rf                         0.7520575             0.8076410
## All.Interact.X.glm                        0.7638373             0.8183833
## All.Interact.X.bayesglm                   0.7591217             0.8140900
## All.Interact.X.glmnet                     0.7532341             0.8087166
## All.Interact.X.no.rnorm.rpart             0.7685578             0.8226715
## All.Interact.X.no.rnorm.rf                0.7768312             0.8301634
## csm.glm                                   0.7497051             0.8054889
## csm.bayesglm                              0.7673772             0.8215999
## csm.glmnet                                0.7744658             0.8280245
## csm.rpart                                 0.7485294             0.8044124
## csm.rf                                    0.7520575             0.8076410
##                               max.Kappa.OOB max.AccuracySD.fit
## MFO.myMFO_classfr                 0.0000000                 NA
## Random.myrandom_classfr           0.0000000                 NA
## Max.cor.Y.cv.0.rpart              0.0000000                 NA
## Max.cor.Y.cv.0.cp.0.rpart         0.5406159                 NA
## Max.cor.Y.rpart                   0.5658292        0.009511979
## Max.cor.Y.glm                     0.5584673        0.014187500
## Interact.High.cor.Y.glm           0.5671369        0.016721875
## Low.cor.X.glm                     0.5701108        0.008872342
## All.X.glm                         0.5659749        0.005787427
## All.X.bayesglm                    0.5742551        0.013804235
## All.X.glmnet                      0.5914905        0.013565899
## All.X.no.rnorm.rpart              0.5861800        0.016668501
## All.X.no.rnorm.rf                 0.5596450        0.013856576
## All.Interact.X.glm                0.5766981        0.009240419
## All.Interact.X.bayesglm           0.5702552        0.014275107
## All.Interact.X.glmnet             0.5612566        0.003377418
## All.Interact.X.no.rnorm.rpart     0.5861800        0.016668501
## All.Interact.X.no.rnorm.rf        0.6023402        0.010841545
## csm.glm                           0.5515164        0.021069919
## csm.bayesglm                      0.5835294        0.002182198
## csm.glmnet                        0.5980144        0.018557757
## csm.rpart                         0.5506630        0.015470002
## csm.rf                            0.5575798        0.009337702
##                               max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                          NA          NA
## Random.myrandom_classfr                    NA          NA
## Max.cor.Y.cv.0.rpart                       NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                  NA          NA
## Max.cor.Y.rpart                   0.025043599          NA
## Max.cor.Y.glm                     0.029788913    943.1953
## Interact.High.cor.Y.glm           0.037163867    946.9576
## Low.cor.X.glm                     0.021731357    935.7122
## All.X.glm                         0.011852251    948.0891
## All.X.bayesglm                    0.025604026    978.4470
## All.X.glmnet                      0.025682439          NA
## All.X.no.rnorm.rpart              0.034485905          NA
## All.X.no.rnorm.rf                 0.030375145          NA
## All.Interact.X.glm                0.020920414    918.5120
## All.Interact.X.bayesglm           0.027036933    961.0875
## All.Interact.X.glmnet             0.010713580          NA
## All.Interact.X.no.rnorm.rpart     0.034485905          NA
## All.Interact.X.no.rnorm.rf        0.024598405          NA
## csm.glm                           0.044773073    913.0612
## csm.bayesglm                      0.005351267    967.8072
## csm.glmnet                        0.040895302          NA
## csm.rpart                         0.032317461          NA
## csm.rf                            0.020835424          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##               label step_major step_minor     bgn    end elapsed
## 11  fit.models_1_rf         11          0 207.247 284.93  77.683
## 12 fit.models_1_end         12          0 284.931     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1 123.330 284.939 161.609
## 12 fit.models          7          2 284.939      NA      NA
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             biddable, startprice.log
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        biddable, startprice.log
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.log
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.log
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                                                                                                                                biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glm                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm                                                                biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart                                                                  biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                                                                     biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glm            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.bayesglm       D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.glmnet         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rpart         D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## All.Interact.X.no.rnorm.rf            D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
## csm.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.bayesglm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.glmnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
## csm.rf                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     prdline.my.fctr, prdline.my.fctr:.clusterid.fctr, prdline.my.fctr*biddable, prdline.my.fctr*startprice.log, prdline.my.fctr*condition.fctr, prdline.my.fctr*D.terms.n.post.stop, prdline.my.fctr*cellular.fctr
##                               max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                           0   0.5000000
## Random.myrandom_classfr                     0   0.5071756
## Max.cor.Y.cv.0.rpart                        0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                   0   0.8781425
## Max.cor.Y.rpart                             3   0.8172434
## Max.cor.Y.glm                               1   0.8402290
## Interact.High.cor.Y.glm                     1   0.8427735
## Low.cor.X.glm                               1   0.8858227
## All.X.glm                                   1   0.8949406
## All.X.bayesglm                              1   0.8909075
## All.X.glmnet                                9   0.8538083
## All.X.no.rnorm.rpart                        3   0.8214419
## All.X.no.rnorm.rf                           3   1.0000000
## All.Interact.X.glm                          1   0.9129304
## All.Interact.X.bayesglm                     1   0.9047371
## All.Interact.X.glmnet                       9   0.8764504
## All.Interact.X.no.rnorm.rpart               3   0.8214419
## All.Interact.X.no.rnorm.rf                  3   1.0000000
## csm.glm                                     1   0.9084139
## csm.bayesglm                                1   0.8986853
## csm.glmnet                                  9   0.8842006
## csm.rpart                                   3   0.8187829
## csm.rf                                      3   0.9985030
##                               opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6320225
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.4       0.8048246
## Max.cor.Y.rpart                                  0.5       0.7628866
## Max.cor.Y.glm                                    0.5       0.7570621
## Interact.High.cor.Y.glm                          0.4       0.7571744
## Low.cor.X.glm                                    0.5       0.7962963
## All.X.glm                                        0.5       0.8045977
## All.X.bayesglm                                   0.5       0.7972350
## All.X.glmnet                                     0.6       0.7605985
## All.X.no.rnorm.rpart                             0.6       0.7821901
## All.X.no.rnorm.rf                                0.5       1.0000000
## All.Interact.X.glm                               0.3       0.8125633
## All.Interact.X.bayesglm                          0.5       0.8088065
## All.Interact.X.glmnet                            0.5       0.7786790
## All.Interact.X.no.rnorm.rpart                    0.6       0.7821901
## All.Interact.X.no.rnorm.rf                       0.5       1.0000000
## csm.glm                                          0.4       0.8236583
## csm.bayesglm                                     0.5       0.8060748
## csm.glmnet                                       0.5       0.7883721
## csm.rpart                                        0.3       0.7555556
## csm.rf                                           0.4       0.9878988
##                               max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                    0.5379877     0.0000000   0.5000000
## Random.myrandom_classfr              0.4620123     0.0000000   0.5191913
## Max.cor.Y.cv.0.rpart                 0.5379877     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart            0.8172485     0.6330658   0.8075096
## Max.cor.Y.rpart                      0.7731117     0.5377260   0.8080205
## Max.cor.Y.glm                        0.7803039     0.5567446   0.8402773
## Interact.High.cor.Y.glm              0.7669516     0.5288692   0.8396662
## Low.cor.X.glm                        0.7566825     0.5097158   0.8280616
## All.X.glm                            0.7535992     0.5028958   0.8270244
## All.X.bayesglm                       0.7659259     0.5277105   0.8348447
## All.X.glmnet                         0.7772206     0.5509902   0.8455918
## All.X.no.rnorm.rpart                 0.8069991     0.6066762   0.8103723
## All.X.no.rnorm.rf                    0.8018487     0.5975601   0.8397895
## All.Interact.X.glm                   0.7628332     0.5212224   0.8209653
## All.Interact.X.bayesglm              0.7638715     0.5235453   0.8287856
## All.Interact.X.glmnet                0.7823425     0.5598826   0.8362259
## All.Interact.X.no.rnorm.rpart        0.8069991     0.6066762   0.8103723
## All.Interact.X.no.rnorm.rf           0.7977398     0.5894791   0.8380950
## csm.glm                              0.7761728     0.5480081   0.8167343
## csm.bayesglm                         0.7741247     0.5443023   0.8334377
## csm.glmnet                           0.7782431     0.5524382   0.8337946
## csm.rpart                            0.7946787     0.5816711   0.8090783
## csm.rf                               0.7915828     0.5791514   0.8329551
##                               opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                                0.5       0.0000000
## Random.myrandom_classfr                          0.4       0.6332046
## Max.cor.Y.cv.0.rpart                             0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                        0.6       0.7439490
## Max.cor.Y.rpart                                  0.5       0.7582697
## Max.cor.Y.glm                                    0.4       0.7604938
## Interact.High.cor.Y.glm                          0.4       0.7636816
## Low.cor.X.glm                                    0.5       0.7595908
## All.X.glm                                        0.5       0.7588832
## All.X.bayesglm                                   0.5       0.7603093
## All.X.glmnet                                     0.6       0.7565337
## All.X.no.rnorm.rpart                             0.6       0.7600000
## All.X.no.rnorm.rf                                0.4       0.7651332
## All.Interact.X.glm                               0.6       0.7533512
## All.Interact.X.bayesglm                          0.5       0.7602041
## All.Interact.X.glmnet                            0.4       0.7637699
## All.Interact.X.no.rnorm.rpart                    0.6       0.7600000
## All.Interact.X.no.rnorm.rf                       0.5       0.7696405
## csm.glm                                          0.5       0.7480720
## csm.bayesglm                                     0.6       0.7570470
## csm.glmnet                                       0.6       0.7682119
## csm.rpart                                        0.3       0.7528231
## csm.rf                                           0.5       0.7568922
##                               max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                    0.5367232     0.0000000
## Random.myrandom_classfr              0.4632768     0.0000000
## Max.cor.Y.cv.0.rpart                 0.5367232     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart            0.7728814     0.5406159
## Max.cor.Y.rpart                      0.7853107     0.5658292
## Max.cor.Y.glm                        0.7807910     0.5584673
## Interact.High.cor.Y.glm              0.7853107     0.5671369
## Low.cor.X.glm                        0.7875706     0.5701108
## All.X.glm                            0.7853107     0.5659749
## All.X.bayesglm                       0.7898305     0.5742551
## All.X.glmnet                         0.8000000     0.5914905
## All.X.no.rnorm.rpart                 0.7966102     0.5861800
## All.X.no.rnorm.rf                    0.7807910     0.5596450
## All.Interact.X.glm                   0.7920904     0.5766981
## All.Interact.X.bayesglm              0.7875706     0.5702552
## All.Interact.X.glmnet                0.7819209     0.5612566
## All.Interact.X.no.rnorm.rpart        0.7966102     0.5861800
## All.Interact.X.no.rnorm.rf           0.8045198     0.6023402
## csm.glm                              0.7785311     0.5515164
## csm.bayesglm                         0.7954802     0.5835294
## csm.glmnet                           0.8022599     0.5980144
## csm.rpart                            0.7774011     0.5506630
## csm.rf                               0.7807910     0.5575798
##                               inv.elapsedtime.everything
## MFO.myMFO_classfr                             2.46913580
## Random.myrandom_classfr                       3.77358491
## Max.cor.Y.cv.0.rpart                          1.58478605
## Max.cor.Y.cv.0.cp.0.rpart                     1.93798450
## Max.cor.Y.rpart                               0.95602294
## Max.cor.Y.glm                                 0.96061479
## Interact.High.cor.Y.glm                       0.98425197
## Low.cor.X.glm                                 0.69735007
## All.X.glm                                     0.59488400
## All.X.bayesglm                                0.33647376
## All.X.glmnet                                  0.14974543
## All.X.no.rnorm.rpart                          0.60386473
## All.X.no.rnorm.rf                             0.05903885
## All.Interact.X.glm                            0.54854635
## All.Interact.X.bayesglm                       0.27586207
## All.Interact.X.glmnet                         0.12498438
## All.Interact.X.no.rnorm.rpart                 0.57175529
## All.Interact.X.no.rnorm.rf                    0.05482757
## csm.glm                                       0.54854635
## csm.bayesglm                                  0.25588536
## csm.glmnet                                    0.11587486
## csm.rpart                                     0.56657224
## csm.rf                                        0.04808617
##                               inv.elapsedtime.final inv.aic.fit
## MFO.myMFO_classfr                       500.0000000          NA
## Random.myrandom_classfr                1000.0000000          NA
## Max.cor.Y.cv.0.rpart                     76.9230769          NA
## Max.cor.Y.cv.0.cp.0.rpart               100.0000000          NA
## Max.cor.Y.rpart                          83.3333333          NA
## Max.cor.Y.glm                            66.6666667 0.001060226
## Interact.High.cor.Y.glm                  58.8235294 0.001056014
## Low.cor.X.glm                             5.0251256 0.001068705
## All.X.glm                                 4.1152263 0.001054753
## All.X.bayesglm                            2.2988506 0.001022028
## All.X.glmnet                              1.2077295          NA
## All.X.no.rnorm.rpart                     15.1515152          NA
## All.X.no.rnorm.rf                         0.1969667          NA
## All.Interact.X.glm                        3.1545741 0.001088717
## All.Interact.X.bayesglm                   1.1961722 0.001040488
## All.Interact.X.glmnet                     1.0245902          NA
## All.Interact.X.no.rnorm.rpart            13.6986301          NA
## All.Interact.X.no.rnorm.rf                0.1739433          NA
## csm.glm                                   3.1250000 0.001095217
## csm.bayesglm                              1.1737089 0.001033264
## csm.glmnet                                0.3980892          NA
## csm.rpart                                13.3333333          NA
## csm.rf                                    0.1223242          NA
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

![](ebayipads_csmmdl_files/figure-html/fit.models_2-1.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_2-2.png) 

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
## 18    All.Interact.X.no.rnorm.rf        0.8045198   0.8380950
## 21                    csm.glmnet        0.8022599   0.8337946
## 11                  All.X.glmnet        0.8000000   0.8455918
## 12          All.X.no.rnorm.rpart        0.7966102   0.8103723
## 17 All.Interact.X.no.rnorm.rpart        0.7966102   0.8103723
## 20                  csm.bayesglm        0.7954802   0.8334377
## 14            All.Interact.X.glm        0.7920904   0.8209653
## 10                All.X.bayesglm        0.7898305   0.8348447
## 15       All.Interact.X.bayesglm        0.7875706   0.8287856
## 8                  Low.cor.X.glm        0.7875706   0.8280616
## 7        Interact.High.cor.Y.glm        0.7853107   0.8396662
## 9                      All.X.glm        0.7853107   0.8270244
## 5                Max.cor.Y.rpart        0.7853107   0.8080205
## 16         All.Interact.X.glmnet        0.7819209   0.8362259
## 6                  Max.cor.Y.glm        0.7807910   0.8402773
## 13             All.X.no.rnorm.rf        0.7807910   0.8397895
## 23                        csm.rf        0.7807910   0.8329551
## 19                       csm.glm        0.7785311   0.8167343
## 22                     csm.rpart        0.7774011   0.8090783
## 4      Max.cor.Y.cv.0.cp.0.rpart        0.7728814   0.8075096
## 1              MFO.myMFO_classfr        0.5367232   0.5000000
## 3           Max.cor.Y.cv.0.rpart        0.5367232   0.5000000
## 2        Random.myrandom_classfr        0.4632768   0.5191913
##    max.Kappa.OOB min.aic.fit opt.prob.threshold.OOB
## 18     0.6023402          NA                    0.5
## 21     0.5980144          NA                    0.6
## 11     0.5914905          NA                    0.6
## 12     0.5861800          NA                    0.6
## 17     0.5861800          NA                    0.6
## 20     0.5835294    967.8072                    0.6
## 14     0.5766981    918.5120                    0.6
## 10     0.5742551    978.4470                    0.5
## 15     0.5702552    961.0875                    0.5
## 8      0.5701108    935.7122                    0.5
## 7      0.5671369    946.9576                    0.4
## 9      0.5659749    948.0891                    0.5
## 5      0.5658292          NA                    0.5
## 16     0.5612566          NA                    0.4
## 6      0.5584673    943.1953                    0.4
## 13     0.5596450          NA                    0.4
## 23     0.5575798          NA                    0.5
## 19     0.5515164    913.0612                    0.5
## 22     0.5506630          NA                    0.3
## 4      0.5406159          NA                    0.6
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

![](ebayipads_csmmdl_files/figure-html/fit.models_2-3.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.models_2-4.png) 

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
##                                               importance
## biddable                                    1.000000e+02
## startprice.log                              9.404554e+01
## idseq.my                                    5.829503e+01
## prdline.my.fctriPadmini:startprice.log      9.239296e+00
## prdline.my.fctriPadAir:startprice.log       8.509567e+00
## prdline.my.fctriPadmini:idseq.my            8.476034e+00
## prdline.my.fctriPad 3+:startprice.log       8.386078e+00
## prdline.my.fctriPadAir:idseq.my             8.220315e+00
## D.ratio.sum.TfIdf.nwrds                     7.950279e+00
## prdline.my.fctriPad 1:startprice.log        7.037547e+00
## D.TfIdf.sum.stem.stop.Ratio                 6.571168e+00
## prdline.my.fctriPad 3+:idseq.my             6.559448e+00
## prdline.my.fctriPadmini 2+:idseq.my         5.226984e+00
## prdline.my.fctriPad 1:idseq.my              5.120671e+00
## D.sum.TfIdf                                 4.617664e+00
## D.TfIdf.sum.post.stop                       4.617532e+00
## prdline.my.fctriPadmini 2+:startprice.log   4.534814e+00
## D.TfIdf.sum.post.stem                       4.392375e+00
## color.fctrBlack                             4.334881e+00
## storage.fctrUnknown                         4.285224e+00
## D.ratio.nstopwrds.nwrds                     4.237746e+00
## storage.fctr64                              4.189194e+00
## prdline.my.fctriPad 2:startprice.log        3.796520e+00
## color.fctrWhite                             3.720983e+00
## D.nstopwrds.log                             3.553877e+00
## D.nchrs.log                                 3.449126e+00
## cellular.fctr1                              3.344855e+00
## storage.fctr16                              3.326872e+00
## D.nuppr.log                                 3.259898e+00
## prdline.my.fctriPad 2:idseq.my              3.156370e+00
## cellular.fctrUnknown                        2.795316e+00
## carrier.fctrUnknown                         2.794373e+00
## color.fctrSpace Gray                        2.789616e+00
## D.nwrds.log                                 2.767589e+00
## prdline.my.fctriPadAir:biddable             2.717394e+00
## prdline.my.fctriPad 2:.clusterid.fctr5      2.526636e+00
## condition.fctrNew                           2.473749e+00
## carrier.fctrVerizon                         2.301742e+00
## condition.fctrNew other (see details)       2.269510e+00
## D.nwrds.unq.log                             1.998496e+00
## D.terms.n.post.stem                         1.981725e+00
## storage.fctr32                              1.879219e+00
## condition.fctrManufacturer refurbished      1.872430e+00
## prdline.my.fctriPad 2:biddable              1.806590e+00
## D.terms.n.post.stem.log                     1.800677e+00
## carrier.fctrAT&T                            1.799152e+00
## prdline.my.fctriPad 1:biddable              1.680080e+00
## D.terms.n.post.stop                         1.637758e+00
## condition.fctrFor parts or not working      1.615396e+00
## prdline.my.fctrUnknown:.clusterid.fctr2     1.613022e+00
## condition.fctrSeller refurbished            1.542534e+00
## D.terms.n.post.stop.log                     1.368098e+00
## D.terms.n.stem.stop.Ratio                   1.365550e+00
## D.ndgts.log                                 1.233854e+00
## prdline.my.fctriPad 3+                      1.210476e+00
## D.npnct13.log                               1.184904e+00
## D.npnct11.log                               1.181740e+00
## color.fctrGold                              1.088644e+00
## prdline.my.fctriPad 3+:biddable             9.996656e-01
## prdline.my.fctriPad 1                       9.697913e-01
## prdline.my.fctriPadmini                     8.895090e-01
## carrier.fctrSprint                          8.170434e-01
## D.npnct08.log                               7.562689e-01
## prdline.my.fctriPadmini:.clusterid.fctr3    7.562502e-01
## prdline.my.fctriPad 1:.clusterid.fctr2      7.415946e-01
## D.npnct05.log                               7.372404e-01
## prdline.my.fctriPad 2                       7.072275e-01
## prdline.my.fctriPadAir                      6.865845e-01
## D.npnct01.log                               5.928110e-01
## prdline.my.fctriPadmini:biddable            5.802216e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4     5.517229e-01
## prdline.my.fctriPadmini 2+                  5.473725e-01
## carrier.fctrT-Mobile                        5.325882e-01
## D.npnct15.log                               5.300095e-01
## prdline.my.fctriPadmini 2+:biddable         4.911016e-01
## prdline.my.fctriPad 1:.clusterid.fctr4      4.806325e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 4.788083e-01
## D.npnct12.log                               4.656639e-01
## D.npnct16.log                               3.934201e-01
## D.npnct24.log                               3.916757e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3     3.905739e-01
## prdline.my.fctriPadAir:.clusterid.fctr3     3.325317e-01
## D.npnct03.log                               3.227649e-01
## prdline.my.fctriPad 2:.clusterid.fctr4      3.034445e-01
## prdline.my.fctrUnknown:.clusterid.fctr3     3.031782e-01
## D.npnct14.log                               2.915722e-01
## prdline.my.fctriPadmini:.clusterid.fctr2    2.526365e-01
## prdline.my.fctriPad 2:.clusterid.fctr2      2.515994e-01
## D.npnct06.log                               2.047493e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 1.922444e-01
## carrier.fctrOther                           1.853682e-01
## prdline.my.fctriPadmini:.clusterid.fctr4    1.657237e-01
## prdline.my.fctriPadAir:.clusterid.fctr4     1.549737e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2     1.245769e-01
## prdline.my.fctriPadAir:.clusterid.fctr2     1.149917e-01
## prdline.my.fctriPad 1:.clusterid.fctr3      7.867550e-02
## prdline.my.fctriPad 2:.clusterid.fctr3      6.143237e-02
## prdline.my.fctriPadmini:.clusterid.fctr5    3.915683e-02
## D.npnct10.log                               1.650574e-02
## D.npnct09.log                               7.666838e-03
## D.npnct28.log                               5.742064e-03
## prdline.my.fctrUnknown:.clusterid.fctr4     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4 0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5 0.000000e+00
##                                             All.Interact.X.no.rnorm.rf.importance
## biddable                                                             1.000000e+02
## startprice.log                                                       9.404554e+01
## idseq.my                                                             5.829503e+01
## prdline.my.fctriPadmini:startprice.log                               9.239296e+00
## prdline.my.fctriPadAir:startprice.log                                8.509567e+00
## prdline.my.fctriPadmini:idseq.my                                     8.476034e+00
## prdline.my.fctriPad 3+:startprice.log                                8.386078e+00
## prdline.my.fctriPadAir:idseq.my                                      8.220315e+00
## D.ratio.sum.TfIdf.nwrds                                              7.950279e+00
## prdline.my.fctriPad 1:startprice.log                                 7.037547e+00
## D.TfIdf.sum.stem.stop.Ratio                                          6.571168e+00
## prdline.my.fctriPad 3+:idseq.my                                      6.559448e+00
## prdline.my.fctriPadmini 2+:idseq.my                                  5.226984e+00
## prdline.my.fctriPad 1:idseq.my                                       5.120671e+00
## D.sum.TfIdf                                                          4.617664e+00
## D.TfIdf.sum.post.stop                                                4.617532e+00
## prdline.my.fctriPadmini 2+:startprice.log                            4.534814e+00
## D.TfIdf.sum.post.stem                                                4.392375e+00
## color.fctrBlack                                                      4.334881e+00
## storage.fctrUnknown                                                  4.285224e+00
## D.ratio.nstopwrds.nwrds                                              4.237746e+00
## storage.fctr64                                                       4.189194e+00
## prdline.my.fctriPad 2:startprice.log                                 3.796520e+00
## color.fctrWhite                                                      3.720983e+00
## D.nstopwrds.log                                                      3.553877e+00
## D.nchrs.log                                                          3.449126e+00
## cellular.fctr1                                                       3.344855e+00
## storage.fctr16                                                       3.326872e+00
## D.nuppr.log                                                          3.259898e+00
## prdline.my.fctriPad 2:idseq.my                                       3.156370e+00
## cellular.fctrUnknown                                                 2.795316e+00
## carrier.fctrUnknown                                                  2.794373e+00
## color.fctrSpace Gray                                                 2.789616e+00
## D.nwrds.log                                                          2.767589e+00
## prdline.my.fctriPadAir:biddable                                      2.717394e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                               2.526636e+00
## condition.fctrNew                                                    2.473749e+00
## carrier.fctrVerizon                                                  2.301742e+00
## condition.fctrNew other (see details)                                2.269510e+00
## D.nwrds.unq.log                                                      1.998496e+00
## D.terms.n.post.stem                                                  1.981725e+00
## storage.fctr32                                                       1.879219e+00
## condition.fctrManufacturer refurbished                               1.872430e+00
## prdline.my.fctriPad 2:biddable                                       1.806590e+00
## D.terms.n.post.stem.log                                              1.800677e+00
## carrier.fctrAT&T                                                     1.799152e+00
## prdline.my.fctriPad 1:biddable                                       1.680080e+00
## D.terms.n.post.stop                                                  1.637758e+00
## condition.fctrFor parts or not working                               1.615396e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                              1.613022e+00
## condition.fctrSeller refurbished                                     1.542534e+00
## D.terms.n.post.stop.log                                              1.368098e+00
## D.terms.n.stem.stop.Ratio                                            1.365550e+00
## D.ndgts.log                                                          1.233854e+00
## prdline.my.fctriPad 3+                                               1.210476e+00
## D.npnct13.log                                                        1.184904e+00
## D.npnct11.log                                                        1.181740e+00
## color.fctrGold                                                       1.088644e+00
## prdline.my.fctriPad 3+:biddable                                      9.996656e-01
## prdline.my.fctriPad 1                                                9.697913e-01
## prdline.my.fctriPadmini                                              8.895090e-01
## carrier.fctrSprint                                                   8.170434e-01
## D.npnct08.log                                                        7.562689e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                             7.562502e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                               7.415946e-01
## D.npnct05.log                                                        7.372404e-01
## prdline.my.fctriPad 2                                                7.072275e-01
## prdline.my.fctriPadAir                                               6.865845e-01
## D.npnct01.log                                                        5.928110e-01
## prdline.my.fctriPadmini:biddable                                     5.802216e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                              5.517229e-01
## prdline.my.fctriPadmini 2+                                           5.473725e-01
## carrier.fctrT-Mobile                                                 5.325882e-01
## D.npnct15.log                                                        5.300095e-01
## prdline.my.fctriPadmini 2+:biddable                                  4.911016e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                               4.806325e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                          4.788083e-01
## D.npnct12.log                                                        4.656639e-01
## D.npnct16.log                                                        3.934201e-01
## D.npnct24.log                                                        3.916757e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                              3.905739e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                              3.325317e-01
## D.npnct03.log                                                        3.227649e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                               3.034445e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                              3.031782e-01
## D.npnct14.log                                                        2.915722e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                             2.526365e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                               2.515994e-01
## D.npnct06.log                                                        2.047493e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                          1.922444e-01
## carrier.fctrOther                                                    1.853682e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                             1.657237e-01
## prdline.my.fctriPadAir:.clusterid.fctr4                              1.549737e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                              1.245769e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                              1.149917e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                               7.867550e-02
## prdline.my.fctriPad 2:.clusterid.fctr3                               6.143237e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                             3.915683e-02
## D.npnct10.log                                                        1.650574e-02
## D.npnct09.log                                                        7.666838e-03
## D.npnct28.log                                                        5.742064e-03
## prdline.my.fctrUnknown:.clusterid.fctr4                              0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                          0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                               0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                          0.000000e+00
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 45
```

![](ebayipads_csmmdl_files/figure-html/fit.models_2-5.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_2-6.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_2-7.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_2-8.png) ![](ebayipads_csmmdl_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 66      10066         N                                             0.134
## 1396    11397         N                                             0.040
## 285     10285         Y                                             0.978
## 1854    11856         N                                             0.512
## 1613    11614         N                                             0.530
## 1409    11410         N                                             0.570
## 409     10409         N                                             0.600
## 851     10851         N                                             0.602
## 602     10602         N                                             0.604
## 1385    11386         N                                             0.614
## 1836    11837         N                                             0.676
## 1835    11836         N                                             0.740
## 1699    11700         N                                             0.942
## 1768    11769         N                                             0.950
## 199     10199         N                                             0.964
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 66                                              N
## 1396                                            N
## 285                                             Y
## 1854                                            Y
## 1613                                            Y
## 1409                                            Y
## 409                                             Y
## 851                                             Y
## 602                                             Y
## 1385                                            Y
## 1836                                            Y
## 1835                                            Y
## 1699                                            Y
## 1768                                            Y
## 199                                             Y
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 66                                                    TRUE
## 1396                                                  TRUE
## 285                                                   TRUE
## 1854                                                 FALSE
## 1613                                                 FALSE
## 1409                                                 FALSE
## 409                                                  FALSE
## 851                                                  FALSE
## 602                                                  FALSE
## 1385                                                 FALSE
## 1836                                                 FALSE
## 1835                                                 FALSE
## 1699                                                 FALSE
## 1768                                                 FALSE
## 199                                                  FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error .label
## 66                                                0.000  10066
## 1396                                              0.000  11397
## 285                                               0.000  10285
## 1854                                              0.012  11856
## 1613                                              0.030  11614
## 1409                                              0.070  11410
## 409                                               0.100  10409
## 851                                               0.102  10851
## 602                                               0.104  10602
## 1385                                              0.114  11386
## 1836                                              0.176  11837
## 1835                                              0.240  11836
## 1699                                              0.442  11700
## 1768                                              0.450  11769
## 199                                               0.464  10199
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 1358    11359         Y                                             0.000
## 1447    11448         Y                                             0.004
## 1426    11427         Y                                             0.010
## 962     10962         Y                                             0.016
## 1212    11212         Y                                             0.026
## 1719    11720         Y                                             0.028
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 1358                                            N
## 1447                                            N
## 1426                                            N
## 962                                             N
## 1212                                            N
## 1719                                            N
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 1358                                                 FALSE
## 1447                                                 FALSE
## 1426                                                 FALSE
## 962                                                  FALSE
## 1212                                                 FALSE
## 1719                                                 FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error
## 1358                                             -0.500
## 1447                                             -0.496
## 1426                                             -0.490
## 962                                              -0.484
## 1212                                             -0.474
## 1719                                             -0.472
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 161     10161         Y                                             0.060
## 277     10277         Y                                             0.252
## 562     10562         Y                                             0.348
## 1056    11056         Y                                             0.416
## 1836    11837         N                                             0.676
## 283     10283         N                                             1.000
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 161                                             N
## 277                                             N
## 562                                             N
## 1056                                            N
## 1836                                            Y
## 283                                             Y
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 161                                                  FALSE
## 277                                                  FALSE
## 562                                                  FALSE
## 1056                                                 FALSE
## 1836                                                 FALSE
## 283                                                  FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error
## 161                                              -0.440
## 277                                              -0.248
## 562                                              -0.152
## 1056                                             -0.084
## 1836                                              0.176
## 283                                               0.500
##      UniqueID sold.fctr sold.fctr.predict.All.Interact.X.no.rnorm.rf.prob
## 581     10581         N                                             0.978
## 491     10491         N                                             0.988
## 413     10413         N                                             0.988
## 1621    11622         N                                             0.996
## 526     10526         N                                             0.998
## 283     10283         N                                             1.000
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf
## 581                                             Y
## 491                                             Y
## 413                                             Y
## 1621                                            Y
## 526                                             Y
## 283                                             Y
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.accurate
## 581                                                  FALSE
## 491                                                  FALSE
## 413                                                  FALSE
## 1621                                                 FALSE
## 526                                                  FALSE
## 283                                                  FALSE
##      sold.fctr.predict.All.Interact.X.no.rnorm.rf.error
## 581                                               0.478
## 491                                               0.488
## 413                                               0.488
## 1621                                              0.496
## 526                                               0.498
## 283                                               0.500
```

![](ebayipads_csmmdl_files/figure-html/fit.models_2-10.png) 

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
## 12 fit.models          7          2 284.939 314.937  29.998
## 13 fit.models          7          3 314.938      NA      NA
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

![](ebayipads_csmmdl_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 314.938 319.942   5.004
## 14 fit.data.training          8          0 319.943      NA      NA
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
## [1] "    indep_vars: D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-1.png) 

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
## importance       108   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               1859   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           108   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold   f.score
## 1        0.0 0.6325855
## 2        0.1 0.8273208
## 3        0.2 0.9466153
## 4        0.3 0.9839817
## 5        0.4 0.9988386
## 6        0.5 1.0000000
## 7        0.6 0.9994183
## 8        0.7 0.9376158
## 9        0.8 0.8556221
## 10       0.9 0.7670251
## 11       1.0 0.2231405
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

![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1 D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.nstopwrds.log, D.npnct24.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.terms.n.post.stop.log, D.nwrds.unq.log, D.terms.n.post.stem.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, prdline.my.fctr*startprice.log, prdline.my.fctr*biddable, prdline.my.fctr*idseq.my, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     25.164                  13.9
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8020428
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9980176                     1     0.5982818
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005254651      0.01117135
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 319.943 347.734  27.791
## 15 fit.data.training          8          1 347.735      NA      NA
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
##                                             All.Interact.X.no.rnorm.rf.importance
## biddable                                                             1.000000e+02
## startprice.log                                                       9.404554e+01
## idseq.my                                                             5.829503e+01
## prdline.my.fctriPadmini:startprice.log                               9.239296e+00
## prdline.my.fctriPad 1:startprice.log                                 7.037547e+00
## prdline.my.fctriPadAir:idseq.my                                      8.220315e+00
## prdline.my.fctriPadAir:startprice.log                                8.509567e+00
## prdline.my.fctriPad 3+:startprice.log                                8.386078e+00
## prdline.my.fctriPad 2:startprice.log                                 3.796520e+00
## prdline.my.fctriPadmini:idseq.my                                     8.476034e+00
## D.ratio.sum.TfIdf.nwrds                                              7.950279e+00
## prdline.my.fctriPadmini 2+:idseq.my                                  5.226984e+00
## prdline.my.fctriPad 3+:idseq.my                                      6.559448e+00
## D.TfIdf.sum.stem.stop.Ratio                                          6.571168e+00
## prdline.my.fctriPadmini 2+:startprice.log                            4.534814e+00
## prdline.my.fctriPad 2:idseq.my                                       3.156370e+00
## prdline.my.fctriPad 1:idseq.my                                       5.120671e+00
## D.ratio.nstopwrds.nwrds                                              4.237746e+00
## color.fctrWhite                                                      3.720983e+00
## D.nstopwrds.log                                                      3.553877e+00
## D.TfIdf.sum.post.stop                                                4.617532e+00
## cellular.fctr1                                                       3.344855e+00
## D.nchrs.log                                                          3.449126e+00
## D.TfIdf.sum.post.stem                                                4.392375e+00
## D.sum.TfIdf                                                          4.617664e+00
## color.fctrBlack                                                      4.334881e+00
## storage.fctr16                                                       3.326872e+00
## storage.fctr64                                                       4.189194e+00
## D.nuppr.log                                                          3.259898e+00
## color.fctrSpace Gray                                                 2.789616e+00
## carrier.fctrUnknown                                                  2.794373e+00
## D.nwrds.log                                                          2.767589e+00
## cellular.fctrUnknown                                                 2.795316e+00
## storage.fctrUnknown                                                  4.285224e+00
## condition.fctrNew                                                    2.473749e+00
## prdline.my.fctriPadAir:biddable                                      2.717394e+00
## storage.fctr32                                                       1.879219e+00
## D.npnct11.log                                                        1.181740e+00
## condition.fctrNew other (see details)                                2.269510e+00
## carrier.fctrVerizon                                                  2.301742e+00
## condition.fctrFor parts or not working                               1.615396e+00
## carrier.fctrAT&T                                                     1.799152e+00
## D.terms.n.post.stem                                                  1.981725e+00
## D.npnct13.log                                                        1.184904e+00
## D.terms.n.post.stop.log                                              1.368098e+00
## D.terms.n.post.stop                                                  1.637758e+00
## D.nwrds.unq.log                                                      1.998496e+00
## D.terms.n.post.stem.log                                              1.800677e+00
## prdline.my.fctriPad 2:biddable                                       1.806590e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                               2.526636e+00
## condition.fctrManufacturer refurbished                               1.872430e+00
## condition.fctrSeller refurbished                                     1.542534e+00
## D.npnct15.log                                                        5.300095e-01
## D.ndgts.log                                                          1.233854e+00
## color.fctrGold                                                       1.088644e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                              1.613022e+00
## prdline.my.fctriPad 3+                                               1.210476e+00
## prdline.my.fctriPad 3+:biddable                                      9.996656e-01
## D.terms.n.stem.stop.Ratio                                            1.365550e+00
## carrier.fctrSprint                                                   8.170434e-01
## prdline.my.fctriPad 1                                                9.697913e-01
## carrier.fctrT-Mobile                                                 5.325882e-01
## prdline.my.fctriPadAir                                               6.865845e-01
## prdline.my.fctriPad 1:biddable                                       1.680080e+00
## prdline.my.fctriPadmini                                              8.895090e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                               3.034445e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                             1.657237e-01
## prdline.my.fctriPadmini 2+                                           5.473725e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                              3.031782e-01
## prdline.my.fctriPad 2                                                7.072275e-01
## D.npnct16.log                                                        3.934201e-01
## D.npnct08.log                                                        7.562689e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                              5.517229e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                               7.415946e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                              3.905739e-01
## D.npnct14.log                                                        2.915722e-01
## D.npnct05.log                                                        7.372404e-01
## prdline.my.fctriPadmini 2+:biddable                                  4.911016e-01
## prdline.my.fctriPadmini:.clusterid.fctr5                             3.915683e-02
## prdline.my.fctriPadmini:.clusterid.fctr3                             7.562502e-01
## D.npnct06.log                                                        2.047493e-01
## D.npnct24.log                                                        3.916757e-01
## prdline.my.fctriPadmini:biddable                                     5.802216e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                          1.922444e-01
## D.npnct01.log                                                        5.928110e-01
## carrier.fctrOther                                                    1.853682e-01
## D.npnct12.log                                                        4.656639e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                              1.149917e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                             2.526365e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                               2.515994e-01
## prdline.my.fctriPadAir:.clusterid.fctr4                              1.549737e-01
## D.npnct03.log                                                        3.227649e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                               4.806325e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                              1.245769e-01
## D.npnct10.log                                                        1.650574e-02
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                          4.788083e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                               7.867550e-02
## prdline.my.fctriPadAir:.clusterid.fctr3                              3.325317e-01
## prdline.my.fctriPad 2:.clusterid.fctr3                               6.143237e-02
## D.npnct09.log                                                        7.666838e-03
## D.npnct28.log                                                        5.742064e-03
## prdline.my.fctrUnknown:.clusterid.fctr4                              0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                               0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                          0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                          0.000000e+00
##                                               importance
## biddable                                    1.000000e+02
## startprice.log                              8.838447e+01
## idseq.my                                    5.277094e+01
## prdline.my.fctriPadmini:startprice.log      9.683368e+00
## prdline.my.fctriPad 1:startprice.log        9.553179e+00
## prdline.my.fctriPadAir:idseq.my             8.170386e+00
## prdline.my.fctriPadAir:startprice.log       7.987419e+00
## prdline.my.fctriPad 3+:startprice.log       7.886110e+00
## prdline.my.fctriPad 2:startprice.log        6.564518e+00
## prdline.my.fctriPadmini:idseq.my            6.252868e+00
## D.ratio.sum.TfIdf.nwrds                     6.066287e+00
## prdline.my.fctriPadmini 2+:idseq.my         5.982734e+00
## prdline.my.fctriPad 3+:idseq.my             5.523726e+00
## D.TfIdf.sum.stem.stop.Ratio                 5.419888e+00
## prdline.my.fctriPadmini 2+:startprice.log   5.038098e+00
## prdline.my.fctriPad 2:idseq.my              4.858351e+00
## prdline.my.fctriPad 1:idseq.my              4.719666e+00
## D.ratio.nstopwrds.nwrds                     4.379868e+00
## color.fctrWhite                             3.799246e+00
## D.nstopwrds.log                             3.773077e+00
## D.TfIdf.sum.post.stop                       3.740726e+00
## cellular.fctr1                              3.700613e+00
## D.nchrs.log                                 3.554992e+00
## D.TfIdf.sum.post.stem                       3.468698e+00
## D.sum.TfIdf                                 3.445068e+00
## color.fctrBlack                             3.338438e+00
## storage.fctr16                              3.176301e+00
## storage.fctr64                              3.174941e+00
## D.nuppr.log                                 3.127558e+00
## color.fctrSpace Gray                        3.103932e+00
## carrier.fctrUnknown                         3.044874e+00
## D.nwrds.log                                 2.948647e+00
## cellular.fctrUnknown                        2.761831e+00
## storage.fctrUnknown                         2.754111e+00
## condition.fctrNew                           2.642641e+00
## prdline.my.fctriPadAir:biddable             2.576494e+00
## storage.fctr32                              1.946215e+00
## D.npnct11.log                               1.882235e+00
## condition.fctrNew other (see details)       1.847878e+00
## carrier.fctrVerizon                         1.823145e+00
## condition.fctrFor parts or not working      1.816412e+00
## carrier.fctrAT&T                            1.628063e+00
## D.terms.n.post.stem                         1.615954e+00
## D.npnct13.log                               1.600156e+00
## D.terms.n.post.stop.log                     1.597720e+00
## D.terms.n.post.stop                         1.574592e+00
## D.nwrds.unq.log                             1.487087e+00
## D.terms.n.post.stem.log                     1.486405e+00
## prdline.my.fctriPad 2:biddable              1.474471e+00
## prdline.my.fctriPad 2:.clusterid.fctr5      1.320461e+00
## condition.fctrManufacturer refurbished      1.305038e+00
## condition.fctrSeller refurbished            1.292569e+00
## D.npnct15.log                               1.246443e+00
## D.ndgts.log                                 1.192747e+00
## color.fctrGold                              1.097366e+00
## prdline.my.fctrUnknown:.clusterid.fctr2     1.045092e+00
## prdline.my.fctriPad 3+                      9.771684e-01
## prdline.my.fctriPad 3+:biddable             8.655007e-01
## D.terms.n.stem.stop.Ratio                   8.641274e-01
## carrier.fctrSprint                          8.232701e-01
## prdline.my.fctriPad 1                       8.079325e-01
## carrier.fctrT-Mobile                        7.723365e-01
## prdline.my.fctriPadAir                      7.463619e-01
## prdline.my.fctriPad 1:biddable              7.225053e-01
## prdline.my.fctriPadmini                     7.172816e-01
## prdline.my.fctriPad 2:.clusterid.fctr4      7.143282e-01
## prdline.my.fctriPadmini:.clusterid.fctr4    6.970082e-01
## prdline.my.fctriPadmini 2+                  6.597996e-01
## prdline.my.fctrUnknown:.clusterid.fctr3     6.523183e-01
## prdline.my.fctriPad 2                       5.835669e-01
## D.npnct16.log                               5.688453e-01
## D.npnct08.log                               5.374434e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4     5.364886e-01
## prdline.my.fctriPad 1:.clusterid.fctr2      5.121854e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3     4.923391e-01
## D.npnct14.log                               4.698068e-01
## D.npnct05.log                               4.595635e-01
## prdline.my.fctriPadmini 2+:biddable         4.325461e-01
## prdline.my.fctriPadmini:.clusterid.fctr5    3.997570e-01
## prdline.my.fctriPadmini:.clusterid.fctr3    3.986979e-01
## D.npnct06.log                               3.663155e-01
## D.npnct24.log                               3.452116e-01
## prdline.my.fctriPadmini:biddable            3.308949e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 2.943240e-01
## D.npnct01.log                               2.925626e-01
## carrier.fctrOther                           2.773431e-01
## D.npnct12.log                               2.603176e-01
## prdline.my.fctriPadAir:.clusterid.fctr2     2.558644e-01
## prdline.my.fctriPadmini:.clusterid.fctr2    2.282130e-01
## prdline.my.fctriPad 2:.clusterid.fctr2      2.271737e-01
## prdline.my.fctriPadAir:.clusterid.fctr4     2.087691e-01
## D.npnct03.log                               2.068028e-01
## prdline.my.fctriPad 1:.clusterid.fctr4      1.849742e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2     1.776392e-01
## D.npnct10.log                               1.721324e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 1.653715e-01
## prdline.my.fctriPad 1:.clusterid.fctr3      1.457727e-01
## prdline.my.fctriPadAir:.clusterid.fctr3     1.064812e-01
## prdline.my.fctriPad 2:.clusterid.fctr3      8.190876e-02
## D.npnct09.log                               2.781385e-03
## D.npnct28.log                               1.470077e-03
## prdline.my.fctrUnknown:.clusterid.fctr4     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5 0.000000e+00
##                                             Final.rf.importance
## biddable                                           1.000000e+02
## startprice.log                                     8.838447e+01
## idseq.my                                           5.277094e+01
## prdline.my.fctriPadmini:startprice.log             9.683368e+00
## prdline.my.fctriPad 1:startprice.log               9.553179e+00
## prdline.my.fctriPadAir:idseq.my                    8.170386e+00
## prdline.my.fctriPadAir:startprice.log              7.987419e+00
## prdline.my.fctriPad 3+:startprice.log              7.886110e+00
## prdline.my.fctriPad 2:startprice.log               6.564518e+00
## prdline.my.fctriPadmini:idseq.my                   6.252868e+00
## D.ratio.sum.TfIdf.nwrds                            6.066287e+00
## prdline.my.fctriPadmini 2+:idseq.my                5.982734e+00
## prdline.my.fctriPad 3+:idseq.my                    5.523726e+00
## D.TfIdf.sum.stem.stop.Ratio                        5.419888e+00
## prdline.my.fctriPadmini 2+:startprice.log          5.038098e+00
## prdline.my.fctriPad 2:idseq.my                     4.858351e+00
## prdline.my.fctriPad 1:idseq.my                     4.719666e+00
## D.ratio.nstopwrds.nwrds                            4.379868e+00
## color.fctrWhite                                    3.799246e+00
## D.nstopwrds.log                                    3.773077e+00
## D.TfIdf.sum.post.stop                              3.740726e+00
## cellular.fctr1                                     3.700613e+00
## D.nchrs.log                                        3.554992e+00
## D.TfIdf.sum.post.stem                              3.468698e+00
## D.sum.TfIdf                                        3.445068e+00
## color.fctrBlack                                    3.338438e+00
## storage.fctr16                                     3.176301e+00
## storage.fctr64                                     3.174941e+00
## D.nuppr.log                                        3.127558e+00
## color.fctrSpace Gray                               3.103932e+00
## carrier.fctrUnknown                                3.044874e+00
## D.nwrds.log                                        2.948647e+00
## cellular.fctrUnknown                               2.761831e+00
## storage.fctrUnknown                                2.754111e+00
## condition.fctrNew                                  2.642641e+00
## prdline.my.fctriPadAir:biddable                    2.576494e+00
## storage.fctr32                                     1.946215e+00
## D.npnct11.log                                      1.882235e+00
## condition.fctrNew other (see details)              1.847878e+00
## carrier.fctrVerizon                                1.823145e+00
## condition.fctrFor parts or not working             1.816412e+00
## carrier.fctrAT&T                                   1.628063e+00
## D.terms.n.post.stem                                1.615954e+00
## D.npnct13.log                                      1.600156e+00
## D.terms.n.post.stop.log                            1.597720e+00
## D.terms.n.post.stop                                1.574592e+00
## D.nwrds.unq.log                                    1.487087e+00
## D.terms.n.post.stem.log                            1.486405e+00
## prdline.my.fctriPad 2:biddable                     1.474471e+00
## prdline.my.fctriPad 2:.clusterid.fctr5             1.320461e+00
## condition.fctrManufacturer refurbished             1.305038e+00
## condition.fctrSeller refurbished                   1.292569e+00
## D.npnct15.log                                      1.246443e+00
## D.ndgts.log                                        1.192747e+00
## color.fctrGold                                     1.097366e+00
## prdline.my.fctrUnknown:.clusterid.fctr2            1.045092e+00
## prdline.my.fctriPad 3+                             9.771684e-01
## prdline.my.fctriPad 3+:biddable                    8.655007e-01
## D.terms.n.stem.stop.Ratio                          8.641274e-01
## carrier.fctrSprint                                 8.232701e-01
## prdline.my.fctriPad 1                              8.079325e-01
## carrier.fctrT-Mobile                               7.723365e-01
## prdline.my.fctriPadAir                             7.463619e-01
## prdline.my.fctriPad 1:biddable                     7.225053e-01
## prdline.my.fctriPadmini                            7.172816e-01
## prdline.my.fctriPad 2:.clusterid.fctr4             7.143282e-01
## prdline.my.fctriPadmini:.clusterid.fctr4           6.970082e-01
## prdline.my.fctriPadmini 2+                         6.597996e-01
## prdline.my.fctrUnknown:.clusterid.fctr3            6.523183e-01
## prdline.my.fctriPad 2                              5.835669e-01
## D.npnct16.log                                      5.688453e-01
## D.npnct08.log                                      5.374434e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4            5.364886e-01
## prdline.my.fctriPad 1:.clusterid.fctr2             5.121854e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3            4.923391e-01
## D.npnct14.log                                      4.698068e-01
## D.npnct05.log                                      4.595635e-01
## prdline.my.fctriPadmini 2+:biddable                4.325461e-01
## prdline.my.fctriPadmini:.clusterid.fctr5           3.997570e-01
## prdline.my.fctriPadmini:.clusterid.fctr3           3.986979e-01
## D.npnct06.log                                      3.663155e-01
## D.npnct24.log                                      3.452116e-01
## prdline.my.fctriPadmini:biddable                   3.308949e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3        2.943240e-01
## D.npnct01.log                                      2.925626e-01
## carrier.fctrOther                                  2.773431e-01
## D.npnct12.log                                      2.603176e-01
## prdline.my.fctriPadAir:.clusterid.fctr2            2.558644e-01
## prdline.my.fctriPadmini:.clusterid.fctr2           2.282130e-01
## prdline.my.fctriPad 2:.clusterid.fctr2             2.271737e-01
## prdline.my.fctriPadAir:.clusterid.fctr4            2.087691e-01
## D.npnct03.log                                      2.068028e-01
## prdline.my.fctriPad 1:.clusterid.fctr4             1.849742e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2            1.776392e-01
## D.npnct10.log                                      1.721324e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2        1.653715e-01
## prdline.my.fctriPad 1:.clusterid.fctr3             1.457727e-01
## prdline.my.fctriPadAir:.clusterid.fctr3            1.064812e-01
## prdline.my.fctriPad 2:.clusterid.fctr3             8.190876e-02
## D.npnct09.log                                      2.781385e-03
## D.npnct28.log                                      1.470077e-03
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 45
```

![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-4.png) ![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-5.png) ![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-6.png) ![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-7.png) ![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-8.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1       10001         N                           0.258
## 2       10002         Y                           1.000
## 91      10091         Y                           1.000
## 1396    11397         N                           0.010
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1                             N                                TRUE
## 2                             Y                                TRUE
## 91                            Y                                TRUE
## 1396                          N                                TRUE
##      sold.fctr.predict.Final.rf.error .label
## 1                                   0  10001
## 2                                   0  10002
## 91                                  0  10091
## 1396                                0  11397
## [1] "Inaccurate: "
## [1] UniqueID                            sold.fctr                          
## [3] sold.fctr.predict.Final.rf.prob     sold.fctr.predict.Final.rf         
## [5] sold.fctr.predict.Final.rf.accurate sold.fctr.predict.Final.rf.error   
## <0 rows> (or 0-length row.names)
```

![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-9.png) 

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

![](ebayipads_csmmdl_files/figure-html/fit.data.training_0-10.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 347.735 353.404    5.67
## 16  predict.data.new          9          0 353.405      NA      NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 45
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

![](ebayipads_csmmdl_files/figure-html/predict.data.new-1.png) 

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

![](ebayipads_csmmdl_files/figure-html/predict.data.new-2.png) 

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

![](ebayipads_csmmdl_files/figure-html/predict.data.new-3.png) 

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

![](ebayipads_csmmdl_files/figure-html/predict.data.new-4.png) 

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

![](ebayipads_csmmdl_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.rf.prob
## 1860    11862      <NA>                           0.202
## 1863    11865      <NA>                           0.498
## 1889    11891      <NA>                           0.830
## 2623    12625      <NA>                           0.442
##      sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## 1860                          N                                  NA
## 1863                          N                                  NA
## 1889                          Y                                  NA
## 2623                          N                                  NA
##      sold.fctr.predict.Final.rf.error .label
## 1860                                0  11862
## 1863                                0  11865
## 1889                                0  11891
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
## NA.97        NA      <NA>                              NA
## NA.106       NA      <NA>                              NA
## NA.157       NA      <NA>                              NA
## NA.420       NA      <NA>                              NA
## NA.576       NA      <NA>                              NA
## NA.716       NA      <NA>                              NA
##        sold.fctr.predict.Final.rf sold.fctr.predict.Final.rf.accurate
## NA.97                        <NA>                                  NA
## NA.106                       <NA>                                  NA
## NA.157                       <NA>                                  NA
## NA.420                       <NA>                                  NA
## NA.576                       <NA>                                  NA
## NA.716                       <NA>                                  NA
##        sold.fctr.predict.Final.rf.error
## NA.97                                NA
## NA.106                               NA
## NA.157                               NA
## NA.420                               NA
## NA.576                               NA
## NA.716                               NA
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

![](ebayipads_csmmdl_files/figure-html/predict.data.new-6.png) 

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
    glb_force_prediction_lst[["0"]] <- c(11885, 11907, 12253, 12585)
    for (obs_id in glb_force_prediction_lst[["0"]])
        submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] <-
            max(0, submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] - 0.5)
    
    glb_force_prediction_lst[["1"]] <- c(11871, 11875, 11886, 
                                         11913, 11931, 11937, 11967, 11994, 11999, 
                                         12000, 12021, 12065, 12072, 12111, 12114, 12126, 
                                         12214, 12233, 12278, 12299, 12446, 12491, 
                                         12505, 12576, 12630)
    for (obs_id in glb_force_prediction_lst[["1"]])
        submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] <-
            min(0.9999, submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] + 0.5)
    
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]

rsp_var_out <- paste0(glb_rsp_var_out, glb_fin_mdl_id)
tmp_newobs_df <- subset(glb_newobs_df[, c(glb_id_var, ".grpid", rsp_var_out)],
                        !is.na(.grpid))
tmp_newobs_df <- merge(tmp_newobs_df, dupgrps_df, by=".grpid", all.x=TRUE)
tmp_newobs_df <- merge(tmp_newobs_df, submit_df, by=glb_id_var, all.x = TRUE)
tmp_newobs_df$.err <- ((tmp_newobs_df$Probability1 >= 0.5) & (tmp_newobs_df$sold.0 > 0) |
                       (tmp_newobs_df$Probability1 <= 0.5) & (tmp_newobs_df$sold.1 > 0))
tmp_newobs_df <- orderBy(~UniqueID, subset(tmp_newobs_df, .err == TRUE))
print("Prediction errors in duplicates:")
```

```
## [1] "Prediction errors in duplicates:"
```

```r
print(tmp_newobs_df)
```

```
## [1] UniqueID                   .grpid                    
## [3] sold.fctr.predict.Final.rf sold.0                    
## [5] sold.1                     sold.NA                   
## [7] .freq                      Probability1              
## [9] .err                      
## <0 rows> (or 0-length row.names)
```

```r
if (nrow(tmp_newobs_df) > 0)
    stop("check Prediction errors in duplicates")
#print(dupobs_df[dupobs_df$.grpid == 26, ])

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
## condit  207.8742  condit  496 122
## use     146.4897     use  291 570
## scratch 128.1897 scratch  286 460
## new     125.5866     new  156 349
## good    120.9521    good  197 234
## screen  107.3402  screen  213 461
##             TfIdf    term freq pos
## top     22.638585     top   24 548
## protect  8.235989 protect    8 418
## zagg     4.828616    zagg    5 598
## bent     2.956965    bent    3  66
## compani  2.882106 compani    2 119
## close    1.895930   close    1 115
##             TfIdf    term freq pos
## think   1.2639536   think    1 537
## toddler 1.2639536 toddler    1 546
## tri     1.2639536     tri    1 551
## typic   1.2639536   typic    1 555
## 975     1.1375583     975    1  16
## 79in    0.9479652    79in    1  15
##     UniqueID
## 520    10520
##                                                                                             descr.my
## 520 Apple iPad mini 1st Generation 16GB, Wi- Fi, 7.9in - Space Gray, great condition comes with the 
## [1] 135
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
## [1] 0.5
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
## [1] 974  71
```

```r
print(dsp_models_df)
```

```
##                         model_id max.Accuracy.OOB max.auc.OOB
## 18    All.Interact.X.no.rnorm.rf        0.8045198   0.8380950
## 21                    csm.glmnet        0.8022599   0.8337946
## 11                  All.X.glmnet        0.8000000   0.8455918
## 12          All.X.no.rnorm.rpart        0.7966102   0.8103723
## 17 All.Interact.X.no.rnorm.rpart        0.7966102   0.8103723
## 20                  csm.bayesglm        0.7954802   0.8334377
## 14            All.Interact.X.glm        0.7920904   0.8209653
## 10                All.X.bayesglm        0.7898305   0.8348447
## 15       All.Interact.X.bayesglm        0.7875706   0.8287856
## 8                  Low.cor.X.glm        0.7875706   0.8280616
## 7        Interact.High.cor.Y.glm        0.7853107   0.8396662
## 9                      All.X.glm        0.7853107   0.8270244
## 5                Max.cor.Y.rpart        0.7853107   0.8080205
## 16         All.Interact.X.glmnet        0.7819209   0.8362259
## 6                  Max.cor.Y.glm        0.7807910   0.8402773
## 13             All.X.no.rnorm.rf        0.7807910   0.8397895
## 23                        csm.rf        0.7807910   0.8329551
## 19                       csm.glm        0.7785311   0.8167343
## 22                     csm.rpart        0.7774011   0.8090783
## 4      Max.cor.Y.cv.0.cp.0.rpart        0.7728814   0.8075096
## 1              MFO.myMFO_classfr        0.5367232   0.5000000
## 3           Max.cor.Y.cv.0.rpart        0.5367232   0.5000000
## 2        Random.myrandom_classfr        0.4632768   0.5191913
##    max.Kappa.OOB min.aic.fit opt.prob.threshold.OOB
## 18     0.6023402          NA                    0.5
## 21     0.5980144          NA                    0.6
## 11     0.5914905          NA                    0.6
## 12     0.5861800          NA                    0.6
## 17     0.5861800          NA                    0.6
## 20     0.5835294    967.8072                    0.6
## 14     0.5766981    918.5120                    0.6
## 10     0.5742551    978.4470                    0.5
## 15     0.5702552    961.0875                    0.5
## 8      0.5701108    935.7122                    0.5
## 7      0.5671369    946.9576                    0.4
## 9      0.5659749    948.0891                    0.5
## 5      0.5658292          NA                    0.5
## 16     0.5612566          NA                    0.4
## 6      0.5584673    943.1953                    0.4
## 13     0.5596450          NA                    0.4
## 23     0.5575798          NA                    0.5
## 19     0.5515164    913.0612                    0.5
## 22     0.5506630          NA                    0.3
## 4      0.5406159          NA                    0.6
## 1      0.0000000          NA                    0.5
## 3      0.0000000          NA                    0.5
## 2      0.0000000          NA                    0.4
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_var)) {
        stop("not implemented yet")
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
##         N 423  52
##         Y 121 289
##    prdline.my .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 3      iPad 2    171    154      0.1929825      0.1932203
## 5     iPadAir    151    137      0.1716792      0.1706215
## 4     iPad 3+    136    123      0.1541353      0.1536723
## 1     Unknown     97     87      0.1090226      0.1096045
## 7 iPadmini 2+    104     94      0.1177945      0.1175141
## 6    iPadmini    127    114      0.1428571      0.1435028
## 2      iPad 1     99     89      0.1115288      0.1118644
##   accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 3                 34               137        0.8011696
## 5                 33               118        0.7814570
## 4                 26               110        0.8088235
## 1                 22                75        0.7731959
## 7                 21                83        0.7980769
## 6                 19               108        0.8503937
## 2                 18                81        0.8181818
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
##                                             All.Interact.X.no.rnorm.rf.importance
## biddable                                                             1.000000e+02
## startprice.log                                                       9.404554e+01
## idseq.my                                                             5.829503e+01
## prdline.my.fctriPadmini:startprice.log                               9.239296e+00
## prdline.my.fctriPadAir:startprice.log                                8.509567e+00
## prdline.my.fctriPadmini:idseq.my                                     8.476034e+00
## prdline.my.fctriPad 3+:startprice.log                                8.386078e+00
## prdline.my.fctriPadAir:idseq.my                                      8.220315e+00
## D.ratio.sum.TfIdf.nwrds                                              7.950279e+00
## prdline.my.fctriPad 1:startprice.log                                 7.037547e+00
## D.TfIdf.sum.stem.stop.Ratio                                          6.571168e+00
## prdline.my.fctriPad 3+:idseq.my                                      6.559448e+00
## prdline.my.fctriPadmini 2+:idseq.my                                  5.226984e+00
## prdline.my.fctriPad 1:idseq.my                                       5.120671e+00
## D.sum.TfIdf                                                          4.617664e+00
## D.TfIdf.sum.post.stop                                                4.617532e+00
## prdline.my.fctriPadmini 2+:startprice.log                            4.534814e+00
## D.TfIdf.sum.post.stem                                                4.392375e+00
## color.fctrBlack                                                      4.334881e+00
## storage.fctrUnknown                                                  4.285224e+00
## D.ratio.nstopwrds.nwrds                                              4.237746e+00
## storage.fctr64                                                       4.189194e+00
## prdline.my.fctriPad 2:startprice.log                                 3.796520e+00
## color.fctrWhite                                                      3.720983e+00
## D.nstopwrds.log                                                      3.553877e+00
## D.nchrs.log                                                          3.449126e+00
## cellular.fctr1                                                       3.344855e+00
## storage.fctr16                                                       3.326872e+00
## D.nuppr.log                                                          3.259898e+00
## prdline.my.fctriPad 2:idseq.my                                       3.156370e+00
## cellular.fctrUnknown                                                 2.795316e+00
## carrier.fctrUnknown                                                  2.794373e+00
## color.fctrSpace Gray                                                 2.789616e+00
## D.nwrds.log                                                          2.767589e+00
## prdline.my.fctriPadAir:biddable                                      2.717394e+00
## prdline.my.fctriPad 2:.clusterid.fctr5                               2.526636e+00
## condition.fctrNew                                                    2.473749e+00
## carrier.fctrVerizon                                                  2.301742e+00
## condition.fctrNew other (see details)                                2.269510e+00
## D.nwrds.unq.log                                                      1.998496e+00
## D.terms.n.post.stem                                                  1.981725e+00
## storage.fctr32                                                       1.879219e+00
## condition.fctrManufacturer refurbished                               1.872430e+00
## prdline.my.fctriPad 2:biddable                                       1.806590e+00
## D.terms.n.post.stem.log                                              1.800677e+00
## carrier.fctrAT&T                                                     1.799152e+00
## prdline.my.fctriPad 1:biddable                                       1.680080e+00
## D.terms.n.post.stop                                                  1.637758e+00
## condition.fctrFor parts or not working                               1.615396e+00
## prdline.my.fctrUnknown:.clusterid.fctr2                              1.613022e+00
## condition.fctrSeller refurbished                                     1.542534e+00
## D.terms.n.post.stop.log                                              1.368098e+00
## D.terms.n.stem.stop.Ratio                                            1.365550e+00
## D.ndgts.log                                                          1.233854e+00
## prdline.my.fctriPad 3+                                               1.210476e+00
## D.npnct13.log                                                        1.184904e+00
## D.npnct11.log                                                        1.181740e+00
## color.fctrGold                                                       1.088644e+00
## prdline.my.fctriPad 3+:biddable                                      9.996656e-01
## prdline.my.fctriPad 1                                                9.697913e-01
## prdline.my.fctriPadmini                                              8.895090e-01
## carrier.fctrSprint                                                   8.170434e-01
## D.npnct08.log                                                        7.562689e-01
## prdline.my.fctriPadmini:.clusterid.fctr3                             7.562502e-01
## prdline.my.fctriPad 1:.clusterid.fctr2                               7.415946e-01
## D.npnct05.log                                                        7.372404e-01
## prdline.my.fctriPad 2                                                7.072275e-01
## prdline.my.fctriPadAir                                               6.865845e-01
## D.npnct01.log                                                        5.928110e-01
## prdline.my.fctriPadmini:biddable                                     5.802216e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4                              5.517229e-01
## prdline.my.fctriPadmini 2+                                           5.473725e-01
## carrier.fctrT-Mobile                                                 5.325882e-01
## D.npnct15.log                                                        5.300095e-01
## prdline.my.fctriPadmini 2+:biddable                                  4.911016e-01
## prdline.my.fctriPad 1:.clusterid.fctr4                               4.806325e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2                          4.788083e-01
## D.npnct12.log                                                        4.656639e-01
## D.npnct16.log                                                        3.934201e-01
## D.npnct24.log                                                        3.916757e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3                              3.905739e-01
## prdline.my.fctriPadAir:.clusterid.fctr3                              3.325317e-01
## D.npnct03.log                                                        3.227649e-01
## prdline.my.fctriPad 2:.clusterid.fctr4                               3.034445e-01
## prdline.my.fctrUnknown:.clusterid.fctr3                              3.031782e-01
## D.npnct14.log                                                        2.915722e-01
## prdline.my.fctriPadmini:.clusterid.fctr2                             2.526365e-01
## prdline.my.fctriPad 2:.clusterid.fctr2                               2.515994e-01
## D.npnct06.log                                                        2.047493e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3                          1.922444e-01
## carrier.fctrOther                                                    1.853682e-01
## prdline.my.fctriPadmini:.clusterid.fctr4                             1.657237e-01
## prdline.my.fctriPadAir:.clusterid.fctr4                              1.549737e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2                              1.245769e-01
## prdline.my.fctriPadAir:.clusterid.fctr2                              1.149917e-01
## prdline.my.fctriPad 1:.clusterid.fctr3                               7.867550e-02
## prdline.my.fctriPad 2:.clusterid.fctr3                               6.143237e-02
## prdline.my.fctriPadmini:.clusterid.fctr5                             3.915683e-02
## D.npnct10.log                                                        1.650574e-02
## D.npnct09.log                                                        7.666838e-03
## D.npnct28.log                                                        5.742064e-03
## prdline.my.fctrUnknown:.clusterid.fctr4                              0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5                               0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5                              0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4                          0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5                          0.000000e+00
##                                               importance
## biddable                                    1.000000e+02
## startprice.log                              8.838447e+01
## idseq.my                                    5.277094e+01
## prdline.my.fctriPadmini:startprice.log      9.683368e+00
## prdline.my.fctriPadAir:startprice.log       7.987419e+00
## prdline.my.fctriPadmini:idseq.my            6.252868e+00
## prdline.my.fctriPad 3+:startprice.log       7.886110e+00
## prdline.my.fctriPadAir:idseq.my             8.170386e+00
## D.ratio.sum.TfIdf.nwrds                     6.066287e+00
## prdline.my.fctriPad 1:startprice.log        9.553179e+00
## D.TfIdf.sum.stem.stop.Ratio                 5.419888e+00
## prdline.my.fctriPad 3+:idseq.my             5.523726e+00
## prdline.my.fctriPadmini 2+:idseq.my         5.982734e+00
## prdline.my.fctriPad 1:idseq.my              4.719666e+00
## D.sum.TfIdf                                 3.445068e+00
## D.TfIdf.sum.post.stop                       3.740726e+00
## prdline.my.fctriPadmini 2+:startprice.log   5.038098e+00
## D.TfIdf.sum.post.stem                       3.468698e+00
## color.fctrBlack                             3.338438e+00
## storage.fctrUnknown                         2.754111e+00
## D.ratio.nstopwrds.nwrds                     4.379868e+00
## storage.fctr64                              3.174941e+00
## prdline.my.fctriPad 2:startprice.log        6.564518e+00
## color.fctrWhite                             3.799246e+00
## D.nstopwrds.log                             3.773077e+00
## D.nchrs.log                                 3.554992e+00
## cellular.fctr1                              3.700613e+00
## storage.fctr16                              3.176301e+00
## D.nuppr.log                                 3.127558e+00
## prdline.my.fctriPad 2:idseq.my              4.858351e+00
## cellular.fctrUnknown                        2.761831e+00
## carrier.fctrUnknown                         3.044874e+00
## color.fctrSpace Gray                        3.103932e+00
## D.nwrds.log                                 2.948647e+00
## prdline.my.fctriPadAir:biddable             2.576494e+00
## prdline.my.fctriPad 2:.clusterid.fctr5      1.320461e+00
## condition.fctrNew                           2.642641e+00
## carrier.fctrVerizon                         1.823145e+00
## condition.fctrNew other (see details)       1.847878e+00
## D.nwrds.unq.log                             1.487087e+00
## D.terms.n.post.stem                         1.615954e+00
## storage.fctr32                              1.946215e+00
## condition.fctrManufacturer refurbished      1.305038e+00
## prdline.my.fctriPad 2:biddable              1.474471e+00
## D.terms.n.post.stem.log                     1.486405e+00
## carrier.fctrAT&T                            1.628063e+00
## prdline.my.fctriPad 1:biddable              7.225053e-01
## D.terms.n.post.stop                         1.574592e+00
## condition.fctrFor parts or not working      1.816412e+00
## prdline.my.fctrUnknown:.clusterid.fctr2     1.045092e+00
## condition.fctrSeller refurbished            1.292569e+00
## D.terms.n.post.stop.log                     1.597720e+00
## D.terms.n.stem.stop.Ratio                   8.641274e-01
## D.ndgts.log                                 1.192747e+00
## prdline.my.fctriPad 3+                      9.771684e-01
## D.npnct13.log                               1.600156e+00
## D.npnct11.log                               1.882235e+00
## color.fctrGold                              1.097366e+00
## prdline.my.fctriPad 3+:biddable             8.655007e-01
## prdline.my.fctriPad 1                       8.079325e-01
## prdline.my.fctriPadmini                     7.172816e-01
## carrier.fctrSprint                          8.232701e-01
## D.npnct08.log                               5.374434e-01
## prdline.my.fctriPadmini:.clusterid.fctr3    3.986979e-01
## prdline.my.fctriPad 1:.clusterid.fctr2      5.121854e-01
## D.npnct05.log                               4.595635e-01
## prdline.my.fctriPad 2                       5.835669e-01
## prdline.my.fctriPadAir                      7.463619e-01
## D.npnct01.log                               2.925626e-01
## prdline.my.fctriPadmini:biddable            3.308949e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4     5.364886e-01
## prdline.my.fctriPadmini 2+                  6.597996e-01
## carrier.fctrT-Mobile                        7.723365e-01
## D.npnct15.log                               1.246443e+00
## prdline.my.fctriPadmini 2+:biddable         4.325461e-01
## prdline.my.fctriPad 1:.clusterid.fctr4      1.849742e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 1.653715e-01
## D.npnct12.log                               2.603176e-01
## D.npnct16.log                               5.688453e-01
## D.npnct24.log                               3.452116e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3     4.923391e-01
## prdline.my.fctriPadAir:.clusterid.fctr3     1.064812e-01
## D.npnct03.log                               2.068028e-01
## prdline.my.fctriPad 2:.clusterid.fctr4      7.143282e-01
## prdline.my.fctrUnknown:.clusterid.fctr3     6.523183e-01
## D.npnct14.log                               4.698068e-01
## prdline.my.fctriPadmini:.clusterid.fctr2    2.282130e-01
## prdline.my.fctriPad 2:.clusterid.fctr2      2.271737e-01
## D.npnct06.log                               3.663155e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 2.943240e-01
## carrier.fctrOther                           2.773431e-01
## prdline.my.fctriPadmini:.clusterid.fctr4    6.970082e-01
## prdline.my.fctriPadAir:.clusterid.fctr4     2.087691e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2     1.776392e-01
## prdline.my.fctriPadAir:.clusterid.fctr2     2.558644e-01
## prdline.my.fctriPad 1:.clusterid.fctr3      1.457727e-01
## prdline.my.fctriPad 2:.clusterid.fctr3      8.190876e-02
## prdline.my.fctriPadmini:.clusterid.fctr5    3.997570e-01
## D.npnct10.log                               1.721324e-01
## D.npnct09.log                               2.781385e-03
## D.npnct28.log                               1.470077e-03
## prdline.my.fctrUnknown:.clusterid.fctr4     0.000000e+00
## prdline.my.fctrUnknown:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPad 1:.clusterid.fctr5      0.000000e+00
## prdline.my.fctriPad 3+:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadAir:.clusterid.fctr5     0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr4 0.000000e+00
## prdline.my.fctriPadmini 2+:.clusterid.fctr5 0.000000e+00
##                                             Final.rf.importance
## biddable                                           1.000000e+02
## startprice.log                                     8.838447e+01
## idseq.my                                           5.277094e+01
## prdline.my.fctriPadmini:startprice.log             9.683368e+00
## prdline.my.fctriPadAir:startprice.log              7.987419e+00
## prdline.my.fctriPadmini:idseq.my                   6.252868e+00
## prdline.my.fctriPad 3+:startprice.log              7.886110e+00
## prdline.my.fctriPadAir:idseq.my                    8.170386e+00
## D.ratio.sum.TfIdf.nwrds                            6.066287e+00
## prdline.my.fctriPad 1:startprice.log               9.553179e+00
## D.TfIdf.sum.stem.stop.Ratio                        5.419888e+00
## prdline.my.fctriPad 3+:idseq.my                    5.523726e+00
## prdline.my.fctriPadmini 2+:idseq.my                5.982734e+00
## prdline.my.fctriPad 1:idseq.my                     4.719666e+00
## D.sum.TfIdf                                        3.445068e+00
## D.TfIdf.sum.post.stop                              3.740726e+00
## prdline.my.fctriPadmini 2+:startprice.log          5.038098e+00
## D.TfIdf.sum.post.stem                              3.468698e+00
## color.fctrBlack                                    3.338438e+00
## storage.fctrUnknown                                2.754111e+00
## D.ratio.nstopwrds.nwrds                            4.379868e+00
## storage.fctr64                                     3.174941e+00
## prdline.my.fctriPad 2:startprice.log               6.564518e+00
## color.fctrWhite                                    3.799246e+00
## D.nstopwrds.log                                    3.773077e+00
## D.nchrs.log                                        3.554992e+00
## cellular.fctr1                                     3.700613e+00
## storage.fctr16                                     3.176301e+00
## D.nuppr.log                                        3.127558e+00
## prdline.my.fctriPad 2:idseq.my                     4.858351e+00
## cellular.fctrUnknown                               2.761831e+00
## carrier.fctrUnknown                                3.044874e+00
## color.fctrSpace Gray                               3.103932e+00
## D.nwrds.log                                        2.948647e+00
## prdline.my.fctriPadAir:biddable                    2.576494e+00
## prdline.my.fctriPad 2:.clusterid.fctr5             1.320461e+00
## condition.fctrNew                                  2.642641e+00
## carrier.fctrVerizon                                1.823145e+00
## condition.fctrNew other (see details)              1.847878e+00
## D.nwrds.unq.log                                    1.487087e+00
## D.terms.n.post.stem                                1.615954e+00
## storage.fctr32                                     1.946215e+00
## condition.fctrManufacturer refurbished             1.305038e+00
## prdline.my.fctriPad 2:biddable                     1.474471e+00
## D.terms.n.post.stem.log                            1.486405e+00
## carrier.fctrAT&T                                   1.628063e+00
## prdline.my.fctriPad 1:biddable                     7.225053e-01
## D.terms.n.post.stop                                1.574592e+00
## condition.fctrFor parts or not working             1.816412e+00
## prdline.my.fctrUnknown:.clusterid.fctr2            1.045092e+00
## condition.fctrSeller refurbished                   1.292569e+00
## D.terms.n.post.stop.log                            1.597720e+00
## D.terms.n.stem.stop.Ratio                          8.641274e-01
## D.ndgts.log                                        1.192747e+00
## prdline.my.fctriPad 3+                             9.771684e-01
## D.npnct13.log                                      1.600156e+00
## D.npnct11.log                                      1.882235e+00
## color.fctrGold                                     1.097366e+00
## prdline.my.fctriPad 3+:biddable                    8.655007e-01
## prdline.my.fctriPad 1                              8.079325e-01
## prdline.my.fctriPadmini                            7.172816e-01
## carrier.fctrSprint                                 8.232701e-01
## D.npnct08.log                                      5.374434e-01
## prdline.my.fctriPadmini:.clusterid.fctr3           3.986979e-01
## prdline.my.fctriPad 1:.clusterid.fctr2             5.121854e-01
## D.npnct05.log                                      4.595635e-01
## prdline.my.fctriPad 2                              5.835669e-01
## prdline.my.fctriPadAir                             7.463619e-01
## D.npnct01.log                                      2.925626e-01
## prdline.my.fctriPadmini:biddable                   3.308949e-01
## prdline.my.fctriPad 3+:.clusterid.fctr4            5.364886e-01
## prdline.my.fctriPadmini 2+                         6.597996e-01
## carrier.fctrT-Mobile                               7.723365e-01
## D.npnct15.log                                      1.246443e+00
## prdline.my.fctriPadmini 2+:biddable                4.325461e-01
## prdline.my.fctriPad 1:.clusterid.fctr4             1.849742e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr2        1.653715e-01
## D.npnct12.log                                      2.603176e-01
## D.npnct16.log                                      5.688453e-01
## D.npnct24.log                                      3.452116e-01
## prdline.my.fctriPad 3+:.clusterid.fctr3            4.923391e-01
## prdline.my.fctriPadAir:.clusterid.fctr3            1.064812e-01
## D.npnct03.log                                      2.068028e-01
## prdline.my.fctriPad 2:.clusterid.fctr4             7.143282e-01
## prdline.my.fctrUnknown:.clusterid.fctr3            6.523183e-01
## D.npnct14.log                                      4.698068e-01
## prdline.my.fctriPadmini:.clusterid.fctr2           2.282130e-01
## prdline.my.fctriPad 2:.clusterid.fctr2             2.271737e-01
## D.npnct06.log                                      3.663155e-01
## prdline.my.fctriPadmini 2+:.clusterid.fctr3        2.943240e-01
## carrier.fctrOther                                  2.773431e-01
## prdline.my.fctriPadmini:.clusterid.fctr4           6.970082e-01
## prdline.my.fctriPadAir:.clusterid.fctr4            2.087691e-01
## prdline.my.fctriPad 3+:.clusterid.fctr2            1.776392e-01
## prdline.my.fctriPadAir:.clusterid.fctr2            2.558644e-01
## prdline.my.fctriPad 1:.clusterid.fctr3             1.457727e-01
## prdline.my.fctriPad 2:.clusterid.fctr3             8.190876e-02
## prdline.my.fctriPadmini:.clusterid.fctr5           3.997570e-01
## D.npnct10.log                                      1.721324e-01
## D.npnct09.log                                      2.781385e-03
## D.npnct28.log                                      1.470077e-03
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

![](ebayipads_csmmdl_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 540 258
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
## 16     predict.data.new          9          0 353.405 359.551   6.146
## 17 display.session.info         10          0 359.551      NA      NA
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
## 11              fit.models          7          1 123.330 284.939 161.609
## 5         extract.features          3          0  20.381  87.484  67.103
## 12              fit.models          7          2 284.939 314.937  29.998
## 14       fit.data.training          8          0 319.943 347.734  27.791
## 10              fit.models          7          0  96.689 123.330  26.641
## 16        predict.data.new          9          0 353.405 359.551   6.146
## 15       fit.data.training          8          1 347.735 353.404   5.670
## 13              fit.models          7          3 314.938 319.942   5.004
## 1              import.data          1          0  11.308  15.312   4.004
## 8          select.features          5          0  91.972  95.802   3.830
## 2             inspect.data          2          0  15.312  19.122   3.810
## 7      manage.missing.data          4          1  88.527  91.971   3.445
## 6             cluster.data          4          0  87.484  88.527   1.043
## 9  partition.data.training          6          0  95.802  96.688   0.887
## 3               scrub.data          2          1  19.123  19.836   0.713
## 4           transform.data          2          2  19.837  20.381   0.544
##    duration
## 11  161.609
## 5    67.103
## 12   29.998
## 14   27.791
## 10   26.641
## 16    6.146
## 15    5.669
## 13    5.004
## 1     4.004
## 8     3.830
## 2     3.810
## 7     3.444
## 6     1.043
## 9     0.886
## 3     0.713
## 4     0.544
## [1] "Total Elapsed Time: 359.551 secs"
```

![](ebayipads_csmmdl_files/figure-html/display.session.info-1.png) 

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
