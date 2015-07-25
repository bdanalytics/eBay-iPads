# eBay:iPads:: sold classification:: dupobs
bdanalytics  

**  **    
**Date: (Sat) Jul 25, 2015**    

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
glb_out_pfx <- "dupobs_"
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
        mod_raw <- gsub(" ans ", " and ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" backplate", " back plate", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" bend ", " bent ", mod_raw, ignore.case=TRUE);         
        mod_raw <- gsub("Best Buy", "BestBuy", mod_raw, ignore.case=TRUE);        
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
        mod_raw <- gsub(" devices", " device", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" Digi\\.", " Digitizer\\.", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" display\\.New ", " display\\. New ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" displays", " display", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" drop ", " dropped ", mod_raw, ignore.case=TRUE);         
        mod_raw <- gsub(" effect ", " affect ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" Excellant ", " Excellent ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" excellently", " excellent", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" feels ", " feel ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" fineiCloud ", " fine iCloud ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("^Gentle ", "Gently ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" GREAT\\.SCreen ", " GREAT\\. SCreen ", mod_raw,
                        ignore.case=TRUE);        
        mod_raw <- gsub(" Framing ", " Frame ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub("iCL0UD", "iCL0UD", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub("inivisible", "invisible", mod_raw, ignore.case=TRUE);         
        mod_raw <- gsub("^iPad Black 3rd generation ", "iPad 3 Black ", mod_raw,
                        ignore.case=TRUE);  
        mod_raw <- gsub(" install\\. ", " installed\\. ", mod_raw, ignore.case=TRUE);   
        mod_raw <- gsub(" manuals ", " manual ", mod_raw, ignore.case=TRUE);        
        mod_raw <- gsub(" mars ", " marks ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" MINT\\.wiped ", " MINT\\. wiped ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" NEW\\!(SCREEN|ONE) ", " NEW\\! \\1 ", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" new looking$", " looks new", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" newer ", " new ", mod_raw, ignore.case=TRUE);                
        mod_raw <- gsub(" opening", " opened", mod_raw, ignore.case=TRUE);
        mod_raw <- gsub(" operated", " operational", mod_raw, ignore.case=TRUE);        
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
                                        ,"a1314","a1430","a1432"
                            ,"abused","across","adaptor","add","antenna","area","arizona"
                                        ,"beginning","bidder","bound"
                                ,"changed","chrome","consumer","contents","control","cream"
    ,"decent","defender","depicted","disclaimer","distressed","divider"
        ,"dlxnqat9g5wt","done","dont","dust","duty"
                                        ,"erased","ereader","exact"
                                        ,"film","flickers"
                                        ,"generic","genuine","glitter"
                                        ,"hdmi","higher","hospital"
                                        ,"impact","instead","interior"
                                        ,"jack","july"
                                        ,"keeps"
    ,"late","letters","lifting","limited","lining","liquid"
        ,"local","long","longer","looping","loss"
                    ,"mb292ll","mc707ll","mc916ll","mc991ll","md789ll","mf432ll","mgye2ll"
                    ,"mind","mixed"
                                        ,"neither","none","november"
                                        ,"occasional","outside"
                ,"period","pet","played","plug","poor","portion","pouch","price","provided"
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

![](ebayipads_dupobs_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 9.156  NA      NA
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
## 1  import.data          1          0  9.156 14.873   5.717
## 2 inspect.data          2          0 14.873     NA      NA
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

![](ebayipads_dupobs_files/figure-html/inspect.data-1.png) 

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

![](ebayipads_dupobs_files/figure-html/inspect.data-2.png) 

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

![](ebayipads_dupobs_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: .rnorm"
```

![](ebayipads_dupobs_files/figure-html/inspect.data-4.png) 

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
## 2 inspect.data          2          0 14.873 18.948   4.075
## 3   scrub.data          2          1 18.948     NA      NA
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
##   1        288    0     4     36       27     172     196
##   Unknown    4    4     2      0        0     331       0
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
##   1        292    0     6     36       27     172     196
##   Unknown    0    0     0      0        0     331       0
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 18.948 19.654   0.706
## 4 transform.data          2          2 19.655     NA      NA
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
for (obs_id in c(10178, 10948, 11514, 11904, 12157, 12210)) {
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
## 4   transform.data          2          2 19.655 20.128   0.473
## 5 extract.features          3          0 20.128     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 20.135  NA      NA
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
## 1                extract.features_bgn          1          0 20.135 20.154
## 2 extract.features_factorize.str.vars          2          0 20.154     NA
##   elapsed
## 1   0.019
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
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 20.154 20.519
## 3       extract.features_process.text          3          0 20.520     NA
##   elapsed
## 2   0.365
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
## 3          0 20.520 22.231   1.711
## 4          1 22.231     NA      NA
## [1] "Remaining compound terms in descr.my: "
##                                                    label step_major
## 4 extract.features_process.text_reporting_compound_terms          3
## 5                          extract.features_build.corpus          4
##   step_minor    bgn    end elapsed
## 4          1 22.231 22.236   0.005
## 5          0 22.237     NA      NA
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
## [1] "Rows: 782; Cols: 4"
##              TfIdf      term freq pos
## condition 207.6333 condition  497 160
## new       125.3927       new  156 472
## used      122.9018      used  239 747
## good      120.6855      good  197 316
## scratches 113.6830 scratches  254 612
## screen    105.9501    screen  210 614
##               TfIdf      term freq pos
## read      28.187366      read   31 573
## installed  6.181673 installed    7 362
## outer      5.432193     outer    5 498
## otterbox   5.260069  otterbox    5 497
## else       2.991578      else    3 243
## connect    1.625083   connect    1 162
##            TfIdf   term freq pos
## high   1.1375583   high    1 341
## hole   1.1375583   hole    1 343
## lights 1.1375583 lights    1 405
## person 1.1375583 person    1 518
## places 1.1375583 places    1 534
## 79in   0.9479652   79in    1  16
##            TfIdf   term freq pos
## high   1.1375583   high    1 341
## hole   1.1375583   hole    1 343
## lights 1.1375583 lights    1 405
## person 1.1375583 person    1 518
## places 1.1375583 places    1 534
## 79in   0.9479652   79in    1  16
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
## [1] "Rows: 641; Cols: 4"
##            TfIdf    term freq pos
## condit  207.6181  condit  495 133
## use     145.9567     use  290 611
## scratch 127.9087 scratch  286 499
## new     125.3927     new  156 385
## good    120.7748    good  197 260
## screen  106.8143  screen  213 500
##              TfIdf     term freq pos
## warranti 16.585575 warranti   24 621
## leather   2.848180  leather    3 324
## card      2.485311     card    3 109
## offer     2.305685    offer    2 395
## known     1.895930    known    1 318
## ope       1.263954      ope    1 400
##           TfIdf  term freq pos
## final 1.1375583 final    1 228
## fold  1.1375583  fold    1 236
## goe   1.1375583   goe    1 258
## high  1.1375583  high    1 280
## hole  1.1375583  hole    1 282
## 79in  0.9479652  79in    1  16
##           TfIdf  term freq pos
## final 1.1375583 final    1 228
## fold  1.1375583  fold    1 236
## goe   1.1375583   goe    1 258
## high  1.1375583  high    1 280
## hole  1.1375583  hole    1 282
## 79in  0.9479652  79in    1  16
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
## terms.n.post.stop         -0.083226560
## TfIdf.sum.post.stop       -0.032554183
## terms.n.post.stem         -0.083066302
## TfIdf.sum.post.stem       -0.035164455
## terms.n.stem.stop.Ratio    0.016573174
## TfIdf.sum.stem.stop.Ratio -0.007890892
##                           label step_major step_minor    bgn    end
## 5 extract.features_build.corpus          4          0 22.237 32.907
## 6  extract.features_extract.DTM          5          0 32.908     NA
##   elapsed
## 5  10.671
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
## 6 extract.features_extract.DTM          5          0 32.908 34.179   1.271
## 7  extract.features_report.DTM          6          0 34.180     NA      NA
## [1] "Reporting TfIDf terms for descr.my..."
## [1] "   Full TermMatrix:"
## <<DocumentTermMatrix (documents: 2657, terms: 641)>>
## Non-/sparse entries: 8314/1694823
## Sparsity           : 100%
## Maximal term length: 16
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
## [1] "   Sparse TermMatrix:"
## <<DocumentTermMatrix (documents: 2657, terms: 8)>>
## Non-/sparse entries: 2067/19189
## Sparsity           : 90%
## Maximal term length: 7
## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
```

```
## Warning in myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full",
## colorcol_name = "in.sprs"): converting in.sprs to class:factor
```

![](ebayipads_dupobs_files/figure-html/extract.features-1.png) 

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](ebayipads_dupobs_files/figure-html/extract.features-2.png) ![](ebayipads_dupobs_files/figure-html/extract.features-3.png) 

```
## Warning in rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df,
## terms_TfIdf_df): object 'full_TfIdf_mtrx' not found
```

```
##                         label step_major step_minor    bgn    end elapsed
## 7 extract.features_report.DTM          6          0 34.180 36.272   2.092
## 8   extract.features_bind.DTM          7          0 36.272     NA      NA
## [1] "Binding DTM for descr.my..."
##                       label step_major step_minor    bgn   end elapsed
## 8 extract.features_bind.DTM          7          0 36.272 36.94   0.668
## 9 extract.features_bind.DXM          8          0 36.940    NA      NA
## [1] "Binding DXM for descr.my..."
```

```
## Warning in rm(log_X_df, txt_X_df): object 'log_X_df' not found
```

![](ebayipads_dupobs_files/figure-html/extract.features-4.png) 

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
## 9  extract.features_bind.DXM          8          0 36.940 75.637  38.697
## 10      extract.features_end          9          0 75.637     NA      NA
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
## 9          0 36.940 75.637  38.697   38.697
## 5          0 22.237 32.907  10.671   10.670
## 7          0 34.180 36.272   2.092    2.092
## 3          0 20.520 22.231   1.711    1.711
## 6          0 32.908 34.179   1.271    1.271
## 8          0 36.272 36.940   0.668    0.668
## 2          0 20.154 20.519   0.365    0.365
## 1          0 20.135 20.154   0.019    0.019
## 4          1 22.231 22.236   0.005    0.005
## [1] "Total Elapsed Time: 75.637 secs"
```

![](ebayipads_dupobs_files/figure-html/extract.features-5.png) 

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

![](ebayipads_dupobs_files/figure-html/extract.features-6.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 20.128 76.969  56.841
## 6     cluster.data          4          0 76.969     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn   end elapsed
## 6        cluster.data          4          0 76.969 77.94   0.971
## 7 manage.missing.data          4          1 77.941    NA      NA
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
##           cellular.fctr     D.terms.n.post.stop   D.TfIdf.sum.post.stop 
##                    1597                    1521                    1521 
##     D.terms.n.post.stem   D.TfIdf.sum.post.stem              D.T.condit 
##                    1521                    1521                    2162 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2367                    2371                    2501 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2460                    2444                    2425 
##               D.T.great                D.T.work               D.T.excel 
##                    2532                    2459                    2558 
##             D.nwrds.log         D.nwrds.unq.log             D.sum.TfIdf 
##                    1520                    1521                    1521 
## D.ratio.sum.TfIdf.nwrds             D.nchrs.log             D.nuppr.log 
##                    1521                    1520                    1522 
##             D.ndgts.log           D.npnct01.log           D.npnct02.log 
##                    2426                    2579                    2657 
##           D.npnct03.log           D.npnct04.log           D.npnct05.log 
##                    2614                    2657                    2592 
##           D.npnct06.log           D.npnct07.log           D.npnct08.log 
##                    2554                    2656                    2581 
##           D.npnct09.log           D.npnct10.log           D.npnct11.log 
##                    2641                    2648                    2301 
##           D.npnct12.log           D.npnct13.log           D.npnct14.log 
##                    2537                    1932                    2582 
##           D.npnct15.log           D.npnct16.log           D.npnct17.log 
##                    2637                    2546                    2657 
##           D.npnct18.log           D.npnct19.log           D.npnct20.log 
##                    2656                    2657                    2657 
##           D.npnct21.log           D.npnct22.log           D.npnct23.log 
##                    2657                    2657                    2657 
##           D.npnct24.log           D.npnct25.log           D.npnct26.log 
##                    1520                    2657                    2657 
##           D.npnct27.log           D.npnct28.log           D.npnct29.log 
##                    2657                    2649                    2657 
##           D.npnct30.log         D.nstopwrds.log                D.P.http 
##                    2657                    1663                    2657 
##                D.P.mini                 D.P.air 
##                    2623                    2637 
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
##           cellular.fctr     D.terms.n.post.stop   D.TfIdf.sum.post.stop 
##                    1597                    1521                    1521 
##     D.terms.n.post.stem   D.TfIdf.sum.post.stem              D.T.condit 
##                    1521                    1521                    2162 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2367                    2371                    2501 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2460                    2444                    2425 
##               D.T.great                D.T.work               D.T.excel 
##                    2532                    2459                    2558 
##             D.nwrds.log         D.nwrds.unq.log             D.sum.TfIdf 
##                    1520                    1521                    1521 
## D.ratio.sum.TfIdf.nwrds             D.nchrs.log             D.nuppr.log 
##                    1521                    1520                    1522 
##             D.ndgts.log           D.npnct01.log           D.npnct02.log 
##                    2426                    2579                    2657 
##           D.npnct03.log           D.npnct04.log           D.npnct05.log 
##                    2614                    2657                    2592 
##           D.npnct06.log           D.npnct07.log           D.npnct08.log 
##                    2554                    2656                    2581 
##           D.npnct09.log           D.npnct10.log           D.npnct11.log 
##                    2641                    2648                    2301 
##           D.npnct12.log           D.npnct13.log           D.npnct14.log 
##                    2537                    1932                    2582 
##           D.npnct15.log           D.npnct16.log           D.npnct17.log 
##                    2637                    2546                    2657 
##           D.npnct18.log           D.npnct19.log           D.npnct20.log 
##                    2656                    2657                    2657 
##           D.npnct21.log           D.npnct22.log           D.npnct23.log 
##                    2657                    2657                    2657 
##           D.npnct24.log           D.npnct25.log           D.npnct26.log 
##                    1520                    2657                    2657 
##           D.npnct27.log           D.npnct28.log           D.npnct29.log 
##                    2657                    2649                    2657 
##           D.npnct30.log         D.nstopwrds.log                D.P.http 
##                    2657                    1663                    2657 
##                D.P.mini                 D.P.air 
##                    2623                    2637 
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
## 389     10389         Y    Unknown
## 1618    11619         Y    Unknown
##                                                                                     descr.my
## 389                                                                       Working condition!
## 1618 Please note that this unit is in working condition, but is locked with another Apple ID
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 389   1.2121489       0           0       0        0          0        0
## 1618  0.3030372       0           0       0        0          0        0
##      D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 389          0 1.8731129         0        0        0       0
## 1618         0 0.4682782         0        0        0       0
```

![](ebayipads_dupobs_files/figure-html/cluster.data-1.png) 

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
## 9   0.0000000       0           0       0 0.0000000          0        0
## 13  0.2203907       0           0       0 0.3412301          0        0
##    D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 9          0 0.000000         0        0        0       0
## 13         0 0.340566         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my                          descr.my
## 165     10165         Y     iPad 1 Excellent condition see pictures.
## 1980    11982      <NA>     iPad 1 Excellent condition see pictures.
##      D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 165   0.6060744       0           0       0        0          0        0
## 1980  0.6060744       0           0       0        0          0        0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 165          0        0  1.186556        0        0       0
## 1980         0        0  1.186556        0        0       0
```

![](ebayipads_dupobs_files/figure-html/cluster.data-2.png) 

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
## 1  0.8080993 0.0000000   0.0000000       0        0  0.0000000 1.172534
## 2  0.0000000 0.5810315   0.2923374       0        0  0.3309884 0.000000
##   D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 1         0        0         0        0        0       0
## 2         0        0         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##     UniqueID sold.fctr prdline.my
## 21     10021         Y     iPad 2
## 329    10329         Y     iPad 2
##                                                                                              descr.my
## 21                                                                                   Crack on screen.
## 329 Item has original packaging. Original packaging has stickers on box. Minor screen flaw (Does not 
##     D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 21           0       0           0       0        0  1.8204364        0
## 329          0       0           0       0        0  0.3640873        0
##     D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 21          0        0         0        0        0       0
## 329         0        0         0        0        0       0
```

![](ebayipads_dupobs_files/figure-html/cluster.data-3.png) 

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
## 3   0.0000000       0   0.0000000       0 0.0000000  0.0000000        0
## 11  0.2203907       0   0.2923374       0 0.3412301  0.3309884        0
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

![](ebayipads_dupobs_files/figure-html/cluster.data-4.png) 

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
##     UniqueID sold.fctr prdline.my
## 96     10096         N    iPadAir
## 461    10461         N    iPadAir
##                                                                                   descr.my
## 96  Open Box Excellent Condition .  All original accessories included in its original box.
## 461                                                                   Excellent condition.
##     D.T.condit D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 96   0.2693664       0           0       0        0          0        0
## 461  1.2121489       0           0       0        0          0        0
##     D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 96          0        0 0.5273584        0        0       0
## 461         0        0 2.3731129        0        0       0
```

![](ebayipads_dupobs_files/figure-html/cluster.data-5.png) 

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
## 76  0.3030372       0           0       0 0.4691913          0        0
##    D.T.great  D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 7          0 0.0000000         0        0        0       0
## 76         0 0.9365565         0        0        0       0
## [1] "min distance(-0.0000) pair:"
##      UniqueID sold.fctr prdline.my
## 384     10384         Y   iPadmini
## 2259    12261      <NA>   iPadmini
##                                                                            descr.my
## 384                                               Great condition and lightly used!
## 2259 I&#039;m great condition! Rarely ever been used and has always been in a case.
##      D.T.condit   D.T.use D.T.scratch D.T.new D.T.good D.T.screen D.T.ipad
## 384   0.6060744 0.7989184           0       0        0          0        0
## 2259  0.3463283 0.4565248           0       0        0          0        0
##      D.T.great D.T.work D.T.excel D.P.http D.P.mini D.P.air
## 384  1.1024496        0         0        0        0       0
## 2259 0.6299712        0         0        0        0       0
```

![](ebayipads_dupobs_files/figure-html/cluster.data-6.png) 

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

![](ebayipads_dupobs_files/figure-html/cluster.data-7.png) 

```
## [1] "glb_allobs_df$prdline.my$.clusterid Entropy: 0.6728 (98.2111 pct)"
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
## 13            iPad 3+_1  80 100 0.6869616  180
## 14            iPad 3+_2  46  14 0.5432728   60
## 15            iPad 3+_3  23  16 0.6769517   39
## 16            iPad 3+_4  17  15 0.6911928   32
## 17            iPadAir_1 133 101 0.6837672  234
## 18            iPadAir_2  52  41 0.6861358   93
## 19            iPadAir_3  18   8 0.6172418   26
## 20        iPadmini 2+_1 100  61 0.6635142  161
## 21        iPadmini 2+_2  11   9 0.6881388   20
## 22        iPadmini 2+_3  14  10 0.6791933   24
## 23           iPadmini_1  99  87 0.6910646  186
## 24           iPadmini_2  17  14 0.6884572   31
## 25           iPadmini_3  11  18 0.6637255   29
## 26           iPadmini_4  12  10 0.6890092   22
## 27           iPadmini_5   7   4 0.6554818   11
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
## 7 manage.missing.data          4          1 77.941 81.458   3.517
## 8     select.features          5          0 81.459     NA      NA
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
## D.terms.n.post.stop                 D.terms.n.post.stop -0.0832265597
## D.terms.n.post.stem                 D.terms.n.post.stem -0.0830663021
## D.npnct14.log                             D.npnct14.log -0.0786203827
## cellular.fctr                             cellular.fctr -0.0730394149
## carrier.fctr                               carrier.fctr -0.0685230948
## D.nwrds.unq.log                         D.nwrds.unq.log -0.0651243063
## D.ndgts.log                                 D.ndgts.log -0.0637072932
## D.npnct09.log                             D.npnct09.log -0.0618253281
## D.nwrds.log                                 D.nwrds.log -0.0590599485
## D.npnct12.log                             D.npnct12.log -0.0568348971
## D.nchrs.log                                 D.nchrs.log -0.0565292623
## D.ratio.nstopwrds.nwrds         D.ratio.nstopwrds.nwrds  0.0556485120
## D.nuppr.log                                 D.nuppr.log -0.0553820542
## D.npnct28.log                             D.npnct28.log -0.0524583244
## D.npnct06.log                             D.npnct06.log -0.0499761958
## D.npnct15.log                             D.npnct15.log  0.0484022793
## D.nstopwrds.log                         D.nstopwrds.log -0.0464404483
## D.npnct24.log                             D.npnct24.log -0.0458449965
## D.npnct16.log                             D.npnct16.log -0.0449403962
## color.fctr                                   color.fctr -0.0433350320
## prdline.my.fctr                         prdline.my.fctr -0.0415814340
## D.npnct08.log                             D.npnct08.log -0.0396513123
## D.T.new                                         D.T.new -0.0383561562
## D.npnct13.log                             D.npnct13.log -0.0373463069
## D.T.condit                                   D.T.condit -0.0371486339
## D.TfIdf.sum.post.stem             D.TfIdf.sum.post.stem -0.0351644554
## D.sum.TfIdf                                 D.sum.TfIdf -0.0351644554
## .clusterid                                   .clusterid -0.0349960850
## .clusterid.fctr                         .clusterid.fctr -0.0349960850
## D.TfIdf.sum.post.stop             D.TfIdf.sum.post.stop -0.0325541826
## D.T.excel                                     D.T.excel  0.0265752158
## D.npnct03.log                             D.npnct03.log  0.0257637868
## D.npnct07.log                             D.npnct07.log  0.0250040676
## D.T.screen                                   D.T.screen  0.0242145889
## D.npnct10.log                             D.npnct10.log -0.0241015016
## D.npnct18.log                             D.npnct18.log -0.0215250231
## D.npnct11.log                             D.npnct11.log -0.0192035548
## D.terms.n.stem.stop.Ratio     D.terms.n.stem.stop.Ratio  0.0165731742
## D.T.ipad                                       D.T.ipad -0.0164082423
## D.T.work                                       D.T.work -0.0124718624
## D.T.use                                         D.T.use  0.0113062650
## D.P.mini                                       D.P.mini -0.0112418293
## storage.fctr                               storage.fctr -0.0110508589
## D.P.air                                         D.P.air -0.0092629952
## D.ratio.sum.TfIdf.nwrds         D.ratio.sum.TfIdf.nwrds  0.0082354655
## D.TfIdf.sum.stem.stop.Ratio D.TfIdf.sum.stem.stop.Ratio -0.0078908915
## D.T.great                                     D.T.great  0.0078649564
## D.T.scratch                                 D.T.scratch -0.0072207615
## .rnorm                                           .rnorm  0.0067562740
## D.npnct01.log                             D.npnct01.log  0.0041255300
## D.T.good                                       D.T.good -0.0005088937
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
## D.terms.n.post.stop                       0 0.0832265597
## D.terms.n.post.stem                       0 0.0830663021
## D.npnct14.log                             0 0.0786203827
## cellular.fctr                             0 0.0730394149
## carrier.fctr                              0 0.0685230948
## D.nwrds.unq.log                           0 0.0651243063
## D.ndgts.log                               0 0.0637072932
## D.npnct09.log                             0 0.0618253281
## D.nwrds.log                               0 0.0590599485
## D.npnct12.log                             0 0.0568348971
## D.nchrs.log                               0 0.0565292623
## D.ratio.nstopwrds.nwrds                   0 0.0556485120
## D.nuppr.log                               0 0.0553820542
## D.npnct28.log                             0 0.0524583244
## D.npnct06.log                             0 0.0499761958
## D.npnct15.log                             0 0.0484022793
## D.nstopwrds.log                           0 0.0464404483
## D.npnct24.log                             0 0.0458449965
## D.npnct16.log                             0 0.0449403962
## color.fctr                                0 0.0433350320
## prdline.my.fctr                           0 0.0415814340
## D.npnct08.log                             0 0.0396513123
## D.T.new                                   1 0.0383561562
## D.npnct13.log                             0 0.0373463069
## D.T.condit                                1 0.0371486339
## D.TfIdf.sum.post.stem                     0 0.0351644554
## D.sum.TfIdf                               0 0.0351644554
## .clusterid                                1 0.0349960850
## .clusterid.fctr                           0 0.0349960850
## D.TfIdf.sum.post.stop                     0 0.0325541826
## D.T.excel                                 1 0.0265752158
## D.npnct03.log                             0 0.0257637868
## D.npnct07.log                             0 0.0250040676
## D.T.screen                                1 0.0242145889
## D.npnct10.log                             0 0.0241015016
## D.npnct18.log                             0 0.0215250231
## D.npnct11.log                             0 0.0192035548
## D.terms.n.stem.stop.Ratio                 0 0.0165731742
## D.T.ipad                                  1 0.0164082423
## D.T.work                                  1 0.0124718624
## D.T.use                                   1 0.0113062650
## D.P.mini                                  1 0.0112418293
## storage.fctr                              0 0.0110508589
## D.P.air                                   1 0.0092629952
## D.ratio.sum.TfIdf.nwrds                   0 0.0082354655
## D.TfIdf.sum.stem.stop.Ratio               0 0.0078908915
## D.T.great                                 1 0.0078649564
## D.T.scratch                               1 0.0072207615
## .rnorm                                    0 0.0067562740
## D.npnct01.log                             0 0.0041255300
## D.T.good                                  1 0.0005088937
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
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0352"
## [1] "cor(sold.fctr, D.sum.TfIdf)=-0.0352"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.sum.TfIdf as highly correlated with
## D.TfIdf.sum.post.stem
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
## [1] "cor(sold.fctr, D.terms.n.post.stem)=-0.0831"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.terms.n.post.stem as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.TfIdf.sum.post.stem, D.TfIdf.sum.post.stop)=0.9977"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0352"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stop)=-0.0326"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stop as highly correlated with
## D.TfIdf.sum.post.stem
```

```
## [1] "cor(D.nchrs.log, D.nwrds.unq.log)=0.9934"
## [1] "cor(sold.fctr, D.nchrs.log)=-0.0565"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0651"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nchrs.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.log, D.nwrds.unq.log)=0.9927"
## [1] "cor(sold.fctr, D.nwrds.log)=-0.0591"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0651"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.log as highly correlated with
## D.nwrds.unq.log
```

```
## [1] "cor(D.nwrds.unq.log, D.terms.n.post.stop)=0.9755"
## [1] "cor(sold.fctr, D.nwrds.unq.log)=-0.0651"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nwrds.unq.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.npnct24.log, D.ratio.nstopwrds.nwrds)=-0.9649"
## [1] "cor(sold.fctr, D.npnct24.log)=-0.0458"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0556"
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
## [1] "cor(D.TfIdf.sum.post.stem, D.ratio.nstopwrds.nwrds)=-0.9261"
## [1] "cor(sold.fctr, D.TfIdf.sum.post.stem)=-0.0352"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0556"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.TfIdf.sum.post.stem as highly correlated with
## D.ratio.nstopwrds.nwrds
```

```
## [1] "cor(D.nstopwrds.log, D.terms.n.post.stop)=0.8952"
## [1] "cor(sold.fctr, D.nstopwrds.log)=-0.0464"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.nstopwrds.log as highly correlated with
## D.terms.n.post.stop
```

```
## [1] "cor(D.ratio.nstopwrds.nwrds, D.terms.n.post.stop)=-0.8682"
## [1] "cor(sold.fctr, D.ratio.nstopwrds.nwrds)=0.0556"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.ratio.nstopwrds.nwrds as highly correlated
## with D.terms.n.post.stop
```

```
## [1] "cor(carrier.fctr, cellular.fctr)=0.8461"
## [1] "cor(sold.fctr, carrier.fctr)=-0.0685"
## [1] "cor(sold.fctr, cellular.fctr)=-0.0730"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified carrier.fctr as highly correlated with
## cellular.fctr
```

```
## [1] "cor(D.npnct13.log, D.terms.n.post.stop)=0.7388"
## [1] "cor(sold.fctr, D.npnct13.log)=-0.0373"
## [1] "cor(sold.fctr, D.terms.n.post.stop)=-0.0832"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified D.npnct13.log as highly correlated with
## D.terms.n.post.stop
```

```
##                             id         cor.y exclude.as.feat    cor.y.abs
## 70                        sold  1.0000000000               1 1.0000000000
## 63                    biddable  0.5481788380               0 0.5481788380
## 56     D.ratio.nstopwrds.nwrds  0.0556485120               0 0.0556485120
## 36               D.npnct15.log  0.0484022793               0 0.0484022793
## 8                    D.T.excel  0.0265752158               1 0.0265752158
## 24               D.npnct03.log  0.0257637868               0 0.0257637868
## 28               D.npnct07.log  0.0250040676               0 0.0250040676
## 14                  D.T.screen  0.0242145889               1 0.0242145889
## 61   D.terms.n.stem.stop.Ratio  0.0165731742               0 0.0165731742
## 15                     D.T.use  0.0113062650               1 0.0113062650
## 57     D.ratio.sum.TfIdf.nwrds  0.0082354655               0 0.0082354655
## 10                   D.T.great  0.0078649564               1 0.0078649564
## 3                       .rnorm  0.0067562740               0 0.0067562740
## 22               D.npnct01.log  0.0041255300               0 0.0041255300
## 9                     D.T.good -0.0005088937               1 0.0005088937
## 13                 D.T.scratch -0.0072207615               1 0.0072207615
## 19 D.TfIdf.sum.stem.stop.Ratio -0.0078908915               0 0.0078908915
## 4                      D.P.air -0.0092629952               1 0.0092629952
## 73                storage.fctr -0.0110508589               0 0.0110508589
## 6                     D.P.mini -0.0112418293               1 0.0112418293
## 16                    D.T.work -0.0124718624               1 0.0124718624
## 11                    D.T.ipad -0.0164082423               1 0.0164082423
## 32               D.npnct11.log -0.0192035548               0 0.0192035548
## 39               D.npnct18.log -0.0215250231               0 0.0215250231
## 31               D.npnct10.log -0.0241015016               0 0.0241015016
## 18       D.TfIdf.sum.post.stop -0.0325541826               0 0.0325541826
## 1                   .clusterid -0.0349960850               1 0.0349960850
## 2              .clusterid.fctr -0.0349960850               0 0.0349960850
## 17       D.TfIdf.sum.post.stem -0.0351644554               0 0.0351644554
## 58                 D.sum.TfIdf -0.0351644554               0 0.0351644554
## 7                   D.T.condit -0.0371486339               1 0.0371486339
## 34               D.npnct13.log -0.0373463069               0 0.0373463069
## 12                     D.T.new -0.0383561562               1 0.0383561562
## 29               D.npnct08.log -0.0396513123               0 0.0396513123
## 69             prdline.my.fctr -0.0415814340               0 0.0415814340
## 66                  color.fctr -0.0433350320               0 0.0433350320
## 37               D.npnct16.log -0.0449403962               0 0.0449403962
## 45               D.npnct24.log -0.0458449965               0 0.0458449965
## 52             D.nstopwrds.log -0.0464404483               0 0.0464404483
## 27               D.npnct06.log -0.0499761958               0 0.0499761958
## 49               D.npnct28.log -0.0524583244               0 0.0524583244
## 53                 D.nuppr.log -0.0553820542               0 0.0553820542
## 20                 D.nchrs.log -0.0565292623               0 0.0565292623
## 33               D.npnct12.log -0.0568348971               0 0.0568348971
## 54                 D.nwrds.log -0.0590599485               0 0.0590599485
## 30               D.npnct09.log -0.0618253281               0 0.0618253281
## 21                 D.ndgts.log -0.0637072932               0 0.0637072932
## 55             D.nwrds.unq.log -0.0651243063               0 0.0651243063
## 64                carrier.fctr -0.0685230948               0 0.0685230948
## 65               cellular.fctr -0.0730394149               0 0.0730394149
## 35               D.npnct14.log -0.0786203827               0 0.0786203827
## 59         D.terms.n.post.stem -0.0830663021               0 0.0830663021
## 60         D.terms.n.post.stop -0.0832265597               0 0.0832265597
## 26               D.npnct05.log -0.1180558939               0 0.1180558939
## 67              condition.fctr -0.1535490071               0 0.1535490071
## 62                    UniqueID -0.1895466260               1 0.1895466260
## 68                    idseq.my -0.1895466260               0 0.1895466260
## 71                  startprice -0.4569767211               1 0.4569767211
## 72              startprice.log -0.4674275376               0 0.4674275376
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
## 70                    <NA>    1.161628    0.10758472   FALSE FALSE
## 63                    <NA>    1.221027    0.10758472   FALSE FALSE
## 56     D.terms.n.post.stop   13.375000    4.19580420   FALSE FALSE
## 36                    <NA>  153.416667    0.16137708   FALSE  TRUE
## 8                     <NA>  138.153846    0.75309306   FALSE  TRUE
## 24                    <NA>   83.227273    0.16137708   FALSE  TRUE
## 28                    <NA> 1858.000000    0.10758472   FALSE  TRUE
## 14                    <NA>   55.064516    0.80688542   FALSE  TRUE
## 61                    <NA>   77.826087    0.48413125   FALSE  TRUE
## 15                    <NA>   47.228571    0.91447015   FALSE  TRUE
## 57                    <NA>   63.000000   34.85745024   FALSE FALSE
## 10                    <NA>  104.470588    0.80688542   FALSE  TRUE
## 3                     <NA>    1.000000  100.00000000   FALSE FALSE
## 22                    <NA>   52.970588    0.32275417   FALSE  TRUE
## 9                     <NA>   47.833333    0.86067778   FALSE  TRUE
## 13                    <NA>   40.390244    0.86067778   FALSE  TRUE
## 19                    <NA>   65.176471   32.92092523   FALSE FALSE
## 4                     <NA>  122.866667    0.16137708   FALSE  TRUE
## 73                    <NA>    2.733138    0.26896181   FALSE FALSE
## 6                     <NA>   91.900000    0.16137708   FALSE  TRUE
## 16                    <NA>   61.357143    0.69930070   FALSE  TRUE
## 11                    <NA>   49.823529    0.80688542   FALSE  TRUE
## 32                    <NA>    9.374269    0.37654653   FALSE FALSE
## 39                    <NA> 1858.000000    0.10758472   FALSE  TRUE
## 31                    <NA>  308.666667    0.16137708   FALSE  TRUE
## 18   D.TfIdf.sum.post.stem   63.000000   34.37331899   FALSE FALSE
## 1                     <NA>    3.448571    0.26896181   FALSE FALSE
## 2                     <NA>    3.448571    0.26896181   FALSE FALSE
## 17 D.ratio.nstopwrds.nwrds   63.000000   34.31952663   FALSE FALSE
## 58   D.TfIdf.sum.post.stem   63.000000   34.31952663   FALSE FALSE
## 7                     <NA>   24.868852    0.91447015   FALSE  TRUE
## 34     D.terms.n.post.stop    5.203065    0.32275417   FALSE FALSE
## 12                    <NA>  103.000000    0.86067778   FALSE  TRUE
## 29                    <NA>   69.576923    0.21516945   FALSE  TRUE
## 69                    <NA>    1.135048    0.37654653   FALSE FALSE
## 66                    <NA>    1.574610    0.26896181   FALSE FALSE
## 37           D.npnct06.log   31.245614    0.16137708   FALSE  TRUE
## 45 D.ratio.nstopwrds.nwrds    1.356147    0.10758472   FALSE FALSE
## 52     D.terms.n.post.stop   13.436782    0.80688542   FALSE FALSE
## 27                    <NA>   33.735849    0.16137708   FALSE  TRUE
## 49                    <NA>  463.250000    0.16137708   FALSE  TRUE
## 53             D.nchrs.log   18.807018    4.41097364   FALSE FALSE
## 20         D.nwrds.unq.log   15.970149    5.70199032   FALSE FALSE
## 33                    <NA>   26.818182    0.21516945   FALSE  TRUE
## 54         D.nwrds.unq.log   12.891566    1.29101668   FALSE FALSE
## 30                    <NA>  308.333333    0.21516945   FALSE  TRUE
## 21                    <NA>   27.031746    0.69930070   FALSE  TRUE
## 55     D.terms.n.post.stop    7.650000    0.80688542   FALSE FALSE
## 64           cellular.fctr    3.186782    0.37654653   FALSE FALSE
## 65                    <NA>    2.116412    0.16137708   FALSE FALSE
## 35                    <NA>   35.333333    0.26896181   FALSE  TRUE
## 59     D.terms.n.post.stop    7.650000    0.80688542   FALSE FALSE
## 60                    <NA>    8.175573    0.80688542   FALSE FALSE
## 26                    <NA>   40.311111    0.10758472   FALSE  TRUE
## 67                    <NA>    4.003460    0.32275417   FALSE FALSE
## 62                    <NA>    1.000000  100.00000000   FALSE FALSE
## 68                    <NA>    1.000000  100.00000000   FALSE FALSE
## 71                    <NA>    2.807692   30.17751479   FALSE FALSE
## 72                    <NA>    2.807692   30.17751479   FALSE FALSE
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
## 70    FALSE            FALSE
## 63    FALSE            FALSE
## 56    FALSE            FALSE
## 36    FALSE            FALSE
## 8     FALSE            FALSE
## 24    FALSE            FALSE
## 28     TRUE            FALSE
## 14    FALSE            FALSE
## 61    FALSE            FALSE
## 15    FALSE            FALSE
## 57    FALSE            FALSE
## 10    FALSE            FALSE
## 3     FALSE            FALSE
## 22    FALSE             TRUE
## 9     FALSE             TRUE
## 13    FALSE            FALSE
## 19    FALSE            FALSE
## 4     FALSE            FALSE
## 73    FALSE            FALSE
## 6     FALSE            FALSE
## 16    FALSE            FALSE
## 11    FALSE            FALSE
## 32    FALSE            FALSE
## 39     TRUE            FALSE
## 31    FALSE            FALSE
## 18    FALSE            FALSE
## 1     FALSE            FALSE
## 2     FALSE            FALSE
## 17    FALSE            FALSE
## 58    FALSE            FALSE
## 7     FALSE            FALSE
## 34    FALSE            FALSE
## 12    FALSE            FALSE
## 29    FALSE            FALSE
## 69    FALSE            FALSE
## 66    FALSE            FALSE
## 37    FALSE            FALSE
## 45    FALSE            FALSE
## 52    FALSE            FALSE
## 27    FALSE            FALSE
## 49    FALSE            FALSE
## 53    FALSE            FALSE
## 20    FALSE            FALSE
## 33    FALSE            FALSE
## 54    FALSE            FALSE
## 30    FALSE            FALSE
## 21    FALSE            FALSE
## 55    FALSE            FALSE
## 64    FALSE            FALSE
## 65    FALSE            FALSE
## 35    FALSE            FALSE
## 59    FALSE            FALSE
## 60    FALSE            FALSE
## 26    FALSE            FALSE
## 67    FALSE            FALSE
## 62    FALSE            FALSE
## 68    FALSE            FALSE
## 71    FALSE            FALSE
## 72    FALSE            FALSE
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

![](ebayipads_dupobs_files/figure-html/select.features-1.png) 

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
##           cellular.fctr     D.terms.n.post.stop   D.TfIdf.sum.post.stop 
##                    1597                    1521                    1521 
##     D.terms.n.post.stem   D.TfIdf.sum.post.stem              D.T.condit 
##                    1521                    1521                    2162 
##                 D.T.use             D.T.scratch                 D.T.new 
##                    2367                    2371                    2501 
##                D.T.good              D.T.screen                D.T.ipad 
##                    2460                    2444                    2425 
##               D.T.great                D.T.work               D.T.excel 
##                    2532                    2459                    2558 
##             D.nwrds.log         D.nwrds.unq.log             D.sum.TfIdf 
##                    1520                    1521                    1521 
## D.ratio.sum.TfIdf.nwrds             D.nchrs.log             D.nuppr.log 
##                    1521                    1520                    1522 
##             D.ndgts.log           D.npnct01.log           D.npnct03.log 
##                    2426                    2579                    2614 
##           D.npnct05.log           D.npnct06.log           D.npnct08.log 
##                    2592                    2554                    2581 
##           D.npnct09.log           D.npnct10.log           D.npnct11.log 
##                    2641                    2648                    2301 
##           D.npnct12.log           D.npnct13.log           D.npnct14.log 
##                    2537                    1932                    2582 
##           D.npnct15.log           D.npnct16.log           D.npnct24.log 
##                    2637                    2546                    1520 
##           D.npnct28.log         D.nstopwrds.log                D.P.mini 
##                    2649                    1663                    2623 
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
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 81.459 85.179    3.72
## 9 partition.data.training          6          0 85.179     NA      NA
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
## [1] 73 12
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
## 70             sold  1.0000000            TRUE 1.0000000       <NA>
## 62         UniqueID -0.1895466            TRUE 0.1895466       <NA>
## sold.fctr sold.fctr         NA            TRUE        NA       <NA>
##           freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 70         1.161628     0.1075847   FALSE FALSE    FALSE            FALSE
## 62         1.000000   100.0000000   FALSE FALSE    FALSE            FALSE
## sold.fctr        NA            NA      NA    NA       NA               NA
##           interaction.feat rsp_var_raw id_var rsp_var
## 70                    <NA>        TRUE     NA      NA
## 62                    <NA>       FALSE   TRUE      NA
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
## [1] 2657   70
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 1859   69
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 974  69
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 885  69
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 798  69
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
## 9  partition.data.training          6          0 85.179 86.041   0.863
## 10              fit.models          7          0 86.042     NA      NA
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
## 1                      0.351                 0.002         0.5
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

![](ebayipads_dupobs_files/figure-html/fit.models_0-1.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-2.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-3.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-4.png) 

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
## 1                      0.265                 0.002   0.5071756
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

![](ebayipads_dupobs_files/figure-html/fit.models_0-5.png) 

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
## 1               0                      0.612                 0.012
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

![](ebayipads_dupobs_files/figure-html/fit.models_0-6.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-7.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-8.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-9.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-10.png) 

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
## 1               0                      0.477                 0.008
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

![](ebayipads_dupobs_files/figure-html/fit.models_0-11.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-12.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-13.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-14.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-15.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-16.png) 

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
## 1                      1.004                 0.012   0.8172434
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

![](ebayipads_dupobs_files/figure-html/fit.models_0-17.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-18.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-19.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-20.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-21.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-22.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-23.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_0-24.png) 

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
## 1                      1.043                 0.015    0.840229
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

![](ebayipads_dupobs_files/figure-html/fit.models_0-25.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-26.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-27.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-28.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.9798  -0.7021  -0.5105   0.7393   2.1205  
## 
## Coefficients:
##                                    Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         2.47942    0.48789   5.082 3.74e-07
## biddable                            2.51697    1.38834   1.813  0.06984
## startprice.log                     -0.70856    0.08961  -7.907 2.63e-15
## `biddable:D.terms.n.post.stop`      0.12864    0.27815   0.462  0.64372
## `biddable:D.TfIdf.sum.post.stem`    0.08918    0.19998   0.446  0.65564
## `biddable:D.ratio.nstopwrds.nwrds` -0.46307    1.37902  -0.336  0.73703
## `biddable:D.npnct06.log`           -0.42306    0.81415  -0.520  0.60332
## `biddable:D.nchrs.log`             -0.08457    0.83777  -0.101  0.91959
## `biddable:D.nwrds.unq.log`         -0.81356    2.75778  -0.295  0.76799
## `biddable:cellular.fctr1`          -0.11270    0.29020  -0.388  0.69776
## `biddable:cellular.fctrUnknown`    -1.14917    0.36338  -3.162  0.00156
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
## Residual deviance:  924.87  on 963  degrees of freedom
## AIC: 946.87
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6346968
## 3        0.2 0.7131367
## 4        0.3 0.7476636
## 5        0.4 0.7563396
## 6        0.5 0.7534247
## 7        0.6 0.7457213
## 8        0.7 0.7054054
## 9        0.8 0.5574803
## 10       0.9 0.4112478
## 11       1.0 0.0000000
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-30.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Interact.High.cor.Y.glm.N
## 1         N                                         410
## 2         Y                                         107
##   sold.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                         114
## 2                                         343
##          Prediction
## Reference   N   Y
##         N 410 114
##         Y 107 343
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.731006e-01   5.440674e-01   7.454801e-01   7.990590e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.208903e-52   6.865042e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6371406
## 3        0.2 0.7063340
## 4        0.3 0.7508691
## 5        0.4 0.7636816
## 6        0.5 0.7496823
## 7        0.6 0.7453083
## 8        0.7 0.6913947
## 9        0.8 0.5291005
## 10       0.9 0.3906250
## 11       1.0 0.0000000
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-32.png) 

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
## 1               1                      1.015                 0.014
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8427523                    0.4       0.7563396        0.7690028
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7454801              0.799059      0.533209   0.8394608
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7636816        0.7853107
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7567658             0.8119416     0.5671369    946.8663
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01318051      0.02966064
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
## [1] "    indep_vars: biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-33.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-34.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-35.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4139  -0.6806  -0.2093   0.6294   2.4504  
## 
## Coefficients: (8 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                    1.115e+01  5.554e+00
## biddable                                       1.756e+00  2.001e-01
## D.npnct15.log                                  2.102e+00  8.597e-01
## D.npnct03.log                                  3.702e-01  1.598e+00
## D.terms.n.stem.stop.Ratio                     -7.307e+00  4.931e+00
## D.ratio.sum.TfIdf.nwrds                       -2.625e-02  1.561e-01
## .rnorm                                         9.069e-03  9.219e-02
## D.npnct01.log                                  1.488e-01  5.351e-01
## D.TfIdf.sum.stem.stop.Ratio                   -6.425e-01  2.954e+00
## storage.fctr16                                 1.292e-01  4.468e-01
## storage.fctr32                                 1.068e-02  4.798e-01
## storage.fctr64                                 6.931e-01  4.660e-01
## storage.fctrUnknown                           -5.908e-02  6.393e-01
## D.npnct11.log                                 -1.448e-01  3.497e-01
## D.npnct10.log                                 -2.305e+01  1.342e+03
## D.npnct08.log                                  6.394e-01  6.570e-01
## `prdline.my.fctriPad 1`                        9.438e-01  5.717e-01
## `prdline.my.fctriPad 2`                        9.306e-01  5.857e-01
## `prdline.my.fctriPad 3+`                       1.418e+00  5.653e-01
## prdline.my.fctriPadAir                         2.141e+00  5.824e-01
## prdline.my.fctriPadmini                        1.218e+00  5.489e-01
## `prdline.my.fctriPadmini 2+`                   1.647e+00  5.975e-01
## color.fctrBlack                                1.525e-01  2.530e-01
## color.fctrGold                                -4.025e-01  4.988e-01
## `color.fctrSpace Gray`                        -1.976e-01  3.271e-01
## color.fctrWhite                               -6.964e-02  2.494e-01
## D.npnct06.log                                 -1.980e+00  1.131e+00
## D.npnct28.log                                 -1.615e+00  1.733e+03
## D.npnct12.log                                  1.061e-01  7.387e-01
## D.npnct09.log                                 -9.715e+00  8.052e+02
## D.ndgts.log                                    6.738e-01  4.421e-01
## cellular.fctr1                                 7.888e-02  2.153e-01
## cellular.fctrUnknown                          -2.491e-01  4.683e-01
## D.npnct14.log                                 -1.634e+00  1.050e+00
## D.terms.n.post.stop                           -8.800e-02  4.845e-02
## D.npnct05.log                                 -3.935e+00  1.811e+00
## `condition.fctrFor parts or not working`      -2.958e-01  3.909e-01
## `condition.fctrManufacturer refurbished`       4.264e-01  5.999e-01
## condition.fctrNew                             -3.395e-01  3.014e-01
## `condition.fctrNew other (see details)`        2.402e-01  4.461e-01
## `condition.fctrSeller refurbished`            -9.435e-01  4.800e-01
## idseq.my                                      -4.371e-04  2.038e-04
## startprice.log                                -1.013e+00  1.451e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.730e+00  7.527e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.308e+00  8.149e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       8.029e-01  7.378e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -8.805e-01  7.819e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -3.761e-01  6.075e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.461e+00  7.590e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  3.781e-01  9.298e-01
## `prdline.my.fctrUnknown:.clusterid.fctr3`      1.458e-01  1.053e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`       5.968e-01  9.082e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.503e+01  7.825e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -2.963e-01  6.662e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -4.547e-01  8.270e-01
## `prdline.my.fctriPadmini:.clusterid.fctr3`     1.059e+00  8.465e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  3.953e-01  8.763e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.631e+00  9.431e-01
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.112e+00  1.308e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      1.009e+00  7.694e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -4.385e-01  9.842e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.685e+00  1.416e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -5.645e-01  1.358e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                     2.008 0.044695 *  
## biddable                                        8.777  < 2e-16 ***
## D.npnct15.log                                   2.445 0.014505 *  
## D.npnct03.log                                   0.232 0.816809    
## D.terms.n.stem.stop.Ratio                      -1.482 0.138380    
## D.ratio.sum.TfIdf.nwrds                        -0.168 0.866453    
## .rnorm                                          0.098 0.921638    
## D.npnct01.log                                   0.278 0.780947    
## D.TfIdf.sum.stem.stop.Ratio                    -0.218 0.827793    
## storage.fctr16                                  0.289 0.772468    
## storage.fctr32                                  0.022 0.982241    
## storage.fctr64                                  1.487 0.136923    
## storage.fctrUnknown                            -0.092 0.926362    
## D.npnct11.log                                  -0.414 0.678791    
## D.npnct10.log                                  -0.017 0.986298    
## D.npnct08.log                                   0.973 0.330434    
## `prdline.my.fctriPad 1`                         1.651 0.098767 .  
## `prdline.my.fctriPad 2`                         1.589 0.112103    
## `prdline.my.fctriPad 3+`                        2.509 0.012109 *  
## prdline.my.fctriPadAir                          3.676 0.000237 ***
## prdline.my.fctriPadmini                         2.220 0.026447 *  
## `prdline.my.fctriPadmini 2+`                    2.756 0.005846 ** 
## color.fctrBlack                                 0.603 0.546642    
## color.fctrGold                                 -0.807 0.419644    
## `color.fctrSpace Gray`                         -0.604 0.545864    
## color.fctrWhite                                -0.279 0.780058    
## D.npnct06.log                                  -1.750 0.080057 .  
## D.npnct28.log                                  -0.001 0.999257    
## D.npnct12.log                                   0.144 0.885800    
## D.npnct09.log                                  -0.012 0.990373    
## D.ndgts.log                                     1.524 0.127523    
## cellular.fctr1                                  0.366 0.714052    
## cellular.fctrUnknown                           -0.532 0.594791    
## D.npnct14.log                                  -1.556 0.119735    
## D.terms.n.post.stop                            -1.816 0.069326 .  
## D.npnct05.log                                  -2.173 0.029771 *  
## `condition.fctrFor parts or not working`       -0.757 0.449213    
## `condition.fctrManufacturer refurbished`        0.711 0.477236    
## condition.fctrNew                              -1.126 0.260016    
## `condition.fctrNew other (see details)`         0.538 0.590311    
## `condition.fctrSeller refurbished`             -1.966 0.049326 *  
## idseq.my                                       -2.145 0.031969 *  
## startprice.log                                 -6.981 2.93e-12 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       3.628 0.000286 ***
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.605 0.108386    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        1.088 0.276463    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.126 0.260122    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.619 0.535793    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.925 0.054237 .  
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.407 0.684291    
## `prdline.my.fctrUnknown:.clusterid.fctr3`       0.139 0.889839    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.657 0.511076    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.019 0.984670    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.445 0.656508    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.550 0.582445    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.251 0.210790    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.451 0.651875    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.729 0.083729 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.851 0.394943    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.311 0.189774    
## `prdline.my.fctriPadAir:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.446 0.655950    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.602 0.009269 ** 
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.416 0.677548    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  809.65  on 911  degrees of freedom
## AIC: 935.65
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-36.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6928515
## 3        0.2 0.7406082
## 4        0.3 0.7723577
## 5        0.4 0.7868132
## 6        0.5 0.7944573
## 7        0.6 0.7892814
## 8        0.7 0.7584416
## 9        0.8 0.6550725
## 10       0.9 0.4916944
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               452
## 2         Y                               106
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                72
## 2                               344
##          Prediction
## Reference   N   Y
##         N 452  72
##         Y 106 344
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.172485e-01   6.304031e-01   7.915181e-01   8.410439e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   6.422756e-75   1.338138e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-38.png) ![](ebayipads_dupobs_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6713287
## 3        0.2 0.7119675
## 4        0.3 0.7451429
## 5        0.4 0.7518428
## 6        0.5 0.7580026
## 7        0.6 0.7503374
## 8        0.7 0.7296137
## 9        0.8 0.6402536
## 10       0.9 0.4808743
## 11       1.0 0.0000000
```

![](ebayipads_dupobs_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.Low.cor.X.glm.N
## 1         N                               400
## 2         Y                               114
##   sold.fctr.predict.Low.cor.X.glm.Y
## 1                                75
## 2                               296
##          Prediction
## Reference   N   Y
##         N 400  75
##         Y 114 296
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.864407e-01   5.677516e-01   7.579436e-01   8.130160e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   5.826387e-54   5.708122e-03 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.427                 0.159
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8851187                    0.5       0.7944573        0.7618107
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7915181             0.8410439     0.5194818   0.8296945
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7580026        0.7864407
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7579436              0.813016     0.5677516    935.6464
##   max.AccuracySD.fit max.KappaSD.fit
## 1           0.009749      0.02418586
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 10 fit.models          7          0  86.042 111.324  25.282
## 11 fit.models          7          1 111.325      NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor     bgn end elapsed
## 1 fit.models_1_bgn          1          0 115.318  NA      NA
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
```

```
##              label step_major step_minor     bgn     end elapsed
## 1 fit.models_1_bgn          1          0 115.318 115.336   0.019
## 2 fit.models_1_glm          2          0 115.337      NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-1.png) ![](ebayipads_dupobs_files/figure-html/fit.models_1-2.png) ![](ebayipads_dupobs_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.5506  -0.6515  -0.1608   0.5949   2.4567  
## 
## Coefficients: (10 not defined because of singularities)
##                                                 Estimate Std. Error
## (Intercept)                                    6.178e+01  4.292e+01
## biddable                                       1.803e+00  2.068e-01
## D.ratio.nstopwrds.nwrds                       -1.748e+01  7.390e+00
## D.npnct15.log                                  1.527e+00  9.535e-01
## D.npnct03.log                                  1.018e+00  1.983e+00
## D.terms.n.stem.stop.Ratio                     -3.393e+01  3.919e+01
## D.ratio.sum.TfIdf.nwrds                       -8.105e-02  6.772e-01
## .rnorm                                         4.380e-03  9.445e-02
## D.npnct01.log                                  9.274e-02  6.268e-01
## D.TfIdf.sum.stem.stop.Ratio                   -7.012e+00  1.819e+01
## storage.fctr16                                 5.772e-02  4.535e-01
## storage.fctr32                                -9.689e-02  4.907e-01
## storage.fctr64                                 6.485e-01  4.754e-01
## storage.fctrUnknown                           -1.633e-01  6.486e-01
## D.npnct11.log                                 -1.090e-01  3.939e-01
## D.npnct10.log                                 -2.504e+01  1.217e+03
## D.TfIdf.sum.post.stop                         -8.331e-01  2.685e+00
## D.TfIdf.sum.post.stem                          1.085e+00  2.792e+00
## D.sum.TfIdf                                           NA         NA
## D.npnct13.log                                  1.072e-01  4.195e-01
## D.npnct08.log                                  8.867e-01  7.267e-01
## `prdline.my.fctriPad 1`                        1.003e+00  5.816e-01
## `prdline.my.fctriPad 2`                        8.797e-01  5.954e-01
## `prdline.my.fctriPad 3+`                       1.323e+00  5.750e-01
## prdline.my.fctriPadAir                         2.161e+00  5.921e-01
## prdline.my.fctriPadmini                        1.223e+00  5.593e-01
## `prdline.my.fctriPadmini 2+`                   1.659e+00  6.059e-01
## color.fctrBlack                                1.822e-01  2.608e-01
## color.fctrGold                                -4.855e-01  5.392e-01
## `color.fctrSpace Gray`                        -1.409e-01  3.368e-01
## color.fctrWhite                                7.647e-03  2.599e-01
## D.npnct16.log                                  2.426e+00  1.905e+00
## D.npnct24.log                                 -4.795e+00  5.913e+00
## D.nstopwrds.log                                4.484e+00  2.124e+00
## D.npnct06.log                                 -4.935e+00  2.309e+00
## D.npnct28.log                                 -2.610e+00  1.743e+03
## D.nuppr.log                                    2.132e+00  3.396e+00
## D.nchrs.log                                   -3.885e+00  4.031e+00
## D.npnct12.log                                  7.763e-01  8.166e-01
## D.nwrds.log                                    8.015e-01  3.335e+00
## D.npnct09.log                                 -8.727e+00  8.198e+02
## D.ndgts.log                                    4.222e-01  5.733e-01
## D.nwrds.unq.log                               -4.625e+00  4.029e+00
## `carrier.fctrAT&T`                             7.071e-02  7.002e-01
## carrier.fctrOther                              1.604e+01  1.597e+03
## carrier.fctrSprint                             8.125e-01  9.375e-01
## `carrier.fctrT-Mobile`                        -1.070e+00  1.181e+00
## carrier.fctrUnknown                           -2.282e-01  4.828e-01
## carrier.fctrVerizon                            3.978e-01  7.522e-01
## cellular.fctr1                                 8.486e-04  6.363e-01
## cellular.fctrUnknown                                  NA         NA
## D.npnct14.log                                 -2.588e+00  1.307e+00
## D.terms.n.post.stem                            3.454e+00  4.287e+00
## D.terms.n.post.stop                           -3.537e+00  4.253e+00
## D.npnct05.log                                 -3.405e+00  1.972e+00
## `condition.fctrFor parts or not working`      -4.507e-01  4.100e-01
## `condition.fctrManufacturer refurbished`       3.981e-01  6.139e-01
## condition.fctrNew                             -2.824e-01  3.107e-01
## `condition.fctrNew other (see details)`        3.039e-02  4.695e-01
## `condition.fctrSeller refurbished`            -1.007e+00  4.921e-01
## idseq.my                                      -4.443e-04  2.092e-04
## startprice.log                                -1.043e+00  1.469e-01
## `prdline.my.fctrUnknown:.clusterid.fctr2`      2.493e+00  8.274e-01
## `prdline.my.fctriPad 1:.clusterid.fctr2`       1.021e+00  8.908e-01
## `prdline.my.fctriPad 2:.clusterid.fctr2`       7.596e-01  8.145e-01
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.330e+00  9.275e-01
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -4.528e-01  6.947e-01
## `prdline.my.fctriPadmini:.clusterid.fctr2`     1.137e+00  8.261e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  1.197e-03  1.020e+00
## `prdline.my.fctrUnknown:.clusterid.fctr3`      3.851e-02  1.185e+00
## `prdline.my.fctriPad 1:.clusterid.fctr3`       3.653e-01  9.627e-01
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.494e+01  7.547e+02
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -8.806e-02  7.145e-01
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -6.875e-01  1.021e+00
## `prdline.my.fctriPadmini:.clusterid.fctr3`     7.195e-01  9.506e-01
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  4.675e-01  9.433e-01
## `prdline.my.fctrUnknown:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.730e+00  1.006e+00
## `prdline.my.fctriPad 2:.clusterid.fctr4`       1.334e+00  1.498e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      1.106e+00  8.923e-01
## `prdline.my.fctriPadAir:.clusterid.fctr4`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -6.931e-01  1.021e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`         NA         NA
## `prdline.my.fctrUnknown:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPad 1:.clusterid.fctr5`              NA         NA
## `prdline.my.fctriPad 2:.clusterid.fctr5`       3.411e+00  1.499e+00
## `prdline.my.fctriPad 3+:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadAir:.clusterid.fctr5`             NA         NA
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -7.792e-01  1.434e+00
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`         NA         NA
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.440 0.149997    
## biddable                                        8.719  < 2e-16 ***
## D.ratio.nstopwrds.nwrds                        -2.366 0.017997 *  
## D.npnct15.log                                   1.602 0.109228    
## D.npnct03.log                                   0.514 0.607460    
## D.terms.n.stem.stop.Ratio                      -0.866 0.386575    
## D.ratio.sum.TfIdf.nwrds                        -0.120 0.904739    
## .rnorm                                          0.046 0.963012    
## D.npnct01.log                                   0.148 0.882380    
## D.TfIdf.sum.stem.stop.Ratio                    -0.386 0.699829    
## storage.fctr16                                  0.127 0.898716    
## storage.fctr32                                 -0.197 0.843465    
## storage.fctr64                                  1.364 0.172548    
## storage.fctrUnknown                            -0.252 0.801171    
## D.npnct11.log                                  -0.277 0.781974    
## D.npnct10.log                                  -0.021 0.983588    
## D.TfIdf.sum.post.stop                          -0.310 0.756374    
## D.TfIdf.sum.post.stem                           0.388 0.697647    
## D.sum.TfIdf                                        NA       NA    
## D.npnct13.log                                   0.256 0.798247    
## D.npnct08.log                                   1.220 0.222425    
## `prdline.my.fctriPad 1`                         1.725 0.084513 .  
## `prdline.my.fctriPad 2`                         1.478 0.139522    
## `prdline.my.fctriPad 3+`                        2.301 0.021395 *  
## prdline.my.fctriPadAir                          3.650 0.000263 ***
## prdline.my.fctriPadmini                         2.187 0.028717 *  
## `prdline.my.fctriPadmini 2+`                    2.738 0.006183 ** 
## color.fctrBlack                                 0.698 0.484880    
## color.fctrGold                                 -0.900 0.367892    
## `color.fctrSpace Gray`                         -0.419 0.675559    
## color.fctrWhite                                 0.029 0.976525    
## D.npnct16.log                                   1.273 0.202928    
## D.npnct24.log                                  -0.811 0.417459    
## D.nstopwrds.log                                 2.111 0.034766 *  
## D.npnct06.log                                  -2.137 0.032586 *  
## D.npnct28.log                                  -0.001 0.998805    
## D.nuppr.log                                     0.628 0.530079    
## D.nchrs.log                                    -0.964 0.335133    
## D.npnct12.log                                   0.951 0.341759    
## D.nwrds.log                                     0.240 0.810089    
## D.npnct09.log                                  -0.011 0.991507    
## D.ndgts.log                                     0.736 0.461475    
## D.nwrds.unq.log                                -1.148 0.251006    
## `carrier.fctrAT&T`                              0.101 0.919566    
## carrier.fctrOther                               0.010 0.991987    
## carrier.fctrSprint                              0.867 0.386129    
## `carrier.fctrT-Mobile`                         -0.906 0.365113    
## carrier.fctrUnknown                            -0.473 0.636368    
## carrier.fctrVerizon                             0.529 0.596945    
## cellular.fctr1                                  0.001 0.998936    
## cellular.fctrUnknown                               NA       NA    
## D.npnct14.log                                  -1.980 0.047667 *  
## D.terms.n.post.stem                             0.806 0.420422    
## D.terms.n.post.stop                            -0.832 0.405657    
## D.npnct05.log                                  -1.726 0.084297 .  
## `condition.fctrFor parts or not working`       -1.099 0.271609    
## `condition.fctrManufacturer refurbished`        0.648 0.516718    
## condition.fctrNew                              -0.909 0.363414    
## `condition.fctrNew other (see details)`         0.065 0.948384    
## `condition.fctrSeller refurbished`             -2.047 0.040654 *  
## idseq.my                                       -2.124 0.033704 *  
## startprice.log                                 -7.102 1.23e-12 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       3.013 0.002589 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.146 0.251773    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.933 0.351023    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.434 0.151523    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.652 0.514553    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.376 0.168767    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.001 0.999064    
## `prdline.my.fctrUnknown:.clusterid.fctr3`       0.033 0.974073    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.379 0.704344    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.020 0.984208    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.123 0.901910    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.674 0.500516    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.757 0.449106    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.496 0.620184    
## `prdline.my.fctrUnknown:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.719 0.085537 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.891 0.373039    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.240 0.215094    
## `prdline.my.fctriPadAir:.clusterid.fctr4`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.679 0.497248    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`      NA       NA    
## `prdline.my.fctrUnknown:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPad 1:.clusterid.fctr5`           NA       NA    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.276 0.022866 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadAir:.clusterid.fctr5`          NA       NA    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.543 0.586788    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.62  on 973  degrees of freedom
## Residual deviance:  784.26  on 894  degrees of freedom
## AIC: 944.26
## 
## Number of Fisher Scoring iterations: 15
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-4.png) ![](ebayipads_dupobs_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.7042030
## 3        0.2 0.7572464
## 4        0.3 0.7795918
## 5        0.4 0.8000000
## 6        0.5 0.8004587
## 7        0.6 0.7912621
## 8        0.7 0.7587097
## 9        0.8 0.6676218
## 10       0.9 0.5065789
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           451                            73
## 2         Y                           101                           349
##          Prediction
## Reference   N   Y
##         N 451  73
##         Y 101 349
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.213552e-01   6.390503e-01   7.958255e-01   8.449246e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.791948e-77   4.067137e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-6.png) ![](ebayipads_dupobs_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6672630
## 3        0.2 0.7081244
## 4        0.3 0.7317621
## 5        0.4 0.7475728
## 6        0.5 0.7573062
## 7        0.6 0.7576975
## 8        0.7 0.7308782
## 9        0.8 0.6448598
## 10       0.9 0.4990958
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glm.N sold.fctr.predict.All.X.glm.Y
## 1         N                           421                            54
## 2         Y                           127                           283
##          Prediction
## Reference   N   Y
##         N 421  54
##         Y 127 283
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.954802e-01   5.836701e-01   7.673772e-01   8.215999e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   4.462035e-58   8.712317e-08 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.453                 0.221
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8949364                    0.5       0.8004587        0.7515416
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7958255             0.8449246     0.4983565   0.8269987
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7576975        0.7954802
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7673772             0.8215999     0.5836701    944.2624
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009327212      0.01557657
##                   label step_major step_minor     bgn     end elapsed
## 2      fit.models_1_glm          2          0 115.337 120.613   5.277
## 3 fit.models_1_bayesglm          3          0 120.614      NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_dupobs_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4001  -0.6734  -0.2284   0.6303   2.3665  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                   10.2842486  6.4893782
## biddable                                       1.7573338  0.1973230
## D.ratio.nstopwrds.nwrds                       -1.6600153  2.3190873
## D.npnct15.log                                  1.7243105  0.9036160
## D.npnct03.log                                  0.1709185  1.6482521
## D.terms.n.stem.stop.Ratio                     -5.3365408  5.2987561
## D.ratio.sum.TfIdf.nwrds                       -0.2975649  0.2792717
## .rnorm                                         0.0085918  0.0914511
## D.npnct01.log                                  0.0976625  0.5307216
## D.TfIdf.sum.stem.stop.Ratio                    0.0820975  3.4101394
## storage.fctr16                                -0.0312320  0.4034695
## storage.fctr32                                -0.1607385  0.4366170
## storage.fctr64                                 0.5414545  0.4252873
## storage.fctrUnknown                           -0.1556108  0.5766591
## D.npnct11.log                                 -0.0703486  0.3534029
## D.npnct10.log                                 -7.4868921  7.5862091
## D.TfIdf.sum.post.stop                          0.0536202  0.2889917
## D.TfIdf.sum.post.stem                          0.0772985  0.3046543
## D.sum.TfIdf                                    0.0772985  0.3046543
## D.npnct13.log                                  0.0038540  0.3520800
## D.npnct08.log                                  0.6436918  0.6721688
## `prdline.my.fctriPad 1`                        0.6129401  0.4905833
## `prdline.my.fctriPad 2`                        0.5590282  0.4989079
## `prdline.my.fctriPad 3+`                       0.9228015  0.4730864
## prdline.my.fctriPadAir                         1.6331927  0.4867491
## prdline.my.fctriPadmini                        0.8234679  0.4623652
## `prdline.my.fctriPadmini 2+`                   1.1789282  0.5051392
## color.fctrBlack                                0.1754209  0.2484243
## color.fctrGold                                -0.4232961  0.4938666
## `color.fctrSpace Gray`                        -0.1515756  0.3206617
## color.fctrWhite                               -0.0476805  0.2450863
## D.npnct16.log                                  2.0953668  1.6045127
## D.npnct24.log                                  0.0096372  2.3183564
## D.nstopwrds.log                                0.7430813  0.6531890
## D.npnct06.log                                 -3.9327506  1.9017435
## D.npnct28.log                                 -0.0586218  2.1808018
## D.nuppr.log                                   -0.0921862  0.4809059
## D.nchrs.log                                   -0.1789244  0.4884053
## D.npnct12.log                                  0.5005917  0.7367023
## D.nwrds.log                                   -0.0174159  0.7577466
## D.npnct09.log                                 -1.8762155  4.8176276
## D.ndgts.log                                    0.6118216  0.4309216
## D.nwrds.unq.log                               -0.3662339  0.9899972
## `carrier.fctrAT&T`                            -0.0925699  0.7617184
## carrier.fctrOther                              1.4827438  1.6840761
## carrier.fctrSprint                             0.5934538  0.9178047
## `carrier.fctrT-Mobile`                        -1.0346543  1.0426395
## carrier.fctrUnknown                           -0.4669640  0.7673418
## carrier.fctrVerizon                            0.2125612  0.7888289
## cellular.fctr1                                 0.1663025  0.7310228
## cellular.fctrUnknown                           0.1023197  0.8486886
## D.npnct14.log                                 -1.9746863  1.0751784
## D.terms.n.post.stem                           -0.1008951  0.1999550
## D.terms.n.post.stop                           -0.1183221  0.1987110
## D.npnct05.log                                 -2.9551434  1.4986226
## `condition.fctrFor parts or not working`      -0.3343721  0.3843888
## `condition.fctrManufacturer refurbished`       0.3922178  0.5731989
## condition.fctrNew                             -0.2738450  0.2966734
## `condition.fctrNew other (see details)`        0.0766691  0.4358370
## `condition.fctrSeller refurbished`            -0.8332128  0.4550953
## idseq.my                                      -0.0004650  0.0002007
## startprice.log                                -0.9436826  0.1330383
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.9712145  0.6807140
## `prdline.my.fctriPad 1:.clusterid.fctr2`       0.8582323  0.7384926
## `prdline.my.fctriPad 2:.clusterid.fctr2`       0.6062147  0.6726430
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.0755689  0.7438557
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -0.3754393  0.5611276
## `prdline.my.fctriPadmini:.clusterid.fctr2`     0.8918425  0.7006279
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2` -0.0683592  0.8317439
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -0.3231263  0.9057692
## `prdline.my.fctriPad 1:.clusterid.fctr3`       0.2816765  0.7876395
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.8031878  1.5583563
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -0.3338476  0.6084017
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -0.6236151  0.8101234
## `prdline.my.fctriPadmini:.clusterid.fctr3`     0.7536847  0.7745361
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  0.4381060  0.7847287
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.6472311  0.8451679
## `prdline.my.fctriPad 2:.clusterid.fctr4`       0.7940189  1.1468944
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      0.8093186  0.7297984
## `prdline.my.fctriPadAir:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -0.5468567  0.8348141
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`       2.7465581  1.1464691
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -0.7231388  1.1060445
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.0000000  2.5000000
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.585 0.113016    
## biddable                                        8.906  < 2e-16 ***
## D.ratio.nstopwrds.nwrds                        -0.716 0.474112    
## D.npnct15.log                                   1.908 0.056361 .  
## D.npnct03.log                                   0.104 0.917410    
## D.terms.n.stem.stop.Ratio                      -1.007 0.313872    
## D.ratio.sum.TfIdf.nwrds                        -1.066 0.286648    
## .rnorm                                          0.094 0.925149    
## D.npnct01.log                                   0.184 0.853999    
## D.TfIdf.sum.stem.stop.Ratio                     0.024 0.980793    
## storage.fctr16                                 -0.077 0.938298    
## storage.fctr32                                 -0.368 0.712765    
## storage.fctr64                                  1.273 0.202965    
## storage.fctrUnknown                            -0.270 0.787276    
## D.npnct11.log                                  -0.199 0.842215    
## D.npnct10.log                                  -0.987 0.323688    
## D.TfIdf.sum.post.stop                           0.186 0.852804    
## D.TfIdf.sum.post.stem                           0.254 0.799708    
## D.sum.TfIdf                                     0.254 0.799708    
## D.npnct13.log                                   0.011 0.991266    
## D.npnct08.log                                   0.958 0.338247    
## `prdline.my.fctriPad 1`                         1.249 0.211515    
## `prdline.my.fctriPad 2`                         1.121 0.262499    
## `prdline.my.fctriPad 3+`                        1.951 0.051105 .  
## prdline.my.fctriPadAir                          3.355 0.000793 ***
## prdline.my.fctriPadmini                         1.781 0.074914 .  
## `prdline.my.fctriPadmini 2+`                    2.334 0.019603 *  
## color.fctrBlack                                 0.706 0.480105    
## color.fctrGold                                 -0.857 0.391386    
## `color.fctrSpace Gray`                         -0.473 0.636430    
## color.fctrWhite                                -0.195 0.845749    
## D.npnct16.log                                   1.306 0.191579    
## D.npnct24.log                                   0.004 0.996683    
## D.nstopwrds.log                                 1.138 0.255279    
## D.npnct06.log                                  -2.068 0.038643 *  
## D.npnct28.log                                  -0.027 0.978555    
## D.nuppr.log                                    -0.192 0.847983    
## D.nchrs.log                                    -0.366 0.714108    
## D.npnct12.log                                   0.680 0.496819    
## D.nwrds.log                                    -0.023 0.981663    
## D.npnct09.log                                  -0.389 0.696945    
## D.ndgts.log                                     1.420 0.155666    
## D.nwrds.unq.log                                -0.370 0.711431    
## `carrier.fctrAT&T`                             -0.122 0.903273    
## carrier.fctrOther                               0.880 0.378616    
## carrier.fctrSprint                              0.647 0.517890    
## `carrier.fctrT-Mobile`                         -0.992 0.321031    
## carrier.fctrUnknown                            -0.609 0.542824    
## carrier.fctrVerizon                             0.269 0.787572    
## cellular.fctr1                                  0.227 0.820041    
## cellular.fctrUnknown                            0.121 0.904038    
## D.npnct14.log                                  -1.837 0.066267 .  
## D.terms.n.post.stem                            -0.505 0.613847    
## D.terms.n.post.stop                            -0.595 0.551544    
## D.npnct05.log                                  -1.972 0.048620 *  
## `condition.fctrFor parts or not working`       -0.870 0.384366    
## `condition.fctrManufacturer refurbished`        0.684 0.493810    
## condition.fctrNew                              -0.923 0.355980    
## `condition.fctrNew other (see details)`         0.176 0.860363    
## `condition.fctrSeller refurbished`             -1.831 0.067122 .  
## idseq.my                                       -2.317 0.020502 *  
## startprice.log                                 -7.093 1.31e-12 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.896 0.003782 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.162 0.245178    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.901 0.367459    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.446 0.148195    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.669 0.503444    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.273 0.203047    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  -0.082 0.934497    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.357 0.721285    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.358 0.720627    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -1.157 0.247228    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.549 0.583192    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.770 0.441432    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.973 0.330514    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.558 0.576647    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.949 0.051296 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.692 0.488736    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.109 0.267447    
## `prdline.my.fctriPadAir:.clusterid.fctr4`       0.000 1.000000    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.655 0.512427    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000 1.000000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000 1.000000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.396 0.016590 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.654 0.513237    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000 1.000000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.6  on 973  degrees of freedom
## Residual deviance:  794.6  on 884  degrees of freedom
## AIC: 974.6
## 
## Number of Fisher Scoring iterations: 16
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6958561
## 3        0.2 0.7479821
## 4        0.3 0.7785642
## 5        0.4 0.7956284
## 6        0.5 0.8004587
## 7        0.6 0.7902439
## 8        0.7 0.7516426
## 9        0.8 0.6402349
## 10       0.9 0.4824121
## 11       1.0 0.0000000
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                451
## 2         Y                                101
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 73
## 2                                349
##          Prediction
## Reference   N   Y
##         N 451  73
##         Y 101 349
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.213552e-01   6.390503e-01   7.958255e-01   8.449246e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   2.791948e-77   4.067137e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6695652
## 3        0.2 0.7065868
## 4        0.3 0.7337808
## 5        0.4 0.7429963
## 6        0.5 0.7593308
## 7        0.6 0.7630522
## 8        0.7 0.7249284
## 9        0.8 0.6410256
## 10       0.9 0.4601113
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.bayesglm.N
## 1         N                                423
## 2         Y                                125
##   sold.fctr.predict.All.X.bayesglm.Y
## 1                                 52
## 2                                285
##          Prediction
## Reference   N   Y
##         N 423  52
##         Y 125 285
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.000000e-01   5.928708e-01   7.721016e-01   8.258843e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   3.310805e-60   6.237644e-08 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.635                 0.362
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8908779                    0.5       0.8004587        0.7669516
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7958255             0.8449246     0.5295107   0.8355584
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.6       0.7630522              0.8
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7721016             0.8258843     0.5928708     974.603
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01212752      0.02326004
##                   label step_major step_minor     bgn     end elapsed
## 3 fit.models_1_bayesglm          3          0 120.614 126.488   5.874
## 4   fit.models_1_glmnet          4          0 126.489      NA      NA
## [1] "fitting model: All.X.glmnet"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
```

```
## Loading required package: glmnet
## Loaded glmnet 2.0-2
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-12.png) 

```
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

![](ebayipads_dupobs_files/figure-html/fit.models_1-13.png) ![](ebayipads_dupobs_files/figure-html/fit.models_1-14.png) 

```
##             Length Class      Mode     
## a0           100   -none-     numeric  
## beta        8900   dgCMatrix  S4       
## df           100   -none-     numeric  
## dim            2   -none-     numeric  
## lambda       100   -none-     numeric  
## dev.ratio    100   -none-     numeric  
## nulldev        1   -none-     numeric  
## npasses        1   -none-     numeric  
## jerr           1   -none-     numeric  
## offset         1   -none-     logical  
## classnames     2   -none-     character
## call           5   -none-     call     
## nobs           1   -none-     numeric  
## lambdaOpt      1   -none-     numeric  
## xNames        89   -none-     character
## problemType    1   -none-     character
## tuneValue      2   data.frame list     
## obsLevels      2   -none-     character
## [1] "min lambda > lambdaOpt:"
##                              (Intercept) 
##                             1.9753355301 
##                                 biddable 
##                             1.4186035520 
##                            D.npnct15.log 
##                             0.8833841816 
##                            D.npnct03.log 
##                             0.0893533503 
##                D.terms.n.stem.stop.Ratio 
##                            -0.5838557245 
##                           storage.fctr64 
##                             0.2428437861 
##                      storage.fctrUnknown 
##                            -0.0280019846 
##                            D.npnct10.log 
##                            -1.1692524418 
##                    D.TfIdf.sum.post.stop 
##                             0.0010344029 
##                    D.TfIdf.sum.post.stem 
##                             0.0018434842 
##                              D.sum.TfIdf 
##                             0.0019102640 
##                    prdline.my.fctriPad 1 
##                             0.0871098970 
##                   prdline.my.fctriPadAir 
##                             0.0740732373 
##                          color.fctrBlack 
##                             0.0963310660 
##                           color.fctrGold 
##                            -0.2353652205 
##                          color.fctrWhite 
##                            -0.0403710740 
##                            D.npnct06.log 
##                            -0.4367253776 
##                            D.npnct09.log 
##                            -0.2765827232 
##                        carrier.fctrOther 
##                             1.2100208205 
##                       carrier.fctrSprint 
##                             0.2709322068 
##                     carrier.fctrT-Mobile 
##                            -0.4551417968 
##                      carrier.fctrUnknown 
##                            -0.3181932496 
##                      carrier.fctrVerizon 
##                             0.0848262090 
##                     cellular.fctrUnknown 
##                            -0.1318723558 
##                            D.npnct14.log 
##                            -0.6804478464 
##                      D.terms.n.post.stem 
##                            -0.0119275180 
##                      D.terms.n.post.stop 
##                            -0.0109172854 
##                            D.npnct05.log 
##                            -1.2291373453 
##   condition.fctrFor parts or not working 
##                             0.0457977249 
##                        condition.fctrNew 
##                            -0.2593969773 
##         condition.fctrSeller refurbished 
##                            -0.2213814714 
##                                 idseq.my 
##                            -0.0003644435 
##                           startprice.log 
##                            -0.3652449952 
##  prdline.my.fctrUnknown:.clusterid.fctr2 
##                             0.5169750662 
##   prdline.my.fctriPad 1:.clusterid.fctr2 
##                             0.5066809982 
##   prdline.my.fctriPad 2:.clusterid.fctr2 
##                             0.0986256212 
##  prdline.my.fctriPad 3+:.clusterid.fctr2 
##                            -0.4630167292 
##  prdline.my.fctriPadAir:.clusterid.fctr2 
##                            -0.0204172384 
## prdline.my.fctriPadmini:.clusterid.fctr2 
##                             0.3470599044 
##  prdline.my.fctrUnknown:.clusterid.fctr3 
##                            -0.4775161075 
##   prdline.my.fctriPad 2:.clusterid.fctr3 
##                            -1.1493044226 
##  prdline.my.fctriPad 3+:.clusterid.fctr3 
##                            -0.2052321581 
##  prdline.my.fctriPadAir:.clusterid.fctr3 
##                            -0.2202881294 
## prdline.my.fctriPadmini:.clusterid.fctr3 
##                             0.2919294663 
##   prdline.my.fctriPad 1:.clusterid.fctr4 
##                            -0.8540524090 
##  prdline.my.fctriPad 3+:.clusterid.fctr4 
##                             0.1690271726 
## prdline.my.fctriPadmini:.clusterid.fctr4 
##                            -0.1873109080 
##   prdline.my.fctriPad 2:.clusterid.fctr5 
##                             1.5723229494 
## prdline.my.fctriPadmini:.clusterid.fctr5 
##                            -0.4799131089 
## [1] "max lambda < lambdaOpt:"
##                                 (Intercept) 
##                               13.0615980462 
##                                    biddable 
##                                1.7984103365 
##                     D.ratio.nstopwrds.nwrds 
##                               -3.8248222824 
##                               D.npnct15.log 
##                                1.7741116999 
##                               D.npnct03.log 
##                                0.0951112633 
##                   D.terms.n.stem.stop.Ratio 
##                               -5.8078718391 
##                     D.ratio.sum.TfIdf.nwrds 
##                               -0.3507968770 
##                                      .rnorm 
##                                0.0065016394 
##                               D.npnct01.log 
##                                0.0249232418 
##                 D.TfIdf.sum.stem.stop.Ratio 
##                               -0.1294975229 
##                              storage.fctr16 
##                               -0.0435819968 
##                              storage.fctr32 
##                               -0.1786652751 
##                              storage.fctr64 
##                                0.5511926582 
##                         storage.fctrUnknown 
##                               -0.1875552339 
##                               D.npnct11.log 
##                               -0.0991024121 
##                               D.npnct10.log 
##                               -9.5282186238 
##                       D.TfIdf.sum.post.stop 
##                                0.0321699706 
##                       D.TfIdf.sum.post.stem 
##                                0.1025316927 
##                                 D.sum.TfIdf 
##                                0.0966686102 
##                               D.npnct13.log 
##                                0.0214655918 
##                               D.npnct08.log 
##                                0.6750349310 
##                       prdline.my.fctriPad 1 
##                                0.9034148359 
##                       prdline.my.fctriPad 2 
##                                0.8136058945 
##                      prdline.my.fctriPad 3+ 
##                                1.2491915621 
##                      prdline.my.fctriPadAir 
##                                2.0214797494 
##                     prdline.my.fctriPadmini 
##                                1.1306267184 
##                  prdline.my.fctriPadmini 2+ 
##                                1.5340693440 
##                             color.fctrBlack 
##                                0.1867262126 
##                              color.fctrGold 
##                               -0.5345521227 
##                        color.fctrSpace Gray 
##                               -0.1816783222 
##                             color.fctrWhite 
##                               -0.0557147994 
##                               D.npnct16.log 
##                                2.3328929191 
##                               D.npnct24.log 
##                               -0.4282994305 
##                             D.nstopwrds.log 
##                                1.3353717770 
##                               D.npnct06.log 
##                               -4.3224674321 
##                               D.npnct28.log 
##                               -0.9513567325 
##                                 D.nuppr.log 
##                               -0.1801112694 
##                                 D.nchrs.log 
##                               -0.4055057056 
##                               D.npnct12.log 
##                                0.5990195386 
##                                 D.nwrds.log 
##                               -0.0025018491 
##                               D.npnct09.log 
##                               -2.1529811598 
##                                 D.ndgts.log 
##                                0.6756134194 
##                             D.nwrds.unq.log 
##                               -0.7258229248 
##                            carrier.fctrAT&T 
##                                0.0841989394 
##                           carrier.fctrOther 
##                                6.1961336462 
##                          carrier.fctrSprint 
##                                0.8380084271 
##                        carrier.fctrT-Mobile 
##                               -1.1270136844 
##                         carrier.fctrUnknown 
##                               -0.2905420116 
##                         carrier.fctrVerizon 
##                                0.3867957555 
##                        cellular.fctrUnknown 
##                                0.0314320802 
##                               D.npnct14.log 
##                               -2.1699943926 
##                         D.terms.n.post.stem 
##                               -0.0984701623 
##                         D.terms.n.post.stop 
##                               -0.1362647628 
##                               D.npnct05.log 
##                               -3.5965727808 
##      condition.fctrFor parts or not working 
##                               -0.4007114963 
##      condition.fctrManufacturer refurbished 
##                                0.4381686537 
##                           condition.fctrNew 
##                               -0.2678093649 
##       condition.fctrNew other (see details) 
##                                0.0699778209 
##            condition.fctrSeller refurbished 
##                               -0.9328038123 
##                                    idseq.my 
##                               -0.0004517569 
##                              startprice.log 
##                               -0.9920755893 
##     prdline.my.fctrUnknown:.clusterid.fctr2 
##                                2.3814999068 
##      prdline.my.fctriPad 1:.clusterid.fctr2 
##                                1.0456498213 
##      prdline.my.fctriPad 2:.clusterid.fctr2 
##                                0.8044137479 
##     prdline.my.fctriPad 3+:.clusterid.fctr2 
##                               -1.1361599926 
##     prdline.my.fctriPadAir:.clusterid.fctr2 
##                               -0.3307491501 
##    prdline.my.fctriPadmini:.clusterid.fctr2 
##                                1.1267871434 
## prdline.my.fctriPadmini 2+:.clusterid.fctr2 
##                                0.0053621728 
##     prdline.my.fctrUnknown:.clusterid.fctr3 
##                               -0.0355033666 
##      prdline.my.fctriPad 1:.clusterid.fctr3 
##                                0.3979133332 
##      prdline.my.fctriPad 2:.clusterid.fctr3 
##                               -5.1691175978 
##     prdline.my.fctriPad 3+:.clusterid.fctr3 
##                               -0.2734142333 
##     prdline.my.fctriPadAir:.clusterid.fctr3 
##                               -0.7578308238 
##    prdline.my.fctriPadmini:.clusterid.fctr3 
##                                0.9344198655 
## prdline.my.fctriPadmini 2+:.clusterid.fctr3 
##                                0.6446447969 
##      prdline.my.fctriPad 1:.clusterid.fctr4 
##                               -1.7920829424 
##      prdline.my.fctriPad 2:.clusterid.fctr4 
##                                1.2955088952 
##     prdline.my.fctriPad 3+:.clusterid.fctr4 
##                                1.0722662339 
##    prdline.my.fctriPadmini:.clusterid.fctr4 
##                               -0.6092010590 
##      prdline.my.fctriPad 2:.clusterid.fctr5 
##                                3.4696099313 
##    prdline.my.fctriPadmini:.clusterid.fctr5 
##                               -0.8395062805 
## character(0)
## character(0)
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-15.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.6442377
## 3        0.2 0.7052117
## 4        0.3 0.7470817
## 5        0.4 0.7771798
## 6        0.5 0.7773933
## 7        0.6 0.7723270
## 8        0.7 0.6685796
## 9        0.8 0.4670051
## 10       0.9 0.2786260
## 11       1.0 0.0000000
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              444
## 2         Y                              113
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               80
## 2                              337
##          Prediction
## Reference   N   Y
##         N 444  80
##         Y 113 337
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.018480e-01   5.993205e-01   7.754050e-01   8.264517e-01   5.379877e-01 
## AccuracyPValue  McnemarPValue 
##   1.767324e-66   2.125576e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.6332046
## 2        0.1 0.6409449
## 3        0.2 0.6810422
## 4        0.3 0.7351695
## 5        0.4 0.7560386
## 6        0.5 0.7564103
## 7        0.6 0.7397260
## 8        0.7 0.6488189
## 9        0.8 0.4288425
## 10       0.9 0.2579281
## 11       1.0 0.0000000
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.glmnet.N
## 1         N                              400
## 2         Y                              115
##   sold.fctr.predict.All.X.glmnet.Y
## 1                               75
## 2                              295
##          Prediction
## Reference   N   Y
##         N 400  75
##         Y 115 295
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.853107e-01   5.653916e-01   7.567658e-01   8.119416e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   1.847896e-53   4.664158e-03 
##       model_id model_method
## 1 All.X.glmnet       glmnet
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               9                      5.319                 0.579
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.874207                    0.5       0.7773933        0.7782494
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.775405             0.8264517     0.5520479   0.8374685
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.7564103        0.7853107
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7567658             0.8119416     0.5653916
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01304713      0.02731799
##                 label step_major step_minor     bgn     end elapsed
## 4 fit.models_1_glmnet          4          0 126.489 135.776   9.287
## 5  fit.models_1_rpart          5          0 135.776      NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0111 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-19.png) ![](ebayipads_dupobs_files/figure-html/fit.models_1-20.png) 

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
##       D.nwrds.unq.log   < 2.138333  to the right, improve=  8.394783, (0 missing)
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
##       D.ratio.sum.TfIdf.nwrds     < 0.3676509 to the right, improve= 5.216095, (0 missing)
##       D.ratio.nstopwrds.nwrds     < 0.4       to the left,  improve= 4.204106, (0 missing)
##       D.TfIdf.sum.stem.stop.Ratio < 0.9255282 to the left,  improve= 3.901876, (0 missing)
##   Surrogate splits:
##       D.ratio.nstopwrds.nwrds < 0.4292763 to the left,  agree=0.648, adj=0.174, (0 split)
##       D.ratio.sum.TfIdf.nwrds < 0.2689021 to the right, agree=0.642, adj=0.159, (0 split)
##       D.TfIdf.sum.post.stop   < 1.734564  to the right, agree=0.636, adj=0.145, (0 split)
##       D.TfIdf.sum.post.stem   < 1.711335  to the right, agree=0.636, adj=0.145, (0 split)
##       D.sum.TfIdf             < 1.711335  to the right, agree=0.636, adj=0.145, (0 split)
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

![](ebayipads_dupobs_files/figure-html/fit.models_1-21.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_1-22.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_1-23.png) 

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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.562                 0.062
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
## 5 fit.models_1_rpart          5          0 135.776 140.938   5.163
## 6    fit.models_1_rf          6          0 140.939      NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
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

![](ebayipads_dupobs_files/figure-html/fit.models_1-24.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 45 on full training set
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-25.png) ![](ebayipads_dupobs_files/figure-html/fit.models_1-26.png) 

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
## importance        88   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                974   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            88   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-27.png) 

```
##    threshold   f.score
## 1        0.0 0.6320225
## 2        0.1 0.8166969
## 3        0.2 0.9404389
## 4        0.3 0.9836066
## 5        0.4 0.9966777
## 6        0.5 1.0000000
## 7        0.6 0.9988877
## 8        0.7 0.9498250
## 9        0.8 0.8578680
## 10       0.9 0.7482615
## 11       1.0 0.1132075
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-28.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_1-29.png) 

```
##    threshold    f.score
## 1        0.0 0.63320463
## 2        0.1 0.66495287
## 3        0.2 0.69902913
## 4        0.3 0.74505495
## 5        0.4 0.76960193
## 6        0.5 0.76485788
## 7        0.6 0.75482094
## 8        0.7 0.74964235
## 9        0.8 0.70481928
## 10       0.9 0.59666667
## 11       1.0 0.05225653
```

![](ebayipads_dupobs_files/figure-html/fit.models_1-30.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   sold.fctr sold.fctr.predict.All.X.no.rnorm.rf.N
## 1         N                                   375
## 2         Y                                    91
##   sold.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                   100
## 2                                   319
##          Prediction
## Reference   N   Y
##         N 375 100
##         Y  91 319
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.841808e-01   5.666714e-01   7.555883e-01   8.108669e-01   5.367232e-01 
## AccuracyPValue  McnemarPValue 
##   5.821830e-53   5.626839e-01 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     15.801                 4.558
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.8008199
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9962198                     1       0.59567   0.8385905
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7696019        0.7841808
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7555883             0.8108669     0.5666714
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01084091      0.02458222
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
## All.X.glmnet                           All.X.glmnet           glmnet
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       biddable, startprice.log
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.log
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            biddable, startprice.log
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              biddable, startprice.log
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                          biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.351
## Random.myrandom_classfr                 0                      0.265
## Max.cor.Y.cv.0.rpart                    0                      0.612
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.477
## Max.cor.Y.rpart                         3                      1.004
## Max.cor.Y.glm                           1                      1.043
## Interact.High.cor.Y.glm                 1                      1.015
## Low.cor.X.glm                           1                      1.427
## All.X.glm                               1                      1.453
## All.X.bayesglm                          1                      2.635
## All.X.glmnet                            9                      5.319
## All.X.no.rnorm.rpart                    3                      1.562
## All.X.no.rnorm.rf                       3                     15.801
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.002   0.5000000
## Random.myrandom_classfr                   0.002   0.5071756
## Max.cor.Y.cv.0.rpart                      0.012   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.008   0.8781425
## Max.cor.Y.rpart                           0.012   0.8172434
## Max.cor.Y.glm                             0.015   0.8402290
## Interact.High.cor.Y.glm                   0.014   0.8427523
## Low.cor.X.glm                             0.159   0.8851187
## All.X.glm                                 0.221   0.8949364
## All.X.bayesglm                            0.362   0.8908779
## All.X.glmnet                              0.579   0.8742070
## All.X.no.rnorm.rpart                      0.062   0.8214419
## All.X.no.rnorm.rf                         4.558   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6320225
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.8048246
## Max.cor.Y.rpart                              0.5       0.7628866
## Max.cor.Y.glm                                0.5       0.7570621
## Interact.High.cor.Y.glm                      0.4       0.7563396
## Low.cor.X.glm                                0.5       0.7944573
## All.X.glm                                    0.5       0.8004587
## All.X.bayesglm                               0.5       0.8004587
## All.X.glmnet                                 0.5       0.7773933
## All.X.no.rnorm.rpart                         0.6       0.7821901
## All.X.no.rnorm.rf                            0.5       1.0000000
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.5379877             0.5060896
## Random.myrandom_classfr          0.4620123             0.4303445
## Max.cor.Y.cv.0.rpart             0.5379877             0.5060896
## Max.cor.Y.cv.0.cp.0.rpart        0.8172485             0.7915181
## Max.cor.Y.rpart                  0.7731117             0.7604190
## Max.cor.Y.glm                    0.7803039             0.7518769
## Interact.High.cor.Y.glm          0.7690028             0.7454801
## Low.cor.X.glm                    0.7618107             0.7915181
## All.X.glm                        0.7515416             0.7958255
## All.X.bayesglm                   0.7669516             0.7958255
## All.X.glmnet                     0.7782494             0.7754050
## All.X.no.rnorm.rpart             0.8069991             0.7882906
## All.X.no.rnorm.rf                0.8008199             0.9962198
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.5696555     0.0000000   0.5000000
## Random.myrandom_classfr               0.4939104     0.0000000   0.5191913
## Max.cor.Y.cv.0.rpart                  0.5696555     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8410439     0.6330658   0.8075096
## Max.cor.Y.rpart                       0.8127790     0.5377260   0.8080205
## Max.cor.Y.glm                         0.8049446     0.5567446   0.8402773
## Interact.High.cor.Y.glm               0.7990590     0.5332090   0.8394608
## Low.cor.X.glm                         0.8410439     0.5194818   0.8296945
## All.X.glm                             0.8449246     0.4983565   0.8269987
## All.X.bayesglm                        0.8449246     0.5295107   0.8355584
## All.X.glmnet                          0.8264517     0.5520479   0.8374685
## All.X.no.rnorm.rpart                  0.8381305     0.6066762   0.8103723
## All.X.no.rnorm.rf                     1.0000000     0.5956700   0.8385905
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6332046
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.6       0.7439490
## Max.cor.Y.rpart                              0.5       0.7582697
## Max.cor.Y.glm                                0.4       0.7604938
## Interact.High.cor.Y.glm                      0.4       0.7636816
## Low.cor.X.glm                                0.5       0.7580026
## All.X.glm                                    0.6       0.7576975
## All.X.bayesglm                               0.6       0.7630522
## All.X.glmnet                                 0.5       0.7564103
## All.X.no.rnorm.rpart                         0.6       0.7600000
## All.X.no.rnorm.rf                            0.4       0.7696019
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.5367232             0.5032294
## Random.myrandom_classfr          0.4632768             0.4300283
## Max.cor.Y.cv.0.rpart             0.5367232             0.5032294
## Max.cor.Y.cv.0.cp.0.rpart        0.7728814             0.7438293
## Max.cor.Y.rpart                  0.7853107             0.7567658
## Max.cor.Y.glm                    0.7807910             0.7520575
## Interact.High.cor.Y.glm          0.7853107             0.7567658
## Low.cor.X.glm                    0.7864407             0.7579436
## All.X.glm                        0.7954802             0.7673772
## All.X.bayesglm                   0.8000000             0.7721016
## All.X.glmnet                     0.7853107             0.7567658
## All.X.no.rnorm.rpart             0.7966102             0.7685578
## All.X.no.rnorm.rf                0.7841808             0.7555883
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.5699717     0.0000000
## Random.myrandom_classfr               0.4967706     0.0000000
## Max.cor.Y.cv.0.rpart                  0.5699717     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8001036     0.5406159
## Max.cor.Y.rpart                       0.8119416     0.5658292
## Max.cor.Y.glm                         0.8076410     0.5584673
## Interact.High.cor.Y.glm               0.8119416     0.5671369
## Low.cor.X.glm                         0.8130160     0.5677516
## All.X.glm                             0.8215999     0.5836701
## All.X.bayesglm                        0.8258843     0.5928708
## All.X.glmnet                          0.8119416     0.5653916
## All.X.no.rnorm.rpart                  0.8226715     0.5861800
## All.X.no.rnorm.rf                     0.8108669     0.5666714
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                  0.009511979      0.02504360          NA
## Max.cor.Y.glm                    0.014187500      0.02978891    943.1953
## Interact.High.cor.Y.glm          0.013180511      0.02966064    946.8663
## Low.cor.X.glm                    0.009749000      0.02418586    935.6464
## All.X.glm                        0.009327212      0.01557657    944.2624
## All.X.bayesglm                   0.012127519      0.02326004    974.6030
## All.X.glmnet                     0.013047135      0.02731799          NA
## All.X.no.rnorm.rpart             0.016668501      0.03448591          NA
## All.X.no.rnorm.rf                0.010840914      0.02458222          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 6  fit.models_1_rf          6          0 140.939 160.449   19.51
## 7 fit.models_1_end          7          0 160.450      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1 111.325 160.457  49.133
## 12 fit.models          7          2 160.458      NA      NA
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
## All.X.glmnet                           All.X.glmnet           glmnet
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               feats
## MFO.myMFO_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .rnorm
## Random.myrandom_classfr                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       biddable, startprice.log
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  biddable, startprice.log
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            biddable, startprice.log
## Max.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              biddable, startprice.log
## Interact.High.cor.Y.glm                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    biddable, startprice.log, biddable:D.terms.n.post.stop, biddable:D.TfIdf.sum.post.stem, biddable:D.ratio.nstopwrds.nwrds, biddable:D.npnct06.log, biddable:D.nchrs.log, biddable:D.nwrds.unq.log, biddable:cellular.fctr
## Low.cor.X.glm                                                                                                                                                                                                                                                          biddable, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct06.log, D.npnct28.log, D.npnct12.log, D.npnct09.log, D.ndgts.log, cellular.fctr, D.npnct14.log, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glm                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.bayesglm            biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.glmnet              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rpart              biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
## All.X.no.rnorm.rf                 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.5071756
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.8781425
## Max.cor.Y.rpart                         3   0.8172434
## Max.cor.Y.glm                           1   0.8402290
## Interact.High.cor.Y.glm                 1   0.8427523
## Low.cor.X.glm                           1   0.8851187
## All.X.glm                               1   0.8949364
## All.X.bayesglm                          1   0.8908779
## All.X.glmnet                            9   0.8742070
## All.X.no.rnorm.rpart                    3   0.8214419
## All.X.no.rnorm.rf                       3   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6320225
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.4       0.8048246
## Max.cor.Y.rpart                              0.5       0.7628866
## Max.cor.Y.glm                                0.5       0.7570621
## Interact.High.cor.Y.glm                      0.4       0.7563396
## Low.cor.X.glm                                0.5       0.7944573
## All.X.glm                                    0.5       0.8004587
## All.X.bayesglm                               0.5       0.8004587
## All.X.glmnet                                 0.5       0.7773933
## All.X.no.rnorm.rpart                         0.6       0.7821901
## All.X.no.rnorm.rf                            0.5       1.0000000
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.5379877     0.0000000   0.5000000
## Random.myrandom_classfr          0.4620123     0.0000000   0.5191913
## Max.cor.Y.cv.0.rpart             0.5379877     0.0000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8172485     0.6330658   0.8075096
## Max.cor.Y.rpart                  0.7731117     0.5377260   0.8080205
## Max.cor.Y.glm                    0.7803039     0.5567446   0.8402773
## Interact.High.cor.Y.glm          0.7690028     0.5332090   0.8394608
## Low.cor.X.glm                    0.7618107     0.5194818   0.8296945
## All.X.glm                        0.7515416     0.4983565   0.8269987
## All.X.bayesglm                   0.7669516     0.5295107   0.8355584
## All.X.glmnet                     0.7782494     0.5520479   0.8374685
## All.X.no.rnorm.rpart             0.8069991     0.6066762   0.8103723
## All.X.no.rnorm.rf                0.8008199     0.5956700   0.8385905
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.4       0.6332046
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.6       0.7439490
## Max.cor.Y.rpart                              0.5       0.7582697
## Max.cor.Y.glm                                0.4       0.7604938
## Interact.High.cor.Y.glm                      0.4       0.7636816
## Low.cor.X.glm                                0.5       0.7580026
## All.X.glm                                    0.6       0.7576975
## All.X.bayesglm                               0.6       0.7630522
## All.X.glmnet                                 0.5       0.7564103
## All.X.no.rnorm.rpart                         0.6       0.7600000
## All.X.no.rnorm.rf                            0.4       0.7696019
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.5367232     0.0000000
## Random.myrandom_classfr          0.4632768     0.0000000
## Max.cor.Y.cv.0.rpart             0.5367232     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart        0.7728814     0.5406159
## Max.cor.Y.rpart                  0.7853107     0.5658292
## Max.cor.Y.glm                    0.7807910     0.5584673
## Interact.High.cor.Y.glm          0.7853107     0.5671369
## Low.cor.X.glm                    0.7864407     0.5677516
## All.X.glm                        0.7954802     0.5836701
## All.X.bayesglm                   0.8000000     0.5928708
## All.X.glmnet                     0.7853107     0.5653916
## All.X.no.rnorm.rpart             0.7966102     0.5861800
## All.X.no.rnorm.rf                0.7841808     0.5666714
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         2.84900285           500.0000000
## Random.myrandom_classfr                   3.77358491           500.0000000
## Max.cor.Y.cv.0.rpart                      1.63398693            83.3333333
## Max.cor.Y.cv.0.cp.0.rpart                 2.09643606           125.0000000
## Max.cor.Y.rpart                           0.99601594            83.3333333
## Max.cor.Y.glm                             0.95877277            66.6666667
## Interact.High.cor.Y.glm                   0.98522167            71.4285714
## Low.cor.X.glm                             0.70077085             6.2893082
## All.X.glm                                 0.68823125             4.5248869
## All.X.bayesglm                            0.37950664             2.7624309
## All.X.glmnet                              0.18800526             1.7271157
## All.X.no.rnorm.rpart                      0.64020487            16.1290323
## All.X.no.rnorm.rf                         0.06328713             0.2193945
##                           inv.aic.fit
## MFO.myMFO_classfr                  NA
## Random.myrandom_classfr            NA
## Max.cor.Y.cv.0.rpart               NA
## Max.cor.Y.cv.0.cp.0.rpart          NA
## Max.cor.Y.rpart                    NA
## Max.cor.Y.glm             0.001060226
## Interact.High.cor.Y.glm   0.001056115
## Low.cor.X.glm             0.001068780
## All.X.glm                 0.001059028
## All.X.bayesglm            0.001026059
## All.X.glmnet                       NA
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
## 13. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 5 rows containing missing values (geom_path).
```

```
## Warning: Removed 103 rows containing missing values (geom_point).
```

```
## Warning: Removed 8 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually if you must have them.
```

![](ebayipads_dupobs_files/figure-html/fit.models_2-1.png) 

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

![](ebayipads_dupobs_files/figure-html/fit.models_2-2.png) 

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
## 10            All.X.bayesglm        0.8000000   0.8355584     0.5928708
## 12      All.X.no.rnorm.rpart        0.7966102   0.8103723     0.5861800
## 9                  All.X.glm        0.7954802   0.8269987     0.5836701
## 8              Low.cor.X.glm        0.7864407   0.8296945     0.5677516
## 7    Interact.High.cor.Y.glm        0.7853107   0.8394608     0.5671369
## 11              All.X.glmnet        0.7853107   0.8374685     0.5653916
## 5            Max.cor.Y.rpart        0.7853107   0.8080205     0.5658292
## 13         All.X.no.rnorm.rf        0.7841808   0.8385905     0.5666714
## 6              Max.cor.Y.glm        0.7807910   0.8402773     0.5584673
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7728814   0.8075096     0.5406159
## 1          MFO.myMFO_classfr        0.5367232   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5367232   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4632768   0.5191913     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 10    974.6030                    0.6
## 12          NA                    0.6
## 9     944.2624                    0.6
## 8     935.6464                    0.5
## 7     946.8663                    0.4
## 11          NA                    0.5
## 5           NA                    0.5
## 13          NA                    0.4
## 6     943.1953                    0.4
## 4           NA                    0.6
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
## 13. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 45 rows containing missing values (geom_point).
```

```
## Warning: Removed 8 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually if you must have them.
```

![](ebayipads_dupobs_files/figure-html/fit.models_2-3.png) 

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
## [1] "Best model id: All.X.bayesglm"
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

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4001  -0.6734  -0.2284   0.6303   2.3665  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                   10.2842486  6.4893782
## biddable                                       1.7573338  0.1973230
## D.ratio.nstopwrds.nwrds                       -1.6600153  2.3190873
## D.npnct15.log                                  1.7243105  0.9036160
## D.npnct03.log                                  0.1709185  1.6482521
## D.terms.n.stem.stop.Ratio                     -5.3365408  5.2987561
## D.ratio.sum.TfIdf.nwrds                       -0.2975649  0.2792717
## .rnorm                                         0.0085918  0.0914511
## D.npnct01.log                                  0.0976625  0.5307216
## D.TfIdf.sum.stem.stop.Ratio                    0.0820975  3.4101394
## storage.fctr16                                -0.0312320  0.4034695
## storage.fctr32                                -0.1607385  0.4366170
## storage.fctr64                                 0.5414545  0.4252873
## storage.fctrUnknown                           -0.1556108  0.5766591
## D.npnct11.log                                 -0.0703486  0.3534029
## D.npnct10.log                                 -7.4868921  7.5862091
## D.TfIdf.sum.post.stop                          0.0536202  0.2889917
## D.TfIdf.sum.post.stem                          0.0772985  0.3046543
## D.sum.TfIdf                                    0.0772985  0.3046543
## D.npnct13.log                                  0.0038540  0.3520800
## D.npnct08.log                                  0.6436918  0.6721688
## `prdline.my.fctriPad 1`                        0.6129401  0.4905833
## `prdline.my.fctriPad 2`                        0.5590282  0.4989079
## `prdline.my.fctriPad 3+`                       0.9228015  0.4730864
## prdline.my.fctriPadAir                         1.6331927  0.4867491
## prdline.my.fctriPadmini                        0.8234679  0.4623652
## `prdline.my.fctriPadmini 2+`                   1.1789282  0.5051392
## color.fctrBlack                                0.1754209  0.2484243
## color.fctrGold                                -0.4232961  0.4938666
## `color.fctrSpace Gray`                        -0.1515756  0.3206617
## color.fctrWhite                               -0.0476805  0.2450863
## D.npnct16.log                                  2.0953668  1.6045127
## D.npnct24.log                                  0.0096372  2.3183564
## D.nstopwrds.log                                0.7430813  0.6531890
## D.npnct06.log                                 -3.9327506  1.9017435
## D.npnct28.log                                 -0.0586218  2.1808018
## D.nuppr.log                                   -0.0921862  0.4809059
## D.nchrs.log                                   -0.1789244  0.4884053
## D.npnct12.log                                  0.5005917  0.7367023
## D.nwrds.log                                   -0.0174159  0.7577466
## D.npnct09.log                                 -1.8762155  4.8176276
## D.ndgts.log                                    0.6118216  0.4309216
## D.nwrds.unq.log                               -0.3662339  0.9899972
## `carrier.fctrAT&T`                            -0.0925699  0.7617184
## carrier.fctrOther                              1.4827438  1.6840761
## carrier.fctrSprint                             0.5934538  0.9178047
## `carrier.fctrT-Mobile`                        -1.0346543  1.0426395
## carrier.fctrUnknown                           -0.4669640  0.7673418
## carrier.fctrVerizon                            0.2125612  0.7888289
## cellular.fctr1                                 0.1663025  0.7310228
## cellular.fctrUnknown                           0.1023197  0.8486886
## D.npnct14.log                                 -1.9746863  1.0751784
## D.terms.n.post.stem                           -0.1008951  0.1999550
## D.terms.n.post.stop                           -0.1183221  0.1987110
## D.npnct05.log                                 -2.9551434  1.4986226
## `condition.fctrFor parts or not working`      -0.3343721  0.3843888
## `condition.fctrManufacturer refurbished`       0.3922178  0.5731989
## condition.fctrNew                             -0.2738450  0.2966734
## `condition.fctrNew other (see details)`        0.0766691  0.4358370
## `condition.fctrSeller refurbished`            -0.8332128  0.4550953
## idseq.my                                      -0.0004650  0.0002007
## startprice.log                                -0.9436826  0.1330383
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.9712145  0.6807140
## `prdline.my.fctriPad 1:.clusterid.fctr2`       0.8582323  0.7384926
## `prdline.my.fctriPad 2:.clusterid.fctr2`       0.6062147  0.6726430
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.0755689  0.7438557
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -0.3754393  0.5611276
## `prdline.my.fctriPadmini:.clusterid.fctr2`     0.8918425  0.7006279
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2` -0.0683592  0.8317439
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -0.3231263  0.9057692
## `prdline.my.fctriPad 1:.clusterid.fctr3`       0.2816765  0.7876395
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -1.8031878  1.5583563
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -0.3338476  0.6084017
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -0.6236151  0.8101234
## `prdline.my.fctriPadmini:.clusterid.fctr3`     0.7536847  0.7745361
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  0.4381060  0.7847287
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -1.6472311  0.8451679
## `prdline.my.fctriPad 2:.clusterid.fctr4`       0.7940189  1.1468944
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      0.8093186  0.7297984
## `prdline.my.fctriPadAir:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr4`    -0.5468567  0.8348141
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`       2.7465581  1.1464691
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -0.7231388  1.1060445
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.0000000  2.5000000
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.585 0.113016    
## biddable                                        8.906  < 2e-16 ***
## D.ratio.nstopwrds.nwrds                        -0.716 0.474112    
## D.npnct15.log                                   1.908 0.056361 .  
## D.npnct03.log                                   0.104 0.917410    
## D.terms.n.stem.stop.Ratio                      -1.007 0.313872    
## D.ratio.sum.TfIdf.nwrds                        -1.066 0.286648    
## .rnorm                                          0.094 0.925149    
## D.npnct01.log                                   0.184 0.853999    
## D.TfIdf.sum.stem.stop.Ratio                     0.024 0.980793    
## storage.fctr16                                 -0.077 0.938298    
## storage.fctr32                                 -0.368 0.712765    
## storage.fctr64                                  1.273 0.202965    
## storage.fctrUnknown                            -0.270 0.787276    
## D.npnct11.log                                  -0.199 0.842215    
## D.npnct10.log                                  -0.987 0.323688    
## D.TfIdf.sum.post.stop                           0.186 0.852804    
## D.TfIdf.sum.post.stem                           0.254 0.799708    
## D.sum.TfIdf                                     0.254 0.799708    
## D.npnct13.log                                   0.011 0.991266    
## D.npnct08.log                                   0.958 0.338247    
## `prdline.my.fctriPad 1`                         1.249 0.211515    
## `prdline.my.fctriPad 2`                         1.121 0.262499    
## `prdline.my.fctriPad 3+`                        1.951 0.051105 .  
## prdline.my.fctriPadAir                          3.355 0.000793 ***
## prdline.my.fctriPadmini                         1.781 0.074914 .  
## `prdline.my.fctriPadmini 2+`                    2.334 0.019603 *  
## color.fctrBlack                                 0.706 0.480105    
## color.fctrGold                                 -0.857 0.391386    
## `color.fctrSpace Gray`                         -0.473 0.636430    
## color.fctrWhite                                -0.195 0.845749    
## D.npnct16.log                                   1.306 0.191579    
## D.npnct24.log                                   0.004 0.996683    
## D.nstopwrds.log                                 1.138 0.255279    
## D.npnct06.log                                  -2.068 0.038643 *  
## D.npnct28.log                                  -0.027 0.978555    
## D.nuppr.log                                    -0.192 0.847983    
## D.nchrs.log                                    -0.366 0.714108    
## D.npnct12.log                                   0.680 0.496819    
## D.nwrds.log                                    -0.023 0.981663    
## D.npnct09.log                                  -0.389 0.696945    
## D.ndgts.log                                     1.420 0.155666    
## D.nwrds.unq.log                                -0.370 0.711431    
## `carrier.fctrAT&T`                             -0.122 0.903273    
## carrier.fctrOther                               0.880 0.378616    
## carrier.fctrSprint                              0.647 0.517890    
## `carrier.fctrT-Mobile`                         -0.992 0.321031    
## carrier.fctrUnknown                            -0.609 0.542824    
## carrier.fctrVerizon                             0.269 0.787572    
## cellular.fctr1                                  0.227 0.820041    
## cellular.fctrUnknown                            0.121 0.904038    
## D.npnct14.log                                  -1.837 0.066267 .  
## D.terms.n.post.stem                            -0.505 0.613847    
## D.terms.n.post.stop                            -0.595 0.551544    
## D.npnct05.log                                  -1.972 0.048620 *  
## `condition.fctrFor parts or not working`       -0.870 0.384366    
## `condition.fctrManufacturer refurbished`        0.684 0.493810    
## condition.fctrNew                              -0.923 0.355980    
## `condition.fctrNew other (see details)`         0.176 0.860363    
## `condition.fctrSeller refurbished`             -1.831 0.067122 .  
## idseq.my                                       -2.317 0.020502 *  
## startprice.log                                 -7.093 1.31e-12 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.896 0.003782 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        1.162 0.245178    
## `prdline.my.fctriPad 2:.clusterid.fctr2`        0.901 0.367459    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.446 0.148195    
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.669 0.503444    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      1.273 0.203047    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  -0.082 0.934497    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -0.357 0.721285    
## `prdline.my.fctriPad 1:.clusterid.fctr3`        0.358 0.720627    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -1.157 0.247228    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -0.549 0.583192    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.770 0.441432    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      0.973 0.330514    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`   0.558 0.576647    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.949 0.051296 .  
## `prdline.my.fctriPad 2:.clusterid.fctr4`        0.692 0.488736    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.109 0.267447    
## `prdline.my.fctriPadAir:.clusterid.fctr4`       0.000 1.000000    
## `prdline.my.fctriPadmini:.clusterid.fctr4`     -0.655 0.512427    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000 1.000000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000 1.000000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.396 0.016590 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000 1.000000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.654 0.513237    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000 1.000000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1344.6  on 973  degrees of freedom
## Residual deviance:  794.6  on 884  degrees of freedom
## AIC: 974.6
## 
## Number of Fisher Scoring iterations: 16
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
##                                      N importance
## startprice.log              100.000000 100.000000
## biddable                     94.720022  94.720022
## idseq.my                     48.121762  48.121762
## prdline.my.fctr              32.302503  32.302503
## D.ratio.nstopwrds.nwrds      26.323096  26.323096
## D.TfIdf.sum.stem.stop.Ratio  23.352887  23.352887
## .rnorm                       20.456519  20.456519
## D.npnct15.log                19.859878  19.859878
## D.npnct03.log                19.699198  19.699198
## D.terms.n.stem.stop.Ratio    19.469403  19.469403
## D.npnct01.log                18.926519  18.926519
## storage.fctr                 18.264306  18.264306
## D.npnct28.log                18.184557  18.184557
## D.ratio.sum.TfIdf.nwrds      17.889190  17.889190
## D.npnct10.log                17.652897  17.652897
## D.npnct09.log                17.387066  17.387066
## D.npnct11.log                16.045510  16.045510
## D.npnct08.log                16.026016  16.026016
## D.TfIdf.sum.post.stop        15.682209  15.682209
## D.npnct12.log                15.644402  15.644402
## D.TfIdf.sum.post.stem        15.228525  15.228525
## D.sum.TfIdf                  15.228525  15.228525
## D.npnct16.log                15.122193  15.122193
## D.npnct06.log                14.718131  14.718131
## D.ndgts.log                  13.597509  13.597509
## D.npnct05.log                12.955382  12.955382
## D.npnct24.log                12.845505  12.845505
## D.npnct14.log                12.808880  12.808880
## D.npnct13.log                12.568451  12.568451
## D.nstopwrds.log              10.795659  10.795659
## .clusterid.fctr              10.171254  10.171254
## D.nwrds.log                   9.988717   9.988717
## color.fctr                    9.241439   9.241439
## D.nuppr.log                   8.117272   8.117272
## D.nchrs.log                   7.930601   7.930601
## carrier.fctr                  6.640438   6.640438
## D.terms.n.post.stop           6.303720   6.303720
## D.nwrds.unq.log               6.158399   6.158399
## D.terms.n.post.stem           6.158399   6.158399
## cellular.fctr                 4.979886   4.979886
## condition.fctr                0.000000   0.000000
##                             All.X.bayesglm.importance
## startprice.log                             100.000000
## biddable                                    94.720022
## idseq.my                                    48.121762
## prdline.my.fctr                             32.302503
## D.ratio.nstopwrds.nwrds                     26.323096
## D.TfIdf.sum.stem.stop.Ratio                 23.352887
## .rnorm                                      20.456519
## D.npnct15.log                               19.859878
## D.npnct03.log                               19.699198
## D.terms.n.stem.stop.Ratio                   19.469403
## D.npnct01.log                               18.926519
## storage.fctr                                18.264306
## D.npnct28.log                               18.184557
## D.ratio.sum.TfIdf.nwrds                     17.889190
## D.npnct10.log                               17.652897
## D.npnct09.log                               17.387066
## D.npnct11.log                               16.045510
## D.npnct08.log                               16.026016
## D.TfIdf.sum.post.stop                       15.682209
## D.npnct12.log                               15.644402
## D.TfIdf.sum.post.stem                       15.228525
## D.sum.TfIdf                                 15.228525
## D.npnct16.log                               15.122193
## D.npnct06.log                               14.718131
## D.ndgts.log                                 13.597509
## D.npnct05.log                               12.955382
## D.npnct24.log                               12.845505
## D.npnct14.log                               12.808880
## D.npnct13.log                               12.568451
## D.nstopwrds.log                             10.795659
## .clusterid.fctr                             10.171254
## D.nwrds.log                                  9.988717
## color.fctr                                   9.241439
## D.nuppr.log                                  8.117272
## D.nchrs.log                                  7.930601
## carrier.fctr                                 6.640438
## D.terms.n.post.stop                          6.303720
## D.nwrds.unq.log                              6.158399
## D.terms.n.post.stem                          6.158399
## cellular.fctr                                4.979886
## condition.fctr                               0.000000
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
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 41
```

![](ebayipads_dupobs_files/figure-html/fit.models_2-4.png) ![](ebayipads_dupobs_files/figure-html/fit.models_2-5.png) ![](ebayipads_dupobs_files/figure-html/fit.models_2-6.png) ![](ebayipads_dupobs_files/figure-html/fit.models_2-7.png) ![](ebayipads_dupobs_files/figure-html/fit.models_2-8.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.bayesglm.prob
## 66      10066         N                            0.03500128
## 1396    11397         N                            0.04164352
## 285     10285         Y                            0.99992051
## 1416    11417         N                            0.62937896
## 1699    11700         N                            0.65233489
## 537     10537         N                            0.68403197
## 1768    11769         N                            0.70369093
## 409     10409         N                            0.70460421
## 1356    11357         N                            0.71335357
## 199     10199         N                            0.76989832
## 282     10282         N                            0.78310867
## 127     10127         N                            0.78349682
## 254     10254         N                            0.78460014
## 851     10851         N                            0.81725329
## 283     10283         N                            0.98760455
##      sold.fctr.predict.All.X.bayesglm
## 66                                  N
## 1396                                N
## 285                                 Y
## 1416                                Y
## 1699                                Y
## 537                                 Y
## 1768                                Y
## 409                                 Y
## 1356                                Y
## 199                                 Y
## 282                                 Y
## 127                                 Y
## 254                                 Y
## 851                                 Y
## 283                                 Y
##      sold.fctr.predict.All.X.bayesglm.accurate
## 66                                        TRUE
## 1396                                      TRUE
## 285                                       TRUE
## 1416                                     FALSE
## 1699                                     FALSE
## 537                                      FALSE
## 1768                                     FALSE
## 409                                      FALSE
## 1356                                     FALSE
## 199                                      FALSE
## 282                                      FALSE
## 127                                      FALSE
## 254                                      FALSE
## 851                                      FALSE
## 283                                      FALSE
##      sold.fctr.predict.All.X.bayesglm.error .label
## 66                               0.00000000  10066
## 1396                             0.00000000  11397
## 285                              0.00000000  10285
## 1416                             0.02937896  11417
## 1699                             0.05233489  11700
## 537                              0.08403197  10537
## 1768                             0.10369093  11769
## 409                              0.10460421  10409
## 1356                             0.11335357  11357
## 199                              0.16989832  10199
## 282                              0.18310867  10282
## 127                              0.18349682  10127
## 254                              0.18460014  10254
## 851                              0.21725329  10851
## 283                              0.38760455  10283
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.All.X.bayesglm.prob
## 15      10015         Y                           0.000104908
## 1855    11857         Y                           0.001135559
## 1803    11804         Y                           0.012483692
## 1704    11705         Y                           0.015222047
## 1358    11359         Y                           0.016490237
## 178     10178         Y                           0.016948223
##      sold.fctr.predict.All.X.bayesglm
## 15                                  N
## 1855                                N
## 1803                                N
## 1704                                N
## 1358                                N
## 178                                 N
##      sold.fctr.predict.All.X.bayesglm.accurate
## 15                                       FALSE
## 1855                                     FALSE
## 1803                                     FALSE
## 1704                                     FALSE
## 1358                                     FALSE
## 178                                      FALSE
##      sold.fctr.predict.All.X.bayesglm.error
## 15                               -0.5998951
## 1855                             -0.5988644
## 1803                             -0.5875163
## 1704                             -0.5847780
## 1358                             -0.5835098
## 178                              -0.5830518
##      UniqueID sold.fctr sold.fctr.predict.All.X.bayesglm.prob
## 1724    11725         Y                            0.09574963
## 675     10675         Y                            0.12537046
## 27      10027         Y                            0.33584920
## 141     10141         Y                            0.37080708
## 1481    11482         N                            0.77169427
## 748     10748         N                            0.85395829
##      sold.fctr.predict.All.X.bayesglm
## 1724                                N
## 675                                 N
## 27                                  N
## 141                                 N
## 1481                                Y
## 748                                 Y
##      sold.fctr.predict.All.X.bayesglm.accurate
## 1724                                     FALSE
## 675                                      FALSE
## 27                                       FALSE
## 141                                      FALSE
## 1481                                     FALSE
## 748                                      FALSE
##      sold.fctr.predict.All.X.bayesglm.error
## 1724                             -0.5042504
## 675                              -0.4746295
## 27                               -0.2641508
## 141                              -0.2291929
## 1481                              0.1716943
## 748                               0.2539583
##      UniqueID sold.fctr sold.fctr.predict.All.X.bayesglm.prob
## 347     10347         N                             0.8539850
## 488     10488         N                             0.9007541
## 526     10526         N                             0.9038049
## 1431    11432         N                             0.9394273
## 283     10283         N                             0.9876045
## 491     10491         N                             0.9903817
##      sold.fctr.predict.All.X.bayesglm
## 347                                 Y
## 488                                 Y
## 526                                 Y
## 1431                                Y
## 283                                 Y
## 491                                 Y
##      sold.fctr.predict.All.X.bayesglm.accurate
## 347                                      FALSE
## 488                                      FALSE
## 526                                      FALSE
## 1431                                     FALSE
## 283                                      FALSE
## 491                                      FALSE
##      sold.fctr.predict.All.X.bayesglm.error
## 347                               0.2539850
## 488                               0.3007541
## 526                               0.3038049
## 1431                              0.3394273
## 283                               0.3876045
## 491                               0.3903817
```

![](ebayipads_dupobs_files/figure-html/fit.models_2-9.png) 

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
## 12 fit.models          7          2 160.458 177.159  16.701
## 13 fit.models          7          3 177.160      NA      NA
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
## [1] "sold.fctr.predict.All.X.bayesglm.prob"    
## [2] "sold.fctr.predict.All.X.bayesglm"         
## [3] "sold.fctr.predict.All.X.bayesglm.accurate"
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

![](ebayipads_dupobs_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 177.160 181.481   4.321
## 14 fit.data.training          8          0 181.481      NA      NA
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
## [1] "fitting model: Final.bayesglm"
## [1] "    indep_vars: biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr"
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.4691  -0.6900  -0.3200   0.6479   2.8499  
## 
## Coefficients:
##                                                 Estimate Std. Error
## (Intercept)                                    8.3033892  4.9300667
## biddable                                       1.5370896  0.1389684
## D.ratio.nstopwrds.nwrds                       -1.9658593  2.1412018
## D.npnct15.log                                  1.9776754  0.7385301
## D.npnct03.log                                 -0.6933728  1.0359646
## D.terms.n.stem.stop.Ratio                      0.2525043  3.9365184
## D.ratio.sum.TfIdf.nwrds                        0.0742532  0.2125512
## .rnorm                                         0.0147608  0.0640771
## D.npnct01.log                                  0.2214312  0.4051376
## D.TfIdf.sum.stem.stop.Ratio                   -2.3800888  2.8078889
## storage.fctr16                                 0.1942966  0.3208871
## storage.fctr32                                 0.1133594  0.3415676
## storage.fctr64                                 0.4264412  0.3311840
## storage.fctrUnknown                            0.5174260  0.4504924
## D.npnct11.log                                  0.0305816  0.2510956
## D.npnct10.log                                  0.5434612  1.3040951
## D.TfIdf.sum.post.stop                         -0.0393093  0.2859242
## D.TfIdf.sum.post.stem                          0.0295285  0.3020463
## D.sum.TfIdf                                    0.0295285  0.3020463
## D.npnct13.log                                 -0.1296193  0.2449487
## D.npnct08.log                                  0.0387829  0.4873544
## `prdline.my.fctriPad 1`                        0.0520009  0.3631591
## `prdline.my.fctriPad 2`                        0.3645185  0.3562388
## `prdline.my.fctriPad 3+`                       0.7440688  0.3553927
## prdline.my.fctriPadAir                         1.4886746  0.3597259
## prdline.my.fctriPadmini                        0.3868539  0.3450205
## `prdline.my.fctriPadmini 2+`                   1.1196646  0.3664707
## color.fctrBlack                                0.1677916  0.1766165
## color.fctrGold                                -0.2965323  0.3707394
## `color.fctrSpace Gray`                         0.0398129  0.2228328
## color.fctrWhite                                0.0220017  0.1702262
## D.npnct16.log                                  1.1738357  1.1456876
## D.npnct24.log                                 -1.8905008  2.1472294
## D.nstopwrds.log                                0.7888635  0.6077276
## D.npnct06.log                                 -1.6225092  1.2723778
## D.npnct28.log                                 -1.5357164  9.2880968
## D.nuppr.log                                   -0.0238192  0.4364738
## D.nchrs.log                                   -0.1320022  0.4620742
## D.npnct12.log                                 -0.0984848  0.4860841
## D.nwrds.log                                    0.2501422  0.7119122
## D.npnct09.log                                 -1.8386426  6.0058599
## D.ndgts.log                                    0.2764099  0.2793421
## D.nwrds.unq.log                               -0.0300525  0.8782860
## `carrier.fctrAT&T`                            -0.1718169  0.7191824
## carrier.fctrOther                              1.5990700  1.5992110
## carrier.fctrSprint                             0.3033087  0.8078622
## `carrier.fctrT-Mobile`                        -0.3892784  0.8810030
## carrier.fctrUnknown                           -0.4521347  0.7213488
## carrier.fctrVerizon                           -0.0110885  0.7275100
## cellular.fctr1                                 0.2814709  0.7026379
## cellular.fctrUnknown                          -0.0215896  0.7681777
## D.npnct14.log                                 -0.7171160  0.5832986
## D.terms.n.post.stem                           -0.0584319  0.1918298
## D.terms.n.post.stop                           -0.1255129  0.1914798
## D.npnct05.log                                 -2.1850166  1.1036368
## `condition.fctrFor parts or not working`      -0.1001831  0.2723663
## `condition.fctrManufacturer refurbished`       0.1656301  0.4323168
## condition.fctrNew                             -0.1949726  0.2135520
## `condition.fctrNew other (see details)`        0.0957792  0.3049501
## `condition.fctrSeller refurbished`            -0.6527678  0.3112225
## idseq.my                                      -0.0005699  0.0001413
## startprice.log                                -1.0681319  0.1092686
## `prdline.my.fctrUnknown:.clusterid.fctr2`      1.3818867  0.4898861
## `prdline.my.fctriPad 1:.clusterid.fctr2`       0.1008549  0.5582433
## `prdline.my.fctriPad 2:.clusterid.fctr2`      -0.2993927  0.4333178
## `prdline.my.fctriPad 3+:.clusterid.fctr2`     -1.0003153  0.5092025
## `prdline.my.fctriPadAir:.clusterid.fctr2`     -0.3846058  0.4120187
## `prdline.my.fctriPadmini:.clusterid.fctr2`     0.0836998  0.5685292
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`  0.0674316  0.6482009
## `prdline.my.fctrUnknown:.clusterid.fctr3`     -1.3206917  0.7596466
## `prdline.my.fctriPad 1:.clusterid.fctr3`      -0.0326264  0.6061938
## `prdline.my.fctriPad 2:.clusterid.fctr3`      -0.3792366  0.6731810
## `prdline.my.fctriPad 3+:.clusterid.fctr3`     -0.5456619  0.4906993
## `prdline.my.fctriPadAir:.clusterid.fctr3`     -0.3396675  0.6205177
## `prdline.my.fctriPadmini:.clusterid.fctr3`     0.7482879  0.5679204
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3` -0.0625266  0.5715438
## `prdline.my.fctrUnknown:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr4`      -0.7812584  0.6760323
## `prdline.my.fctriPad 2:.clusterid.fctr4`       0.9145859  0.7019221
## `prdline.my.fctriPad 3+:.clusterid.fctr4`      0.7380543  0.5423606
## `prdline.my.fctriPadAir:.clusterid.fctr4`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr4`     0.0178703  0.6206451
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`  0.0000000  2.5000000
## `prdline.my.fctrUnknown:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPad 1:.clusterid.fctr5`       0.0000000  2.5000000
## `prdline.my.fctriPad 2:.clusterid.fctr5`       1.3837747  0.6847777
## `prdline.my.fctriPad 3+:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadAir:.clusterid.fctr5`      0.0000000  2.5000000
## `prdline.my.fctriPadmini:.clusterid.fctr5`    -0.4971432  0.7960420
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`  0.0000000  2.5000000
##                                               z value Pr(>|z|)    
## (Intercept)                                     1.684  0.09214 .  
## biddable                                       11.061  < 2e-16 ***
## D.ratio.nstopwrds.nwrds                        -0.918  0.35856    
## D.npnct15.log                                   2.678  0.00741 ** 
## D.npnct03.log                                  -0.669  0.50330    
## D.terms.n.stem.stop.Ratio                       0.064  0.94886    
## D.ratio.sum.TfIdf.nwrds                         0.349  0.72683    
## .rnorm                                          0.230  0.81781    
## D.npnct01.log                                   0.547  0.58468    
## D.TfIdf.sum.stem.stop.Ratio                    -0.848  0.39664    
## storage.fctr16                                  0.605  0.54485    
## storage.fctr32                                  0.332  0.73998    
## storage.fctr64                                  1.288  0.19788    
## storage.fctrUnknown                             1.149  0.25073    
## D.npnct11.log                                   0.122  0.90306    
## D.npnct10.log                                   0.417  0.67687    
## D.TfIdf.sum.post.stop                          -0.137  0.89065    
## D.TfIdf.sum.post.stem                           0.098  0.92212    
## D.sum.TfIdf                                     0.098  0.92212    
## D.npnct13.log                                  -0.529  0.59669    
## D.npnct08.log                                   0.080  0.93657    
## `prdline.my.fctriPad 1`                         0.143  0.88614    
## `prdline.my.fctriPad 2`                         1.023  0.30619    
## `prdline.my.fctriPad 3+`                        2.094  0.03629 *  
## prdline.my.fctriPadAir                          4.138 3.50e-05 ***
## prdline.my.fctriPadmini                         1.121  0.26218    
## `prdline.my.fctriPadmini 2+`                    3.055  0.00225 ** 
## color.fctrBlack                                 0.950  0.34210    
## color.fctrGold                                 -0.800  0.42380    
## `color.fctrSpace Gray`                          0.179  0.85820    
## color.fctrWhite                                 0.129  0.89716    
## D.npnct16.log                                   1.025  0.30557    
## D.npnct24.log                                  -0.880  0.37862    
## D.nstopwrds.log                                 1.298  0.19427    
## D.npnct06.log                                  -1.275  0.20225    
## D.npnct28.log                                  -0.165  0.86867    
## D.nuppr.log                                    -0.055  0.95648    
## D.nchrs.log                                    -0.286  0.77513    
## D.npnct12.log                                  -0.203  0.83944    
## D.nwrds.log                                     0.351  0.72531    
## D.npnct09.log                                  -0.306  0.75950    
## D.ndgts.log                                     0.990  0.32242    
## D.nwrds.unq.log                                -0.034  0.97270    
## `carrier.fctrAT&T`                             -0.239  0.81118    
## carrier.fctrOther                               1.000  0.31735    
## carrier.fctrSprint                              0.375  0.70733    
## `carrier.fctrT-Mobile`                         -0.442  0.65859    
## carrier.fctrUnknown                            -0.627  0.53080    
## carrier.fctrVerizon                            -0.015  0.98784    
## cellular.fctr1                                  0.401  0.68872    
## cellular.fctrUnknown                           -0.028  0.97758    
## D.npnct14.log                                  -1.229  0.21892    
## D.terms.n.post.stem                            -0.305  0.76067    
## D.terms.n.post.stop                            -0.655  0.51215    
## D.npnct05.log                                  -1.980  0.04772 *  
## `condition.fctrFor parts or not working`       -0.368  0.71300    
## `condition.fctrManufacturer refurbished`        0.383  0.70163    
## condition.fctrNew                              -0.913  0.36124    
## `condition.fctrNew other (see details)`         0.314  0.75346    
## `condition.fctrSeller refurbished`             -2.097  0.03596 *  
## idseq.my                                       -4.034 5.49e-05 ***
## startprice.log                                 -9.775  < 2e-16 ***
## `prdline.my.fctrUnknown:.clusterid.fctr2`       2.821  0.00479 ** 
## `prdline.my.fctriPad 1:.clusterid.fctr2`        0.181  0.85663    
## `prdline.my.fctriPad 2:.clusterid.fctr2`       -0.691  0.48961    
## `prdline.my.fctriPad 3+:.clusterid.fctr2`      -1.964  0.04948 *  
## `prdline.my.fctriPadAir:.clusterid.fctr2`      -0.933  0.35058    
## `prdline.my.fctriPadmini:.clusterid.fctr2`      0.147  0.88296    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr2`   0.104  0.91715    
## `prdline.my.fctrUnknown:.clusterid.fctr3`      -1.739  0.08211 .  
## `prdline.my.fctriPad 1:.clusterid.fctr3`       -0.054  0.95708    
## `prdline.my.fctriPad 2:.clusterid.fctr3`       -0.563  0.57320    
## `prdline.my.fctriPad 3+:.clusterid.fctr3`      -1.112  0.26613    
## `prdline.my.fctriPadAir:.clusterid.fctr3`      -0.547  0.58411    
## `prdline.my.fctriPadmini:.clusterid.fctr3`      1.318  0.18764    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr3`  -0.109  0.91289    
## `prdline.my.fctrUnknown:.clusterid.fctr4`       0.000  1.00000    
## `prdline.my.fctriPad 1:.clusterid.fctr4`       -1.156  0.24782    
## `prdline.my.fctriPad 2:.clusterid.fctr4`        1.303  0.19258    
## `prdline.my.fctriPad 3+:.clusterid.fctr4`       1.361  0.17357    
## `prdline.my.fctriPadAir:.clusterid.fctr4`       0.000  1.00000    
## `prdline.my.fctriPadmini:.clusterid.fctr4`      0.029  0.97703    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr4`   0.000  1.00000    
## `prdline.my.fctrUnknown:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPad 1:.clusterid.fctr5`        0.000  1.00000    
## `prdline.my.fctriPad 2:.clusterid.fctr5`        2.021  0.04330 *  
## `prdline.my.fctriPad 3+:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPadAir:.clusterid.fctr5`       0.000  1.00000    
## `prdline.my.fctriPadmini:.clusterid.fctr5`     -0.625  0.53229    
## `prdline.my.fctriPadmini 2+:.clusterid.fctr5`   0.000  1.00000    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2566.7  on 1858  degrees of freedom
## Residual deviance: 1608.1  on 1769  degrees of freedom
## AIC: 1788.1
## 
## Number of Fisher Scoring iterations: 13
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](ebayipads_dupobs_files/figure-html/fit.data.training_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.6325855
## 2        0.1 0.6773163
## 3        0.2 0.7309312
## 4        0.3 0.7625133
## 5        0.4 0.7762399
## 6        0.5 0.7814408
## 7        0.6 0.7840617
## 8        0.7 0.7429359
## 9        0.8 0.6326373
## 10       0.9 0.4424460
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   sold.fctr sold.fctr.predict.Final.bayesglm.N
## 1         N                                913
## 2         Y                                250
##   sold.fctr.predict.Final.bayesglm.Y
## 1                                 86
## 2                                610
##          Prediction
## Reference   N   Y
##         N 913  86
##         Y 250 610
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.192577e-01   6.315955e-01   8.009972e-01   8.365047e-01   5.373857e-01 
## AccuracyPValue  McnemarPValue 
##  5.457450e-144   5.981426e-19
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](ebayipads_dupobs_files/figure-html/fit.data.training_0-2.png) 

```
##         model_id model_method
## 1 Final.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       feats
## 1 biddable, D.ratio.nstopwrds.nwrds, D.npnct15.log, D.npnct03.log, D.terms.n.stem.stop.Ratio, D.ratio.sum.TfIdf.nwrds, .rnorm, D.npnct01.log, D.TfIdf.sum.stem.stop.Ratio, storage.fctr, D.npnct11.log, D.npnct10.log, D.TfIdf.sum.post.stop, D.TfIdf.sum.post.stem, D.sum.TfIdf, D.npnct13.log, D.npnct08.log, prdline.my.fctr, color.fctr, D.npnct16.log, D.npnct24.log, D.nstopwrds.log, D.npnct06.log, D.npnct28.log, D.nuppr.log, D.nchrs.log, D.npnct12.log, D.nwrds.log, D.npnct09.log, D.ndgts.log, D.nwrds.unq.log, carrier.fctr, cellular.fctr, D.npnct14.log, D.terms.n.post.stem, D.terms.n.post.stop, D.npnct05.log, condition.fctr, idseq.my, startprice.log, prdline.my.fctr:.clusterid.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.277                   0.5
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8739914                    0.6       0.7840617        0.7821373
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.8009972             0.8365047     0.5594478    1788.075
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.009931481      0.01992347
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 181.481 186.502   5.021
## 15 fit.data.training          8          1 186.502      NA      NA
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
##                                    N.x All.X.bayesglm.importance
## startprice.log              100.000000                100.000000
## biddable                     94.720022                 94.720022
## idseq.my                     48.121762                 48.121762
## D.ratio.nstopwrds.nwrds      26.323096                 26.323096
## D.TfIdf.sum.stem.stop.Ratio  23.352887                 23.352887
## D.npnct15.log                19.859878                 19.859878
## D.terms.n.stem.stop.Ratio    19.469403                 19.469403
## D.npnct03.log                19.699198                 19.699198
## D.npnct01.log                18.926519                 18.926519
## D.npnct10.log                17.652897                 17.652897
## .rnorm                       20.456519                 20.456519
## storage.fctr                 18.264306                 18.264306
## D.npnct28.log                18.184557                 18.184557
## D.npnct09.log                17.387066                 17.387066
## D.npnct11.log                16.045510                 16.045510
## D.npnct08.log                16.026016                 16.026016
## D.npnct12.log                15.644402                 15.644402
## D.ratio.sum.TfIdf.nwrds      17.889190                 17.889190
## D.npnct16.log                15.122193                 15.122193
## D.npnct06.log                14.718131                 14.718131
## D.npnct14.log                12.808880                 12.808880
## D.TfIdf.sum.post.stop        15.682209                 15.682209
## D.ndgts.log                  13.597509                 13.597509
## D.TfIdf.sum.post.stem        15.228525                 15.228525
## D.sum.TfIdf                  15.228525                 15.228525
## D.npnct05.log                12.955382                 12.955382
## D.npnct13.log                12.568451                 12.568451
## color.fctr                    9.241439                  9.241439
## D.npnct24.log                12.845505                 12.845505
## D.nstopwrds.log              10.795659                 10.795659
## .clusterid.fctr              10.171254                 10.171254
## prdline.my.fctr              32.302503                 32.302503
## D.nwrds.log                   9.988717                  9.988717
## D.nuppr.log                   8.117272                  8.117272
## D.nchrs.log                   7.930601                  7.930601
## carrier.fctr                  6.640438                  6.640438
## cellular.fctr                 4.979886                  4.979886
## D.terms.n.post.stop           6.303720                  6.303720
## D.nwrds.unq.log               6.158399                  6.158399
## D.terms.n.post.stem           6.158399                  6.158399
## condition.fctr                0.000000                  0.000000
##                                    N.y importance
## startprice.log              100.000000 100.000000
## biddable                     95.026117  95.026117
## idseq.my                     49.511380  49.511380
## D.ratio.nstopwrds.nwrds      27.012354  27.012354
## D.TfIdf.sum.stem.stop.Ratio  22.182931  22.182931
## D.npnct15.log                20.409144  20.409144
## D.terms.n.stem.stop.Ratio    20.407526  20.407526
## D.npnct03.log                19.628451  19.628451
## D.npnct01.log                19.349887  19.349887
## D.npnct10.log                18.632122  18.632122
## .rnorm                       18.288850  18.288850
## storage.fctr                 18.248570  18.248570
## D.npnct28.log                18.170112  18.170112
## D.npnct09.log                17.752750  17.752750
## D.npnct11.log                17.304168  17.304168
## D.npnct08.log                16.596594  16.596594
## D.npnct12.log                16.183600  16.183600
## D.ratio.sum.TfIdf.nwrds      16.015847  16.015847
## D.npnct16.log                16.010185  16.010185
## D.npnct06.log                15.782577  15.782577
## D.npnct14.log                15.280287  15.280287
## D.TfIdf.sum.post.stop        14.864382  14.864382
## D.ndgts.log                  14.605229  14.605229
## D.TfIdf.sum.post.stem        14.423240  14.423240
## D.sum.TfIdf                  14.423240  14.423240
## D.npnct05.log                13.947319  13.947319
## D.npnct13.log                13.585120  13.585120
## color.fctr                   13.332115  13.332115
## D.npnct24.log                12.688763  12.688763
## D.nstopwrds.log              12.403890  12.403890
## .clusterid.fctr              11.936703  11.936703
## prdline.my.fctr              11.912115  11.912115
## D.nwrds.log                  10.911417  10.911417
## D.nuppr.log                   9.940809   9.940809
## D.nchrs.log                   9.277722   9.277722
## carrier.fctr                  9.121777   9.121777
## cellular.fctr                 8.675136   8.675136
## D.terms.n.post.stop           8.265215   8.265215
## D.nwrds.unq.log               8.221215   8.221215
## D.terms.n.post.stem           8.221215   8.221215
## condition.fctr                0.000000   0.000000
##                             Final.bayesglm.importance
## startprice.log                             100.000000
## biddable                                    95.026117
## idseq.my                                    49.511380
## D.ratio.nstopwrds.nwrds                     27.012354
## D.TfIdf.sum.stem.stop.Ratio                 22.182931
## D.npnct15.log                               20.409144
## D.terms.n.stem.stop.Ratio                   20.407526
## D.npnct03.log                               19.628451
## D.npnct01.log                               19.349887
## D.npnct10.log                               18.632122
## .rnorm                                      18.288850
## storage.fctr                                18.248570
## D.npnct28.log                               18.170112
## D.npnct09.log                               17.752750
## D.npnct11.log                               17.304168
## D.npnct08.log                               16.596594
## D.npnct12.log                               16.183600
## D.ratio.sum.TfIdf.nwrds                     16.015847
## D.npnct16.log                               16.010185
## D.npnct06.log                               15.782577
## D.npnct14.log                               15.280287
## D.TfIdf.sum.post.stop                       14.864382
## D.ndgts.log                                 14.605229
## D.TfIdf.sum.post.stem                       14.423240
## D.sum.TfIdf                                 14.423240
## D.npnct05.log                               13.947319
## D.npnct13.log                               13.585120
## color.fctr                                  13.332115
## D.npnct24.log                               12.688763
## D.nstopwrds.log                             12.403890
## .clusterid.fctr                             11.936703
## prdline.my.fctr                             11.912115
## D.nwrds.log                                 10.911417
## D.nuppr.log                                  9.940809
## D.nchrs.log                                  9.277722
## carrier.fctr                                 9.121777
## cellular.fctr                                8.675136
## D.terms.n.post.stop                          8.265215
## D.nwrds.unq.log                              8.221215
## D.terms.n.post.stem                          8.221215
## condition.fctr                               0.000000
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 41
```

![](ebayipads_dupobs_files/figure-html/fit.data.training_0-3.png) ![](ebayipads_dupobs_files/figure-html/fit.data.training_0-4.png) ![](ebayipads_dupobs_files/figure-html/fit.data.training_0-5.png) ![](ebayipads_dupobs_files/figure-html/fit.data.training_0-6.png) ![](ebayipads_dupobs_files/figure-html/fit.data.training_0-7.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## 15      10015         Y                            0.06652246
## 11      10011         Y                            0.18308575
## 17      10017         Y                            0.25749470
## 141     10141         Y                            0.35574103
## 3       10003         Y                            0.37851102
## 14      10014         Y                            0.49592550
## 2       10002         Y                            0.99793570
## 91      10091         Y                            0.99997322
## 1396    11397         N                            0.03031021
## 135     10135         N                            0.60257264
## 1       10001         N                            0.65335221
## 127     10127         N                            0.72540687
## 120     10120         N                            0.76598889
## 182     10182         N                            0.81245804
## 103     10103         N                            0.88154075
##      sold.fctr.predict.Final.bayesglm
## 15                                  N
## 11                                  N
## 17                                  N
## 141                                 N
## 3                                   N
## 14                                  N
## 2                                   Y
## 91                                  Y
## 1396                                N
## 135                                 Y
## 1                                   Y
## 127                                 Y
## 120                                 Y
## 182                                 Y
## 103                                 Y
##      sold.fctr.predict.Final.bayesglm.accurate
## 15                                       FALSE
## 11                                       FALSE
## 17                                       FALSE
## 141                                      FALSE
## 3                                        FALSE
## 14                                       FALSE
## 2                                         TRUE
## 91                                        TRUE
## 1396                                      TRUE
## 135                                      FALSE
## 1                                        FALSE
## 127                                      FALSE
## 120                                      FALSE
## 182                                      FALSE
## 103                                      FALSE
##      sold.fctr.predict.Final.bayesglm.error .label
## 15                             -0.533477543  10015
## 11                             -0.416914253  10011
## 17                             -0.342505298  10017
## 141                            -0.244258967  10141
## 3                              -0.221488982  10003
## 14                             -0.104074496  10014
## 2                               0.000000000  10002
## 91                              0.000000000  10091
## 1396                            0.000000000  11397
## 135                             0.002572636  10135
## 1                               0.053352213  10001
## 127                             0.125406871  10127
## 120                             0.165988886  10120
## 182                             0.212458037  10182
## 103                             0.281540745  10103
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## 1704    11705         Y                            0.01723298
## 1358    11359         Y                            0.02804533
## 1803    11804         Y                            0.04235968
## 15      10015         Y                            0.06652246
## 1523    11524         Y                            0.07340917
## 1783    11784         Y                            0.07751727
##      sold.fctr.predict.Final.bayesglm
## 1704                                N
## 1358                                N
## 1803                                N
## 15                                  N
## 1523                                N
## 1783                                N
##      sold.fctr.predict.Final.bayesglm.accurate
## 1704                                     FALSE
## 1358                                     FALSE
## 1803                                     FALSE
## 15                                       FALSE
## 1523                                     FALSE
## 1783                                     FALSE
##      sold.fctr.predict.Final.bayesglm.error
## 1704                             -0.5827670
## 1358                             -0.5719547
## 1803                             -0.5576403
## 15                               -0.5334775
## 1523                             -0.5265908
## 1783                             -0.5224827
##      UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## 1730    11731         Y                             0.1618909
## 312     10312         Y                             0.2454240
## 717     10717         Y                             0.2678947
## 37      10037         Y                             0.3645673
## 562     10562         Y                             0.4660657
## 902     10902         Y                             0.4708093
##      sold.fctr.predict.Final.bayesglm
## 1730                                N
## 312                                 N
## 717                                 N
## 37                                  N
## 562                                 N
## 902                                 N
##      sold.fctr.predict.Final.bayesglm.accurate
## 1730                                     FALSE
## 312                                      FALSE
## 717                                      FALSE
## 37                                       FALSE
## 562                                      FALSE
## 902                                      FALSE
##      sold.fctr.predict.Final.bayesglm.error
## 1730                             -0.4381091
## 312                              -0.3545760
## 717                              -0.3321053
## 37                               -0.2354327
## 562                              -0.1339343
## 902                              -0.1291907
##      UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## 1390    11391         N                             0.9264140
## 283     10283         N                             0.9505481
## 1249    11250         N                             0.9596814
## 491     10491         N                             0.9879498
## 1690    11691         N                             0.9929848
## 1470    11471         N                             0.9975641
##      sold.fctr.predict.Final.bayesglm
## 1390                                Y
## 283                                 Y
## 1249                                Y
## 491                                 Y
## 1690                                Y
## 1470                                Y
##      sold.fctr.predict.Final.bayesglm.accurate
## 1390                                     FALSE
## 283                                      FALSE
## 1249                                     FALSE
## 491                                      FALSE
## 1690                                     FALSE
## 1470                                     FALSE
##      sold.fctr.predict.Final.bayesglm.error
## 1390                              0.3264140
## 283                               0.3505481
## 1249                              0.3596814
## 491                               0.3879498
## 1690                              0.3929848
## 1470                              0.3975641
```

![](ebayipads_dupobs_files/figure-html/fit.data.training_0-8.png) 

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
## [1] "sold.fctr.predict.Final.bayesglm.prob"
## [2] "sold.fctr.predict.Final.bayesglm"
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

![](ebayipads_dupobs_files/figure-html/fit.data.training_0-9.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn    end elapsed
## 15 fit.data.training          8          1 186.502 192.37   5.868
## 16  predict.data.new          9          0 192.370     NA      NA
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
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 41
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

![](ebayipads_dupobs_files/figure-html/predict.data.new-1.png) 

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

![](ebayipads_dupobs_files/figure-html/predict.data.new-2.png) 

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

![](ebayipads_dupobs_files/figure-html/predict.data.new-3.png) 

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

![](ebayipads_dupobs_files/figure-html/predict.data.new-4.png) 

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

![](ebayipads_dupobs_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## 1860    11862      <NA>                             0.1426249
## 1863    11865      <NA>                             0.5852284
## 1889    11891      <NA>                             0.9999594
## 2623    12625      <NA>                             0.0168142
##      sold.fctr.predict.Final.bayesglm
## 1860                                N
## 1863                                N
## 1889                                Y
## 2623                                N
##      sold.fctr.predict.Final.bayesglm.accurate
## 1860                                        NA
## 1863                                        NA
## 1889                                        NA
## 2623                                        NA
##      sold.fctr.predict.Final.bayesglm.error .label
## 1860                                      0  11862
## 1863                                      0  11865
## 1889                                      0  11891
## 2623                                      0  12625
## [1] "Inaccurate: "
##      UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## NA         NA      <NA>                                    NA
## NA.1       NA      <NA>                                    NA
## NA.2       NA      <NA>                                    NA
## NA.3       NA      <NA>                                    NA
## NA.4       NA      <NA>                                    NA
## NA.5       NA      <NA>                                    NA
##      sold.fctr.predict.Final.bayesglm
## NA                               <NA>
## NA.1                             <NA>
## NA.2                             <NA>
## NA.3                             <NA>
## NA.4                             <NA>
## NA.5                             <NA>
##      sold.fctr.predict.Final.bayesglm.accurate
## NA                                          NA
## NA.1                                        NA
## NA.2                                        NA
## NA.3                                        NA
## NA.4                                        NA
## NA.5                                        NA
##      sold.fctr.predict.Final.bayesglm.error
## NA                                       NA
## NA.1                                     NA
## NA.2                                     NA
## NA.3                                     NA
## NA.4                                     NA
## NA.5                                     NA
##        UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## NA.25        NA      <NA>                                    NA
## NA.81        NA      <NA>                                    NA
## NA.139       NA      <NA>                                    NA
## NA.148       NA      <NA>                                    NA
## NA.170       NA      <NA>                                    NA
## NA.737       NA      <NA>                                    NA
##        sold.fctr.predict.Final.bayesglm
## NA.25                              <NA>
## NA.81                              <NA>
## NA.139                             <NA>
## NA.148                             <NA>
## NA.170                             <NA>
## NA.737                             <NA>
##        sold.fctr.predict.Final.bayesglm.accurate
## NA.25                                         NA
## NA.81                                         NA
## NA.139                                        NA
## NA.148                                        NA
## NA.170                                        NA
## NA.737                                        NA
##        sold.fctr.predict.Final.bayesglm.error
## NA.25                                      NA
## NA.81                                      NA
## NA.139                                     NA
## NA.148                                     NA
## NA.170                                     NA
## NA.737                                     NA
##        UniqueID sold.fctr sold.fctr.predict.Final.bayesglm.prob
## NA.792       NA      <NA>                                    NA
## NA.793       NA      <NA>                                    NA
## NA.794       NA      <NA>                                    NA
## NA.795       NA      <NA>                                    NA
## NA.796       NA      <NA>                                    NA
## NA.797       NA      <NA>                                    NA
##        sold.fctr.predict.Final.bayesglm
## NA.792                             <NA>
## NA.793                             <NA>
## NA.794                             <NA>
## NA.795                             <NA>
## NA.796                             <NA>
## NA.797                             <NA>
##        sold.fctr.predict.Final.bayesglm.accurate
## NA.792                                        NA
## NA.793                                        NA
## NA.794                                        NA
## NA.795                                        NA
## NA.796                                        NA
## NA.797                                        NA
##        sold.fctr.predict.Final.bayesglm.error
## NA.792                                     NA
## NA.793                                     NA
## NA.794                                     NA
## NA.795                                     NA
## NA.796                                     NA
## NA.797                                     NA
```

```
## Warning: Removed 798 rows containing missing values (geom_point).
```

![](ebayipads_dupobs_files/figure-html/predict.data.new-6.png) 

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
    glb_force_prediction_lst[["0"]] <- c(11885)
    for (obs_id in glb_force_prediction_lst[["0"]])
        submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] <-
            max(0, submit_df[submit_df[, glb_id_var] == obs_id, "Probability1"] - 0.5)
    
    glb_force_prediction_lst[["1"]] <- c(11871, 11886, 11931, 11967, 11994, 11999, 
                                         12000, 12065, 12072, 12111, 12126, 
                                         12214, 12233, 12299, 12446, 12491, 
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
## [1] UniqueID                         .grpid                          
## [3] sold.fctr.predict.Final.bayesglm sold.0                          
## [5] sold.1                           sold.NA                         
## [7] .freq                            Probability1                    
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
    #print(glb_allobs_df[which(TfIdf_mtrx[, 246] > 0), c(glb_id_var, glb_txt_vars)])
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
## condit  207.6181  condit  495 133
## use     145.9567     use  290 611
## scratch 127.9087 scratch  286 499
## new     125.3927     new  156 385
## good    120.7748    good  197 260
## screen  106.8143  screen  213 500
##            TfIdf   term freq pos
## noth   13.436880   noth   14 391
## cabl   12.515281   cabl   18 103
## invis   4.689153  invis    5 304
## sprint  2.672499 sprint    2 549
## let     1.625083    let    1 328
## middl   1.263954  middl    1 365
##           TfIdf  term freq pos
## final 1.1375583 final    1 228
## fold  1.1375583  fold    1 236
## goe   1.1375583   goe    1 258
## high  1.1375583  high    1 280
## hole  1.1375583  hole    1 282
## 79in  0.9479652  79in    1  16
##     UniqueID
## 520    10520
##                                                                                             descr.my
## 520 Apple iPad mini 1st Generation 16GB, Wi- Fi, 7.9in - Space Gray, great condition comes with the 
## [1] 177
## [1] "    Top_n post_stem_words TfIDf terms for descr.my:"
##              TfIdf      term freq pos top_n
## paperwork 1.421948 paperwork    1 418  TRUE
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
## [1] "glb_sel_mdl_id: All.X.bayesglm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.bayesglm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 974  69
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 10            All.X.bayesglm        0.8000000   0.8355584     0.5928708
## 12      All.X.no.rnorm.rpart        0.7966102   0.8103723     0.5861800
## 9                  All.X.glm        0.7954802   0.8269987     0.5836701
## 8              Low.cor.X.glm        0.7864407   0.8296945     0.5677516
## 7    Interact.High.cor.Y.glm        0.7853107   0.8394608     0.5671369
## 11              All.X.glmnet        0.7853107   0.8374685     0.5653916
## 5            Max.cor.Y.rpart        0.7853107   0.8080205     0.5658292
## 13         All.X.no.rnorm.rf        0.7841808   0.8385905     0.5666714
## 6              Max.cor.Y.glm        0.7807910   0.8402773     0.5584673
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7728814   0.8075096     0.5406159
## 1          MFO.myMFO_classfr        0.5367232   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.5367232   0.5000000     0.0000000
## 2    Random.myrandom_classfr        0.4632768   0.5191913     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 10    974.6030                    0.6
## 12          NA                    0.6
## 9     944.2624                    0.6
## 8     935.6464                    0.5
## 7     946.8663                    0.4
## 11          NA                    0.5
## 5           NA                    0.5
## 13          NA                    0.4
## 6     943.1953                    0.4
## 4           NA                    0.6
## 1           NA                    0.5
## 3           NA                    0.5
## 2           NA                    0.4
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
## [1] "All.X.bayesglm OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 423  52
##         Y 125 285
##    prdline.my .n.OOB .n.Tst .freqRatio.Tst .freqRatio.OOB
## 3      iPad 2    171    154      0.1929825      0.1932203
## 5     iPadAir    151    137      0.1716792      0.1706215
## 1     Unknown     97     87      0.1090226      0.1096045
## 2      iPad 1     99     89      0.1115288      0.1118644
## 4     iPad 3+    136    123      0.1541353      0.1536723
## 6    iPadmini    127    114      0.1428571      0.1435028
## 7 iPadmini 2+    104     94      0.1177945      0.1175141
##   accurate.OOB.FALSE accurate.OOB.TRUE max.accuracy.OOB
## 3                 36               135        0.7894737
## 5                 30               121        0.8013245
## 1                 28                69        0.7113402
## 2                 22                77        0.7777778
## 4                 21               115        0.8455882
## 6                 21               106        0.8346457
## 7                 19                85        0.8173077
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
##                                    N.x All.X.bayesglm.importance
## startprice.log              100.000000                100.000000
## biddable                     94.720022                 94.720022
## idseq.my                     48.121762                 48.121762
## prdline.my.fctr              32.302503                 32.302503
## D.ratio.nstopwrds.nwrds      26.323096                 26.323096
## D.TfIdf.sum.stem.stop.Ratio  23.352887                 23.352887
## .rnorm                       20.456519                 20.456519
## D.npnct15.log                19.859878                 19.859878
## D.npnct03.log                19.699198                 19.699198
## D.terms.n.stem.stop.Ratio    19.469403                 19.469403
## D.npnct01.log                18.926519                 18.926519
## storage.fctr                 18.264306                 18.264306
## D.npnct28.log                18.184557                 18.184557
## D.ratio.sum.TfIdf.nwrds      17.889190                 17.889190
## D.npnct10.log                17.652897                 17.652897
## D.npnct09.log                17.387066                 17.387066
## D.npnct11.log                16.045510                 16.045510
## D.npnct08.log                16.026016                 16.026016
## D.TfIdf.sum.post.stop        15.682209                 15.682209
## D.npnct12.log                15.644402                 15.644402
## D.TfIdf.sum.post.stem        15.228525                 15.228525
## D.sum.TfIdf                  15.228525                 15.228525
## D.npnct16.log                15.122193                 15.122193
## D.npnct06.log                14.718131                 14.718131
## D.ndgts.log                  13.597509                 13.597509
## D.npnct05.log                12.955382                 12.955382
## D.npnct24.log                12.845505                 12.845505
## D.npnct14.log                12.808880                 12.808880
## D.npnct13.log                12.568451                 12.568451
## D.nstopwrds.log              10.795659                 10.795659
## .clusterid.fctr              10.171254                 10.171254
## D.nwrds.log                   9.988717                  9.988717
## color.fctr                    9.241439                  9.241439
## D.nuppr.log                   8.117272                  8.117272
## D.nchrs.log                   7.930601                  7.930601
## carrier.fctr                  6.640438                  6.640438
## D.terms.n.post.stop           6.303720                  6.303720
## D.nwrds.unq.log               6.158399                  6.158399
## D.terms.n.post.stem           6.158399                  6.158399
## cellular.fctr                 4.979886                  4.979886
## condition.fctr                0.000000                  0.000000
##                                    N.y importance
## startprice.log              100.000000 100.000000
## biddable                     95.026117  95.026117
## idseq.my                     49.511380  49.511380
## prdline.my.fctr              11.912115  11.912115
## D.ratio.nstopwrds.nwrds      27.012354  27.012354
## D.TfIdf.sum.stem.stop.Ratio  22.182931  22.182931
## .rnorm                       18.288850  18.288850
## D.npnct15.log                20.409144  20.409144
## D.npnct03.log                19.628451  19.628451
## D.terms.n.stem.stop.Ratio    20.407526  20.407526
## D.npnct01.log                19.349887  19.349887
## storage.fctr                 18.248570  18.248570
## D.npnct28.log                18.170112  18.170112
## D.ratio.sum.TfIdf.nwrds      16.015847  16.015847
## D.npnct10.log                18.632122  18.632122
## D.npnct09.log                17.752750  17.752750
## D.npnct11.log                17.304168  17.304168
## D.npnct08.log                16.596594  16.596594
## D.TfIdf.sum.post.stop        14.864382  14.864382
## D.npnct12.log                16.183600  16.183600
## D.TfIdf.sum.post.stem        14.423240  14.423240
## D.sum.TfIdf                  14.423240  14.423240
## D.npnct16.log                16.010185  16.010185
## D.npnct06.log                15.782577  15.782577
## D.ndgts.log                  14.605229  14.605229
## D.npnct05.log                13.947319  13.947319
## D.npnct24.log                12.688763  12.688763
## D.npnct14.log                15.280287  15.280287
## D.npnct13.log                13.585120  13.585120
## D.nstopwrds.log              12.403890  12.403890
## .clusterid.fctr              11.936703  11.936703
## D.nwrds.log                  10.911417  10.911417
## color.fctr                   13.332115  13.332115
## D.nuppr.log                   9.940809   9.940809
## D.nchrs.log                   9.277722   9.277722
## carrier.fctr                  9.121777   9.121777
## D.terms.n.post.stop           8.265215   8.265215
## D.nwrds.unq.log               8.221215   8.221215
## D.terms.n.post.stem           8.221215   8.221215
## cellular.fctr                 8.675136   8.675136
## condition.fctr                0.000000   0.000000
##                             Final.bayesglm.importance
## startprice.log                             100.000000
## biddable                                    95.026117
## idseq.my                                    49.511380
## prdline.my.fctr                             11.912115
## D.ratio.nstopwrds.nwrds                     27.012354
## D.TfIdf.sum.stem.stop.Ratio                 22.182931
## .rnorm                                      18.288850
## D.npnct15.log                               20.409144
## D.npnct03.log                               19.628451
## D.terms.n.stem.stop.Ratio                   20.407526
## D.npnct01.log                               19.349887
## storage.fctr                                18.248570
## D.npnct28.log                               18.170112
## D.ratio.sum.TfIdf.nwrds                     16.015847
## D.npnct10.log                               18.632122
## D.npnct09.log                               17.752750
## D.npnct11.log                               17.304168
## D.npnct08.log                               16.596594
## D.TfIdf.sum.post.stop                       14.864382
## D.npnct12.log                               16.183600
## D.TfIdf.sum.post.stem                       14.423240
## D.sum.TfIdf                                 14.423240
## D.npnct16.log                               16.010185
## D.npnct06.log                               15.782577
## D.ndgts.log                                 14.605229
## D.npnct05.log                               13.947319
## D.npnct24.log                               12.688763
## D.npnct14.log                               15.280287
## D.npnct13.log                               13.585120
## D.nstopwrds.log                             12.403890
## .clusterid.fctr                             11.936703
## D.nwrds.log                                 10.911417
## color.fctr                                  13.332115
## D.nuppr.log                                  9.940809
## D.nchrs.log                                  9.277722
## carrier.fctr                                 9.121777
## D.terms.n.post.stop                          8.265215
## D.nwrds.unq.log                              8.221215
## D.terms.n.post.stem                          8.221215
## cellular.fctr                                8.675136
## condition.fctr                               0.000000
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

![](ebayipads_dupobs_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification)
    print(table(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)]))
```

```
## 
##   N   Y 
## 602 196
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
## 16     predict.data.new          9          0 192.370 198.286   5.917
## 17 display.session.info         10          0 198.287      NA      NA
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
## 5         extract.features          3          0  20.128  76.969  56.841
## 11              fit.models          7          1 111.325 160.457  49.133
## 10              fit.models          7          0  86.042 111.324  25.282
## 12              fit.models          7          2 160.458 177.159  16.701
## 16        predict.data.new          9          0 192.370 198.286   5.917
## 15       fit.data.training          8          1 186.502 192.370   5.868
## 1              import.data          1          0   9.156  14.873   5.717
## 14       fit.data.training          8          0 181.481 186.502   5.021
## 13              fit.models          7          3 177.160 181.481   4.321
## 2             inspect.data          2          0  14.873  18.948   4.075
## 8          select.features          5          0  81.459  85.179   3.720
## 7      manage.missing.data          4          1  77.941  81.458   3.517
## 6             cluster.data          4          0  76.969  77.940   0.971
## 9  partition.data.training          6          0  85.179  86.041   0.863
## 3               scrub.data          2          1  18.948  19.654   0.706
## 4           transform.data          2          2  19.655  20.128   0.473
##    duration
## 5    56.841
## 11   49.132
## 10   25.282
## 12   16.701
## 16    5.916
## 15    5.868
## 1     5.717
## 14    5.021
## 13    4.321
## 2     4.075
## 8     3.720
## 7     3.517
## 6     0.971
## 9     0.862
## 3     0.706
## 4     0.473
## [1] "Total Elapsed Time: 198.286 secs"
```

![](ebayipads_dupobs_files/figure-html/display.session.info-1.png) 

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
## [19] brglm_0.5-9         mgcv_1.8-6          car_2.0-25         
## [22] nnet_7.3-10         lazyeval_0.1.10     pbkrtest_0.4-2     
## [25] magrittr_1.5        evaluate_0.7        nlme_3.1-121       
## [28] class_7.3-13        tools_3.2.1         formatR_1.2        
## [31] munsell_0.4.2       compiler_3.2.1      e1071_1.6-6        
## [34] caTools_1.17.1      nloptr_1.0.4        bitops_1.0-6       
## [37] labeling_0.3        rmarkdown_0.7       gtable_0.1.2       
## [40] codetools_0.2-14    abind_1.4-3         R6_2.1.0           
## [43] knitr_1.10.5        KernSmooth_2.23-15  stringi_0.5-5      
## [46] Rcpp_0.11.6         coda_0.17-1
```
